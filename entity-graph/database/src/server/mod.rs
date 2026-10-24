//! A thin HTTP wrapper over the public API: JSON in, JSON out, one endpoint
//! per operation, no business logic. Behind the `server` cargo feature.
//!
//! An embedder opens a [`Store`], registers any write middleware, then builds
//! a router with [`router`] or serves it with [`serve`]. The store is not
//! `Sync`, so it lives behind an `Arc<Mutex<..>>` and every call runs in
//! `tokio::task::spawn_blocking`.

mod dto;
mod error;

use std::collections::HashMap;
use std::net::SocketAddr;
use std::sync::{Arc, Mutex};

use axum::extract::State;
use axum::http::header::AUTHORIZATION;
use axum::http::StatusCode;
use axum::response::Response;
use axum::routing::{get, post};
use axum::{middleware, Json, Router};
use serde::Deserialize;
use serde_json::json;
use tokio::net::TcpListener;

use crate::error::Result;
use crate::store::Store;
use crate::types::{Direction, ParentRef, SeqRange, WriteBatch};
use crate::{Filter, FindOptions, Follow, ProveOptions, WalkOptions};
use dto::query::{
    node_predicate, FilterDto, FindDto, FindOptionsDto, FollowDto, ProveDto, ProveOptionsDto,
    WalkDto, WalkOptionsDto,
};
use dto::{
    link_groups_to_dto, parse_branch, value_map_to_json, value_to_json, BranchInfoDto,
    DirectionDto, LinkDto, LinkGroupDto, ParentRefDto, SeqRangeDto, UndoOutcomeDto, WriteBatchDto,
    WriteResultDto,
};
use error::{ApiError, ApiJson};

/// A store shared across request tasks.
pub type SharedStore = Arc<Mutex<Store>>;

/// Server configuration.
#[derive(Clone, Debug, Default)]
pub struct Config {
    /// When set, every request (except `/health`) must carry
    /// `Authorization: Bearer <token>`.
    pub token: Option<String>,
}

#[derive(Clone)]
struct AppState {
    store: SharedStore,
    token: Option<String>,
}

/// Build the router for a store. The store is wrapped in an `Arc<Mutex<..>>`.
pub fn router(store: Store, config: Config) -> Router {
    router_with(Arc::new(Mutex::new(store)), config)
}

/// Build the router for an already shared store, so an embedder can keep a
/// handle to it (middleware is meant to be registered on the store first).
pub fn router_with(store: SharedStore, config: Config) -> Router {
    let state = AppState {
        store,
        token: config.token,
    };
    let protected = Router::new()
        .route("/write", post(write))
        .route("/archive_entity", post(archive_entity))
        .route("/restore_entity", post(restore_entity))
        .route("/undo", post(undo))
        .route("/values", post(values))
        .route("/value", post(value))
        .route("/links", post(links))
        .route("/links_including_archived", post(links_including_archived))
        .route("/children", post(children))
        .route("/parents", post(parents))
        .route("/exists", post(exists))
        .route("/is_archived", post(is_archived))
        .route("/find", post(find))
        .route("/walk", post(walk))
        .route("/prove", post(prove))
        .route("/branches", get(branches))
        .route("/branch", post(branch))
        .route("/create_branch", post(create_branch))
        .route("/fork", post(fork))
        .route("/pull", post(pull))
        .route("/merge", post(merge))
        .route("/set_parents", post(set_parents))
        .route("/archive_branch", post(archive_branch))
        .layer(middleware::from_fn_with_state(state.clone(), auth));
    Router::new()
        .route("/health", get(health))
        .merge(protected)
        .with_state(state)
}

/// Bind a listener on `addr` (for example `127.0.0.1:0` for a free port).
pub async fn bind(addr: SocketAddr) -> std::io::Result<TcpListener> {
    TcpListener::bind(addr).await
}

/// Serve the API on `listener` until the process gets Ctrl+C.
pub async fn serve(store: Store, listener: TcpListener, config: Config) -> std::io::Result<()> {
    axum::serve(listener, router(store, config))
        .with_graceful_shutdown(shutdown_signal())
        .await
}

async fn shutdown_signal() {
    let _ = tokio::signal::ctrl_c().await;
}

/// Run a blocking store call off the async runtime, mapping errors.
async fn on_store<F, T>(state: &AppState, f: F) -> std::result::Result<T, ApiError>
where
    F: FnOnce(&mut Store) -> Result<T> + Send + 'static,
    T: Send + 'static,
{
    let store = state.store.clone();
    let joined = tokio::task::spawn_blocking(move || {
        // A panic inside a store call poisons the mutex. Recover: the store's
        // own transaction has already rolled back on unwind, so the data is
        // consistent and later requests can go on.
        let mut guard = store.lock().unwrap_or_else(|e| e.into_inner());
        f(&mut guard)
    })
    .await;
    match joined {
        Ok(result) => result.map_err(ApiError::from),
        Err(e) => Err(ApiError(
            StatusCode::INTERNAL_SERVER_ERROR,
            "internal",
            e.to_string(),
        )),
    }
}

/// Reject a request that lacks the configured bearer token.
async fn auth(
    State(state): State<AppState>,
    req: axum::extract::Request,
    next: middleware::Next,
) -> std::result::Result<Response, ApiError> {
    if let Some(token) = &state.token {
        let presented = req
            .headers()
            .get(AUTHORIZATION)
            .and_then(|h| h.to_str().ok())
            .and_then(|h| h.strip_prefix("Bearer "));
        if presented != Some(token.as_str()) {
            return Err(ApiError(
                StatusCode::UNAUTHORIZED,
                "unauthorized",
                "missing or invalid bearer token".into(),
            ));
        }
    }
    Ok(next.run(req).await)
}

type ApiResult<T> = std::result::Result<Json<T>, ApiError>;

// --- writes ---------------------------------------------------------------

async fn health() -> Json<serde_json::Value> {
    Json(json!({ "ok": true }))
}

async fn write(
    State(state): State<AppState>,
    ApiJson(body): ApiJson<WriteBatchDto>,
) -> ApiResult<WriteResultDto> {
    let batch: WriteBatch = body.try_into()?;
    let result = on_store(&state, move |s| s.write(batch)).await?;
    Ok(Json(result.into()))
}

#[derive(Deserialize)]
struct ArchiveEntityReq {
    branch: String,
    author: String,
    entity: String,
}

async fn archive_entity(
    State(state): State<AppState>,
    ApiJson(body): ApiJson<ArchiveEntityReq>,
) -> ApiResult<WriteResultDto> {
    let id = parse_branch(&body.branch)?;
    let (author, entity) = (body.author, body.entity);
    let result = on_store(&state, move |s| s.archive_entity(id, &author, &entity)).await?;
    Ok(Json(result.into()))
}

#[derive(Deserialize)]
struct RestoreEntityReq {
    branch: String,
    author: String,
    entity: String,
    entity_type: String,
}

async fn restore_entity(
    State(state): State<AppState>,
    ApiJson(body): ApiJson<RestoreEntityReq>,
) -> ApiResult<WriteResultDto> {
    let id = parse_branch(&body.branch)?;
    let (author, entity, ty) = (body.author, body.entity, body.entity_type);
    let result = on_store(&state, move |s| s.restore_entity(id, &author, &entity, &ty)).await?;
    Ok(Json(result.into()))
}

#[derive(Deserialize)]
struct UndoReq {
    branch: String,
    author: String,
}

async fn undo(
    State(state): State<AppState>,
    ApiJson(body): ApiJson<UndoReq>,
) -> ApiResult<Option<UndoOutcomeDto>> {
    let id = parse_branch(&body.branch)?;
    let author = body.author;
    let outcome = on_store(&state, move |s| s.undo(id, &author)).await?;
    Ok(Json(outcome.map(UndoOutcomeDto::from)))
}

// --- value and link reads -------------------------------------------------

#[derive(Deserialize)]
struct EntitiesReq {
    branch: String,
    entities: Vec<String>,
}

async fn values(
    State(state): State<AppState>,
    ApiJson(body): ApiJson<EntitiesReq>,
) -> ApiResult<serde_json::Value> {
    let id = parse_branch(&body.branch)?;
    let entities = body.entities;
    let map = on_store(&state, move |s| {
        let refs: Vec<&str> = entities.iter().map(String::as_str).collect();
        s.values(id, &refs)
    })
    .await?;
    let obj: serde_json::Map<String, serde_json::Value> = map
        .iter()
        .map(|(id, vals)| (id.clone(), value_map_to_json(vals)))
        .collect();
    Ok(Json(serde_json::Value::Object(obj)))
}

#[derive(Deserialize)]
struct ValueReq {
    branch: String,
    entity: String,
    key: String,
}

async fn value(
    State(state): State<AppState>,
    ApiJson(body): ApiJson<ValueReq>,
) -> ApiResult<serde_json::Value> {
    let id = parse_branch(&body.branch)?;
    let (entity, key) = (body.entity, body.key);
    let found = on_store(&state, move |s| s.value(id, &entity, &key)).await?;
    Ok(Json(
        found
            .as_ref()
            .map(value_to_json)
            .unwrap_or(serde_json::Value::Null),
    ))
}

#[derive(Deserialize)]
struct LinksReq {
    branch: String,
    entities: Vec<String>,
    groups: Vec<(String, DirectionDto)>,
}

async fn do_links(
    state: AppState,
    body: LinksReq,
    typed_only: bool,
) -> ApiResult<HashMap<String, Vec<LinkGroupDto>>> {
    let id = parse_branch(&body.branch)?;
    let entities = body.entities;
    let groups: Vec<(String, Direction)> = body
        .groups
        .into_iter()
        .map(|(t, d)| (t, d.into()))
        .collect();
    let map = on_store(&state, move |s| {
        let erefs: Vec<&str> = entities.iter().map(String::as_str).collect();
        let grefs: Vec<(&str, Direction)> = groups.iter().map(|(t, d)| (t.as_str(), *d)).collect();
        if typed_only {
            s.links(id, &erefs, &grefs)
        } else {
            s.links_including_archived(id, &erefs, &grefs)
        }
    })
    .await?;
    let out = map
        .into_iter()
        .map(|(id, groups)| (id, link_groups_to_dto(groups)))
        .collect();
    Ok(Json(out))
}

async fn links(
    State(state): State<AppState>,
    ApiJson(body): ApiJson<LinksReq>,
) -> ApiResult<HashMap<String, Vec<LinkGroupDto>>> {
    do_links(state, body, true).await
}

async fn links_including_archived(
    State(state): State<AppState>,
    ApiJson(body): ApiJson<LinksReq>,
) -> ApiResult<HashMap<String, Vec<LinkGroupDto>>> {
    do_links(state, body, false).await
}

#[derive(Deserialize)]
struct OneGroupReq {
    branch: String,
    entity: String,
    link_type: String,
}

async fn children(
    State(state): State<AppState>,
    ApiJson(body): ApiJson<OneGroupReq>,
) -> ApiResult<Vec<LinkDto>> {
    let id = parse_branch(&body.branch)?;
    let (entity, link_type) = (body.entity, body.link_type);
    let links = on_store(&state, move |s| s.children(id, &entity, &link_type)).await?;
    Ok(Json(links.into_iter().map(LinkDto::from).collect()))
}

async fn parents(
    State(state): State<AppState>,
    ApiJson(body): ApiJson<OneGroupReq>,
) -> ApiResult<Vec<LinkDto>> {
    let id = parse_branch(&body.branch)?;
    let (entity, link_type) = (body.entity, body.link_type);
    let links = on_store(&state, move |s| s.parents(id, &entity, &link_type)).await?;
    Ok(Json(links.into_iter().map(LinkDto::from).collect()))
}

async fn exists(
    State(state): State<AppState>,
    ApiJson(body): ApiJson<EntitiesReq>,
) -> ApiResult<HashMap<String, bool>> {
    let id = parse_branch(&body.branch)?;
    let entities = body.entities;
    let map = on_store(&state, move |s| {
        let refs: Vec<&str> = entities.iter().map(String::as_str).collect();
        s.exists(id, &refs)
    })
    .await?;
    Ok(Json(map))
}

async fn is_archived(
    State(state): State<AppState>,
    ApiJson(body): ApiJson<EntitiesReq>,
) -> ApiResult<HashMap<String, bool>> {
    let id = parse_branch(&body.branch)?;
    let entities = body.entities;
    let map = on_store(&state, move |s| {
        let refs: Vec<&str> = entities.iter().map(String::as_str).collect();
        s.is_archived(id, &refs)
    })
    .await?;
    Ok(Json(map))
}

// --- find / walk / prove --------------------------------------------------

#[derive(Deserialize)]
struct FindReq {
    branch: String,
    filter: FilterDto,
    #[serde(default)]
    options: Option<FindOptionsDto>,
}

async fn find(
    State(state): State<AppState>,
    ApiJson(body): ApiJson<FindReq>,
) -> ApiResult<FindDto> {
    let id = parse_branch(&body.branch)?;
    let filter: Filter = body.filter.into();
    let options: FindOptions = body.options.unwrap_or_default().into();
    let page = on_store(&state, move |s| s.find(id, &filter, options)).await?;
    Ok(Json(page.into()))
}

#[derive(Deserialize)]
struct WalkReq {
    branch: String,
    start: Vec<String>,
    follow: FollowDto,
    #[serde(default)]
    descend: Option<FilterDto>,
    #[serde(default)]
    include: Option<FilterDto>,
    #[serde(default)]
    options: Option<WalkOptionsDto>,
}

async fn walk(
    State(state): State<AppState>,
    ApiJson(body): ApiJson<WalkReq>,
) -> ApiResult<WalkDto> {
    let id = parse_branch(&body.branch)?;
    let start = body.start;
    let follow: Follow = body.follow.into();
    let descend = body.descend.map(Filter::from);
    let include = body.include.map(Filter::from);
    let options: WalkOptions = body.options.unwrap_or_default().into();
    let result = on_store(&state, move |s| {
        let refs: Vec<&str> = start.iter().map(String::as_str).collect();
        s.walk(
            id,
            &refs,
            &follow,
            node_predicate(descend),
            node_predicate(include),
            options,
        )
    })
    .await?;
    Ok(Json(result.into()))
}

#[derive(Deserialize)]
struct ProveReq {
    branch: String,
    item: String,
    roots: Vec<String>,
    relationships: FollowDto,
    #[serde(default)]
    options: Option<ProveOptionsDto>,
}

async fn prove(
    State(state): State<AppState>,
    ApiJson(body): ApiJson<ProveReq>,
) -> ApiResult<ProveDto> {
    let id = parse_branch(&body.branch)?;
    let item = body.item;
    let roots = body.roots;
    let relationships: Follow = body.relationships.into();
    let options: ProveOptions = body.options.unwrap_or_default().into();
    let result = on_store(&state, move |s| {
        let refs: Vec<&str> = roots.iter().map(String::as_str).collect();
        s.prove(id, &item, &refs, &relationships, options)
    })
    .await?;
    Ok(Json(result.into()))
}

// --- branches -------------------------------------------------------------

async fn branches(State(state): State<AppState>) -> ApiResult<Vec<BranchInfoDto>> {
    let list = on_store(&state, |s| s.branches()).await?;
    Ok(Json(list.into_iter().map(Into::into).collect()))
}

#[derive(Deserialize)]
struct BranchReq {
    branch: String,
}

async fn branch(
    State(state): State<AppState>,
    ApiJson(body): ApiJson<BranchReq>,
) -> ApiResult<BranchInfoDto> {
    let id = parse_branch(&body.branch)?;
    let info = on_store(&state, move |s| s.branch(id)).await?;
    Ok(Json(info.into()))
}

#[derive(Deserialize)]
struct CreateBranchReq {
    name: String,
    #[serde(default)]
    parents: Vec<ParentRefDto>,
}

async fn create_branch(
    State(state): State<AppState>,
    ApiJson(body): ApiJson<CreateBranchReq>,
) -> ApiResult<serde_json::Value> {
    let name = body.name;
    let parents: Vec<ParentRef> = body
        .parents
        .into_iter()
        .map(ParentRef::try_from)
        .collect::<Result<_>>()?;
    let id = on_store(&state, move |s| s.create_branch(&name, &parents)).await?;
    Ok(Json(json!({ "branch": id.to_string() })))
}

#[derive(Deserialize)]
struct ForkReq {
    from: String,
    name: String,
}

async fn fork(
    State(state): State<AppState>,
    ApiJson(body): ApiJson<ForkReq>,
) -> ApiResult<serde_json::Value> {
    let from = parse_branch(&body.from)?;
    let name = body.name;
    let id = on_store(&state, move |s| s.fork(from, &name)).await?;
    Ok(Json(json!({ "branch": id.to_string() })))
}

#[derive(Deserialize)]
struct PullReq {
    into: String,
    from: String,
    range: SeqRangeDto,
}

async fn pull(
    State(state): State<AppState>,
    ApiJson(body): ApiJson<PullReq>,
) -> ApiResult<serde_json::Value> {
    let into = parse_branch(&body.into)?;
    let from = parse_branch(&body.from)?;
    let range: SeqRange = body.range.into();
    on_store(&state, move |s| s.pull(into, from, range)).await?;
    Ok(Json(json!({})))
}

#[derive(Deserialize)]
struct MergeReq {
    into: String,
    from: String,
}

async fn merge(
    State(state): State<AppState>,
    ApiJson(body): ApiJson<MergeReq>,
) -> ApiResult<serde_json::Value> {
    let into = parse_branch(&body.into)?;
    let from = parse_branch(&body.from)?;
    on_store(&state, move |s| s.merge(into, from)).await?;
    Ok(Json(json!({})))
}

#[derive(Deserialize)]
struct SetParentsReq {
    branch: String,
    #[serde(default)]
    parents: Vec<ParentRefDto>,
}

async fn set_parents(
    State(state): State<AppState>,
    ApiJson(body): ApiJson<SetParentsReq>,
) -> ApiResult<serde_json::Value> {
    let id = parse_branch(&body.branch)?;
    let parents: Vec<ParentRef> = body
        .parents
        .into_iter()
        .map(ParentRef::try_from)
        .collect::<Result<_>>()?;
    on_store(&state, move |s| s.set_parents(id, &parents)).await?;
    Ok(Json(json!({})))
}

#[derive(Deserialize)]
struct ArchiveBranchReq {
    branch: String,
    archived: bool,
}

async fn archive_branch(
    State(state): State<AppState>,
    ApiJson(body): ApiJson<ArchiveBranchReq>,
) -> ApiResult<serde_json::Value> {
    let id = parse_branch(&body.branch)?;
    let archived = body.archived;
    on_store(&state, move |s| s.archive_branch(id, archived)).await?;
    Ok(Json(json!({})))
}
