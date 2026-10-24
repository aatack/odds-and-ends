//! A thin HTTP wrapper over the public API: JSON in, JSON out, one endpoint
//! per operation, no business logic. Behind the `server` cargo feature.
//!
//! An embedder opens a [`Store`], registers any write middleware, then builds
//! a router with [`router`] or serves it with [`serve`]. The store is not
//! `Sync`, so it lives behind an `Arc<Mutex<..>>` and every call runs in
//! `tokio::task::spawn_blocking`.

mod dto;
mod error;

use std::net::SocketAddr;
use std::sync::{Arc, Mutex};

use axum::extract::State;
use axum::http::header::AUTHORIZATION;
use axum::http::StatusCode;
use axum::response::Response;
use axum::routing::{get, post};
use axum::{middleware, Json, Router};
use serde::Deserialize;
use tokio::net::TcpListener;

use crate::error::Result;
use crate::store::Store;
use crate::types::{ParentRef, WriteBatch};
use dto::{BranchInfoDto, WriteBatchDto, WriteResultDto};
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
        .route("/branches", get(branches))
        .route("/branch", post(branch))
        .route("/create_branch", post(create_branch))
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
        let mut guard = store.lock().expect("store mutex poisoned");
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

async fn health() -> Json<serde_json::Value> {
    Json(serde_json::json!({ "ok": true }))
}

async fn write(
    State(state): State<AppState>,
    ApiJson(body): ApiJson<WriteBatchDto>,
) -> std::result::Result<Json<WriteResultDto>, ApiError> {
    let batch: WriteBatch = body.try_into()?;
    let result = on_store(&state, move |s| s.write(batch)).await?;
    Ok(Json(result.into()))
}

async fn branches(
    State(state): State<AppState>,
) -> std::result::Result<Json<Vec<BranchInfoDto>>, ApiError> {
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
) -> std::result::Result<Json<BranchInfoDto>, ApiError> {
    let id = dto::parse_branch(&body.branch)?;
    let info = on_store(&state, move |s| s.branch(id)).await?;
    Ok(Json(info.into()))
}

#[derive(Deserialize)]
struct CreateBranchReq {
    name: String,
    #[serde(default)]
    parents: Vec<dto::ParentRefDto>,
}

async fn create_branch(
    State(state): State<AppState>,
    ApiJson(body): ApiJson<CreateBranchReq>,
) -> std::result::Result<Json<serde_json::Value>, ApiError> {
    let name = body.name;
    let parents: Vec<ParentRef> = body
        .parents
        .into_iter()
        .map(ParentRef::try_from)
        .collect::<Result<_>>()?;
    let id = on_store(&state, move |s| s.create_branch(&name, &parents)).await?;
    Ok(Json(serde_json::json!({ "branch": id.to_string() })))
}
