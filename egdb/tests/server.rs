//! Integration tests for the HTTP API. Each test opens a store on its own
//! temp file, binds an ephemeral port, spawns the server, and drives it with
//! a real HTTP client. Behind the `server` feature; without it this file is
//! empty.
#![cfg(feature = "server")]

use std::path::PathBuf;
use std::sync::atomic::{AtomicU64, Ordering};

use egdb::server::{self, Config};
use egdb::{Error, Op, Store, WriteContext};
use reqwest::Client;
use serde_json::{json, Value};

/// A temp db path that removes its files when dropped.
struct TempDb(PathBuf);

impl TempDb {
    fn new() -> Self {
        static N: AtomicU64 = AtomicU64::new(0);
        let n = N.fetch_add(1, Ordering::SeqCst);
        let path =
            std::env::temp_dir().join(format!("egdb_server_test_{}_{n}.db", std::process::id()));
        let db = TempDb(path);
        db.remove();
        db
    }
    fn remove(&self) {
        for suffix in ["", "-wal", "-shm"] {
            let _ = std::fs::remove_file(format!("{}{suffix}", self.0.display()));
        }
    }
}

impl Drop for TempDb {
    fn drop(&mut self) {
        self.remove();
    }
}

/// A running server: its base URL and the temp db guard.
struct Server {
    base: String,
    _db: TempDb,
}

/// Start a server with `config`, running `setup` on the store first (for
/// example to register middleware).
async fn start(config: Config, setup: impl FnOnce(&mut Store)) -> Server {
    let db = TempDb::new();
    let mut store = Store::open(&db.0).unwrap();
    setup(&mut store);
    let listener = server::bind("127.0.0.1:0".parse().unwrap()).await.unwrap();
    let addr = listener.local_addr().unwrap();
    tokio::spawn(async move {
        let _ = server::serve(store, listener, config).await;
    });
    Server {
        base: format!("http://{addr}"),
        _db: db,
    }
}

/// The default server: no token, no middleware.
async fn plain() -> Server {
    start(Config::default(), |_| {}).await
}

// --- HTTP helpers ---------------------------------------------------------

async fn post(base: &str, path: &str, body: Value) -> reqwest::Response {
    Client::new()
        .post(format!("{base}{path}"))
        .json(&body)
        .send()
        .await
        .unwrap()
}

async fn get(base: &str, path: &str) -> reqwest::Response {
    Client::new()
        .get(format!("{base}{path}"))
        .send()
        .await
        .unwrap()
}

/// POST and parse the JSON body, asserting 200.
async fn post_ok(base: &str, path: &str, body: Value) -> Value {
    let r = post(base, path, body).await;
    assert_eq!(r.status().as_u16(), 200, "POST {path}");
    r.json().await.unwrap()
}

async fn make_branch(base: &str) -> String {
    let v = post_ok(
        base,
        "/create_branch",
        json!({ "name": "main", "parents": [] }),
    )
    .await;
    v["branch"].as_str().unwrap().to_owned()
}

// --- tests ----------------------------------------------------------------

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn health_is_open() {
    let s = plain().await;
    let r = get(&s.base, "/health").await;
    assert_eq!(r.status().as_u16(), 200);
    assert_eq!(r.json::<Value>().await.unwrap(), json!({ "ok": true }));
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn branch_create_list_get() {
    let s = plain().await;
    let b = make_branch(&s.base).await;

    let list = get(&s.base, "/branches")
        .await
        .json::<Value>()
        .await
        .unwrap();
    assert_eq!(list.as_array().unwrap().len(), 1);
    assert_eq!(list[0]["id"], b);

    let info = post_ok(&s.base, "/branch", json!({ "branch": b })).await;
    assert_eq!(info["name"], "main");
    assert_eq!(info["head"], 0);
    assert_eq!(info["archived"], false);
    assert_eq!(info["parents"], json!([]));
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn write_and_reads() {
    let s = plain().await;
    let b = make_branch(&s.base).await;
    // a, c typed tasks; link a->c explicit 2.5 and a->d timestamp; d typed.
    let w = post_ok(
        &s.base,
        "/write",
        json!({
            "branch": b, "author": "u",
            "ops": [
                { "set": { "entity": "a", "key": "type", "value": "task" } },
                { "set": { "entity": "a", "key": "title", "value": "alpha" } },
                { "set": { "entity": "c", "key": "type", "value": "task" } },
                { "set": { "entity": "d", "key": "type", "value": "task" } },
                { "link": { "source": "a", "destination": "c", "link_type": "child", "sort": { "explicit": 2.5 } } },
                { "link": { "source": "a", "destination": "d", "link_type": "child" } }
            ]
        }),
    )
    .await;
    assert_eq!(w["first_seq"], 1);
    assert_eq!(w["last_seq"], 6);

    // values
    let vals = post_ok(
        &s.base,
        "/values",
        json!({ "branch": b, "entities": ["a"] }),
    )
    .await;
    assert_eq!(vals["a"]["title"], "alpha");
    assert_eq!(vals["a"]["type"], "task");

    // value_events: who set each value, and when
    let events = post_ok(
        &s.base,
        "/value_events",
        json!({ "branch": b, "entities": ["a", "nobody"] }),
    )
    .await;
    assert!(events["a"]["title"]["timestamp"].as_i64().unwrap() > 0);
    assert!(events["a"]["title"]["author"].is_string());
    assert_eq!(events["nobody"], json!({}));

    // value_log and link_log: every event, the newest first, filtered
    let log = post_ok(&s.base, "/value_log", json!({ "branch": b, "entities": ["a"], "keys": ["title"] })).await;
    assert_eq!(log[0]["entity"], "a");
    assert_eq!(log[0]["value"], "alpha");
    assert!(log[0]["event"]["author"].is_string());
    let none = post_ok(&s.base, "/link_log", json!({ "branch": b, "authors": ["nobody"] })).await;
    assert_eq!(none, json!([]));

    // value
    let one = post_ok(
        &s.base,
        "/value",
        json!({ "branch": b, "entity": "a", "key": "title" }),
    )
    .await;
    assert_eq!(one, json!("alpha"));
    let missing = post_ok(
        &s.base,
        "/value",
        json!({ "branch": b, "entity": "a", "key": "nope" }),
    )
    .await;
    assert_eq!(missing, Value::Null);

    // links: a's outbound children, sorted by sort key (explicit 2.5, then ts)
    let links = post_ok(
        &s.base,
        "/links",
        json!({ "branch": b, "entities": ["a"], "groups": [["child", "outbound"]] }),
    )
    .await;
    let groups = links["a"].as_array().unwrap();
    assert_eq!(groups.len(), 1);
    assert_eq!(groups[0]["link_type"], "child");
    assert_eq!(groups[0]["direction"], "outbound");
    let ls = groups[0]["links"].as_array().unwrap();
    assert_eq!(ls.len(), 2);
    assert_eq!(ls[0]["destination"], "c");
    assert_eq!(ls[0]["sort"], 2.5);

    // children / parents helpers
    let kids = post_ok(
        &s.base,
        "/children",
        json!({ "branch": b, "entity": "a", "link_type": "child" }),
    )
    .await;
    assert_eq!(kids.as_array().unwrap().len(), 2);
    let ps = post_ok(
        &s.base,
        "/parents",
        json!({ "branch": b, "entity": "c", "link_type": "child" }),
    )
    .await;
    assert_eq!(ps[0]["source"], "a");

    // exists / is_archived
    let ex = post_ok(
        &s.base,
        "/exists",
        json!({ "branch": b, "entities": ["a", "zzz"] }),
    )
    .await;
    assert_eq!(ex["a"], true);
    assert_eq!(ex["zzz"], false);
    let ar = post_ok(
        &s.base,
        "/is_archived",
        json!({ "branch": b, "entities": ["a"] }),
    )
    .await;
    assert_eq!(ar["a"], false);

    // removed sort: unlink a->c; it drops out of children.
    post_ok(
        &s.base,
        "/write",
        json!({ "branch": b, "author": "u", "ops": [
            { "link": { "source": "a", "destination": "c", "link_type": "child", "sort": "removed" } }
        ] }),
    )
    .await;
    let kids = post_ok(
        &s.base,
        "/children",
        json!({ "branch": b, "entity": "a", "link_type": "child" }),
    )
    .await;
    let dests: Vec<&str> = kids
        .as_array()
        .unwrap()
        .iter()
        .map(|l| l["destination"].as_str().unwrap())
        .collect();
    assert_eq!(dests, vec!["d"]);

    // archive c's type; links hides it, links_including_archived shows it.
    post_ok(
        &s.base,
        "/archive_entity",
        json!({ "branch": b, "author": "u", "entity": "d" }),
    )
    .await;
    let shown = post_ok(
        &s.base,
        "/links",
        json!({ "branch": b, "entities": ["a"], "groups": [["child", "outbound"]] }),
    )
    .await;
    assert_eq!(shown["a"][0]["links"].as_array().unwrap().len(), 0);
    let all = post_ok(
        &s.base,
        "/links_including_archived",
        json!({ "branch": b, "entities": ["a"], "groups": [["child", "outbound"]] }),
    )
    .await;
    assert_eq!(all["a"][0]["links"].as_array().unwrap().len(), 1);
    let ar = post_ok(
        &s.base,
        "/is_archived",
        json!({ "branch": b, "entities": ["d"] }),
    )
    .await;
    assert_eq!(ar["d"], true);
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn archive_then_restore_entity() {
    let s = plain().await;
    let b = make_branch(&s.base).await;
    post_ok(
        &s.base,
        "/write",
        json!({ "branch": b, "author": "u", "ops": [
        { "set": { "entity": "e", "key": "type", "value": "task" } } ] }),
    )
    .await;
    post_ok(
        &s.base,
        "/archive_entity",
        json!({ "branch": b, "author": "u", "entity": "e" }),
    )
    .await;
    assert_eq!(
        post_ok(
            &s.base,
            "/is_archived",
            json!({ "branch": b, "entities": ["e"] })
        )
        .await["e"],
        true
    );
    post_ok(
        &s.base,
        "/restore_entity",
        json!({ "branch": b, "author": "u", "entity": "e", "entity_type": "note" }),
    )
    .await;
    assert_eq!(
        post_ok(
            &s.base,
            "/value",
            json!({ "branch": b, "entity": "e", "key": "type" })
        )
        .await,
        json!("note")
    );
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn find_filter_order_and_paging() {
    let s = plain().await;
    let b = make_branch(&s.base).await;
    let mut ops = Vec::new();
    for (id, score) in [("a", 3), ("b", 1), ("c", 2)] {
        ops.push(json!({ "set": { "entity": id, "key": "type", "value": "task" } }));
        ops.push(json!({ "set": { "entity": id, "key": "score", "value": score } }));
    }
    post_ok(
        &s.base,
        "/write",
        json!({ "branch": b, "author": "u", "ops": ops }),
    )
    .await;

    // filter: all tasks, ordered by score ascending -> b, c, a
    let page = post_ok(
        &s.base,
        "/find",
        json!({
            "branch": b,
            "filter": { "eq": { "key": "type", "value": "task" } },
            "options": { "order": { "value_asc": "score" } }
        }),
    )
    .await;
    let ids: Vec<&str> = page["rows"]
        .as_array()
        .unwrap()
        .iter()
        .map(|r| r["id"].as_str().unwrap())
        .collect();
    assert_eq!(ids, vec!["b", "c", "a"]);
    assert_eq!(page["next"], Value::Null);

    // paging by entity id with limit 2 -> page 1 has a,b and a next cursor
    let p1 = post_ok(
        &s.base,
        "/find",
        json!({
            "branch": b, "filter": { "eq": { "key": "type", "value": "task" } },
            "options": { "limit": 2 }
        }),
    )
    .await;
    assert_eq!(p1["rows"].as_array().unwrap().len(), 2);
    let cursor = p1["next"].as_str().unwrap().to_owned();
    let p2 = post_ok(
        &s.base,
        "/find",
        json!({
            "branch": b, "filter": { "eq": { "key": "type", "value": "task" } },
            "options": { "limit": 2, "cursor": cursor }
        }),
    )
    .await;
    assert_eq!(p2["rows"].as_array().unwrap().len(), 1);
    assert_eq!(p2["rows"][0]["id"], "c");
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn walk_follow_predicates_and_paging() {
    let s = plain().await;
    let b = make_branch(&s.base).await;
    post_ok(
        &s.base,
        "/write",
        json!({ "branch": b, "author": "u", "ops": [
        { "set": { "entity": "root", "key": "type", "value": "list" } },
        { "set": { "entity": "a", "key": "type", "value": "task" } },
        { "set": { "entity": "b", "key": "type", "value": "task" } },
        { "link": { "source": "root", "destination": "a", "link_type": "child" } },
        { "link": { "source": "root", "destination": "b", "link_type": "child" } },
        { "set": { "entity": "n", "key": "type", "value": "note" } },
        { "link": { "source": "root", "destination": "n", "link_type": "child" } }
    ] }),
    )
    .await;
    // Children of type note are not followed.
    let follow = json!({ "list": [["child", "outbound", "task"]] });

    // include only tasks: root is visited but not emitted.
    let w = post_ok(
        &s.base,
        "/walk",
        json!({
            "branch": b, "start": ["root"], "follow": follow,
            "include": { "eq": { "key": "type", "value": "task" } }
        }),
    )
    .await;
    assert_eq!(w["paths"], json!([["root", "a"], ["root", "b"]]));
    assert_eq!(w["steps"], 3);
    assert_eq!(w["next"], Value::Null);
    // The cache holds the entities on the paths, root included.
    let mut cached: Vec<&str> = w["cache"]["nodes"]
        .as_object()
        .unwrap()
        .keys()
        .map(String::as_str)
        .collect();
    cached.sort();
    assert_eq!(cached, vec!["a", "b", "root"]);
    assert_eq!(w["cache"]["nodes"]["a"]["values"]["type"], "task");

    // descend only where the absent key "never" is present (never) -> the walk
    // does not go below the start, so only the start is visited.
    let shallow = post_ok(
        &s.base,
        "/walk",
        json!({
            "branch": b, "start": ["root"], "follow": follow,
            "descend": { "not_null": { "key": "never" } }
        }),
    )
    .await;
    assert_eq!(shallow["steps"], 1);

    // steps paging: limit 1 step -> next resumes.
    let p1 = post_ok(
        &s.base,
        "/walk",
        json!({
            "branch": b, "start": ["root"], "follow": follow, "options": { "steps": 1 }
        }),
    )
    .await;
    assert_eq!(p1["steps"], 1);
    assert!(p1["next"].is_array());

    // backward from b: b, a, root. Collapsing root stops the forward walk.
    let back = post_ok(
        &s.base,
        "/walk",
        json!({
            "branch": b, "start": ["root", "b"], "follow": follow,
            "options": { "direction": "backward" }
        }),
    )
    .await;
    assert_eq!(
        back["paths"],
        json!([["root", "b"], ["root", "a"], ["root"]])
    );
    let collapsed = post_ok(
        &s.base,
        "/walk",
        json!({
            "branch": b, "start": ["root"], "follow": follow, "collapsed": [["root"]]
        }),
    )
    .await;
    assert_eq!(collapsed["steps"], 1);
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn prove_proved_and_no_path() {
    let s = plain().await;
    let b = make_branch(&s.base).await;
    post_ok(
        &s.base,
        "/write",
        json!({ "branch": b, "author": "u", "ops": [
        { "set": { "entity": "root", "key": "type", "value": "list" } },
        { "set": { "entity": "a", "key": "type", "value": "task" } },
        { "set": { "entity": "lonely", "key": "type", "value": "task" } },
        { "link": { "source": "root", "destination": "a", "link_type": "child" } }
    ] }),
    )
    .await;
    // A wildcard child type (null) follows children of any type.
    let follow = json!({ "list": [["child", "outbound", null]] });

    let proved = post_ok(
        &s.base,
        "/prove",
        json!({ "branch": b, "start": ["root"], "follow": follow, "item": "a" }),
    )
    .await;
    assert_eq!(proved["outcome"]["proved"]["path"], json!(["root", "a"]));
    assert_eq!(proved["stats"]["depth_reached"], 1);

    let none = post_ok(
        &s.base,
        "/prove",
        json!({ "branch": b, "start": ["root"], "follow": follow, "item": "lonely" }),
    )
    .await;
    assert_eq!(none["outcome"], json!("no_path"));

    // The walk's filters apply: an include that rejects the item, or a
    // descend that stops at the start, gives no path.
    for extra in [
        json!({ "include": { "eq": { "key": "type", "value": "list" } } }),
        json!({ "descend": { "not_null": { "key": "never" } } }),
        json!({ "collapsed": [["root"]] }),
    ] {
        let mut body = json!({ "branch": b, "start": ["root"], "follow": follow, "item": "a" });
        body.as_object_mut()
            .unwrap()
            .extend(extra.as_object().unwrap().clone());
        let o = post_ok(&s.base, "/prove", body).await;
        assert_eq!(o["outcome"], json!("no_path"), "{extra}");
    }
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn branch_operations() {
    let s = plain().await;
    let main = make_branch(&s.base).await;
    post_ok(
        &s.base,
        "/write",
        json!({ "branch": main, "author": "u", "ops": [
        { "set": { "entity": "n", "key": "text", "value": "base" } } ] }),
    )
    .await;

    // fork
    let fork = post_ok(&s.base, "/fork", json!({ "from": main, "name": "agent" })).await["branch"]
        .as_str()
        .unwrap()
        .to_owned();
    post_ok(
        &s.base,
        "/write",
        json!({ "branch": fork, "author": "ag", "ops": [
        { "set": { "entity": "n", "key": "text", "value": "draft" } } ] }),
    )
    .await;
    assert_eq!(
        post_ok(
            &s.base,
            "/value",
            json!({ "branch": main, "entity": "n", "key": "text" })
        )
        .await,
        json!("base")
    );

    // merge agent into main -> main sees "draft"
    assert_eq!(
        post_ok(&s.base, "/merge", json!({ "into": main, "from": fork })).await,
        json!({})
    );
    assert_eq!(
        post_ok(
            &s.base,
            "/value",
            json!({ "branch": main, "entity": "n", "key": "text" })
        )
        .await,
        json!("draft")
    );

    // pull: a fresh branch pulls main's open range and sees it
    let other = post_ok(
        &s.base,
        "/create_branch",
        json!({ "name": "other", "parents": [] }),
    )
    .await["branch"]
        .as_str()
        .unwrap()
        .to_owned();
    assert_eq!(
        post_ok(
            &s.base,
            "/pull",
            json!({ "into": other, "from": main, "range": { "start": 1 } })
        )
        .await,
        json!({})
    );
    assert_eq!(
        post_ok(
            &s.base,
            "/value",
            json!({ "branch": other, "entity": "n", "key": "text" })
        )
        .await,
        json!("draft")
    );

    // set_parents: drop other's parents -> it sees nothing
    assert_eq!(
        post_ok(
            &s.base,
            "/set_parents",
            json!({ "branch": other, "parents": [] })
        )
        .await,
        json!({})
    );
    assert_eq!(
        post_ok(
            &s.base,
            "/value",
            json!({ "branch": other, "entity": "n", "key": "text" })
        )
        .await,
        Value::Null
    );

    // archive_branch
    assert_eq!(
        post_ok(
            &s.base,
            "/archive_branch",
            json!({ "branch": fork, "archived": true })
        )
        .await,
        json!({})
    );
    let info = post_ok(&s.base, "/branch", json!({ "branch": fork })).await;
    assert_eq!(info["archived"], true);
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn undo_ok_and_refused() {
    let s = plain().await;
    // ok: write then undo on a plain branch
    let b = make_branch(&s.base).await;
    post_ok(
        &s.base,
        "/write",
        json!({ "branch": b, "author": "u", "ops": [
        { "set": { "entity": "e", "key": "x", "value": 1 } } ] }),
    )
    .await;
    let out = post_ok(&s.base, "/undo", json!({ "branch": b, "author": "u" })).await;
    assert_eq!(out["removed"], json!([1, 1]));
    assert_eq!(
        post_ok(
            &s.base,
            "/value",
            json!({ "branch": b, "entity": "e", "key": "x" })
        )
        .await,
        Value::Null
    );
    // nothing left to undo -> null
    assert_eq!(
        post_ok(&s.base, "/undo", json!({ "branch": b, "author": "u" })).await,
        Value::Null
    );

    // refused 409: a fork freezes the batch, so undo is refused
    let c = make_branch(&s.base).await;
    post_ok(
        &s.base,
        "/write",
        json!({ "branch": c, "author": "u", "ops": [
        { "set": { "entity": "e", "key": "x", "value": 1 } } ] }),
    )
    .await;
    post_ok(&s.base, "/fork", json!({ "from": c, "name": "frozen" })).await;
    let r = post(&s.base, "/undo", json!({ "branch": c, "author": "u" })).await;
    assert_eq!(r.status().as_u16(), 409);
    assert_eq!(r.json::<Value>().await.unwrap()["error"], "undo_refused");
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn error_mapping() {
    let s = plain().await;
    let b = make_branch(&s.base).await;

    // bad JSON -> 400 invalid
    let r = Client::new()
        .post(format!("{}/write", s.base))
        .header("content-type", "application/json")
        .body("{ not json")
        .send()
        .await
        .unwrap();
    assert_eq!(r.status().as_u16(), 400);
    assert_eq!(r.json::<Value>().await.unwrap()["error"], "invalid");

    // bad uuid -> 400 invalid
    let r = post(&s.base, "/branch", json!({ "branch": "not-a-uuid" })).await;
    assert_eq!(r.status().as_u16(), 400);
    assert_eq!(r.json::<Value>().await.unwrap()["error"], "invalid");

    // unknown branch -> 404
    let r = post(
        &s.base,
        "/branch",
        json!({ "branch": "00000000-0000-0000-0000-000000000000" }),
    )
    .await;
    assert_eq!(r.status().as_u16(), 404);
    assert_eq!(r.json::<Value>().await.unwrap()["error"], "unknown_branch");

    // empty batch -> 400 invalid
    let r = post(
        &s.base,
        "/write",
        json!({ "branch": b, "author": "u", "ops": [] }),
    )
    .await;
    assert_eq!(r.status().as_u16(), 400);
    assert_eq!(r.json::<Value>().await.unwrap()["error"], "invalid");

    // archived branch -> 409
    post_ok(
        &s.base,
        "/archive_branch",
        json!({ "branch": b, "archived": true }),
    )
    .await;
    let r = post(
        &s.base,
        "/write",
        json!({ "branch": b, "author": "u", "ops": [
        { "set": { "entity": "e", "key": "x", "value": 1 } } ] }),
    )
    .await;
    assert_eq!(r.status().as_u16(), 409);
    assert_eq!(r.json::<Value>().await.unwrap()["error"], "branch_archived");
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn middleware_transform_over_http() {
    // Middleware stamps `seen_by` on every touched entity.
    let s = start(Config::default(), |store| {
        store.add_middleware(|ctx: &WriteContext, ops: &mut Vec<Op>| {
            let touched: Vec<String> = ops
                .iter()
                .filter_map(|op| match op {
                    Op::SetValue { entity, .. } => Some(entity.clone()),
                    _ => None,
                })
                .collect();
            for e in touched {
                ops.push(Op::set(e, "seen_by", ctx.author));
            }
            Ok(())
        });
    })
    .await;
    let b = make_branch(&s.base).await;
    post_ok(
        &s.base,
        "/write",
        json!({ "branch": b, "author": "mw", "ops": [
        { "set": { "entity": "e", "key": "x", "value": 1 } } ] }),
    )
    .await;
    assert_eq!(
        post_ok(
            &s.base,
            "/value",
            json!({ "branch": b, "entity": "e", "key": "seen_by" })
        )
        .await,
        json!("mw")
    );
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn middleware_reject_over_http() {
    let s = start(Config::default(), |store| {
        store.add_middleware(|_: &WriteContext, _: &mut Vec<Op>| Err(Error::Rejected("no".into())));
    })
    .await;
    let b = make_branch(&s.base).await;
    let r = post(
        &s.base,
        "/write",
        json!({ "branch": b, "author": "u", "ops": [
        { "set": { "entity": "e", "key": "x", "value": 1 } } ] }),
    )
    .await;
    assert_eq!(r.status().as_u16(), 400);
    assert_eq!(r.json::<Value>().await.unwrap()["error"], "rejected");
    // nothing written
    assert_eq!(
        post_ok(
            &s.base,
            "/value",
            json!({ "branch": b, "entity": "e", "key": "x" })
        )
        .await,
        Value::Null
    );
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn bearer_token() {
    let s = start(
        Config {
            token: Some("s3cret".into()),
        },
        |_| {},
    )
    .await;
    // health is open
    assert_eq!(get(&s.base, "/health").await.status().as_u16(), 200);
    // no token -> 401
    let r = get(&s.base, "/branches").await;
    assert_eq!(r.status().as_u16(), 401);
    // wrong token -> 401
    let r = Client::new()
        .get(format!("{}/branches", s.base))
        .bearer_auth("wrong")
        .send()
        .await
        .unwrap();
    assert_eq!(r.status().as_u16(), 401);
    // right token -> 200
    let r = Client::new()
        .get(format!("{}/branches", s.base))
        .bearer_auth("s3cret")
        .send()
        .await
        .unwrap();
    assert_eq!(r.status().as_u16(), 200);
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn value_round_trip() {
    let s = plain().await;
    let b = make_branch(&s.base).await;
    post_ok(
        &s.base,
        "/write",
        json!({ "branch": b, "author": "u", "ops": [
        { "set": { "entity": "e", "key": "n", "value": Value::Null } },
        { "set": { "entity": "e", "key": "bool", "value": true } },
        { "set": { "entity": "e", "key": "int", "value": 7 } },
        { "set": { "entity": "e", "key": "float", "value": 2.0 } },
        { "set": { "entity": "e", "key": "str", "value": "hi" } },
        { "set": { "entity": "e", "key": "arr", "value": [1, 2, 3] } },
        { "set": { "entity": "e", "key": "obj", "value": { "k": "v" } } }
    ] }),
    )
    .await;

    let get_v = |key: &'static str| {
        let base = s.base.clone();
        let branch = b.clone();
        async move {
            post_ok(
                &base,
                "/value",
                json!({ "branch": branch, "entity": "e", "key": key }),
            )
            .await
        }
    };

    assert_eq!(get_v("n").await, Value::Null); // null cleared -> absent
    assert_eq!(get_v("bool").await, json!(true));
    let i = get_v("int").await;
    assert!(i.is_i64());
    assert_eq!(i, json!(7));
    let f = get_v("float").await;
    assert!(f.is_f64(), "2.0 stays a float, got {f}");
    assert_eq!(f.as_f64().unwrap(), 2.0);
    assert_eq!(get_v("str").await, json!("hi"));
    assert_eq!(get_v("arr").await, json!([1, 2, 3]));
    assert_eq!(get_v("obj").await, json!({ "k": "v" }));
}
