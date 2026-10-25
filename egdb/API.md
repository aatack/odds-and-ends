# egdb HTTP API

A thin REST wrapper over the `egdb` store. JSON goes in and JSON comes out.
There is one endpoint per store operation and no business logic in the server.
The API is behind the `server` cargo feature, so the library has no web
dependencies by default.

## Run the server

```sh
cargo run --release --features server --bin egdb-server -- notes.db [--addr 127.0.0.1:7878] [--token T]
```

- The first argument is the database file. It is made if it does not exist.
- `--addr` sets the bind address. The default is `127.0.0.1:7878` (loopback).
- `--token` sets a bearer token (see [Auth](#auth)).

Environment variables are fallbacks for the arguments: `EGDB_DB`, `EGDB_ADDR`
and `EGDB_TOKEN`. The server stops on Ctrl+C.

The examples below use two shell variables:

```sh
URL=http://127.0.0.1:7878
B=$(curl -s -X POST $URL/create_branch -H 'content-type: application/json' \
      -d '{"name":"main","parents":[]}' | sed 's/.*"branch":"\([^"]*\)".*/\1/')
```

## Embed the server

An embedder opens a `Store`, registers any write middleware on it, and then
builds a router or serves it. Middleware must be registered on the `Store`
before it is passed in.

```rust
use egdb::server::{self, Config};
use egdb::{Error, Op, Store, WriteContext};

#[tokio::main]
async fn main() -> std::io::Result<()> {
    let mut store = Store::open("notes.db").unwrap();
    // Reject writes to the reserved author.
    store.add_middleware(|ctx: &WriteContext, _ops: &mut Vec<Op>| {
        if ctx.author == "system" {
            return Err(Error::Rejected("the system author is reserved".into()));
        }
        Ok(())
    });

    let addr = "127.0.0.1:7878".parse().unwrap();
    let listener = server::bind(addr).await?;
    server::serve(store, listener, Config::default()).await
}
```

The public items are in `egdb::server`:

- `router(store, config) -> axum::Router` wraps the store in an `Arc<Mutex<..>>`.
- `router_with(shared, config)` takes an `Arc<Mutex<Store>>` the embedder keeps.
- `bind(addr) -> TcpListener` and `serve(store, listener, config)` run it.
- `Config { token: Option<String> }`.

The store is not thread safe, so all calls run behind one mutex, one at a time.

## Auth

When `--token` (or `Config.token`) is set, every request must send
`Authorization: Bearer <token>`. A missing or wrong token gets `401`. `/health`
is always open.

```sh
curl -s $URL/branches -H "Authorization: Bearer $EGDB_TOKEN"
```

## Errors

Every error is a JSON body with a kind and a message:

```json
{ "error": "unknown_branch", "message": "no branch 0191..." }
```

| kind | status | when |
| --- | ---: | --- |
| `invalid` | 400 | bad JSON, bad UUID, or an invalid op (empty id, non-finite number, empty batch) |
| `rejected` | 400 | a write middleware rejected the batch |
| `unauthorized` | 401 | missing or wrong bearer token |
| `unknown_branch` | 404 | no branch with that ID |
| `branch_archived` | 409 | the branch is archived and refuses writes |
| `undo_refused` | 409 | undo cannot run (a later event, or a frozen follower) |
| `schema`, `corrupt`, `internal`, `not_implemented` | 500 | other errors |

## Values as JSON

| `Value` | JSON |
| --- | --- |
| `Null` | `null` |
| `Bool` | `true` / `false` |
| `Int` | an integer, for example `7` |
| `Float` | a non-integer number; `2.0` stays a float |
| `Text` | a string |
| `Json` | an array or an object |

When JSON comes in, an integer becomes `Int` and any other number (for example
`2.0`) becomes `Float`. An array or an object becomes `Json`. A `Null` value in
a write clears the key, so a later read does not return it.

## Not exposed

- `Filter::Custom` and any closure predicate. A `walk` can take `descend` and
  `include` only as filters (see [`/walk`](#post-walk)), not as free closures.
- All store calls are serialised behind one mutex; there is no parallel read.

---

## Endpoints

### GET /health

```sh
curl -s $URL/health
```

```json
{ "ok": true }
```

### POST /write

Request: `{branch, author, timestamp?, ops}`. An op is `{"set": {entity, key,
value}}` or `{"link": {source, destination, link_type, sort?}}`. `sort` is
`"timestamp"` (the default), `{"explicit": <number>}` or `"removed"`.

```sh
curl -s -X POST $URL/write -H 'content-type: application/json' -d '{
  "branch":"'$B'","author":"u","ops":[
    {"set":{"entity":"a","key":"type","value":"task"}},
    {"set":{"entity":"b","key":"type","value":"task"}},
    {"link":{"source":"a","destination":"b","link_type":"child","sort":{"explicit":1.5}}}
  ]}'
```

```json
{ "branch": "...", "first_seq": 1, "last_seq": 3, "timestamp": 1700000000000 }
```

A batch that middleware drops to empty returns an empty range (`first_seq =
last_seq + 1`).

### POST /archive_entity

Request: `{branch, author, entity}`. Response: a write result.

```sh
curl -s -X POST $URL/archive_entity -H 'content-type: application/json' \
  -d '{"branch":"'$B'","author":"u","entity":"a"}'
```

### POST /restore_entity

Request: `{branch, author, entity, entity_type}`. Response: a write result.

```sh
curl -s -X POST $URL/restore_entity -H 'content-type: application/json' \
  -d '{"branch":"'$B'","author":"u","entity":"a","entity_type":"task"}'
```

### POST /undo

Request: `{branch, author}`. Response: the undo outcome, or `null` when there
is nothing to undo.

```sh
curl -s -X POST $URL/undo -H 'content-type: application/json' \
  -d '{"branch":"'$B'","author":"u"}'
```

```json
{ "removed": [1, 2], "events": 2, "restored": 0 }
```

### POST /values

Request: `{branch, entities}`. Response: a map of entity ID to its values.

```sh
curl -s -X POST $URL/values -H 'content-type: application/json' \
  -d '{"branch":"'$B'","entities":["a"]}'
```

```json
{ "a": { "type": "task" } }
```

### POST /value

Request: `{branch, entity, key}`. Response: the value, or `null`.

```sh
curl -s -X POST $URL/value -H 'content-type: application/json' \
  -d '{"branch":"'$B'","entity":"a","key":"type"}'
```

```json
"task"
```

### POST /links

Request: `{branch, entities, groups}`. A group is a `[link_type, direction]`
pair; direction is `"outbound"` or `"inbound"`. Response: a map of entity ID to
an array of groups, each with its links. A JSON object cannot key on the
`(type, direction)` pair, so the groups are an array.

```sh
curl -s -X POST $URL/links -H 'content-type: application/json' \
  -d '{"branch":"'$B'","entities":["a"],"groups":[["child","outbound"]]}'
```

```json
{
  "a": [
    { "link_type": "child", "direction": "outbound", "links": [
      { "source": "a", "destination": "b", "link_type": "child", "sort": 1.5,
        "event": { "branch": "...", "seq": 3, "timestamp": 1700000000000, "author": "u" } }
    ] }
  ]
}
```

### POST /links_including_archived

The same as [`/links`](#post-links), but it does not hide links to archived
entities (those with a null type).

```sh
curl -s -X POST $URL/links_including_archived -H 'content-type: application/json' \
  -d '{"branch":"'$B'","entities":["a"],"groups":[["child","outbound"]]}'
```

### POST /children

Request: `{branch, entity, link_type}`. Response: the outbound links of one
type, as an array.

```sh
curl -s -X POST $URL/children -H 'content-type: application/json' \
  -d '{"branch":"'$B'","entity":"a","link_type":"child"}'
```

### POST /parents

The same as [`/children`](#post-children) but for inbound links.

```sh
curl -s -X POST $URL/parents -H 'content-type: application/json' \
  -d '{"branch":"'$B'","entity":"b","link_type":"child"}'
```

### POST /exists

Request: `{branch, entities}`. Response: a map of entity ID to a boolean.

```sh
curl -s -X POST $URL/exists -H 'content-type: application/json' \
  -d '{"branch":"'$B'","entities":["a","zzz"]}'
```

```json
{ "a": true, "zzz": false }
```

### POST /is_archived

Request: `{branch, entities}`. Response: a map of entity ID to a boolean.

```sh
curl -s -X POST $URL/is_archived -H 'content-type: application/json' \
  -d '{"branch":"'$B'","entities":["a"]}'
```

### POST /find

Request: `{branch, filter, options?}`. A filter is one of:

- `{"eq": {key, value}}`, and `ne`, `lt`, `le`, `gt`, `ge` with the same shape;
- `{"contains": {key, value}}`, `contains_ignore_case`, `prefix` (text);
- `{"in": {key, values}}`;
- `{"is_null": {key}}`, `{"not_null": {key}}`;
- `{"and": [..]}`, `{"or": [..]}`, `{"not": <filter>}`.

`options` is `{limit?, cursor?, order?, values?, include_archived?,
scan_limit?}`. `order` is `"entity_id"` (the default), `"insertion"`,
`{"value_asc": key}` or `{"value_desc": key}`. Each absent field uses the
library default. Response: `{rows, next, scanned}`; `next` is the cursor of the
next page, or `null`.

```sh
curl -s -X POST $URL/find -H 'content-type: application/json' \
  -d '{"branch":"'$B'","filter":{"eq":{"key":"type","value":"task"}},"options":{"limit":100}}'
```

```json
{ "rows": [ { "id": "a", "values": { "type": "task" } } ], "next": null, "scanned": 1 }
```

### POST /walk

Request: `{branch, start, follow, descend?, include?, collapsed?, options?}`.

- `start` is a path; its last ID is the first node to visit.
- `follow` maps an entity's type to the `[link_type, direction, child_type]`
  rules to follow. A rule follows the links of that type and direction to
  entities whose `type` is `child_type` only. From a system, follow outbound
  `contains` links to requirements and outbound `verified by` links to tests:
  `{ "system": [["contains", "outbound", "requirement"], ["verified by", "outbound", "test"]] }`.
  Children come link group by link group, in the order the rules first name
  each `(link_type, direction)`, in link order within a group.
- `descend` and `include` are filters (the same shape as [`/find`](#post-find)),
  tested on each node's id and values. Absent means always true. `include`
  decides if a node is in the result; `descend` decides if the walk goes below
  it.
- `collapsed` is a list of paths (each from `start[0]`) that the walk does not
  go below, as if `descend` said no for them. A client uses it for the rows
  that the user has folded.
- `options` is `{steps?, read_ahead?: {depth?, width?}, visited?,
  direction?}`. `direction` is `"forward"` (the default) or
  `"backward"`: a backward walk visits the same nodes in reverse order, from
  `start` back to `start[0]`, nearest first. It cannot use `visited`.

Response: `{paths, cache, next, steps, loads}`, plus `visited` when a visited
set is used. `paths` are the paths of the emitted nodes, in visiting order.
`cache` is `{nodes, links}` for the entities on those paths only: `nodes` maps
each ID to `{id, values}`, and `links` maps each ID whose type follows rules
to its link groups (every live link of each followed `(link_type, direction)`,
as from `/links`). `next` is the path to resume from, or `null`.

```sh
curl -s -X POST $URL/walk -H 'content-type: application/json' -d '{
  "branch":"'$B'","start":["a"],
  "follow":{"task":[["child","outbound","task"]]},
  "include":{"eq":{"key":"type","value":"task"}}}'
```

```json
{ "paths": [ ["a"] ],
  "cache": { "nodes": { "a": { "id": "a", "values": { "type": "task" } } },
             "links": { "a": [ { "link_type": "child", "direction": "outbound", "links": [] } ] } },
  "next": null, "steps": 1, "loads": 1 }
```

### POST /prove

Request: `{branch, item, roots, relationships, options?}`. `relationships` maps
a type to the `[link_type, direction]` pairs that an entity of that type steps
down, to a child of any type: `{ "list": [["child", "outbound"]] }`. `options` is `{max_depth?, max_nodes?,
include_archived?, strategy?}`; `strategy` is `"backward"` (the default),
`"bidirectional"` or `"walk"`. Response: `{outcome, stats}`. `outcome` is
`{"proved": {path, links}}`, `"no_path"` or `"unknown"`.

```sh
curl -s -X POST $URL/prove -H 'content-type: application/json' -d '{
  "branch":"'$B'","item":"b","roots":["a"],
  "relationships":{"task":[["child","outbound"]]}}'
```

```json
{ "outcome": { "proved": { "path": ["a", "b"], "links": [ ... ] } },
  "stats": { "nodes_visited": 1, "link_reads": 1, "value_reads": 2, "sql_queries": 3, "depth_reached": 1 } }
```

### GET /branches

```sh
curl -s $URL/branches
```

```json
[ { "id": "...", "name": "main", "archived": false, "head": 2, "parents": [], "created_at": 1700000000000 } ]
```

### POST /branch

Request: `{branch}`. Response: one branch.

```sh
curl -s -X POST $URL/branch -H 'content-type: application/json' -d '{"branch":"'$B'"}'
```

### POST /create_branch

Request: `{name, parents}`. A parent is `{branch, range: {start, end?}}`; an
absent or null `end` is an open range. Response: `{branch}`.

```sh
curl -s -X POST $URL/create_branch -H 'content-type: application/json' \
  -d '{"name":"main","parents":[]}'
```

```json
{ "branch": "0191..." }
```

### POST /fork

Request: `{from, name}`. Response: `{branch}`.

```sh
curl -s -X POST $URL/fork -H 'content-type: application/json' \
  -d '{"from":"'$B'","name":"agent"}'
```

### POST /pull

Request: `{into, from, range: {start, end?}}`. Response: `{}`.

```sh
curl -s -X POST $URL/pull -H 'content-type: application/json' \
  -d '{"into":"'$B'","from":"'$B'","range":{"start":1}}'
```

### POST /merge

Request: `{into, from}`. Response: `{}`.

```sh
curl -s -X POST $URL/merge -H 'content-type: application/json' \
  -d '{"into":"'$B'","from":"'$B'"}'
```

### POST /set_parents

Request: `{branch, parents}` (parents as in [`/create_branch`](#post-create_branch)).
Response: `{}`.

```sh
curl -s -X POST $URL/set_parents -H 'content-type: application/json' \
  -d '{"branch":"'$B'","parents":[]}'
```

### POST /archive_branch

Request: `{branch, archived}`. `archived` true archives the branch; false
restores it. Response: `{}`.

```sh
curl -s -X POST $URL/archive_branch -H 'content-type: application/json' \
  -d '{"branch":"'$B'","archived":true}'
```
