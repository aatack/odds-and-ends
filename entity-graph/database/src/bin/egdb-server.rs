//! A small HTTP server over an egdb store. Behind the `server` feature.
//!
//! ```text
//! egdb-server <db-path> [--addr 127.0.0.1:7878] [--token T]
//! ```
//!
//! Fallbacks from the environment: `EGDB_DB`, `EGDB_ADDR`, `EGDB_TOKEN`.

use std::net::SocketAddr;
use std::process::ExitCode;

use egdb::server::{self, Config};
use egdb::Store;

const DEFAULT_ADDR: &str = "127.0.0.1:7878";

const HELP: &str = "\
egdb-server: a minimal REST API over an egdb store.

Usage:
    egdb-server <db-path> [--addr <host:port>] [--token <token>]

Arguments:
    <db-path>        SQLite file for the store (or set EGDB_DB).

Options:
    --addr <addr>    Bind address (default 127.0.0.1:7878, or EGDB_ADDR).
    --token <token>  Require 'Authorization: Bearer <token>' (or EGDB_TOKEN).
    -h, --help       Show this help.
";

#[tokio::main]
async fn main() -> ExitCode {
    match run().await {
        Ok(()) => ExitCode::SUCCESS,
        Err(message) => {
            eprintln!("error: {message}");
            ExitCode::FAILURE
        }
    }
}

async fn run() -> Result<(), String> {
    let mut args = std::env::args().skip(1);
    let mut db: Option<String> = None;
    let mut addr: Option<String> = None;
    let mut token: Option<String> = None;
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "-h" | "--help" => {
                print!("{HELP}");
                return Ok(());
            }
            "--addr" => addr = Some(args.next().ok_or("--addr needs a value")?),
            "--token" => token = Some(args.next().ok_or("--token needs a value")?),
            flag if flag.starts_with("--") => return Err(format!("unknown flag {flag}")),
            path if db.is_none() => db = Some(path.to_owned()),
            _ => return Err("only one db path is allowed".into()),
        }
    }

    let db = db
        .or_else(|| std::env::var("EGDB_DB").ok())
        .ok_or("a db path is required (argument or EGDB_DB)")?;
    let addr = addr
        .or_else(|| std::env::var("EGDB_ADDR").ok())
        .unwrap_or_else(|| DEFAULT_ADDR.to_owned());
    let token = token.or_else(|| std::env::var("EGDB_TOKEN").ok());

    let addr: SocketAddr = addr.parse().map_err(|_| format!("bad address {addr:?}"))?;
    let store = Store::open(&db).map_err(|e| format!("cannot open {db:?}: {e}"))?;
    let listener = server::bind(addr)
        .await
        .map_err(|e| format!("cannot bind {addr}: {e}"))?;
    let bound = listener.local_addr().map_err(|e| e.to_string())?;
    println!("egdb-server listening on http://{bound}");
    if token.is_some() {
        println!("a bearer token is required");
    }
    println!("press Ctrl+C to stop");

    server::serve(store, listener, Config { token })
        .await
        .map_err(|e| e.to_string())
}
