# egdb demo

An outline over an egdb server, drawn with `egdb-client/react` and TanStack
Virtual. It shows the client's paradigm (see `../client/PARADIGM.md`): rows
are calculated in the browser, data loads in batches, and writes show at once.

```sh
# 1. the server (from egdb/)
cargo run --release --features server --bin egdb-server -- demo.db

# 2. the demo (from egdb/demo/); this also builds ../client
npm install
npm run seed          # ~12k entities: node seed.mjs [url] [depth] [fanout]
npm run dev           # proxies /api to EGDB_URL (default http://127.0.0.1:7878)
```

The demo runs the client from its source (`../client/src`, by a Vite alias),
so after a `git pull` there is nothing to rebuild.

Try:

- Scroll far down. The footer shows the anchor (the first row on screen) and
  how many rows are calculated: always a few dozen.
- Click ▾ to fold a row: no request is sent.
- Type in the box to filter the rows. The table stays at the same place in the
  tree. Clear the box to go back.
- Double-click a row's text to edit it; ＋ adds a child, × archives the row.
  Each shows before the server answers.
- The right panel is `useFind`: tasks whose text contains the filter.
