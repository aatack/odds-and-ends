/**
 * The server. Two routes: the page, and the same figures as JSON for anything
 * that would rather read them itself.
 */

import { createServer } from "node:http";
import type { DatabaseSync } from "node:sqlite";
import { balance, steps } from "./debt.ts";
import { events } from "./db.ts";
import { page } from "./page.ts";

export function serve(db: DatabaseSync, port: number): Promise<string> {
  const server = createServer((request, response) => {
    const now = Date.now();
    const history = events(db);
    const view = { now, debt: balance(history, now), steps: steps(history, now) };
    const path = new URL(request.url ?? "/", "http://localhost").pathname;

    if (path === "/debt.json") {
      response.writeHead(200, { "content-type": "application/json" });
      response.end(JSON.stringify({ ...view, events: history }, null, 2));
    } else if (path === "/") {
      response.writeHead(200, { "content-type": "text/html; charset=utf-8" });
      response.end(page(view));
    } else {
      response.writeHead(404, { "content-type": "text/plain" });
      response.end("Nothing here\n");
    }
  });

  return new Promise((resolve, reject) => {
    server.once("error", reject);
    server.listen(port, "127.0.0.1", () => {
      const address = server.address();
      resolve(`http://127.0.0.1:${typeof address === "object" && address ? address.port : port}`);
    });
  });
}
