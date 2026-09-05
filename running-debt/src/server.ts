/**
 * The server. The page, the same figures as JSON for anything that would rather
 * read them itself, and the two things the page can write back: a new event, and
 * a Sunday let off.
 */

import { createServer } from "node:http";
import type { IncomingMessage, ServerResponse } from "node:http";
import type { DatabaseSync } from "node:sqlite";
import { balance, maintenance, steps, type EventKind } from "./debt.ts";
import { add, events, forgive, forgiven } from "./db.ts";
import { page, type View } from "./page.ts";
import { instantOf } from "./time.ts";

const KINDS: EventKind[] = ["penalty", "run", "cycle"];

function view(db: DatabaseSync): View {
  const now = Date.now();
  const history = events(db);
  const letOff = forgiven(db);
  const debt = balance(history, now, letOff);
  return { now, debt, maintenance: maintenance(debt), steps: steps(history, now, letOff) };
}

async function body(request: IncomingMessage): Promise<Record<string, unknown>> {
  const chunks: Buffer[] = [];
  let size = 0;
  for await (const chunk of request) {
    size += chunk.length;
    if (size > 64 * 1024) throw new Error("That is too much to send");
    chunks.push(chunk as Buffer);
  }
  const parsed: unknown = JSON.parse(Buffer.concat(chunks).toString() || "{}");
  if (typeof parsed !== "object" || parsed === null) throw new Error("Expected an object");
  return parsed as Record<string, unknown>;
}

/** A wall time in UK time, as the page writes it: `2026-09-04T18:30`. */
function moment(written: unknown): number {
  if (typeof written !== "string") throw new Error("Expected a date and a time");
  const found = /^(\d{4})-(\d{2})-(\d{2})[T ](\d{2}):(\d{2})/.exec(written);
  if (!found) throw new Error(`Cannot read "${written}" as a date`);
  const [, year, month, day, hour, minute] = found;
  return instantOf(Number(year), Number(month), Number(day), Number(hour), Number(minute));
}

function write(db: DatabaseSync, path: string, sent: Record<string, unknown>): void {
  if (path === "/events") {
    const kind = sent.kind;
    if (typeof kind !== "string" || !KINDS.includes(kind as EventKind)) {
      throw new Error(`No such kind of event: ${String(kind)}`);
    }
    const km = kind === "penalty" ? 0 : Number(sent.km);
    if (kind !== "penalty" && (!Number.isFinite(km) || km <= 0)) {
      throw new Error(`Cannot read "${String(sent.km)}" as a distance`);
    }
    add(db, kind as EventKind, km, moment(sent.at));
  } else if (path === "/forgive") {
    const at = Number(sent.at);
    if (!Number.isFinite(at)) throw new Error("Expected the time of a Sunday");
    forgive(db, at, sent.forgiven === true);
  } else {
    throw new Error("Nothing here");
  }
}

export function serve(db: DatabaseSync, port: number): Promise<string> {
  const server = createServer((request, response) => {
    const path = new URL(request.url ?? "/", "http://localhost").pathname;
    const send = (status: number, type: string, text: string) => {
      response.writeHead(status, { "content-type": type });
      response.end(text);
    };

    if (request.method === "POST") {
      answer(request, response, db, path, send);
    } else if (path === "/debt.json") {
      send(200, "application/json", JSON.stringify(view(db), null, 2));
    } else if (path === "/") {
      send(200, "text/html; charset=utf-8", page(view(db)));
    } else {
      send(404, "text/plain", "Nothing here\n");
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

function answer(
  request: IncomingMessage,
  response: ServerResponse,
  db: DatabaseSync,
  path: string,
  send: (status: number, type: string, text: string) => void,
): void {
  body(request)
    .then((sent) => {
      write(db, path, sent);
      send(200, "application/json", JSON.stringify(view(db)));
    })
    .catch((error: unknown) => {
      send(400, "application/json", JSON.stringify({
        error: error instanceof Error ? error.message : String(error),
      }));
    });
  response.on("error", () => {});
}
