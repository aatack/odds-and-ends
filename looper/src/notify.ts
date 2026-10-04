// The tool the looping agent uses to reach you: a tiny MCP server over stdio,
// handed to Claude with `--mcp-config`, exposing `tell_user` and `ask_user`.
//
// It is a separate process from the loop, spawned and killed by Claude itself, so
// it reports what it sent by appending to `.looper/outbox.jsonl`. The loop reads
// that file when the wake ends: an `ask` is what makes it wait for your reply
// rather than pressing straight on.
//
// The protocol is small enough to speak by hand — initialize, tools/list,
// tools/call — which keeps Looper free of dependencies.

import { appendFileSync, existsSync, statSync } from "node:fs";
import { join, relative, resolve, isAbsolute } from "node:path";
import { createInterface } from "node:readline";
import { Telegram } from "./telegram.ts";

const stateDir = process.env.LOOPER_STATE_DIR;
const repo = process.env.LOOPER_REPO;
const token = process.env.TELEGRAM_BOT_TOKEN;
const chatId = process.env.TELEGRAM_CHAT_ID;

if (!stateDir || !repo || !token || !chatId) {
  console.error(
    "looper notify: LOOPER_STATE_DIR, LOOPER_REPO, TELEGRAM_BOT_TOKEN and " +
      "TELEGRAM_CHAT_ID must all be set. This server is meant to be launched by " +
      "Looper, not by hand."
  );
  process.exit(1);
}

const telegram = new Telegram(token, chatId);

/** Telegram's own ceiling on an upload, minus a little room. */
const maxFileBytes = 45 * 1024 * 1024;

const tools = [
  {
    name: "tell_user",
    description:
      "Send a Telegram message to the user. Use this tool to give the user data " +
      "that they want to know. Examples are a result that they asked for, or a " +
      "decision that changes the work. Possibly the user does not reply. Do not " +
      "wait for a reply. Do not use this tool frequently. Use it a maximum of a " +
      "few times each day. You can attach files from the working repository. " +
      "Images show in the message.",
    inputSchema: {
      type: "object",
      properties: {
        message: {
          type: "string",
          description: "The message, as plain text, in ASD-STE100 Simplified Technical English.",
        },
        files: {
          type: "array",
          items: { type: "string" },
          description: "Optional paths inside the working repo to attach.",
        },
      },
      required: ["message"],
      additionalProperties: false,
    },
  },
  {
    name: "ask_user",
    description:
      "Ask the user a question. Use this tool only when you cannot continue " +
      "without the answer. Examples are a decision that only the user can make, " +
      "a credential that you do not have, or a decision about the direction of " +
      "the work. Looper sends the question to Telegram. The answer comes to you " +
      "in a later prompt. After you ask, write in the notes where you stopped. " +
      "Then stop. Do not use this tool to ask for permission to continue. You " +
      "make the decisions.",
    inputSchema: {
      type: "object",
      properties: {
        question: {
          type: "string",
          description: "The question, as plain text, in ASD-STE100 Simplified Technical English.",
        },
        files: {
          type: "array",
          items: { type: "string" },
          description: "Optional paths inside the working repo to attach.",
        },
      },
      required: ["question"],
      additionalProperties: false,
    },
  },
];

/**
 * Resolve an attachment against the repo, refusing anything outside it. The
 * agent is told to stay inside its repo; this is where that is actually enforced,
 * because a file path is the one place it could reach out of it.
 */
function resolveAttachment(path: string): string {
  const absolute = isAbsolute(path) ? resolve(path) : resolve(repo!, path);
  const within = relative(repo!, absolute);
  if (within.startsWith("..") || isAbsolute(within)) {
    throw new Error(
      `${path} is outside the working repository. You can send only files in the repository.`
    );
  }
  if (!existsSync(absolute)) throw new Error(`${path} does not exist.`);
  const size = statSync(absolute).size;
  if (size > maxFileBytes) {
    throw new Error(`${path} is ${Math.round(size / 1e6)} MB. This is too large for Telegram.`);
  }
  return absolute;
}

async function deliver(kind: "tell" | "ask", text: string, files: string[]): Promise<string> {
  const attachments = files.map(resolveAttachment);
  await telegram.send(text);
  for (const file of attachments) await telegram.sendFile(file);
  appendFileSync(
    join(stateDir!, "outbox.jsonl"),
    JSON.stringify({ at: new Date().toISOString(), kind, text, files: attachments }) + "\n"
  );
  return kind === "ask"
    ? "Looper sent the question. The answer will come in a later prompt. " +
        "Write in the notes where you stopped. Then stop now."
    : `Looper sent the message${attachments.length ? ` with ${attachments.length} attachment(s)` : ""}.`;
}

// ---------------------------------------------------------------------------
// the protocol

interface Request {
  jsonrpc: "2.0";
  id?: number | string;
  method: string;
  params?: Record<string, unknown>;
}

function write(message: unknown): void {
  process.stdout.write(JSON.stringify(message) + "\n");
}

function respond(id: number | string, result: unknown): void {
  write({ jsonrpc: "2.0", id, result });
}

function fail(id: number | string, code: number, message: string): void {
  write({ jsonrpc: "2.0", id, error: { code, message } });
}

async function handle(request: Request): Promise<void> {
  const { id, method, params } = request;
  // Notifications carry no id and take no reply.
  if (id === undefined) return;

  if (method === "initialize") {
    const asked = params?.protocolVersion;
    respond(id, {
      protocolVersion: typeof asked === "string" ? asked : "2025-06-18",
      capabilities: { tools: {} },
      serverInfo: { name: "looper", version: "0.1.0" },
    });
    return;
  }
  if (method === "ping") {
    respond(id, {});
    return;
  }
  if (method === "tools/list") {
    respond(id, { tools });
    return;
  }
  if (method === "tools/call") {
    const name = params?.name;
    const args = (params?.arguments ?? {}) as Record<string, unknown>;
    const files = Array.isArray(args.files) ? (args.files as string[]).map(String) : [];
    try {
      if (name === "tell_user") {
        const message = String(args.message ?? "").trim();
        if (!message) throw new Error("The message is empty. Write a message.");
        respond(id, text(await deliver("tell", message, files)));
        return;
      }
      if (name === "ask_user") {
        const question = String(args.question ?? "").trim();
        if (!question) throw new Error("The question is empty. Write a question.");
        respond(id, text(await deliver("ask", question, files)));
        return;
      }
      fail(id, -32602, `There is no tool with the name ${String(name)}.`);
    } catch (error) {
      // Reported as a tool result rather than a protocol error, so the agent sees
      // what went wrong and can fix its own call.
      respond(id, { ...text(`Error: ${(error as Error).message}`), isError: true });
    }
    return;
  }
  fail(id, -32601, `Unknown method: ${method}`);
}

function text(value: string) {
  return { content: [{ type: "text", text: value }] };
}

const lines = createInterface({ input: process.stdin });
lines.on("line", (line) => {
  if (!line.trim()) return;
  let request: Request;
  try {
    request = JSON.parse(line) as Request;
  } catch {
    write({ jsonrpc: "2.0", id: null, error: { code: -32700, message: "Parse error" } });
    return;
  }
  void handle(request).catch((error: unknown) => {
    if (request.id !== undefined) fail(request.id, -32603, (error as Error).message);
  });
});
