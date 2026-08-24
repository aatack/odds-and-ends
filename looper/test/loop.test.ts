// One wake, end to end, with both sides faked: a stand-in Bot API on localhost,
// and a `claude` earlier on PATH that prints a canned stream of events and calls
// the notify tool by writing to the outbox, exactly as the real tool does.
//
// What is being tested is the part that is hard to check by reading: that a wake
// is built, run, read and recorded, that asking a question is noticed, and that a
// message sent while the loop is up lands in the next prompt.

import { test } from "node:test";
import assert from "node:assert/strict";
import { execFileSync, spawn } from "node:child_process";
import { createServer } from "node:http";
import type { Server } from "node:http";
import { mkdtempSync, mkdirSync, writeFileSync, readFileSync, chmodSync } from "node:fs";
import { tmpdir } from "node:os";
import { join, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { readCap } from "../src/claude.ts";

const looperDir = join(dirname(fileURLToPath(import.meta.url)), "..");

/**
 * Run Looper as a child and wait for it. It has to be awaited rather than run
 * synchronously: the fake Bot API lives in this process, so blocking here would
 * stop it answering the very calls being tested.
 */
function runLooper(
  repo: string,
  env: Record<string, string>
): Promise<{ status: number | null; stderr: string }> {
  return new Promise((settle) => {
    const child = spawn(
      process.execPath,
      [join(looperDir, "src", "index.ts"), "--once", "--repo", repo],
      { env: { ...process.env, ...env } }
    );
    let stderr = "";
    child.stderr.setEncoding("utf8");
    child.stderr.on("data", (chunk: string) => (stderr += chunk));
    child.stdout.resume();
    child.on("close", (status) => settle({ status, stderr }));
  });
}

/** A Bot API that answers the four calls Looper makes, and remembers what it was sent. */
function fakeTelegram(updates: unknown[]): Promise<{ server: Server; sent: string[]; url: string }> {
  const sent: string[] = [];
  let pending = updates;
  const server = createServer((request, response) => {
    let body = "";
    request.on("data", (chunk) => (body += chunk));
    request.on("end", () => {
      const method = (request.url ?? "").split("/").pop();
      const reply = (result: unknown) => {
        response.writeHead(200, { "content-type": "application/json" });
        response.end(JSON.stringify({ ok: true, result }));
      };
      if (method === "getMe") return reply({ username: "looperbot" });
      if (method === "sendMessage") {
        sent.push((JSON.parse(body) as { text: string }).text);
        return reply({ message_id: sent.length });
      }
      if (method === "getUpdates") {
        // Hand the queued updates over once, then go quiet like a real long poll.
        const result = pending;
        pending = [];
        return reply(result);
      }
      reply({});
    });
  });
  return new Promise((settle) => {
    server.listen(0, "127.0.0.1", () => {
      const address = server.address();
      const port = typeof address === "object" && address ? address.port : 0;
      settle({ server, sent, url: `http://127.0.0.1:${port}` });
    });
  });
}

/**
 * A `claude` that behaves like the real one for one wake: it saves the prompt and
 * the arguments it was given, says it asked the user something by writing to the
 * outbox, and prints the events Looper reads. `dies` instead makes it fall over
 * saying nothing, the way a session that cannot be resumed does, and `overloaded`
 * prints what it prints when it has given up retrying a 529 — note the
 * `subtype: "success"` alongside `is_error`, which is what the real one does.
 * `limited` prints what a spent session cap looks like: a 429 whose text says
 * when it lifts and never says "limit" in any of the words a cap is usually found
 * by.
 */
type Behaviour = "asks" | "dies" | "overloaded" | "limited";

function fakeClaude(dir: string, repo: string, behaviour: Behaviour = "asks"): string {
  const bin = join(dir, "bin");
  mkdirSync(bin, { recursive: true });
  const body =
    behaviour === "dies"
      ? `echo "No conversation found with session ID: old" >&2
exit 1
`
      : behaviour === "overloaded"
        ? `echo '{"type":"system","subtype":"init","session_id":"11111111-2222-3333-4444-555555555555"}'
echo '{"type":"result","subtype":"success","is_error":true,"session_id":"11111111-2222-3333-4444-555555555555","result":"API Error: 529 Overloaded. This is a server-side issue, usually temporary.","total_cost_usd":0.0018,"num_turns":1,"api_error_status":529,"terminal_reason":"api_error"}'
`
        : behaviour === "limited"
          ? `echo '{"type":"system","subtype":"init","session_id":"11111111-2222-3333-4444-555555555555"}'
echo '{"type":"result","subtype":"success","is_error":true,"session_id":"11111111-2222-3333-4444-555555555555","result":"You'"'"'ve hit your session limit \u00b7 resets 9:50am (America/Los_Angeles)","num_turns":1,"api_error_status":429,"terminal_reason":"api_error"}'
`
        : `printf '%s\\n' '{"at":"2026-01-01T00:00:00.000Z","kind":"ask","text":"which way?"}' \\
  >> ${JSON.stringify(join(repo, ".looper", "outbox.jsonl"))}
echo '{"type":"system","subtype":"init","session_id":"11111111-2222-3333-4444-555555555555"}'
echo '{"type":"assistant","session_id":"11111111-2222-3333-4444-555555555555","message":{"content":[{"type":"tool_use","name":"Bash","input":{"command":"git status"}}]}}'
echo '{"type":"result","subtype":"success","is_error":false,"session_id":"11111111-2222-3333-4444-555555555555","result":"asked and stopped","total_cost_usd":0.01,"num_turns":2}'
`;
  const script = `#!/bin/sh
# The account check comes before any wake, and takes no prompt.
if [ "$1" = "auth" ]; then
  echo '{"loggedIn":true,"authMethod":"claude.ai","email":"looper@example.com","subscriptionType":"max"}'
  exit 0
fi
cat > ${JSON.stringify(join(dir, "prompt.txt"))}
printf '%s\\n' "$@" > ${JSON.stringify(join(dir, "args.txt"))}
${body}`;
  writeFileSync(join(bin, "claude"), script);
  chmodSync(join(bin, "claude"), 0o755);
  return bin;
}

test("a wake is run, read and recorded", async () => {
  const dir = mkdtempSync(join(tmpdir(), "looper-test-"));
  const repo = join(dir, "repo");
  mkdirSync(join(repo, ".looper"), { recursive: true });
  execFileSync("git", ["init", "-q"], { cwd: repo });

  const telegram = await fakeTelegram([
    { update_id: 7, message: { text: "try the other one", date: 1767225600, chat: { id: 999 } } },
  ]);
  const bin = fakeClaude(dir, repo);

  const run = await runLooper(repo, {
    PATH: `${bin}:${process.env.PATH}`,
    TELEGRAM_API_BASE: telegram.url,
    TELEGRAM_BOT_TOKEN: "111:test",
    TELEGRAM_CHAT_ID: "999",
    NOTES_MCP_URL: "http://127.0.0.1:1/mcp",
    NOTES_MCP_TOKEN: "notes-token",
    LOOPER_TASK: "task-note",
    XDG_CONFIG_HOME: join(dir, "config"),
  });
  telegram.server.close();

  assert.equal(run.status, 0, run.stderr);

  const prompt = readFileSync(join(dir, "prompt.txt"), "utf8");
  assert.match(prompt, /The task is note task-note/);
  assert.match(prompt, /This is the first wake/);

  const args = readFileSync(join(dir, "args.txt"), "utf8");
  assert.match(args, /--permission-mode\nauto/);
  assert.match(args, /mcp__notes mcp__looper/);
  assert.match(args, /notes-token/);

  const state = JSON.parse(readFileSync(join(repo, ".looper", "state.json"), "utf8")) as {
    runs: number;
    awaitingReply: boolean;
    lastRun: { outcome: string; text: string; sessionId: string };
  };
  assert.equal(state.runs, 1);
  assert.equal(state.awaitingReply, true, "the outbox said it asked something");
  assert.equal(state.lastRun.outcome, "asked");
  assert.equal(state.lastRun.text, "asked and stopped");
  assert.equal(state.lastRun.sessionId, "11111111-2222-3333-4444-555555555555");

  // `.looper` ignores itself, so nothing the loop writes can reach a commit.
  assert.equal(readFileSync(join(repo, ".looper", ".gitignore"), "utf8").trim(), "*");
  const tracked = execFileSync("git", ["status", "--porcelain"], { cwd: repo, encoding: "utf8" });
  assert.equal(tracked.includes(".looper"), false, tracked);
});

test("a message sent while the loop is up reaches the next prompt", async () => {
  const dir = mkdtempSync(join(tmpdir(), "looper-test-"));
  const repo = join(dir, "repo");
  mkdirSync(join(repo, ".looper"), { recursive: true });
  execFileSync("git", ["init", "-q"], { cwd: repo });
  // A message that arrived before this run, as if it had been picked up by the
  // listener and left in the state for the next wake.
  writeFileSync(
    join(repo, ".looper", "state.json"),
    JSON.stringify({
      runs: 3,
      telegramOffset: 5,
      pending: [{ updateId: 5, at: 1767225600000, text: "use sqlite, not postgres" }],
      awaitingReply: true,
      failures: 0,
      lastRun: {
        at: new Date().toISOString(),
        outcome: "asked",
        sessionId: "old",
        text: "I asked which database to use.",
        durationMs: 120_000,
        costUsd: 0.5,
      },
    })
  );

  const telegram = await fakeTelegram([]);
  const bin = fakeClaude(dir, repo);
  const run = await runLooper(repo, {
    PATH: `${bin}:${process.env.PATH}`,
    TELEGRAM_API_BASE: telegram.url,
    TELEGRAM_BOT_TOKEN: "111:test",
    TELEGRAM_CHAT_ID: "999",
    NOTES_MCP_URL: "http://127.0.0.1:1/mcp",
    NOTES_MCP_TOKEN: "notes-token",
    LOOPER_TASK: "task-note",
    XDG_CONFIG_HOME: join(dir, "config"),
  });
  telegram.server.close();

  assert.equal(run.status, 0, run.stderr);
  const prompt = readFileSync(join(dir, "prompt.txt"), "utf8");
  assert.match(prompt, /use sqlite, not postgres/);
  assert.match(prompt, /I asked which database to use/);
  assert.match(prompt, /wake \(number 4\)/);
  // Resume is the default, so a wake after another continues its session.
  assert.match(readFileSync(join(dir, "args.txt"), "utf8"), /--resume\nold/);
});

test("a session that cannot be resumed is dropped, not retried forever", async () => {
  const dir = mkdtempSync(join(tmpdir(), "looper-test-"));
  const repo = join(dir, "repo");
  mkdirSync(join(repo, ".looper"), { recursive: true });
  execFileSync("git", ["init", "-q"], { cwd: repo });
  writeFileSync(
    join(repo, ".looper", "state.json"),
    JSON.stringify({
      runs: 1,
      telegramOffset: 0,
      pending: [],
      awaitingReply: false,
      failures: 0,
      lastRun: {
        at: new Date().toISOString(),
        outcome: "done",
        sessionId: "old",
        text: "",
        durationMs: 1000,
        costUsd: null,
      },
    })
  );

  const telegram = await fakeTelegram([]);
  const bin = fakeClaude(dir, repo, "dies");
  const run = await runLooper(repo, {
    PATH: `${bin}:${process.env.PATH}`,
    TELEGRAM_API_BASE: telegram.url,
    TELEGRAM_BOT_TOKEN: "111:test",
    TELEGRAM_CHAT_ID: "999",
    NOTES_MCP_URL: "http://127.0.0.1:1/mcp",
    NOTES_MCP_TOKEN: "notes-token",
    LOOPER_TASK: "task-note",
    XDG_CONFIG_HOME: join(dir, "config"),
  });
  telegram.server.close();

  assert.equal(run.status, 0, run.stderr);
  assert.match(readFileSync(join(dir, "args.txt"), "utf8"), /--resume\nold/);
  const state = JSON.parse(readFileSync(join(repo, ".looper", "state.json"), "utf8")) as {
    failures: number;
    lastRun: { outcome: string; sessionId: string | null };
  };
  assert.equal(state.lastRun.outcome, "failed");
  assert.equal(state.lastRun.sessionId, null, "the dead session id is forgotten");
  assert.equal(state.failures, 1);
});


test("an overloaded API is waited out, not treated as a failure", async () => {
  const dir = mkdtempSync(join(tmpdir(), "looper-test-"));
  const repo = join(dir, "repo");
  mkdirSync(join(repo, ".looper"), { recursive: true });
  execFileSync("git", ["init", "-q"], { cwd: repo });
  // A message waiting to be handed over, and a session worth keeping: an overload
  // must lose neither, since the agent never got as far as reading the prompt.
  writeFileSync(
    join(repo, ".looper", "state.json"),
    JSON.stringify({
      runs: 2,
      telegramOffset: 0,
      pending: [{ updateId: 3, at: Date.now(), text: "use sqlite, not postgres" }],
      awaitingReply: false,
      failures: 0,
      overloads: 0,
      lastRun: {
        at: new Date().toISOString(),
        outcome: "done",
        sessionId: "old",
        text: "found the parser",
        durationMs: 1000,
        costUsd: null,
      },
    })
  );

  const telegram = await fakeTelegram([]);
  const bin = fakeClaude(dir, repo, "overloaded");
  const run = await runLooper(repo, {
    PATH: `${bin}:${process.env.PATH}`,
    TELEGRAM_API_BASE: telegram.url,
    TELEGRAM_BOT_TOKEN: "111:test",
    TELEGRAM_CHAT_ID: "999",
    NOTES_MCP_URL: "http://127.0.0.1:1/mcp",
    NOTES_MCP_TOKEN: "notes-token",
    LOOPER_TASK: "task-note",
    XDG_CONFIG_HOME: join(dir, "config"),
  });
  telegram.server.close();

  assert.equal(run.status, 0, run.stderr);
  const state = JSON.parse(readFileSync(join(repo, ".looper", "state.json"), "utf8")) as {
    failures: number;
    overloads: number;
    pending: { text: string }[];
    lastRun: { outcome: string; text: string; sessionId: string | null; error?: string };
  };
  assert.equal(state.lastRun.outcome, "overloaded");
  assert.equal(state.failures, 0, "an overload is not a failure");
  assert.equal(state.overloads, 1);
  assert.equal(state.lastRun.sessionId, "11111111-2222-3333-4444-555555555555");
  assert.equal(state.lastRun.text, "", "the API error is not the agent's sign-off");
  assert.match(state.lastRun.error ?? "", /api error 529/);
  assert.deepEqual(
    state.pending.map((message) => message.text),
    ["use sqlite, not postgres"],
    "a message the agent never saw goes back in the queue"
  );
  assert.deepEqual(telegram.sent, [], "nothing is worth telling you about one overload");
});


test("a spent session cap is read as one, and waited out rather than retried", async () => {
  const dir = mkdtempSync(join(tmpdir(), "looper-test-"));
  const repo = join(dir, "repo");
  mkdirSync(join(repo, ".looper"), { recursive: true });
  execFileSync("git", ["init", "-q"], { cwd: repo });
  // A message waiting, and failures already piled up: a cap is not a fault, so it
  // must clear the count rather than adding to it, and the message must come back
  // marked so that it cannot wake the loop straight back into the same cap.
  writeFileSync(
    join(repo, ".looper", "state.json"),
    JSON.stringify({
      runs: 4,
      telegramOffset: 0,
      pending: [{ updateId: 9, at: Date.now(), text: "carry on with the parser" }],
      awaitingReply: false,
      failures: 7,
      overloads: 0,
      lastRun: {
        at: new Date().toISOString(),
        outcome: "done",
        sessionId: "old",
        text: "found the parser",
        durationMs: 1000,
        costUsd: null,
      },
    })
  );

  const telegram = await fakeTelegram([]);
  const bin = fakeClaude(dir, repo, "limited");
  const run = await runLooper(repo, {
    PATH: `${bin}:${process.env.PATH}`,
    TELEGRAM_API_BASE: telegram.url,
    TELEGRAM_BOT_TOKEN: "111:test",
    TELEGRAM_CHAT_ID: "999",
    NOTES_MCP_URL: "http://127.0.0.1:1/mcp",
    NOTES_MCP_TOKEN: "notes-token",
    LOOPER_TASK: "task-note",
    XDG_CONFIG_HOME: join(dir, "config"),
  });
  telegram.server.close();

  assert.equal(run.status, 0, run.stderr);
  const state = JSON.parse(readFileSync(join(repo, ".looper", "state.json"), "utf8")) as {
    failures: number;
    pending: { text: string; tried?: boolean }[];
    lastRun: { outcome: string; sessionId: string | null };
  };
  assert.equal(state.lastRun.outcome, "limited", "a 429 is a cap, not a failure");
  assert.equal(state.failures, 0, "a cap clears the failure backoff rather than feeding it");
  assert.equal(state.lastRun.sessionId, "11111111-2222-3333-4444-555555555555");
  assert.equal(state.pending.length, 1, "the message the agent never saw is kept");
  assert.equal(state.pending[0].tried, true, "but it has had its turn at waking the loop");
});

test("a cap says when it lifts, in the words the API uses", () => {
  const text = "You've hit your session limit \u00b7 resets 9:50am (America/Los_Angeles)";
  const cap = readCap(text, 429);
  assert.ok(cap, "a 429 is a cap even though its text never says so");
  const resetAt = cap.resetAt;
  assert.ok(resetAt !== null, "and the reset time is read out of the prose");
  assert.ok(resetAt > Date.now(), "the reset is always the next one, never a past one");
  assert.ok(resetAt - Date.now() <= 86_400_000, "and never more than a day away");
  const there = new Intl.DateTimeFormat("en-GB", {
    timeZone: "America/Los_Angeles",
    hour12: false,
    hour: "2-digit",
    minute: "2-digit",
  }).format(new Date(resetAt));
  assert.equal(there, "09:50");

  // The agent's own writing is never read this way, but Claude's is: an error that
  // says nothing about a cap and carries no 429 is not one.
  assert.equal(readCap("api error 400: your request was malformed", 400), null);
  // The epoch form the CLI sometimes uses still wins over any prose.
  assert.deepEqual(readCap("usage limit reached|1740000000"), { resetAt: 1740000000000 });
});
