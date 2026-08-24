// One wake: `claude -p` run once in the repo, with the notes server and Looper's
// own notify tool wired in, and its stream of events read as it goes.
//
// The stream (`--output-format stream-json`) is kept for two reasons: every line
// is written to `.looper/runs/`, which is the only record of what the agent
// actually did, and the tool calls can be echoed to the terminal so a long wake
// isn't a silent hour.

import { spawn } from "node:child_process";
import { createWriteStream } from "node:fs";
import { randomUUID } from "node:crypto";
import type { Config } from "./config.ts";

export interface RunResult {
  /** The session, so `claude --resume <id>` can open up what happened. */
  sessionId: string;
  /** The agent's closing words. */
  text: string;
  /** True when Claude ran and finished of its own accord. */
  ok: boolean;
  /** Set when the run stopped because a usage cap was reached. */
  limit: { resetAt: number | null } | null;
  /** Why it ended badly, when it did. */
  error?: string;
  durationMs: number;
  costUsd: number | null;
  turns: number | null;
  toolCalls: number;
}

/**
 * Claude reports a spent cap in prose rather than in a field, so this is a text
 * match. It is only ever applied to Claude's own error output — never to the
 * agent's writing — so the agent mentioning "rate limit" in a note can't put the
 * loop to sleep for three hours.
 */
const capPatterns = [
  /usage limit reached/i,
  /rate limit/i,
  /\b429\b/,
  /quota (?:exceeded|exhausted)/i,
  /out of (?:usage )?credits?/i,
  /upgrade to (?:continue|increase)/i,
];

/** `... limit reached|1740000000` — the epoch second the cap lifts, when given. */
const resetPattern = /limit reached\|(\d{10,13})/;

function readCap(text: string): { resetAt: number | null } | null {
  if (!capPatterns.some((pattern) => pattern.test(text))) return null;
  const match = resetPattern.exec(text);
  if (!match) return { resetAt: null };
  const value = Number(match[1]);
  return { resetAt: value > 1e12 ? value : value * 1000 };
}

/** The MCP servers the agent gets, and nothing else: `--strict-mcp-config` drops the rest. */
function mcpConfig(config: Config, stateDir: string, looperDir: string): string {
  return JSON.stringify({
    mcpServers: {
      notes: {
        type: "http",
        url: config.notes.url,
        headers: { Authorization: `Bearer ${config.notes.token}` },
      },
      looper: {
        type: "stdio",
        command: process.execPath,
        args: [`${looperDir}/src/notify.ts`],
        env: {
          LOOPER_STATE_DIR: stateDir,
          LOOPER_REPO: config.repo,
          TELEGRAM_BOT_TOKEN: config.telegram.token,
          TELEGRAM_CHAT_ID: config.telegram.chatId,
        },
      },
    },
  });
}

export interface RunOptions {
  config: Config;
  prompt: string;
  /** Where `.looper` is, for the notify tool's outbox. */
  stateDir: string;
  /** Looper's own directory, so the notify server can be found. */
  looperDir: string;
  /** Where to write the raw stream of events. */
  logPath: string;
  /** The session to continue, when the config asks for continuity. */
  resume: string | null;
  /** Progress, one line at a time. */
  onEvent?: (line: string) => void;
}

/** The command a wake will run, for `--dry-run` and for the log. */
export function buildArgs(options: RunOptions): { args: string[]; sessionId: string } {
  const { config } = options;
  const sessionId = options.resume ?? randomUUID();
  const args = [
    "--print",
    "--output-format",
    "stream-json",
    // stream-json in print mode is only allowed alongside --verbose.
    "--verbose",
    "--model",
    config.model,
    "--permission-mode",
    config.permissionMode,
    "--mcp-config",
    mcpConfig(config, options.stateDir, options.looperDir),
    "--strict-mcp-config",
    // The two servers are allowed wholesale: an autonomous agent that has to ask
    // for permission to read its own task cannot get started, and the classifier
    // named by --permission-mode still governs everything else.
    "--allowedTools",
    "mcp__notes mcp__looper",
  ];
  if (config.effort) args.push("--effort", config.effort);
  if (config.fallbackModel) args.push("--fallback-model", config.fallbackModel);
  args.push(...(options.resume ? ["--resume", sessionId] : ["--session-id", sessionId]));
  return { args, sessionId };
}

/**
 * Run one wake to completion. Never rejects: a spawn failure, a non-zero exit, a
 * spent cap and a timeout are all outcomes the loop has an answer for, so they
 * come back on the result instead of as exceptions.
 */
export function runWake(options: RunOptions): {
  sessionId: string;
  completion: Promise<RunResult>;
  stop: () => void;
} {
  const { args, sessionId } = buildArgs(options);
  const started = Date.now();
  const log = createWriteStream(options.logPath, { flags: "a" });

  let child: ReturnType<typeof spawn> | null = null;
  let stopped = false;
  const kill = () => {
    if (!child) return;
    child.kill("SIGTERM");
    // A wedged run must not hold the loop; SIGKILL is the backstop.
    setTimeout(() => {
      try {
        child?.kill("SIGKILL");
      } catch {
        /* already gone */
      }
    }, 10_000);
  };

  const completion = new Promise<RunResult>((settle) => {
    let text = "";
    let effectiveSession = sessionId;
    let cost: number | null = null;
    let turns: number | null = null;
    let toolCalls = 0;
    let cap: { resetAt: number | null } | null = null;
    let resultError: string | null = null;
    let stderr = "";
    let timedOut = false;
    let done = false;

    const finish = (result: Omit<RunResult, "durationMs" | "toolCalls" | "sessionId">) => {
      if (done) return;
      done = true;
      clearTimeout(timer);
      log.end();
      settle({
        ...result,
        sessionId: effectiveSession,
        durationMs: Date.now() - started,
        toolCalls,
      });
    };

    try {
      child = spawn("claude", args, {
        cwd: options.config.repo,
        stdio: ["pipe", "pipe", "pipe"],
        env: process.env,
      });
    } catch (error) {
      finish({
        text: "",
        ok: false,
        limit: null,
        error: `Could not start claude: ${(error as Error).message}`,
        costUsd: null,
        turns: null,
      });
      return;
    }

    const timer = setTimeout(() => {
      timedOut = true;
      kill();
    }, options.config.timing.runTimeout);

    // Events arrive as one JSON object per line, but a line can be split across
    // chunks, so the tail is held back until its newline turns up.
    let buffer = "";
    child.stdout!.setEncoding("utf8");
    child.stdout!.on("data", (chunk: string) => {
      buffer += chunk;
      let newline = buffer.indexOf("\n");
      while (newline !== -1) {
        const line = buffer.slice(0, newline);
        buffer = buffer.slice(newline + 1);
        newline = buffer.indexOf("\n");
        if (!line.trim()) continue;
        log.write(line + "\n");
        let event: Event;
        try {
          event = JSON.parse(line) as Event;
        } catch {
          continue;
        }
        if (event.session_id) effectiveSession = event.session_id;
        if (event.type === "assistant" && event.message?.content) {
          for (const block of event.message.content) {
            if (block.type === "tool_use") {
              toolCalls += 1;
              options.onEvent?.(describeToolUse(block));
            }
          }
        }
        if (event.type === "result") {
          if (typeof event.result === "string") text = event.result;
          if (typeof event.total_cost_usd === "number") cost = event.total_cost_usd;
          if (typeof event.num_turns === "number") turns = event.num_turns;
          const failed = event.is_error === true || event.subtype !== "success";
          if (failed) {
            resultError = `${event.subtype ?? "error"}${event.result ? `: ${event.result}` : ""}`;
            cap = cap ?? readCap(String(event.result ?? "") + " " + (event.subtype ?? ""));
          }
        }
      }
    });

    child.stderr!.setEncoding("utf8");
    child.stderr!.on("data", (chunk: string) => {
      stderr += chunk;
      cap = cap ?? readCap(chunk);
    });

    child.on("error", (error: NodeJS.ErrnoException) => {
      finish({
        text,
        ok: false,
        limit: null,
        error:
          error.code === "ENOENT"
            ? "The `claude` CLI is not on PATH."
            : `claude failed to run: ${error.message}`,
        costUsd: cost,
        turns,
      });
    });

    child.on("close", (code) => {
      const trailing = stderr.trim();
      cap = cap ?? readCap(trailing);
      if (stopped) {
        finish({ text, ok: false, limit: null, error: "Stopped.", costUsd: cost, turns });
        return;
      }
      if (timedOut) {
        finish({
          text,
          ok: false,
          limit: cap,
          error: "The wake ran past its timeout and was killed.",
          costUsd: cost,
          turns,
        });
        return;
      }
      if (cap) {
        finish({ text, ok: false, limit: cap, error: resultError ?? trailing, costUsd: cost, turns });
        return;
      }
      if (code !== 0 || resultError) {
        finish({
          text,
          ok: false,
          limit: null,
          error: resultError ?? `claude exited with code ${code}${trailing ? `: ${trailing}` : ""}`,
          costUsd: cost,
          turns,
        });
        return;
      }
      finish({ text, ok: true, limit: null, costUsd: cost, turns });
    });

    child.stdin!.write(options.prompt);
    child.stdin!.end();
  });

  return {
    sessionId,
    completion,
    stop: () => {
      stopped = true;
      kill();
    },
  };
}

interface Block {
  type: string;
  name?: string;
  input?: Record<string, unknown>;
}

interface Event {
  type?: string;
  subtype?: string;
  session_id?: string;
  result?: unknown;
  is_error?: boolean;
  total_cost_usd?: number;
  num_turns?: number;
  message?: { content?: Block[] };
}

/** A tool call as one short line: `Bash(git commit -m ...)`. */
function describeToolUse(block: Block): string {
  const name = block.name ?? "tool";
  const input = block.input ?? {};
  const detail =
    typeof input.command === "string"
      ? input.command
      : typeof input.file_path === "string"
        ? input.file_path
        : typeof input.path === "string"
          ? input.path
          : typeof input.pattern === "string"
            ? input.pattern
            : typeof input.description === "string"
              ? input.description
              : "";
  const trimmed = detail.replace(/\s+/g, " ").slice(0, 80);
  return trimmed ? `${name}(${trimmed})` : name;
}
