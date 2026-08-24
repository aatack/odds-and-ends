// One wake: `claude -p` run once in the repo, with the notes server and Looper's
// own notify tool wired in, and its stream of events read as it goes.
//
// The stream (`--output-format stream-json`) is kept for two reasons: every line
// is written to `.looper/runs/`, which is the only record of what the agent
// actually did, and the tool calls can be echoed to the terminal so a long wake
// isn't a silent hour.

import { execFileSync, spawn } from "node:child_process";
import { createWriteStream } from "node:fs";
import { randomUUID } from "node:crypto";
import type { Config } from "./config.ts";

/**
 * The environment a wake runs in. `CLAUDE_CONFIG_DIR` is the whole of how an
 * account is chosen: credentials, settings and sessions all live in that
 * directory, so pointing a repo at its own is what makes it that repo's account.
 */
export function claudeEnv(config: Config): NodeJS.ProcessEnv {
  return config.claudeConfigDir
    ? { ...process.env, CLAUDE_CONFIG_DIR: config.claudeConfigDir }
    : process.env;
}

export interface Account {
  loggedIn: boolean;
  email?: string;
  subscriptionType?: string;
  orgName?: string;
}

/**
 * Who the wakes will run as, asked of `claude` itself. Checked before the loop
 * starts: an account that isn't logged in fails every wake identically, and it is
 * far better to say so on the terminal than to discover it an hour later.
 */
export function whoseAccount(config: Config): Account {
  // `claude auth status --json` exits 1 when nobody is logged in, but still prints
  // the JSON that says so — so the output is what matters here and the exit code
  // is not. Only output that isn't JSON at all counts as a real failure.
  let output: string;
  try {
    output = execFileSync("claude", ["auth", "status", "--json"], {
      encoding: "utf8",
      env: claudeEnv(config),
      timeout: 30_000,
      stdio: ["ignore", "pipe", "ignore"],
    });
  } catch (error) {
    const printed = (error as { stdout?: string }).stdout;
    if (!printed?.trim()) throw error;
    output = printed;
  }
  try {
    return JSON.parse(output) as Account;
  } catch {
    throw new Error(`claude said something unexpected: ${output.trim().slice(0, 200)}`);
  }
}

export interface RunResult {
  /** The session, so `claude --resume <id>` can open up what happened. */
  sessionId: string;
  /** The agent's closing words. */
  text: string;
  /** True when Claude ran and finished of its own accord. */
  ok: boolean;
  /** Set when the run stopped because a usage cap was reached. */
  limit: { resetAt: number | null } | null;
  /**
   * Set when the wake died on the API itself rather than on anything the agent or
   * Looper did. `transient` marks the kind worth another go in a couple of
   * minutes — an overloaded server — as against a request that will be refused
   * the same way forever.
   */
  apiError: { status: number | null; transient: boolean } | null;
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

/**
 * `resets 9:50am (America/Los_Angeles)` — the other way a cap says when it lifts,
 * as a wall clock in a named zone. Worth reading: the alternative is waiting out a
 * cap by a blind guess, which is hours of doing nothing or a retry that is
 * refused all over again.
 */
const resetClockPattern = /resets?\s+(?:at\s+)?(\d{1,2})(?::(\d{2}))?\s*(am|pm)?(?:\s*\(([^)]+)\))?/i;

/** Exported for the test that checks a session limit is read as one. */
export function readCap(
  text: string,
  status: number | null = null
): { resetAt: number | null } | null {
  // A 429 is a cap by definition, and the one the API sends most — "You've hit
  // your session limit" — has none of the words below anywhere in it, so the
  // status has to count on its own. Without this the wake reads as an ordinary
  // failure, and the loop backs off blind instead of waiting for the reset.
  if (status !== 429 && !capPatterns.some((pattern) => pattern.test(text))) return null;
  const match = resetPattern.exec(text);
  if (match) {
    const value = Number(match[1]);
    return { resetAt: value > 1e12 ? value : value * 1000 };
  }
  return { resetAt: readResetClock(text) };
}

/**
 * When the clock time in `text` next comes round, in the zone the text names or
 * the machine's own when it names none. A cap that resets at 9:50 is 9:50 today
 * if that is still to come and 9:50 tomorrow if it has been, which is right
 * either way round: no cap lasts a day.
 */
function readResetClock(text: string, now = Date.now()): number | null {
  const match = resetClockPattern.exec(text);
  if (!match) return null;
  let hour = Number(match[1]);
  const minute = Number(match[2] ?? 0);
  if (hour > 23 || minute > 59) return null;
  const meridiem = match[3]?.toLowerCase();
  if (meridiem === "pm" && hour < 12) hour += 12;
  if (meridiem === "am" && hour === 12) hour = 0;
  const there = clockIn(match[4]?.trim() ?? null, now);
  if (there === null) return null;
  const day = 86_400;
  const wait = (((hour * 3600 + minute * 60 - there) % day) + day) % day;
  return now + wait * 1000;
}

/**
 * The time of day where a zone is now, in seconds — or null if it isn't a zone
 * Node knows, in which case the cap is waited out blind rather than woken at the
 * wrong hour. A clocks-change between now and the reset would put this an hour
 * out; the cap refuses once more and is read again, which is the same as any
 * other early retry.
 */
function clockIn(zone: string | null, now: number): number | null {
  try {
    const parts = new Intl.DateTimeFormat("en-GB", {
      ...(zone ? { timeZone: zone } : {}),
      hour12: false,
      hour: "2-digit",
      minute: "2-digit",
      second: "2-digit",
    }).formatToParts(new Date(now));
    const value = (type: string) => Number(parts.find((part) => part.type === type)?.value ?? NaN);
    // Midnight comes back as hour 24 from some builds of ICU.
    const seconds = (value("hour") % 24) * 3600 + value("minute") * 60 + value("second");
    return Number.isFinite(seconds) ? seconds : null;
  } catch {
    return null;
  }
}

/**
 * A server-side error, in the words the CLI uses when it gives up retrying one.
 * Only ever applied to Claude's own error output, the same as the cap patterns:
 * the agent writing "overloaded" in a note must not put the loop to sleep.
 */
const overloadPatterns = [/\boverloaded\b/i, /\b5(?:00|02|03|04|29)\b/];

/**
 * Whether a finished wake died on the API. The result event says so in
 * `api_error_status` and `terminal_reason`, which is far better than reading the
 * message: a 5xx is capacity and worth retrying soon, while a 4xx is a request
 * that will be refused identically forever — bar a 429, which is a spent cap and
 * is read as one above, before this ever runs.
 */
function readApiError(event: Event): { status: number | null; transient: boolean } | null {
  const status = typeof event.api_error_status === "number" ? event.api_error_status : null;
  const said = event.terminal_reason === "api_error" || event.is_api_error_message === true;
  if (status === null && !said) return null;
  return { status, transient: status === null ? false : status >= 500 || status === 408 };
}

/** The same thing from loose text, for a run that fell over without a result event. */
function readOverload(text: string): { status: number | null; transient: boolean } | null {
  if (!overloadPatterns.some((pattern) => pattern.test(text))) return null;
  const status = /\b(5(?:00|02|03|04|29))\b/.exec(text);
  return { status: status ? Number(status[1]) : null, transient: true };
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
    let apiError: { status: number | null; transient: boolean } | null = null;
    let resultError: string | null = null;
    let stderr = "";
    let timedOut = false;
    let done = false;

    const finish = (
      result: Omit<RunResult, "durationMs" | "toolCalls" | "sessionId" | "apiError">
    ) => {
      if (done) return;
      done = true;
      clearTimeout(timer);
      log.end();
      settle({
        ...result,
        apiError,
        sessionId: effectiveSession,
        durationMs: Date.now() - started,
        toolCalls,
      });
    };

    try {
      child = spawn("claude", args, {
        cwd: options.config.repo,
        stdio: ["pipe", "pipe", "pipe"],
        env: claudeEnv(options.config),
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
          if (typeof event.total_cost_usd === "number") cost = event.total_cost_usd;
          if (typeof event.num_turns === "number") turns = event.num_turns;
          const failed = event.is_error === true || event.subtype !== "success";
          if (failed) {
            // The `result` of a failed wake is the CLI's error message, not the
            // agent's sign-off, so it is kept as the error and nowhere else:
            // handing "API Error: 529" to the next wake as its own last words
            // would be a lie, and it would make a stillborn wake look like one
            // that had something to say.
            resultError = describeFailure(event);
            cap =
              cap ??
              readCap(
                String(event.result ?? "") + " " + (event.subtype ?? ""),
                typeof event.api_error_status === "number" ? event.api_error_status : null
              );
            apiError = apiError ?? readApiError(event) ?? readOverload(resultError);
          } else if (typeof event.result === "string") {
            text = event.result;
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
      if (!cap && code !== 0) apiError = apiError ?? readOverload(trailing);
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
  is_api_error_message?: boolean;
  api_error_status?: number;
  terminal_reason?: string;
  total_cost_usd?: number;
  num_turns?: number;
  message?: { content?: Block[] };
}

/**
 * Why a wake ended badly, in one line. The subtype is not enough on its own: an
 * API error arrives as `subtype: "success"` with `is_error` set, so "success:
 * API Error: 529" is what the naive reading gives you.
 */
function describeFailure(event: Event): string {
  const detail = typeof event.result === "string" ? event.result : "";
  const label =
    typeof event.api_error_status === "number"
      ? `api error ${event.api_error_status}`
      : event.terminal_reason && event.terminal_reason !== "success"
        ? event.terminal_reason
        : event.subtype && event.subtype !== "success"
          ? event.subtype
          : "error";
  return detail ? `${label}: ${detail}` : label;
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
