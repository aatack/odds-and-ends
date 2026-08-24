// Looper's configuration: two env files, merged, and asked for on the terminal
// when something is missing.
//
// Secrets go in one global file (~/.config/looper/env) because the same Telegram
// bot and the same notes server serve every task, while what a particular
// directory is *for* belongs beside the work, in <repo>/.looper/env. Anything
// required but absent is prompted for before the loop starts and then written
// back, so the second run of a directory is unattended.

import { existsSync, mkdirSync, readFileSync, writeFileSync, chmodSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { homedir } from "node:os";
import { createInterface } from "node:readline/promises";

/** Where the shared secrets live: one Telegram bot and one notes server for every task. */
export const globalEnvPath = join(
  process.env.XDG_CONFIG_HOME ?? join(homedir(), ".config"),
  "looper",
  "env"
);

/** Where a directory's own settings live: which task it serves, and its timings. */
export function repoEnvPath(repo: string): string {
  return join(repo, ".looper", "env");
}

export interface Timing {
  /** Gap after a wake that ended cleanly, before the next one. */
  turn: number;
  /** Gap after a wake that failed — doubling each time, up to a day. */
  stall: number;
  /** Gap after a wake lost to an overloaded API — doubling, up to the stall gap. */
  overload: number;
  /** Gap after hitting a usage cap, when the cap doesn't say when it resets. */
  limit: number;
  /** How long to hold off waking after the agent asked the user something. */
  question: number;
  /** Quiet time after your last Telegram message before the reply counts as finished. */
  grace: number;
  /** Hard ceiling on one wake, after which the process is killed. */
  runTimeout: number;
}

export interface Config {
  /** The git repo the agent works in, and never outside of. */
  repo: string;
  /**
   * A `CLAUDE_CONFIG_DIR` for the wakes, which is what pins this repo to one
   * Claude account: the whole config directory, credentials included, lives
   * there. Null means whichever account `claude` is logged into normally.
   */
  claudeConfigDir: string | null;
  /** The note that defines the task: an entity id, or an alias like `@index`. */
  task: string;
  model: string;
  effort: string | null;
  fallbackModel: string | null;
  permissionMode: string;
  /** `resume` continues the last Claude session; `fresh` starts a new one each wake. */
  sessionMode: "resume" | "fresh";
  telegram: { token: string; chatId: string };
  notes: { url: string; token: string };
  timing: Timing;
}

// ---------------------------------------------------------------------------
// env files

/** Parse `KEY=value` lines, ignoring blanks and `#` comments, unwrapping quotes. */
export function parseEnv(text: string): Record<string, string> {
  const values: Record<string, string> = {};
  for (const line of text.split("\n")) {
    const trimmed = line.trim();
    if (!trimmed || trimmed.startsWith("#")) continue;
    const eq = trimmed.indexOf("=");
    if (eq === -1) continue;
    const key = trimmed.slice(0, eq).trim();
    let value = trimmed.slice(eq + 1).trim();
    if (
      (value.startsWith('"') && value.endsWith('"')) ||
      (value.startsWith("'") && value.endsWith("'"))
    ) {
      value = value.slice(1, -1);
    }
    values[key] = value;
  }
  return values;
}

function readEnv(path: string): Record<string, string> {
  return existsSync(path) ? parseEnv(readFileSync(path, "utf8")) : {};
}

/**
 * Write values into an env file, replacing the lines for keys it already has and
 * appending the rest. Rewriting rather than regenerating keeps the comments and
 * ordering a person put there by hand.
 */
function upsertEnv(path: string, values: Record<string, string>): void {
  mkdirSync(dirname(path), { recursive: true });
  const lines = existsSync(path) ? readFileSync(path, "utf8").split("\n") : [];
  const remaining = { ...values };
  const rewritten = lines.map((line) => {
    const eq = line.indexOf("=");
    if (eq === -1 || line.trim().startsWith("#")) return line;
    const key = line.slice(0, eq).trim();
    if (!(key in remaining)) return line;
    const value = remaining[key];
    delete remaining[key];
    return `${key}=${value}`;
  });
  while (rewritten.length && rewritten[rewritten.length - 1].trim() === "") rewritten.pop();
  for (const [key, value] of Object.entries(remaining)) rewritten.push(`${key}=${value}`);
  writeFileSync(path, rewritten.join("\n") + "\n", { mode: 0o600 });
  // The global file holds a bot token; keep it unreadable to other users even if
  // it existed with looser permissions before.
  try {
    chmodSync(path, 0o600);
  } catch {
    /* not ours to chmod; the write above still succeeded */
  }
}

/**
 * Resolve a path to absolute, expanding a leading `~` first — which `resolve`
 * does not do, and which an env file written by hand will almost always use.
 */
export function expandPath(path: string): string {
  const trimmed = path.trim();
  const expanded =
    trimmed === "~" || trimmed.startsWith("~/") ? join(homedir(), trimmed.slice(1)) : trimmed;
  return resolve(expanded);
}

// ---------------------------------------------------------------------------
// durations

/** Parse `90s`, `5m`, `3h`, `1d`, or a bare number of seconds, into milliseconds. */
export function parseDuration(text: string): number {
  const match = /^(\d+(?:\.\d+)?)\s*(ms|s|m|h|d)?$/i.exec(text.trim());
  if (!match) throw new Error(`Not a duration: ${text} (try 90s, 5m, 3h, 1d)`);
  const amount = Number(match[1]);
  const unit = (match[2] ?? "s").toLowerCase();
  const scale =
    unit === "ms"
      ? 1
      : unit === "s"
        ? 1000
        : unit === "m"
          ? 60_000
          : unit === "h"
            ? 3_600_000
            : 86_400_000;
  return Math.round(amount * scale);
}

/** A duration as something to read in a log line: `4h 20m`, `90s`. */
export function formatDuration(ms: number): string {
  if (ms < 1000) return `${ms}ms`;
  const seconds = Math.round(ms / 1000);
  if (seconds < 90) return `${seconds}s`;
  const minutes = Math.floor(seconds / 60);
  if (minutes < 90) return `${minutes}m`;
  const hours = Math.floor(minutes / 60);
  const rest = minutes % 60;
  return rest ? `${hours}h ${rest}m` : `${hours}h`;
}

// ---------------------------------------------------------------------------
// prompting

/**
 * Ask a question on the terminal. `hidden` reads without echoing, so a pasted
 * token doesn't sit in the scrollback; it needs a TTY, and falls back to a plain
 * visible read when there isn't one.
 */
async function ask(question: string, opts: { hidden?: boolean } = {}): Promise<string> {
  if (opts.hidden && process.stdin.isTTY) return askHidden(question);
  const rl = createInterface({ input: process.stdin, output: process.stdout });
  try {
    return (await rl.question(question)).trim();
  } finally {
    rl.close();
  }
}

/** Ctrl-C, and the two things a terminal sends for backspace. */
const interrupt = String.fromCharCode(3);
const rubout = [String.fromCharCode(127), String.fromCharCode(8)];

function askHidden(question: string): Promise<string> {
  return new Promise((resolvePromise, reject) => {
    process.stdout.write(question);
    let value = "";
    const stdin = process.stdin;
    const wasRaw = stdin.isRaw;
    const restore = () => {
      stdin.removeListener("data", onData);
      stdin.setRawMode(wasRaw);
      stdin.pause();
    };
    const onData = (chunk: string) => {
      for (const char of chunk) {
        if (char === "\r" || char === "\n") {
          restore();
          process.stdout.write("\n");
          resolvePromise(value.trim());
          return;
        }
        if (char === interrupt) {
          restore();
          reject(new Error("Interrupted."));
          return;
        }
        if (rubout.includes(char)) {
          value = value.slice(0, -1);
          continue;
        }
        value += char;
      }
    };
    stdin.setRawMode(true);
    stdin.resume();
    stdin.setEncoding("utf8");
    stdin.on("data", onData);
  });
}

/** One thing Looper needs to know, and where the answer is kept. */
interface Question {
  key: string;
  scope: "global" | "repo";
  hidden?: boolean;
  prompt: string;
  /** Shown once above the prompt, for the things that need explaining. */
  help?: string;
}

const questions: Question[] = [
  {
    key: "TELEGRAM_BOT_TOKEN",
    scope: "global",
    hidden: true,
    prompt: "Telegram bot token: ",
    help:
      "Looper reaches you as a Telegram bot. Message @BotFather, send /newbot, and\n" +
      "paste the token it gives you (it looks like 123456789:AAaBb...).",
  },
  {
    key: "TELEGRAM_CHAT_ID",
    scope: "global",
    prompt: "Telegram chat id (leave blank to detect it): ",
    help:
      "Which chat the bot talks to. Leave this blank and Looper will wait for you\n" +
      "to send your bot a message, then take the chat id from that.",
  },
  {
    key: "NOTES_MCP_URL",
    scope: "global",
    prompt: "Notes MCP url: ",
    help:
      "The notes server the agent reads its task from and writes its findings to,\n" +
      "e.g. http://127.0.0.1:36901/<key>/mcp.",
  },
  { key: "NOTES_MCP_TOKEN", scope: "global", hidden: true, prompt: "Notes MCP bearer token: " },
  {
    key: "LOOPER_TASK",
    scope: "repo",
    prompt: "Task note id: ",
    help:
      "The note that says what to build. The agent reads it and everything under\n" +
      "it at every wake, and writes its own notes back under it.",
  },
];

// ---------------------------------------------------------------------------
// loading

export interface LoadOptions {
  /** The directory the agent will work in. */
  repo: string;
  /** Ask for anything missing (false for `--dry-run`, which shouldn't block). */
  interactive: boolean;
  /** Called with a bot token to watch for a first message, when no chat id is set. */
  detectChatId?: (token: string) => Promise<string>;
}

/**
 * Read both env files over the real environment, ask for whatever is still
 * missing, and hand back a fully-resolved config. Real environment variables win
 * over the files, so a one-off `LOOPER_MODEL=sonnet looper` works.
 */
export async function loadConfig(opts: LoadOptions): Promise<Config> {
  const repo = resolve(opts.repo);
  const values: Record<string, string> = {
    ...readEnv(globalEnvPath),
    ...readEnv(repoEnvPath(repo)),
  };
  for (const key of Object.keys(process.env)) {
    if (key.startsWith("LOOPER_") || key.startsWith("TELEGRAM_") || key.startsWith("NOTES_")) {
      const value = process.env[key];
      if (value) values[key] = value;
    }
  }

  const pendingWrites: { global: Record<string, string>; repo: Record<string, string> } = {
    global: {},
    repo: {},
  };
  let introduced = false;

  for (const question of questions) {
    if (values[question.key]) continue;
    if (!opts.interactive) {
      throw new Error(
        `${question.key} is not set. Run \`looper\` without --dry-run to be asked for it, ` +
          `or add it to ${question.scope === "global" ? globalEnvPath : repoEnvPath(repo)}.`
      );
    }
    if (!introduced) {
      introduced = true;
      console.log("\nLooper needs a few things before it can start.\n");
    }
    if (question.help) console.log(question.help);
    let answer = await ask(question.prompt, { hidden: question.hidden });
    if (!answer && question.key === "TELEGRAM_CHAT_ID" && opts.detectChatId) {
      answer = await opts.detectChatId(values.TELEGRAM_BOT_TOKEN ?? "");
    }
    if (!answer) throw new Error(`${question.key} is required.`);
    values[question.key] = answer;
    pendingWrites[question.scope][question.key] = answer;
    console.log("");
  }

  if (Object.keys(pendingWrites.global).length) {
    upsertEnv(globalEnvPath, pendingWrites.global);
    console.log(`Saved to ${globalEnvPath}`);
  }
  if (Object.keys(pendingWrites.repo).length) {
    upsertEnv(repoEnvPath(repo), pendingWrites.repo);
    console.log(`Saved to ${repoEnvPath(repo)}`);
  }

  const duration = (key: string, fallback: string) => parseDuration(values[key] ?? fallback);
  // Resume by default, and let auto-compaction handle the growth: continuity is
  // worth more than a tidy transcript, and the notes are still the memory of
  // record. `fresh` starts every wake from nothing but the notes.
  const sessionMode = values.LOOPER_SESSION_MODE ?? "resume";
  if (sessionMode !== "fresh" && sessionMode !== "resume") {
    throw new Error(`LOOPER_SESSION_MODE must be "fresh" or "resume", not ${sessionMode}.`);
  }

  return {
    repo,
    claudeConfigDir: values.LOOPER_CLAUDE_CONFIG_DIR
      ? expandPath(values.LOOPER_CLAUDE_CONFIG_DIR)
      : null,
    task: values.LOOPER_TASK,
    // `opus` is the alias for the latest Opus, which is what a long-running
    // background task wants; pin LOOPER_MODEL to a full name to be specific.
    model: values.LOOPER_MODEL ?? "opus",
    effort: values.LOOPER_EFFORT ?? null,
    fallbackModel: values.LOOPER_FALLBACK_MODEL ?? null,
    permissionMode: values.LOOPER_PERMISSION_MODE ?? "auto",
    sessionMode,
    telegram: { token: values.TELEGRAM_BOT_TOKEN, chatId: values.TELEGRAM_CHAT_ID },
    notes: { url: values.NOTES_MCP_URL, token: values.NOTES_MCP_TOKEN },
    timing: {
      turn: duration("LOOPER_TURN_SLEEP", "5m"),
      stall: duration("LOOPER_STALL_SLEEP", "30m"),
      overload: duration("LOOPER_OVERLOAD_SLEEP", "2m"),
      limit: duration("LOOPER_LIMIT_SLEEP", "3h"),
      question: duration("LOOPER_QUESTION_WAIT", "6h"),
      grace: duration("LOOPER_GRACE", "90s"),
      runTimeout: duration("LOOPER_RUN_TIMEOUT", "60m"),
    },
  };
}
