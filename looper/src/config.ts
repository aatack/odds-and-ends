// Looper's configuration: two env files, merged, and asked for on the terminal
// when something is missing.
//
// Secrets go in one global file (~/.config/looper/env) because the same Telegram
// bot serves every task, while what a particular directory is *for* belongs
// beside the work, in <repo>/.looper/env. Anything
// required but absent is prompted for before the loop starts and then written
// back, so the second run of a directory is unattended.

import { execFileSync } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, writeFileSync, chmodSync } from "node:fs";
import { dirname, isAbsolute, join, relative, resolve } from "node:path";
import { homedir } from "node:os";
import { createInterface } from "node:readline/promises";

/** Where the shared secrets live: one Telegram bot for every task. */
export const globalEnvPath = join(
  process.env.XDG_CONFIG_HOME ?? join(homedir(), ".config"),
  "looper",
  "env"
);

/** Where a directory's own settings live: which task it serves, and its timings. */
export function repoEnvPath(repo: string): string {
  return join(repo, ".looper", "env");
}

/**
 * The top of the git repo a directory is in, or null when it is in none. Asked of
 * git rather than found by looking for `.git`, so a subdirectory, a worktree and
 * a submodule all count.
 */
export function findGitRoot(dir: string): string | null {
  try {
    const top = execFileSync("git", ["rev-parse", "--show-toplevel"], {
      cwd: dir,
      encoding: "utf8",
      timeout: 5000,
      stdio: ["ignore", "pipe", "ignore"],
    }).trim();
    return top ? resolve(top) : null;
  } catch {
    return null;
  }
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
  /** The same, after /wait, for the next wake only. */
  longGrace: number;
  /** Hard ceiling on one wake, after which the process is killed. */
  runTimeout: number;
}

export interface Config {
  /**
   * The directory the agent works in, and never changes anything outside of: a
   * git repo, or any directory inside one. Its task, its notes and `.looper`
   * all live here.
   */
  repo: string;
  /**
   * The top of the git repo that `repo` is in — the same directory when Looper
   * runs at the top, and a parent when it runs in a subdirectory of a bigger repo.
   */
  gitRoot: string;
  /**
   * A `CLAUDE_CONFIG_DIR` for the wakes, which is what pins this repo to one
   * Claude account: the whole config directory, credentials included, lives
   * there. Null means whichever account `claude` is logged into normally.
   */
  claudeConfigDir: string | null;
  /**
   * Set once you have said this repo should use the computer's own account, so
   * the question of which account to use is not asked again.
   */
  defaultAccountChosen: boolean;
  /**
   * The note that defines the task, as a path relative to the repo. The repo is
   * the agent's Obsidian vault, so this is an ordinary markdown file in it.
   */
  task: string;
  /** The folder in the repo, relative to it, where the agent keeps its own notes. */
  notesDir: string;
  model: string;
  effort: string | null;
  fallbackModel: string | null;
  permissionMode: string;
  /** `resume` continues the last Claude session; `fresh` starts a new one each wake. */
  sessionMode: "resume" | "fresh";
  /** Every this many wakes, one is spent tidying the notes in a new session. 0 never. */
  tidyEvery: number;
  telegram: { token: string; chatId: string };
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
export function upsertEnv(path: string, values: Record<string, string>): void {
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
export async function ask(question: string, opts: { hidden?: boolean } = {}): Promise<string> {
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
const escape = String.fromCharCode(27);

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
    // Inside an escape sequence: a terminal can wrap a paste in bracketed-paste
    // markers, or send arrow keys, and none of that is part of the answer.
    let escaping = false;
    const onData = (chunk: string) => {
      for (const char of chunk) {
        if (escaping) {
          if (/[A-Za-z~]/.test(char)) escaping = false;
          continue;
        }
        if (char === escape) {
          escaping = true;
          continue;
        }
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
          if (value) process.stdout.write("\b \b");
          value = value.slice(0, -1);
          continue;
        }
        // Any other control character — a Ctrl+V that a console delivers as a
        // keystroke rather than a paste, say — would only corrupt the answer.
        if (char < " ") continue;
        value += char;
        process.stdout.write("*");
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
];

/**
 * A path from an env file as one inside the repo, with forward slashes so it
 * reads the same in the prompt on every platform. Anything that would land
 * outside the repo is refused: the agent is told never to go there.
 */
function withinRepo(repo: string, key: string, path: string): string {
  const inside = relative(repo, resolve(repo, path));
  if (!inside || inside.startsWith("..") || isAbsolute(inside)) {
    throw new Error(`${key} must be a path inside the repo, not ${path}.`);
  }
  return inside.split("\\").join("/");
}

/**
 * Make sure the task note exists, asking for it on the terminal when it does not.
 * The agent can do nothing without a task, so a wake with no note is a wake
 * spent finding that out; one line typed here is enough to start from, and the
 * note is a file in the vault to flesh out by hand whenever you like.
 */
async function ensureTask(repo: string, task: string, interactive: boolean): Promise<void> {
  const path = join(repo, task);
  if (existsSync(path) || !interactive) return;
  if (!process.stdin.isTTY) {
    throw new Error(`There is no task note at ${path}. Write the task there, then run Looper again.`);
  }
  console.log(
    `\nThere is no task note at ${task} yet. Say what the agent should work on — a line\n` +
      `is enough, and you can expand the note in Obsidian later.`
  );
  const description = await ask("Task: ");
  if (!description) throw new Error(`Write the task into ${path}, then run Looper again.`);
  mkdirSync(dirname(path), { recursive: true });
  writeFileSync(path, `# Task\n\n${description}\n`);
  console.log(`Saved to ${path}\n`);
}

/** What a bot token looks like: the bot's number, a colon, and a long secret. */
const botTokenShape = /^\d+:[A-Za-z0-9_-]{30,}$/;

/**
 * Take a bot token as typed, and keep asking until Telegram accepts one. Checked
 * here rather than after the chat question, because detecting the chat needs a
 * working token, and a token that has picked up something odd on the way in — a
 * paste that did not paste, a stray space — otherwise fails as a bare
 * "Not Found" from Telegram, with nothing to say which answer was wrong.
 */
async function settleBotToken(
  first: string,
  check: ((token: string) => Promise<string>) | undefined
): Promise<string> {
  let token = first.replace(/\s+/g, "");
  for (;;) {
    if (!token) throw new Error("TELEGRAM_BOT_TOKEN is required.");
    if (!botTokenShape.test(token)) {
      console.log(
        `That is not a bot token: one looks like 123456789:AAaBb..., about 46 characters, ` +
          `and ${token.length} arrived. If pasting did nothing, paste with a right-click.`
      );
    } else if (check) {
      try {
        const name = await check(token);
        console.log(`That is @${name}.`);
        return token;
      } catch (error) {
        console.log(
          `Telegram does not accept that token (${(error as Error).message}). Copy it again ` +
            `from @BotFather — /mybots, the bot, API Token.`
        );
      }
    } else {
      return token;
    }
    token = (await ask("Telegram bot token: ", { hidden: true })).replace(/\s+/g, "");
  }
}

// ---------------------------------------------------------------------------
// loading

export interface LoadOptions {
  /** The directory the agent will work in. */
  repo: string;
  /** Ask for anything missing (false for `--dry-run`, which shouldn't block). */
  interactive: boolean;
  /** Called with a bot token to watch for a first message, when no chat id is set. */
  detectChatId?: (token: string) => Promise<string>;
  /** Called with a bot token to see whether Telegram accepts it; resolves to the bot's name. */
  checkBotToken?: (token: string) => Promise<string>;
}

/**
 * Read both env files over the real environment, ask for whatever is still
 * missing, and hand back a fully-resolved config. Real environment variables win
 * over the files, so a one-off `LOOPER_MODEL=sonnet looper` works.
 */
export async function loadConfig(opts: LoadOptions): Promise<Config> {
  const repo = resolve(opts.repo);
  const global = readEnv(globalEnvPath);
  const values: Record<string, string> = { ...global, ...readEnv(repoEnvPath(repo)) };
  for (const key of Object.keys(process.env)) {
    if (key.startsWith("LOOPER_") || key.startsWith("TELEGRAM_")) {
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
    if (question.key === "TELEGRAM_BOT_TOKEN") {
      answer = await settleBotToken(answer, opts.checkBotToken);
      // Saved now rather than with the rest: detecting the chat comes next and
      // can time out, and a token Telegram has just accepted should not have to
      // be pasted again because of it.
      upsertEnv(globalEnvPath, { TELEGRAM_BOT_TOKEN: answer });
    }
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

  const tidyEvery = Number(values.LOOPER_TIDY_EVERY ?? "20");
  if (!Number.isInteger(tidyEvery) || tidyEvery < 0) {
    throw new Error(`LOOPER_TIDY_EVERY must be a whole number of wakes, not ${values.LOOPER_TIDY_EVERY}.`);
  }

  const task = withinRepo(repo, "LOOPER_TASK", values.LOOPER_TASK ?? "TASK.md");
  const notesDir = withinRepo(repo, "LOOPER_NOTES_DIR", values.LOOPER_NOTES_DIR ?? "notes");
  await ensureTask(repo, task, opts.interactive);

  return {
    repo,
    gitRoot: findGitRoot(repo) ?? repo,
    claudeConfigDir: values.LOOPER_CLAUDE_CONFIG_DIR
      ? expandPath(values.LOOPER_CLAUDE_CONFIG_DIR)
      : null,
    defaultAccountChosen: values.LOOPER_CLAUDE_ACCOUNT === "default",
    task,
    notesDir,
    // Pinned to a full name rather than the `opus` alias, so a new release does
    // not change the model under a task part way through; LOOPER_MODEL moves it.
    model: values.LOOPER_MODEL ?? "claude-opus-5-5",
    effort: values.LOOPER_EFFORT ?? null,
    fallbackModel: values.LOOPER_FALLBACK_MODEL ?? null,
    // Auto mode: a classifier approves or refuses each action, so an agent with
    // nobody watching can work without permission prompts, and without the
    // blanket approval of --dangerously-skip-permissions.
    permissionMode: values.LOOPER_PERMISSION_MODE ?? "auto",
    sessionMode,
    tidyEvery,
    telegram: { token: values.TELEGRAM_BOT_TOKEN, chatId: values.TELEGRAM_CHAT_ID },
    timing: {
      turn: duration("LOOPER_TURN_SLEEP", "5m"),
      stall: duration("LOOPER_STALL_SLEEP", "30m"),
      overload: duration("LOOPER_OVERLOAD_SLEEP", "2m"),
      limit: duration("LOOPER_LIMIT_SLEEP", "3h"),
      question: duration("LOOPER_QUESTION_WAIT", "6h"),
      grace: duration("LOOPER_GRACE", "60s"),
      longGrace: duration("LOOPER_LONG_GRACE", "5m"),
      runTimeout: duration("LOOPER_RUN_TIMEOUT", "60m"),
    },
  };
}
