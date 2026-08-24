// `looper` — start the loop in the current directory, and keep it running until
// you stop it.

import { existsSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { loadConfig, globalEnvPath, repoEnvPath, formatDuration } from "./config.ts";
import type { Config } from "./config.ts";
import { buildArgs, whoseAccount } from "./claude.ts";
import type { Account } from "./claude.ts";
import { buildPrompt } from "./prompt.ts";
import { State } from "./state.ts";
import { Telegram, detectChatId } from "./telegram.ts";
import { Loop } from "./loop.ts";

const looperDir = resolve(dirname(fileURLToPath(import.meta.url)), "..");

const help = `looper — keep an agent working on one task in the background.

Usage:
  looper [options]

Run it in the git repo you want the agent to work in. It reads its task from a
note on your notes server, wakes an agent to work on it, and messages you on
Telegram when the agent has something to say or something to ask. Answer the
message and your reply reaches the agent at its next wake.

Options:
  --once        Run a single wake and stop. The way to try a task out.
  --dry-run     Print the prompt and the command that would run, and stop.
  --repo <dir>  Work in this directory instead of the current one.
  -h, --help    This.

Configuration lives in two files, and you are asked for anything missing the
first time:
  ${globalEnvPath}
    TELEGRAM_BOT_TOKEN, TELEGRAM_CHAT_ID, NOTES_MCP_URL, NOTES_MCP_TOKEN
  <repo>/.looper/env
    LOOPER_TASK, and any of the settings below

Settings (all optional):
  LOOPER_CLAUDE_CONFIG_DIR
                         a CLAUDE_CONFIG_DIR for this repo's wakes, which is how
                         a repo is pinned to one Claude account.  Log that
                         account in once with
                         CLAUDE_CONFIG_DIR=<dir> claude auth login
  LOOPER_MODEL           default opus
  LOOPER_EFFORT          low | medium | high | xhigh | max
  LOOPER_FALLBACK_MODEL  a model to fall back to when the first is overloaded
  LOOPER_PERMISSION_MODE default auto
  LOOPER_SESSION_MODE    resume (default) continues the last session;
                         fresh starts a new one each wake
  LOOPER_TURN_SLEEP      gap after a clean wake, default 5m
  LOOPER_STALL_SLEEP     gap after a failed wake, doubling, default 30m
  LOOPER_OVERLOAD_SLEEP  gap after an overloaded API, doubling, default 2m
  LOOPER_LIMIT_SLEEP     gap after a usage cap with no stated reset, default 3h
  LOOPER_QUESTION_WAIT   how long to wait for your answer, default 6h
  LOOPER_GRACE           quiet time before your reply counts as done, default 90s
  LOOPER_RUN_TIMEOUT     ceiling on one wake, default 60m

State, logs and every wake's transcript are kept in <repo>/.looper, which
ignores itself in git.`;

interface Args {
  once: boolean;
  dryRun: boolean;
  repo: string;
}

function parseArgs(argv: string[]): Args | null {
  const args: Args = { once: false, dryRun: false, repo: process.cwd() };
  for (let index = 0; index < argv.length; index += 1) {
    const argument = argv[index];
    if (argument === "--once") args.once = true;
    else if (argument === "--dry-run") args.dryRun = true;
    else if (argument === "--repo") args.repo = argv[(index += 1)] ?? process.cwd();
    else if (argument === "-h" || argument === "--help") return null;
    else {
      console.error(`Unknown option: ${argument}\n`);
      return null;
    }
  }
  return args;
}

/** The command line, with the two tokens taken out, for `--dry-run` and the log. */
function redact(args: string[], config: Config): string {
  return args
    .map((argument) =>
      argument
        .replaceAll(config.notes.token, "<notes-token>")
        .replaceAll(config.telegram.token, "<telegram-token>")
    )
    .map((argument) => (/[\s"]/.test(argument) ? JSON.stringify(argument) : argument))
    .join(" ");
}

/**
 * Which account the wakes will run as, or a clear exit. This is asked of `claude`
 * before anything starts, because a logged-out account fails every wake the same
 * way, and the fix is one command the message can just hand over.
 */
function readAccount(config: Config): Account {
  let account: Account;
  try {
    account = whoseAccount(config);
  } catch (error) {
    console.error(`Could not ask claude which account it is using: ${(error as Error).message}`);
    console.error("Is the `claude` CLI installed and on PATH?");
    process.exit(1);
  }
  if (!account.loggedIn) {
    const where = config.claudeConfigDir;
    console.error(`Claude is not logged in${where ? ` in ${where}` : ""}.`);
    console.error(
      where
        ? `Log that account in once with:\n  CLAUDE_CONFIG_DIR=${where} claude auth login`
        : "Log in with `claude auth login`, or set LOOPER_CLAUDE_CONFIG_DIR to a directory\n" +
            "that holds the account you want this repo to use."
    );
    process.exit(1);
  }
  return account;
}

/** The account as one line, for `--dry-run`, whatever state it is in. */
function describeAccount(config: Config): string {
  const where = config.claudeConfigDir ?? "the default config directory";
  try {
    const account = whoseAccount(config);
    return account.loggedIn
      ? `${account.email ?? "unknown"}` +
          `${account.subscriptionType ? ` (${account.subscriptionType})` : ""}` +
          `${account.orgName ? `, ${account.orgName}` : ""} — from ${where}`
      : `not logged in — from ${where}`;
  } catch (error) {
    return `could not be read: ${(error as Error).message}`;
  }
}

async function main(): Promise<void> {
  const asked = process.argv.includes("-h") || process.argv.includes("--help");
  const args = parseArgs(process.argv.slice(2));
  if (!args) {
    console.log(help);
    process.exit(asked ? 0 : 1);
  }

  const repo = resolve(args.repo);
  if (!existsSync(join(repo, ".git"))) {
    console.error(
      `${repo} is not a git repository. Looper keeps the agent inside one repo, and\n` +
        `commits are how a wake hands work to the next, so run \`git init\` there first.`
    );
    process.exit(1);
  }

  const config = await loadConfig({
    repo,
    interactive: !args.dryRun,
    detectChatId: (token) => detectChatId(token),
  });
  const state = new State(config.repo);
  const telegram = new Telegram(config.telegram.token, config.telegram.chatId);

  if (args.dryRun) {
    const prompt = buildPrompt({ config, state: state.data, messages: state.data.pending });
    const { args: claudeArgs } = buildArgs({
      config,
      prompt,
      stateDir: state.dir,
      looperDir,
      logPath: "/dev/null",
      resume: null,
    });
    // Which account this repo would use is one of the things --dry-run is for, so
    // it is reported rather than enforced here: a logged-out account is printed
    // as such instead of stopping the run.
    const account = describeAccount(config);
    const prefix = config.claudeConfigDir ? `CLAUDE_CONFIG_DIR=${config.claudeConfigDir} ` : "";
    console.log(`--- account ---\n${account}\n`);
    console.log(`--- claude ---\n${prefix}claude ${redact(claudeArgs, config)}\n`);
    console.log(`--- prompt ---\n${prompt}`);
    return;
  }

  const account = readAccount(config);

  // Fail before the loop starts rather than at the first thing the agent wants to
  // say: a bad token here is a typo, and a typo should not cost an hour.
  const username = await telegram.whoAmI().catch((error: Error) => {
    console.error(`Telegram is not working: ${error.message}`);
    console.error(`Check TELEGRAM_BOT_TOKEN in ${globalEnvPath}.`);
    process.exit(1);
  });

  const { timing } = config;
  state.log(
    `looper on ${config.repo} — task ${config.task}, model ${config.model}, ` +
      `@${username}, ${state.data.runs} wake(s) so far`
  );
  state.log(
    `account: ${account.email ?? "unknown"}` +
      `${account.subscriptionType ? ` (${account.subscriptionType})` : ""}` +
      `${config.claudeConfigDir ? ` from ${config.claudeConfigDir}` : ""}`
  );
  state.log(
    `gaps: ${formatDuration(timing.turn)} between wakes, ${formatDuration(timing.stall)} after a ` +
      `failure, ${formatDuration(timing.limit)} on a cap, ${formatDuration(timing.question)} for an answer`
  );
  if (!existsSync(repoEnvPath(config.repo))) {
    state.log(`no ${repoEnvPath(config.repo)}; settings came from the environment`);
  }

  const loop = new Loop({ config, state, telegram, looperDir, once: args.once });

  let stopping = false;
  for (const signal of ["SIGINT", "SIGTERM"] as const) {
    process.on(signal, () => {
      if (stopping) process.exit(130);
      stopping = true;
      state.log(`${signal} — stopping after the current wake is killed`);
      loop.stop();
    });
  }

  if (!args.once) {
    await telegram
      .send(
        `Looper is running on ${config.repo}, working on ${config.task}. ` +
          `Message me here and the agent will see it at its next wake.`
      )
      .catch((error: Error) => state.log(`could not send the opening message: ${error.message}`));
  }

  await loop.run();
  state.log("stopped");
  // The Telegram long poll can be mid-flight; nothing is owed to it, so go.
  process.exit(0);
}

main().catch((error: Error) => {
  console.error(error.message);
  process.exit(1);
});
