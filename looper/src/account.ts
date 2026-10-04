// Which Claude account the wakes run as, settled on the terminal before the loop
// starts.
//
// An account is a `CLAUDE_CONFIG_DIR`: credentials, settings and sessions all
// live in that one directory, so giving a repo its own directory is giving it its
// own account. The first run of a repo asks whether the computer's own account
// will do; if not, it asks what it needs, makes a directory for the other
// account, runs `claude auth login` in it, and writes the directory into the
// repo's `.looper/env`. Every run after that just checks the login still holds,
// and offers to log in again when it does not.

import { spawnSync } from "node:child_process";
import { existsSync, mkdirSync, readdirSync } from "node:fs";
import { join } from "node:path";
import { homedir } from "node:os";
import { ask, repoEnvPath, upsertEnv } from "./config.ts";
import type { Config } from "./config.ts";
import { whoseAccount } from "./claude.ts";
import type { Account } from "./claude.ts";

/** Where the accounts Looper sets up are kept, one directory each, shared by every repo. */
export const accountsDir = join(
  process.env.XDG_CONFIG_HOME ?? join(homedir(), ".config"),
  "looper",
  "claude"
);

/** An account as one line: `you@example.com (max)`. */
export function describeIdentity(account: Account): string {
  return (
    `${account.email ?? "unknown"}` +
    `${account.subscriptionType ? ` (${account.subscriptionType})` : ""}` +
    `${account.orgName ? `, ${account.orgName}` : ""}`
  );
}

/**
 * Make sure the wakes have a logged-in account, asking on the terminal for
 * whatever is needed, and hand it back. Exits with a message when it cannot be
 * done — a missing CLI, or no terminal to ask on — since every wake would fail
 * the same way.
 */
export async function settleAccount(config: Config): Promise<Account> {
  const interactive = Boolean(process.stdin.isTTY);

  // The first run of a repo with nothing chosen yet: whose account is it to be?
  if (!config.claudeConfigDir && !config.defaultAccountChosen && interactive) {
    const current = read(config);
    console.log(
      current.loggedIn
        ? `\nThis computer's Claude account is ${describeIdentity(current)}.`
        : "\nClaude is not logged in on this computer."
    );
    const answer = await ask(
      current.loggedIn
        ? "Should this repo's wakes run as that account? [Y/n] "
        : "Should this repo use this computer's own account once it is logged in? [Y/n] "
    );
    if (/^n/i.test(answer)) {
      config.claudeConfigDir = await setUpOther(config);
    } else {
      upsertEnv(repoEnvPath(config.repo), { LOOPER_CLAUDE_ACCOUNT: "default" });
      config.defaultAccountChosen = true;
      console.log(`Saved to ${repoEnvPath(config.repo)}\n`);
    }
  }

  const account = read(config);
  if (account.loggedIn) return account;

  const where = config.claudeConfigDir;
  console.error(`Claude is not logged in${where ? ` in ${where}` : ""}.`);
  if (!interactive) {
    console.error(
      `Run Looper from a terminal to log in, or log in by hand with:\n  ` +
        (where ? `CLAUDE_CONFIG_DIR=${where} claude auth login` : "claude auth login")
    );
    process.exit(1);
  }
  const answer = await ask("Log in now? [Y/n] ");
  if (/^n/i.test(answer)) process.exit(1);
  login(where, {});
  const after = read(config);
  if (!after.loggedIn) {
    console.error("Still not logged in, so there is nothing for the wakes to run as.");
    process.exit(1);
  }
  console.log(`Logged in as ${describeIdentity(after)}.\n`);
  return after;
}

/**
 * Set up an account other than the computer's own: ask which, make it a
 * directory of its own, log it in there, and pin the repo to it. An account set
 * up for one repo can be picked again by name for another, so a second repo on
 * the same account does not mean logging in twice.
 */
async function setUpOther(config: Config): Promise<string> {
  const existing = existsSync(accountsDir)
    ? readdirSync(accountsDir, { withFileTypes: true })
        .filter((entry) => entry.isDirectory())
        .map((entry) => entry.name)
    : [];
  if (existing.length) {
    console.log("\nAccounts Looper has set up before:");
    for (const name of existing) {
      const account = whoseAccountIn(join(accountsDir, name));
      console.log(
        `  ${name} — ${account?.loggedIn ? describeIdentity(account) : "not logged in"}`
      );
    }
    console.log("Give one of those names to use it again, or a new name to set one up.");
  } else {
    console.log(
      "\nLooper keeps each extra account in a directory of its own, so it never touches\n" +
        "the login the rest of this computer uses."
    );
  }

  const email = await ask("Email address of the account (blank to choose in the browser): ");
  const suggested = suggestName(email, config.repo);
  const name = (await ask(`A short name for it [${suggested}]: `)) || suggested;
  if (!/^[\w.-]+$/.test(name)) throw new Error(`Use letters, digits, dots and dashes for the name, not ${name}.`);
  const dir = join(accountsDir, name);

  const already = existsSync(dir) ? whoseAccountIn(dir) : null;
  if (already?.loggedIn) {
    console.log(`${name} is already logged in as ${describeIdentity(already)}.`);
  } else {
    const billing = await ask(
      "Pay for its usage with (1) a Claude subscription, or (2) Anthropic Console API credit? [1] "
    );
    mkdirSync(dir, { recursive: true });
    console.log(`\nLogging in to ${dir} — a browser window will open.\n`);
    login(dir, { console: billing.trim() === "2", email: email || undefined });
    const now = whoseAccountIn(dir);
    if (!now?.loggedIn) throw new Error(`The login did not complete; run Looper again to retry.`);
    console.log(`\nLogged in as ${describeIdentity(now)}.`);
    if (email && now.email && now.email.toLowerCase() !== email.toLowerCase()) {
      console.log(`That is not ${email}. Run \`looper\` again and answer n to choose again.`);
    }
  }

  upsertEnv(repoEnvPath(config.repo), { LOOPER_CLAUDE_CONFIG_DIR: dir });
  console.log(`Saved to ${repoEnvPath(config.repo)}\n`);
  return dir;
}

/** `claude auth login`, run in the foreground on this terminal, in `dir` when given. */
function login(dir: string | null, opts: { console?: boolean; email?: string }): void {
  const args = ["auth", "login"];
  if (opts.console) args.push("--console");
  if (opts.email) args.push("--email", opts.email);
  const result = spawnSync("claude", args, {
    stdio: "inherit",
    env: dir ? { ...process.env, CLAUDE_CONFIG_DIR: dir } : process.env,
  });
  if (result.error) throw new Error(`Could not run claude auth login: ${result.error.message}`);
}

/** The account in a config dir, or null if `claude` could not say. */
function whoseAccountIn(dir: string): Account | null {
  try {
    return whoseAccount({ claudeConfigDir: dir });
  } catch {
    return null;
  }
}

/** The account a config would use, or an exit if `claude` is not there to ask. */
function read(config: Config): Account {
  try {
    return whoseAccount(config);
  } catch (error) {
    console.error(`Could not ask claude which account it is using: ${(error as Error).message}`);
    console.error("Is the `claude` CLI installed and on PATH?");
    process.exit(1);
  }
}

/** A default name for a new account: the email's local part, or the repo's name. */
function suggestName(email: string, repo: string): string {
  const base = email ? email.split("@")[0] : (repo.split(/[\\/]/).pop() ?? "looper");
  return base.toLowerCase().replace(/[^\w.-]+/g, "-") || "looper";
}
