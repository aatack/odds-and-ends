// The prompt the agent is woken with. This is the part of Looper that decides
// what the thing actually is, so it is kept in one piece and read as prose.
//
// Every wake is the same standing brief plus a short account of the situation:
// what happened last time, what you have said since, and where the repo stands.
// The agent's memory is the notes MCP, not this prompt — the prompt only carries
// what the notes cannot know.

import { execFileSync } from "node:child_process";
import type { Config } from "./config.ts";
import type { Incoming } from "./telegram.ts";
import type { StateData } from "./state.ts";

/** How much of the last wake's closing words to carry over. */
const handoffLimit = 3000;

export interface PromptInput {
  config: Config;
  state: StateData;
  /** Your messages since the agent last ran. */
  messages: Incoming[];
}

export function buildPrompt({ config, state, messages }: PromptInput): string {
  const wake = state.runs + 1;
  const sections = [
    standing(config),
    situation(config, state, messages, wake),
    working(),
  ];
  return sections.join("\n\n");
}

function standing(config: Config): string {
  return `You are Looper: an agent that works on one long-running task, on its own, in the
background. Nobody is watching this session and nobody is waiting on a reply.
You will be woken again after it ends, so the job of each wake is to move the
task on and leave it in a state your next self can pick straight up.

## Where you are

- Your working directory is ${config.repo}, a git repo. Everything you build,
  write or run goes in there. Do not read or write anything outside it, and do
  not push to a remote or publish anything anywhere.
- Your task, your memory and your working notes all live in the notes server
  (the \`mcp__notes__*\` tools). The task is note ${config.task}: read it, and
  what sits under it, before you decide anything.
- You have no memory of earlier wakes beyond what is written in those notes and
  what appears below. So anything your next self will need — what you learnt,
  what you tried, what you would do next — has to be written into the notes
  before you stop. Nothing else survives.`;
}

function situation(
  config: Config,
  state: StateData,
  messages: Incoming[],
  wake: number
): string {
  const parts: string[] = [`## This wake (number ${wake})`];

  if (!state.lastRun) {
    parts.push(
      `This is the first wake in this directory. Start by reading the task note and
everything under it, then get your bearings in the repo. If it is empty, it is
yours to lay out.`
    );
  } else {
    const { at, outcome, durationMs, text, error } = state.lastRun;
    const ago = describeGap(Date.parse(at));
    const ended =
      outcome === "done"
        ? "ended normally"
        : outcome === "asked"
          ? "ended after asking the user something"
          : outcome === "limited"
            ? "was cut short by a usage cap"
            : outcome === "overloaded"
              ? "never really ran: the API was overloaded"
              : `ended badly (${error ?? "unknown error"})`;
    parts.push(
      `Your last wake ${ended}, ${ago}, after ${Math.round(durationMs / 60_000)} minutes of work.`
    );
    if (text.trim()) {
      parts.push(`It signed off with:\n\n${indent(tail(text, handoffLimit))}`);
    }
  }

  if (messages.length) {
    const rendered = messages
      .map((message) => `[${new Date(message.at).toISOString()}] ${message.text}`)
      .join("\n");
    parts.push(
      `The user has sent you this since — it is the most important thing in this
prompt, and takes priority over whatever you had planned:\n\n${indent(rendered)}`
    );
  } else if (state.awaitingReply) {
    parts.push(
      `You asked the user something and they have not answered yet — they may simply be
asleep, and the answer may still arrive. Get on with something that does not
depend on it. If there is genuinely nothing else worth doing, say so briefly in
the notes and stop.`
    );
  }

  const repo = describeRepo(config.repo);
  if (repo) parts.push(`Where the repo stands:\n\n${indent(repo)}`);

  return parts.join("\n\n");
}

function working(): string {
  return `## How to work

Every wake is the same three steps: read, act, write.

**Read.** The task note and what sits under it, including whatever your last self
left there. That is where you find out what has already been tried and what was
going to happen next.

**Act.** Pick the most valuable next thing, do it properly, check it works, and
commit it. Small commits with clear messages. Finishing one thing beats starting
three. You are trusted to decide: do not ask for permission to proceed, and do
not wait to be told which option to take — choose, write down why, and go. Leave
the repo working; if you cannot, say so plainly in the notes.

**Write.** Before you stop, write what you did, what you found out and what you
would do next back into the notes. The notes are yours to use as you see fit —
lay them out however suits the work — but never end a wake without leaving
something the next one can pick up, because there is nothing else it will have.
Write them as notes: a note per point, nested, in the voice of the notes already
there, not as one long paragraph.

## Reaching the user

You have two tools, and they are the only way to reach anybody:

- \`mcp__looper__tell_user\` — something they would want to know: a result, a
  finished piece, a decision you took that changes the shape of the work. They
  may not reply. Use it sparingly: a few times a day at most, not every wake.
- \`mcp__looper__ask_user\` — a question you genuinely cannot get past: a
  decision only they can make, a credential you do not have, a fork in the road
  where both ways are expensive. Ask, write down where you got to, and end your
  turn. Their answer will be in the prompt at your next wake.

Silence is the normal state. A wake that quietly did good work and wrote it down
is a good wake.

## Ending your turn

Stop when you have finished the thing you picked, or when you are blocked and
have written down why. Ending your turn is expected — you will be woken again
shortly. Do not pad the wake out, and do not start something large you cannot
leave in a sane state.`;
}

// ---------------------------------------------------------------------------
// small helpers

function indent(text: string): string {
  return text
    .split("\n")
    .map((line) => `  ${line}`)
    .join("\n");
}

/** Keep the end of a long message: the conclusion is the part worth carrying. */
function tail(text: string, limit: number): string {
  const trimmed = text.trim();
  return trimmed.length <= limit ? trimmed : `[...] ${trimmed.slice(-limit)}`;
}

function describeGap(from: number): string {
  const minutes = Math.round((Date.now() - from) / 60_000);
  if (minutes < 60) return `${minutes} minutes ago`;
  const hours = Math.round(minutes / 60);
  return hours < 48 ? `${hours} hours ago` : `${Math.round(hours / 24)} days ago`;
}

/**
 * A few lines of git, so the agent knows where it left the repo without having
 * to spend a tool call finding out. Silent on anything that fails — a directory
 * that isn't a repo yet is a normal way to start.
 */
function describeRepo(repo: string): string | null {
  const git = (...args: string[]) => {
    try {
      // stderr is discarded: a repo with no commits yet makes git complain, and
      // that complaint is not news to anyone.
      return execFileSync("git", args, {
        cwd: repo,
        encoding: "utf8",
        timeout: 5000,
        stdio: ["ignore", "pipe", "ignore"],
      }).trim();
    } catch {
      return "";
    }
  };
  const branch = git("rev-parse", "--abbrev-ref", "HEAD");
  if (!branch) return null;
  const log = git("log", "-3", "--format=%h %s");
  const dirty = git("status", "--porcelain");
  const lines = [`On branch ${branch}.`];
  if (log) lines.push("Last commits:", ...log.split("\n").map((line) => `  ${line}`));
  lines.push(
    dirty
      ? `${dirty.split("\n").length} file(s) uncommitted — probably yours from last time.`
      : "Working tree clean."
  );
  return lines.join("\n");
}
