// The prompt the agent is woken with. This is the part of Looper that decides
// what the thing actually is, so it is kept in one piece and read as prose.
//
// Every wake is the same standing brief plus a short account of the situation:
// what happened last time, what you have said since, and where the repo stands.
// The agent's memory is its notes — markdown files in the repo, which doubles as
// an Obsidian vault — not this prompt: the prompt only carries what the notes
// cannot know.

import { execFileSync } from "node:child_process";
import type { Config } from "./config.ts";
import type { Incoming } from "./telegram.ts";
import type { Exchange, StateData } from "./state.ts";

/** How much of the last wake's closing words to carry over. */
const handoffLimit = 3000;

/**
 * How much of each of the agent's own Telegram messages to show it again. Its
 * side is there so that your replies make sense, and the gist does that; yours
 * are always shown whole.
 */
const ownMessageLimit = 1500;

export interface PromptInput {
  config: Config;
  state: StateData;
  /** Your messages since the agent last ran. */
  messages: Incoming[];
  /** Everything said on Telegram before now, both ways, oldest first. */
  conversation: Exchange[];
  /** Spend this wake tidying the notes rather than working on the task. */
  tidy?: boolean;
}

export function buildPrompt({ config, state, messages, conversation, tidy }: PromptInput): string {
  const wake = state.runs + 1;
  const sections = [
    standing(config),
    situation(config, state, messages, conversation, wake),
    tidy ? tidying(config) : working(config),
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
- The repo is also an Obsidian vault, and your task and your memory are notes
  in it: plain markdown files, read and written with your ordinary file tools.
  The task is ${config.task}, written by the user — read it before you decide
  anything, and leave it as they wrote it. Your own notes live in
  ${config.notesDir}/, starting from ${config.notesDir}/Index.md.
- Write them for Obsidian: link notes to each other with [[wikilinks]] (by note
  name, without the .md), one topic to a note. Commit them alongside the work
  they describe. Leave .obsidian/ alone — it is the user's editor settings.
- You have no memory of earlier wakes beyond what is written in those notes and
  what appears below. So anything your next self will need has to be written
  into the notes before you stop. Nothing else survives.`;
}

function situation(
  config: Config,
  state: StateData,
  messages: Incoming[],
  conversation: Exchange[],
  wake: number
): string {
  const parts: string[] = [`## This wake (number ${wake})`];

  if (!state.lastRun) {
    parts.push(
      `This is the first wake in this directory. Start by reading the task note and
whatever is already in ${config.notesDir}/, then get your bearings in the repo.
If there are no notes yet, they are yours to lay out.`
    );
  } else {
    const { at, outcome, durationMs, text, error, tidy } = state.lastRun;
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
      `Your last wake ${ended}, ${ago}, after ${Math.round(durationMs / 60_000)} minutes of ` +
        (tidy ? "tidying the notes. They should be in good order to work from." : "work.")
    );
    if (text.trim()) {
      parts.push(`It signed off with:\n\n${indent(tail(text, handoffLimit))}`);
    }
  }

  const talk = describeConversation(conversation, messages);
  if (talk) {
    parts.push(
      messages.length
        ? `Everything said between you and the user on Telegram, oldest first. The
messages marked NEW arrived since your last wake: they are the most important
thing in this prompt, and take priority over whatever you had planned. The older
ones still stand unless the user has since said otherwise.\n\n${indent(talk)}`
        : `Everything said between you and the user on Telegram, oldest first. Nothing
new has arrived since your last wake, but what is here still stands unless the
user has since said otherwise.\n\n${indent(talk)}`
    );
  }
  if (!messages.length && state.awaitingReply) {
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

/**
 * The brief for a tidy-up wake: every so often, a new session that does no work
 * on the task and only puts the notes back in order. Wakes edit the notes as they
 * go, but each one in a hurry and from inside its own context, so the notes drift
 * towards a log however they are told; a reader with no context of its own is
 * what notices.
 */
function tidying(config: Config): string {
  return `## This wake is for tidying the notes

This wake is not for work on the task. It is a new session, with none of your
earlier context, and its only job is to put the notes in ${config.notesDir}/ back
in order, so that the wakes after it can work from them. Read them as someone
coming to them cold — that is what you are.

1. Read the task note, every note in ${config.notesDir}/, and enough of the repo
   and its recent git log to know where things really stand.
2. Make the notes say how things stand now, and nothing else:
   - Delete what is no longer true, what has been superseded, and anything that
     is only history — what was done, and when. Git keeps that.
   - Remove todos that are done, and put the rest in order, most valuable first.
   - Check findings against the code where that is cheap, and fix or delete the
     ones that are wrong.
   - Merge notes that say the same thing; split a note that covers several
     topics; fix broken [[wikilinks]]; make sure every note is linked from
     ${config.notesDir}/Index.md, and that the index is short.
   - Keep what the next wake needs: findings, the reasons behind decisions,
     what was ruled out and why, open questions. Shorter is better, but do not
     throw away anything that cost real work to find out.
3. Leave the task note as the user wrote it, and do not change any code.
4. Commit the notes, with a message that says they were tidied, and end your
   turn.

Do not message the user about the tidy-up. If you find something in the notes
that they need to know — a question nobody asked them, a problem nobody told
them about — use \`mcp__looper__tell_user\` or \`mcp__looper__ask_user\` as normal.`;
}

function working(config: Config): string {
  return `## How to work

Every wake is the same three steps: read, act, write.

**Read.** The task note, then your notes from ${config.notesDir}/Index.md
outwards, including whatever your last self left there. That is where you find
out what has already been tried and what was going to happen next.

**Act.** Pick the most valuable next thing, do it properly, check it works, and
commit it. Small commits with clear messages. Finishing one thing beats starting
three. You are trusted to decide: do not ask for permission to proceed, and do
not wait to be told which option to take — choose, write down why, and go. Leave
the repo working; if you cannot, say so plainly in the notes.

**Write.** Before you stop, bring the notes up to date. They describe how things
stand now, not what happened: git already keeps the history, so the notes are
not a log, a diary or a list of what each wake did. Keep in them only what your
next self needs to carry on:

- **Findings** — what is true about the problem and the code that is not obvious
  from reading it: how things work, what was tried and ruled out (and why), the
  decisions in force and their reasons.
- **Todos** — what is left, most valuable first, with enough detail to start on.
  Remove a todo when it is done rather than ticking it off; the commit is the
  record that it happened.
- **Open questions** — what you are waiting on the user for, and what you are
  unsure of.

Edit notes in place rather than appending to them: when something you wrote is
no longer true, change it or delete it. A note that grows with every wake is
being used as a log. Keep ${config.notesDir}/Index.md short — the current
status in a few lines, the todos, and links to the notes that hold the findings —
and give each finding its own note when it is more than a line or two. Never end a wake
without the notes saying where things stand, because there is nothing else your
next self will have. Commit them.

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

/**
 * The whole Telegram conversation as lines to read, with the user's new messages
 * marked. A new message that is not in the logs yet — one handed back after a
 * wake that never read it is, but a hand-edited state might not be — is added
 * rather than lost.
 */
function describeConversation(conversation: Exchange[], messages: Incoming[]): string {
  const key = (said: { at: number; text: string }) => `${said.at} ${said.text}`;
  const fresh = new Set(messages.map(key));
  const known = new Set(conversation.filter((said) => said.from === "user").map(key));
  const all = [...conversation];
  for (const message of messages) {
    if (!known.has(key(message))) all.push({ at: message.at, from: "user", text: message.text });
  }
  all.sort((a, b) => a.at - b.at);
  return all
    .map((exchange) => {
      const when = new Date(exchange.at).toISOString().slice(0, 16).replace("T", " ");
      if (exchange.from === "agent") {
        const how = exchange.kind === "ask" ? "you asked" : "you said";
        return `[${when}] ${how}: ${tail(exchange.text, ownMessageLimit).replace(/\n/g, "\n    ")}`;
      }
      const mark = fresh.has(key(exchange)) ? "NEW " : "";
      return `[${when}] ${mark}user: ${exchange.text.replace(/\n/g, "\n    ")}`;
    })
    .join("\n");
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
