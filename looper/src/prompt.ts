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
  // Nothing here numbers the sessions or calls them anything: an agent told it is
  // on "wake 314" writes about wake 314 in its notes, its commits and its
  // messages, and none of that is any use to anybody.
  const sections = [
    standing(config),
    situation(config, state, messages, conversation),
    tidy ? tidying(config) : working(config),
  ];
  return sections.join("\n\n");
}

function standing(config: Config): string {
  return `You work on one long task, alone and in the background. No person watches this
session or waits for a reply. When you stop, the task continues later in a new
session.

${where(config)} Do not push to a remote. Do not publish anything.

The directory is an Obsidian vault. The task is in ${config.task}. The user wrote
it. Do not change it. Your notes are in ${config.notesDir}/. Start from
${config.notesDir}/Index.md. Use [[wikilinks]] to link the notes. Do not change
the .obsidian/ directory.

You do not remember earlier sessions. Only your notes and this prompt continue
from one session to the next.

Write all your English text in ASD-STE100 Simplified Technical English. This
includes your notes, your commit messages and your messages to the user. Use
short sentences, one instruction in each sentence, the active voice, simple
tenses, and words with one meaning.`;
}

/** Where the agent works: a whole repo, or one directory of a bigger one. */
function where(config: Config): string {
  if (config.gitRoot === config.repo) {
    return `Your working directory is ${config.repo}. It is a git repository. Do not
read or write files outside it.`;
  }
  return `Your working directory is ${config.repo}. It is a subdirectory of the git
repository at ${config.gitRoot}. You can read the rest of the repository. Change
and commit only the files in your working directory.`;
}

function situation(
  config: Config,
  state: StateData,
  messages: Incoming[],
  conversation: Exchange[]
): string {
  const parts: string[] = [];

  if (!state.lastRun) {
    parts.push(
      `There are no earlier sessions in this directory. If there are no notes, you
decide how to organize them.`
    );
  } else {
    const { at, outcome, text, error, tidy } = state.lastRun;
    const ago = describeGap(Date.parse(at));
    const ended = tidy
      ? "put the notes in order"
      : outcome === "done" || outcome === "asked"
        ? "stopped correctly"
        : outcome === "limited"
          ? "stopped because of a usage limit"
          : outcome === "overloaded"
            ? "did not start, because the API was overloaded"
            : `failed (${error ?? "unknown error"})`;
    parts.push(`The last session ${ended} ${ago}.`);
    if (text.trim()) {
      parts.push(`Its last message was:\n\n${indent(tail(text, handoffLimit))}`);
    }
  }

  const talk = describeConversation(conversation, messages);
  if (talk) {
    parts.push(
      messages.length
        ? `This is all the conversation between you and the user, the oldest message
first. The messages with the mark NEW came after the last session. They have
priority over your plan. A newer message can change an older one.\n\n${indent(talk)}`
        : `This is all the conversation between you and the user, the oldest message
first. A newer message can change an older one.\n\n${indent(talk)}`
    );
  }
  if (!messages.length && state.awaitingReply) {
    parts.push(
      `The user did not answer your question yet. Do work that does not depend on the
answer. If there is no such work, stop.`
    );
  }

  const repo = describeRepo(config.repo);
  if (repo) parts.push(`The condition of your working directory in git:\n\n${indent(repo)}`);

  return parts.join("\n\n");
}

/**
 * The brief for a tidy-up: every so often, a new session that does no work on
 * the task and only puts the notes back in order. Sessions edit the notes as
 * they go, but each one in a hurry and from inside its own context, so the notes
 * drift towards a log however they are told; a reader with no context of its own
 * is what notices.
 */
function tidying(config: Config): string {
  return `## Put the notes in order

Do not work on the task in this session. Only put the notes in
${config.notesDir}/ in order. Read them as a new reader.

1. Read the task, all the notes, and sufficient parts of the repository and its
   git log.
2. Remove data that is not true now. Remove history. Remove todos that are
   complete.
3. Put the todos in order, with the most valuable todo first.
4. Merge notes that contain the same data. Repair broken [[wikilinks]].
5. Make sure that ${config.notesDir}/Index.md is short and has a link to each
   note.
6. Do not remove findings that took much work to find.
7. Do not change the task note or the code.
8. Commit the notes.
9. Stop.`;
}

function working(config: Config): string {
  return `## How to work

Read the task and your notes. Select the most valuable next item. Do it fully.
Make sure that it operates correctly. Commit it with a clear message. You make
the decisions. Do not ask for permission to continue.

Before you stop, update the notes. They show the current condition of the work,
not its history. Git keeps the history. Keep only:

- Findings: facts that are not clear from the code, and decisions with their
  reasons.
- Todos: the work that is not complete, with the most valuable item first.
  Remove a todo when it is complete.
- Open questions.

Change a note where it is. Remove data that is not true now. Keep
${config.notesDir}/Index.md short, with links to the other notes. Commit the
notes.

To speak to the user, use \`mcp__looper__tell_user\` for news that they want to
know. Use \`mcp__looper__ask_user\` for a question that stops your work. Use these
tools rarely. After you ask a question, write in the notes where you stopped.
Then stop. The answer comes in a later prompt.

Stop when you complete the item. Also stop when a problem stops your work and
the notes show why.`;
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
  // Told apart by Telegram's id, not by when and what: Telegram stamps a message
  // to the second, so two quick identical ones would otherwise be shown as one.
  const fresh = new Set(messages.map((message) => message.updateId));
  const known = new Set(conversation.map((said) => said.id));
  const all = [...conversation];
  for (const message of messages) {
    if (!known.has(message.updateId)) {
      all.push({ id: message.updateId, at: message.at, from: "user", text: message.text });
    }
  }
  all.sort((a, b) => a.at - b.at);
  return all
    .map((exchange) => {
      const when = new Date(exchange.at).toISOString().slice(0, 16).replace("T", " ");
      if (exchange.from === "agent") {
        const how = exchange.kind === "ask" ? "you asked" : "you said";
        return `[${when}] ${how}: ${tail(exchange.text, ownMessageLimit).replace(/\n/g, "\n    ")}`;
      }
      const mark = exchange.id !== undefined && fresh.has(exchange.id) ? "NEW " : "";
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
  // Limited to this directory, which is all of the repo when Looper runs at the
  // top, and only the agent's part of it when it runs in a subdirectory: other
  // people's commits and edits elsewhere are not its business.
  const log = git("log", "-3", "--format=%h %s", "--", ".");
  const dirty = git("status", "--porcelain", "--", ".");
  const lines = [`The branch is ${branch}.`];
  if (log) lines.push("The last commits are:", ...log.split("\n").map((line) => `  ${line}`));
  lines.push(
    dirty
      ? `${dirty.split("\n").length} file(s) have changes that are not committed.`
      : "All changes are committed."
  );
  return lines.join("\n");
}
