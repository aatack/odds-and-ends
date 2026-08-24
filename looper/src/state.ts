// Everything Looper remembers between wakes, kept in `.looper/` inside the repo
// the agent works in.
//
// It lives in the repo so that the state, the logs and the work are all in one
// place — but `.looper/.gitignore` ignores the whole directory, so none of it can
// ever end up in a commit the agent makes.

import {
  appendFileSync,
  existsSync,
  mkdirSync,
  readFileSync,
  renameSync,
  writeFileSync,
} from "node:fs";
import { join } from "node:path";
import type { Incoming } from "./telegram.ts";

/** How a wake ended, which is what decides how long to wait before the next. */
export type Outcome = "done" | "asked" | "failed" | "limited";

export interface LastRun {
  /** When the wake finished, as an ISO string. */
  at: string;
  outcome: Outcome;
  /** Claude's session id, so `claude --resume <id>` can be used to look inside. */
  sessionId: string | null;
  /** The agent's closing words, kept to hand to its next self. */
  text: string;
  durationMs: number;
  costUsd: number | null;
  /** Why it failed, when it did. */
  error?: string;
}

export interface StateData {
  /** How many wakes this directory has had. */
  runs: number;
  /** Telegram's update offset, so a restart doesn't replay old messages. */
  telegramOffset: number;
  /** Your messages that the agent hasn't been shown yet. */
  pending: Incoming[];
  /** Set when the agent asked something, cleared when a reply is handed over. */
  awaitingReply: boolean;
  /** Consecutive failed wakes, which is what the stall backoff is measured in. */
  failures: number;
  lastRun: LastRun | null;
}

/** One thing the agent sent you during a wake, as recorded by the notify tool. */
export interface Sent {
  at: string;
  kind: "tell" | "ask";
  text: string;
  files?: string[];
}

const empty: StateData = {
  runs: 0,
  telegramOffset: 0,
  pending: [],
  awaitingReply: false,
  failures: 0,
  lastRun: null,
};

export class State {
  readonly dir: string;
  private readonly statePath: string;
  private readonly outboxPath: string;
  private readonly logPath: string;
  data: StateData;

  constructor(repo: string) {
    this.dir = join(repo, ".looper");
    this.statePath = join(this.dir, "state.json");
    this.outboxPath = join(this.dir, "outbox.jsonl");
    this.logPath = join(this.dir, "looper.log");
    mkdirSync(join(this.dir, "runs"), { recursive: true });
    // A gitignore that ignores itself, so the agent committing everything in the
    // repo can never commit Looper's state, logs, or its own env file.
    const ignore = join(this.dir, ".gitignore");
    if (!existsSync(ignore)) writeFileSync(ignore, "*\n");
    this.data = this.read();
  }

  private read(): StateData {
    if (!existsSync(this.statePath)) return { ...empty };
    try {
      return { ...empty, ...(JSON.parse(readFileSync(this.statePath, "utf8")) as StateData) };
    } catch {
      // A truncated state file (a kill mid-write) shouldn't stop the loop; the
      // notes hold the work, and this only holds the bookkeeping.
      this.log("state.json could not be read; starting from a blank state.");
      return { ...empty };
    }
  }

  /** Persist through a temporary file, so a kill can never leave half a state. */
  save(): void {
    const temporary = `${this.statePath}.tmp`;
    writeFileSync(temporary, JSON.stringify(this.data, null, 2) + "\n");
    renameSync(temporary, this.statePath);
  }

  /** Write a line to the terminal and to `.looper/looper.log`. */
  log(line: string): void {
    const stamped = `${new Date().toISOString()} ${line}`;
    console.log(stamped);
    try {
      appendFileSync(this.logPath, stamped + "\n");
    } catch {
      /* the terminal already has it */
    }
  }

  /** Where a wake's raw stream of events is written, one JSON object per line. */
  runLog(run: number): string {
    const stamp = new Date().toISOString().replace(/[:.]/g, "-");
    return join(this.dir, "runs", `${String(run).padStart(4, "0")}-${stamp}.jsonl`);
  }

  /**
   * Take everything the agent sent during the wake that just ended. The notify
   * tool runs in its own process, so the outbox file is how it reports back; it
   * is drained here, and kept in `sent.jsonl` as a record.
   */
  drainOutbox(): Sent[] {
    if (!existsSync(this.outboxPath)) return [];
    const text = readFileSync(this.outboxPath, "utf8");
    writeFileSync(this.outboxPath, "");
    if (text.trim()) appendFileSync(join(this.dir, "sent.jsonl"), text);
    const sent: Sent[] = [];
    for (const line of text.split("\n")) {
      if (!line.trim()) continue;
      try {
        sent.push(JSON.parse(line) as Sent);
      } catch {
        /* a half-written line; the message itself already went to Telegram */
      }
    }
    return sent;
  }

  /** Record an arriving message, both in the state and in a durable log. */
  receive(messages: Incoming[]): void {
    if (!messages.length) return;
    this.data.pending.push(...messages);
    this.data.telegramOffset = Math.max(
      this.data.telegramOffset,
      ...messages.map((m) => m.updateId + 1)
    );
    appendFileSync(
      join(this.dir, "inbox.jsonl"),
      messages.map((m) => JSON.stringify(m)).join("\n") + "\n"
    );
    this.save();
  }

  /** Hand the pending messages to a wake, clearing them and the question flag. */
  takePending(): Incoming[] {
    const pending = this.data.pending;
    this.data.pending = [];
    if (pending.length) this.data.awaitingReply = false;
    this.save();
    return pending;
  }
}
