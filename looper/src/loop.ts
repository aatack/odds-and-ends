// The loop itself: wake the agent, see how the wake ended, decide how long to
// leave it, and go again.
//
// The whole design is in that decision. A wake that ended cleanly is followed by
// a short gap; a wake that failed backs off, doubling, because a loop that
// retries a broken thing every five minutes burns a night's tokens on nothing; a
// wake that asked you something waits for your answer, and a spent usage cap
// waits for the cap. Anything you send lands in the next prompt, and — once you
// have stopped typing — cuts the waiting short.

import { formatDuration } from "./config.ts";
import type { Config } from "./config.ts";
import { runWake } from "./claude.ts";
import type { RunResult } from "./claude.ts";
import { buildPrompt } from "./prompt.ts";
import { State } from "./state.ts";
import type { Outcome, StateData } from "./state.ts";
import type { Incoming, Telegram } from "./telegram.ts";
import { nextMove, readCommand } from "./commands.ts";
import type { Command } from "./commands.ts";

/** How long a cap is given past its stated reset, so the first retry isn't a second too early. */
const capBuffer = 2 * 60_000;

/** Nothing waits longer than this, however bad things get. */
const maxWait = 24 * 3_600_000;

/** Failures in a row before you are told the loop is stuck. */
const complainAfter = 3;

/**
 * Overloads in a row before you are told. Higher than `complainAfter` because the
 * gap starts at two minutes and doubles: by the sixth the API has been refusing
 * work for well over an hour, which is worth a message, and the five before it
 * are weather.
 */
const complainAfterOverloads = 6;

export interface LoopOptions {
  config: Config;
  state: State;
  telegram: Telegram;
  /** Looper's own directory, so the notify server can be found. */
  looperDir: string;
  /** Run one wake and stop, rather than looping. */
  once: boolean;
}

export class Loop {
  private readonly options: LoopOptions;
  private stopping = false;
  private stopCurrentWake: (() => void) | null = null;
  /** True while a wake is running, so a command can say what it will do. */
  private running = false;
  /** Set by /go: the next wait ends at once. */
  private goRequested = false;
  /** Set while Telegram refuses to poll because something else is, so it is said once. */
  private conflicted = false;

  constructor(options: LoopOptions) {
    this.options = options;
  }

  /** Ask the loop to finish: the running wake is killed and nothing new starts. */
  stop(): void {
    this.stopping = true;
    this.stopCurrentWake?.();
  }

  async run(): Promise<void> {
    const { config, state, once } = this.options;
    // Listening runs alongside the wakes and is never awaited: it sits in a
    // 25-second poll most of the time, and nothing is owed to it when the loop is
    // done. It swallows its own errors, so nothing here can be left unhandled.
    void this.listen();

    while (!this.stopping) {
      const messages = state.takePending();
      // Starting a wake answers both commands: /go has had its way, and /wait
      // only ever asked for the one longer pause.
      this.goRequested = false;
      if (state.data.waitLonger) {
        state.data.waitLonger = false;
        state.save();
      }
      const tidy = tidyDue(config, state.data, messages.length);
      const prompt = buildPrompt({
        config,
        state: state.data,
        messages,
        conversation: state.conversation(),
        tidy,
      });
      const run = state.data.runs + 1;
      // A tidy-up always starts a new session: it is there to read the notes as
      // they stand, with nothing in its context to paper over what they leave out.
      // The wakes after it carry on from it, which is the point — they start from
      // a session that has just read everything afresh.
      const resume =
        config.sessionMode === "resume" && !tidy ? (state.data.lastRun?.sessionId ?? null) : null;

      const wake = runWake({
        config,
        prompt,
        stateDir: state.dir,
        looperDir: this.options.looperDir,
        logPath: state.runLog(run),
        resume,
        onEvent: (line) => state.log(`  · ${line}`),
      });
      this.stopCurrentWake = wake.stop;
      this.running = true;
      state.log(
        `wake ${run} started${tidy ? " to tidy the notes" : ""} ` +
          `(session ${wake.sessionId.slice(0, 8)}, ${config.model}` +
          `${messages.length ? `, ${messages.length} message(s) from you` : ""})`
      );

      const result = await wake.completion;
      this.stopCurrentWake = null;
      this.running = false;
      if (this.stopping) break;

      const sent = state.drainOutbox();
      const asked = sent.some((message) => message.kind === "ask");
      const outcome = describe(result, asked);
      this.record(outcome, result, tidy);

      // A wake that never got as far as a single tool call never really read the
      // prompt, so anything you had said goes back in the queue for the next one
      // rather than being lost with it. This covers a spent cap and an overloaded
      // API as well as a failure: in all three the agent may never have been
      // handed the prompt at all.
      const stillborn = !result.toolCalls && !result.text.trim();
      if (outcome !== "done" && outcome !== "asked" && messages.length && stillborn) {
        // Marked as having had their turn. A wake that dies on the API dies in
        // seconds, so a message that could still cut the next wait short the
        // moment it was handed back would spin the backoff away entirely: fail,
        // wake on the same message, fail, every two seconds until the cap lifted.
        state.data.pending.unshift(...messages.map((message) => ({ ...message, tried: true })));
        state.save();
      }

      // A session that cannot be resumed — deleted, or left half-written by a
      // kill — would fail the same way forever, so the id is dropped and the next
      // wake starts a new session from the notes. Only a real failure counts: a
      // wake the API never took is no evidence at all about the session.
      if (resume && outcome === "failed" && stillborn && state.data.lastRun) {
        state.data.lastRun = { ...state.data.lastRun, sessionId: null };
        state.save();
        state.log("that session could not be resumed; the next wake will start a fresh one");
      }

      state.log(
        `wake ${run} ${outcome} in ${formatDuration(result.durationMs)} ` +
          `(${result.toolCalls} tool calls${result.costUsd === null ? "" : `, $${result.costUsd.toFixed(2)}`}` +
          `${sent.length ? `, ${sent.length} message(s) sent` : ""})` +
          (result.error ? ` — ${result.error.slice(0, 300)}` : "")
      );

      if (this.fatal(result)) break;
      if (once) break;

      const plan = this.plan(outcome, result);
      state.log(`sleeping ${formatDuration(plan.until - Date.now())} — ${plan.reason}`);
      await this.wait(plan.until, plan.wakeOnMessage);
    }

    this.stopping = true;
  }

  /** Fold a wake's result and what it sent into one word for how it ended. */
  private record(outcome: Outcome, result: RunResult, tidy: boolean): void {
    const { state } = this.options;
    state.data.runs += 1;
    // Only a tidy-up that finished counts; one cut short by a cap or a failure is
    // tried again at the next quiet wake.
    if (tidy && outcome === "done") state.data.lastTidy = state.data.runs;
    state.data.awaitingReply = outcome === "asked";
    // An overload leaves the failure count alone rather than clearing it: it is
    // neither a failure nor a wake that worked, so it should not reset a backoff
    // that a real fault has earned.
    if (outcome === "failed") state.data.failures += 1;
    else if (outcome !== "overloaded") state.data.failures = 0;
    state.data.overloads = outcome === "overloaded" ? state.data.overloads + 1 : 0;
    state.data.lastRun = {
      at: new Date().toISOString(),
      outcome,
      sessionId: result.sessionId,
      text: result.text,
      durationMs: result.durationMs,
      costUsd: result.costUsd,
      ...(tidy ? { tidy: true } : {}),
      ...(result.error ? { error: result.error } : {}),
    };
    state.save();
  }

  /**
   * Some failures are not worth waiting out: a missing CLI or a rejected login
   * will fail identically forever, so the loop says so and stops.
   */
  private fatal(result: RunResult): boolean {
    const error = result.error ?? "";
    if (!/not on PATH|Could not start claude|Invalid API key|authentication/i.test(error)) {
      return false;
    }
    this.options.state.log(`stopping: ${error}`);
    void this.options.telegram
      .send(`Looper has stopped and needs you: ${error}`)
      .catch(() => undefined);
    return true;
  }

  /** When to wake next, and whether a message from you should bring that forward. */
  private plan(
    outcome: Outcome,
    result: RunResult
  ): { until: number; reason: string; wakeOnMessage: boolean } {
    const { timing } = this.options.config;
    const { failures } = this.options.state.data;
    const now = Date.now();

    if (outcome === "limited") {
      const reset = result.limit?.resetAt;
      // Nothing you can say will lift a cap, so a message doesn't cut this short.
      // A reset already in the past — a clock out of step, or a stale one read
      // from an old message — still buys a real gap, since retrying a cap the
      // instant it refuses is the one thing that must not happen.
      return reset
        ? {
            until: Math.max(reset + capBuffer, now + timing.overload),
            reason: "usage cap, until it resets",
            wakeOnMessage: false,
          }
        : { until: now + timing.limit, reason: "usage cap, no reset given", wakeOnMessage: false };
    }
    if (outcome === "overloaded") {
      // Nothing is wrong here, so the wait is short and the ceiling is the gap a
      // real failure would have got. The doubling is what stops a long outage
      // being retried every two minutes all night.
      const { overloads } = this.options.state.data;
      const ceiling = Math.max(timing.stall, timing.overload);
      const backoff = Math.min(timing.overload * 2 ** Math.max(0, overloads - 1), ceiling);
      if (overloads === complainAfterOverloads) {
        void this.options.telegram
          .send(
            `Looper has lost ${overloads} wakes in a row to an overloaded API on ` +
              `${this.options.config.repo}. Nothing is broken; it is still trying.`
          )
          .catch(() => undefined);
      }
      return {
        until: now + backoff,
        reason: `the API is overloaded (${overloads} in a row)`,
        wakeOnMessage: true,
      };
    }
    if (outcome === "failed") {
      const backoff = Math.min(timing.stall * 2 ** Math.max(0, failures - 1), maxWait);
      if (failures === complainAfter) {
        void this.options.telegram
          .send(
            `Looper has failed ${failures} wakes in a row on ${this.options.config.repo}. ` +
              `Latest: ${(result.error ?? "unknown").slice(0, 500)}`
          )
          .catch(() => undefined);
      }
      return {
        until: now + backoff,
        reason: `${failures} failure(s) in a row`,
        wakeOnMessage: true,
      };
    }
    if (outcome === "asked") {
      return { until: now + timing.question, reason: "waiting on your answer", wakeOnMessage: true };
    }
    // A wake that used no tools did nothing: it read its notes and stopped, or it
    // came straight back for a reason nothing here can see. Waking it again in
    // five minutes would be a loop that spends tokens to no end, so it gets the
    // long gap instead.
    if (!result.toolCalls) {
      return { until: now + timing.stall, reason: "that wake did nothing", wakeOnMessage: true };
    }
    return { until: now + timing.turn, reason: "between wakes", wakeOnMessage: true };
  }

  /** How long you must have been quiet before a wake may start. */
  private quiet(): number {
    const { state, config } = this.options;
    return state.data.waitLonger ? config.timing.longGrace : config.timing.grace;
  }

  /**
   * Sleep until `until`, or until you have finished talking. A message doesn't
   * wake the agent on its own: the wait continues until you have been quiet for
   * the grace period, so several messages in a row arrive as one thought rather
   * than interrupting the loop several times — and a wake that falls due while
   * you are still typing waits for you too, so a burst is never split across two
   * wakes. Only something you have said since the last wake brings a wake
   * forward: a message already handed to a wake that died waits with the rest.
   * /go ends the wait at once, whatever else is true.
   */
  private async wait(until: number, wakeOnMessage: boolean): Promise<void> {
    const { state } = this.options;
    let held = false;
    while (!this.stopping) {
      const move = nextMove({
        now: Date.now(),
        until,
        lastHeard: state.data.lastHeard,
        quiet: this.quiet(),
        fresh: state.data.pending.filter((message) => !message.tried).length,
        wakeOnMessage,
        go: this.goRequested,
      });
      if (move === "go") {
        this.goRequested = false;
        state.log("starting now: you said /go");
        return;
      }
      if (move === "message") {
        state.log(`waking early: ${state.data.pending.length} message(s) from you`);
        return;
      }
      if (move === "due") return;
      if (!held && Date.now() >= until) {
        held = true;
        state.log(`holding until you have been quiet for ${formatDuration(this.quiet())}`);
      }
      await sleep(250);
    }
  }

  /** Carry out a command, and say on Telegram what it did. */
  private obey(command: Command): void {
    const { state, telegram, config } = this.options;
    state.log(`you said /${command}`);
    // Telegram sends /start when you first open a bot; it is not for anyone.
    if (command === "start") return;
    let reply: string;
    if (command === "go") {
      this.goRequested = true;
      reply = this.running
        ? "A session is running. The next one starts as soon as it ends."
        : "Starting now.";
    } else {
      state.data.waitLonger = true;
      state.hear();
      const pause = formatDuration(config.timing.longGrace);
      reply =
        (this.running ? "The session that is running carries on. " : "") +
        `The next session waits until you have been quiet for ${pause}. Send /go to start it sooner.`;
    }
    void telegram.send(reply).catch(() => undefined);
  }

  /**
   * Long-poll Telegram for as long as the loop lives, so a message can arrive
   * while a wake is running and be waiting in the next prompt. Errors are logged
   * and retried: a flaky connection must not take the loop down.
   */
  private async listen(): Promise<void> {
    const { state, telegram } = this.options;
    while (!this.stopping) {
      try {
        const { messages, offset } = await telegram.poll(state.data.telegramOffset, 25);
        this.conflicted = false;
        const said: Incoming[] = [];
        for (const message of messages) {
          const command = readCommand(message.text);
          if (command) this.obey(command);
          else said.push(message);
        }
        // Every message is kept, and the offset moves past commands too, so
        // Telegram never hands any of them over twice.
        state.receive(said, offset);
        for (const message of said) {
          state.log(`you said: ${message.text.replace(/\s+/g, " ").slice(0, 120)}`);
        }
      } catch (error) {
        const reason = (error as Error).message;
        // Telegram lets one program at a time ask a bot for its messages, and
        // whichever asks takes them: two Loopers on one bot each lose some of
        // what you send to the other. That is worth saying plainly, once.
        if (/conflict/i.test(reason)) {
          if (!this.conflicted) {
            state.log(
              "another program is reading this bot's messages — most likely a second Looper " +
                "with the same bot token. Messages go to whichever asks first, so each running " +
                "Looper needs a bot of its own."
            );
          }
          this.conflicted = true;
        } else {
          state.log(`telegram poll failed: ${reason}`);
        }
        await sleep(15_000);
      }
    }
  }
}

/**
 * Whether the next wake should tidy the notes rather than work. One is due every
 * `tidyEvery` wakes, but never in place of a wake you have just written to: what
 * you said comes first, and the tidy-up waits for the next quiet wake.
 */
export function tidyDue(config: Config, data: StateData, messages: number): boolean {
  if (!config.tidyEvery || messages) return false;
  return data.runs - data.lastTidy >= config.tidyEvery;
}

function describe(result: RunResult, asked: boolean): Outcome {
  if (result.limit) return "limited";
  // A transient API error is not the agent failing, and a wake spent watching the
  // CLI retry ten times is not a wake at all — so it gets its own outcome, a
  // short retry, and no place in the failure count.
  if (!result.ok && result.apiError?.transient) return "overloaded";
  if (!result.ok) return "failed";
  return asked ? "asked" : "done";
}

function sleep(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}
