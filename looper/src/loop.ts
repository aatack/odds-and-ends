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
import type { Outcome } from "./state.ts";
import type { Telegram } from "./telegram.ts";

/** How long a cap is given past its stated reset, so the first retry isn't a second too early. */
const capBuffer = 2 * 60_000;

/** Nothing waits longer than this, however bad things get. */
const maxWait = 24 * 3_600_000;

/** Failures in a row before you are told the loop is stuck. */
const complainAfter = 3;

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
      const prompt = buildPrompt({ config, state: state.data, messages });
      const run = state.data.runs + 1;
      const resume = config.sessionMode === "resume" ? (state.data.lastRun?.sessionId ?? null) : null;

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
      state.log(
        `wake ${run} started (session ${wake.sessionId.slice(0, 8)}, ${config.model}` +
          `${messages.length ? `, ${messages.length} message(s) from you` : ""})`
      );

      const result = await wake.completion;
      this.stopCurrentWake = null;
      if (this.stopping) break;

      const sent = state.drainOutbox();
      const asked = sent.some((message) => message.kind === "ask");
      const outcome = describe(result, asked);
      this.record(outcome, result);

      // A wake that never got as far as a single tool call never really read the
      // prompt, so anything you had said goes back in the queue for the next one
      // rather than being lost with it.
      if (outcome === "failed" && messages.length && !result.toolCalls && !result.text.trim()) {
        state.data.pending.unshift(...messages);
        state.save();
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
  private record(outcome: Outcome, result: RunResult): void {
    const { state } = this.options;
    state.data.runs += 1;
    state.data.awaitingReply = outcome === "asked";
    state.data.failures = outcome === "failed" ? state.data.failures + 1 : 0;
    state.data.lastRun = {
      at: new Date().toISOString(),
      outcome,
      sessionId: result.sessionId,
      text: result.text,
      durationMs: result.durationMs,
      costUsd: result.costUsd,
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
      return reset
        ? { until: reset + capBuffer, reason: "usage cap, until it resets", wakeOnMessage: false }
        : { until: now + timing.limit, reason: "usage cap, no reset given", wakeOnMessage: false };
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
    return { until: now + timing.turn, reason: "between wakes", wakeOnMessage: true };
  }

  /**
   * Sleep until `until`, or until you have finished talking. A message doesn't
   * wake the agent on its own: the wait continues until the grace period has
   * passed with nothing new, so three messages in a row arrive as one thought
   * rather than interrupting the loop three times.
   */
  private async wait(until: number, wakeOnMessage: boolean): Promise<void> {
    const { state, config } = this.options;
    while (!this.stopping && Date.now() < until) {
      if (wakeOnMessage && state.data.pending.length) {
        const last = Math.max(...state.data.pending.map((message) => message.at));
        const settled = Date.now() - Math.max(last, 0) >= config.timing.grace;
        if (settled) {
          state.log(`waking early: ${state.data.pending.length} message(s) from you`);
          return;
        }
      }
      await sleep(1000);
    }
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
        if (messages.length) {
          state.receive(messages);
          for (const message of messages) {
            state.log(`you said: ${message.text.replace(/\s+/g, " ").slice(0, 120)}`);
          }
        } else if (offset !== state.data.telegramOffset) {
          state.data.telegramOffset = offset;
          state.save();
        }
      } catch (error) {
        state.log(`telegram poll failed: ${(error as Error).message}`);
        await sleep(15_000);
      }
    }
  }
}

function describe(result: RunResult, asked: boolean): Outcome {
  if (result.limit) return "limited";
  if (!result.ok) return "failed";
  return asked ? "asked" : "done";
}

function sleep(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}
