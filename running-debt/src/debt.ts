/**
 * The rules. Debt is measured in kilometres; everything that happens to it is a
 * step, so the balance over time is a staircase rather than a curve.
 */

import { addDays, instantOf, localTime } from "./time.ts";

export const PENALTY_KM = 3;
export const GROWTH = 1.5;
export const CYCLING_RATE = 3; // kilometres cycled per kilometre of debt cleared

export type EventKind = "penalty" | "run" | "cycle";

export interface DebtEvent {
  id: number;
  at: number; // milliseconds since the epoch
  kind: EventKind;
  km: number;
}

export type StepCause = EventKind | "growth";

/** One change to the balance, and what the balance was left at. */
export interface Step {
  at: number;
  cause: StepCause;
  km: number; // the distance behind the step, zero for growth and penalties
  change: number; // what it did to the debt, signed
  before: number;
  after: number;
  forgiven: boolean; // a Sunday whose 50% was let off
}

/**
 * What an event on its own does to a debt of `debt`. Nothing takes the debt
 * below zero: pay off more than you owe and the surplus is simply gone, rather
 * than banked against the next penalty.
 */
function apply(event: DebtEvent, debt: number): number {
  switch (event.kind) {
    case "penalty":
      return debt + PENALTY_KM;
    case "run":
      return Math.max(0, debt - event.km);
    case "cycle":
      return Math.max(0, debt - event.km / CYCLING_RATE);
  }
}

/**
 * What it would take each week, in kilometres cycled, to hold this debt still:
 * Sunday adds half of it, and cycling clears a kilometre for every three.
 */
export function maintenance(debt: number): number {
  return debt * (GROWTH - 1) * CYCLING_RATE;
}

/**
 * Every Sunday at 4am UK time between two instants, in order. The first one is
 * strictly after `from`, so an event that lands exactly on a growth is not grown
 * twice.
 */
export function growthTimes(from: number, to: number): number[] {
  const start = localTime(from);
  let sunday = addDays(start, (7 - start.weekday) % 7);
  const times: number[] = [];
  for (;;) {
    const at = instantOf(sunday.year, sunday.month, sunday.day, 4);
    if (at > to) return times;
    if (at > from) times.push(at);
    sunday = addDays(sunday, 7);
  }
}

/**
 * The whole history as a staircase: the debt from the first event through to
 * `now`, one step per thing that changed it. A Sunday listed in `forgiven` is
 * still a step, but one that changes nothing.
 */
export function steps(
  events: DebtEvent[],
  now: number,
  forgiven: Iterable<number> = [],
): Step[] {
  const letOff = new Set(forgiven);
  const ordered = [...events].sort((a, b) => a.at - b.at || a.id - b.id);
  if (ordered.length === 0) return [];

  const growths = growthTimes(ordered[0]!.at, now);
  const out: Step[] = [];
  let debt = 0;
  let event = 0;
  let growth = 0;

  // The two streams merged, growth first when they land together, so a Sunday
  // run pays off the week's interest rather than dodging it.
  while (event < ordered.length || growth < growths.length) {
    const nextEvent = ordered[event];
    const nextGrowth = growths[growth];
    const isGrowth =
      nextGrowth !== undefined &&
      (nextEvent === undefined || nextGrowth <= nextEvent.at);

    const at = isGrowth ? nextGrowth! : nextEvent!.at;
    const spared = isGrowth && letOff.has(at);
    const after = isGrowth
      ? spared
        ? debt
        : debt * GROWTH
      : apply(nextEvent!, debt);
    out.push({
      at,
      cause: isGrowth ? "growth" : nextEvent!.kind,
      km: isGrowth ? 0 : nextEvent!.km,
      change: after - debt,
      before: debt,
      after,
      forgiven: spared,
    });
    debt = after;
    if (isGrowth) growth++;
    else event++;
  }
  return out;
}

/** What is owed right now. */
export function balance(
  events: DebtEvent[],
  now: number,
  forgiven: Iterable<number> = [],
): number {
  const history = steps(events, now, forgiven);
  return history.length === 0 ? 0 : history[history.length - 1]!.after;
}
