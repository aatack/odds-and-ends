import assert from "node:assert/strict";
import { test } from "node:test";
import { balance, growthTimes, maintenance, steps, type DebtEvent } from "../src/debt.ts";
import { instantOf } from "../src/time.ts";

let next = 0;
const event = (
  month: number,
  day: number,
  kind: DebtEvent["kind"],
  km = 0,
  hour = 12,
): DebtEvent => ({ id: ++next, at: instantOf(2026, month, day, hour), kind, km });

const close = (found: number, wanted: number) =>
  assert.ok(Math.abs(found - wanted) < 1e-9, `${found} is not ${wanted}`);

test("every Sunday at 4am, and no other day", () => {
  const found = growthTimes(instantOf(2026, 6, 4, 12), instantOf(2026, 6, 25, 12));
  assert.deepEqual(
    found.map((at) => new Date(at).toISOString()),
    [
      "2026-06-07T03:00:00.000Z",
      "2026-06-14T03:00:00.000Z",
      "2026-06-21T03:00:00.000Z",
    ],
  );
});

test("a growth exactly at the start is not counted twice", () => {
  const sunday = instantOf(2026, 6, 7, 4);
  assert.equal(growthTimes(sunday, sunday + 1000).length, 0);
});

test("a penalty is three kilometres", () => {
  close(balance([event(6, 4, "penalty")], instantOf(2026, 6, 5, 12)), 3);
});

test("running pays a kilometre for a kilometre", () => {
  const history = [event(6, 1, "penalty"), event(6, 2, "run", 2)];
  close(balance(history, instantOf(2026, 6, 3, 12)), 1);
});

test("cycling pays a kilometre for three, fractions and all", () => {
  const history = [event(6, 1, "penalty"), event(6, 2, "cycle", 1.2)];
  close(balance(history, instantOf(2026, 6, 3, 12)), 2.6);
});

test("Sunday adds half of what is owed", () => {
  const history = [event(6, 4, "penalty")];
  close(balance(history, instantOf(2026, 6, 8, 12)), 4.5);
});

test("growth compounds week on week", () => {
  const history = [event(6, 4, "penalty")];
  close(balance(history, instantOf(2026, 6, 22, 12)), 3 * 1.5 ** 3);
});

test("Sunday morning comes before Sunday midday", () => {
  const history = [event(6, 4, "penalty"), event(6, 7, "run", 3)];
  const found = steps(history, instantOf(2026, 6, 8, 12));
  assert.deepEqual(
    found.map((step) => step.cause),
    ["penalty", "growth", "run"],
  );
  close(found[2]!.after, 1.5);
});

test("paying off more than you owe leaves nothing owed, not credit", () => {
  const history = [event(6, 4, "penalty"), event(6, 5, "run", 8)];
  close(balance(history, instantOf(2026, 6, 22, 12)), 0);
});

test("a forgiven Sunday changes nothing, and the next one still lands", () => {
  const history = [event(6, 4, "penalty")];
  const spared = instantOf(2026, 6, 7, 4);
  const found = steps(history, instantOf(2026, 6, 15, 12), [spared]);
  assert.deepEqual(found.map((step) => step.cause), ["penalty", "growth", "growth"]);
  assert.equal(found[1]!.forgiven, true);
  close(found[1]!.after, 3);
  close(found[2]!.after, 4.5);
});

test("forgiving a Sunday that is not one is simply ignored", () => {
  const history = [event(6, 4, "penalty")];
  close(balance(history, instantOf(2026, 6, 8, 12), [instantOf(2026, 6, 6, 4)]), 4.5);
});

test("holding a debt still costs three kilometres cycled for every two owed", () => {
  close(maintenance(92), 138);
  close(maintenance(0), 0);
});

test("two events at the same minute both count, in the order written", () => {
  const history = [event(6, 1, "penalty"), event(6, 2, "cycle", 3), event(6, 2, "cycle", 6)];
  close(balance(history, instantOf(2026, 6, 3, 12)), 0);
});

test("nothing owed and nothing recorded is an empty staircase", () => {
  assert.deepEqual(steps([], Date.now()), []);
  assert.equal(balance([], Date.now()), 0);
});
