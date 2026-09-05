import assert from "node:assert/strict";
import { test } from "node:test";
import { addDays, instantOf, localTime } from "../src/time.ts";

test("wall time in winter is UTC", () => {
  assert.equal(new Date(instantOf(2026, 1, 4, 4)).toISOString(), "2026-01-04T04:00:00.000Z");
});

test("wall time in summer is an hour ahead of UTC", () => {
  assert.equal(new Date(instantOf(2026, 6, 7, 4)).toISOString(), "2026-06-07T03:00:00.000Z");
});

test("4am survives both ends of summer time", () => {
  assert.equal(new Date(instantOf(2026, 3, 29, 4)).toISOString(), "2026-03-29T03:00:00.000Z");
  assert.equal(new Date(instantOf(2026, 10, 25, 4)).toISOString(), "2026-10-25T04:00:00.000Z");
});

test("a week later is the same hour, not the same number of hours", () => {
  const before = localTime(instantOf(2026, 3, 22, 4));
  const after = addDays(before, 7);
  assert.deepEqual(
    [after.year, after.month, after.day, after.weekday],
    [2026, 3, 29, 0],
  );
  assert.equal(
    instantOf(after.year, after.month, after.day, 4) - instantOf(2026, 3, 22, 4),
    6 * 24 * 60 * 60 * 1000 + 23 * 60 * 60 * 1000,
  );
});

test("the clocks say what they say", () => {
  const summer = localTime(Date.UTC(2026, 5, 7, 3, 30));
  assert.deepEqual([summer.hour, summer.minute, summer.weekday], [4, 30, 0]);
});
