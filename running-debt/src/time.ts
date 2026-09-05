/**
 * Every rule in this app is written in UK time, so every calendar question --
 * which day it is, when 4am is -- has to be asked of `Europe/London` rather than
 * of the machine's own zone.
 */

export const ZONE = "Europe/London";

const parts = new Intl.DateTimeFormat("en-GB", {
  timeZone: ZONE,
  hourCycle: "h23",
  year: "numeric",
  month: "2-digit",
  day: "2-digit",
  hour: "2-digit",
  minute: "2-digit",
  second: "2-digit",
  weekday: "short",
});

const WEEKDAYS = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];

export interface LocalTime {
  year: number;
  month: number; // 1-12
  day: number;
  hour: number;
  minute: number;
  second: number;
  weekday: number; // 0 is Sunday
}

/** What the clocks in the UK say at this instant. */
export function localTime(instant: number): LocalTime {
  const found: Record<string, string> = {};
  for (const part of parts.formatToParts(instant)) found[part.type] = part.value;
  return {
    year: Number(found.year),
    month: Number(found.month),
    day: Number(found.day),
    hour: Number(found.hour),
    minute: Number(found.minute),
    second: Number(found.second),
    weekday: WEEKDAYS.indexOf(found.weekday!),
  };
}

/** How far ahead of UTC the UK is at this instant, in milliseconds. */
function offset(instant: number): number {
  const local = localTime(instant);
  const asIfUtc = Date.UTC(
    local.year,
    local.month - 1,
    local.day,
    local.hour,
    local.minute,
    local.second,
  );
  return asIfUtc - Math.floor(instant / 1000) * 1000;
}

/**
 * The instant at which UK clocks read this wall time. Two passes, because the
 * offset depends on the answer: guess with the offset at the naive instant, then
 * correct with the offset at the guess. In the hour that the clocks skip in
 * spring there is no such instant, and this lands just after it.
 */
export function instantOf(
  year: number,
  month: number,
  day: number,
  hour = 0,
  minute = 0,
): number {
  const naive = Date.UTC(year, month - 1, day, hour, minute);
  const once = naive - offset(naive);
  return naive - offset(once);
}

/** The same wall time, a whole number of calendar days later. */
export function addDays(time: LocalTime, days: number): LocalTime {
  const moved = new Date(
    Date.UTC(time.year, time.month - 1, time.day + days, 12),
  );
  return {
    ...time,
    year: moved.getUTCFullYear(),
    month: moved.getUTCMonth() + 1,
    day: moved.getUTCDate(),
    weekday: moved.getUTCDay(),
  };
}
