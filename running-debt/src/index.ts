/**
 * The command line. `running-debt` on its own puts the plot on a port; the rest
 * is for writing down what you did.
 */

import { add, databasePath, events, open, remove, seed } from "./db.ts";
import { balance, type EventKind } from "./debt.ts";
import { serve } from "./server.ts";
import { instantOf } from "./time.ts";

const USAGE = `running-debt -- what you owe, and how it got that way

  running-debt [--port <n>]        plot it, on http://127.0.0.1:4747
  running-debt cycle <km> [when]   a cycle of that many kilometres
  running-debt run <km> [when]     a run of that many kilometres
  running-debt penalty [when]      three more kilometres owed
  running-debt list                every event, oldest first
  running-debt drop <id>           take one back out
  running-debt now                 what is owed, and nothing else

<when> is a date, or a date and a time, read as UK time: 2026-09-04, or
2026-09-04 18:30. It defaults to now.

The database is ${databasePath()}; RUNNING_DEBT_DB moves it.
`;

/** A date, or a date and a time, in UK time. Anything else is an error. */
function moment(words: string[]): number {
  if (words.length === 0) return Date.now();
  const written = words.join(" ").trim().replace("T", " ");
  const found = /^(\d{4})-(\d{2})-(\d{2})(?:[ ](\d{1,2}):(\d{2}))?$/.exec(written);
  if (!found) throw new Error(`Cannot read "${written}" as a date`);
  const [, year, month, day, hour, minute] = found;
  return instantOf(
    Number(year),
    Number(month),
    Number(day),
    hour === undefined ? 12 : Number(hour),
    minute === undefined ? 0 : Number(minute),
  );
}

function distance(word: string | undefined): number {
  const km = Number(word);
  if (!Number.isFinite(km) || km <= 0) throw new Error(`Cannot read "${word}" as a distance`);
  return km;
}

const stamp = new Intl.DateTimeFormat("en-GB", {
  timeZone: "Europe/London",
  year: "numeric",
  month: "short",
  day: "numeric",
  hour: "2-digit",
  minute: "2-digit",
  hourCycle: "h23",
});

function announce(id: number, kind: EventKind, km: number, at: number, debt: number): void {
  const what = kind === "penalty" ? "penalty" : `${km} km ${kind}`;
  console.log(`#${id}  ${what}, ${stamp.format(at)}`);
  console.log(`${debt.toFixed(2)} km owed`);
}

async function main(argv: string[]): Promise<void> {
  if (argv[0] === "--help" || argv[0] === "-h") {
    console.log(USAGE);
    return;
  }

  const db = open();
  const planted = seed(db);
  if (planted > 0) console.log(`Wrote ${planted} events into ${databasePath()}`);

  const [command, ...rest] = argv;
  const now = () => balance(events(db), Date.now());

  switch (command) {
    case undefined:
    case "--port": {
      const port = command === "--port" ? Number(rest[0]) : 4747;
      if (!Number.isInteger(port) || port < 0) throw new Error(`Cannot read "${rest[0]}" as a port`);
      const address = await serve(db, port);
      console.log(`${now().toFixed(2)} km owed -- ${address}`);
      return;
    }
    case "cycle":
    case "run": {
      const kind: EventKind = command === "cycle" ? "cycle" : "run";
      const km = distance(rest[0]);
      const at = moment(rest.slice(1));
      const written = add(db, kind, km, at);
      announce(written.id, kind, km, at, now());
      break;
    }
    case "penalty": {
      const at = moment(rest);
      const written = add(db, "penalty", 0, at);
      announce(written.id, "penalty", 0, at, now());
      break;
    }
    case "list": {
      for (const event of events(db)) {
        const what = event.kind === "penalty" ? "penalty" : `${event.km} km ${event.kind}`;
        console.log(`#${event.id}\t${stamp.format(event.at)}\t${what}`);
      }
      break;
    }
    case "drop": {
      const id = Number(rest[0]);
      if (!Number.isInteger(id)) throw new Error(`Cannot read "${rest[0]}" as an id`);
      if (!remove(db, id)) throw new Error(`There is no event #${id}`);
      console.log(`Dropped #${id}. ${now().toFixed(2)} km owed`);
      break;
    }
    case "now": {
      console.log(now().toFixed(2));
      break;
    }
    default:
      throw new Error(`No such command: ${command}`);
  }
  db.close();
}

main(process.argv.slice(2)).catch((error: unknown) => {
  console.error(error instanceof Error ? error.message : error);
  process.exitCode = 1;
});
