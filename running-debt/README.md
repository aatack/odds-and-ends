# Running debt

A debt measured in kilometres. It grows by half every Sunday morning, penalties
add to it, and running and cycling pay it off. This plots what is owed from the
first penalty to right now, and lets you write down what you did.

Nothing to install: it is TypeScript with no runtime dependencies, and Node runs
it directly.

## Run it

You need [Node.js](https://nodejs.org/) v22.18+.

```bash
cd running-debt
npm start
```

That prints what you owe and a URL. Open the URL.

The first run writes the events into `debt.db` beside the code, so there is no
setup step; delete that file to start again. It is not in the repository, and
`RUNNING_DEBT_DB` moves it somewhere else.

## Write down what you did

```bash
npm start -- cycle 12.3           # a 12.3 km cycle, just now
npm start -- run 5 2026-09-04     # a 5 km run at midday on the 4th
npm start -- penalty              # three more kilometres owed
npm start -- list                 # every event, oldest first
npm start -- drop 17              # take one back out
npm start -- now                  # the number, and nothing else
```

Times are read and shown in UK time: `2026-09-04`, or `2026-09-04 18:30`. A date
with no time means midday.

`npm link` in this directory gives you a `running-debt` command that does the
same from anywhere, and `running-debt --help` lists it all.

## The rules

- Every Sunday at 4am, what is owed grows by 50%.
- A penalty adds 3 km.
- Running clears a kilometre of debt for every kilometre run.
- Cycling clears a kilometre for every three cycled, fractions included.

## What it decided for you

The events came from a note that left a few things unsaid. Each of these is one
line to change if it is wrong:

- **"4th June: debt incurred" is read as one penalty, so 3 km.** It is the only
  amount the rules name. `SEED` in `src/db.ts`.
- **The events are all 2026.** The note gave days and months only. `SEED_YEAR`
  in `src/db.ts`.
- **Sunday's 50% lands before a Sunday run**, since 4am comes before midday. So
  the 3 km run on Sunday 7 June paid off a debt that had already grown to 4.5 km,
  and left 1.5 km rather than nothing -- which is why there is still a debt.
- **Paying off more than you owe banks the surplus, and the surplus does not
  grow.** A balance below zero is credit against the next penalty; growing it by
  half a week would be a reward rather than a debt. `steps` in `src/debt.ts`.

## The pieces

- **`src/debt.ts`** -- the rules, and the balance over time as a staircase. The
  file to read first.
- **`src/time.ts`** -- UK time. Every rule is written in it, so every question
  about which day it is has to be asked of `Europe/London`.
- **`src/db.ts`** -- one table of events. Everything else is worked out from
  them.
- **`src/page.ts`** -- the plot, as one file of HTML.
- **`src/server.ts`** -- the page, and `/debt.json` for reading it yourself.
- **`src/index.ts`** -- the command line.

## Type-checking and tests

```bash
npm install     # only needed for these two
npm run typecheck
npm test
```
