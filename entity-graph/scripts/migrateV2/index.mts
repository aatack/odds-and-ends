// Turn a pensive v2 store into one this app can open.
//
//   npm run migrate:v2 -- <old.db> <new.db> --author <name>
//   npm run migrate:v2 -- <old.db> --dry-run --author <name>
//
// It reads the old file and writes a new one, and that is the whole of what it
// touches: the old store is opened read-only, an output path that already exists
// is refused rather than overwritten, and `--dry-run` writes nothing at all —
// which is how to look at a store before deciding to migrate it.
//
// Every event needs an author and v2 recorded none, so `--author` is required
// and has no default: the name on all of this is being invented, and it should
// be invented deliberately.
//
// The check runs before anything is written, and a run that finds a difference
// between what v2 showed and what this store would show stops there. A migration
// that quietly loses one entity in a thousand is worse than one that refuses.

import { existsSync } from 'node:fs'
import { resolve } from 'node:path'
import { SqliteInterface } from '../../src/core/interface/sqlite'
import { readV2 } from './read.mjs'
import { translate, verify, type Discrepancy, type Warning } from './translate.mjs'

const USAGE = `usage: npm run migrate:v2 -- <old.db> <new.db> --author <name> [--dry-run]`

interface Options {
  input: string
  output: string | null
  author: string
  dryRun: boolean
}

function parseOptions(argv: string[]): Options {
  const positional: string[] = []
  let author: string | null = null
  let dryRun = false

  for (let i = 0; i < argv.length; i++) {
    const arg = argv[i]!
    if (arg === '--dry-run') dryRun = true
    else if (arg === '--author') author = argv[++i] ?? null
    else if (arg.startsWith('--author=')) author = arg.slice('--author='.length)
    else if (arg.startsWith('-')) throw new Error(`Unknown option ${arg}\n${USAGE}`)
    else positional.push(arg)
  }

  const [input, output = null, ...rest] = positional
  if (!input) throw new Error(`No v2 database given\n${USAGE}`)
  if (rest.length > 0) throw new Error(`Unexpected argument ${rest[0]}\n${USAGE}`)
  if (!author) throw new Error(`--author is required: v2 recorded none, so one has to be chosen\n${USAGE}`)
  if (!output && !dryRun) throw new Error(`No output path given\n${USAGE}`)

  return { input: resolve(input), output: output === null ? null : resolve(output), author, dryRun }
}

const count = (n: number): string => n.toLocaleString('en-GB')

function reportWarnings(warnings: Warning[]): void {
  if (warnings.length === 0) return

  const byKind = new Map<string, string[]>()
  for (const { kind, detail } of warnings) {
    byKind.set(kind, [...(byKind.get(kind) ?? []), detail])
  }

  console.log(`\nWarnings (${count(warnings.length)})`)
  for (const [kind, details] of byKind) {
    console.log(`  ${kind} (${count(details.length)})`)
    for (const detail of details.slice(0, 3)) console.log(`    ${detail}`)
    if (details.length > 3) console.log(`    …and ${count(details.length - 3)} more`)
  }
}

const brief = (value: unknown): string => {
  const text = JSON.stringify(value)
  return text.length > 200 ? `${text.slice(0, 200)}…` : text
}

function reportDiscrepancies(discrepancies: Discrepancy[]): void {
  console.log(`\n${count(discrepancies.length)} entities do not survive the translation:`)
  for (const { id, field, expected, actual } of discrepancies.slice(0, 10)) {
    console.log(`  ${id} · ${field}`)
    console.log(`    v2 shows  ${brief(expected)}`)
    console.log(`    this says ${brief(actual)}`)
  }
  if (discrepancies.length > 10) console.log(`  …and ${count(discrepancies.length - 10)} more`)
  console.log(`\nNothing was written. This is a bug in the migration, not in the data.`)
}

async function main(): Promise<void> {
  const { input, output, author, dryRun } = parseOptions(process.argv.slice(2))

  if (!existsSync(input)) throw new Error(`No such file: ${input}`)
  // A dry run writes nothing, so it has nothing to say about where it would
  // have written: the path is checked only when it is about to be used.
  if (!dryRun && output !== null) {
    if (output === input) throw new Error(`The output is the input; this only ever writes a new file`)
    if (existsSync(output)) throw new Error(`${output} already exists, and this will not write over it`)
  }

  const store = readV2(input)
  console.log(
    `Read ${count(store.rows.length)} rows and ${count(store.resources.length)} resources from ${input}`
  )

  const translation = translate(store, author)
  const keys = [...translation.keyCounts.entries()].sort((a, b) => b[1] - a[1])
  if (keys.length > 0) console.log(`  ${keys.map(([key, n]) => `${key} ${count(n)}`).join(' · ')}`)

  const values = translation.events.filter((event) => event.type === 'value').length
  const links = translation.events.length - values
  console.log(
    `Translated into ${count(values)} value events and ${count(links)} links across ` +
      `${count(translation.ids.size)} entities, as ${author}`
  )

  const check = verify(translation)
  reportWarnings([...translation.warnings, ...check.warnings])

  if (check.discrepancies.length > 0) {
    reportDiscrepancies(check.discrepancies)
    process.exitCode = 1
    return
  }
  console.log(`\nChecked ${count(check.checked)} entities against v2's own rollup: nothing differs`)

  if (dryRun || output === null) {
    console.log(`Dry run: nothing written`)
    return
  }

  const store3 = new SqliteInterface(output)
  try {
    await store3.writeEvents(translation.events)
    for (const resource of translation.resources) await store3.writeResource(resource)
  } finally {
    store3.close()
  }
  console.log(`Wrote ${output}`)
}

main().catch((error: unknown) => {
  console.error(error instanceof Error ? error.message : String(error))
  process.exitCode = 1
})
