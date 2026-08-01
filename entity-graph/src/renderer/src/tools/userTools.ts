import { bucketEvents, rollupEntity, str, type Entity } from '../../../core/entity'
import { readToolArguments } from '../../../core/toolArguments'
import { runToolScript } from '../helpers/codeRunner'
import { scanEvents } from '../source/entity'
import { currentSourceId } from '../source/transport'
import { atom } from '../state/atom'
import { argsFromSchema, summarise } from './declared'
import type { KeyBinding } from './keys'
import type { ArgSpec, ToolReach, ToolScope, ToolSpec } from './types'

// Tools the user wrote in the graph. A note under `@tools` describing itself —
// what it's called, what arguments it takes, and a body — becomes a tool of the
// app: it appears in the palette, it can be given a key, and other scripts can
// call it by name.
//
// Which is the whole reason this lives on this side. The body runs in the same
// sandbox a `type: code` entity does, with the same `tool` façade, so a
// user-defined tool reaches everything in the registry — the frame tools, the
// writes, the server's integrations. None of that is available to the server,
// which is why its half of this (`core/source/userTools.ts`) can list a
// user-defined tool but not run one.
//
// Runtime only and pointedly not persisted, exactly like the integrations: this
// is a cache of what the store says, and the store is the source of truth.

/**
 * The reserved entity whose direct children are the user's tools. Spelled here
 * rather than imported from `core/source/userTools`, which is the server's half
 * and pulls zod and the whole default tool set in behind it — the same reason
 * `ROOT_ID` is spelled in `state/types`.
 */
export const TOOLS_ENTITY_ID = '@tools'

export const userToolsAtom = atom<ToolSpec[]>([])

/**
 * What a load found. The skipped list is the point of it: a note under `@tools`
 * that isn't quite a tool is passed over, and without something to say so the
 * only symptom is a tool that never appears — which is indistinguishable from
 * having forgotten to reload.
 */
export interface ToolsLoaded {
  tools: ToolSpec[]
  /** Children of `@tools` that aren't tools, and what each is missing. */
  skipped: { id: string; why: string }[]
  /** Tools that loaded, but with something on them that didn't read. */
  warnings: { id: string; why: string }[]
  /** How many notes are linked under `@tools` at all. */
  linked: number
}

const NOTHING: ToolsLoaded = { tools: [], skipped: [], warnings: [], linked: 0 }

/** Which source's tools these are. Null when none is open. */
let loadedFrom: string | null = null

// --- Reading the definitions ------------------------------------------------

/** Roll up a set of ids from one scan of the store. */
async function readEntities(ids: string[]): Promise<Map<string, Entity>> {
  if (ids.length === 0) return new Map()
  const { events } = await scanEvents(ids)
  const buckets = bucketEvents(ids, events)
  return new Map(ids.map((id) => [id, rollupEntity(id, buckets.get(id) ?? [])]))
}

/**
 * Load the tools defined in the open source, replacing whatever was loaded
 * before. Two reads rather than one: `scanEvents` overscans a couple of layers
 * past what it was asked for, so a single call would very likely bring the tools
 * back too, but "very likely" is not a contract, and a tool that silently didn't
 * load is a bad way to find that out.
 *
 * Failure is not an error. A store with no `@tools` entity simply has no tools of
 * its own, and that is the ordinary case rather than something to report.
 */
export async function loadUserTools(): Promise<ToolsLoaded> {
  const sourceId = currentSourceId()
  if (!sourceId) {
    userToolsAtom.set([])
    loadedFrom = null
    return NOTHING
  }
  try {
    const root = (await readEntities([TOOLS_ENTITY_ID])).get(TOOLS_ENTITY_ID)
    const childIds = root?.outboundLinks ?? []
    const defined = await readEntities(childIds)
    // Guard against a slow load landing after the source has moved on.
    if (currentSourceId() !== sourceId) return NOTHING
    const found = { ...toolSpecs(childIds, defined), linked: childIds.length }
    userToolsAtom.set(found.tools)
    loadedFrom = sourceId
    return found
  } catch {
    if (currentSourceId() === sourceId) {
      userToolsAtom.set([])
      loadedFrom = sourceId
    }
    return NOTHING
  }
}

/** Drop what's loaded — the source is closing, or another is opening. */
export function clearUserTools(): void {
  userToolsAtom.set([])
  loadedFrom = null
}

/** Whether the open source's tools have been read yet. */
export const userToolsLoaded = (): boolean => loadedFrom === currentSourceId()

// --- A definition as a tool -------------------------------------------------

/** A tool's body, and how it expects to be called. */
interface Body {
  code: string
  /**
   * Whether the code is a *function* to be applied to the arguments, rather than
   * a body that reads them off `context`.
   */
  applied: boolean
}

/**
 * Where a tool's body comes from. `execute` is the one to write: an expression
 * evaluating to a function, which is then called with the arguments in the order
 * the definition declared them — so a tool reads like a function, because it is
 * one.
 *
 * `script` is the other shape, kept because it was here first: statements rather
 * than an expression, reading their arguments off `context` and handing back
 * whatever the last of them evaluates to.
 *
 * Both live on the tool's own note. An earlier version looked for a `type: code`
 * child instead, which is gone: a value is edited in the inspector on the same
 * ground the note itself is, and one place to look beats two.
 */
function bodyOf(tool: Entity): Body | null {
  const execute = str(tool.values.execute)
  if (execute) return { code: execute, applied: true }
  const script = str(tool.values.script)
  if (script) return { code: script, applied: false }
  return null
}

/**
 * The source actually run for an `execute` body: the expression, evaluated, and
 * then applied to the arguments positionally.
 *
 * The arguments are named rather than inlined — `context.args` is already in the
 * sandbox, put there by the run's context — so nothing of the *values* is spliced
 * into source text. Only the argument names are, and those are quoted as string
 * literals rather than written as identifiers, so a name with a space or a quote
 * in it is a lookup and not a syntax error.
 *
 * An argument the user left empty is absent from `context.args` and so arrives as
 * `undefined`, which is what a function not given a parameter gets anyway.
 *
 * The newlines around the expression matter: a trailing line comment on the last
 * line of it would otherwise swallow the closing bracket.
 */
export function appliedSource(execute: string, args: ArgSpec[]): string {
  const passed = args.map((a) => `context.args[${JSON.stringify(a.name)}]`).join(', ')
  return [
    'const __tool = (',
    execute,
    ')',
    "if (typeof __tool !== 'function')",
    "  throw new Error('`execute` must be an expression that evaluates to a function')",
    `__tool(${passed})`,
  ].join('\n')
}

const SCOPES = new Set<ToolScope>(['frame', 'group', 'app'])
const REACHES = new Set<ToolReach>(['ui', 'source', 'external'])

const scopeOf = (v: unknown): ToolScope => {
  const scope = str(v)
  return scope && SCOPES.has(scope as ToolScope) ? (scope as ToolScope) : 'app'
}

/**
 * How far a user tool is taken to reach, and so whether its calls are kept. The
 * default is `external` — not because every one of them phones out, but because
 * the log is the only account of what a script did, and a tool the user wrote
 * themselves is exactly the kind whose runs are worth being able to look back at.
 * A tool that only moves the selection says `reach: ui` and stops filling it.
 */
const reachOf = (v: unknown): ToolReach => {
  const reach = str(v)
  return reach && REACHES.has(reach as ToolReach) ? (reach as ToolReach) : 'external'
}

/**
 * A key as a definition writes it: `g`, `shift+g`, `mod+shift+g`. `mod` is Ctrl
 * or ⌘, matching the binding the built-in tools declare, so a definition never
 * has to know which machine it is on.
 *
 * A binding that collides with a built-in loses: the router takes the first tool
 * in the registry that binds a key within a scope, and these trail the built-ins.
 */
function keyOf(v: unknown): KeyBinding[] | undefined {
  const spec = str(v)
  if (!spec) return undefined
  const parts = spec.split('+').map((p) => p.trim().toLowerCase()).filter(Boolean)
  const key = parts.pop()
  if (!key) return undefined
  const binding: KeyBinding = { key }
  for (const part of parts) {
    if (part === 'shift') binding.shift = true
    else if (part === 'mod' || part === 'ctrl' || part === 'cmd' || part === 'meta') binding.mod = true
    else if (part === 'alt' || part === 'option') binding.alt = true
    // Anything else is not a modifier this app has, and is ignored rather than
    // turned into a binding that would never match.
  }
  return [binding]
}

/**
 * One definition as a tool, or null when the entity isn't one. Two things are
 * required: a `name`, which is the tool's id and how a script names it, and a
 * body, without which there is nothing to run — so a heading or a note left under
 * `@tools` is passed over rather than becoming a tool that fails when invoked.
 *
 * Everything else has a default, `arguments` included: a tool that takes none is
 * an ordinary tool, not an incomplete one.
 */
function toolSpec(tool: Entity, name: string, args: ArgSpec[]): ToolSpec | null {
  const body = bodyOf(tool)
  if (!body) return null

  const description = str(tool.values.description)
  const keys = keyOf(tool.values.key)
  // Nothing about the source depends on what the arguments are *set to*, only on
  // what they are, so it is built once here rather than on every invocation.
  const source = body.applied ? appliedSource(body.code, args) : body.code

  return {
    id: name,
    // The note's own text is the label when it has one: that is what the user
    // reads in the outline, so it should be what they read in the palette.
    label: str(tool.values.label) ?? str(tool.values.text) ?? name,
    // The id is an alias so a tool found under one name in a script can be found
    // by the same name in the palette, whatever its label says.
    aliases: [name, ...(description ? [description] : [])],
    hint: 'Yours',
    scope: scopeOf(tool.values.scope),
    reach: reachOf(tool.values.reach),
    ...(args.length ? { args } : {}),
    ...(keys ? { keys } : {}),
    // A body that writes does so through the write tools, and each of those
    // refreshes on its own way out, so the default is that this one didn't.
    ...(tool.values.mutates === true ? { mutates: true } : {}),
    run: async (values, call) => {
      // The arguments are put where a script already looks. `context` is how an
      // `events` key reads the entity it sits on, and a tool's arguments are the
      // same kind of thing — what this run is about — so they are folded in
      // alongside, and kept together under `args`, which is both what a body can
      // read them by name from and what an `execute` function is applied to.
      const context = {
        ...call.context,
        values: { ...call.context.values, ...values, args: values },
      }
      const { result, logs } = await runToolScript(name, source, context)
      // Nowhere else for them to go: the tool's result is what the log keeps, and
      // a body debugged by printing has to print somewhere.
      for (const line of logs) console.log(`[${name}]`, line)
      return { data: result, message: summarise(result) ?? `Ran ${name}` }
    },
  }
}

/**
 * The definitions as tools, in the order `@tools` lists them, and a note of every
 * child that didn't become one. Nothing here is an error — a heading under
 * `@tools` is a perfectly reasonable thing to have — but the reasons are worth
 * carrying out, because from the outside a skipped definition and an unreloaded
 * one look exactly alike.
 */
function toolSpecs(
  childIds: string[],
  defined: Map<string, Entity>,
): Pick<ToolsLoaded, 'tools' | 'skipped' | 'warnings'> {
  const tools: ToolSpec[] = []
  const skipped: { id: string; why: string }[] = []
  const warnings: { id: string; why: string }[] = []
  const seen = new Set<string>()
  for (const id of childIds) {
    const entity = defined.get(id)
    if (!entity) continue
    const name = str(entity.values.name)
    if (!name) {
      skipped.push({ id, why: 'no `name`' })
      continue
    }
    // First of a name wins, as it does on the server: two tools answering to one
    // name would make which of them a script reached depend on the child order.
    if (seen.has(name)) {
      skipped.push({ id, why: `another tool is already called ${name}` })
      continue
    }
    // A list of arguments is what a definition writes; a schema is what the
    // prompts are built from. One is nicer to write and the other is what the
    // server publishes, so the conversion lives in core where both can see it.
    const declared = readToolArguments(entity.values.arguments)
    const spec = toolSpec(entity, name, argsFromSchema(declared.schema))
    if (!spec) {
      skipped.push({ id, why: 'no `execute`' })
      continue
    }
    // The tool still loads — it just takes nothing, which is exactly the failure
    // that reads as a broken body rather than a broken declaration.
    if (declared.unreadable) {
      warnings.push({ id: name, why: '`arguments` is not a list, so it takes none' })
    }
    seen.add(name)
    tools.push(spec)
  }
  return { tools, skipped, warnings }
}
