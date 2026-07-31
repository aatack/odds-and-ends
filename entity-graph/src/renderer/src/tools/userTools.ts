import { bucketEvents, rollupEntity, str, type Entity } from '../../../core/entity'
import { runToolScript } from '../helpers/codeRunner'
import { scanEvents } from '../source/entity'
import { currentSourceId } from '../source/transport'
import { atom } from '../state/atom'
import { argsFromSchema, summarise } from './declared'
import type { KeyBinding } from './keys'
import type { ToolReach, ToolScope, ToolSpec } from './types'

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
export async function loadUserTools(): Promise<void> {
  const sourceId = currentSourceId()
  if (!sourceId) {
    userToolsAtom.set([])
    loadedFrom = null
    return
  }
  try {
    const root = (await readEntities([TOOLS_ENTITY_ID])).get(TOOLS_ENTITY_ID)
    const childIds = root?.outboundLinks ?? []
    // The bodies hang off the tools, so this read has to reach a layer further.
    const defined = await readEntities(childIds)
    const bodies = await readEntities(
      childIds.flatMap((id) => defined.get(id)?.outboundLinks ?? []),
    )
    // Guard against a slow load landing after the source has moved on.
    if (currentSourceId() !== sourceId) return
    userToolsAtom.set(toolSpecs(childIds, defined, bodies))
    loadedFrom = sourceId
  } catch {
    if (currentSourceId() === sourceId) {
      userToolsAtom.set([])
      loadedFrom = sourceId
    }
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

/**
 * Where a tool's body comes from. A `script` value is the direct way to say it,
 * and the only one a store can be sure of holding — but a script is code, and
 * code wants more than one line and somewhere to be run from, so a child marked
 * `type: code` counts too. That child is an ordinary code entity: it can be
 * edited in place and pressed play on, which is how a tool gets debugged before
 * anything is bound to it.
 */
function bodyOf(tool: Entity, bodies: Map<string, Entity>): string | null {
  const own = str(tool.values.script)
  if (own) return own
  for (const childId of tool.outboundLinks) {
    const child = bodies.get(childId)
    if (str(child?.values.type) !== 'code') continue
    const code = str(child?.values.text)
    if (code) return code
  }
  return null
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
function toolSpec(tool: Entity, bodies: Map<string, Entity>): ToolSpec | null {
  const name = str(tool.values.name)
  if (!name) return null
  const body = bodyOf(tool, bodies)
  if (!body) return null

  const description = str(tool.values.description)
  const keys = keyOf(tool.values.key)
  const args = argsFromSchema(tool.values.arguments)

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
      // alongside, and kept together under `args` for a body that would rather
      // say which is which.
      const context = {
        ...call.context,
        values: { ...call.context.values, ...values, args: values },
      }
      const { result, logs } = await runToolScript(name, body, context)
      // Nowhere else for them to go: the tool's result is what the log keeps, and
      // a body debugged by printing has to print somewhere.
      for (const line of logs) console.log(`[${name}]`, line)
      return { data: result, message: summarise(result) ?? `Ran ${name}` }
    },
  }
}

/** The definitions as tools, in the order `@tools` lists them. */
function toolSpecs(
  childIds: string[],
  defined: Map<string, Entity>,
  bodies: Map<string, Entity>,
): ToolSpec[] {
  const specs: ToolSpec[] = []
  const seen = new Set<string>()
  for (const id of childIds) {
    const entity = defined.get(id)
    if (!entity) continue
    const spec = toolSpec(entity, bodies)
    // First of a name wins, as it does on the server: two tools answering to one
    // name would make which of them a script reached depend on the child order.
    if (!spec || seen.has(spec.id)) continue
    seen.add(spec.id)
    specs.push(spec)
  }
  return specs
}
