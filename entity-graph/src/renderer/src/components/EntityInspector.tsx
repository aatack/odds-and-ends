import React, { useCallback, useEffect, useState } from 'react'
import { Check, Trash03 } from '@untitledui/icons'
import type { Entity } from '../../../core/entity'
import { entitiesAtom, refreshDerived, refreshEntities, type LoadState } from '../../../core/cache'
import { cn } from '../helpers/cn'
import { useAtomValue } from '../state/hooks'
import { clearUndo } from '../state/undo'
import { EntityPill } from './EntityPill'
import { Button } from './ui/Button'
import { Field } from './ui/Field'
import { IconButton } from './ui/IconButton'
import { Input } from './ui/Input'
import { Modal } from './ui/Modal'
import { TextEditor } from './ui/TextEditor'

const api = window.entityGraph

/** The surface a `type: code` entity is edited on, which is this one too. */
const CODE = 'font-mono text-[12.5px] leading-5 text-gray-900'

const EMPTY = 'text-xs text-gray-400'

interface Props {
  /** Id of the open source (the handle `sourceCall` resolves by). */
  sourceId: string
  /** The entity being inspected. */
  entityId: string
  user: string
  onClose: () => void
}

/** One panel each, because each of them is a list that wants the whole height. */
type Tab = 'values' | 'links' | 'inbound' | 'cache'

const TABS: { id: Tab; label: string }[] = [
  { id: 'values', label: 'Values' },
  { id: 'links', label: 'Links' },
  { id: 'inbound', label: 'Linked from' },
  { id: 'cache', label: 'In the cache' },
]

/**
 * Low-level inspector for a single entity: its values, editable, and its links in
 * both directions, plus the client's own picture of it. Everything goes through
 * the source's raw tools, mirroring the global debug panel but scoped to one
 * entity.
 *
 * A panel rather than a dialog — nearly the whole window, one section at a time.
 * The sections don't have to compete for room because they are never on screen
 * together, and each of them is a list that will happily take all there is: an
 * entity with thirty values and an entity linked from thirty places are the same
 * entity.
 */
export function EntityInspector({ sourceId, entityId, user, onClose }: Props): React.JSX.Element {
  const [values, setValues] = useState<Record<string, unknown>>({})
  const [links, setLinks] = useState<string[]>([])
  const [inbound, setInbound] = useState<string[]>([])
  const [missing, setMissing] = useState(false)
  const [error, setError] = useState<string | null>(null)
  const [tab, setTab] = useState<Tab>('values')

  const load = useCallback(async (): Promise<void> => {
    setError(null)
    try {
      // Straight from the source rather than from the cache, and rolled up by
      // the store: the point of this panel is what is actually written down,
      // without a type's defaults or an `events` script laid over the top.
      const read = (await api.sourceCall(sourceId, 'readEntities', {
        entityIds: [entityId],
      })) as Record<string, Entity>
      const self = read[entityId]
      setValues(self?.values ?? {})
      setLinks(self?.outboundLinks ?? [])
      setInbound(self?.inboundLinks ?? [])
      // No values and nothing pointing either way: an id that was written to by
      // accident, or one that has since been fully unlinked.
      setMissing(
        !!self &&
          Object.keys(self.values).length === 0 &&
          self.outboundLinks.length === 0 &&
          self.inboundLinks.length === 0,
      )
    } catch (e) {
      setError(e instanceof Error ? e.message : String(e))
    }
  }, [sourceId, entityId])

  useEffect(() => {
    void load()
  }, [load])

  // The inspector writes raw events rather than going through a tool — that's
  // the point of it — so it has to do by hand what the call machine would: tell
  // open frames to refetch, and strand the undo stack, whose events are no longer
  // the store's most recent.
  const write = async (fn: () => Promise<unknown>): Promise<void> => {
    setError(null)
    try {
      await fn()
      clearUndo()
      refreshEntities()
      await load()
    } catch (e) {
      setError(e instanceof Error ? e.message : String(e))
    }
  }

  const writeValue = (key: string, value: unknown): Promise<void> =>
    write(() => api.sourceCall(sourceId, 'writeValue', { entityId, key, value, author: user }))

  /** `action` is the store's own: 0 adds the link, 1 takes it away. */
  const writeLink = (from: string, to: string, action: 0 | 1): Promise<void> =>
    write(() =>
      api.sourceCall(sourceId, 'writeLink', {
        sourceId: from,
        destinationId: to,
        action,
        author: user,
      }),
    )

  const counts: Record<Tab, number | null> = {
    values: Object.keys(values).length,
    links: links.length,
    inbound: inbound.length,
    // Not a list, and the one thing in here that isn't the store's.
    cache: null,
  }

  return (
    <Modal title="Inspect entity" onClose={onClose} size="large">
      <p className="-mt-1 shrink-0 break-all font-mono text-xs text-gray-500">{entityId}</p>

      <div className="mt-3 flex shrink-0 items-center gap-1">
        {TABS.map((t) => (
          <button
            key={t.id}
            onClick={() => setTab(t.id)}
            className={cn(
              'flex items-center gap-1.5 rounded-md px-2.5 py-1.5 text-[13px] focus:outline-none',
              'focus-visible:ring-2 focus-visible:ring-brand-500/40',
              t.id === tab
                ? 'bg-gray-100 font-medium text-gray-900'
                : 'text-gray-500 hover:text-gray-900',
            )}
          >
            {t.label}
            {counts[t.id] !== null && <span className="text-xs text-gray-400">{counts[t.id]}</span>}
          </button>
        ))}
      </div>

      {error && <p className="mt-3 shrink-0 text-[13px] text-error-600">{error}</p>}
      {missing && (
        <p className={cn('mt-3 shrink-0', EMPTY)}>
          Nothing is recorded against this id — no values, and no links either way.
        </p>
      )}

      {/* The one scrolling region. The id and the tab bar above it stay put, so
          moving between tabs never moves the tabs. */}
      <div className="mt-3 min-h-0 flex-1 space-y-4 overflow-y-auto pr-1">
        {tab === 'values' && <Values values={values} onWrite={writeValue} />}
        {tab === 'links' && (
          <Links
            ids={links}
            empty="No outbound links."
            onRemove={(dest) => writeLink(entityId, dest, 1)}
            onAdd={(dest) => writeLink(entityId, dest, 0)}
          />
        )}
        {/* Inbound is the direction you need to trace a stray link back to whoever
            made it. Removing one writes from the other end, which is the only way
            to say it — a link belongs to its source. */}
        {tab === 'inbound' && (
          <Links
            ids={inbound}
            empty="Nothing links to this entity."
            onRemove={(src) => writeLink(src, entityId, 1)}
          />
        )}
        {tab === 'cache' && <Cached entityId={entityId} />}
      </div>
    </Modal>
  )
}

// --- Values -----------------------------------------------------------------

const pretty = (value: unknown): string => {
  try {
    return JSON.stringify(value, null, 2) ?? String(value)
  } catch {
    return String(value)
  }
}

function Values({
  values,
  onWrite,
}: {
  values: Record<string, unknown>
  onWrite: (key: string, value: unknown) => Promise<void>
}): React.JSX.Element {
  // Sorted, because the order the store hands them back in is the order they were
  // first written, which is nothing to anyone looking for one of them.
  const keys = Object.keys(values).sort()
  return (
    <>
      {keys.length === 0 ? (
        <p className={EMPTY}>No values.</p>
      ) : (
        keys.map((key) => (
          <ValueEditor
            key={key}
            name={key}
            value={values[key]}
            onWrite={(next) => onWrite(key, next)}
          />
        ))
      )}
      <div className="pt-2">
        <WriteValue onWrite={onWrite} />
      </div>
    </>
  )
}

/**
 * One key, editable, on the ground a `type: code` entity is edited on.
 *
 * A string is edited as itself and anything else as JSON. That distinction earns
 * its keep on exactly the values worth coming in here for: `text` and `events`
 * hold prose and code, and `"const a = 1\nconst b = 2"` on one escaped line is
 * not something anyone can read, let alone edit. What goes back keeps the shape
 * it was read in — a string stays a string, and an object has to stay valid JSON.
 *
 * Nothing is written until the tick is pressed. Blur doesn't commit and nor does
 * Enter, which puts in a newline: this is the raw store, where a value half-typed
 * is not a value, and the tick is there only when there is something to write.
 *
 * The bin writes null, which is what taking a value off *is* in an append-only
 * store — and, since a null value falls back to the type's, is also how a key
 * goes back to its default. The row stays, showing that null: this panel is what
 * is written down, and "cleared" and "never written" are the same thing to
 * everything downstream but not to the events, which are what it is here to show.
 */
function ValueEditor({
  name,
  value,
  onWrite,
}: {
  name: string
  value: unknown
  onWrite: (value: unknown) => Promise<void>
}): React.JSX.Element {
  const isText = typeof value === 'string'
  const original = isText ? value : pretty(value)
  const [draft, setDraft] = useState(original)
  const [error, setError] = useState<string | null>(null)

  // Follow the store: a write lands, the panel re-reads, and the draft is then the
  // same text as the value — which is what takes the tick away again.
  useEffect(() => {
    setDraft(original)
    setError(null)
  }, [original])

  const changed = draft !== original

  const save = (): void => {
    let next: unknown = draft
    if (!isText) {
      try {
        next = JSON.parse(draft)
      } catch {
        setError('not valid JSON')
        return
      }
    }
    setError(null)
    void onWrite(next)
  }

  return (
    <section>
      <div className="flex items-center gap-2 pb-1 pl-0.5">
        <span className="font-mono text-xs text-gray-900">{name}</span>
        <span className="text-[11px] text-gray-400">{isText ? 'text' : 'JSON'}</span>
        <div className="flex-1" />
        {error && <span className="text-xs text-error-600">{error}</span>}
        {changed && (
          <IconButton title={`Write ${name}`} onClick={save} className="text-brand-600">
            <Check size={15} />
          </IconButton>
        )}
        {/* Absent once the key is already null: there is nothing left to clear,
            and a bin that writes what is already there is a button that does
            nothing but add an event. */}
        {value !== null && (
          <IconButton title={`Clear ${name}`} onClick={() => void onWrite(null)}>
            <Trash03 size={14} />
          </IconButton>
        )}
      </div>
      {/* The editor grows to its content and this box stops at a screenful, so a
          long value scrolls here rather than running off the bottom of the panel.
          The padding belongs to the editor rather than the box, or it would scroll
          away from the top of a value taller than the box. */}
      <div className="max-h-[40vh] overflow-auto rounded-md bg-gray-100 shadow-xs">
        <TextEditor
          multiline
          eager
          value={draft}
          setValue={setDraft}
          className={cn(CODE, 'px-2.5 py-1.5')}
        />
      </div>
    </section>
  )
}

/**
 * Adding a key that isn't there yet. JSON either way here: a key with no value
 * has no shape to keep, so this is where a value becomes a number or an object.
 */
function WriteValue({
  onWrite,
}: {
  onWrite: (key: string, value: unknown) => Promise<void>
}): React.JSX.Element {
  const [key, setKey] = useState('')
  const [value, setValue] = useState('')
  const [error, setError] = useState<string | null>(null)

  const submit = (): void => {
    if (!key.trim()) return
    let parsed: unknown
    try {
      parsed = JSON.parse(value)
    } catch {
      setError('Value must be valid JSON')
      return
    }
    setError(null)
    void onWrite(key.trim(), parsed)
    setKey('')
    setValue('')
  }

  return (
    <div className="space-y-2">
      <div className="flex items-end gap-2">
        <Field label="New key" className="w-40">
          <Input value={key} onChange={(e) => setKey(e.target.value)} placeholder="text" />
        </Field>
        <Field label="Value (JSON)" className="flex-1">
          <Input mono value={value} onChange={(e) => setValue(e.target.value)} placeholder='"hello"' />
        </Field>
        <Button variant="secondary" onClick={submit} disabled={!key.trim() || !value.trim()}>
          Write
        </Button>
      </div>
      {error && <p className="text-xs text-error-600">{error}</p>}
    </div>
  )
}

// --- Links ------------------------------------------------------------------

function Links({
  ids,
  empty,
  onRemove,
  onAdd,
}: {
  ids: string[]
  empty: string
  onRemove: (id: string) => Promise<void>
  /** Absent for the inbound list, which is not this entity's to add to. */
  onAdd?: (id: string) => Promise<void>
}): React.JSX.Element {
  return (
    <>
      {ids.length === 0 ? (
        <p className={EMPTY}>{empty}</p>
      ) : (
        <div className="overflow-hidden rounded-md bg-gray-50 font-mono text-xs">
          {ids.map((id) => (
            <div key={id} className="flex items-center gap-2 px-3 py-1">
              <span className="truncate text-gray-900">{id}</span>
              {/* What the id *is*, beside what it says. A pill rather than a label
                  because a pill is still the entity: right-click it for the tool
                  list on that entity, middle-click it for a tab. Asking for the
                  label is also what fetches it, so a list of ids fills itself in.
                  No click action of its own — the panel's subject shouldn't change
                  under a stray click, and there is nowhere to go back to. */}
              <EntityPill id={id} className="shrink-0" />
              <div className="flex-1" />
              <IconButton title="Remove link" onClick={() => void onRemove(id)}>
                <Trash03 size={14} />
              </IconButton>
            </div>
          ))}
        </div>
      )}
      {onAdd && <AddLink onAdd={onAdd} />}
    </>
  )
}

function AddLink({ onAdd }: { onAdd: (dest: string) => Promise<void> }): React.JSX.Element {
  const [dest, setDest] = useState('')

  const submit = (): void => {
    if (!dest.trim()) return
    void onAdd(dest.trim())
    setDest('')
  }

  return (
    <div className="flex items-end gap-2 pt-2">
      <Field label="Destination id" className="flex-1">
        <Input mono value={dest} onChange={(e) => setDest(e.target.value)} placeholder="entity id" />
      </Field>
      <Button variant="secondary" onClick={submit} disabled={!dest.trim()}>
        Add link
      </Button>
    </div>
  )
}

// --- The cache --------------------------------------------------------------

/** What a load state means, said once so the panel doesn't have to explain it. */
const STATE_NOTE: Record<LoadState, string> = {
  unloaded: 'not read yet',
  loading: 'reading',
  loaded: 'read',
  error: 'failed',
}

/**
 * What the *cache* holds for this entity, as against what the store does. The
 * other tabs read straight from the source; this one is the only place the
 * client's own picture is visible, which matters mostly for one thing: an
 * `events` script runs in the background, once a session, and until now failed
 * in complete silence.
 */
function Cached({ entityId }: { entityId: string }): React.JSX.Element {
  const cache = useAtomValue(entitiesAtom)
  const entry = cache[entityId]
  const script = entry?.entity.values.events

  return (
    <>
      <div className="flex items-start justify-between gap-2">
        {entry ? (
          <div className="min-w-0 flex-1 overflow-hidden rounded-md bg-gray-50 font-mono text-xs">
            <Fact label="events" value={`${entry.events.length} (${STATE_NOTE[entry.loaded]})`} />
            <Fact
              label="derived"
              value={
                script == null
                  ? 'no events script'
                  : `${entry.derived.length} (${STATE_NOTE[entry.derivedState]})`
              }
            />
            {entry.error && <Fact label="read error" value={entry.error} bad />}
            {entry.derivedError && <Fact label="script error" value={entry.derivedError} bad />}
          </div>
        ) : (
          <p className={cn('flex-1', EMPTY)}>
            Nothing here — the client has never had reason to read this entity.
          </p>
        )}
        <Button variant="tertiary" size="sm" onClick={refreshDerived}>
          Recompute derived
        </Button>
      </div>
      {entry?.derived.length ? (
        <pre className="overflow-auto rounded-md bg-gray-50 p-3 font-mono text-xs">
          {JSON.stringify(entry.derived, null, 2)}
        </pre>
      ) : null}
      {script != null && entry?.derivedState === 'loaded' && !entry.derived.length && (
        <p className={EMPTY}>
          The script ran and returned no events. Anything it logged is in the devtools console.
        </p>
      )}
    </>
  )
}

function Fact({
  label,
  value,
  bad,
}: {
  label: string
  value: string
  bad?: boolean
}): React.JSX.Element {
  return (
    <div className="flex gap-2 px-3 py-1.5">
      <span className="w-24 shrink-0 text-gray-900">{label}</span>
      <span className={cn('break-all', bad ? 'text-error-600' : 'text-gray-500')}>{value}</span>
    </div>
  )
}
