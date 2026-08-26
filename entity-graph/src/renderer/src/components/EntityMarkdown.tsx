import React, { useEffect, useMemo, useState } from 'react'
import { Loading02 } from '@untitledui/icons'
import { Markdown } from './ui/Markdown'
import { CodeEditor } from './ui/CodeEditor'
import { Button } from './ui/Button'
import { CALL_STATUS } from './callStatus'
import { TypePill } from './TypePill'
import type { MarkdownFieldProps, MarkdownFields } from './ui/markdownFields'
import { elapsedTime } from '../helpers/time'
import { useAtomValue, useCallRunning, useCalls, useGetEntities } from '../state/hooks'
import { rowKey } from '../state/derive'
import { runTool } from '../tools/call'
import { integrationsAtom } from '../tools/integrationTools'
import { findToolByName, nearestToolNames } from '../tools/registry'
import { userToolsAtom } from '../tools/userTools'
import type { ToolSpec } from '../tools/types'

// An entity's text, rendered — with the custom inline forms wired up. `Markdown`
// itself knows nothing about entities or tools; this is where the two meet, and
// it is the component the outliner uses.
//
// Every field acts on the entity whose text it appears in, not on the selection:
// a button in a row's prose is a button on *that* row, wherever the cursor
// happens to be. `within` is what says so — the call is born along the row's own
// path, so the entity id is the row's and the folded context is the one that path
// gives (the tab's frame stack, then the row's ancestors), exactly as it would be
// for a script run on the same row.

/** Which entity a field belongs to, and where in the frame it sits. */
interface Where {
  entityId: string
  path: string[]
}

export function EntityMarkdown({
  entityId,
  path,
  text,
  className,
  style,
  highlight,
}: {
  entityId: string
  /** The row's path in the frame, which is what a field's calls are aimed along. */
  path: string[]
  text: string
  className?: string
  style?: React.CSSProperties
  /** The frame's find text, marked wherever the row says it. */
  highlight?: string
}): React.JSX.Element {
  // Keyed on the path's serialisation rather than the array: the rows are rebuilt
  // whenever anything lands in the entity cache, so the array is new constantly
  // while what it says is the same. The map's identity is a parse away for the
  // renderer, and a remount away for anything with a caret in it.
  const at = rowKey(path)
  const fields = useMemo<MarkdownFields>(() => {
    const where: Where = { entityId, path: at.split('\0') }
    return {
      button: ({ arg, text }: MarkdownFieldProps) => (
        <FieldButton where={where} name={arg} label={text} />
      ),
      codeEditor: ({ arg, text }: MarkdownFieldProps) => (
        <FieldCodeEditor where={where} field={arg} hint={text} />
      ),
      pill: ({ text }: MarkdownFieldProps) => <FieldPill label={text} />,
      tool: ({ arg, text }: MarkdownFieldProps) => <FieldToolCall callId={arg} label={text} />,
    }
  }, [entityId, at])

  return (
    <Markdown
      text={text}
      className={className}
      style={style}
      fields={fields}
      highlight={highlight}
    />
  )
}

/**
 * The tool a field names — by its id, or by the name a script would call it
 * (`tool.inspectEntity(…)`), since a button in a row of text is naming a tool for
 * the same reasons a script is. Read through the atoms the declared tools land
 * in, so a button naming an integration stops being dead once the source has
 * opened and said what the server can do.
 */
function useTool(name: string): ToolSpec | undefined {
  const integrations = useAtomValue(integrationsAtom)
  const userTools = useAtomValue(userToolsAtom)
  return useMemo(() => findToolByName(name), [name, integrations, userTools])
}

/**
 * `[@button:tool](label)` — the Actions button, shrunk to sit in a line of prose.
 * Pressing it is a gesture like any other: the call is the user's, so it toasts
 * what it did and keeps itself in the activity log on the same terms.
 *
 * It also says while it is going. A button that answers a minute later with a
 * toast and nothing in between reads as a button that did nothing, so the press
 * is held on to and the call watched — the mark beside the label is the same
 * still glyph a row wears while its entity is arriving, since nothing in this app
 * moves. Pressing a tool that still wants an argument opens the palette instead,
 * and there is no call to watch: `runTool` says so by handing back nothing.
 */
function FieldButton({
  where,
  name,
  label,
}: {
  where: Where
  name: string
  label: string
}): React.JSX.Element {
  const tool = useTool(name)
  const [callId, setCallId] = useState<string | null>(null)
  const running = useCallRunning(callId)
  // Named but not found: the same suggestion a script's error would carry, since
  // there is nowhere else a mistyped name in a row of text can show up.
  const nearest = tool ? [] : nearestToolNames(name)
  return (
    <Button
      variant="secondary"
      size="sm"
      // Small enough not to open up the line it sits in, and in the UI's own sans
      // rather than the serif of the text around it: it is a control, not prose.
      className="mx-0.5 h-5 px-1.5 align-middle font-sans text-[12px]"
      // A second press while the first is still going is nearly always a press
      // that thought nothing had happened.
      disabled={!tool || running}
      title={
        tool
          ? running
            ? `${tool.label} — running`
            : tool.label
          : `No tool “${name}”${nearest.length ? `. Did you mean ${nearest.join(', ')}?` : ''}`
      }
      onClick={() => tool && setCallId(runTool(tool.id, { within: where.path }))}
    >
      {running && <Loading02 size={11} className="shrink-0 text-gray-400" />}
      {label || tool?.label || name}
    </Button>
  )
}

/**
 * `[@codeEditor:field](hint)` — a code box over one of the entity's values.
 * Reading the value is also what loads the entity, so a field draws itself as
 * soon as its entity arrives.
 */
function FieldCodeEditor({
  where,
  field,
  hint,
}: {
  where: Where
  field: string
  hint: string
}): React.JSX.Element {
  const get = useGetEntities()
  const value = get([where.entityId])[where.entityId].values[field]
  return (
    <CodeEditor
      // Only text can be shown in a code box. Anything else — a number, an
      // object, the `true` that makes a row a section — reads as empty, and
      // typing replaces it, rather than the box pretending to be a JSON editor
      // it isn't.
      value={typeof value === 'string' ? value : ''}
      setValue={(next) =>
        runTool('entity.field.set', {
          within: where.path,
          extra: { fieldKey: field, fieldText: next },
        })
      }
      placeholder={hint}
    />
  )
}

/**
 * `[@pill](text)` — the pill a typed row wears, for a word in the middle of a
 * sentence. It is the one field that acts on nothing: a type pill is drawn from a
 * value on the entity, and this is the same shape said by the text itself, for
 * everything a row wants set apart that no value accounts for. No `:arg`, since
 * the word in the parens is the whole of it.
 */
function FieldPill({ label }: { label: string }): React.JSX.Element {
  return <TypePill label={label} className="mx-0.5 align-middle" />
}

/** A clock, ticking while something is going on and stopped when it isn't. */
function useNow(ticking: boolean): number {
  const [now, setNow] = useState(() => Date.now())
  useEffect(() => {
    if (!ticking) return
    // Straight away as well as on the interval: a call that started while this
    // row was off screen would otherwise show a stale second for a whole one.
    setNow(Date.now())
    const timer = setInterval(() => setNow(Date.now()), 1000)
    return () => clearInterval(timer)
  }, [ticking])
  return now
}

/**
 * `[@tool:callId](label)` — how a call is getting on, as a pill. The id is the
 * *call's* and not the tool's: a tool is run many times and what a note wants to
 * watch is the one turn it is about. A script names the call it is about to make
 * (`tool['claude.runPrompt']({ …, $callId: id })`), writes the field into a note,
 * and the note follows it from there.
 *
 * While it runs the pill counts: a session takes minutes, and the useful thing to
 * know is how many of them have gone rather than that it is still going. After
 * that it says how it ended, and the log is where the result actually is.
 */
function FieldToolCall({ callId, label }: { callId: string; label: string }): React.JSX.Element {
  const calls = useCalls()
  const call = calls.find((c) => c.callId === callId)
  const running = call?.outcome.kind === 'running'
  const now = useNow(running)
  const status = call ? CALL_STATUS[call.outcome.kind] : null
  // No record is the ordinary state either side of a call: written before the
  // script gets to the call, and still here long after the log has rolled past
  // it. Neither is worth an alarm, so it reads as the absence it is.
  //
  // Lower case, unlike the same word in the log: this one sits in a sentence
  // beside whatever the field was labelled, and `Claude Done` reads as two
  // labels rather than one thing and its state.
  const said = !call
    ? 'no record'
    : running
      ? elapsedTime(now - call.settledAt)
      : CALL_STATUS[call.outcome.kind].label.toLowerCase()
  return (
    <TypePill
      label={[label, said].filter(Boolean).join(' ')}
      dot={status?.color ?? 'gray'}
      title={
        call?.outcome.kind === 'error'
          ? call.outcome.message
          : call
            ? undefined
            : `Nothing in the activity log is called ${callId}`
      }
      className="mx-0.5 align-middle"
    />
  )
}
