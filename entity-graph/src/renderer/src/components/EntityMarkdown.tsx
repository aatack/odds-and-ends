import React, { useMemo } from 'react'
import { Markdown } from './ui/Markdown'
import { CodeEditor } from './ui/CodeEditor'
import { Button } from './ui/Button'
import { TypePill } from './TypePill'
import type { MarkdownFieldProps, MarkdownFields } from './ui/markdownFields'
import { useAtomValue, useGetEntities } from '../state/hooks'
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
}: {
  entityId: string
  /** The row's path in the frame, which is what a field's calls are aimed along. */
  path: string[]
  text: string
  className?: string
  style?: React.CSSProperties
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
    }
  }, [entityId, at])

  return <Markdown text={text} className={className} style={style} fields={fields} />
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
      disabled={!tool}
      title={
        tool
          ? tool.label
          : `No tool “${name}”${nearest.length ? `. Did you mean ${nearest.join(', ')}?` : ''}`
      }
      onClick={() => tool && runTool(tool.id, { within: where.path })}
    >
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
