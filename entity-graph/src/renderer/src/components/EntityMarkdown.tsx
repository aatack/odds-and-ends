import React, { useMemo } from 'react'
import { Markdown } from './ui/Markdown'
import { CodeEditor } from './ui/CodeEditor'
import { TypePill } from './TypePill'
import { PillBackground, PillContent, PillWrapper } from './EntityPill'
import type { MarkdownFieldProps, MarkdownFields } from './ui/markdownFields'
import { useGetEntities } from '../state/hooks'
import { rowKey } from '../state/derive'
import { runTool } from '../tools/call'

// An entity's text, rendered — with the custom inline forms wired up. `Markdown`
// itself knows nothing about entities; this is where the two meet, and it is the
// component the outliner uses.
//
// A field acts on the entity whose text it appears in, not on the selection: a
// code box in a row's prose writes to *that* row, wherever the cursor happens to
// be. `within` is what says so — the call is born along the row's own path.

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
      codeEditor: ({ arg, text }: MarkdownFieldProps) => (
        <FieldCodeEditor where={where} field={arg} hint={text} />
      ),
      entity: ({ arg, text }: MarkdownFieldProps) => <FieldEntity id={arg} fallback={text} />,
      pill: ({ text }: MarkdownFieldProps) => <FieldPill label={text} />,
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

/**
 * `[@entity:id](label)` — another entity, mentioned in the middle of a sentence,
 * as a pill that can be right-clicked and followed like any other.
 *
 * The label is a *fallback* rather than the caption: an entity with text of its
 * own shows that, so a note renamed is renamed everywhere it is mentioned, which
 * is the whole reason a mention names an id rather than spelling out a name. The
 * fallback is what shows until the entity has one.
 */
function FieldEntity({ id, fallback }: { id: string; fallback: string }): React.JSX.Element {
  const entities = useGetEntities()
  const known = !!entities([id])[id]?.values.text
  return (
    <PillWrapper id={id} title={fallback} className="mx-0.5 align-middle">
      <PillBackground>
        {known ? <PillContent id={id} /> : <span className="font-serif">{fallback}</span>}
      </PillBackground>
    </PillWrapper>
  )
}
