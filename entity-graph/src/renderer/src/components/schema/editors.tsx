import React, { createContext, useContext, useMemo } from 'react'
import { EDITOR_KEYWORD, kindOf, type JsonSchema, type Kind } from './schema'

// Custom editors: a host's own control for a value, used in place of the form's.
//
// A schema names its editor — `{ "type": "string", "x-editor": "colour" }` — and
// the host supplies what the name means, as React, through the `editors` prop of
// `SchemaForm` or `SchemaBuilder`:
//
//   <SchemaForm schema={…} value={…} setValue={…} editors={{
//     colour: {
//       name: 'Colour picker',
//       kinds: ['string'],
//       render: ({ value, onChange }) => (
//         <input type="color" value={String(value ?? '#000000')}
//                onChange={(e) => onChange(e.target.value)} />
//       ),
//     },
//   }} />
//
// So a schema stays plain JSON — something an agent can write, or a store can
// hold — and what an editor does lives in code. A schema naming an editor the
// host hasn't registered gets the ordinary control, so a schema never breaks for
// want of one.
//
// What an editor replaces is the control on the right of a line. The line around
// it — its label, the warning when the value breaks the schema, a list item's
// cross — is still the form's. With `x-popout` as well, the line keeps its pencil
// and the editor is drawn in the dialog instead of the form.
//
// An editor that runs code rather than drawing a control — a native picker, a
// tool call — is an editor that draws a button and calls `onChange` when the code
// comes back:
//
//   render: ({ value, onChange }) => (
//     <button onClick={async () => onChange(await pickSomething(value))}>Pick…</button>
//   )
//
// Editors registered anywhere above are inherited: a form inside a pop-out, a
// builder inside a form, and the builder's own option lines all see them, and
// an `editors` prop lower down adds to (or overrides) what it inherits.

/** What an editor is handed: the value, its schema, and how to change it. */
export interface EditorProps {
  /** The value as it stands; `undefined` where none has been given. */
  value: unknown
  /** Hand back the new value. `undefined` takes the value away. */
  onChange: (value: unknown) => void
  /** The schema the value is drawn from, `x-editor` and all. */
  schema: JsonSchema
  /** The line's label, where it has one — for an accessible name, say. */
  label?: string
}

export interface Editor {
  /** Draw the control. It fills the right of its line, which is 32px tall. */
  render: (props: EditorProps) => React.ReactNode
  /** What the builder calls it when offering it. The id where absent. */
  name?: string
  /**
   * The kinds of schema it can edit, which is where the builder offers it.
   * Absent: every kind.
   */
  kinds?: readonly Kind[]
  /**
   * It takes more than one line, so the label sits at the top of the line rather
   * than in its middle, as for a list.
   */
  tall?: boolean
}

/** Editors by id: the id is what a schema's `x-editor` names. */
export type Editors = Record<string, Editor>

const EditorsContext = createContext<Editors>({})

/**
 * Make editors available to every form and builder inside, on top of any already
 * there. `SchemaForm` and `SchemaBuilder` do this with their `editors` prop; use
 * it directly to register editors once, high up, for everything below.
 */
export function EditorsProvider({
  editors,
  children,
}: {
  editors?: Editors
  children: React.ReactNode
}): React.JSX.Element {
  const inherited = useContext(EditorsContext)
  const merged = useMemo(
    () => (editors ? { ...inherited, ...editors } : inherited),
    [inherited, editors],
  )
  return <EditorsContext.Provider value={merged}>{children}</EditorsContext.Provider>
}

/** Every editor registered here. */
export const useEditors = (): Editors => useContext(EditorsContext)

/** The editor a schema names, if it names one that is registered. */
export function useEditorFor(schema: JsonSchema): Editor | null {
  const editors = useEditors()
  const id = schema[EDITOR_KEYWORD]
  return typeof id === 'string' ? (editors[id] ?? null) : null
}

/** The editors that can edit a kind of schema, by id — what the builder offers. */
export function editorsFor(editors: Editors, schema: JsonSchema): [string, Editor][] {
  const kind = kindOf(schema)
  return Object.entries(editors).filter(([, e]) => !e.kinds || e.kinds.includes(kind))
}
