// The schema components: a form for a value a JSON Schema describes, and a
// builder for the schema itself. Everything here depends only on React, the
// icon set and three of the app's UI primitives (`../ui/Dropdown`, `Modal`,
// `TextEditor`) — Dropdown and Modal for the keyboard: while one is
// open it has the keys, which is the app's rule for dialogs.
//
// A host can draw any value its own way: a schema names an editor
// (`x-editor`), and `editors` on the form or builder says what the name means.
// `./editors` is the whole of it.

export { SchemaForm } from './SchemaForm'
export { SchemaBuilder } from './SchemaBuilder'
export { JsonEditor } from './JsonEditor'
export {
  EditorsProvider,
  useEditors,
  type Editor,
  type EditorProps,
  type Editors,
} from './editors'
export {
  KINDS,
  KIND_NAMES,
  EDITOR_KEYWORD,
  META_SCHEMA,
  MONOSPACE_KEYWORD,
  MULTILINE_KEYWORD,
  POPOUT_KEYWORD,
  defaultFor,
  isObject,
  kindOf,
  matches,
  optionNames,
  pretty,
  problemOf,
  type JsonSchema,
  type Kind,
} from './schema'
