// The schema components: a form for a value a JSON Schema describes, and a
// builder for the schema itself. Everything here depends only on React, the
// icon set and four of the app's UI primitives (`../ui/Dropdown`, `Modal`,
// `Switch`, `TextEditor`) — Dropdown and Modal for the keyboard: while one is
// open it has the keys, which is the app's rule for dialogs.

export { SchemaForm } from './SchemaForm'
export { SchemaBuilder } from './SchemaBuilder'
export { JsonEditor } from './JsonEditor'
export {
  KINDS,
  KIND_NAMES,
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
