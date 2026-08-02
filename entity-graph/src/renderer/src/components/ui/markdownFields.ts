import type { Root, RootContent, Text } from 'mdast'

// The custom inline forms entity text can carry: `[@type:arg](text)`, which the
// renderer hands to a component instead of drawing. A row is prose most of the
// time, and occasionally it wants a button in the middle of a sentence or a box
// that edits one of the entity's values — so the syntax is deliberately the one
// markdown already has for a link, with an `@type:` label that no real link
// would use.
//
// The `:arg` is optional, because not every form has anything to point at: a
// button names a tool and a code box names a value key, but `[@pill](text)` is
// the whole of what a pill needs. A form written without one arrives with an
// empty `arg` rather than being left in the text.
//
// Links themselves are untouched: only a label of exactly that shape is taken,
// and only for a type the caller said it can render. Anything else — including
// `[@nonesuch:x](y)` — falls through and renders as it always did.
//
// Two shapes have to be caught, because markdown parses the same form two
// different ways. `[@button:x]()` is a valid link (empty destination), so it
// arrives as a `link` node; `[@codeEditor:x](a hint)` is *not* (a link
// destination may not contain spaces), so it arrives as literal text. Handling
// both here is what lets the text in the parens be an ordinary phrase.
//
// Fenced and inline code are safe by construction: neither is a text node, and
// the walk below only rewrites text and links. A row explaining the syntax can
// therefore quote it.

/** What a field's two parts are called once parsed out of the label. */
export interface MarkdownFieldProps {
  /** The `:arg` half of the label — a tool id, a value key. */
  arg: string
  /** Whatever stood in the parens: a hint, a caption. */
  text: string
}

/** Field renderers by type name, e.g. `{ button: …, codeEditor: … }`. */
export type MarkdownFields = Record<string, React.ComponentType<MarkdownFieldProps>>

/**
 * The element the plugin leaves behind, which the renderer maps to a component.
 * A hyphen makes it a custom element, so it can never collide with a tag the
 * markdown itself produced.
 */
export const FIELD_TAG = 'markdown-field'

/**
 * The parts are carried as `data-` attributes rather than plain properties
 * because that is the one naming a hast → JSX conversion passes through
 * untouched; a camel-cased property is liable to arrive lower-cased.
 */
export interface FieldElementProps {
  'data-field-type'?: string
  'data-field-arg'?: string
  'data-field-text'?: string
}

/** A whole form, found in text: `[@type:arg](text)`, or `[@type](text)`. */
const IN_TEXT = /\[@([A-Za-z][A-Za-z0-9]*)(?::([^\]\s]*))?\]\(([^()\n]*)\)/g

/** The label half of it, which is all a parsed link has left of the form. */
const AS_LABEL = /^@([A-Za-z][A-Za-z0-9]*)(?::(\S*))?$/

interface Field {
  type: string
  arg: string
  text: string
}

/**
 * The node the transform inserts. Its own type is never rendered — `hName` and
 * `hProperties` are what the markdown → HTML step reads, and an unknown node
 * carrying them becomes exactly that element.
 */
const fieldNode = (field: Field): RootContent =>
  ({
    type: 'markdownField',
    data: {
      hName: FIELD_TAG,
      hProperties: {
        'data-field-type': field.type,
        'data-field-arg': field.arg,
        'data-field-text': field.text,
      },
    },
  }) as unknown as RootContent

interface Parent {
  children: RootContent[]
}

const hasChildren = (node: RootContent): node is RootContent & Parent =>
  Array.isArray((node as Parent).children)

/** The field a parsed link stands for, if its label says it is one. */
function linkField(node: RootContent & Parent, known: Set<string>): Field | null {
  if (node.type !== 'link') return null
  const [only] = node.children
  if (node.children.length !== 1 || !only || only.type !== 'text') return null
  const match = AS_LABEL.exec(only.value)
  if (!match || !known.has(match[1])) return null
  return { type: match[1], arg: match[2] ?? '', text: node.url }
}

/** One text node, split around the forms in it. The node itself if there are none. */
function splitText(node: Text, known: Set<string>): RootContent[] {
  if (!node.value.includes('[@')) return [node]
  const out: RootContent[] = []
  let at = 0
  for (const match of node.value.matchAll(IN_TEXT)) {
    const [whole, type, arg, text] = match
    if (!known.has(type)) continue
    if (match.index > at) out.push({ type: 'text', value: node.value.slice(at, match.index) })
    out.push(fieldNode({ type, arg: arg ?? '', text }))
    at = match.index + whole.length
  }
  if (out.length === 0) return [node]
  if (at < node.value.length) out.push({ type: 'text', value: node.value.slice(at) })
  return out
}

function rewrite(node: Parent, known: Set<string>): void {
  const out: RootContent[] = []
  for (const child of node.children) {
    if (child.type === 'text') {
      out.push(...splitText(child, known))
      continue
    }
    if (hasChildren(child)) {
      const field = linkField(child, known)
      if (field) {
        out.push(fieldNode(field))
        continue
      }
      rewrite(child, known)
    }
    out.push(child)
  }
  node.children = out
}

/**
 * The remark plugin, for the field types the caller can actually draw. A type it
 * doesn't name is left in the text, so an unrecognised form reads as the source
 * that produced it rather than disappearing.
 */
export const markdownFieldsPlugin =
  (types: readonly string[]) =>
  () =>
  (tree: Root): void => {
    if (types.length > 0) rewrite(tree, new Set(types))
  }
