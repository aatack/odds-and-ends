import type { Root, RootContent, Text } from 'mdast'

// One phrase, marked wherever it appears in rendered text. The find filter is
// what asks for it — a row kept because it says the word is a row where the word
// is worth pointing at — but nothing here knows that, so anything else with a
// phrase to draw attention to can hand one in.
//
// It works on the *rendered* text rather than on the source, which is the whole
// reason it is a plugin and not a `replace` on the string: a match spanning a
// piece of markup is not a match anybody typed, and wrapping one in a tag would
// break the markup it straddles. Splitting text nodes leaves every other node
// exactly as it was.
//
// Matching is the filter's own: case-insensitive, on the trimmed phrase, so what
// lights up is what the row was kept for.

/**
 * The node the transform inserts. `hName` is what the markdown → HTML step reads
 * off an unknown node, so this becomes a `<mark>` around the matched text; the
 * colour is in `index.css` with the rest of the markdown's typography.
 */
const markNode = (value: string): RootContent =>
  ({
    type: 'markdownHighlight',
    children: [{ type: 'text', value }],
    data: { hName: 'mark' },
  }) as unknown as RootContent

interface Parent {
  children: RootContent[]
}

const hasChildren = (node: RootContent): node is RootContent & Parent =>
  Array.isArray((node as Parent).children)

/** One text node, split around the matches in it. The node itself if there are none. */
function splitText(node: Text, needle: string): RootContent[] {
  const haystack = node.value.toLowerCase()
  if (!haystack.includes(needle)) return [node]
  const out: RootContent[] = []
  let at = 0
  for (let found = haystack.indexOf(needle); found >= 0; found = haystack.indexOf(needle, at)) {
    if (found > at) out.push({ type: 'text', value: node.value.slice(at, found) })
    // Sliced out of the original rather than written from the needle, so a match
    // keeps the capitals it was typed with.
    out.push(markNode(node.value.slice(found, found + needle.length)))
    at = found + needle.length
  }
  if (at < node.value.length) out.push({ type: 'text', value: node.value.slice(at) })
  return out
}

function rewrite(node: Parent, needle: string): void {
  const out: RootContent[] = []
  for (const child of node.children) {
    if (child.type === 'text') {
      out.push(...splitText(child, needle))
      continue
    }
    if (hasChildren(child)) rewrite(child, needle)
    out.push(child)
  }
  node.children = out
}

/**
 * The remark plugin. An empty phrase marks nothing rather than everything, which
 * matters: the find field is on screen holding an empty string for as long as it
 * takes to type the first letter.
 */
export const markdownHighlightPlugin =
  (highlight: string) =>
  () =>
  (tree: Root): void => {
    const needle = highlight.trim().toLowerCase()
    if (needle) rewrite(tree, needle)
  }
