import type { EntityRow } from '../state/derive'

// Exporting a subtree as markdown. The one place the app hands its contents to
// something else, and the reason it is worth having on a phone at all: the outline
// is where the thinking is, and the message you are about to send is somewhere else.

/** Markdown has six heading levels; a deeper section takes the last of them. */
const HEADING_LIMIT = 6

/**
 * The entity at `start` and its visible descendants, as markdown.
 *
 * Folding is taken at its word: children of a folded row are not on screen, so they
 * are not in the export, and the folded row itself is left out along with them —
 * exporting the title of a branch whose contents are hidden reads as a complete
 * list when it isn't. The row being exported is the exception, having been named
 * explicitly.
 *
 * The tree stays a tree: every row is a bullet at its own depth, and a section
 * carries a `#` per level below the exported row on top of that bullet, so the
 * outline you exported is still the outline you paste.
 */
export function subtreeMarkdown(rows: EntityRow[], start: number): string {
  const from = rows[start]
  if (!from) return ''
  const lines: string[] = []
  for (let i = start; i < rows.length; i++) {
    const row = rows[i]
    if (i > start && row.depth <= from.depth) break
    if (i > start && row.collapsed && row.hasChildren) continue
    const depth = row.depth - from.depth
    const text = row.text ?? ''
    const heading = row.section ? `${'#'.repeat(Math.min(depth + 1, HEADING_LIMIT))} ` : ''
    if (i === start) {
      lines.push(`${heading}${text}`)
      continue
    }
    const box = row.open === true ? '[ ] ' : row.open === false ? '[x] ' : ''
    lines.push(`${'  '.repeat(depth - 1)}- ${box}${heading}${text}`)
  }
  return lines.join('\n')
}
