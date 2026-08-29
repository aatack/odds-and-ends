import type { TreeRow } from './tree'

// An outline as markdown: the one format the graph hands to something that isn't
// one of its own screens. Shared, because there are three callers and they have
// to agree — the desktop's export, the phone's, and the answer the `query` tool
// gives an agent over MCP, which is the same outline with the entity ids down the
// left so that a model can act on a row it has read.

/** Markdown has six heading levels; a deeper section takes the last of them. */
const HEADING_LIMIT = 6

export interface MarkdownOptions {
  /**
   * Put each row's entity id in front of it, which is what makes the outline
   * addressable: a model reads a line and already has the id to write to. The
   * column is padded to the widest id so the indentation still reads as
   * indentation — ids differ in length the moment `@index` sits above a uuid.
   */
  ids?: boolean
}

/**
 * Rows as markdown, each at its own depth.
 *
 * A row at depth 0 is the title of what was asked for rather than an item in it,
 * so it takes neither bullet nor checkbox — but it keeps its `#` if it is a
 * section, that being what a section *is*. Everything below it is a bullet
 * indented by its depth, and a section carries a `#` per level on top of that
 * bullet, so the outline that went in is still the outline that comes out. Deep
 * rows therefore indent past the four spaces markdown reads as code: the shape is
 * worth more here than the strict reading.
 */
export function outlineMarkdown(rows: readonly TreeRow[], opts: MarkdownOptions = {}): string {
  const width = opts.ids ? Math.max(0, ...rows.map((row) => row.id.length)) : 0
  const lines: string[] = []

  for (const row of rows) {
    const bullet = row.depth === 0 ? '' : `${'    '.repeat(row.depth - 1)}- `
    const box =
      row.depth === 0 ? '' : row.open === true ? '[ ] ' : row.open === false ? '[x] ' : ''
    const heading = row.section ? `${'#'.repeat(Math.min(row.depth + 1, HEADING_LIMIT))} ` : ''
    const opener = `${bullet}${box}${heading}`
    const [first, ...rest] = (row.text ?? '').split('\n')

    if (!opts.ids) {
      lines.push(`${opener}${first}`, ...rest)
      continue
    }
    // A text with newlines in it — a fenced code block, a table — would otherwise
    // put lines in the id column that name no entity. Its later lines get a blank
    // column and the row's own indentation instead, so every line that starts
    // with an id is a row, and a row's text stays under its own bullet.
    lines.push(`${row.id.padEnd(width)}  ${opener}${first}`)
    for (const line of rest) lines.push(`${' '.repeat(width + 2 + opener.length)}${line}`)
  }

  return lines.join('\n')
}

/**
 * The row at `start` and its descendants, as markdown.
 *
 * Folding is taken at its word: children of a folded row are not on screen, so
 * they are not in `rows` to begin with, and the folded row itself is left out
 * along with them — exporting the title of a branch whose contents are hidden
 * reads as a complete list when it isn't. The row being exported is the
 * exception, having been named explicitly, so it comes out folded or not.
 *
 * Depths are rebased on it: what was exported is the title of what comes back,
 * whatever depth it happened to sit at.
 */
export function subtreeMarkdown(
  rows: readonly TreeRow[],
  start: number,
  opts: MarkdownOptions = {},
): string {
  const from = rows[start]
  if (!from) return ''
  const kept: TreeRow[] = []
  for (let i = start; i < rows.length; i++) {
    const row = rows[i]
    if (i > start && row.depth <= from.depth) break
    if (i > start && row.collapsed && row.hasChildren) continue
    kept.push({ ...row, depth: row.depth - from.depth })
  }
  return outlineMarkdown(kept, opts)
}
