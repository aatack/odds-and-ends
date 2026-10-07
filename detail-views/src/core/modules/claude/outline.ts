/** An item made from a piece of Claude's answer: a heading, a bullet, or a block. */
export interface OutlineItem {
  text: string
  section?: boolean
  /** A task-list bullet: true unticked, false ticked. */
  open?: boolean
  children: OutlineItem[]
}

export interface Outline {
  /** The prose before the first heading or bullet: the response's own text. */
  lead: string
  items: OutlineItem[]
}

const heading = /^(#{1,6})\s+(.*?)\s*#*\s*$/
const bullet = /^(\s*)(?:[-*+]|\d+[.)])\s+(.*)$/
const box = /^\[([ xX])\]\s+(.*)$/
const rule = /^\s{0,3}([-*_])(\s*\1){2,}\s*$/
const fence = /^\s*(```|~~~)/

const indentOf = (line: string) => line.length - line.trimStart().length

/**
 * Markdown as a tree of items. Headings are sections holding what follows
 * them, down to the next heading as big; bullets nest by indentation, under
 * their heading; paragraphs, code blocks and tables are one item each, under
 * the heading or bullet they sit in. Prose before any of that is the lead.
 */
export function outline(markdown: string): Outline {
  const lines = markdown.replace(/\r\n?/g, '\n').split('\n')
  const lead: string[] = []
  const root: OutlineItem[] = []
  const headings: { level: number; item: OutlineItem }[] = []
  /** The bullets open under the current heading, outermost first. */
  let bullets: { indent: number; item: OutlineItem }[] = []
  /** The paragraph being gathered, and the bullet it continues, if any. */
  let paragraph: string[] = []
  let paragraphIndent = 0
  let structured = false

  const container = () => headings.at(-1)?.item.children ?? root
  /** Where a block at this indent goes: inside the deepest bullet it is indented under, else its heading. */
  const placeFor = (indent: number) => {
    while (bullets.length && bullets.at(-1)!.indent >= indent) bullets.pop()
    return bullets.at(-1)?.item.children ?? container()
  }
  const add = (item: OutlineItem, indent: number) => {
    placeFor(indent).push(item)
    structured = true
  }
  const flush = () => {
    if (!paragraph.length) return
    const text = paragraph.join('\n')
    paragraph = []
    if (!structured && !bullets.length && !headings.length) lead.push(text)
    else add({ text, children: [] }, paragraphIndent)
  }

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i]
    if (fence.test(line)) {
      flush()
      const marker = line.trim().slice(0, 3)
      const block = [line]
      while (++i < lines.length) {
        block.push(lines[i])
        if (lines[i].trim().startsWith(marker)) break
      }
      const indent = indentOf(line)
      const text = block.map((one) => one.slice(Math.min(indent, indentOf(one)))).join('\n')
      if (!structured && !bullets.length && !headings.length) lead.push(text)
      else add({ text, children: [] }, indent)
      continue
    }
    if (!line.trim()) {
      flush()
      continue
    }
    if (rule.test(line)) {
      flush()
      continue
    }
    const titled = heading.exec(line)
    if (titled) {
      flush()
      const level = titled[1].length
      while (headings.length && headings.at(-1)!.level >= level) headings.pop()
      const item: OutlineItem = { text: titled[2], section: true, children: [] }
      container().push(item)
      headings.push({ level, item })
      bullets = []
      structured = true
      continue
    }
    const listed = bullet.exec(line)
    if (listed) {
      flush()
      const indent = listed[1].length
      const ticked = box.exec(listed[2])
      const item: OutlineItem = ticked ? { text: ticked[2], open: ticked[1] === ' ', children: [] } : { text: listed[2], children: [] }
      add(item, indent)
      bullets.push({ indent, item })
      continue
    }
    // A line straight after a bullet, indented under it, carries on the bullet's text.
    const last = bullets.at(-1)
    if (!paragraph.length && last && indentOf(line) > last.indent && lines[i - 1]?.trim() && !heading.test(lines[i - 1])) {
      last.item.text += ` ${line.trim()}`
      continue
    }
    if (!paragraph.length) paragraphIndent = indentOf(line)
    paragraph.push(paragraph.length ? line.trimStart() : line.trim())
  }
  flush()
  return { lead: lead.join('\n\n'), items: root }
}
