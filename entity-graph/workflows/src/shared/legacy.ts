// Reading an older build's values, as the engine (engine/store.ts) and the window (egdb-client's records) both do.

type Values = Record<string, unknown>
const num = (x: unknown): number | null => (typeof x === 'number' ? x : null)

/**
 * An older build's `task` and `note` as the topic they are now, read and never rewritten (so the older build still reads
 * them): a task's `title` is its `text`, and it is open till its `resolved` turned true (unless an `open` was set since);
 * a note keeps its checkbox (`open`) and shows its notes. Anything else is as it was.
 */
export function legacyValues(x: Values): Values {
  if (x.type === 'task') {
    const { title, resolved, cancelled: _c, data: _d, ...rest } = x
    return { ...rest, type: 'topic', text: rest.text ?? title, open: typeof rest.open === 'boolean' ? rest.open : !(resolved === true || num(rest.resolved_at) !== null) }
  }
  if (x.type === 'note') return { ...x, type: 'topic', display: x.display ?? 'notes' }
  return x
}
