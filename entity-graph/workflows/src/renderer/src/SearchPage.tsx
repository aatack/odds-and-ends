import { ArrowDown, ArrowUp, X } from 'lucide-react'
import { useEffect, useState } from 'react'
import { BUILTIN_SECTIONS, DEFAULT_SECTIONS, type SearchSection } from '../../shared/search'
import { fenced, Markdown } from './components/ui/Markdown'
import { SECTIONS } from './sections'
import { api } from './useWorkflows'

const QUERY_EXAMPLE = { and: [{ eq: { key: 'type', value: 'task' } }, { eq: { key: 'resolved', value: false } }, { contains_ignore_case: { key: 'title', value: 'review' } }] }
const FUNCTION_EXAMPLE = `// context.text is what is typed; tools are the MCP's (no unsafe ones). Answer rows.
const items = await tools.list_items({})
return items.filter((i) => !i.closed && i.assignees.length === 0).map((i) => ({ id: i.id, type: 'task', left: i.title, right: 'nobody has it' }))`

/** A section, drafted: a query's filter as JSON text, so it can be typed. */
type Draft = SearchSection & { filterText?: string; error?: string }

const draftOf = (s: SearchSection): Draft => (s.kind === 'query' ? { ...s, filterText: JSON.stringify(s.filter, null, 2) } : s)

/**
 * The search page's sections, one list for every finder (the Tasks view, the pickers): kept on the `search` item,
 * which bots change too (set_search_sections). Each is built in, a query (egdb's filter, over every event's values),
 * or a function (JavaScript answering rows). Save keeps them; Defaults puts the built-in ones back.
 */
export function SearchPage() {
  const [drafts, setDrafts] = useState<Draft[] | null>(null)
  const [error, setError] = useState<string | null>(null)
  const [saved, setSaved] = useState(false)
  useEffect(() => void api().searchSections().then((s) => setDrafts(s.map(draftOf))), [])
  if (!drafts) return <p className="muted">Reading…</p>
  const set = (i: number, d: Partial<Draft>) => {
    setSaved(false)
    setDrafts(drafts.map((x, j) => (j === i ? ({ ...x, ...d } as Draft) : x)))
  }
  const move = (i: number, by: number) => {
    const next = [...drafts]
    const [d] = next.splice(i, 1)
    next.splice(i + by, 0, d)
    setDrafts(next)
    setSaved(false)
  }
  const save = async () => {
    setError(null)
    try {
      const sections: SearchSection[] = drafts.map((d) => {
        if (d.kind === 'query') {
          const { filterText, error: _e, ...q } = d
          return { ...q, filter: JSON.parse(filterText ?? '{}') }
        }
        const { error: _e, filterText: _f, ...rest } = d
        return rest as SearchSection
      })
      await api().setSearchSections(sections)
      setSaved(true)
    } catch (e) {
      setError((e as Error).message.replace(/^Error invoking remote method '\w+': (Error: )?/, ''))
    }
  }
  return (
    <div className="page search-page">
      <Markdown
        className="muted"
        text="What every finder shows (the Tasks view, `@`, the pickers), in order. What is typed filters every section; a function is told it too. Bots can change these as well (`set_search_sections`)."
      />
      {error && <div className="error">{error}</div>}
      {drafts.map((d, i) => (
        <section key={i} className="card search-section">
          <div className="search-section-head">
            <select
              value={d.kind}
              onChange={(e) => {
                const kind = e.target.value as Draft['kind']
                setSaved(false)
                setDrafts(
                  drafts.map((x, j) =>
                    j !== i
                      ? x
                      : kind === 'builtin'
                        ? { kind, id: 'needsYou' }
                        : kind === 'query'
                          ? { kind, label: 'label' in x && x.label ? x.label : 'New query', filter: QUERY_EXAMPLE, filterText: JSON.stringify(QUERY_EXAMPLE, null, 2) }
                          : { kind, label: 'label' in x && x.label ? x.label : 'New function', source: FUNCTION_EXAMPLE },
                  ),
                )
              }}
            >
              <option value="builtin">Built in</option>
              <option value="query">Query</option>
              <option value="function">Function</option>
            </select>
            {d.kind === 'builtin' ? (
              <select value={d.id} onChange={(e) => set(i, { id: e.target.value as (typeof BUILTIN_SECTIONS)[number] })}>
                {BUILTIN_SECTIONS.map((id) => (
                  <option key={id} value={id}>
                    {SECTIONS[id].label}
                  </option>
                ))}
              </select>
            ) : (
              <input className="search-label" value={d.label} placeholder="Its heading" onChange={(e) => set(i, { label: e.target.value })} />
            )}
            <span className="buttons">
              <button data-ui className="link" disabled={i === 0} title="Up" onClick={() => move(i, -1)}>
                <ArrowUp className="icon" />
              </button>
              <button data-ui className="link" disabled={i === drafts.length - 1} title="Down" onClick={() => move(i, 1)}>
                <ArrowDown className="icon" />
              </button>
              <button data-ui className="link" title="Remove" onClick={() => setDrafts(drafts.filter((_, j) => j !== i))}>
                <X className="icon" />
              </button>
            </span>
          </div>
          {d.kind === 'query' && (
            <textarea className="search-code" rows={6} value={d.filterText} onChange={(e) => set(i, { filterText: e.target.value })} spellCheck={false} />
          )}
          {d.kind === 'function' && (
            <textarea className="search-code" rows={8} value={d.source} onChange={(e) => set(i, { source: e.target.value })} spellCheck={false} />
          )}
        </section>
      ))}
      <div className="buttons">
        <button data-ui onClick={() => setDrafts([...drafts, { kind: 'query', label: 'New query', filter: QUERY_EXAMPLE, filterText: JSON.stringify(QUERY_EXAMPLE, null, 2) }])}>
          Add a section
        </button>
        <button data-ui onClick={() => setDrafts(DEFAULT_SECTIONS.map(draftOf))}>Defaults</button>
        <button data-ui className="primary" onClick={() => void save()}>
          Save
        </button>
        {saved && <span className="muted">Saved.</span>}
      </div>
      <details>
        <summary className="muted">How queries and functions work</summary>
        <Markdown
          text={`A **query** is egdb's filter, over every event's values (\`type\`, \`title\`, \`text\`, \`resolved\` and the rest of its params) and \`parent\`, \`assignees\`, \`created_by\`, \`created_at\`:\n\n${fenced(JSON.stringify(QUERY_EXAMPLE, null, 2), 'json')}\n\nA **function** runs as a script's body does, and answers rows \`{ id, type, left, right? }\` (markdown):\n\n${fenced(FUNCTION_EXAMPLE, 'js')}`}
        />
      </details>
    </div>
  )
}
