import { Star } from 'lucide-react'
import { useEffect, useRef, useState } from 'react'
import type { Catalog, CustomAction, ItemKind } from '../../shared/types'
import { actionIcon } from './actionIcons'
import { Markdown } from './components/ui/Markdown'
import { itemActions } from './itemActions'
import { Form } from './forms'
import { Params } from './Params'
import { TypeIcon, TYPE_NAME } from './icons'
import { api } from './useWorkflows'

type Act = (fn: () => Promise<unknown>) => Promise<void>
type Draft = Omit<CustomAction, 'id'> & { id?: string }

/** How a custom action is edited: through the one form, like everything else. */
const ACTION_FORM = {
  type: 'object',
  properties: {
    name: { type: 'string', title: 'Name' },
    description: { type: 'string', title: 'Description', 'x-multiline': true, description: 'What it does: shown on hover, and what its code is written from.' },
    notes: { type: 'string', title: 'Notes', 'x-multiline': true, description: 'More for the model writing its code: how, edge cases, which actions to use. Only shown here.' },
    icon: { type: 'string', title: 'Icon', 'x-editor': 'action-icon' },
    important: { type: 'boolean', title: 'Important', description: 'A button in the header, rather than in the menu beside the buttons.' },
    source: {
      type: 'string',
      title: 'Code',
      'x-multiline': true,
      'x-monospace': true,
      description: 'async (context, actions) => null | "said in the chat" | { message?, events? }. Generate writes it from the description and notes.',
    },
  },
  required: ['name'],
}

/**
 * Every event type, with what can be done to its events: the app's own actions
 * (read only: which are buttons, which are in the menu), then the human's own,
 * to add, change, generate the code of, or remove.
 */
/** `mobile`: one column, full width. */
export function TypesPage({ catalog, custom, act, mobile = false }: { catalog: Catalog; custom: CustomAction[]; act: Act; mobile?: boolean }) {
  const [draft, setDraft] = useState<Draft | null>(null)
  const [generating, setGenerating] = useState(false)
  /** The type whose fields are open, if any. */
  const [fields, setFields] = useState<string | null>(null)
  const noop = () => {}

  // Every change is saved as it is made, a moment after the typing stops. Saves
  // go one at a time, so a new action is inserted once and then updated by the id
  // it was given. `opened` counts the drafts opened, and `ids` holds the id each
  // new one got, so a save that lands after another was opened goes to its own.
  const latest = useRef<Draft | null>(null)
  const opened = useRef(0)
  const ids = useRef(new Map<number, string>())
  const timer = useRef<ReturnType<typeof setTimeout> | null>(null)
  const saving = useRef<Promise<unknown>>(Promise.resolve())
  const save = () => {
    if (timer.current) clearTimeout(timer.current)
    timer.current = null
    const which = opened.current
    const snapshot = latest.current
    saving.current = saving.current.then(() => {
      if (!snapshot?.name.trim()) return
      const d = { ...snapshot, id: snapshot.id ?? ids.current.get(which) }
      return act(async () => {
        const { id } = await api().saveCustomAction(d)
        if (d.id) return
        ids.current.set(which, id)
        if (which !== opened.current) return
        latest.current = latest.current && { ...latest.current, id }
        setDraft((now) => now && { ...now, id })
      })
    })
    return saving.current
  }
  const edit = (d: Draft) => {
    latest.current = d
    setDraft(d)
    if (timer.current) clearTimeout(timer.current)
    timer.current = setTimeout(() => void save(), 400)
  }
  /** Opens another action (or none), saving what was being edited first. */
  const open = (d: Draft | null) => {
    if (timer.current) void save()
    opened.current++
    latest.current = d
    setDraft(d)
  }
  useEffect(() => () => void (timer.current && save()), [])

  const generate = () =>
    draft &&
    act(async () => {
      setGenerating(true)
      try {
        const which = opened.current
        const source = await api().generateCustomAction(draft)
        if (which === opened.current && latest.current) edit({ ...latest.current, source })
      } finally {
        setGenerating(false)
      }
    })
  return (
    <div className={`page types-page ${mobile ? 'mobile' : ''}`}>
      {catalog.events.map((t) => {
        const builtIn = itemActions({ id: '', type: t.type, resolved: false, summary: '' }, { act: async () => {}, skip: noop })
        const mine = custom.filter((c) => c.type === t.type)
        const editing = draft?.type === t.type
        return (
          <section key={t.type} className="card">
            <h2 className="row-inline">
              <TypeIcon type={t.type as ItemKind} />
              {TYPE_NAME[t.type as ItemKind] ?? t.type} <code className="muted">{t.type}</code>
              {t.blocking && <span className="badge">blocker</span>}
              <button data-ui className="link fields-toggle" onClick={() => setFields(fields === t.type ? null : t.type)}>
                {fields === t.type ? 'Hide fields' : 'Fields'}
              </button>
            </h2>
            <Markdown className="muted" text={fields === t.type ? t.description : t.description.split('\n')[0]} />
            {fields === t.type && (
              <>
                <Params schema={t.params} />
                <code className="muted">→ {t.data}</code>
              </>
            )}
            <div className="type-actions">
              {builtIn.map((a) => (
                <div key={a.id} className="type-action builtin" title={a.title}>
                  <a.icon className={`icon ${a.tone}`} />
                  <span>{a.label}</span>
                  {a.important && <Star className="icon important" aria-label="Important" />}
                </div>
              ))}
              {mine.map((c) => {
                const Icon = actionIcon(c.icon)
                return (
                  <button key={c.id} className={`type-action ${draft?.id === c.id ? 'editing' : ''}`} title={c.description} onClick={() => open(c)}>
                    <Icon className="icon" />
                    <span>{c.name}</span>
                    {c.important && <Star className="icon important" aria-label="Important" />}
                  </button>
                )
              })}
              {!editing && (
                <button
                  className="type-action add"
                  onClick={() => open({ type: t.type, name: '', description: '', notes: '', source: '', icon: 'zap', important: false })}
                >
                  + New
                </button>
              )}
            </div>
            {editing && draft && (
              <div className="type-editor">
                <Form schema={ACTION_FORM} value={draft} setValue={(v) => edit({ ...draft, ...(v as Draft) })} />
                <div className="buttons">
                  <button data-ui disabled={generating || !(draft.description.trim() || draft.notes.trim())} onClick={() => void generate()}>
                    {generating ? 'Generating…' : draft.source.trim() ? 'Regenerate code' : 'Generate code'}
                  </button>
                  {draft.id && (
                    <button
                      data-ui
                      onClick={() =>
                        act(async () => {
                          if (timer.current) clearTimeout(timer.current)
                          timer.current = null
                          await saving.current
                          await api().removeCustomAction(latest.current?.id ?? draft.id!)
                          open(null)
                        })
                      }
                    >
                      Delete
                    </button>
                  )}
                  <button data-ui onClick={() => open(null)}>
                    Done
                  </button>
                  {!draft.name.trim() && <span className="muted">Saved once it has a name.</span>}
                </div>
              </div>
            )}
          </section>
        )
      })}
    </div>
  )
}
