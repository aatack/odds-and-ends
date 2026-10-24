import { Star } from 'lucide-react'
import { useState } from 'react'
import type { Catalog, CustomAction, EventKind } from '../../shared/types'
import { ACTION_ICONS, actionIcon } from './actionIcons'
import { Markdown } from './components/ui/Markdown'
import { eventActions } from './eventActions'
import { Form } from './forms'
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
    icon: { title: 'Icon', enum: Object.keys(ACTION_ICONS) },
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
export function TypesPage({ catalog, custom, act }: { catalog: Catalog; custom: CustomAction[]; act: Act }) {
  const [draft, setDraft] = useState<Draft | null>(null)
  const [generating, setGenerating] = useState(false)
  const noop = () => {}
  const generate = () =>
    draft &&
    act(async () => {
      setGenerating(true)
      try {
        const source = await api().generateCustomAction(draft)
        setDraft((d) => d && { ...d, source })
      } finally {
        setGenerating(false)
      }
    })
  return (
    <div className="page types-page">
      {catalog.events.map((t) => {
        const builtIn = eventActions({ id: '', type: t.type, resolved: false, summary: '' }, { act: async () => {}, skip: noop, attach: noop })
        const mine = custom.filter((c) => c.type === t.type)
        const editing = draft?.type === t.type
        return (
          <section key={t.type} className="card">
            <h2 className="row-inline">
              <TypeIcon type={t.type as EventKind} />
              {TYPE_NAME[t.type as EventKind] ?? t.type} <code className="muted">{t.type}</code>
            </h2>
            <Markdown className="muted" text={t.description.split('\n')[0]} />
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
                  <button key={c.id} className={`type-action ${draft?.id === c.id ? 'editing' : ''}`} title={c.description} onClick={() => setDraft(c)}>
                    <Icon className="icon" />
                    <span>{c.name}</span>
                    {c.important && <Star className="icon important" aria-label="Important" />}
                  </button>
                )
              })}
              {!editing && (
                <button
                  className="type-action add"
                  onClick={() => setDraft({ type: t.type, name: '', description: '', notes: '', source: '', icon: 'zap', important: false })}
                >
                  + New action
                </button>
              )}
            </div>
            {editing && draft && (
              <div className="type-editor">
                <Form schema={ACTION_FORM} value={draft} setValue={(v) => setDraft({ ...draft, ...(v as Draft) })} />
                <div className="buttons">
                  <button
                    data-ui
                    className="primary"
                    disabled={!draft.name.trim()}
                    onClick={() =>
                      act(async () => {
                        await api().saveCustomAction(draft)
                        setDraft(null)
                      })
                    }
                  >
                    Save
                  </button>
                  <button data-ui disabled={generating || !(draft.description.trim() || draft.notes.trim())} onClick={() => void generate()}>
                    {generating ? 'Generating…' : draft.source.trim() ? 'Regenerate code' : 'Generate code'}
                  </button>
                  {draft.id && (
                    <button
                      data-ui
                      onClick={() =>
                        act(async () => {
                          await api().removeCustomAction(draft.id!)
                          setDraft(null)
                        })
                      }
                    >
                      Delete
                    </button>
                  )}
                  <button data-ui onClick={() => setDraft(null)}>
                    Cancel
                  </button>
                </div>
              </div>
            )}
          </section>
        )
      })}
    </div>
  )
}
