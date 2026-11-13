import { Star } from 'lucide-react'
import { useEffect, useState } from 'react'
import type { Catalog, ItemInfo, ItemKind } from '../../shared/types'
import { Markdown } from './components/ui/Markdown'
import { itemActions } from './itemActions'
import { Params } from './Params'
import { TypeIcon, TYPE_NAME } from './icons'
import { ScriptCard } from './ScriptCard'
import { api } from './useWorkflows'

type Act = (fn: () => Promise<unknown>) => Promise<void>

/**
 * Every item type, with what can be done to its items: the app's own actions (read only: which are buttons, which are
 * in the menu), then the scripts the type owns (`set_owner` to its name), each a card that opens it. Every item of the
 * type offers those scripts in its right pane. New script makes one owned by the type, and opens it; bots write them
 * just as well (see the script type's description).
 */
export function TypesPage({ catalog, act, onOpen, version, mobile = false }: { catalog: Catalog; act: Act; onOpen: (id: string) => void; version: unknown; mobile?: boolean }) {
  /** The type whose fields are open, if any. */
  const [fields, setFields] = useState<string | null>(null)
  const [scripts, setScripts] = useState<Record<string, ItemInfo[]>>({})
  // Read again whenever anything changes (a bot may have given a type a script).
  useEffect(() => {
    let live = true
    void Promise.all(catalog.types.map(async (t) => [t.type, await api().typeScripts(t.type)] as const)).then((all) => live && setScripts(Object.fromEntries(all)))
    return () => {
      live = false
    }
  }, [catalog, version])
  const newScript = (type: string) =>
    act(async () => {
      const [id] = await api().addItems(null, [{ type: 'script', params: { name: `New ${type} script`, source: 'return null' } }])
      await api().setOwner(id, type)
      onOpen(id)
    })
  return (
    <div className={`page types-page ${mobile ? 'mobile' : ''}`}>
      {catalog.types.map((t) => {
        const builtIn = itemActions({ id: '', type: t.type, closed: false, summary: '' }, { act: async () => {} })
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
              <button className="type-action add" onClick={() => void newScript(t.type)}>
                + New script
              </button>
            </div>
            {(scripts[t.type] ?? []).length > 0 && (
              <div className="type-scripts">
                {scripts[t.type].map((s) => (
                  // Pressed, a script here opens (there is no item to run it on); every item of the type runs it from its right pane.
                  <div key={s.id} className="type-script" onClick={(e) => !(e.target as HTMLElement).closest('button') && onOpen(s.id)}>
                    <ScriptCard info={s} on={null} act={act} />
                  </div>
                ))}
              </div>
            )}
          </section>
        )
      })}
    </div>
  )
}
