import { autoUpdate, flip, offset, shift, useFloating } from '@floating-ui/react-dom'
import { useAtom, useSetAtom } from 'jotai'
import { ChevronRight, X } from 'lucide-react'
import { useEffect, useState, type MouseEvent, type ReactNode } from 'react'
import type { ItemInfoDetail, Overview } from '../../shared/types'
import { DISPLAYS, type Display } from '../../shared/views'
import { ItemPicker } from './ItemPicker'
import type { Recent } from './sections'
import { itemActions } from './itemActions'
import { everyone, nameOf } from './people'
import { api } from './useWorkflows'
import { itemText } from './itemText'
import { forwardAtom, HISTORY_LIMIT, menuAtom, renameAtom, sameView, viewsAtom, type View } from './views'

/** What to spread on anything standing for an item, so a right-click on it opens the item menu. */
export function useItemMenu() {
  const setMenu = useSetAtom(menuAtom)
  return (id: string) => ({
    onContextMenu: (e: MouseEvent) => {
      e.preventDefault()
      e.stopPropagation()
      setMenu({ x: e.clientX, y: e.clientY, id })
    },
  })
}

type Act = (fn: () => Promise<unknown>) => Promise<void>
/** A link the menu offers, and the picker its Choose… opens once the menu has closed. */
type LinkKind = 'pin' | 'assign' | 'owner'
const SUGGESTED = 5

/**
 * The right-click menu on an item, the same for every type, in sections:
 * - **Open** (its focus; hover for its other views: Comments, Notes, Assignments, Debug, Edit log), and Rename for a
 *   topic on screen.
 * - **Copy** (its card, as text; hover for its pill, card, focus or id).
 * - **Links**: Pin to, Assign to and Set owner, each with suggestions, Choose… (the picker) and the links it has
 *   now, to take off.
 * - **Its actions** (`itemActions`: Close or Reopen, its type's own, the custom ones), as the right pane has them.
 */
export function ItemMenu({ actOn, overview, recent }: { overview: Overview; recent: Recent; actOn: (id: string) => Act }) {
  const [menu, setMenu] = useAtom(menuAtom)
  const [views, setViews] = useAtom(viewsAtom)
  const setForwards = useSetAtom(forwardAtom)
  const setRename = useSetAtom(renameAtom)
  // The item itself, read as the menu opens: its actions, and the links it has.
  const [detail, setDetail] = useState<ItemInfoDetail | null>(null)
  // Which submenu is open (one at a time, by its row).
  const [sub, setSub] = useState<string | null>(null)
  const [picking, setPicking] = useState<{ kind: LinkKind; id: string } | null>(null)
  // Placed by Floating UI at the cursor: flipped above it, or shifted along, so it always stays in the window,
  // and placed again as it grows (its actions come in a moment after it opens).
  const { refs, floatingStyles } = useFloating({
    strategy: 'fixed',
    placement: 'bottom-start',
    middleware: [offset(2), flip({ padding: 8 }), shift({ padding: 8 })],
    whileElementsMounted: autoUpdate,
  })
  useEffect(() => {
    if (!menu) return
    const { x, y } = menu
    refs.setReference({
      getBoundingClientRect: () => ({ x, y, left: x, top: y, right: x, bottom: y, width: 0, height: 0 }),
    })
  }, [menu, refs])
  useEffect(() => {
    setDetail(null)
    setSub(null)
    if (menu) void api().itemInfoDetail(menu.id).then(setDetail)
  }, [menu])
  useEffect(() => {
    if (!menu) return
    const close = () => setMenu(null)
    const onKey = (e: KeyboardEvent) => e.key === 'Escape' && close()
    window.addEventListener('mousedown', close)
    window.addEventListener('keydown', onKey)
    window.addEventListener('blur', close)
    return () => {
      window.removeEventListener('mousedown', close)
      window.removeEventListener('keydown', onKey)
      window.removeEventListener('blur', close)
    }
  }, [menu, setMenu])

  const picker = picking && <LinkPicker {...picking} overview={overview} recent={recent} actOn={actOn} onClose={() => setPicking(null)} />
  if (!menu) return picker || null
  const id = menu.id
  const info = detail?.info.id === id ? detail.info : null
  const done = (fn: () => unknown) => () => {
    setMenu(null)
    fn()
  }
  // Opened as any view is (useWorkflows' open): on top of the stack, clearing what Back took off.
  const push = (view: View) => {
    if (!sameView(views[views.length - 1], view)) setViews([...views, view].slice(-HISTORY_LIMIT))
    setForwards([])
  }
  // A view picked here is kept as its display, as the header's menu keeps it.
  const openAs = (display: Display) => {
    if (info && display !== info.params.display) void actOn(id)(() => api().setValue(id, 'display', display))
    push({ kind: 'event', id })
  }
  const copy = (text: string | null | Promise<string | null>) =>
    void Promise.resolve(text).then((t) => {
      if (t !== null) void navigator.clipboard.writeText(t)
    })
  const top = views[views.length - 1]
  // The item on screen, which anything else is likeliest to be pinned to.
  const focused = top && (top.kind === 'event' || top.kind === 'debug') && top.id !== id ? top.id : null
  const title = (other: string) => overview.items.find((i) => i.id === other)?.title ?? detail?.pinnedIn.find((p) => p.id === other)?.summary.split('\n')[0] ?? other
  const who = (other: string) => {
    const n = nameOf(overview, other)
    return n === 'you' ? 'You' : n
  }
  const assignees = detail?.item?.item.assignees ?? []
  const owner = info?.owner ?? null
  const pinnedIn = info?.pinnedIn ?? []
  // Suggested: for pins, the item on screen, then what was viewed lately; for people, you, then whoever things went to lately, then the bots.
  const people = [...new Set([overview.me.id, ...overview.recentAssignees, ...overview.bots.map((b) => b.id)])]
  const links: { kind: LinkKind; label: string; suggest: string[]; has: string[]; name: (id: string) => string; set: (other: string, on: boolean) => Promise<unknown> }[] = [
    {
      kind: 'pin',
      label: 'Pin to',
      suggest: [...new Set([...(focused ? [focused] : []), ...recent.viewed])].filter((o) => o !== id && !pinnedIn.includes(o) && overview.items.some((i) => i.id === o && !i.closed)),
      has: pinnedIn,
      name: title,
      set: (other, on) => api().pin(other, [id], on),
    },
    {
      kind: 'assign',
      label: 'Assign to',
      suggest: info?.closed ? [] : people.filter((p) => !assignees.includes(p)),
      has: assignees,
      name: who,
      set: (other, on) => api().assign(id, other, on),
    },
    {
      kind: 'owner',
      label: 'Set owner',
      suggest: people.filter((p) => p !== owner),
      has: owner ? [owner] : [],
      name: who,
      set: (other, on) => api().setOwner(id, on ? other : null),
    },
  ]
  // A row with a submenu, shown while the mouse is over it; pressed, it does the row's own thing.
  const row = (key: string, label: ReactNode, onClick: () => void, children: ReactNode, tip?: string) => (
    <div key={key} className="menu-row" onMouseEnter={() => setSub(key)}>
      <button className="menu-parent" title={tip} onClick={onClick}>
        {label}
        <ChevronRight className="icon" />
      </button>
      {sub === key && <div className={`menu menu-sub ${menu.x > window.innerWidth - 480 ? 'leftward' : ''}`}>{children}</div>}
    </div>
  )
  return (
    <div className="menu item-menu" ref={refs.setFloating} style={floatingStyles} onMouseDown={(e) => e.stopPropagation()}>
      {row(
        'open',
        'Open',
        done(() => openAs('focus')),
        <>
          {DISPLAYS.map((d) => (
            <button key={d.id} onClick={done(() => openAs(d.id as Display))}>
              {d.label}
            </button>
          ))}
          <div className="menu-separator" />
          <button onClick={done(() => push({ kind: 'debug', id }))}>Debug</button>
          <button onClick={done(() => push({ kind: 'log', id }))}>Edit log</button>
        </>,
        'Open its focus; hover for its other views',
      )}
      {/* A topic on screen is renamed in its header. */}
      {info?.type === 'topic' && sameView(top, { kind: 'event', id }) && (
        <button onMouseEnter={() => setSub(null)} onClick={done(() => setRename(id))}>
          Rename
        </button>
      )}
      {row(
        'copy',
        'Copy',
        done(() => copy(itemText(id, 'card', overview))),
        <>
          <button onClick={done(() => copy(itemText(id, 'pill', overview)))}>Pill text</button>
          <button onClick={done(() => copy(itemText(id, 'card', overview)))}>Card text</button>
          <button onClick={done(() => copy(itemText(id, 'focus', overview)))}>Focus text</button>
          <button onClick={done(() => copy(id))}>ID</button>
        </>,
        'Copy it as its card reads; hover for its pill, focus or id',
      )}
      <div className="menu-separator" />
      {links.map((l) =>
        row(
          l.kind,
          l.label,
          done(() => setPicking({ kind: l.kind, id })),
          <>
            {l.suggest.slice(0, SUGGESTED).map((o) => (
              <button key={o} onClick={done(() => void actOn(id)(() => l.set(o, true)))}>
                <span className="menu-target">{l.name(o)}</span>
              </button>
            ))}
            <button onClick={done(() => setPicking({ kind: l.kind, id }))}>Choose…</button>
            {l.has.length > 0 && (
              <>
                <div className="menu-separator" />
                <div className="menu-note">Unlink</div>
                {l.has.map((o) => (
                  <button key={o} className="menu-action" title={`Take ${l.name(o)} off`} onClick={done(() => void actOn(id)(() => l.set(o, false)))}>
                    <X className="icon" />
                    <span className="menu-target">{l.name(o)}</span>
                  </button>
                ))}
              </>
            )}
          </>,
        ),
      )}
      {info && (
        <>
          <div className="menu-separator" />
          {itemActions(info, { act: actOn(id) }).map((a) => (
            <button key={a.id} className="menu-action" title={a.title} onMouseEnter={() => setSub(null)} onClick={done(a.run)}>
              <a.icon className={`icon ${a.tone}`} />
              {a.label}
            </button>
          ))}
        </>
      )}
    </div>
  )
}

/**
 * Choose…, for one of the menu's links: the picker as a multiselect over what it links to (items to pin it to; people
 * and bots to assign it to), ticking what it has now; picking a row links it, or takes it off. An owner is one: picking
 * someone gives it to them.
 */
function LinkPicker({ kind, id, overview, recent, actOn, onClose }: { kind: LinkKind; id: string; overview: Overview; recent: Recent; actOn: (id: string) => Act; onClose: () => void }) {
  const [value, setValue] = useState<string[] | null>(null)
  useEffect(() => {
    void api().itemInfoDetail(id).then((d) =>
      setValue(!d ? [] : kind === 'pin' ? d.info.pinnedIn : kind === 'assign' ? (d.item?.item.assignees ?? []) : d.info.owner ? [d.info.owner] : []),
    )
  }, [id, kind])
  if (!value) return null
  const people = everyone(overview).map((p) => ({ ...p, name: p.id === overview.me.id ? `${p.name} (you)` : p.name }))
  const act = actOn(id)
  if (kind === 'owner')
    return (
      <ItemPicker
        title="Set owner"
        items={[]}
        sections={[]}
        recent={recent}
        verb="Give"
        bots={people}
        onPick={(to) => {
          onClose()
          void act(() => api().setOwner(id, to))
        }}
        onClose={onClose}
      />
    )
  return (
    <ItemPicker
      title={kind === 'pin' ? 'Pin it to' : 'Assign to'}
      items={kind === 'pin' ? overview.items.filter((i) => i.id !== id) : []}
      sections={kind === 'pin' ? undefined : []}
      recent={recent}
      verb={kind === 'pin' ? 'Pin' : 'Assign'}
      bots={kind === 'pin' ? [] : people}
      multiple
      value={value}
      setValue={(next) => {
        const on = next.filter((o) => !value.includes(o))
        const off = value.filter((o) => !next.includes(o))
        setValue(next)
        void act(async () => {
          for (const o of on) await (kind === 'pin' ? api().pin(o, [id], true) : api().assign(id, o, true))
          for (const o of off) await (kind === 'pin' ? api().pin(o, [id], false) : api().assign(id, o, false))
        })
      }}
      onClose={onClose}
    />
  )
}
