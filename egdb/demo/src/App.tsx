import { useVirtualizer, type Virtualizer } from '@tanstack/react-virtual'
import { f, op, pathKey, type Follow, type Path, type TreeRow } from 'egdb-client'
import { useFind, useHistory, useWalk, useWrite } from 'egdb-client/react'
import { useRef, useState } from 'react'

const ROW = 26
const FOLLOW: Follow = { list: [['child', 'outbound', 'list'], ['child', 'outbound', 'task']] }
const ROOT: Path = ['root']

export function App() {
  const [text, setText] = useState('')
  const writer = useWrite()
  const history = useHistory()
  return (
    <div style={{ display: 'flex', height: '100vh', font: '14px system-ui', color: '#292929' }}>
      <div style={{ flex: 2, display: 'flex', flexDirection: 'column', borderRight: '1px solid #eee' }}>
        <div style={{ display: 'flex', gap: 6, margin: 8 }}>
          <input
            placeholder="Show rows whose text contains…"
            value={text}
            onChange={(e) => setText(e.target.value)}
            style={{ flex: 1, padding: 6 }}
          />
          <button disabled={history.undo === 0} onClick={() => void writer.undo()}>Undo</button>
          <button disabled={history.redo === 0} onClick={() => void writer.redo()}>Redo</button>
        </div>
        <Outline text={text} />
      </div>
      <div style={{ flex: 1, display: 'flex', flexDirection: 'column' }}>
        <Tasks text={text} />
      </div>
    </div>
  )
}

function Outline({ text }: { text: string }) {
  const [collapsed, setCollapsed] = useState<Path[]>([])
  const scroller = useRef<HTMLDivElement>(null)
  const virtualizer = useRef<Virtualizer<HTMLDivElement, Element> | null>(null)
  const [range, setRange] = useState({ start: 0, end: 40 })
  const walk = useWalk({
    root: ROOT,
    follow: FOLLOW,
    collapsed,
    include: text ? f.containsIgnoreCase('text', text) : undefined,
    range,
    // Rows appeared or went above the anchor: scroll with them so nothing moves.
    onShift: (shift) => {
      const v = virtualizer.current
      if (v) v.scrollToOffset((v.scrollOffset ?? 0) + shift * ROW)
    },
  })
  virtualizer.current = useVirtualizer({
    count: walk.count,
    getScrollElement: () => scroller.current,
    estimateSize: () => ROW,
    overscan: 10,
    onChange: (v) => {
      const r = v.range
      if (!r) return
      setRange((p) => (p.start === r.startIndex && p.end === r.endIndex + 1 ? p : { start: r.startIndex, end: r.endIndex + 1 }))
    },
  })
  const v = virtualizer.current
  const toggle = (path: Path) => {
    const key = pathKey(path)
    setCollapsed((c) => (c.some((p) => pathKey(p) === key) ? c.filter((p) => pathKey(p) !== key) : [...c, path]))
  }

  return (
    <>
      {/* The hook keeps rows still itself; the browser's scroll anchoring would fight it. */}
      <div ref={scroller} style={{ flex: 1, overflow: 'auto', overflowAnchor: 'none' }}>
        <div style={{ height: v.getTotalSize(), position: 'relative' }}>
          {v.getVirtualItems().map((item) => (
            <div key={item.index} style={{ position: 'absolute', top: item.start, height: ROW, left: 0, right: 0 }}>
              <Row row={walk.rowAt(item.index)} onToggle={toggle} expand={(p) => setCollapsed((c) => c.filter((x) => pathKey(x) !== pathKey(p)))} />
            </div>
          ))}
        </div>
      </div>
      <div style={{ padding: '4px 8px', fontSize: 12, color: '#888', borderTop: '1px solid #eee' }}>
        rows {range.start}–{range.end} of {walk.atEnd ? walk.count : `${walk.count}+`} · anchor row {walk.anchorIndex}:{' '}
        {walk.anchor.slice(1).join(' / ') || 'root'} · computed {walk.rows.length}
        {walk.loading ? ' · loading' : ''}
      </div>
    </>
  )
}

function Row({ row, onToggle, expand }: { row: TreeRow | undefined; onToggle: (p: Path) => void; expand: (p: Path) => void }) {
  const writer = useWrite()
  const [editing, setEditing] = useState(false)
  if (!row) return <div style={{ padding: '4px 8px', color: '#ccc' }}>…</div>
  const text = typeof row.values?.text === 'string' ? row.values.text : ''
  const addChild = () => {
    const id = crypto.randomUUID()
    void writer.write([
      op.set(row.id, 'type', 'list'),
      op.set(id, 'type', 'task'),
      op.set(id, 'text', 'New item'),
      // A negative key sorts before the existing children, so the new row shows right under this one.
      op.link(row.id, id, 'child', { explicit: -Date.now() }),
    ])
    expand(row.path)
  }
  return (
    <div style={{ display: 'flex', alignItems: 'center', gap: 6, height: ROW, paddingLeft: 8 + row.depth * 18 }} className="row">
      <span style={{ width: 14, color: '#999', userSelect: 'none' }} onClick={() => row.hasChildren && onToggle(row.path)}>
        {row.hasChildren ? (row.collapsed ? '▸' : '▾') : row.childrenLoading ? '·' : ''}
      </span>
      {row.status === 'loading' ? (
        <span style={{ color: '#bbb' }}>loading…</span>
      ) : editing ? (
        <input
          autoFocus
          defaultValue={text}
          onBlur={(e) => {
            setEditing(false)
            if (e.target.value !== text) void writer.set(row.id, 'text', e.target.value)
          }}
          onKeyDown={(e) => e.key === 'Enter' && (e.target as HTMLInputElement).blur()}
        />
      ) : (
        <span onDoubleClick={() => setEditing(true)} style={{ color: row.values?.type === 'task' ? '#292929' : '#4b4b9b' }}>
          {text || <i style={{ color: '#bbb' }}>untitled</i>}
        </span>
      )}
      {row.status === 'ready' && (
        <span style={{ marginLeft: 'auto', marginRight: 8, color: '#aaa', fontSize: 12 }}>
          <span onClick={addChild} title="Add a child">＋</span>{' '}
          {row.depth > 0 && <span onClick={() => void writer.archive(row.id)} title="Archive">×</span>}
        </span>
      )}
    </div>
  )
}

function Tasks({ text }: { text: string }) {
  const [limit, setLimit] = useState(50)
  const filter = text ? f.and(f.eq('type', 'task'), f.containsIgnoreCase('text', text)) : f.eq('type', 'task')
  const find = useFind({ filter, limit, pageSize: 100 })
  return (
    <>
      <div style={{ padding: 8, color: '#888' }}>
        Tasks{text ? ` containing “${text}”` : ''}: {find.done ? find.total : `${find.total}+`}
      </div>
      <div style={{ flex: 1, overflow: 'auto', padding: '0 8px' }}>
        {find.rows.map((row) => (
          <div key={row.id} style={{ height: ROW }}>
            {row.status === 'loading' ? '…' : String(row.values?.text ?? '')}
          </div>
        ))}
        {!find.done && (
          <button onClick={() => setLimit((l) => l + 50)} disabled={find.loading} style={{ margin: 8 }}>
            More
          </button>
        )}
      </div>
    </>
  )
}
