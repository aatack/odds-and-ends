import assert from 'node:assert/strict'
import { test } from 'node:test'
import { memoryCore } from '../../core/testing.ts'
import type { Api } from './api.ts'
import { memoryEnvironment } from './environment.ts'
import { Session } from './session.ts'

/** The app with no screen: a Session over a Core, through the same Api the window uses. */
async function headless() {
  const core = memoryCore()
  const a = core.actions
  const api: Api = {
    scan: async (ids) => a.scan({ ids }),
    load: (request) => a.load(request),
    submit: (id, text) => a.submit({ id, text }),
    perform: (id, action, text) => a.perform({ id, action, text }),
    toggle: async (id) => a.toggle({ id }),
    markRead: (id) => a.markRead({ id }),
    older: (id) => a.older({ id }),
    unlink: async (parent, child) => a.unlink({ parent, child }),
    link: async (parent, child) => a.link({ parent, child }),
    move: async (child, from, to) => a.move({ child, from, to }),
    create: async (parent, text, values) => a.create({ parent, text, values }),
    setValue: async (id, key, value) => a.setValue({ id, key, value }),
    setText: async (id, text) => a.setText({ id, text }),
    undo: async () => a.undo(),
    redo: async (events) => a.redo({ events }),
    onChange: (listener) => core.onChange(listener),
    openExternal: () => {},
  }
  const session = new Session(api, memoryEnvironment())
  const stop = await session.start()
  const settle = async () => {
    for (let i = 0; i < 3; i++) {
      await session.cache.idle()
      await new Promise((resolve) => setTimeout(resolve, 170))
    }
  }
  /** The tree on screen, as depth and text, with the selected row marked. */
  const screen = () =>
    session.get().shown.rows.map((row) =>
      row.kind === 'input' ? `${'  '.repeat(row.depth)}[new]` : `${'  '.repeat(row.row.depth)}${row.selected ? '>' : ''}${String(row.row.entity.data.text ?? '')}`,
    )
  const select = (text: string) => {
    const row = session.get().shown.rows.find((one) => one.kind === 'entity' && one.row.entity.data.text === text)
    assert.ok(row && row.kind === 'entity', `no row ${text}`)
    session.select(row.row.path)
  }
  const type = async (text: string) => {
    session.setEditDraft(text)
    session.commitEdit()
    await settle()
  }
  return { core, session, stop, settle, screen, select, type }
}

test('a view is a tree to navigate and edit, from the keyboard, with nothing on screen', async () => {
  const { session, stop, settle, screen, select, type } = await headless()
  session.navigate('tasks')
  await settle()
  assert.deepEqual(screen(), ['>Tasks'])

  // Enter: a note under the selection, typed in place, then selected.
  session.startCreate()
  assert.deepEqual(screen(), ['>Tasks', '  [new]'])
  await type('groceries')
  assert.deepEqual(screen(), ['Tasks', '  >groceries'])
  session.startCreate()
  await type('milk')
  select('groceries')
  session.startCreate()
  await type('eggs')
  assert.deepEqual(screen(), ['Tasks', '  groceries', '    milk', '    >eggs'])

  // w and s move through rows; moving the cursor walks nothing again.
  const walked = session.get().view
  const before = session.get().shown.rows
  session.move(-1)
  // Only the two rows the cursor left and reached are new objects; the rest redraw nothing.
  const after = session.get().shown.rows
  assert.equal(after.filter((row, i) => row !== before[i]).length, 2)
  assert.deepEqual(screen(), ['Tasks', '  groceries', '    >milk', '    eggs'])
  assert.equal(session.get().view, walked)

  // e: edit the text in place.
  session.startEdit()
  await type('oat milk')
  assert.deepEqual(screen(), ['Tasks', '  groceries', '    >oat milk', '    eggs'])

  // Folding: ArrowLeft shuts a row, and the selection inside it comes back when it opens.
  select('groceries')
  session.fold(false)
  assert.deepEqual(screen(), ['Tasks', '  >groceries'])
  session.fold(true)

  // x: move eggs under a new note; press x on the target to finish.
  select('Tasks')
  session.startCreate()
  await type('breakfast')
  select('eggs')
  session.pick('move')
  assert.equal(session.get().state.picking?.tool, 'move')
  select('breakfast')
  session.pick('move')
  await settle()
  assert.deepEqual(screen(), ['Tasks', '  groceries', '    oat milk', '  breakfast', '    >eggs'].map((line) => line.replace('>', '')).map((line, i) => (i === 3 ? '  >breakfast' : line)))

  // r: link oat milk under breakfast too: it shows in both places.
  select('breakfast')
  session.pick('link')
  select('oat milk')
  session.pick('link')
  await settle()
  assert.deepEqual(
    screen().map((line) => line.replace('>', '')),
    ['Tasks', '  groceries', '    oat milk', '  breakfast', '    eggs', '    oat milk'],
  )

  // Backspace: out of this parent only; the cursor goes to the row above.
  const second = session.get().shown.rows.filter((row) => row.kind === 'entity' && row.row.entity.data.text === 'oat milk')[1]
  assert.ok(second.kind === 'entity')
  session.select(second.row.path)
  session.unlinkSelected()
  await settle()
  assert.deepEqual(screen(), ['Tasks', '  groceries', '    oat milk', '  breakfast', '    >eggs'])

  // d pushes a view of the selection; Shift+A pops it.
  select('breakfast')
  session.open()
  await settle()
  assert.deepEqual(screen(), ['breakfast', '  >eggs'])
  session.back()
  assert.equal(session.get().view?.root?.data.text, 'Tasks')

  // Ctrl+F: the tree keeps rows that say it, and the rows above them.
  session.openFind()
  session.setFind('eg')
  assert.deepEqual(screen().map((line) => line.replace('>', '')), ['Tasks', '  breakfast', '    eggs'])
  session.clearFind()

  // ? makes a checkbox note, Space ticks it; / makes a heading.
  select('Tasks')
  session.startCreate({ open: true })
  await type('call the bank')
  assert.equal(session.selected()?.entity.data.open, true)
  session.toggle()
  await settle()
  assert.equal(session.selected()?.entity.data.open, false)
  select('Tasks')
  session.startCreate({ section: true })
  await type('Later')
  assert.equal(session.selected()?.entity.data.section, true)

  // Ctrl+Z takes my last action back off the store; Ctrl+Y puts it back as it was.
  select('Tasks')
  session.startCreate()
  await type('mistake')
  assert.ok(screen().some((line) => line.includes('mistake')))
  await session.undo()
  await settle()
  assert.ok(!screen().some((line) => line.includes('mistake')))
  await session.redo()
  await settle()
  assert.ok(screen().some((line) => line.includes('mistake')))
  // Any other write clears what could be redone.
  await session.undo()
  assert.equal(session.get().state.undone.length, 1)
  select('Tasks')
  session.startCreate()
  await type('something else')
  assert.equal(session.get().state.undone.length, 0)
  stop()
})
