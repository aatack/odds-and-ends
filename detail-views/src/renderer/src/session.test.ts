import assert from 'node:assert/strict'
import { test } from 'node:test'
import type { AppEvent } from '../../core/graph/events.ts'
import { link, values } from '../../core/graph/events.ts'
import { memoryCore } from '../../core/testing.ts'
import type { Api } from './api.ts'
import { memoryEnvironment } from './environment.ts'
import { Session } from './session.ts'
import { actionsNow, runTool, tools } from './tools.ts'

/** The app with no screen: a Session over a Core, through the same Api the window uses. */
async function headless(options: Parameters<typeof memoryCore>[0] = {}) {
  const core = memoryCore(options)
  const a = core.actions
  const api: Api = {
    scan: async (ids) => a.scan({ ids }),
    load: (request) => a.load(request),
    submit: (id, text) => a.submit({ id, text }),
    perform: (id, action, text) => a.perform({ id, action, text }),
    toggle: async (id) => a.toggle({ id }),
    markRead: (path) => a.markRead({ path }),
    context: async (first, second) => a.context({ first, second }),
    pauseSlack: (paused) => a.pauseSlack({ paused }),
    clearCache: async () => a.clearCache(),
    older: (id) => a.older({ id }),
    unlink: async (parent, child) => a.unlink({ parent, child }),
    link: async (parent, child) => a.link({ parent, child }),
    move: async (child, from, to) => a.move({ child, from, to }),
    create: async (parent, text, values) => a.create({ parent, text, values }),
    setValue: async (id, key, value) => a.setValue({ id, key, value }),
    setText: async (id, text) => a.setText({ id, text }),
    split: async (id) => a.split({ id }),
    undo: async () => a.undo(),
    redo: async (events) => a.redo({ events }),
    claudeCreate: (input) => a.claudeCreate(input),
    visit: async (id) => a.visit({ id }),
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

  // a: up to the parent row.
  session.selectParent()
  assert.equal(session.selected()?.entity.data.text, 'groceries')
  select('milk')

  // The phone's buttons are the registry's actions as they stand: on a note, the same as the keys.
  const labels = actionsNow(session).map((action) => action.label)
  for (const label of ['Back', 'Open', 'Parent', 'Note', 'Edit', 'Remove', 'Move', 'Link to…', 'Undo', 'Find']) assert.ok(labels.includes(label), label)
  assert.ok(!labels.includes('Tick'))
  runTool(session, 'select.parent')
  assert.equal(session.selected()?.entity.data.text, 'groceries')
  select('milk')

  // 3: the selected entity as JSON, from the cache; Escape (or 3) closes it.
  session.inspect()
  assert.equal(JSON.parse(session.inspected()!.json).values.text, 'milk')
  session.closeInspector()
  assert.equal(session.get().state.inspecting, null)

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
  // Entering a module starts the stack again at its root.
  session.open()
  session.enterModule('tasks')
  assert.deepEqual([session.get().state.trail, session.get().state.at], [['tasks'], 0])

  // Ctrl+F: the tree keeps rows that say it, and the rows above them.
  session.openFind()
  // Ctrl+F asks for the keyboard once; the field spends the request.
  assert.ok(session.get().findFocus > 0)
  session.findFocused()
  assert.equal(session.get().findFocus, 0)
  session.setFind('eg')
  assert.deepEqual(screen().map((line) => line.replace('>', '')), ['Tasks', '  breakfast', '    eggs'])
  // Leaving and coming back to a view with a find keeps it, without asking for the keyboard.
  select('breakfast')
  session.open()
  session.back()
  assert.equal(session.get().state.finds.tasks, 'eg')
  assert.equal(session.get().findFocus, 0)
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

test('a view walks a page at first and doubles as it nears its end; a new query starts again', async () => {
  const { core, session, stop, settle } = await headless()
  for (let i = 0; i < 450; i++) core.actions.create({ parent: 'tasks', text: `note ${i}` })
  session.navigate('tasks')
  await settle()
  const rows = () => session.get().view!.rows.length
  assert.equal(rows(), 200)
  assert.equal(session.get().view!.complete, false)
  session.loadMore()
  assert.equal(rows(), 400)
  session.loadMore()
  assert.equal(rows(), 451)
  assert.equal(session.get().view!.complete, true)
  // A find is a new query: its budget starts at a page again. Clearing it is
  // the first query again, with the budget it had.
  session.openFind()
  session.setFind('note 1')
  assert.equal(session.get().view!.rows.length < 200, true)
  session.clearFind()
  assert.equal(rows(), 451)
  stop()
})

test('chat keeps its newest end when its walk is cut short', async () => {
  const { core, session, stop, settle } = await headless()
  const { link, values } = await import('../../core/graph/events.ts')
  const events: AppEvent[] = [...values('slack:conv:C1', { type: 'slack.conversation', channel: 'C1', kind: 'channel', name: 'busy' }, 0, 'slack')]
  for (let i = 0; i < 300; i++) {
    const ts = `${1700000000 + i}.000000`
    events.push(...values(`slack:msg:C1:${ts}`, { type: 'slack.message', channel: 'C1', ts, text: `m${i}` }, (1700000000 + i) * 1000, 'slack'))
    events.push(link('slack:conv:C1', `slack:msg:C1:${ts}`, (1700000000 + i) * 1000, 'slack'))
  }
  core.cache.write(events)
  session.navigate('slack:conv:C1')
  await settle()
  const texts = session.get().view!.rows.slice(1).map((row) => row.entity.data.text)
  assert.equal(texts.length, 199)
  assert.equal(texts.at(-1), 'm299')
  assert.equal(session.selected()?.entity.data.text, 'm299')
  session.loadMore()
  assert.equal(session.get().view!.rows.slice(1)[0].entity.data.text, 'm0')
  stop()
})

test('Shift+K starts a Claude session and pushes its chat; the box under it prompts it, each prompt and answer in order', async () => {
  const run = async (command: string, args: string[]) => (command.endsWith('claude') ? JSON.stringify({ result: `you said ${args[1]}` }) : '')
  const { session, stop, settle, screen } = await headless({ run })
  session.navigate('tasks')
  await settle()
  session.openClaudeDialog()
  session.setDialog({ name: 'helper', cwd: '' })
  session.submitDialog()
  await settle()
  const root = session.root()!
  assert.equal(root.type, 'claude.session')
  assert.equal(session.get().view!.compose, 'claude')
  session.setDraft('hi')
  await session.send()
  await settle()
  assert.equal(session.get().state.drafts[root.id], undefined)
  const lines = screen().map((line) => line.replace('>', ''))
  assert.deepEqual(lines.slice(1), ['  hi', '  you said hi'], lines.join('\n'))
  // The cursor starts at the newest end, as in chat.
  assert.equal(session.selected()?.entity.data.text, 'you said hi')
  stop()
})

test('Ctrl+Enter opens what is in context with the path in the side panel; its box prompts it, and its close closes only it', async () => {
  const sent: string[] = []
  const run = async (command: string, args: string[]) => {
    if (!command.endsWith('claude')) return ''
    sent.push(args[1])
    return JSON.stringify({ result: `you said ${args[1].split('\n').at(-1)}` })
  }
  const { core, session, stop, settle } = await headless({ run })
  core.owned.write([
    ...values('n', { text: 'parent', context: 'ctx' }, 1, 'me'),
    ...values('x', { text: 'child' }, 1, 'me'),
    ...values('ctx', { type: 'context' }, 1, 'me'),
    ...values('s', { type: 'claude.session', text: 'helper', cwd: '/tmp', context: 'ctx' }, 1, 'me'),
    link('tasks', 'n', 1, 'me'),
    link('n', 'x', 1, 'me'),
    link('ctx', 'n', 1, 'me'),
    link('ctx', 's', 2, 'me'),
  ])
  session.navigate('tasks')
  await settle()
  session.navigate('n')
  await settle()
  session.select(['n', 'x'])
  // Up from the child to its parent, whose context holds the session.
  runTool(session, 'side.open')
  await settle()
  assert.equal(session.get().state.side, 's')
  assert.equal(session.get().sideView?.compose, 'claude')
  assert.equal(session.get().state.sideComposing, true)
  // Opening it asks the view it squeezes to bring the selected row to the middle.
  assert.ok(session.get().centre > 0)
  session.resizeSide(500.4)
  assert.equal(session.get().state.sideWidth, 500)
  session.setSideDraft('hi')
  await session.sendSide()
  await settle()
  assert.deepEqual(core.entity('s').outboundLinks.map((id) => core.item(id)!.data.text), ['hi', 'you said hi'])
  // The first prompt tells Claude what else is in the session's context; the prompt item is only what I wrote.
  assert.deepEqual(sent, ['[Session context: n]\n\nhi'])
  // `k` puts the selected item in the session's next prompt, as a reference after what is there.
  runTool(session, 'session.send')
  session.setSideDraft(`${session.get().state.drafts.s} and`)
  runTool(session, 'session.send')
  assert.equal(session.get().state.drafts.s, '[](entity:x) and [](entity:x)')
  session.setSideDraft('')
  assert.equal(session.get().state.drafts.s, undefined)
  // Ctrl+Enter in its box closes it again.
  const ctrlEnter = tools.find((tool) => tool.scope === 'input' && tool.keys.includes('Ctrl+Enter') && (tool.enabled?.(session, 'side') ?? true))!
  ctrlEnter.run(session, 'side')
  assert.equal(session.get().state.side, null)
  runTool(session, 'side.open')
  assert.equal(session.get().state.side, 's')
  // Each window closes its own way: the side panel closes, the view stays.
  session.closeSide()
  assert.equal(session.get().state.side, null)
  assert.equal(session.root()?.id, 'n')
  // Nothing in context: nothing opens.
  session.enterModule('tasks')
  session.select(['tasks'])
  runTool(session, 'side.open')
  assert.equal(session.get().state.side, null)
  stop()
})

test('wiping the cache takes three presses, wipes only the cache, and can be called off', async () => {
  const { core, session, stop, settle } = await headless()
  core.cache.write(values('slack:conv:C1', { type: 'slack.conversation', channel: 'C1', kind: 'channel' }, 0, 'slack'))
  core.owned.write(values('n1', { text: 'mine' }, 1, 'me'))
  const label = () => actionsNow(session).find((action) => action.id === 'cache.wipe')!.label
  runTool(session, 'cache.wipe')
  runTool(session, 'cache.wipeCancel')
  assert.equal(session.get().state.wiping, 0)
  runTool(session, 'cache.wipe')
  runTool(session, 'cache.wipe')
  assert.match(label(), /Really/)
  assert.equal(core.item('slack:conv:C1')!.data.channel, 'C1')
  runTool(session, 'cache.wipe')
  await settle()
  assert.equal(session.get().state.wiping, 0)
  assert.equal(core.item('slack:conv:C1')!.data.channel, undefined)
  assert.equal(core.item('n1')!.data.text, 'mine')
  stop()
})
