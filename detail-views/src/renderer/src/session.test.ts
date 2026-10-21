import assert from 'node:assert/strict'
import { test } from 'node:test'
import { mkdtempSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import type { AppEvent } from '../../core/graph/events.ts'
import { link, values } from '../../core/graph/events.ts'
import { fakeGh, memoryCore } from '../../core/testing.ts'
import type { Api } from './api.ts'
import { memoryEnvironment } from './environment.ts'
import { Session } from './session.ts'
import { contextWindow } from './state.ts'
import { actionsNow, runTool, tools } from './tools.ts'

/** The app with no screen: a Session over a Core, through the same Api the window uses. */
async function headless(options: Parameters<typeof memoryCore>[0] = {}, ui: { topics?: boolean } = {}) {
  const core = memoryCore(options)
  const a = core.actions
  const copied: string[] = []
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
    claudeSetDirectory: (session, cwd, worktree) => a.claudeSetDirectory({ session, cwd, worktree }),
    topicCreate: (name, path, parent, display, context) => a.topicCreate({ name, ...path, parent, display, context }),
    topicAddPath: (topic, cwd, worktree) => a.topicAddPath({ topic, cwd, worktree }),
    topicDispatch: (topic) => a.topicDispatch({ topic }),
    topicPaste: (topic, text) => a.topicPaste({ topic, text }),
    topicAttach: (topic, file) => a.topicAttach({ topic, file }),
    topicResolveCheck: (topic) => a.topicResolveCheck({ topic }),
    topicResolve: (topic, force) => a.topicResolve({ topic, force }),
    topicCheckOut: (topic) => a.topicCheckOut({ topic }),
    topicCheckOutDefault: (topic) => a.topicCheckOutDefault({ topic }),
    topicPush: (topic) => a.topicPush({ topic }),
    topicWait: (topic, on) => a.topicWait({ topic, on }),
    topicUnwait: (topic, on) => a.topicUnwait({ topic, on }),
    topicSnooze: (topic, until) => a.topicSnooze({ topic, until }),
    topicDismiss: (topic, on, message) => a.topicDismiss({ topic, on, message }),
    topicSlackSend: (topic, on, text) => core.topicSlackSend({ topic, on, text }),
    visit: async (id) => a.visit({ id }),
    onChange: (listener) => core.onChange(listener),
    openExternal: () => {},
    copy: async (text) => void copied.push(text),
  }
  const session = new Session(api, memoryEnvironment(), ui)
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
  return { core, session, stop, settle, screen, select, type, copied, api }
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

  // Backspace: out of this parent only; the cursor goes to the next sibling, or the parent after the last.
  const second = session.get().shown.rows.filter((row) => row.kind === 'entity' && row.row.entity.data.text === 'oat milk')[1]
  assert.ok(second.kind === 'entity')
  session.select(second.row.path)
  session.unlinkSelected()
  await settle()
  assert.deepEqual(screen(), ['Tasks', '  groceries', '    oat milk', '  >breakfast', '    eggs'])

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
  assert.equal(session.get().state.prompting, true)
  // Opening it asks the view it squeezes to bring the selected row to the middle.
  assert.ok(session.get().centre > 0)
  session.resizeSide(500.4)
  assert.equal(session.get().state.sideWidth, 500)
  session.setWindowDraft('side', 'hi')
  await session.sendWindow()
  await settle()
  assert.deepEqual(core.entity('s').outboundLinks.map((id) => core.item(id)!.data.text), ['hi', 'you said hi'])
  // The first prompt tells Claude what else is in the session's context; the prompt item is only what I wrote.
  assert.deepEqual(sent, ['[Session context: n]\n\nhi'])
  // Escape in its box hands the keys back to the view; `k` puts the selected item in the session's next prompt, as a reference after what is there.
  session.leavePrompt()
  runTool(session, 'session.send')
  session.setWindowDraft('side', `${session.get().state.drafts.s}and`)
  session.leavePrompt()
  runTool(session, 'session.send')
  assert.equal(session.get().state.drafts.s, '[](entity:x) and [](entity:x) ')
  session.setWindowDraft('side', '')
  assert.equal(session.get().state.drafts.s, undefined)
  // `k` with the panel closed opens the session in context first, and focuses its box.
  session.closeSide()
  runTool(session, 'session.send')
  assert.equal(session.get().state.side, 's')
  assert.equal(session.get().state.prompting, true)
  assert.equal(session.get().state.drafts.s, '[](entity:x) ')
  session.setWindowDraft('side', '')
  // Ctrl+Enter in its box closes it again.
  const ctrlEnter = tools.find((tool) => tool.scope === 'input' && tool.keys.includes('Ctrl+Enter') && (tool.enabled?.(session, 'window') ?? true))!
  ctrlEnter.run(session, 'window')
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

test('a peek pressed in takes the keys: its own cursor, notes and prompt box, and Shift+A closes it', async () => {
  const sent: string[] = []
  const run = async (command: string, args: string[]) => {
    if (!command.endsWith('claude')) return ''
    sent.push(args[1])
    return JSON.stringify({ result: 'ok' })
  }
  const { core, session, stop, settle } = await headless({ run })
  core.owned.write([
    ...values('n', { text: 'parent' }, 1, 'me'),
    ...values('a', { text: 'first' }, 1, 'me'),
    ...values('b', { text: 'second' }, 1, 'me'),
    ...values('s', { type: 'claude.session', text: 'helper', cwd: '/tmp' }, 1, 'me'),
    link('tasks', 'n', 1, 'me'),
    link('n', 'a', 1, 'me'),
    link('n', 'b', 2, 'me'),
  ])
  session.navigate('tasks')
  await settle()
  const anchor = { x: 0, y: 0, width: 0, height: 0 }
  const peekOf = async (id: string) => {
    session.hoverPeek({ kind: 'entity', id }, anchor, null)
    await new Promise((resolve) => setTimeout(resolve, 400))
    await settle()
    return session.get().state.peeks.at(-1)!.key
  }
  const mainSelection = session.get().shown.selectedPath
  const key = await peekOf('n')
  session.focusWindow(key)
  // The cursor moves in the peek; the main view's stays where it was.
  assert.deepEqual(session.selected()?.path, ['n', 'a'])
  runTool(session, 'select.down')
  assert.deepEqual(session.selected()?.path, ['n', 'b'])
  assert.deepEqual(session.get().shown.selectedPath, mainSelection)
  // Enter adds a note under the peek's selected row.
  runTool(session, 'note.create')
  session.setEditDraft('under second')
  session.commitEdit()
  await settle()
  assert.deepEqual(core.entity('b').outboundLinks.map((id) => core.item(id)!.data.text), ['under second'])
  assert.equal(session.selected()?.entity.data.text, 'under second')
  // Leaving it for the main view (the pointer elsewhere) closes it.
  session.focusWindow(null)
  assert.equal(session.get().state.peeks.length, 0)
  // A session's peek has its prompt box; Shift+A closes the peek, not the view.
  const chat = await peekOf('s')
  session.composeWindow(chat, true)
  assert.equal(session.get().state.window, chat)
  session.setWindowDraft(chat, 'hello')
  await session.sendWindow()
  await settle()
  assert.deepEqual(sent, ['hello'])
  runTool(session, 'view.back')
  assert.equal(session.get().state.peeks.length, 0)
  assert.equal(session.get().state.window, null)
  assert.equal(session.root()?.id, 'tasks')
  stop()
})

test('Tab in a chat goes to the next chat, newest made first, in its place on the stack; Shift+Tab goes back', async () => {
  const { core, session, stop, settle } = await headless()
  core.owned.write([
    ...values('a', { type: 'claude.session', text: 'a' }, 3, 'me'),
    ...values('b', { type: 'claude.session', text: 'b', runningSince: 2 }, 2, 'me'),
    ...values('c', { type: 'claude.session', text: 'c' }, 1, 'me'),
    // c was active last: first in the list, but not in Tab's order.
    ...values('c', { lastRead: 0 }, 9, 'me'),
    link('claude', 'a', 3, 'me'),
    link('claude', 'b', 2, 'me'),
    link('claude', 'c', 1, 'me'),
  ])
  session.enterModule('claude')
  await settle()
  assert.deepEqual(session.get().view!.rows.slice(1).map((row) => row.entity.id), ['c', 'a', 'b'])
  session.navigate('a')
  await settle()
  const depth = session.get().state.trail.length
  // b too, though Claude is answering in it; round from the last to the first.
  session.nextChat(1)
  assert.equal(focusedOf(session), 'b')
  session.nextChat(1)
  assert.equal(focusedOf(session), 'c')
  session.nextChat(1)
  assert.equal(focusedOf(session), 'a')
  session.nextChat(-1)
  assert.equal(focusedOf(session), 'c')
  assert.equal(session.get().state.trail.length, depth)
  stop()
})

test('an item opened out in a chat\'s panel is a window: a press gives it the keys, and they move its own selection; Shift+A folds it', async () => {
  const { core, session, stop, settle } = await headless()
  core.owned.write([
    ...values('chat', { type: 'claude.session', text: 'chat', context: 'ctx' }, 1, 'me'),
    ...values('task', { text: 'task', context: 'ctx' }, 1, 'me'),
    ...values('ctx', { type: 'context' }, 1, 'me'),
    ...values('one', { text: 'one' }, 2, 'me'),
    ...values('two', { text: 'two' }, 3, 'me'),
    link('claude', 'chat', 1, 'me'),
    link('ctx', 'chat', 1, 'me'),
    link('ctx', 'task', 1, 'me'),
    link('task', 'one', 2, 'me'),
    link('task', 'two', 3, 'me'),
  ])
  session.enterModule('claude')
  session.navigate('chat')
  await settle()
  session.toggleChatContext('task')
  await settle()
  assert.deepEqual(session.get().contextViews.task.rows.map((row) => row.entity.id), ['task', 'one', 'two'])
  session.focusWindow(contextWindow('task'))
  session.select(['task', 'one'])
  session.move(1)
  assert.equal(session.selected()?.entity.id, 'two')
  // The chat's own selection is untouched.
  assert.equal(session.get().shown.selectedPath.includes('two'), false)
  session.back()
  assert.equal(session.get().state.chatContext.chat, undefined)
  assert.equal(session.get().state.window, null)
  assert.equal(focusedOf(session), 'chat')
  stop()
})

test('a chat\'s box switches to notes with ← and → while empty: a note goes in the chat as a row, Enter adds under it, and Tab goes back to the box', async () => {
  const { core, session, stop, settle } = await headless()
  core.owned.write([...values('chat', { type: 'claude.session', text: 'chat' }, 1, 'me'), link('claude', 'chat', 1, 'me')])
  session.enterModule('claude')
  session.navigate('chat')
  await settle()
  assert.equal(session.chatMode(), 'prompt')
  session.setDraft('typed')
  assert.equal(session.canCycleChatMode(), false)
  session.setDraft('')
  session.cycleChatMode(1)
  assert.equal(session.chatMode(), 'note')
  session.setDraft('remember this')
  await session.send()
  await settle()
  const [note] = core.entity('chat').outboundLinks
  assert.equal(core.item(note)!.type, 'note')
  assert.equal(core.item(note)!.data.text, 'remember this')
  // Its rows are the tree's: the note is selected (the newest end), Enter adds a note under it.
  assert.equal(session.selected()?.entity.id, note)
  session.startCreate()
  session.setEditDraft('more on it')
  session.commitEdit()
  await settle()
  assert.equal(core.item(core.entity(note).outboundLinks[0])!.data.text, 'more on it')
  // Tab, while the rows have the keys, hands them back to the box.
  assert.equal(session.canReturnToPrompt(), true)
  session.returnToPrompt()
  assert.equal(session.get().state.composing, true)
  assert.equal(session.canReturnToPrompt(), false)
  session.cycleChatMode(-1)
  assert.equal(session.chatMode(), 'prompt')
  stop()
})

const focusedOf = (session: { get(): { state: { trail: string[]; at: number } } }) => session.get().state.trail[session.get().state.at]

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

test('topics mode: a topic is made from the list and opened; the box adds messages; Ctrl+Enter sends them all, with what is typed', async () => {
  const sent: string[] = []
  const run = async (command: string, args: string[]) => {
    if (!command.endsWith('claude')) throw new Error(`unexpected ${command}`)
    sent.push(args[1])
    return JSON.stringify({ result: 'on it', is_error: false })
  }
  const { core, session, stop, settle, screen } = await headless({ run }, { topics: true })
  const dispatch = tools.find((tool) => tool.id === 'topic.dispatch')!
  try {
  await settle()
  assert.equal(session.get().view?.root?.type, 'topics.home')
  assert.deepEqual(session.get().topics, [])

  session.setTopicName('Billing')
  session.createTopic()
  await settle()
  const [topic] = session.get().topics
  assert.equal(topic.data.text, 'Billing')
  assert.equal(session.get().view?.root?.id, topic.id)
  // The box is closed until Enter opens it.
  assert.equal(session.get().state.composing, false)
  assert.equal(session.get().state.drafts.topics, undefined)
  assert.equal(dispatch.enabled!(session, 'topic'), false)

  session.returnToPrompt()
  session.setDraft('first')
  await session.send()
  await settle()
  // Enter closed it again once the message was in.
  assert.equal(session.get().state.composing, false)
  session.returnToPrompt()
  session.setDraft('second')
  assert.equal(dispatch.enabled!(session, 'topic'), true)
  dispatch.run(session, 'topic')
  await settle()
  assert.deepEqual(sent, ['first\n\nsecond'])
  assert.deepEqual(screen(), ['Billing', '  first', '  second', '  >on it'])
  assert.equal(session.get().state.drafts[topic.id], undefined)
  assert.equal(session.get().state.composing, false)
  assert.equal(dispatch.enabled!(session, 'topic'), false)
  assert.ok(core.item(topic.id)!.data.cwd)

  // Nothing in the chat is selectable, so Enter adds no note under anything.
  const key = (name: string, field: string | null) =>
    tools.find((tool) => tool.keys.includes(name) && (field === null ? tool.scope !== 'input' : tool.scope === 'input') && (tool.enabled?.(session, field) ?? true))?.id
  assert.equal(session.selected(), null)
  session.move(1)
  assert.equal(session.selected(), null)
  // Enter opens the box; Enter in it empty closes it. With text in it, Enter adds it.
  assert.equal(key('Enter', null), 'topic.prompt')
  session.returnToPrompt()
  assert.equal(session.get().state.composing, true)
  assert.equal(key('Enter', 'topic'), 'topic.leave')
  session.setDraft('third')
  assert.equal(key('Enter', 'topic'), 'composer.submit')
  } finally {
    stop()
  }
})

test('topics mode: Ctrl+Enter while Claude is answering sends nothing more; what is typed goes in, for the next', async () => {
  const sent: string[] = []
  let answer: (() => void) | null = null
  const run = (command: string, args: string[]) => {
    if (!command.endsWith('claude')) throw new Error(`unexpected ${command}`)
    sent.push(args[1])
    return new Promise<string>((resolve) => (answer = () => resolve(JSON.stringify({ result: 'done', is_error: false }))))
  }
  const { session, stop, settle, screen } = await headless({ run }, { topics: true })
  const dispatch = tools.find((tool) => tool.id === 'topic.dispatch')!
  try {
  await settle()
  session.setTopicName('Slow')
  session.createTopic()
  await settle()
  session.returnToPrompt()
  session.setDraft('first')
  dispatch.run(session, 'topic')
  await settle()
  assert.deepEqual(sent, ['first'])
  // Answering: nothing to send, so Ctrl+Enter is off, until something is typed.
  assert.equal(session.canDispatch(), false)
  assert.equal(dispatch.enabled!(session, null), false)
  // It says so, rather than falling through to the side panel's Ctrl+Enter.
  const ctrlEnter = tools.find((tool) => tool.scope !== 'input' && tool.keys.includes('Ctrl+Enter') && (tool.enabled?.(session, null) ?? true))!
  assert.equal(ctrlEnter.id, 'topic.dispatchRefused')
  ctrlEnter.run(session, null)
  assert.equal(session.get().toast, 'Claude is still answering')
  session.returnToPrompt()
  session.setDraft('second')
  assert.equal(dispatch.enabled!(session, 'topic'), true)
  dispatch.run(session, 'topic')
  await settle()
  assert.deepEqual(sent, ['first'])
  assert.equal(session.get().state.composing, false)
  assert.match(session.get().toast ?? '', /still answering/)
  answer!()
  await settle()
  assert.equal(dispatch.enabled!(session, null), true)
  dispatch.run(session, null)
  await settle()
  assert.deepEqual(sent, ['first', 'second'])
  assert.equal(screen().length, 5)
  answer!()
  await settle()
  } finally {
    stop()
  }
})

test('topics mode: a paste goes into the topic only while its box is closed (open, the box takes it as text)', async () => {
  const { session, stop, settle, screen } = await headless({}, { topics: true })
  try {
  await settle()
  session.setTopicName('Links')
  session.createTopic()
  await settle()
  assert.equal(session.canPaste(), true)
  await session.paste('https://github.com/me/app/tree/main')
  await settle()
  assert.equal(screen().length, 2)
  session.returnToPrompt()
  assert.equal(session.canPaste(), false)
  await session.paste('ignored')
  await settle()
  assert.equal(screen().length, 2)
  } finally {
    stop()
  }
})

test('topics mode: Ctrl+. opens a form for a new topic, from anywhere; Enter in it makes the topic and opens it', async () => {
  const { session, stop, settle } = await headless({}, { topics: true })
  const key = (name: string, field: string | null) =>
    tools.find((tool) => tool.keys.includes(name) && (field === null || tool.scope === 'input' || name.startsWith('Ctrl+')) && (tool.enabled?.(session, field) ?? true))?.id
  try {
  await settle()
  assert.equal(key('Ctrl+.', 'topic-name'), 'topic.new')
  session.openTopicDialog()
  assert.deepEqual(session.get().state.dialog, { kind: 'topic', name: '', display: 'task', context: '', path: '', worktree: false })
  // Not a second form over the first.
  assert.equal(key('Ctrl+.', null), undefined)
  // Nothing made without a name.
  session.submitDialog()
  assert.ok(session.hasDialog())
  session.setDialog({ name: 'Flights' })
  assert.equal(key('Enter', 'dialog-topic-name'), 'dialog.submit')
  session.submitDialog()
  await settle()
  assert.equal(session.hasDialog(), false)
  const [topic] = session.get().topics
  assert.equal(topic.data.text, 'Flights')
  assert.equal(session.get().view?.root?.id, topic.id)
  } finally {
    stop()
  }
})

test('a PR of mine: Approve asks nothing; its line is copied straight away, then it is approved here and set to merge', async () => {
  const calls: string[][] = []
  const { session, stop, settle, copied } = await headless({ gh: fakeGh(calls) })
  const id = 'github:pr:https://github.com/o/r/pull/7'
  try {
  session.navigate(id)
  await settle()
  await settle()
  session.startAction('approve')
  assert.equal(session.get().state.acting, null)
  await settle()
  await settle()
  assert.match(copied[0] ?? '', /^\[#7\]\(https:\/\/github\.com\/o\/r\/pull\/7\) /)
  assert.ok(calls.some((args) => args[0] === 'pr' && args[1] === 'merge' && args.includes('--auto')))
  } finally {
    stop()
  }
})

test('topics mode: a thoughts topic is a tree of notes, worked as any tree is, with nothing of Claude\'s', async () => {
  const { session, stop, settle } = await headless({}, { topics: true })
  const key = (name: string) => tools.find((tool) => tool.keys.includes(name) && tool.scope !== 'input' && (tool.enabled?.(session, null) ?? true))?.id
  try {
  await settle()
  session.openTopicDialog()
  // A path typed for a task is dropped on switching to thoughts.
  session.setDialog({ name: 'Ideas', path: '/tmp', display: 'thoughts' })
  session.submitDialog()
  await settle()
  const view = () => session.get().view!
  assert.equal(view().root?.data.display, 'thoughts')
  assert.deepEqual(view().root && session.get().item(view().root!.id)?.data.text, 'Ideas')
  assert.equal(view().rows.length, 1)
  // A tree from the top, with no box: Enter adds a note under the selected row, as anywhere.
  assert.equal(view().startsAtEnd, false)
  assert.equal(view().compose, null)
  assert.equal(key('Enter'), 'note.create')
  session.startCreate()
  session.setEditDraft('First thought')
  await session.commitEdit()
  await settle()
  assert.deepEqual(view().rows.map((row) => [row.depth, row.entity.data.text]), [[0, 'Ideas'], [1, 'First thought']])
  // Claude's things are not offered; snooze and Resolve are.
  assert.equal(session.canDispatch(), false)
  assert.equal(key('Ctrl+Enter') === 'topic.dispatch', false)
  assert.equal(session.canSnooze(), true)
  assert.equal(session.canResolveTopic(), true)
  } finally {
    stop()
  }
})

test('topics mode: a new topic\'s form starts with a context: the task open, or the thought selected; cleared, the topic stands alone', async () => {
  const { session, stop, settle } = await headless({}, { topics: true })
  const make = async (name: string, change: Record<string, unknown> = {}) => {
    session.openTopicDialog()
    session.setDialog({ name, ...change })
    session.submitDialog()
    await settle()
    return session.get().view!.root!
  }
  const contextOf = (id: string) => session.get().item(id)?.data.context
  try {
  await settle()
  // Nothing open: no context.
  session.openTopicDialog()
  assert.equal((session.get().state.dialog as { context: string }).context, '')
  session.cancelDialog()
  const task = await make('Task')
  // On a task: the task.
  session.openTopicDialog()
  assert.equal((session.get().state.dialog as { context: string }).context, task.id)
  session.cancelDialog()
  const related = await make('Related')
  assert.ok(contextOf(related.id))
  assert.equal(contextOf(related.id), contextOf(task.id))
  // Cleared: on its own.
  const alone = await make('Alone', { context: '' })
  assert.equal(contextOf(alone.id) ?? null, null)
  // On thoughts: the row selected.
  const thoughts = await make('Thoughts', { context: '', display: 'thoughts' })
  session.startCreate()
  session.setEditDraft('A thought')
  await session.commitEdit()
  await settle()
  const note = session.get().view!.rows[1].entity
  session.select([thoughts.id, note.id])
  session.openTopicDialog()
  assert.equal((session.get().state.dialog as { context: string }).context, note.id)
  session.cancelDialog()
  } finally {
    stop()
  }
})

test('topics mode: Ctrl+L finds a topic by its name, fuzzily; the clock hides topics waiting (snoozed, or on what is not back) from the list, the open one apart', async () => {
  const { core, session, stop, settle } = await headless({}, { topics: true })
  const key = (name: string, field: string | null) => tools.find((tool) => tool.keys.includes(name) && (field === null ? tool.scope !== 'input' : true) && (tool.enabled?.(session, field) ?? true))?.id
  const names = () => session.get().topics.map((topic) => topic.data.text)
  const open = () => session.get().view?.root?.data.text
  try {
  await settle()
  for (const name of ['Flights to Lisbon', 'Fix login bug', 'Quarterly report']) {
    session.setTopicName(name)
    session.createTopic()
    await settle()
  }
  assert.equal(key('Ctrl+l', null), 'topic.jump')
  session.openJump()
  // Nothing typed: every topic, in the list's order.
  assert.deepEqual(session.jumpMatches().map((topic) => topic.data.text), names())
  session.setDialog({ query: 'fl', at: 0 })
  assert.deepEqual(session.jumpMatches().map((topic) => topic.data.text), ['Flights to Lisbon', 'Fix login bug'])
  assert.equal(key('ArrowDown', 'dialog-jump'), 'jump.down')
  session.moveJump(1)
  session.moveJump(1)
  assert.equal((session.get().state.dialog as { at: number }).at, 1)
  assert.equal(key('Enter', 'dialog-jump'), 'dialog.submit')
  session.submitDialog()
  await settle()
  assert.equal(session.hasDialog(), false)
  assert.equal(open(), 'Fix login bug')
  session.openJump()
  session.setDialog({ query: 'zzz', at: 0 })
  assert.deepEqual(session.jumpMatches(), [])
  session.cancelDialog()

  // Snoozed: still listed until I hide them; then gone, but for the one open.
  session.openTopicDialog()
  session.cancelDialog()
  for (const name of ['Quarterly report', 'Fix login bug']) {
    session.openJump()
    session.setDialog({ query: name, at: 0 })
    session.submitDialog()
    await settle()
    session.openSnoozeDialog()
    session.setDialog({ length: '2d' })
    session.submitDialog()
    await settle()
  }
  // Waiting on another topic: nothing to do yet either.
  session.setTopicName('Blocked')
  session.createTopic()
  await settle()
  const blocked = session.get().view!.root!.id
  const lisbon = session.get().topics.find((topic) => topic.data.text === 'Flights to Lisbon')!.id
  await core.actions.link({ parent: blocked, child: lisbon })
  await core.actions.topicWait({ topic: blocked, on: [lisbon] })
  session.openTopic(lisbon)
  await settle()
  assert.equal(names().length, 4)
  runTool(session, 'topics.hideSnoozed')
  await settle()
  assert.equal(session.get().state.hideSnoozed, true)
  assert.deepEqual(names(), ['Flights to Lisbon'])
  runTool(session, 'topics.hideSnoozed')
  await settle()
  assert.equal(names().length, 4)
  } finally {
    stop()
  }
})

test('topics mode: `.` opens the next topic down the list where Claude isn’t working, round from the open one', async () => {
  let answer: (() => void) | null = null
  const run = (command: string) => {
    if (!command.endsWith('claude')) throw new Error(`unexpected ${command}`)
    return new Promise<string>((resolve) => (answer = () => resolve(JSON.stringify({ result: 'done', is_error: false }))))
  }
  const { session, stop, settle } = await headless({ run }, { topics: true })
  const next = tools.find((tool) => tool.id === 'topic.nextIdle')!
  const open = () => session.get().view?.root?.data.text
  try {
  await settle()
  for (const name of ['a', 'b', 'c']) {
    session.setTopicName(name)
    session.createTopic()
    await settle()
  }
  // Newest first: c, b, a; c is open. Claude works in b.
  const b = session.get().topics[1]
  session.openTopic(b.id)
  session.returnToPrompt()
  session.setDraft('go')
  await session.dispatch()
  await settle()
  assert.deepEqual(session.get().topics.map((topic) => topic.data.text), ['b', 'c', 'a'])
  assert.equal(next.enabled!(session, null), true)
  next.run(session, null)
  assert.equal(open(), 'c')
  next.run(session, null)
  assert.equal(open(), 'a')
  // Round past b, still working.
  next.run(session, null)
  assert.equal(open(), 'c')
  answer!()
  await settle()
  next.run(session, null)
  assert.equal(open(), 'a')
  next.run(session, null)
  assert.equal(open(), 'b')
  } finally {
    stop()
  }
})

test('topics mode: Resolve takes the open topic out of the list and leaves the window empty; with work to lose, a form asks first', async () => {
  const { session, stop, settle, api } = await headless({}, { topics: true })
  try {
  await settle()
  session.setTopicName('Done')
  session.createTopic()
  await settle()
  const [topic] = session.get().topics
  assert.equal(session.canResolveTopic(), true)
  session.resolveTopic()
  await settle()
  assert.deepEqual(session.get().topics, [])
  assert.equal(session.get().view?.root?.type, 'topics.home')

  // With work to lose (a check that says so): the form, then Enter resolves anyway.
  session.setTopicName('Busy')
  session.createTopic()
  await settle()
  const [busy] = session.get().topics
  const check = api.topicResolveCheck
  const forced: boolean[] = []
  const resolve = api.topicResolve
  api.topicResolveCheck = async () => ({ warnings: ['1 commit not pushed'], error: null })
  api.topicResolve = (id, force) => (forced.push(force), resolve(id, force))
  session.resolveTopic()
  await settle()
  assert.deepEqual(session.get().state.dialog, { kind: 'resolve', topic: busy.id, warnings: ['1 commit not pushed'] })
  const enter = tools.find((tool) => tool.keys.includes('Enter') && tool.scope !== 'input' && (tool.enabled?.(session, null) ?? true))!
  assert.equal(enter.id, 'confirm.submit')
  enter.run(session, null)
  await settle()
  assert.deepEqual(forced, [true])
  assert.deepEqual(session.get().topics, [])
  api.topicResolveCheck = check
  assert.ok(topic)
  } finally {
    stop()
  }
})

test('topics mode: the new-topic form takes a path; the path picker adds one typed, with Enter; each is an item in the chat', async () => {
  const dir = mkdtempSync(join(tmpdir(), 'topic-path-'))
  const other = mkdtempSync(join(tmpdir(), 'topic-path-'))
  const { session, stop, settle, screen } = await headless({}, { topics: true })
  try {
  await settle()
  session.openTopicDialog()
  session.setDialog({ name: 'With a path', path: dir, worktree: false })
  session.submitDialog()
  await settle()
  assert.equal(session.get().view?.root?.data.text, 'With a path')
  // (The tree's own selection mark aside: a topic's chat shows none.)
  const lines = () => screen().slice(1).map((line) => line.replace('>', ''))
  assert.deepEqual(lines(), [`  ${dir}`])

  const add = tools.find((tool) => tool.keys.includes('Enter') && tool.scope === 'input' && (tool.enabled?.(session, 'path') ?? true))!
  assert.equal(add.id, 'path.add')
  session.setPathDraft(other)
  add.run(session, 'path')
  await settle()
  assert.deepEqual(lines(), [`  ${dir}`, `  ${other}`])
  // The box empties once it is in.
  assert.equal(session.get().state.drafts[`${session.get().view!.root!.id}#path`] ?? '', '')
  } finally {
    stop()
  }
})

test('topics mode: Tab points the box at each Slack thread or conversation in the chat; Enter then sends there, once; a wait keeps `.` away until someone answers', async () => {
  const posted: string[] = []
  const slack = (async (input: string | URL | Request, init?: RequestInit) => {
    const method = String(input).split('/').pop()!
    const params = init!.body as URLSearchParams
    const answers: Record<string, unknown> = {
      'auth.test': { user_id: 'UME', url: 'https://x.slack.com/' },
      'users.conversations': { channels: [] },
      'conversations.list': { channels: [] },
      'users.list': { members: [] },
      'search.messages': { messages: { matches: [], paging: { pages: 1 } } },
      'chat.postMessage': { ts: '300.000000', message: { ts: '300.000000', user: 'UME', text: params.get('text'), thread_ts: params.get('thread_ts') } },
    }
    if (method === 'chat.postMessage') posted.push(`${params.get('channel')} ${params.get('thread_ts')} ${params.get('text')}`)
    return new Response(JSON.stringify({ ok: true, ...(answers[method] as object) }))
  }) as typeof fetch
  const { core, session, stop, settle } = await headless({ fetch: slack, now: () => 250_000 }, { topics: true })
  const tab = tools.find((tool) => tool.id === 'topic.target')!
  const next = tools.find((tool) => tool.id === 'topic.nextIdle')!
  try {
    await core.slack.setToken('xoxp-1')
    const message = 'slack:msg:C1:100.000000'
    core.cache.write([...values(message, { type: 'slack.message', channel: 'C1', ts: '100.000000', user: 'UOTHER', text: 'is it down?' }, 100_000, 'slack'), link('slack:conv:C1', message, 100_000, 'slack')])
    for (const name of ['other', 'outage']) {
      session.setTopicName(name)
      session.createTopic()
      await settle()
    }
    const topic = session.get().view!.root!.id
    await core.actions.topicPaste({ topic, text: 'https://x.slack.com/archives/C1/p100000000' })
    await settle()

    // A note, then the thread, then the conversation, then a note again.
    session.returnToPrompt()
    assert.equal(tab.enabled!(session, 'topic'), true)
    assert.equal(session.topicTarget(), null)
    tab.run(session, 'topic')
    assert.equal(session.topicTarget()?.on, message)
    tab.run(session, 'topic')
    assert.equal(session.topicTarget()?.on, 'slack:conv:C1')
    tab.run(session, 'topic')
    assert.equal(session.topicTarget(), null)
    tab.run(session, 'topic')

    // Ctrl+Enter never takes what is meant for Slack to Claude.
    session.setDraft('Looking into it')
    assert.match(String(session.dispatchRefusal()), /Slack/)
    await session.send()
    await settle()
    assert.deepEqual(posted, ['C1 100.000000 Looking into it'])
    assert.equal(session.topicTarget(), null)
    assert.equal(session.get().view!.root!.id, topic)
    assert.ok(core.entity(topic).outboundLinks.includes('slack:msg:C1:300.000000'))

    // Waiting on the thread: `.` passes the topic by until someone else answers in it.
    session.waitOn(message, 'thread')
    await settle()
    assert.deepEqual(session.get().waiting, [topic])
    assert.deepEqual(session.get().topicWaits.map(({ wait, candidates }) => [wait.slack, candidates]), [[message, []]])
    next.run(session, null)
    assert.equal(session.get().view?.root?.data.text, 'other')
    assert.equal(next.enabled!(session, null), false)
    core.cache.write([...values('slack:msg:C1:310.000000', { type: 'slack.message', channel: 'C1', ts: '310.000000', threadTs: '100.000000', user: 'UOTHER', text: 'fixed' }, 310_000, 'slack'), link(message, 'slack:msg:C1:310.000000', 310_000, 'slack')])
    await settle()
    assert.deepEqual(session.get().waiting, [])
    next.run(session, null)
    assert.equal(session.get().view?.root?.data.text, 'outage')
    assert.deepEqual(session.get().topicWaits.map(({ candidates }) => candidates), [['slack:msg:C1:310.000000']])
    session.acceptCandidate('slack:msg:C1:310.000000')
    await settle()
    assert.deepEqual(session.get().topicWaits.map(({ candidates }) => candidates), [[]])
    session.unwait(message)
    await settle()
    assert.deepEqual(session.get().topicWaits, [])
  } finally {
    stop()
  }
})

test('topics mode: `?` on a topic opens the new-topic form for one that goes into this one\'s chat too, then opens it', async () => {
  const { session, stop, settle, core } = await headless({}, { topics: true })
  const key = (field: string | null) => tools.find((tool) => tool.keys.includes('?') && tool.scope !== 'input' && (tool.enabled?.(session, field) ?? true))?.id
  try {
  await settle()
  // Not before a topic is open.
  assert.notEqual(key(null), 'topic.child')
  session.setTopicName('Release')
  session.createTopic()
  await settle()
  const release = session.get().view!.root!.id
  assert.equal(key(null), 'topic.child')
  session.openChildTopicDialog()
  assert.equal((session.get().state.dialog as { parent?: string }).parent, release)
  session.setDialog({ name: 'Fix the build' })
  session.submitDialog()
  await settle()
  const fix = session.get().view!.root!
  assert.equal(fix.data.text, 'Fix the build')
  assert.deepEqual(core.entity(release).outboundLinks, [fix.id])
  assert.equal(session.get().topics.length, 2)
  } finally {
    stop()
  }
})
