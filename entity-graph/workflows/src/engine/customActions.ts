// Custom actions: things the human defines, per item type, on the Types page.
// One is a script (JavaScript, run in QuickJS like any other script) handed a
// context (the item it was run on, and its item) and the safe actions. It is
// usually written by a model from a description and some notes, so it is held
// to the bots' rule: no unsafe actions; it asks for one with an approval item.

import { randomUUID } from 'node:crypto'
import { z } from 'zod'
import type { CustomAction, ItemSpec } from '../shared/types'
import type { Engine } from './engine'
import { objectSchema } from './schema'
import { runScript } from './script'
import type { CustomActionRow, Store } from './store'

// The human's own actions, per item type: configuration, like settings, so
// edited in place. Running one is recorded as an action item in the item.
const toAction = ({ created_at: _c, updated_at: _u, ...a }: CustomActionRow): CustomAction => a

/** What a custom action's function may return: nothing, a line for the chat, or that and items to add in the item. */
const result = z.union([
  z.null(),
  z.string(),
  z.object({
    message: z.string().optional(),
    items: z.array(z.object({ type: z.string(), params: z.record(z.unknown()).default({}), ref: z.string().optional() })).optional(),
    // The old name (from when items were called items): actions written before still return it.
    events: z.array(z.object({ type: z.string(), params: z.record(z.unknown()).default({}), ref: z.string().optional() })).optional(),
  }),
])

export class CustomActions {
  constructor(
    private engine: Engine,
    private store: Store,
  ) {}

  list(): CustomAction[] {
    return [...this.store.customActions.values()]
      .sort((a, b) => a.type.localeCompare(b.type) || Number(b.important) - Number(a.important) || a.created_at - b.created_at)
      .map(toAction)
  }

  get(id: string): CustomAction | null {
    const r = this.store.customActions.get(id)
    return r ? toAction(r) : null
  }

  /** Adds one (no id) or changes one. */
  save(a: Omit<CustomAction, 'id'> & { id?: string }): CustomAction {
    if (!this.engine.itemTypes.some((t) => t.type === a.type)) throw new Error(`No item type ${a.type}.`)
    if (!a.name.trim()) throw new Error('An action needs a name.')
    const now = this.engine.now()
    const id = a.id ?? randomUUID()
    if (a.id && !this.get(a.id)) throw new Error(`No custom action ${a.id}.`)
    this.store.putCustomAction({
      id, type: a.type, name: a.name.trim(), description: a.description, notes: a.notes, source: a.source, icon: a.icon || 'zap',
      important: Boolean(a.important), created_at: this.store.customActions.get(id)?.created_at ?? now, updated_at: now,
    })
    this.engine.emit('change')
    return this.get(id)!
  }

  remove(id: string): void {
    this.store.deleteCustomAction(id)
    this.engine.emit('change')
  }

  /**
   * Runs one on an item: its function gets the context (the item, its item,
   * the time) and the safe actions; what it returns is said in the item's
   * chat, as an action item, and any items it returns are added there.
   */
  async run(id: string, itemId: string, actor = 'human', runId: string | null = null): Promise<{ message: string | null }> {
    const action = this.get(id)
    if (!action) throw new Error(`No custom action ${id}.`)
    const info = this.engine.info(itemId)
    if (!info) throw new Error(`No item ${itemId}.`)
    if (info.type !== action.type) throw new Error(`${action.name} is for ${action.type} items, not ${info.type}.`)
    if (!action.source.trim()) throw new Error(`${action.name} has no code yet: write or generate it on the Types page.`)
    const taskId = info.type === 'task' ? info.id : info.parentId
    const item = taskId ? this.engine.get(taskId) : null
    if (!item) throw new Error('That item is in no item.')
    const context = {
      item: { id: item.id, title: item.title, parentId: item.parentId, closed: item.closed, withYou: item.withYou },
      event: { id: info.id, type: info.type, summary: info.summary, params: info.params, data: info.data, resolved: info.resolved, parentId: info.parentId },
      now: new Date(this.engine.now()).toISOString(),
    }
    const ran = await runScript(action.source, context, (name, args) =>
      this.engine.callAction(name, args, { engine: this.engine, author: 'bot', actor: 'script', itemId: item.id, runId }),
    )
    const parsed = result.safeParse(ran.value ?? null)
    if (!parsed.success) throw new Error(`It returned ${JSON.stringify(ran.value)?.slice(0, 200)}, not null, a string, or { message?, items? }.`)
    const out = parsed.data
    const message = typeof out === 'string' ? out : (out?.message ?? null)
    const specs = typeof out === 'object' && out ? ([...(out.items ?? []), ...(out.events ?? [])] as ItemSpec[]) : []
    // Its items are held to the bots' rules, and checked before anything is added.
    this.engine.prepare(specs, 'bot')
    // What it said is a note in the task, by the script; what it returned, a bot's (held to the bots' rules).
    if (message) await this.engine.addItems(item.id, [{ type: 'note', params: { text: message } }], 'script', 'human')
    if (specs.length) await this.engine.addItems(item.id, specs, actor, 'bot')
    return { message }
  }

  /**
   * Writes an action's code from its description and notes, with a model
   * (Sonnet, no tools): the function, told the context it gets, the actions it
   * may call and the items it may add.
   */
  async generate(a: { type: string; name: string; description: string; notes: string; source?: string }): Promise<string> {
    if (!a.description.trim() && !a.notes.trim()) throw new Error('Say what it should do first: a description, or notes.')
    const actions = this.engine.actions
      .filter((x) => !x.unsafe)
      .map((x) => `- ${x.name}(${JSON.stringify(objectSchema(x.input).properties ?? {})}): ${x.description}`)
    const types = this.engine.itemTypes.filter((t) => t.bots).map((t) => `- ${t.type} ${JSON.stringify(objectSchema(t.params).properties ?? {})}: ${t.description}`)
    const unsafe = this.engine.actions.filter((x) => x.unsafe).map((x) => `- ${x.name}: ${x.description}`)
    const system = `You write one custom action for a workflow app, as a JavaScript function expression. Reply with the code only: no prose, no markdown fences.

The function is: async (context, actions) => result
- It runs on one item of type "${a.type}". context is { item: { id, title, parentId, closed, withYou }, event: { id, type, summary, params, data, resolved, parentId }, now (ISO time) }: item is the task it is in, and event the item it runs on (the name is from when items were called events). For a task, the two are the same.
- actions.<name>(args) calls an action and resolves to its result (parsed JSON). These are the only ones it may call:
${actions.join('\n')}
- It may not call unsafe actions (they change things outside the app). To get one done, return an approval item for the human: { type: 'approval', params: { action, inputs, reason } }. The unsafe actions are:
${unsafe.join('\n')}
- It returns null, a string (said in the item's chat), or { message?, items? }: items are added in the item. Item types it may add:
${types.join('\n')}
- It runs in a sandbox (QuickJS): no fetch, no require, no timers. console.log is kept. Keep it short and plain.`
    const prompt = [
      `Action: ${a.name || '(unnamed)'}`,
      a.description.trim() && `What it does: ${a.description.trim()}`,
      a.notes.trim() && `Notes: ${a.notes.trim()}`,
      a.source?.trim() && `The code so far (rewrite it to match the above):\n${a.source.trim()}`,
    ]
      .filter(Boolean)
      .join('\n\n')
    const out = await this.engine.exec('claude', ['-p', prompt, '--model', 'sonnet', '--tools', '', '--strict-mcp-config', '--no-session-persistence', '--system-prompt', system])
    return out.trim().replace(/^```[\w-]*\n/, '').replace(/\n```$/, '').trim()
  }
}
