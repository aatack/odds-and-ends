// Custom actions: things the human defines, per event type, on the Types page.
// One is a script (JavaScript, run in QuickJS like any other script) handed a
// context (the event it was run on, and its item) and the safe actions. It is
// usually written by a model from a description and some notes, so it is held
// to the bots' rule: no unsafe actions; it asks for one with an approval event.

import { randomUUID } from 'node:crypto'
import { z } from 'zod'
import type { CustomAction, EventSpec } from '../shared/types'
import type { Engine } from './engine'
import { objectSchema } from './schema'
import { runScript } from './script'
import type { CustomActionRow, Store } from './store'

// The human's own actions, per event type: configuration, like settings, so
// edited in place. Running one is recorded as an action event in the item.
const toAction = ({ created_at: _c, updated_at: _u, ...a }: CustomActionRow): CustomAction => a

/** What a custom action's function may return: nothing, a line for the chat, or that and events to add in the item. */
const result = z.union([
  z.null(),
  z.string(),
  z.object({
    message: z.string().optional(),
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
    if (!this.engine.eventTypes.some((t) => t.type === a.type)) throw new Error(`No event type ${a.type}.`)
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
   * Runs one on an event: its function gets the context (the event, its item,
   * the time) and the safe actions; what it returns is said in the item's
   * chat, as an action event, and any events it returns are added there.
   */
  async run(id: string, eventId: string, actor = 'human', runId: string | null = null): Promise<{ message: string | null }> {
    const action = this.get(id)
    if (!action) throw new Error(`No custom action ${id}.`)
    const event = this.engine.event(eventId)
    if (!event) throw new Error(`No event ${eventId}.`)
    if (event.type !== action.type) throw new Error(`${action.name} is for ${action.type} events, not ${event.type}.`)
    if (!action.source.trim()) throw new Error(`${action.name} has no code yet: write or generate it on the Types page.`)
    const itemId = event.type === 'item' ? event.id : event.parentId
    const item = itemId ? this.engine.get(itemId) : null
    if (!item) throw new Error('That event is in no item.')
    const context = {
      item: { id: item.id, title: item.title, parentId: item.parentId, closed: item.closed, withHuman: item.withHuman, priority: item.priority },
      event: { id: event.id, type: event.type, summary: event.summary, params: event.params, data: event.data, resolved: event.resolved, parentId: event.parentId },
      now: new Date(this.engine.now()).toISOString(),
    }
    const started = this.engine.now()
    let message: string | null = null
    let events: EventSpec[] = []
    try {
      const ran = await runScript(action.source, context, (name, args) =>
        this.engine.callAction(name, args, { engine: this.engine, author: 'bot', actor: 'script', itemId: item.id, runId }),
      )
      const parsed = result.safeParse(ran.value ?? null)
      if (!parsed.success) throw new Error(`It returned ${JSON.stringify(ran.value)?.slice(0, 200)}, not null, a string, or { message?, events? }.`)
      const out = parsed.data
      message = typeof out === 'string' ? out : (out?.message ?? null)
      events = typeof out === 'object' && out ? ((out.events ?? []) as EventSpec[]) : []
      // Its events are held to the bots' rules, and checked before anything is added.
      this.engine.prepare(events, 'bot')
      if (ran.logs.length) this.engine.mark(item.id, 'custom_action_logs', { eventId, actor, data: { action: action.name, logs: ran.logs } })
    } catch (e) {
      this.engine.mark(item.id, 'custom_action_run', { eventId, actor, runId, data: { action: action.name, id, ok: false, error: (e as Error).message, ms: this.engine.now() - started } })
      throw e
    }
    this.engine.mark(item.id, 'custom_action_run', { eventId, actor, runId, data: { action: action.name, id, ok: true, ms: this.engine.now() - started } })
    // The record of the run is the app's own; what the script returned is a bot's.
    await this.engine.addEvents(item.id, [{ type: 'action', params: { name: action.name, inputs: { event: eventId }, output: { message } } }], actor, 'human', runId)
    if (events.length) await this.engine.addEvents(item.id, events, actor, 'bot', runId)
    return { message }
  }

  /**
   * Writes an action's code from its description and notes, with a model
   * (Sonnet, no tools): the function, told the context it gets, the actions it
   * may call and the events it may add.
   */
  async generate(a: { type: string; name: string; description: string; notes: string; source?: string }): Promise<string> {
    if (!a.description.trim() && !a.notes.trim()) throw new Error('Say what it should do first: a description, or notes.')
    const actions = this.engine.actions
      .filter((x) => !x.unsafe)
      .map((x) => `- ${x.name}(${JSON.stringify(objectSchema(x.input).properties ?? {})}): ${x.description}`)
    const events = this.engine.eventTypes.filter((t) => t.bots).map((t) => `- ${t.type} ${JSON.stringify(objectSchema(t.params).properties ?? {})}: ${t.description}`)
    const unsafe = this.engine.actions.filter((x) => x.unsafe).map((x) => `- ${x.name}: ${x.description}`)
    const system = `You write one custom action for a workflow app, as a JavaScript function expression. Reply with the code only: no prose, no markdown fences.

The function is: async (context, actions) => result
- It runs on one event of type "${a.type}". context is { item: { id, title, parentId, closed, withHuman, priority }, event: { id, type, summary, params, data, resolved, parentId }, now (ISO time) }. For an item event, item and event are the same item.
- actions.<name>(args) calls an action and resolves to its result (parsed JSON). These are the only ones it may call:
${actions.join('\n')}
- It may not call unsafe actions (they change things outside the app). To get one done, return an approval event for the human: { type: 'approval', params: { action, inputs, reason } }. The unsafe actions are:
${unsafe.join('\n')}
- It returns null, a string (said in the item's chat), or { message?, events? }: events are added in the item. Event types it may add:
${events.join('\n')}
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
