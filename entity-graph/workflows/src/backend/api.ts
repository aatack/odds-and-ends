// What the backend answers over HTTP: every method of WorkflowsAPI that is not
// an event subscription, by name, with its arguments as a JSON array, made as
// the person the call says it is from (the `x-workflows-user` header; there is
// no password, as the app is local). The app's main process passes the
// renderer's calls through to it (src/main/backend.ts).

import type { WorkflowsAPI } from '../shared/types'
import type { Admin } from '../engine/admin'
import type { Engine } from '../engine/engine'
import { catalog } from '../engine/mcp'

type Methods = Omit<WorkflowsAPI, 'onChange' | 'onBack' | 'onForward' | 'setUser' | 'openSource' | 'closeSource'>
type Handlers = { [K in keyof Methods]: (...args: Parameters<Methods[K]>) => unknown }

/** Calls that need nobody signed in: signing in, and seeing who there is to sign in as. */
export const OPEN_METHODS = new Set(['signIn', 'users', 'saveUser', 'catalog'])

export function handlers(engine: Engine, admin: Admin, me: string): Handlers {
  return {
    overview: () => engine.overview(me),
    detail: (id) => engine.detail(id, me),
    eventDetail: (id) => engine.eventDetail(id, me),
    catalog: () => catalog(engine),
    next: (skip) => engine.next(skip, me),
    viewed: (id) => engine.viewed(id, me),
    skipped: (id) => engine.skipped(id, me),
    create: (title, parentId) => engine.create(title, { parentId, actor: me }),
    send: (id, preset, text) => engine.send(id, preset, text, me),
    approve: (eventId, inputs) => engine.approve(eventId, inputs, me),
    cancel: (eventId) => engine.cancel(eventId, me),
    close: (id) => engine.close(id, { actor: me }),
    attach: (eventId, targetId) => engine.attach(eventId, targetId, me),
    directives: () => engine.directives(),
    addDirective: (text) => engine.addDirective(text, me),
    editDirective: (id, text) => engine.editDirective(id, text, me),
    setPriority: (id, on) => engine.setPriority(id, on, me),
    rename: (id, title) => engine.rename(id, title, me),
    snooze: (id, until) => engine.snooze(id, until, me),
    unsnooze: (id) => engine.unsnooze(id, me),
    summarise: (id) => engine.summarise(id, me),
    customActions: () => engine.customActions.list(),
    saveCustomAction: (a) => engine.customActions.save(a),
    removeCustomAction: (id) => engine.customActions.remove(id),
    runCustomAction: async (id, eventId) => void (await engine.customActions.run(id, eventId, me)),
    generateCustomAction: (a) => engine.customActions.generate(a),
    handledOutside: (id) => engine.handledOutside(id, me),
    pollSources: () => engine.pollSources(true),
    saveBot: (bot) => engine.saveAgent(bot, me),
    deleteBot: (id) => engine.deleteAgent(id, me),
    assign: (itemId, to, on) => engine.assign(itemId, to, on, me),
    assignments: (assignee, days) => engine.assignments(assignee, days),
    stale: (hours) => engine.stale(hours, me),
    wakeBot: (id) => engine.wakeAgent(id),
    users: () => engine.users(),
    saveUser: (username, name) => engine.saveUser(username, name, me),
    signIn: (username) => engine.signIn(username),
    integrations: () => admin.integrations(),
    addSlack: (token) => admin.addSlack(token),
    removeSlack: (id) => admin.removeSlack(id),
    mcp: () => admin.mcpView(),
    regenerateMcpToken: () => admin.regenerateMcpToken(),
    setMcpPort: (port) => admin.setMcpPort(port),
  }
}
