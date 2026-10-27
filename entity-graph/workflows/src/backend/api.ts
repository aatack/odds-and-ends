// What the backend answers over HTTP: every method of WorkflowsAPI that is not
// an event subscription, by name, with its arguments as a JSON array, made as
// the person the call says it is from (the `x-workflows-user` header; there is
// no password, as the app is local). The app's main process passes the
// renderer's calls through to it (src/main/backend.ts).

import type { WorkflowsAPI } from '../shared/types'
import type { Admin } from '../engine/admin'
import type { Engine } from '../engine/engine'
import { catalog } from '../engine/mcp'

type Methods = Omit<WorkflowsAPI, 'onChange' | 'onBack' | 'onForward' | 'setUser' | 'openSource' | 'closeSource' | 'setToken' | 'token'>
type Handlers = { [K in keyof Methods]: (...args: Parameters<Methods[K]>) => unknown }

/** Calls that need nobody signed in: signing in, seeing who there is to sign in as, and which source it is (the phone asks first). */
export const OPEN_METHODS = new Set(['signIn', 'users', 'saveUser', 'catalog', 'source'])

export function handlers(engine: Engine, admin: Admin, me: string, source = ''): Handlers {
  return {
    overview: () => engine.overview(me),
    detail: (id) => engine.detail(id, me),
    eventDetail: (id) => engine.eventDetail(id, me),
    catalog: () => catalog(engine),
    next: (skip) => engine.next(skip, me),
    inspect: (id) => engine.inspect(id),
    create: (title, parentId) => engine.create(title, { parentId, actor: me }),
    send: (id, preset, text) => engine.send(id, preset, text, me),
    approve: (eventId, inputs) => engine.approve(eventId, inputs, me),
    cancel: (eventId) => engine.cancel(eventId, me),
    runNow: (eventId) => engine.runNow(eventId, me),
    close: (id) => engine.close(id, { actor: me }),
    reopen: (id) => engine.reopen(id, me),
    react: (eventId, on) => engine.react(eventId, on, me),
    addEvents: (itemId, events) => engine.addEvents(itemId, events, me, 'human'),
    searchSections: () => engine.searchSections(),
    setSearchSections: (sections) => engine.setSearchSections(sections, me),
    runSearchSection: (section, text) => engine.runSearchSection(section, text),
    setWidgetState: (eventId, state) => engine.setWidgetState(eventId, state, me),
    editLog: (query) => engine.store.log(query),
    processOutput: (eventId, n) => ({ status: engine.processes.status(eventId), lines: engine.processes.output(eventId, n) }),
    rename: (id, title) => engine.rename(id, title, me),
    customActions: () => engine.customActions.list(),
    saveCustomAction: (a) => engine.customActions.save(a),
    removeCustomAction: (id) => engine.customActions.remove(id),
    runCustomAction: async (id, eventId) => void (await engine.customActions.run(id, eventId, me)),
    generateCustomAction: (a) => engine.customActions.generate(a),
    saveBot: (bot) => engine.saveAgent(bot, me),
    deleteBot: (id) => engine.deleteAgent(id, me),
    assign: (itemId, to, on) => engine.assign(itemId, to, on, me),
    wakeBot: (id) => engine.wakeAgent(id),
    users: () => engine.users(),
    saveUser: (username, name) => engine.saveUser(username, name, me),
    signIn: (username) => engine.signIn(username),
    integrations: () => admin.integrations(),
    addSlack: (token) => admin.addSlack(token),
    removeSlack: (id) => admin.removeSlack(id),
    setIntegrationValue: (id, key, value) => admin.setIntegrationValue(id, key, value),
    clearSlackCache: () => admin.clearSlackCache(),
    slackView: (eventId, thread) => admin.slackView(eventId, thread),
    setDeepgramKey: (key) => engine.settings.setDeepgramKey(key),
    hasDeepgram: () => engine.settings.deepgramKey() !== null,
    deepgramKey: () => engine.settings.deepgramKey()?.key ?? null,
    mcp: () => admin.mcpView(),
    regenerateMcpToken: () => admin.regenerateMcpToken(),
    setMcpPort: (port) => admin.setMcpPort(port),
    source: () => source,
    phone: () => admin.phoneView(),
    setPhoneTailnet: (on) => admin.setPhoneTailnet(on),
    setPhonePort: (port) => admin.setPhonePort(port),
    regeneratePhoneToken: () => admin.regeneratePhoneToken(),
  }
}
