// What the backend answers over HTTP: every method of WorkflowsAPI that is not
// an item subscription, by name, with its arguments as a JSON array, made as
// the person the call says it is from (the `x-workflows-user` header; there is
// no password, as the app is local). The app's main process passes the
// renderer's calls through to it (src/main/backend.ts).

import type { WorkflowsAPI } from '../shared/types'
import { slackConversations } from '../engine/slack'
import type { Admin } from '../engine/admin'
import type { Engine } from '../engine/engine'
import { catalog } from '../engine/mcp'
import { egdbCall, type EgdbTarget } from './egdbProxy'

type Methods = Omit<WorkflowsAPI, 'onChange' | 'onBack' | 'onForward' | 'setUser' | 'openSource' | 'closeSource' | 'setToken' | 'token' | 'copyFile'>
type Handlers = { [K in keyof Methods]: (...args: Parameters<Methods[K]>) => unknown }

/** Calls that need nobody signed in: signing in, seeing who there is to sign in as, and which source it is (the phone asks first). */
export const OPEN_METHODS = new Set(['signIn', 'users', 'saveUser', 'catalog', 'source'])

export function handlers(engine: Engine, admin: Admin, me: string, source = '', egdb: EgdbTarget | null = null): Handlers {
  return {
    overview: () => engine.overview(me),
    detail: (id) => engine.detail(id, me),
    itemInfoDetail: (id) => engine.itemInfoDetail(id, me),
    catalog: () => catalog(engine),
    next: (skip, later) => engine.next(skip, me, later ?? []),
    inspect: (id) => engine.inspect(id),
    create: (title, parentId, id) => engine.create(title, { parentId, actor: me, id }),
    send: (id, preset, text, commentId) => engine.send(id, preset, text, me, commentId),
    cancel: (itemId) => engine.cancel(itemId, me),
    runNow: (itemId, given) => engine.runNow(itemId, me, given ?? {}),
    close: (id) => engine.close(id, { actor: me }),
    reopen: (id) => engine.reopen(id, me),
    write: (ops) => engine.write(ops, me),
    egdb: (route, body) => {
      if (!egdb) throw new Error('This backend has no egdb.')
      return egdbCall(egdb, engine, route, body, me)
    },
    addItems: (itemId, specs, via) => engine.addItems(itemId, specs, me, 'human', via ?? 'comments'),
    uploadFile: (itemId, name, base64) => engine.uploadFile(itemId, name, base64, me),
    fileData: async (itemId) => engine.fileData(itemId),
    fileText: async (itemId) => engine.fileText(itemId),
    saveFileText: async (itemId, text) => engine.saveFileText(itemId, text),
    runTool: (itemId, name, args) => engine.runTool(itemId, name, args, me),
    pin: (itemId, itemIds, on) => engine.pin(itemId, itemIds, on, me),
    setValue: (itemId, key, value) => engine.setValue(itemId, key, value, me),
    setOpen: (itemId, open) => engine.setOpen(itemId, open, me),
    moveItem: (itemId, to) => engine.moveItem(itemId, to, me),
    newNotes: (title, pinTo) => engine.newNotes(title, pinTo, me),
    subtree: (itemId) => engine.subtree(itemId),
    setInferredType: (itemId) => engine.setInferredType(itemId, me),
    finder: (who) => engine.finder(who, 16, me),
    items: async (ids) => ids.flatMap((id) => engine.get(id, me) ?? []),
    setOwner: (itemId, owner) => engine.setOwner(itemId, owner, me),
    editLog: (query) => engine.store.log(query),
    rename: (id, title) => engine.rename(id, title, me),
    typeScripts: (type) => engine.typeScripts(type),
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
    trustBotFolders: () => admin.trustBotFolders(),
    slackView: (itemId, thread) => admin.slackView(itemId, thread),
    slackConversations: () => slackConversations(engine),
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
    setPhoneTailnetPort: (port) => admin.setPhoneTailnetPort(port),
    regeneratePhoneToken: () => admin.regeneratePhoneToken(),
  }
}
