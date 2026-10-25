// What the backend answers over HTTP: every method of WorkflowsAPI that is not
// an event subscription, by name, with its arguments as a JSON array. The app's
// main process passes the renderer's calls through to it (src/main/backend.ts).

import type { WorkflowsAPI } from '../shared/types'
import type { Admin } from '../engine/admin'
import type { Engine } from '../engine/engine'
import { catalog } from '../engine/mcp'

type Methods = Omit<WorkflowsAPI, 'onChange' | 'onBack' | 'onForward'>
type Handlers = { [K in keyof Methods]: (...args: Parameters<Methods[K]>) => unknown }

/** For the app alone: secrets saved with Electron's safeStorage, which only the app can read, handed back to be kept anew. */
export interface LegacySecrets {
  legacySecrets: () => string[]
  replaceLegacySecrets: (plain: Record<string, string>) => void
}

export function handlers(engine: Engine, admin: Admin): Handlers & LegacySecrets {
  return {
    overview: () => engine.overview(),
    detail: (id) => engine.detail(id),
    eventDetail: (id) => engine.eventDetail(id),
    catalog: () => catalog(engine),
    next: (skip) => engine.next(skip),
    viewed: (id) => engine.viewed(id),
    skipped: (id) => engine.skipped(id),
    create: (title, parentId) => engine.create(title, { parentId }),
    send: (id, preset, text) => engine.send(id, preset, text),
    approve: (eventId, inputs) => engine.approve(eventId, inputs),
    cancel: (eventId) => engine.cancel(eventId),
    close: (id) => engine.close(id),
    attach: (eventId, targetId) => engine.attach(eventId, targetId),
    directives: () => engine.directives(),
    addDirective: (text) => engine.addDirective(text),
    editDirective: (id, text) => engine.editDirective(id, text),
    setPriority: (id, on) => engine.setPriority(id, on),
    rename: (id, title) => engine.rename(id, title),
    snooze: (id, until) => engine.snooze(id, until),
    unsnooze: (id) => engine.unsnooze(id),
    summarise: (id) => engine.summarise(id),
    customActions: () => engine.customActions.list(),
    saveCustomAction: (a) => engine.customActions.save(a),
    removeCustomAction: (id) => engine.customActions.remove(id),
    runCustomAction: async (id, eventId) => void (await engine.customActions.run(id, eventId)),
    generateCustomAction: (a) => engine.customActions.generate(a),
    handledOutside: (id) => engine.handledOutside(id),
    pollSources: () => engine.pollSources(true),
    saveBot: (bot) => engine.saveAgent(bot),
    deleteBot: (id) => engine.deleteAgent(id),
    assign: (itemId, to, on) => engine.assign(itemId, to, on),
    assignments: (assignee, days) => engine.assignments(assignee, days),
    stale: (hours) => engine.stale(hours),
    wakeBot: (id) => engine.wakeAgent(id),
    integrations: () => admin.integrations(),
    addSlack: (token) => admin.addSlack(token),
    removeSlack: (id) => admin.removeSlack(id),
    mcp: () => admin.mcpView(),
    regenerateMcpToken: () => admin.regenerateMcpToken(),
    setMcpPort: (port) => admin.setMcpPort(port),
    legacySecrets: () => engine.settings.legacySecrets(),
    replaceLegacySecrets: (plain) => engine.settings.replaceLegacySecrets(plain),
  }
}
