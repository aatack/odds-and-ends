import type { Admin } from '../engine/admin'
import type { Engine } from '../engine/engine'
import { catalog } from '../engine/mcp'
import type { Handlers } from '../engine/server'
import type { WorkerPool } from '../engine/workers'

export type { Handler, Handlers } from '../engine/server'

/** The whole WorkflowsAPI as a name → fn table, so both transports dispatch the same surface. */
export function makeHandlers(engine: Engine, admin: Admin, pool: WorkerPool): Handlers {
  return {
    overview: () => engine.overview(pool.status()),
    detail: (id: string) => engine.detail(id),
    eventDetail: (id: string) => engine.eventDetail(id),
    catalog: () => catalog(engine),
    next: (skip: string[]) => engine.next(skip),
    viewed: (id: string) => engine.viewed(id),
    skipped: (id: string) => engine.skipped(id),
    create: (title: string, parentId: string | null) => engine.create(title, { parentId }),
    send: (id: string, preset: string, text: string) => engine.send(id, preset, text),
    approve: (eventId: string, inputs: Record<string, unknown>) => engine.approve(eventId, inputs),
    cancel: (eventId: string) => engine.cancel(eventId),
    close: (id: string) => engine.close(id),
    handledOutside: (id: string) => engine.handledOutside(id),
    pollSources: () => engine.pollSources(true),
    integrations: () => admin.integrations(),
    addSlack: (token: string) => admin.addSlack(token),
    removeSlack: (id: string) => admin.removeSlack(id),
    mcp: () => admin.mcpView(),
    regenerateMcpToken: () => admin.regenerateMcpToken(),
    setMcpPort: (port: number) => admin.setMcpPort(port),
    // Phone-admin methods: in the IPC table for the desktop window, but NOT in the
    // HTTP allowlist (server.ts PHONE_METHODS) — a remote client may not change its own access.
    phone: () => admin.phoneView(),
    setPhoneEnabled: (on: boolean) => admin.setPhoneEnabled(on),
    setPhonePort: (port: number) => admin.setPhonePort(port),
    regeneratePhoneToken: () => admin.regeneratePhoneToken(),
    runTailscaleServe: () => admin.runTailscaleServe(),
  }
}
