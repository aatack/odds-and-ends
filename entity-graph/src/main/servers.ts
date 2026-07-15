import { spawn, type ChildProcess } from 'child_process'
import net from 'net'
import { randomBytes } from 'crypto'
import { join } from 'path'
import { app } from 'electron'
import { v4 as uuidv4 } from 'uuid'
import type { Connection, LocalServer } from '../core/client'
import { store, type LocalServerDef } from './store'

const HOST = '127.0.0.1'

/** Grab an OS-assigned free port on the loopback interface. */
function findFreePort(): Promise<number> {
  return new Promise((resolve, reject) => {
    const srv = net.createServer()
    srv.on('error', reject)
    srv.listen(0, HOST, () => {
      const port = (srv.address() as net.AddressInfo).port
      srv.close(() => resolve(port))
    })
  })
}

const sleep = (ms: number) => new Promise((r) => setTimeout(r, ms))

/** Poll until the server answers on `port` (any HTTP response means it's up). */
async function waitUntilUp(port: number, timeoutMs = 15000): Promise<void> {
  const deadline = Date.now() + timeoutMs
  while (Date.now() < deadline) {
    try {
      await fetch(`http://${HOST}:${port}/admin`)
      return
    } catch {
      await sleep(150)
    }
  }
  throw new Error(`local server on port ${port} did not start in time`)
}

/**
 * Manages local server child processes. Each `create()` spins up the *unchanged*
 * server (`server/src/main.ts`) on its own port with its own admin token and
 * config DB, and registers a linked admin connection so the UI can manage it.
 */
export class ServerManager {
  private children = new Map<string, ChildProcess>()

  private configDbPath(id: string): string {
    return join(app.getPath('userData'), 'servers', id, 'config.db')
  }

  private spawnChild(def: LocalServerDef): void {
    const root = app.getAppPath()
    const tsxBin = join(root, 'node_modules', '.bin', 'tsx')
    const entry = join(root, 'server', 'src', 'main.ts')
    const child = spawn(tsxBin, [entry], {
      cwd: join(root, 'server'),
      env: {
        ...process.env,
        PORT: String(def.port),
        HOST,
        ADMIN_TOKEN: def.adminToken,
        CONFIG_DB: this.configDbPath(def.id),
      },
      stdio: ['ignore', 'pipe', 'pipe'],
    })
    child.stderr?.on('data', (d: Buffer) =>
      // eslint-disable-next-line no-console
      console.error(`[server ${def.label}] ${d.toString().trimEnd()}`),
    )
    child.on('exit', (code) => {
      this.children.delete(def.id)
      // eslint-disable-next-line no-console
      if (code) console.error(`[server ${def.label}] exited with code ${code}`)
    })
    this.children.set(def.id, child)
  }

  private defs(): LocalServerDef[] {
    return store.get('localServers')
  }

  /** Spawn every persisted local server (called on app ready). Best-effort. */
  startAll(): void {
    for (const def of this.defs()) {
      if (!this.children.has(def.id)) this.spawnChild(def)
    }
  }

  /** Kill every running child (called on quit). */
  stopAll(): void {
    for (const child of this.children.values()) child.kill()
    this.children.clear()
  }

  list(): LocalServer[] {
    const connections = store.get('connections')
    return this.defs().map((d) => ({
      id: d.id,
      label: d.label,
      port: d.port,
      baseUrl: `http://${HOST}:${d.port}`,
      running: this.children.has(d.id),
      connectionId: connections.find((c) => c.localServerId === d.id)?.id,
    }))
  }

  /** Create, start, and register a new local server. Returns its admin connection id. */
  async create(label: string): Promise<{ id: string; connectionId: string }> {
    const id = uuidv4()
    const port = await findFreePort()
    const def: LocalServerDef = { id, label, port, adminToken: randomBytes(24).toString('hex') }
    store.set('localServers', [...this.defs(), def])
    this.spawnChild(def)
    await waitUntilUp(port)

    const connectionId = uuidv4()
    const conn: Connection = {
      id: connectionId,
      label,
      baseUrl: `http://${HOST}:${port}`,
      kind: 'admin',
      token: def.adminToken,
      localServerId: id,
    }
    store.set('connections', [...store.get('connections'), conn])
    return { id, connectionId }
  }

  async start(id: string): Promise<void> {
    if (this.children.has(id)) return
    const def = this.defs().find((d) => d.id === id)
    if (!def) throw new Error(`no local server "${id}"`)
    this.spawnChild(def)
    await waitUntilUp(def.port)
  }

  stop(id: string): void {
    this.children.get(id)?.kill()
    this.children.delete(id)
  }

  /** Stop the server and forget it (its config DB is left on disk). */
  remove(id: string): void {
    this.stop(id)
    store.set(
      'localServers',
      this.defs().filter((d) => d.id !== id),
    )
    const remaining = store.get('connections').filter((c) => c.localServerId !== id)
    store.set('connections', remaining)
    if (!remaining.some((c) => c.id === store.get('activeId'))) store.set('activeId', null)
  }
}
