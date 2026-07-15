import { spawn, type ChildProcess } from 'child_process'
import net from 'net'
import { randomBytes } from 'crypto'
import { join } from 'path'
import { app } from 'electron'
import { v4 as uuidv4 } from 'uuid'
import type { NewServer, Server, ServerView } from '../core/client'
import { store } from './store'

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
 * Manages servers and, for local ones, their child processes. Each local server
 * runs the *unchanged* server (`server/src/main.ts`) on its own port with its
 * own admin token and config DB.
 */
export class ServerManager {
  private children = new Map<string, ChildProcess>()

  private configDbPath(id: string): string {
    return join(app.getPath('userData'), 'servers', id, 'config.db')
  }

  private servers(): Server[] {
    return store.get('servers')
  }

  private save(servers: Server[]): void {
    store.set('servers', servers)
  }

  get(id: string): Server | undefined {
    return this.servers().find((s) => s.id === id)
  }

  private spawnChild(server: Server): void {
    if (server.localPort === undefined || !server.adminToken) return
    const root = app.getAppPath()
    const tsxBin = join(root, 'node_modules', '.bin', 'tsx')
    const entry = join(root, 'server', 'src', 'main.ts')
    const child = spawn(tsxBin, [entry], {
      cwd: join(root, 'server'),
      env: {
        ...process.env,
        PORT: String(server.localPort),
        HOST,
        ADMIN_TOKEN: server.adminToken,
        CONFIG_DB: this.configDbPath(server.id),
      },
      stdio: ['ignore', 'pipe', 'pipe'],
    })
    child.stderr?.on('data', (d: Buffer) =>
      // eslint-disable-next-line no-console
      console.error(`[server ${server.label}] ${d.toString().trimEnd()}`),
    )
    child.on('exit', (code) => {
      this.children.delete(server.id)
      // eslint-disable-next-line no-console
      if (code) console.error(`[server ${server.label}] exited with code ${code}`)
    })
    this.children.set(server.id, child)
  }

  /** Spawn every persisted local server (called on app ready). Best-effort. */
  startAll(): void {
    for (const s of this.servers()) {
      if (s.localPort !== undefined && !this.children.has(s.id)) this.spawnChild(s)
    }
  }

  /** Kill every running child (called on quit). */
  stopAll(): void {
    for (const child of this.children.values()) child.kill()
    this.children.clear()
  }

  list(): ServerView[] {
    return this.servers().map((s) => ({
      id: s.id,
      label: s.label,
      baseUrl: s.baseUrl,
      kind: s.localPort !== undefined ? 'local' : 'external',
      admin: !!s.adminToken,
      running: this.children.has(s.id),
    }))
  }

  /** Register an external server (base URL + optional admin token). */
  addExternal(cfg: NewServer): string {
    const id = uuidv4()
    const server: Server = {
      id,
      label: cfg.label,
      baseUrl: (cfg.baseUrl ?? '').replace(/\/+$/, ''),
      ...(cfg.adminToken ? { adminToken: cfg.adminToken } : {}),
    }
    this.save([...this.servers(), server])
    return id
  }

  /** Create, start, and register a new local server. */
  async createLocal(label: string): Promise<{ id: string }> {
    const id = uuidv4()
    const port = await findFreePort()
    const server: Server = {
      id,
      label,
      baseUrl: `http://${HOST}:${port}`,
      adminToken: randomBytes(24).toString('hex'),
      localPort: port,
    }
    this.save([...this.servers(), server])
    this.spawnChild(server)
    await waitUntilUp(port)
    return { id }
  }

  /** Patch a server's editable fields (label, external base URL, admin token). */
  update(id: string, patch: Partial<NewServer>): void {
    this.save(
      this.servers().map((s) => {
        if (s.id !== id) return s
        const next: Server = { ...s, id }
        if (patch.label !== undefined) next.label = patch.label
        // Only external servers may change their base URL.
        if (patch.baseUrl !== undefined && s.localPort === undefined)
          next.baseUrl = patch.baseUrl.replace(/\/+$/, '')
        if (patch.adminToken !== undefined)
          patch.adminToken ? (next.adminToken = patch.adminToken) : delete next.adminToken
        return next
      }),
    )
  }

  async start(id: string): Promise<void> {
    if (this.children.has(id)) return
    const server = this.get(id)
    if (!server || server.localPort === undefined) throw new Error(`no local server "${id}"`)
    this.spawnChild(server)
    await waitUntilUp(server.localPort)
  }

  stop(id: string): void {
    this.children.get(id)?.kill()
    this.children.delete(id)
  }

  /** Stop the server (if local) and forget it, along with its source connections. */
  remove(id: string): void {
    this.stop(id)
    this.save(this.servers().filter((s) => s.id !== id))
    store.set(
      'sourceConnections',
      store.get('sourceConnections').filter((c) => c.serverId !== id),
    )
  }
}
