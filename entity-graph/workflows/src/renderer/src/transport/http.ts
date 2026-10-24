// The phone transport: the same WorkflowsAPI the preload gives desktop, but over
// HTTP + SSE instead of Electron IPC. Chosen when window.workflows is absent.
import type { WorkflowsAPI } from '../../../shared/types'

const TOKEN_KEY = 'workflows.token'

export const getToken = (): string | null => localStorage.getItem(TOKEN_KEY)
export const setToken = (token: string): void => localStorage.setItem(TOKEN_KEY, token)
export const clearToken = (): void => localStorage.removeItem(TOKEN_KEY)

/** The app subscribes to learn that the token is gone (401), so it can show Login. */
let onUnauthorized: (() => void) | null = null
export const setOnUnauthorized = (cb: () => void): void => {
  onUnauthorized = cb
}

/** A 401 clears the stored token and tells the app to show Login. */
function unauthorized(): void {
  clearToken()
  onUnauthorized?.()
}

function authHeaders(): Record<string, string> {
  const token = getToken()
  return token ? { Authorization: `Bearer ${token}` } : {}
}

/** Every data method: POST /api/<name> with { args }, bearer in the header. */
async function callMethod(name: string, args: unknown[]): Promise<unknown> {
  const res = await fetch(`/api/${name}`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json', ...authHeaders() },
    body: JSON.stringify({ args }),
  })
  if (res.status === 401) {
    unauthorized()
    throw new Error('Not authorised')
  }
  if (!res.ok) throw new Error((await res.text()) || `HTTP ${res.status}`)
  const text = await res.text()
  return text ? JSON.parse(text) : undefined
}

const call = (name: string) => (...args: unknown[]) => callMethod(name, args)

/** Methods the phone cannot have: they change its own access or run a CLI. */
const desktopOnly = (): Promise<never> => Promise.reject(new Error('Desktop only'))

/** The push channel: read /api/events as a stream, reconnecting on drop. No token in the URL. */
function openEvents(cb: () => void): () => void {
  let closed = false
  const controller = new AbortController()

  const connect = async (): Promise<void> => {
    try {
      const res = await fetch('/api/events', { headers: authHeaders(), signal: controller.signal })
      if (res.status === 401) return unauthorized()
      if (!res.ok || !res.body) throw new Error(`events HTTP ${res.status}`)
      // Fetch once on every (re)connect, so changes missed while disconnected are not lost.
      cb()
      const reader = res.body.getReader()
      const decoder = new TextDecoder()
      // Any non-empty data line means "something changed"; SSE framing is otherwise ignored.
      for (;;) {
        const { value, done } = await reader.read()
        if (done) break
        if (decoder.decode(value, { stream: true }).includes('data:')) cb()
      }
    } catch {
      // fall through to reconnect
    }
    if (!closed) setTimeout(() => void connect(), 2000)
  }

  void connect()
  return () => {
    closed = true
    controller.abort()
  }
}

export const httpApi: WorkflowsAPI = {
  overview: call('overview') as WorkflowsAPI['overview'],
  detail: call('detail') as WorkflowsAPI['detail'],
  eventDetail: call('eventDetail') as WorkflowsAPI['eventDetail'],
  catalog: call('catalog') as WorkflowsAPI['catalog'],
  next: call('next') as WorkflowsAPI['next'],
  viewed: call('viewed') as WorkflowsAPI['viewed'],
  skipped: call('skipped') as WorkflowsAPI['skipped'],
  create: call('create') as WorkflowsAPI['create'],
  send: call('send') as WorkflowsAPI['send'],
  approve: call('approve') as WorkflowsAPI['approve'],
  cancel: call('cancel') as WorkflowsAPI['cancel'],
  close: call('close') as WorkflowsAPI['close'],
  handledOutside: call('handledOutside') as WorkflowsAPI['handledOutside'],
  pollSources: call('pollSources') as WorkflowsAPI['pollSources'],
  integrations: call('integrations') as WorkflowsAPI['integrations'],
  addSlack: call('addSlack') as WorkflowsAPI['addSlack'],
  removeSlack: call('removeSlack') as WorkflowsAPI['removeSlack'],
  mcp: call('mcp') as WorkflowsAPI['mcp'],
  regenerateMcpToken: call('regenerateMcpToken') as WorkflowsAPI['regenerateMcpToken'],
  setMcpPort: call('setMcpPort') as WorkflowsAPI['setMcpPort'],
  // The phone-admin methods are desktop-only: a remote client can neither run a CLI nor change its own access.
  phone: desktopOnly as WorkflowsAPI['phone'],
  setPhoneEnabled: desktopOnly as WorkflowsAPI['setPhoneEnabled'],
  setPhonePort: desktopOnly as WorkflowsAPI['setPhonePort'],
  regeneratePhoneToken: desktopOnly as WorkflowsAPI['regeneratePhoneToken'],
  runTailscaleServe: desktopOnly as WorkflowsAPI['runTailscaleServe'],
  onChange: (cb) => openEvents(cb),
  // On the phone the browser's own history drives Back; there is no desktop Ctrl+W channel.
  onBack: () => () => {},
}
