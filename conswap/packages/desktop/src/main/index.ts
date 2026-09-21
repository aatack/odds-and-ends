import { spawn } from 'node:child_process'
import type { ChildProcess } from 'node:child_process'
import { createRequire } from 'node:module'
import { fileURLToPath } from 'node:url'
import { app, BrowserWindow, shell } from 'electron'

const port = Number.parseInt(process.env.CONSWAP_PORT ?? '4319', 10)
const serverUrl = process.env.CONSWAP_SERVER ?? `http://127.0.0.1:${port}`
process.env.CONSWAP_SERVER_URL = serverUrl

let server: ChildProcess | null = null

/** Where the compiled backend lives, however this app was started. */
function backendEntry(): string {
  try {
    return createRequire(import.meta.url).resolve('@conswap/backend')
  } catch {
    return fileURLToPath(new URL('../../../backend/dist/index.js', import.meta.url))
  }
}

async function alive(): Promise<boolean> {
  try {
    const response = await fetch(`${serverUrl}/health`)
    return response.ok
  } catch {
    return false
  }
}

/**
 * The app starts its own backend, so there is only one thing to run. Pointing
 * CONSWAP_SERVER somewhere else skips this and talks to that instead.
 */
async function startServer(): Promise<void> {
  if (process.env.CONSWAP_SERVER) return
  if (await alive()) return

  const node = process.env.CONSWAP_NODE ?? 'node'
  server = spawn(node, [backendEntry()], {
    env: { ...process.env, CONSWAP_PORT: String(port) },
    stdio: ['ignore', 'pipe', 'pipe'],
  })
  server.stdout?.on('data', (chunk: Buffer) => process.stdout.write(chunk))
  server.stderr?.on('data', (chunk: Buffer) => process.stderr.write(chunk))
  server.on('exit', (code) => {
    process.stderr.write(`conswap: the backend stopped with code ${String(code)}\n`)
    server = null
  })

  const deadline = Date.now() + 20_000
  while (Date.now() < deadline) {
    if (await alive()) return
    await new Promise((resolve) => setTimeout(resolve, 200))
  }
  process.stderr.write('conswap: the backend never came up\n')
}

function createWindow(): void {
  const window = new BrowserWindow({
    width: 1280,
    height: 860,
    minWidth: 820,
    minHeight: 520,
    show: false,
    backgroundColor: '#0b0b0d',
    title: 'conswap',
    webPreferences: {
      preload: fileURLToPath(new URL('../preload/index.mjs', import.meta.url)),
      sandbox: false,
    },
  })

  window.on('ready-to-show', () => window.show())
  window.webContents.setWindowOpenHandler((details) => {
    void shell.openExternal(details.url)
    return { action: 'deny' }
  })

  const devServer = process.env.ELECTRON_RENDERER_URL
  if (devServer) void window.loadURL(devServer)
  else void window.loadFile(fileURLToPath(new URL('../renderer/index.html', import.meta.url)))
}

void app.whenReady().then(async () => {
  await startServer()
  createWindow()
  app.on('activate', () => {
    if (BrowserWindow.getAllWindows().length === 0) createWindow()
  })
})

app.on('window-all-closed', () => {
  if (process.platform !== 'darwin') app.quit()
})

app.on('before-quit', () => {
  server?.kill('SIGTERM')
})
