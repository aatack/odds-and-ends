import { chmodSync, readFileSync, writeFileSync } from 'node:fs'
import { mkdir, writeFile } from 'node:fs/promises'
import { tmpdir } from 'node:os'
import { basename, join } from 'node:path'
import { pathToFileURL } from 'node:url'
import { app, BrowserWindow, clipboard, ipcMain, safeStorage, session, shell, type WebContents } from 'electron'
import { isRemote, sourceFile } from '../backend/info'
import { backend, remote, type Backend } from './backend'

// A profile of its own (local storage, window state), apart from the older app's, so the two run side by side.
app.setPath('userData', join(app.getPath('appData'), 'workflows-egdb'))

/** The session a link's page is shown in (LinkFocus's webview). */
const LINKS = 'persist:links'

// A remote source's phone token, kept per URL so the next start opens it without asking again. In the app's userData
// folder, mode 0600; encrypted with the OS keychain (safeStorage) where it is available, else plain (a `plain:`/`enc:`
// marker says which). The window never receives it back: only the URL it is kept under.
const remoteTokensFile = (): string => join(app.getPath('userData'), 'remote.json')
const readTokens = (): Record<string, string> => {
  try {
    return JSON.parse(readFileSync(remoteTokensFile(), 'utf8')) as Record<string, string>
  } catch {
    return {}
  }
}
const writeTokens = (all: Record<string, string>): void => {
  const file = remoteTokensFile()
  writeFileSync(file, JSON.stringify(all), { mode: 0o600 })
  // The mode above only takes on a new file; make sure of it on one that was there already.
  chmodSync(file, 0o600)
}
const readToken = (url: string): string | null => {
  const stored = readTokens()[url]
  if (stored?.startsWith('enc:') && safeStorage.isEncryptionAvailable()) {
    try {
      return safeStorage.decryptString(Buffer.from(stored.slice(4), 'base64'))
    } catch {
      return null
    }
  }
  return stored?.startsWith('plain:') ? stored.slice(6) : null
}
const saveToken = (url: string, token: string): void => {
  writeTokens({ ...readTokens(), [url]: safeStorage.isEncryptionAvailable() ? `enc:${safeStorage.encryptString(token).toString('base64')}` : `plain:${token}` })
}
const forgetToken = (url: string): void => {
  const all = readTokens()
  if (!(url in all)) return
  delete all[url]
  writeTokens(all)
}

/**
 * Ctrl+W (rather than closing the window, as the default menu would), Alt+Left and Ctrl+Shift+Tab take the top view off
 * the stack; Alt+Right and Ctrl+Tab put the last one taken off back on. Pressed in the app or in a link's page.
 */
function navKeys(to: WebContents, event: Electron.Event, input: Electron.Input): void {
  if (input.type !== 'keyDown' || input.meta) return
  const ctrlTab = input.control && !input.alt && input.key === 'Tab'
  const ctrlW = input.control && !input.alt && !input.shift && input.key.toLowerCase() === 'w'
  const altLeft = input.alt && !input.control && !input.shift && input.key === 'ArrowLeft'
  const altRight = input.alt && !input.control && !input.shift && input.key === 'ArrowRight'
  if (!ctrlTab && !ctrlW && !altLeft && !altRight) return
  event.preventDefault()
  to.send(altRight || (ctrlTab && !input.shift) ? 'forward' : 'back')
}

/**
 * The app is a window onto a backend on one source (a file), which the person
 * chooses as they start: one already running on it, or one the app starts
 * itself and stops when it is done with the source. See backend.ts and src/backend.
 */
async function main(): Promise<void> {
  await app.whenReady()
  const win = new BrowserWindow({
    width: 1200,
    height: 800,
    title: 'YAWN',
    // A link's focus is a webview: the page in a tab of its own, not a frame in the app's page. Framed, a page is a
    // third party (its cookies refused, its own checks against being framed tripped), and Google's and GitHub's sign-in
    // fail there; in a webview it is top-level, as in a browser. Every guest is locked down below.
    webPreferences: { preload: join(__dirname, '../preload/index.js'), sandbox: false, webviewTag: true },
  })
  // The links' pages share one session, kept on disk (signing in once lasts), apart from the app's own. It says it is
  // Chrome, without Electron or the app in its user agent: Google refuses to sign in a browser it takes for embedded.
  const links = session.fromPartition(LINKS)
  links.setUserAgent(links.getUserAgent().replace(/ Electron\/\S+/, '').replace(new RegExp(` ${app.getName()}/\\S+`, 'i'), ''))
  // A guest is somebody else's page: no preload, no node, its own sandbox, only http(s), only in the links' session.
  win.webContents.on('will-attach-webview', (event, preferences, params) => {
    delete preferences.preload
    preferences.nodeIntegration = false
    preferences.contextIsolation = true
    preferences.sandbox = true
    if (!/^https?:/.test(params.src) || params.partition !== LINKS) event.preventDefault()
  })
  win.webContents.on('did-attach-webview', (_e, guest) => {
    // A sign-in's popup (window.open with a size: Google's, GitHub's) opens in a window of the app, in the same session,
    // so it can answer the page that opened it; any other new tab or window goes to the browser.
    guest.setWindowOpenHandler(({ url, disposition }) => {
      if (disposition === 'new-window' && /^https?:/.test(url))
        return { action: 'allow', overrideBrowserWindowOptions: { width: 520, height: 680, autoHideMenuBar: true, webPreferences: { sandbox: true, contextIsolation: true, nodeIntegration: false } } }
      if (/^https?:/.test(url)) void shell.openExternal(url)
      return { action: 'deny' }
    })
    // The view's keys still work while the page has the keyboard.
    guest.on('before-input-event', (event, input) => navKeys(win.webContents, event, input))
  })

  let current: Backend | null = null
  const watch = (opened: Backend): void => {
    opened.onChange(() => current === opened && !win.isDestroyed() && win.webContents.send('changed'))
    current = opened
  }
  /**
   * Opens a source, letting go of the one open before; answers what the window keeps as its source. A file path (made
   * if it is not there) opens a backend of its own; a URL (http/https) opens a remote one, reached by `token` — kept
   * here per URL (the window never receives it back), so a later open with no token reuses it. A refused token forgets
   * the kept one and is reported, so the window can ask for it again.
   */
  ipcMain.handle('openSource', async (_e, typed: string, token?: string) => {
    if (isRemote(typed)) {
      const url = typed.trim().replace(/\/$/, '')
      if (current?.file === url) return url
      await current?.close()
      current = null
      const using = token ?? readToken(url)
      if (using == null) throw new Error('No token for this server yet: paste the phone token.')
      let opened: Backend
      try {
        opened = await remote(url, using)
      } catch (e) {
        // A kept token the server now refuses: forget it, so the next open asks for one afresh.
        if (token === undefined && /token is refused/i.test((e as Error).message)) forgetToken(url)
        throw e
      }
      saveToken(url, using)
      watch(opened)
      console.log(`workflows: ${url}, remote backend`)
      return url
    }
    const file = sourceFile(typed)
    if (current?.file === file) return file
    await current?.close()
    current = null
    const opened = await backend(file)
    watch(opened)
    console.log(`workflows: ${file}, backend at ${opened.info.url} (${opened.owned ? 'started by the app' : 'already running'})`)
    return file
  })
  ipcMain.handle('closeSource', async () => {
    await current?.close()
    current = null
  })
  ipcMain.handle('rpc', (_e, method: string, args: unknown[], user: string | null) => {
    if (!current) throw new Error('Open a source first.')
    return current.call(method, args, user)
  })

  // A file item onto the clipboard as a file: its own path when it is a file on this machine, otherwise its bytes
  // written to the temporary folder first. On Linux as a URI list (what file managers and Chromium's paste read);
  // elsewhere as its path, as text.
  ipcMain.handle('copyFile', async (_e, itemId: string, user: string | null) => {
    if (!current) throw new Error('Open a source first.')
    // A remote source's path names nothing on this machine, so always take the file's bytes; a local one uses its path.
    let path: string | null = null
    if (!isRemote(current.file)) {
      const text = (await current.call('fileText', [itemId], user)) as { path: string | null; missing: boolean }
      path = text.path && !text.missing ? text.path : null
    }
    if (!path) {
      const { name, base64 } = (await current.call('fileData', [itemId], user)) as { name: string; base64: string }
      const dir = join(tmpdir(), 'yawn-clipboard', itemId)
      await mkdir(dir, { recursive: true })
      path = join(dir, basename(name) || 'file')
      await writeFile(path, Buffer.from(base64, 'base64'))
    }
    if (process.platform === 'linux') clipboard.writeBuffer('text/uri-list', Buffer.from(`${pathToFileURL(path).href}\r\n`))
    else clipboard.writeText(path)
  })

  win.webContents.setWindowOpenHandler(({ url }) => {
    if (/^https?:/.test(url)) void shell.openExternal(url)
    return { action: 'deny' }
  })
  win.webContents.on('before-input-event', (event, input) => navKeys(win.webContents, event, input))
  if (process.env.ELECTRON_RENDERER_URL) void win.loadURL(process.env.ELECTRON_RENDERER_URL)
  else void win.loadFile(join(__dirname, '../renderer/index.html'))

  // A backend the app started stops with it; one it found running goes on.
  let quitting = false
  app.on('before-quit', (e) => {
    if (quitting || !current?.owned) return
    e.preventDefault()
    quitting = true
    void current.close().finally(() => app.quit())
  })
  app.on('window-all-closed', () => app.quit())
}

void main().catch((e) => {
  console.error('workflows:', (e as Error).message)
  app.exit(1)
})
