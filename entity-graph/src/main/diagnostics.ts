import { app, powerMonitor, type BrowserWindow } from 'electron'
import { appendFileSync, mkdirSync } from 'fs'
import { join } from 'path'

// A record of what happened to the window, for the failures that leave nothing
// else behind: a white screen after the lid is shut shows nothing on screen, and
// in development the main process's output is only in a terminal and the
// renderer's console nowhere at all. Each line is one event, timestamped, in
// `userData/logs/diagnostics.log`.
//
// What it records is the set of things that could each produce a blank window:
// the machine sleeping and waking, a renderer or GPU process dying, the page
// navigating or reloading (Vite's client reloads it when its socket drops),
// a load failing, the window hanging, and every warning and error the page logs
// — an uncaught render error unmounts React and logs exactly one of those.

let file: string | null = null

export function logDiagnostic(event: string, detail?: unknown): void {
  try {
    if (!file) {
      const dir = join(app.getPath('userData'), 'logs')
      mkdirSync(dir, { recursive: true })
      file = join(dir, 'diagnostics.log')
    }
    const extra = detail === undefined ? '' : ` ${JSON.stringify(detail)}`
    appendFileSync(file, `${new Date().toISOString()} ${event}${extra}\n`)
  } catch {
    // A log that cannot be written must not take the app down with it.
  }
}

/** The machine and its processes. Once, at startup. */
export function watchApp(): void {
  logDiagnostic('app.ready', { version: app.getVersion(), electron: process.versions.electron })
  for (const name of ['suspend', 'resume', 'lock-screen', 'unlock-screen'] as const) {
    powerMonitor.on(name as 'suspend', () => logDiagnostic(`power.${name}`))
  }
  app.on('child-process-gone', (_e, details) => logDiagnostic('child-process-gone', details))
}

/** One window: what its page did, and what became of it. */
export function watchWindow(win: BrowserWindow): void {
  const page = win.webContents
  page.on('render-process-gone', (_e, details) => logDiagnostic('render-process-gone', details))
  page.on('unresponsive', () => logDiagnostic('window.unresponsive'))
  page.on('responsive', () => logDiagnostic('window.responsive'))
  page.on('did-start-navigation', (details) => {
    if (details.isMainFrame) logDiagnostic('page.navigate', { url: details.url, sameDocument: details.isSameDocument })
  })
  page.on('did-finish-load', () => logDiagnostic('page.loaded'))
  page.on('did-fail-load', (_e, code, description, url, isMainFrame) => {
    if (isMainFrame) logDiagnostic('page.load-failed', { code, description, url })
  })
  page.on('console-message', (_e, level, message, line, source) => {
    // Warnings and errors, and anything Vite's client says about its socket.
    if (level >= 2 || message.startsWith('[vite]')) {
      logDiagnostic('page.console', { level, message, source: `${source}:${line}` })
    }
  })
}
