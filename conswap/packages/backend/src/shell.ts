import { spawn } from 'node:child_process'

export interface ShellResult {
  code: number
  stdout: string
  stderr: string
  timedOut: boolean
}

export interface ShellOptions {
  cwd?: string
  input?: string
  timeoutMs?: number
  env?: Record<string, string>
}

/** Runs a command without a shell, so nothing in a topic can be interpolated into one. */
export function run(command: string, args: string[], options: ShellOptions = {}): Promise<ShellResult> {
  return new Promise((resolve) => {
    const child = spawn(command, args, {
      cwd: options.cwd,
      env: { ...process.env, ...options.env },
    })
    let stdout = ''
    let stderr = ''
    let timedOut = false
    const timeout = options.timeoutMs
      ? setTimeout(() => {
          timedOut = true
          child.kill('SIGKILL')
        }, options.timeoutMs)
      : null

    child.stdout.on('data', (chunk: Buffer) => {
      stdout += chunk.toString()
    })
    child.stderr.on('data', (chunk: Buffer) => {
      stderr += chunk.toString()
    })
    child.on('error', (error) => {
      if (timeout) clearTimeout(timeout)
      resolve({ code: -1, stdout, stderr: `${stderr}${String(error)}`, timedOut })
    })
    child.on('close', (code) => {
      if (timeout) clearTimeout(timeout)
      resolve({ code: code ?? -1, stdout, stderr, timedOut })
    })

    if (options.input !== undefined) child.stdin.write(options.input)
    child.stdin.end()
  })
}

/** A command that is expected to print JSON, and whose failure is not interesting. */
export async function runJson<T>(command: string, args: string[], options: ShellOptions = {}): Promise<T | null> {
  const result = await run(command, args, options)
  if (result.code !== 0) return null
  try {
    return JSON.parse(result.stdout) as T
  } catch {
    return null
  }
}
