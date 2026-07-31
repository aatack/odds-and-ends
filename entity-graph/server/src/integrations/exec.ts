import { execFile, type ExecFileException } from 'child_process'

// Running other people's programs. Everything here takes an argument *vector*
// and never a command line, so no shell is involved and nothing a tool is handed
// can be read as syntax — a pull request title containing `;` is just a title.

/** Output beyond this is a runaway, not a result. */
const MAX_OUTPUT = 16 * 1024 * 1024

/** Long enough for a CLI call, which is what almost all of these are. */
const TIMEOUT_MS = 60_000

export interface CommandResult {
  exitCode: number
  stdout: string
  stderr: string
}

export interface RunOptions {
  /** Where to run it. Defaults to the server's own working directory. */
  cwd?: string
  /** How long to let it run before killing it. */
  timeoutMs?: number
  /**
   * Written to the program's standard input, which is then closed. This is how
   * anything long is handed over — an argument vector has a length ceiling and
   * shows up in `ps`, and a prompt has neither business.
   */
  stdin?: string
}

/** A duration as the error will read it: seconds while they're few, else minutes. */
const spell = (ms: number): string =>
  ms < 120_000 ? `${Math.round(ms / 1000)}s` : `${Math.round(ms / 60_000)} minutes`

/** Run a program to completion. A non-zero exit is a result, not a throw. */
export function run(
  command: string,
  args: string[],
  options: RunOptions = {},
): Promise<CommandResult> {
  const timeout = options.timeoutMs ?? TIMEOUT_MS
  return new Promise((resolve, reject) => {
    const child = execFile(
      command,
      args,
      { maxBuffer: MAX_OUTPUT, timeout, encoding: 'utf8', cwd: options.cwd },
      (error: ExecFileException | null, stdout, stderr) => {
        if (error?.code === 'ENOENT') {
          reject(new Error(`\`${command}\` is not installed, or not on this server's PATH`))
          return
        }
        if (error?.killed) {
          reject(new Error(`\`${command}\` took longer than ${spell(timeout)} and was killed`))
          return
        }
        const exitCode = typeof error?.code === 'number' ? error.code : error ? 1 : 0
        resolve({ exitCode, stdout, stderr })
      },
    )
    if (options.stdin !== undefined) {
      // A program that exits without reading its input breaks the pipe, which is
      // its business and not an error of ours — the exit code says what happened.
      child.stdin?.on('error', () => undefined)
      child.stdin?.end(options.stdin)
    }
  })
}

/**
 * Run a program, and on failure raise its own complaint. CLIs say why they
 * failed far better than we could — `gh`'s "could not resolve to a Repository"
 * beats "exit code 1".
 */
export async function ok(
  command: string,
  args: string[],
  options: RunOptions = {},
): Promise<CommandResult> {
  const result = await run(command, args, options)
  if (result.exitCode !== 0) {
    const said = result.stderr.trim() || result.stdout.trim()
    throw new Error(said || `\`${command}\` exited with code ${result.exitCode}`)
  }
  return result
}

/** Run a program that was asked for JSON, and hand back what it said. */
export async function json<T>(
  command: string,
  args: string[],
  options: RunOptions = {},
): Promise<T> {
  const { stdout } = await ok(command, args, options)
  try {
    return JSON.parse(stdout) as T
  } catch {
    throw new Error(`\`${command}\` did not return JSON: ${stdout.slice(0, 300)}`)
  }
}

/**
 * What a command that only *did* something has to say for itself. `gh` writes
 * its confirmations to stderr, so both streams count.
 */
export const said = (result: CommandResult): string =>
  [result.stdout.trim(), result.stderr.trim()].filter(Boolean).join('\n')
