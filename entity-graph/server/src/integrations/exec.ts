import { execFile, type ExecFileException } from 'child_process'

// Running other people's programs. Everything here takes an argument *vector*
// and never a command line, so no shell is involved and nothing a tool is handed
// can be read as syntax — a pull request title containing `;` is just a title.

/** Output beyond this is a runaway, not a result. */
const MAX_OUTPUT = 16 * 1024 * 1024

const TIMEOUT_MS = 60_000

export interface CommandResult {
  exitCode: number
  stdout: string
  stderr: string
}

/** Run a program to completion. A non-zero exit is a result, not a throw. */
export function run(command: string, args: string[]): Promise<CommandResult> {
  return new Promise((resolve, reject) => {
    execFile(
      command,
      args,
      { maxBuffer: MAX_OUTPUT, timeout: TIMEOUT_MS, encoding: 'utf8' },
      (error: ExecFileException | null, stdout, stderr) => {
        if (error?.code === 'ENOENT') {
          reject(new Error(`\`${command}\` is not installed, or not on this server's PATH`))
          return
        }
        if (error?.killed) {
          reject(new Error(`\`${command}\` took longer than ${TIMEOUT_MS / 1000}s and was killed`))
          return
        }
        const exitCode = typeof error?.code === 'number' ? error.code : error ? 1 : 0
        resolve({ exitCode, stdout, stderr })
      },
    )
  })
}

/**
 * Run a program, and on failure raise its own complaint. CLIs say why they
 * failed far better than we could — `gh`'s "could not resolve to a Repository"
 * beats "exit code 1".
 */
export async function ok(command: string, args: string[]): Promise<CommandResult> {
  const result = await run(command, args)
  if (result.exitCode !== 0) {
    const said = result.stderr.trim() || result.stdout.trim()
    throw new Error(said || `\`${command}\` exited with code ${result.exitCode}`)
  }
  return result
}

/** Run a program that was asked for JSON, and hand back what it said. */
export async function json<T>(command: string, args: string[]): Promise<T> {
  const { stdout } = await ok(command, args)
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
