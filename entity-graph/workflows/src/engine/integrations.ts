import { execFile } from 'node:child_process'
import type { z } from 'zod'
import type { Field } from '../shared/types'

/** Runs a CLI and answers its stdout. Handed in, so the tests can stand in for gh. */
export type Exec = (cmd: string, args: string[]) => Promise<string>

export const execCli: Exec = (cmd, args) =>
  new Promise((resolve, reject) => {
    execFile(cmd, args, { maxBuffer: 32 * 1024 * 1024 }, (err, stdout, stderr) => {
      if (err) reject(new Error(`${cmd} ${args[0] ?? ''} failed: ${(stderr || err.message).trim()}`))
      else resolve(stdout)
    })
  })

/** A read an agent may make at any time. */
export interface ReadTool {
  name: string
  description: string
  input: z.ZodRawShape
  run: (args: Record<string, unknown>, exec: Exec) => Promise<string>
}

/**
 * Something other people can see. An agent never runs one: it asks for one, the
 * operator sees the form filled in, and runs it, changes it, or does something else.
 */
export interface Action {
  id: string
  label: string
  description: string
  fields: Field[]
  run: (inputs: Record<string, string>, exec: Exec) => Promise<string>
}

/** Something outside the tool that an item can wait for. */
export interface WaitKind {
  id: string
  description: string
  input: z.ZodRawShape
  describe: (params: Record<string, unknown>) => string
  /** Runs once as the wait starts; what it answers is kept with the params. */
  start?: (params: Record<string, unknown>, exec: Exec) => Promise<Record<string, unknown>>
  /** What happened, once it has; null while still waiting. */
  check: (params: Record<string, unknown>, exec: Exec) => Promise<string | null>
}

/** Where new items come from. Polled from the central loop. */
export interface Source {
  id: string
  everyMinutes: number
  poll: (exec: Exec) => Promise<{ ref: string; title: string; url: string | null; state: string }[]>
}
