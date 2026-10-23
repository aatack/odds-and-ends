// The two things the engine is built from: actions (what anyone can do) and
// handoff types (what an item can wait for). docs/handoffs.md is the long form.

import { execFile } from 'node:child_process'
import type { z } from 'zod'
import type { Handoff, HandoffKind, HandoffSpec, Item } from '../shared/types'
import type { Engine } from './engine'

/** Runs a CLI and answers its stdout. Handed in, so the tests can stand in for gh. */
export type Exec = (cmd: string, args: string[]) => Promise<string>

export const execCli: Exec = (cmd, args) =>
  new Promise((resolve, reject) => {
    execFile(cmd, args, { maxBuffer: 32 * 1024 * 1024 }, (err, stdout, stderr) => {
      if (err) reject(new Error(`${cmd} ${args[0] ?? ''} failed: ${(stderr || err.message).trim()}`))
      else resolve(stdout)
    })
  })

/**
 * Who is ultimately behind a call. A script is its author's: one a bot wrote
 * may do no more than the bot could.
 */
export type Author = 'human' | 'bot'

export interface ActionContext {
  engine: Engine
  author: Author
  /** What the history records as having done it: 'human', 'bot', 'script', 'mcp'. */
  actor: string
  /** The item the caller is working on. A bot, or a bot's script, may change no other. */
  itemId: string | null
  /** The bot run making the call, if one is. */
  runId: string | null
}

/**
 * One thing anyone can do: the same list for people and scripts, less the unsafe
 * ones for bots.
 *
 * RULE: an action that changes anything outside this app (posts, comments,
 * reviews, merges, sends, deploys, deletes) is `unsafe: true`. Bots, and scripts
 * a bot wrote, never get one: it is left out of their MCP tools, and
 * `Engine.callAction` refuses it. A bot asks for one as the approval on a human
 * handoff, and the human runs it. Every action must say which it is: `unsafe` is
 * required so that the choice is never left to a default.
 */
export interface Action {
  name: string
  description: string
  input: z.ZodRawShape
  unsafe: boolean
  run: (args: Record<string, unknown>, ctx: ActionContext) => Promise<unknown>
}

export interface HandoffContext {
  engine: Engine
  item: Item
  handoff: Handoff
  /** The handoff resolved just before this one, with its resolution. */
  previous: Handoff | null
  /** Whoever set this handoff, as an author. */
  author: Author
}

/**
 * One kind of handoff. Human and bot handoffs have none of the hooks: they
 * resolve when the human or the bot hands the item on. A wait has `check`;
 * an instant one, like a script, has `run`.
 */
export interface HandoffType {
  type: HandoffKind
  description: string
  params: z.ZodRawShape
  /** What its resolution holds, for the docs and the bots. */
  resolution: string
  /**
   * Turns one handoff into the ones actually queued, as it is set: so an
   * bot's Slack message gets a human approval put in front of it.
   */
  expand?: (params: Record<string, unknown>, author: Author) => HandoffSpec[]
  /** As it becomes active. What it answers is merged into its params. */
  start?: (params: Record<string, unknown>, ctx: HandoffContext) => Promise<Record<string, unknown> | void>
  /** Polled by the loop, with backoff: the resolution once done, null while still waiting. */
  check?: (params: Record<string, unknown>, ctx: HandoffContext) => Promise<Record<string, unknown> | null>
  /** Resolves at once, answering what goes at the front of the queue. */
  run?: (
    params: Record<string, unknown>,
    ctx: HandoffContext,
  ) => Promise<{ resolution: Record<string, unknown>; next: HandoffSpec[] }>
}

/** Where new items come from. Polled from the central loop. */
export interface Source {
  id: string
  everyMinutes: number
  poll: (
    engine: Engine,
  ) => Promise<{ ref: string; title: string; url: string | null; state: string; next: HandoffSpec[] }[]>
}
