// The two things the engine is built from: actions (what anyone can do) and
// event types (what can happen to an item, and what it can wait for).
// docs/events.md is the long form.

import { execFile } from 'node:child_process'
import type { z } from 'zod'
import type { EventKind, EventSpec, Item, ItemEvent } from '../shared/types'
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
  /** The human is running an approval's action: the approval's resolution records it, not an action event. */
  approving?: boolean
}

/**
 * One thing anyone can do: the same list for people and scripts, less the unsafe
 * ones for bots.
 *
 * RULE: an action that changes anything outside this app (posts, comments,
 * reviews, merges, sends, deploys, deletes) is `unsafe: true`. Bots, and scripts
 * a bot wrote, never get one: it is left out of their MCP tools, and
 * `Engine.callAction` refuses it. A bot asks for one as the approval on a human
 * approval event, and the human runs it. Every action must say which it is: `unsafe` is
 * required so that the choice is never left to a default.
 */
export interface Action {
  name: string
  description: string
  input: z.ZodRawShape
  unsafe: boolean
  run: (args: Record<string, unknown>, ctx: ActionContext) => Promise<unknown>
}

export interface EventContext {
  engine: Engine
  /** The item the event sits in; for an item event with no parent, the item itself. */
  item: Item
  event: ItemEvent
  /** What the event's last check handed on (a script's `state`), and how to hand something on to the next. */
  memo: unknown
  setMemo: (value: unknown) => void
  /** How many times it has been checked since the app started. */
  runs: number
  /** Whoever added this event, as an author. */
  author: Author
}

/**
 * One kind of event: everything that differs from one kind to the next. The
 * rest (who, when, resolved or not) is the same for all; see ItemEvent.
 *
 * A plain type (`blocking: false`) resolves as it is added. A blocker stays
 * open until `check` answers its data, or something resolves it from outside:
 * a bot's run ending, the human approving, a cancel.
 *
 * To add one: write it in eventTypes.ts, add its name to EventKind, give it an
 * icon in the renderer's icons.tsx, and a row in docs/events.md.
 */
export interface EventType {
  type: EventKind
  /** What it is for, for the bots (event_types) and the MCP page. */
  description: string
  /** What it takes. Checked as it is added. */
  params: z.ZodRawShape
  /** What it resolves with, in words, for the docs and the bots. */
  data: string
  blocking: boolean
  /** One line saying what it is, from its params: the row in the blockers panel, and how a prompt names it. */
  describe: (params: Record<string, unknown>) => string
  /**
   * What it says in the chat once it has resolved (not called when cancelled:
   * a cancel reads the same for every type). Null to say nothing, as a bot event
   * does, whose reply is a message event of its own. Left out, it says `describe`.
   */
  said?: (params: Record<string, unknown>, data: Record<string, unknown>) => string | null
  /** A blocker the human resolves (an approval): while only these are open, the item is still the human's. */
  human?: boolean
  /** May a bot add it, in its output? */
  bots?: boolean
  /** Turns one event into those actually added. */
  expand?: (params: Record<string, unknown>, author: Author) => EventSpec[]
  /** As it is added. It may change its params, or resolve it at once. */
  start?: (
    params: Record<string, unknown>,
    ctx: EventContext,
  ) => Promise<{ params?: Record<string, unknown>; resolve?: Record<string, unknown> } | void>
  /** Polled by the loop on its schedule: the data once it has happened, null while it hasn't. */
  check?: (params: Record<string, unknown>, ctx: EventContext) => Promise<Record<string, unknown> | null>
  /**
   * When `check` runs, as seconds to wait: before the first check, then between
   * each, the last repeating. Left out, the default backoff: at once, then 1, 2,
   * 5 and then every 10 minutes. Checks run at most once a second, and one check
   * of an event never starts before the last has finished.
   */
  schedule?: (params: Record<string, unknown>) => number[]
}

/** Where new items come from. Polled from the central loop; what it answers is added in no parent. */
export interface Source {
  id: string
  everyMinutes: number
  /** Item events to add, each with a `key` so that polling again adds none twice. */
  poll: (engine: Engine) => Promise<EventSpec[]>
}
