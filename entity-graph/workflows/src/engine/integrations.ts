// The two things the engine is built from: actions (what anyone can do) and
// item types (what can happen to an item, and what it can wait for).
// docs/items.md is the long form.

import { execFile } from 'node:child_process'
import type { z } from 'zod'
import type { ItemKind, ItemSpec, Item, ItemInfo } from '../shared/types'
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
  /** The human is running an approval's action: the approval's resolution records it, not an action item. */
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
 * approval item, and the human runs it. Every action must say which it is: `unsafe` is
 * required so that the choice is never left to a default.
 */
export interface Action {
  name: string
  description: string
  input: z.ZodRawShape
  unsafe: boolean
  run: (args: Record<string, unknown>, ctx: ActionContext) => Promise<unknown>
}

/**
 * One kind of item: everything that differs from one kind to the next. The
 * rest (who, when, resolved or not) is the same for all; see ItemInfo.
 *
 * A plain type (`blocking: false`) resolves as it is added. A blocker stays
 * open until `check` answers its data, or something resolves it from outside:
 * a bot's run ending, the human approving, a cancel.
 *
 * To add one: write it in itemTypes.ts, add its name to ItemKind, give it an
 * icon in the renderer's icons.tsx, and a row in docs/items.md.
 */
export interface ItemType {
  type: ItemKind
  /** What it is for, for the bots (item_types) and the MCP page. */
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
   * a cancel reads the same for every type). Null to say nothing, as a bot item
   * does, whose reply is a message item of its own. Left out, it says `describe`.
   */
  said?: (params: Record<string, unknown>, data: Record<string, unknown>) => string | null
  /** A blocker the human resolves (an approval): while only these are open, the item is still the human's. */
  human?: boolean
  /** May a bot add it, in its output? */
  bots?: boolean
  /** Turns one item into those actually added. */
  expand?: (params: Record<string, unknown>, author: Author) => ItemSpec[]
}

/**
 * One integration, in the shape every integration takes (AGENTS.md, Integrations): how it is set up, the tools it
 * gives (each a read, or a write: `unsafe`), and the triggers it can fire. The Integrations page draws each from this
 * alone, in four sections: setup, read, write, triggers.
 */
export interface Integration {
  id: string
  name: string
  /** What it is for, in a line or two (markdown). */
  description: string
  /** How it is signed in: written once and reused (a CLI's own login, a token in its setup, or OAuth), and how to do it. */
  auth: { kind: 'cli' | 'token' | 'oauth'; instructions: string }
  /** What is put in to set it up: each value its own field, with how to get it beside it. */
  setup: SetupField[]
  /** Its tools: thin wrappers round its calls. A write is `unsafe`: only a person runs it. */
  tools: Action[]
  /** What it can tell the app has happened. A script polling its read tools stands in until one is built. */
  triggers: { name: string; description: string }[]
  /** How it stands now, in a line (signed in as whom); throws when it can't be used. */
  status?: (engine: Engine) => Promise<string>
}

/** A value an integration is set up with: kept as a setting (a secret encrypted), shown unless it is a secret. */
export interface SetupField {
  key: string
  label: string
  /** How to get it, shown beside it (markdown). */
  instructions: string
  secret?: boolean
}

/** Where new items come from. Polled from the central loop; what it answers is added in no parent. */
export interface Source {
  id: string
  everyMinutes: number
  /** Task items to add, each with a `key` so that polling again adds none twice. */
  poll: (engine: Engine) => Promise<ItemSpec[]>
}
