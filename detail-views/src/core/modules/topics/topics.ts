import { randomUUID } from 'node:crypto'
import { mkdirSync, mkdtempSync, writeFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { basename, join } from 'node:path'
import { link, value, values, type AppEvent } from '../../graph/events.ts'
import { prEntityId, type Entity } from '../../types.ts'
import { ask, author as claudeAuthor } from '../claude/claude.ts'
import type { Module, ModuleContext } from '../module.ts'
import { me } from '../tasks/tasks.ts'
import { branchOfUrl } from '../github/view.ts'
import { messageOfUrl } from '../slack/view.ts'
import { answering, promptPart, topicsIds as ids, topicsView, unsent } from './view.ts'

/** A file pasted into a topic: its name, its type, and what is in it, base64. */
export interface PastedFile {
  name: string
  mime: string
  data: string
}

/**
 * Topics, each a chat I write into as I go: a message is only a note under
 * it, until a dispatch sends every message since the last one to Claude at
 * once, in the topic's directory.
 */
export class Topics implements Module {
  readonly view = topicsView

  private readonly context: ModuleContext
  /** One dispatch at a time per topic, in the order made. */
  private readonly queues = new Map<string, Promise<unknown>>()

  constructor(context: ModuleContext) {
    this.context = context
  }

  /** A new topic, named, under the topics root. */
  create(name: string): AppEvent[] {
    const text = name.trim()
    if (!text) throw new Error('A topic needs a name')
    const id = randomUUID()
    const at = this.context.now()
    const events = [...values(id, { type: 'topic', text }, at, me), link(ids.root, id, at, me)]
    this.context.owned.write(events)
    return events
  }

  /** The box under a topic: a message in it (a note), sent to nobody yet. */
  async submit(entity: Entity, text: string): Promise<AppEvent[]> {
    if (entity.type !== 'topic') return []
    const events = this.message(entity.id, text, this.context.now())
    this.context.owned.write(events)
    return events
  }

  /**
   * Text pasted into a topic, as what it is: a link to a PR, a Slack message
   * or a GitHub branch is that item, linked under the topic; anything else
   * (another link too) is a message of its text.
   */
  paste(topic: string, text: string): AppEvent[] {
    if (this.context.lens.read(topic)?.type !== 'topic') throw new Error('Not a topic')
    const trimmed = text.trim()
    if (!trimmed) return []
    const at = this.context.now()
    const item = /^https?:\/\/\S+$/.test(trimmed) ? (prEntityId(trimmed) ?? messageOfUrl(trimmed) ?? branchOfUrl(trimmed)) : null
    const events = item ? [link(topic, item, at, me)] : this.message(topic, trimmed, at)
    this.context.owned.write(events)
    return events
  }

  /** A file pasted into a topic: kept under the app's data directory, as a file item under the topic. */
  attach(topic: string, file: PastedFile): AppEvent[] {
    if (this.context.lens.read(topic)?.type !== 'topic') throw new Error('Not a topic')
    const id = randomUUID()
    const name = basename(file.name) || 'pasted'
    const dir = join(this.context.dataDir, 'files', id)
    mkdirSync(dir, { recursive: true })
    const path = join(dir, name)
    const bytes = Buffer.from(file.data, 'base64')
    writeFileSync(path, bytes)
    const at = this.context.now()
    const events = [...values(id, { type: 'file', text: name, path, mime: file.mime, size: bytes.length }, at, me), link(topic, id, at, me)]
    this.context.owned.write(events)
    return events
  }

  private message(topic: string, text: string, at: number): AppEvent[] {
    const id = randomUUID()
    return [value(id, 'text', text, at, me), link(topic, id, at, me)]
  }

  /**
   * Sends every message since the last dispatch to Claude as one prompt, and
   * puts a response after them for the answer to fill. It runs in the
   * topic's `cwd`; a topic without one is given a new temporary directory,
   * kept, so later dispatches resume the conversation there. Refused while
   * Claude is still answering the one before.
   */
  dispatch(topic: string): AppEvent[] {
    const lens = this.context.lens
    if (lens.read(topic)?.type !== 'topic') throw new Error('Not a topic')
    // Never two prompts at once: the messages wait for the next dispatch.
    if (answering(topic, lens)) throw new Error('Claude is still answering')
    const messages = unsent(topic, lens)
    if (!messages.length) throw new Error('Nothing new to send')
    const prompt = messages.map((id) => promptPart(id, lens)).join('\n\n')
    const at = this.context.now()
    const responseId = randomUUID()
    const cwd = this.context.data.get(topic).cwd
    const events: AppEvent[] = [
      // Claude's, like `started`: undoing my last edit doesn't take the conversation's directory away.
      ...(typeof cwd === 'string' && cwd ? [] : [value(topic, 'cwd', mkdtempSync(join(tmpdir(), 'claude-')), at, claudeAuthor)]),
      ...values(responseId, { type: 'claude.response', text: '', running: true }, at, claudeAuthor),
      // A moment later, so it sorts after the last message even when made the same instant.
      link(topic, responseId, at + 1, claudeAuthor),
    ]
    this.context.owned.write(events)
    const before = this.queues.get(topic) ?? Promise.resolve()
    const next = before.then(() => ask(this.context, topic, responseId, prompt))
    this.queues.set(topic, next.catch(() => {}))
    return events
  }
}
