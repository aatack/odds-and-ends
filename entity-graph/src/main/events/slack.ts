import { LogLevel, SocketModeClient } from '@slack/socket-mode'
import { bucketEvents, rollupEntity } from '../../core/entity'
import {
  conversationInfo,
  kindOf,
  messageAt,
  searchPage,
  whoHolds,
  SEARCH_PAGES,
  type Conversation,
  type SearchMatch,
  type SlackMessage,
  type SlackWorkspace,
} from '../integrations/slack'
import { EntityWriter, type EntityDraft } from './writer'
import { Feed, type FeedOptions } from './feed'

// Slack, read into the store. Two ways in and one way out: a search that walks
// backwards from a cursor, and — when there is an app-level token — a WebSocket
// that hands over the same messages seconds after they are sent. Both make the
// same entities, and the entity ids are what lets them overlap freely: the
// socket delivering a message the search then finds again writes nothing the
// second time.
//
// **Search is the one call that covers everywhere.** `conversations.history` is
// one channel's top-level messages and nothing else, so a feed built on it would
// be a call per conversation and would still miss every thread reply.
// `search.messages` with no search text at all — modifiers only, sorted by time
// — is "everything, newest first", across channels, DMs and threads alike. It
// needs a user token; a bot token cannot search under any scope.

/** Slack's search bound is a date, so a window is at worst a day too wide. */
const DAY = 86_400
const WEEK = 7 * DAY

/**
 * How far the cursor is wound back before each request. Search runs off an index
 * and an index lags; a minute of overlap costs nothing, because reading the same
 * message twice writes nothing the second time.
 */
const OVERLAP = 60

/** How often the search runs. Tier 2 allows twenty a minute; this wants one. */
const POLL_MS = 60_000

/** `YYYY-MM-DD`, which is the only granularity `after:` and `before:` have. */
const day = (seconds: number): string => new Date(seconds * 1000).toISOString().slice(0, 10)

/** A Slack `ts` as a number of seconds, or null for anything that isn't one. */
const asSeconds = (ts: string | undefined | null): number | null => {
  const n = Number(ts)
  return Number.isFinite(n) && n > 0 ? n : null
}

/**
 * The entity one message is: its permalink, with the query string taken off.
 *
 * The permalink *is* the message's own id — it names the workspace, the channel
 * and the timestamp all at once, it is what Slack's own "Copy link" gives you,
 * and it is a thing that can be clicked. What is stripped is the
 * `?thread_ts=…&cid=…` a reply's link carries: whether we happened to learn about
 * a message *as a reply* must not change which entity it is, or the same message
 * read two ways would be two notes.
 */
export function messageId(permalink: string): string {
  try {
    const url = new URL(permalink)
    return `${url.origin}${url.pathname}`
  } catch {
    return permalink
  }
}

/**
 * The thread a search hit belongs to, or null. A match carries no `thread_ts`
 * field — it is not a message, it is a hit — but its permalink ends
 * `?thread_ts=…` when it is a reply, which is the only place the fact survives.
 */
export function threadOf(permalink: string | undefined | null): string | null {
  if (!permalink) return null
  try {
    return new URL(permalink).searchParams.get('thread_ts')
  } catch {
    return null
  }
}

/**
 * A permalink, built rather than asked for. Slack's own form is the workspace
 * URL, the channel and the timestamp with its dot taken out — so a message that
 * arrived over the socket, which carries no permalink, costs no call to name.
 *
 * Built for every message rather than taken from a search hit when there is one,
 * because this is what an entity is *identified* by: one way of arriving at the
 * string means a message found twice is one note, where two ways that usually
 * agree would be two notes on the day they didn't.
 */
export function permalinkFor(
  workspace: string,
  channel: string,
  ts: string,
  threadTs?: string | null,
): string {
  const base = `${workspace.replace(/\/$/, '')}/archives/${channel}/p${ts.replace('.', '')}`
  return threadTs && threadTs !== ts ? `${base}?thread_ts=${threadTs}&cid=${channel}` : base
}

/** What to call a conversation: a channel wears a `#`, somebody wears an `@`. */
const conversationName = (c: Conversation): string => {
  const kind = kindOf(c)
  if (!c.name) return c.id
  return kind === 'dm' ? `@${c.name}` : `#${c.name}`
}

/** A message as the socket delivers one; the fields this cares about. */
interface SocketMessage {
  type?: string
  subtype?: string
  channel?: string
  ts?: string
  thread_ts?: string
  user?: string
  bot_id?: string
  text?: string
  deleted_ts?: string
  message?: SocketMessage
  previous_message?: SocketMessage
  item?: { type?: string; channel?: string; ts?: string }
  reaction?: string
}

export interface SlackFeedConfig {
  userToken: string
  appToken: string
  cursor: string
  muted: string
}

/**
 * One `slackEvents` node, running.
 *
 * The order at start is the whole of why a restart is safe. The socket is
 * connected *first* and everything it delivers is held; then the catch-up runs
 * from the cursor; then the held events are written. Connected afterwards
 * instead, the gap between the last page of the catch-up and the socket coming
 * up would be a hole — short, and silent, which is worse.
 */
export class SlackFeed extends Feed<SlackFeedConfig> {
  private socket: SocketModeClient | null = null
  private held: SocketMessage[] | null = null
  private workspace: SlackWorkspace | null = null
  /** Conversations already described, so a busy channel is looked up once. */
  private conversations = new Map<string, Conversation>()
  /** Thread parents already read, so a busy thread is fetched once. */
  private parents = new Map<string, EntityDraft>()

  constructor(options: FeedOptions<SlackFeedConfig>) {
    super(options, POLL_MS)
  }

  protected async begin(): Promise<void> {
    const { userToken } = this.config()
    if (!userToken.trim()) throw new Error('No user token — this node has nothing to read with')
    this.workspace = await whoHolds(userToken)
    // Every entity here is named by its permalink, and a permalink begins with
    // the workspace's own address. Without one there is nothing to call anything.
    if (!this.workspace.url) throw new Error('Slack did not say which workspace this token is for')
    await this.connect()
  }

  protected async end(): Promise<void> {
    const socket = this.socket
    this.socket = null
    this.held = null
    this.parents.clear()
    await socket?.disconnect().catch(() => undefined)
  }

  // --- Socket Mode ---------------------------------------------------------

  /**
   * The WebSocket, when there is an app-level token for one. It goes *out* from
   * this machine, so nothing here listens and no endpoint is exposed; the
   * library owns the reconnection and the acknowledgements.
   */
  private async connect(): Promise<void> {
    const { appToken } = this.config()
    if (!appToken.trim()) return
    // Quiet: the library narrates every ping at the default level, and there is
    // nobody reading this app's stdout.
    const socket = new SocketModeClient({ appToken, logLevel: LogLevel.ERROR })
    socket.on('slack_event', async (payload: { ack: () => Promise<void>; body?: unknown }) => {
      // Acknowledged first and always: Slack redelivers what it is not told
      // about, and an event this cannot make sense of is still received.
      await payload.ack().catch(() => undefined)
      const body = payload.body as { event?: SocketMessage } | undefined
      if (body?.event) void this.deliver(body.event)
    })
    this.socket = socket
    // Held from the moment it is connected, and released by the first catch-up.
    this.held = []
    await socket.start()
  }

  /** One socket event: held while the catch-up runs, written otherwise. */
  private async deliver(event: SocketMessage): Promise<void> {
    if (this.held) {
      this.held.push(event)
      return
    }
    await this.apply([event]).catch((e) => this.failed(e))
  }

  // --- The poll ------------------------------------------------------------

  /**
   * One pass: everything since the cursor, then the cursor moved. That order is
   * the rule the whole design rests on — a crash between the two reads the same
   * stretch again next time, which is free, where the other order loses it.
   */
  protected async pass(): Promise<void> {
    const { userToken, cursor } = this.config()
    const now = Date.now() / 1000
    // A node is given its cursor when it is added, so an empty one means a node
    // from before that. Now rather than yesterday: a feed switched on today is
    // asking to be told what happens next, not to import what already did.
    const from = asSeconds(cursor) ?? now
    // Wound back before every request, not only the first: a message indexed a
    // few seconds late is otherwise behind the cursor by the time it appears.
    const floor = from - OVERLAP
    // A cursor left behind for a week is not caught up in one search. Stepping a
    // day at a time keeps each request the size of a day whatever it is reading.
    const ceiling = now - floor > WEEK ? floor + DAY : null

    const query = [
      // Both bounds are dates, and exclusive of the day they name, so each is
      // given a day of room and the timestamps are compared properly below.
      `after:${day(floor - DAY)}`,
      ...(ceiling ? [`before:${day(ceiling + DAY)}`] : []),
    ].join(' ')

    const matches: SearchMatch[] = []
    for (let page = 1; page <= SEARCH_PAGES && this.running; page++) {
      const found = await searchPage(userToken, query, page)
      if (!found.matches.length) break
      matches.push(...found.matches)
      // Newest first, so the page's last hit says whether the walk has gone past
      // the cursor. Read to there rather than to a count: how many messages a
      // day holds is not something to guess at.
      const oldest = Math.min(...found.matches.map((m) => asSeconds(m.ts) ?? Infinity))
      if (oldest < floor) break
      if (page >= found.pages) break
    }

    const kept = matches.filter((m) => {
      const ts = asSeconds(m.ts)
      return ts !== null && ts >= floor && (ceiling === null || ts < ceiling)
    })
    await this.write(await this.fromMatches(kept))

    this.advance({ cursor: (ceiling ?? now).toFixed(6) })
    this.say(
      ceiling
        ? `Catching up — read to ${day(ceiling)}`
        : `Up to date${kept.length ? `, ${kept.length} in the last minute` : ''}`,
    )
    // Still behind: come straight back rather than waiting out the poll.
    if (ceiling && this.running) return this.pass()

    // The catch-up is complete, so whatever the socket held while it ran can go
    // in — after it, and in the order it arrived.
    const held = this.held
    if (held) {
      this.held = null
      await this.apply(held)
    }
  }

  // --- Turning what arrived into entities -----------------------------------

  /** Conversation ids this node is told never to write. */
  private muted(): Set<string> {
    return new Set(
      this.config()
        .muted.split(',')
        .map((s) => s.trim())
        .filter(Boolean),
    )
  }

  /**
   * The conversation an id names. Search hands one over with the hit, so this is
   * only reached for what came off the socket, which names the channel and
   * nothing else. Remembered: a workspace's channels do not get renamed often.
   */
  private async conversation(channel: string): Promise<Conversation> {
    const known = this.conversations.get(channel)
    if (known) return known
    const found = await conversationInfo(this.config().userToken, channel).catch(() => ({
      id: channel,
    }))
    this.conversations.set(channel, found)
    return found
  }

  /**
   * The channel a message is in, as an entity. Made when the first message in it
   * arrives rather than by listing conversations: a channel nothing has been
   * said in is not news.
   */
  private channelDraft(c: Conversation): EntityDraft {
    this.conversations.set(c.id, c)
    return {
      id: c.id,
      values: {
        // The `type` is what makes a row read as the thing it is rather than as
        // a plain bullet, and is what a type note under `@types` would describe
        // if the user writes one. Nothing here depends on that note existing.
        type: 'slack/channel',
        text: conversationName(c),
        'slack/channel': c.id,
        'slack/kind': kindOf(c),
      },
    }
  }

  /** A message's permalink, which is also the id of the entity it becomes. */
  private link(channel: string, ts: string, threadTs?: string | null): string {
    return permalinkFor(this.workspace?.url ?? '', channel, ts, threadTs)
  }

  private idFor(channel: string, ts: string): string {
    return messageId(this.link(channel, ts))
  }

  /**
   * One message, as the note it becomes.
   *
   * Neither the channel nor the thread is written down. The channel is in the
   * permalink and the message is linked under the channel's own note; the thread
   * is the note it hangs off. A value saying either again is a second copy to
   * keep in step with the first.
   *
   * `text` is left out when it isn't known rather than written empty, so a
   * message read again later fills it in instead of confirming a blank.
   */
  private messageDraft(channel: string, message: Partial<SlackMessage> & { ts: string }, parentId: string): EntityDraft {
    const threadTs =
      message.thread_ts && message.thread_ts !== message.ts ? message.thread_ts : null
    return {
      id: this.idFor(channel, message.ts),
      parentId,
      values: {
        type: 'slack/message',
        text: message.text,
        'slack/ts': message.ts,
        'slack/user': message.user ?? message.bot_id ?? null,
        'slack/permalink': this.link(channel, message.ts, threadTs),
      },
    }
  }

  /**
   * The message a reply hangs off, read in full. A thread parent older than the
   * cursor is never in the batch that turns up its replies, and a note with
   * children and nothing written on it is the one thing nobody can act on — so
   * it is fetched rather than stubbed. Once per thread per run: the answer does
   * not change, and a busy thread would otherwise cost a call per reply.
   */
  private async parentDraft(channel: string, ts: string): Promise<EntityDraft> {
    const id = this.idFor(channel, ts)
    const known = this.parents.get(id)
    if (known) return known
    const message = await messageAt(this.config().userToken, channel, ts).catch(() => null)
    const draft = this.messageDraft(channel, message ?? { ts }, channel)
    this.parents.set(id, draft)
    return draft
  }

  /** Every entity a page of search hits implies, ready to be written as one. */
  private async fromMatches(matches: SearchMatch[]): Promise<EntityDraft[]> {
    const muted = this.muted()
    const drafts: EntityDraft[] = []
    for (const match of matches) {
      const channel = match.channel?.id
      const ts = match.ts
      if (!channel || !ts || muted.has(channel)) continue
      const threadTs = threadOf(match.permalink)
      const reply = threadTs && threadTs !== ts ? threadTs : null

      drafts.push(
        this.channelDraft(
          match.channel?.name ? { ...match.channel, id: channel } : await this.conversation(channel),
        ),
      )
      if (reply) drafts.push(await this.parentDraft(channel, reply))
      drafts.push(
        this.messageDraft(
          channel,
          { ts, text: match.text, user: match.user, thread_ts: reply ?? undefined },
          reply ? this.idFor(channel, reply) : channel,
        ),
      )
    }
    return drafts
  }

  /**
   * A batch off the socket. One handler covers all of it, because Slack's own
   * shapes do: a thread reply is a `message` with `thread_ts` set, and an edit
   * and a delete are a `message` with a subtype saying which.
   */
  private async apply(events: SocketMessage[]): Promise<void> {
    const muted = this.muted()
    const drafts: EntityDraft[] = []
    // A reaction says which one changed, not what the set now is, so the set is
    // read off the entity and the changes applied to it. Gathered first so that
    // several reactions to one message cost one read between them.
    const reactions: { id: string; name: string; delta: number }[] = []

    for (const event of events) {
      if (event.type === 'reaction_added' || event.type === 'reaction_removed') {
        const channel = event.item?.channel
        const ts = event.item?.ts
        if (!channel || !ts || !event.reaction || muted.has(channel)) continue
        reactions.push({
          id: this.idFor(channel, ts),
          name: event.reaction,
          delta: event.type === 'reaction_added' ? 1 : -1,
        })
        continue
      }
      if (event.type !== 'message') continue
      const channel = event.channel
      if (!channel || muted.has(channel)) continue

      if (event.subtype === 'message_deleted') {
        const ts = event.deleted_ts ?? event.previous_message?.ts
        if (!ts) continue
        // Kept rather than removed: what was said and then unsaid is a thing
        // that happened, and the note may already have been read and filed.
        drafts.push({
          id: this.idFor(channel, ts),
          values: { 'slack/deleted': true },
          ifKnown: true,
        })
        continue
      }

      // An edit arrives wrapped: the new message is inside, the old one beside
      // it. Only the text can have changed, but writing the lot is the same code
      // as a new message and the writer drops whatever already matches.
      const message = event.subtype === 'message_changed' ? event.message : event
      const ts = message?.ts
      if (!message || !ts) continue
      const threadTs = message.thread_ts && message.thread_ts !== ts ? message.thread_ts : null

      drafts.push(this.channelDraft(await this.conversation(channel)))
      if (threadTs) drafts.push(await this.parentDraft(channel, threadTs))
      drafts.push(
        this.messageDraft(
          channel,
          { ts, text: message.text ?? '', user: message.user ?? message.bot_id, thread_ts: threadTs ?? undefined },
          threadTs ? this.idFor(channel, threadTs) : channel,
        ),
      )
    }

    if (reactions.length) drafts.push(...(await this.reactionDrafts(reactions)))
    await this.write(drafts)
  }

  /**
   * Reactions as they now stand. A reaction makes no entity of its own — it is
   * something that happened *to* a message — so this is one value on the
   * message, an object of name to count, which both reads plainly and takes a
   * delta without ambiguity.
   */
  private async reactionDrafts(
    changes: { id: string; name: string; delta: number }[],
  ): Promise<EntityDraft[]> {
    const built = await this.pensive()
    if ('problem' in built) throw new Error(built.problem)
    const ids = [...new Set(changes.map((c) => c.id))]
    const buckets = bucketEvents(ids, await built.pensive.readEvents(ids))

    const counts = new Map<string, Record<string, number>>()
    for (const id of ids) {
      const held = rollupEntity(id, buckets.get(id) ?? []).values['slack/reactions']
      counts.set(id, { ...((held as Record<string, number> | undefined) ?? {}) })
    }
    for (const change of changes) {
      const on = counts.get(change.id)!
      const next = (on[change.name] ?? 0) + change.delta
      if (next > 0) on[change.name] = next
      else delete on[change.name]
    }
    return ids.map((id) => ({
      id,
      // A reaction is not a reading of the message, so it does not conjure one:
      // a note whose whole content is that somebody reacted to something nobody
      // has read is worse than not knowing.
      ifKnown: true,
      // Sorted, so that the same set of reactions is the same value however it
      // was arrived at — otherwise every read would look like a change.
      values: {
        'slack/reactions': Object.fromEntries(
          Object.entries(counts.get(id)!).sort(([a], [b]) => a.localeCompare(b)),
        ),
      },
    }))
  }

  /** Write a batch through the store this node is plugged into. */
  private async write(drafts: EntityDraft[]): Promise<void> {
    if (!drafts.length) return
    const built = await this.pensive()
    if ('problem' in built) throw new Error(built.problem)
    await new EntityWriter(built.pensive, 'slack').write(drafts)
  }
}
