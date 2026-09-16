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
import { EntityWriter, type EntityDraft, type WriteReport } from './writer'
import { Feed, type FeedOptions } from './feed'

// Slack, read into the store: a search that walks backwards from a cursor, once
// a minute.
//
// **There is one way in, and it is a poll.** Socket Mode would make the same
// entities appear seconds after a message is sent rather than within the minute,
// but it costs a second token, a second set of event subscriptions and, in most
// workspaces, an administrator's approval — and what it buys is promptness
// rather than anything the inbox does not eventually get. So the node takes one
// field, and everything else about it follows: no reactions, no edits, no
// deletions, because a search hands back the message as it now stands and says
// nothing about what happened to it.
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
 * A permalink, built rather than asked for: Slack's own form is the workspace
 * URL, the channel, and the timestamp with its dot taken out.
 *
 * Built for every message rather than taken from the search hit that came with
 * one, because this is what an entity is *identified* by. One way of arriving at
 * the string means a message found twice is one note, where two ways that
 * usually agree would be two notes on the day they didn't — and a thread parent
 * fetched by timestamp has no hit to take a permalink from at all.
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

export interface SlackFeedConfig {
  userToken: string
  cursor: string
}

/**
 * One `slackEvents` node, running.
 *
 * A restart is safe because a pass is safe: it reads from the cursor, writes,
 * and only then moves the cursor, and reading the same stretch twice writes
 * nothing the second time.
 */
export class SlackFeed extends Feed<SlackFeedConfig> {
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
    this.note(`Signed in to ${this.workspace.url} as @${this.workspace.handle}`, this.workspace)
  }

  protected async end(): Promise<void> {
    this.parents.clear()
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
      // Kept raw, page by page: when nothing is arriving, the question is
      // whether Slack is handing anything over at all, and this is the only
      // place that can answer it.
      this.note(
        `Searched \`${query}\`, page ${page} of ${found.pages} — ${found.matches.length} back`,
        found.matches,
      )
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
    if (matches.length !== kept.length) {
      this.note(
        `Kept ${kept.length} of ${matches.length} — the rest are older than the cursor`,
        { cursor: new Date(floor * 1000).toISOString(), until: ceiling && new Date(ceiling * 1000).toISOString() },
      )
    }
    const written = await this.write(await this.fromMatches(kept))
    if (written) this.note(`Wrote ${written.touched} notes, ${written.created} of them new`, written)

    this.advance({ cursor: (ceiling ?? now).toFixed(6) })
    this.say(
      ceiling
        ? `Catching up — read to ${day(ceiling)}`
        : `Up to date${kept.length ? `, ${kept.length} in the last minute` : ''}`,
    )
    // Still behind: come straight back rather than waiting out the poll.
    if (ceiling && this.running) return this.pass()
  }

  // --- Turning what arrived into entities -----------------------------------

  /**
   * The conversation an id names, for the hits search did not name one on.
   * Remembered: a workspace's channels do not get renamed often.
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
    const drafts: EntityDraft[] = []
    for (const match of matches) {
      const channel = match.channel?.id
      const ts = match.ts
      if (!channel || !ts) continue
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

  /** Write a batch through the store this node is plugged into. */
  private async write(drafts: EntityDraft[]): Promise<WriteReport | null> {
    if (!drafts.length) return null
    const built = await this.pensive()
    if ('problem' in built) throw new Error(built.problem)
    return new EntityWriter(built.pensive, 'slack').write(drafts)
  }
}
