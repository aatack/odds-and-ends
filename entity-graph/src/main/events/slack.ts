import {
  conversationInfo,
  historySince,
  identityFor,
  kindOf,
  listConversations,
  messageAt,
  messagesAround,
  searchPage,
  whoHolds,
  SEARCH_PAGES,
  type Conversation,
  type SearchMatch,
  type SlackMessage,
  type SlackWorkspace,
} from '../integrations/slack'
import { EntityWriter, type EntityDraft, type WriteReport } from './writer'
import { mentionsIn, slackToMarkdown } from './mrkdwn'
import { Feed, type FeedOptions } from './feed'

// Slack, read into the store: a search that walks backwards from a cursor, twice
// a minute — and, when the search comes back empty-handed, the conversations
// themselves, read one at a time.
//
// **The sweep is there because search is an index and an index can be wrong.**
// One `search.messages` call covers every channel, DM and thread at once, which
// is why it is the route; but it answers `total: 0` for a workspace that is
// plainly not empty often enough that a feed built on it alone is a feed that
// silently does nothing. `conversations.history` takes exact timestamps rather
// than dates and reads the conversation rather than an index of it, so it cannot
// be wrong in that way — it is only expensive, being a call per conversation. So
// it runs when search has found nothing, a handful of conversations per pass,
// round-robin.
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
 * How far the cursor is wound back before each request, and — separately — how
 * far back every pass reaches whatever the cursor says.
 *
 * **`REACH` is the one that matters, and the absence of it was a bug.** Search
 * runs off an index, and an index lags: a message sent at T is findable at
 * T+something, and that something is seconds usually and minutes sometimes. With
 * the window bounded by a cursor that moves to *now* on every pass, a message
 * indexed a minute and a half late falls between two windows and is never seen
 * again — the feed polls happily, finds nothing, and loses the message. So every
 * pass re-reads the last ten minutes regardless of where the cursor is, which
 * costs nothing: reading the same message twice writes nothing the second time.
 *
 * `OVERLAP` still does the same job for the other direction — a catch-up whose
 * cursor is older than `REACH`, where the window is set by the cursor.
 */
const OVERLAP = 60
const REACH = 10 * 60

/**
 * How often the search runs. Search is Tier 2 — twenty requests a minute — and a
 * pass is one of them in the ordinary case, so twice a minute is well inside it
 * and halves how long a message sits in Slack before it is here.
 */
const POLL_MS = 30_000

/**
 * The stretch of time one pass reads, in seconds.
 *
 * `floor` is where it starts. It is the cursor wound back a little — and never
 * less far back than {@link REACH}, which is the whole of the fix for a feed
 * that polls happily and finds nothing: a message indexed after the window that
 * should have held it has moved on is a message lost for good, and a window
 * wider than the poll is what stops that happening.
 *
 * `ceiling` is where it stops, and is null in the ordinary case — a pass reads
 * up to now. It is only set when the cursor is more than a week behind, where
 * the whole gap in one search would be an unbounded request; then it is a day at
 * a time, and a pass that sets one comes straight back for the next day.
 *
 * A cursor of null means a node from before the app started writing one when the
 * node is added. Now rather than yesterday: switching a feed on is a decision
 * about what happens next, not a request to import what has already been said.
 */
export function searchWindow(
  cursor: number | null,
  now: number,
): { floor: number; ceiling: number | null } {
  const from = cursor ?? now
  const floor = Math.min(from - OVERLAP, now - REACH)
  return { floor, ceiling: now - floor > WEEK ? floor + DAY : null }
}

/**
 * The query one pass runs: a date bound, and no search text at all. Slack wants a
 * non-empty `query` but not a *term*, so bounds alone filter the lot, and
 * `sort: timestamp` is what turns "everything" into "the most recent of
 * everything".
 *
 * Both bounds are given {@link MARGIN} of room, which is what stops a date
 * written in UTC from landing on the wrong side of a bound read in somebody
 * else's timezone. The window is enforced on the timestamps, not here.
 */
export function searchQuery(floor: number, ceiling: number | null): string {
  return [
    `after:${day(floor - MARGIN)}`,
    ...(ceiling ? [`before:${day(ceiling + MARGIN)}`] : []),
  ].join(' ')
}

/** Thread parents remembered before the lot is forgotten and read again. */
const PARENTS = 500

/**
 * How many conversations one pass reads when it is sweeping, and how long the
 * list of them is kept.
 *
 * `conversations.history` is Tier 3 — fifty requests a minute for an app that is
 * internal to its workspace — so eight a pass at two passes a minute leaves most
 * of that for the threads. A workspace of forty conversations comes round every
 * two and a half minutes, which is well inside the ten the window reaches back.
 */
const SWEEP = 8
const CONVERSATIONS_MS = 5 * 60_000

/** `YYYY-MM-DD`, which is the only granularity `after:` and `before:` have. */
const day = (seconds: number): string => new Date(seconds * 1000).toISOString().slice(0, 10)

/**
 * How much slack to leave around each date bound, in days.
 *
 * **One day was not enough, and that was the bug that made the feed find
 * nothing.** `after:` and `before:` take a date, not a time, and Slack reads that
 * date in the *searcher's own timezone* — while {@link day} can only write one in
 * UTC, since that is the only timezone this end knows it shares with the other.
 * The two disagree by up to a day: at ten at night the machine's UTC date has
 * already rolled over, so `after:<a day before the window>` came out as today's
 * date, which to Slack means *tomorrow* — a bound in the future, matching
 * nothing, every poll, all evening.
 *
 * Two days of margin covers any timezone on earth in either direction. It costs
 * nothing: the bounds only keep the search shallow, and the timestamps below do
 * the precise work.
 */
const MARGIN = 2 * DAY

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

/** Whether an id is somebody rather than somewhere. `W…` is an enterprise grid. */
const isPerson = (id: string): boolean => id.startsWith('U') || id.startsWith('W')

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
  /** The notes each of those mentions, kept beside it for the same reason. */
  private mentions = new Map<string, EntityDraft[]>()
  /** What a mentioned id is called, so a name costs one lookup ever. */
  private names = new Map<string, string>()
  /** Everywhere this token can see, and when that was last asked. */
  private everywhere: { at: number; value: Conversation[] } | null = null
  /** How far round the sweep has got, so each pass takes the next few. */
  private sweptTo = 0

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
    this.names.clear()
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
    const { floor, ceiling } = searchWindow(asSeconds(cursor), now)

    const query = searchQuery(floor, ceiling)

    const matches: SearchMatch[] = []
    for (let page = 1; page <= SEARCH_PAGES && this.running; page++) {
      const found = await searchPage(userToken, query, page)
      // Kept raw, page by page: when nothing is arriving, the question is
      // whether Slack is handing anything over at all, and this is the only
      // place that can answer it.
      // Said in words rather than in counts. "page 1 of 0, 0 of 0 back" is
      // perfectly precise and tells nobody anything; the question somebody has
      // in front of this panel is "did Slack have anything to give me", and that
      // is what the line should answer.
      this.note(
        found.total === 0
          ? `Slack's search matched nothing at all — not one message anywhere it can see, ` +
            `over the whole period it was asked about (\`${query}\`)`
          : `Slack's search matched ${found.total} messages; this is page ${page} of ` +
            `${found.pages}, with ${found.matches.length} of them (\`${query}\`)`,
        // The window as well as the query: the bounds are dates and the window
        // is to the second, so "the search found nothing" and "the search found
        // things and none were recent enough" read alike without both.
        {
          window: {
            from: new Date(floor * 1000).toISOString(),
            until: ceiling ? new Date(ceiling * 1000).toISOString() : 'now',
          },
          matches: found.matches,
        },
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
        `${kept.length} of those ${matches.length} are inside the window and will be written; ` +
          'the rest are older than it and have been read already',
        {
          from: new Date(floor * 1000).toISOString(),
          until: ceiling ? new Date(ceiling * 1000).toISOString() : 'now',
        },
      )
    }
    const drafts = await this.fromMatches(kept)
    // Search found nothing at all — not "nothing recent", nothing in the whole
    // couple of days it was asked about. For a workspace anybody is using that
    // is a broken index rather than a quiet afternoon, so the conversations get
    // read directly instead. Paced, and only while search is unhelpful.
    if (!matches.length) drafts.push(...(await this.sweep(floor, ceiling)))

    const written = await this.write(drafts)
    if (written) this.note(`Wrote ${written.touched} notes, ${written.created} of them new`, written)

    this.advance({ cursor: (ceiling ?? now).toFixed(6) })
    this.say(
      ceiling
        ? `Catching up — read to ${day(ceiling)}`
        : `Up to date${kept.length ? `, ${kept.length} in the last ${Math.round(REACH / 60)} minutes` : ''}`,
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
      // Never in the inbox, however it was first heard of. A channel is where
      // messages arrive, not one of the things that arrives — an inbox is a list
      // of what to read, and nobody reads a channel.
      context: true,
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

  /**
   * A message's text as markdown, with everybody and everywhere it mentions
   * written down as a note so the mention has a name to show.
   *
   * The lookups are the only reason this is asynchronous: a mention is a raw id
   * and Slack is the only thing that knows whose. Each one is asked about once
   * for as long as the feed runs — a workspace's people do not get renamed
   * often, and a busy channel mentions the same handful of them all day.
   */
  private async render(
    text: string | undefined,
    into: EntityDraft[],
  ): Promise<string | undefined> {
    if (text === undefined) return undefined
    const names: Record<string, string> = {}
    for (const id of mentionsIn(text)) {
      const name = await this.nameOf(id)
      if (name === null) continue
      names[id] = name
      // Written down, but not into the inbox: somebody mentioned in passing is
      // a thing to point at, not a thing that has arrived. A channel gets the
      // note it would have got anyway, which is already out of the inbox.
      into.push(
        isPerson(id)
          ? { id, context: true, values: { type: 'slack/user', text: name } }
          : this.channelDraft(await this.conversation(id)),
      )
    }
    return slackToMarkdown(text, names)
  }

  /** What a mentioned id is called: `@alex`, `#general`. Null if nobody knows. */
  private async nameOf(id: string): Promise<string | null> {
    const known = this.names.get(id)
    if (known !== undefined) return known
    const name = isPerson(id)
      ? await identityFor(this.config().userToken, id)
          .then((who) => `@${who.name}`)
          .catch(() => null)
      : await this.conversation(id).then((c) => conversationName(c))
    if (name === null || name === id) return null
    this.names.set(id, name)
    return name
  }

  /** A message's permalink, which is also the id of the entity it becomes. */
  private link(channel: string, ts: string, threadTs?: string | null): string {
    return permalinkFor(this.workspace?.url ?? '', channel, ts, threadTs)
  }

  private idFor(channel: string, ts: string): string {
    return messageId(this.link(channel, ts))
  }

  /**
   * One message, as the note it becomes: what was said, and who said it.
   *
   * **Nothing else is written down, because nothing else would be new.** The
   * note's own id is the permalink, and the permalink is the workspace, the
   * channel and the timestamp spelled out — so a value for any of those is a
   * second copy of something already there, to be kept in step with it for no
   * gain. Where it is in the outline says the rest: under the channel's note, or
   * under the message it replies to.
   *
   * `text` is left out when it isn't known rather than written empty, so a
   * message read again later fills it in instead of confirming a blank.
   */
  private messageDraft(
    channel: string,
    message: Partial<SlackMessage> & { ts: string },
    parentId: string,
  ): EntityDraft {
    return {
      id: this.idFor(channel, message.ts),
      parentId,
      values: {
        type: 'slack/message',
        text: message.text,
        'slack/user': message.user ?? message.bot_id ?? null,
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
  private async parentDraft(channel: string, ts: string, into: EntityDraft[]): Promise<EntityDraft> {
    const id = this.idFor(channel, ts)
    const known = this.parents.get(id)
    if (known) {
      into.push(...(this.mentions.get(id) ?? []))
      return known
    }
    const message = await messageAt(this.config().userToken, channel, ts).catch(() => null)
    const mentioned: EntityDraft[] = []
    const text = await this.render(message?.text, mentioned)
    const draft = this.messageDraft(channel, { ...(message ?? { ts }), text }, channel)
    // The people it mentions go in beside it: the cached draft is the message
    // alone, but the notes it points at have to be written the first time too.
    this.mentions.set(id, mentioned)
    // Forgotten wholesale rather than one at a time: this is a saving, not a
    // record, and a feed left running for a month must not grow a map of every
    // thread it has ever seen a reply in.
    if (this.parents.size >= PARENTS) {
      this.parents.clear()
      this.mentions.clear()
    }
    this.parents.set(id, draft)
    into.push(...mentioned)
    return draft
  }

  /** Everywhere the token can see, asked for again now and then. */
  private async places(): Promise<Conversation[]> {
    const now = Date.now()
    if (this.everywhere && now - this.everywhere.at < CONVERSATIONS_MS) return this.everywhere.value
    const value = await listConversations(this.config().userToken)
    this.everywhere = { at: now, value }
    return value
  }

  /**
   * A few conversations, read straight rather than through the index.
   *
   * Round-robin rather than all of them: this is a call per conversation and the
   * window reaches back ten minutes, so as long as the whole ring comes round
   * inside that, nothing is missed by taking it slowly.
   */
  private async sweep(floor: number, ceiling: number | null): Promise<EntityDraft[]> {
    const token = this.config().userToken
    const all = await this.places()
    if (!all.length) return []

    // Wrapped by hand: the ring has to come round, and `slice` past the end
    // would quietly shorten the last batch of every lap.
    const take = Array.from({ length: Math.min(SWEEP, all.length) }, (_, i) => all[(this.sweptTo + i) % all.length])
    this.sweptTo = (this.sweptTo + take.length) % all.length

    const drafts: EntityDraft[] = []
    let found = 0
    for (const conversation of take) {
      const messages = await historySince(token, conversation.id, floor, ceiling).catch(() => [])
      if (!messages.length) continue
      drafts.push(this.channelDraft(conversation))
      for (const message of messages) {
        const text = await this.render(message.text, drafts)
        drafts.push(this.messageDraft(conversation.id, { ...message, text }, conversation.id))
        found++
        // `conversations.history` is top-level messages only, so a thread's
        // replies are a call of their own — worth making only when the parent
        // says one has arrived inside the window.
        const latest = asSeconds(message.latest_reply)
        if (latest === null || latest < floor) continue
        const thread = await messagesAround(token, conversation.id, message.ts).catch(() => [])
        for (const reply of thread) {
          const at = asSeconds(reply.ts)
          if (reply.ts === message.ts || at === null || at < floor) continue
          if (ceiling !== null && at >= ceiling) continue
          const body = await this.render(reply.text, drafts)
          drafts.push(
            this.messageDraft(
              conversation.id,
              { ...reply, text: body },
              this.idFor(conversation.id, message.ts),
            ),
          )
          found++
        }
      }
    }
    this.note(
      found === 0
        ? `Read ${take.length} of your ${all.length} conversations directly; nothing in them ` +
          'inside the window either, so Slack really is quiet'
        : // The decisive line, and the reason the sweep exists. Search said there
          // was nothing anywhere; the conversations themselves say otherwise, and
          // the two cannot both be right.
          `Slack's search is not answering for this token — it matched nothing, but reading ` +
          `${take.length} of your ${all.length} conversations directly found ${found} messages`,
      take.map((c) => ({ id: c.id, name: c.name ?? null })),
    )
    return drafts
  }

  /** Every entity a page of search hits implies, ready to be written as one. */
  private async fromMatches(matches: SearchMatch[]): Promise<EntityDraft[]> {
    const drafts: EntityDraft[] = []
    // A hit with no channel or no timestamp cannot be named, so it cannot be
    // written. It should never happen, which is exactly why it is worth saying
    // when it does rather than quietly returning nothing.
    const unnameable = matches.filter((m) => !m.channel?.id || !m.ts)
    if (unnameable.length) {
      this.note(`Skipped ${unnameable.length} hits with no channel or timestamp`, unnameable)
    }
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
      if (reply) drafts.push(await this.parentDraft(channel, reply, drafts))
      const text = await this.render(match.text, drafts)
      drafts.push(
        this.messageDraft(
          channel,
          { ts, text, user: match.user, thread_ts: reply ?? undefined },
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
