import type { Pensive } from '../../core/pensive/index'

// What the two feeds have in common, which is nearly all of the running of one:
// a cursor to keep, a pass to make on a timer, a line to say about how it is
// getting on, and a way of being switched off.
//
// The part worth naming is the cursor. **It is written after the entities, never
// before**, and it is wound back before each request rather than trusted. Both
// rules are the same bet: that reading a stretch twice is free — which it is,
// because an entity's id is made from the thing's own id, so the second reading
// writes nothing — and that reading it *not at all* is the failure nobody
// notices. So every choice here is made in favour of the overlap.

/**
 * A feed as whatever is running it holds one: started, stopped, and asked how it
 * is getting on. What it reads and what it writes are its own business.
 */
export interface RunningFeed {
  start(): Promise<void>
  stop(): Promise<void>
  status(): { problem: string | null; activity: string | null }
}

/** What a feed needs of the world outside it. */
export interface FeedOptions<C> {
  nodeId: string
  /** The node's name, for the sentences a feed says about itself. */
  label: string
  /** The node's config as it stands now — the user may have edited it. */
  config: () => C
  /**
   * Write part of the config back without disturbing anything. A cursor moves
   * every minute, and rebuilding every pensive downstream each time it did would
   * be the most expensive thing the app does.
   */
  advance: (patch: Partial<C>) => void
  /** The store it writes into: whatever is plugged into the node. */
  pensive: () => Promise<{ pensive: Pensive } | { problem: string }>
  /** Something the page would want to redraw for. */
  changed: () => void
  /**
   * Keep one line, and optionally the raw thing it is about, for the node's
   * inspector. This is the only account of a feed that is *working*: a poll that
   * finds nothing and a poll that never happened look identical from outside,
   * and telling them apart is the whole of debugging one of these.
   */
  note: (summary: string, detail?: unknown) => void
}

export abstract class Feed<C> implements RunningFeed {
  /** False from the moment it is asked to stop, checked everywhere that loops. */
  protected running = false
  private timer: NodeJS.Timeout | null = null
  private activity: string | null = null
  private problem: string | null = null
  /** Whichever pass is in flight, so two never overlap. */
  private inFlight: Promise<void> | null = null
  /** Whether {@link begin} has been through once without throwing. */
  private begun = false

  constructor(
    protected options: FeedOptions<C>,
    /** How long between passes. A feed the service paces may change it. */
    protected pollMs: number,
  ) {}

  protected config(): C {
    return this.options.config()
  }

  protected advance(patch: Partial<C>): void {
    this.options.advance(patch)
  }

  protected pensive(): Promise<{ pensive: Pensive } | { problem: string }> {
    return this.options.pensive()
  }

  /** One line for the inspector, and the raw thing it is about. */
  protected note(summary: string, detail?: unknown): void {
    this.options.note(summary, detail)
  }

  /** Whatever a feed has to do once, before its first pass. */
  protected async begin(): Promise<void> {}
  /** Whatever it has to let go of. */
  protected async end(): Promise<void> {}
  /** One reading of everything since the cursor, and then the cursor moved. */
  protected abstract pass(): Promise<void>

  /** What it is doing, for the node to draw. */
  protected say(activity: string): void {
    if (this.activity === activity && !this.problem) return
    this.activity = activity
    this.problem = null
    this.options.changed()
  }

  /** What went wrong. Said once rather than on every pass that repeats it. */
  protected failed(error: unknown): void {
    const message = error instanceof Error ? error.message : String(error)
    // Kept every time, unlike the line the node shows: the same failure twice is
    // not news to look at, but it is very much news that it is still happening.
    this.options.note(`Failed: ${message}`, error instanceof Error ? error.stack : error)
    if (this.problem === message) return
    this.problem = message
    this.options.changed()
  }

  status(): { problem: string | null; activity: string | null } {
    return { problem: this.problem, activity: this.activity }
  }

  /**
   * Start, and keep going. The catch-up runs before the timer is set, so the
   * first pass is the one that reads everything missed while the app was shut.
   */
  async start(): Promise<void> {
    if (this.running) return
    this.running = true
    this.say(`Starting ${this.options.label}`)
    await this.tick()
    this.schedule()
  }

  async stop(): Promise<void> {
    this.running = false
    this.begun = false
    if (this.timer) clearTimeout(this.timer)
    this.timer = null
    // The pass in flight is waited for rather than abandoned: it may be half way
    // through writing, and the cursor has not moved yet.
    await this.inFlight?.catch(() => undefined)
    await this.end().catch(() => undefined)
  }

  /**
   * The next pass. A timeout set after each one rather than an interval, so a
   * service that asks to be polled less often — GitHub says so in a header — is
   * obeyed from the next pass rather than from the next restart, and so a pass
   * that took longer than the gap doesn't come back to a queue of them.
   */
  private schedule(): void {
    if (!this.running) return
    this.timer = setTimeout(() => {
      void this.tick().then(() => this.schedule())
    }, this.pollMs)
  }

  /** One pass, never two at once — a slow one must not be lapped by the timer. */
  private async tick(): Promise<void> {
    if (!this.running || this.inFlight) return
    const run = this.once().catch((e) => this.failed(e))
    this.inFlight = run
    await run
    this.inFlight = null
  }

  /**
   * Beginning, if it has not begun, and then one pass.
   *
   * The two are one step so that **a bad start is retried rather than fatal**. A
   * token Slack refuses, a network that was down when the app opened: none of
   * those is a reason for a node to sit there dead until something else happens
   * to restart it, and a feed that has given up looks exactly like one that is
   * working and finding nothing.
   */
  private async once(): Promise<void> {
    if (!this.begun) {
      await this.begin()
      this.begun = true
    }
    await this.pass()
  }
}
