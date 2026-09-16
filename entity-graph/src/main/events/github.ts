import { ok } from '../integrations/exec'
import { fetchJsonResponse } from '../integrations/http'
import { runIntegrationTool } from '../integrations/index'
import { EntityWriter, type EntityDraft } from './writer'
import { Feed, type FeedOptions } from './feed'

// GitHub, read into the store. One call covers every repository — `/notifications`
// is the whole of what GitHub thinks is worth telling you — and everything else
// here follows from what that call is and is not.
//
// **A notification names a thread, not an event.** It says "something happened
// on this pull request, and here is why you were told"; it does not say what.
// So a poll is two halves: the notifications, which decide *where* to look, and
// then the comments and reviews on those threads since the cursor, which are
// what actually happened. The notification itself becomes no entity — it would
// be a note saying only that there is a note to read.
//
// There is no Socket Mode here and no equivalent of one; GitHub's push side is a
// webhook, which needs an address on the internet. So this node only polls, and
// is paced by the `X-Poll-Interval` header GitHub asks to be obeyed.

const API = 'https://api.github.com'

/** GitHub's own pace when it hasn't said otherwise. */
const POLL_MS = 60_000

/** Wound back before each request: a comment written on the cursor's second. */
const OVERLAP_MS = 60_000

/**
 * How often the pull requests nobody notified us about are looked for. A
 * notification only comes for a thread you are subscribed to, so your own open
 * pull requests can go quiet for days without a single one arriving — but they
 * change slowly, and this is the expensive half.
 */
const MINE_MS = 15 * 60_000

/** One page of notifications, and how many pages are worth following. */
const PER_PAGE = 100
const MAX_PAGES = 10

/**
 * The next page, out of the `Link` header — GitHub's own way of saying there is
 * one. Read rather than counted: a page number guessed at past the end is a
 * request for nothing, where the absence of this header is the end, stated.
 */
export function nextPage(headers: Headers): string | null {
  for (const part of (headers.get('link') ?? '').split(',')) {
    const match = /<([^>]+)>\s*;\s*rel="next"/.exec(part)
    if (match) return match[1]
  }
  return null
}

export interface GithubFeedConfig {
  token: string
  cursor: string
  lastModified: string
}

interface Notification {
  id: string
  reason: string
  updated_at: string
  subject: { title?: string; url?: string; type?: string }
  repository: { full_name?: string }
}

interface Subject {
  number?: number
  title?: string
  html_url?: string
  state?: string
  draft?: boolean
  merged_at?: string | null
  user?: { login?: string }
  pull_request?: unknown
  head?: { sha?: string }
}

interface Comment {
  id: number
  body?: string
  html_url?: string
  user?: { login?: string }
  issue_url?: string
  pull_request_url?: string
}

interface Review {
  id: number
  body?: string
  html_url?: string
  state?: string
  submitted_at?: string | null
  user?: { login?: string }
}

/** `owner/repo#123`, which is how every other tool in this app names one too. */
const threadId = (repo: string, number: number): string => `${repo}#${number}`

/** The issue or pull request number at the end of an API or web URL. */
const numberIn = (url: string | undefined): number | null => {
  const match = /\/(?:issues|pulls|pull)\/(\d+)/.exec(url ?? '')
  return match ? Number(match[1]) : null
}

/**
 * The id of a comment, in GitHub's own namespaced form: the fragment its web
 * URL ends in — `issuecomment-2412345678`, `discussion_r1234567`,
 * `pullrequestreview-987654`. A bare comment id would not do, because an issue
 * comment and a review comment are numbered from different sequences and the two
 * would eventually collide on one entity.
 */
export function commentId(url: string | undefined, fallback: string): string {
  const hash = url?.split('#')[1]
  return hash ? hash : fallback
}

/** What a pull request is doing, in one word. */
export function stateOf(subject: Subject): string {
  if (subject.merged_at) return 'merged'
  if (subject.draft && subject.state === 'open') return 'draft'
  return subject.state ?? 'open'
}

/**
 * How the checks are getting on, rolled into one word. A run that has not
 * finished outranks a failure that has, since "still going" is the honest answer
 * while it is.
 */
export function checksOf(runs: { status?: string; conclusion?: string | null }[]): string | null {
  if (!runs.length) return null
  if (runs.some((r) => r.status !== 'completed')) return 'running'
  const bad = new Set(['failure', 'timed_out', 'cancelled', 'action_required', 'stale'])
  if (runs.some((r) => bad.has(r.conclusion ?? ''))) return 'failing'
  return 'passing'
}

/** One `slackEvents`-shaped node, but for GitHub. */
export class GithubFeed extends Feed<GithubFeedConfig> {
  private token = ''
  /** When `github.listMyPullRequests` was last asked. */
  private askedMine = 0

  constructor(options: FeedOptions<GithubFeedConfig>) {
    super(options, POLL_MS)
  }

  protected async begin(): Promise<void> {
    const written = this.config().token.trim()
    if (written) {
      this.token = written
      return
    }
    // `gh` is already signed in on this machine and its token is the one the
    // app's other GitHub tools use, so an empty field is the ordinary case
    // rather than a missing configuration.
    const found = await ok('gh', ['auth', 'token']).catch(() => null)
    this.token = found?.stdout.trim() ?? ''
    if (!this.token) {
      throw new Error(
        'No token, and `gh auth token` had none — run `gh auth refresh --scopes notifications`',
      )
    }
  }

  /** One GitHub request, as this node. */
  private async get<T>(
    path: string,
    query: Record<string, string | number | boolean | undefined> = {},
    headers: Record<string, string> = {},
  ): Promise<{ status: number; body: T | null; headers: Headers }> {
    return fetchJsonResponse<T>(`${API}${path}`, {
      query,
      headers: {
        Authorization: `Bearer ${this.token}`,
        Accept: 'application/vnd.github+json',
        'X-GitHub-Api-Version': '2022-11-28',
        ...headers,
      },
      // "Nothing has changed" is the answer a conditional request is asking for,
      // and it is the cheap one — a 304 costs no rate limit at all.
      expect: [304],
    })
  }

  protected async pass(): Promise<void> {
    const { cursor, lastModified } = this.config()
    const started = Date.now()
    const since = new Date((cursor ? Date.parse(cursor) : started - 86_400_000) - OVERLAP_MS)

    const answer = await this.get<Notification[]>(
      '/notifications',
      { since: since.toISOString(), all: true, per_page: PER_PAGE },
      // Sent rather than the cursor: it is GitHub's own idea of when the feed
      // last changed, and `since` alone would re-download an unchanged list.
      lastModified ? { 'If-Modified-Since': lastModified } : {},
    )

    // GitHub says how often it wants to be asked. It is about a minute, and
    // obeying it is the difference between a poll and a scrape.
    const interval = Number(answer.headers.get('x-poll-interval'))
    if (Number.isFinite(interval) && interval > 0) this.pollMs = interval * 1000

    if (answer.status === 304 || !answer.body?.length) {
      await this.mine()
      this.advance({ cursor: new Date(started).toISOString() })
      this.say('Up to date')
      return
    }

    // A minute's worth fits in one page; a first run over a day of a busy
    // account does not, and the pages past the first are the ones that would be
    // lost for good once the cursor moves over them.
    const notifications = [...answer.body]
    let next = nextPage(answer.headers)
    for (let page = 1; page < MAX_PAGES && next && this.running; page++) {
      const more = await this.get<Notification[]>(next.slice(API.length))
      if (!more.body?.length) break
      notifications.push(...more.body)
      next = nextPage(more.headers)
    }

    await this.write(await this.fromNotifications(notifications, since))

    // Only now: the entities are in, so a crash before the next line costs a
    // re-read of the same minute and nothing else.
    this.advance({
      cursor: new Date(started).toISOString(),
      lastModified: answer.headers.get('last-modified') ?? lastModified,
    })
    await this.mine()
    this.say(`Read ${notifications.length} notification${notifications.length === 1 ? '' : 's'}`)
  }

  /**
   * Everything a batch of notifications turns out to be about: the threads
   * themselves, and then the comments and reviews on them since the cursor.
   */
  private async fromNotifications(
    notifications: Notification[],
    since: Date,
  ): Promise<EntityDraft[]> {
    // Grouped by repository, because that is the shape the comment endpoints
    // take: one call per repo covers every thread in it rather than one each.
    const repos = new Map<string, Map<number, Notification>>()
    for (const notification of notifications) {
      const repo = notification.repository?.full_name
      const number = numberIn(notification.subject?.url)
      if (!repo || number === null) continue
      const threads = repos.get(repo) ?? new Map<number, Notification>()
      // The newest reason wins, which is the one the list is sorted to give
      // last: a thread mentioned and then reviewed says "review requested".
      threads.set(number, notification)
      repos.set(repo, threads)
    }

    const drafts: EntityDraft[] = []
    for (const [repo, threads] of repos) {
      for (const [number, notification] of threads) {
        const draft = await this.threadDraft(repo, number, notification)
        if (draft) drafts.push(draft)
      }
      drafts.push(...(await this.commentDrafts(repo, threads, since)))
    }
    return drafts
  }

  /** One pull request or issue, read in full because a notification isn't one. */
  private async threadDraft(
    repo: string,
    number: number,
    notification: Notification,
  ): Promise<EntityDraft | null> {
    const url = notification.subject?.url
    if (!url || !url.startsWith(API)) return null
    const { body: subject } = await this.get<Subject>(url.slice(API.length))
    if (!subject) return null
    const pull = !!subject.pull_request || /\/pulls\/\d+$/.test(url)
    return {
      id: threadId(repo, number),
      values: {
        // An issue and a pull request are one type here, as they nearly are to
        // GitHub: the same thread, the same comments, the same reasons to care.
        type: 'github/pullRequest',
        text: subject.title ?? notification.subject?.title ?? threadId(repo, number),
        'github/url': subject.html_url ?? null,
        'github/state': stateOf(subject),
        'github/author': subject.user?.login ?? null,
        'github/repo': repo,
        // Straight off the notification: `review_requested`, `mention`,
        // `assign`, `author`, `comment`, `state_change`, `ci_activity`, …
        'github/reason': notification.reason ?? null,
        'github/checks': pull ? await this.checks(repo, subject.head?.sha) : undefined,
      },
    }
  }

  /** How the checks on a pull request's head commit are getting on. */
  private async checks(repo: string, sha: string | undefined): Promise<string | null> {
    if (!sha) return null
    const { body } = await this.get<{ check_runs?: { status?: string; conclusion?: string | null }[] }>(
      `/repos/${repo}/commits/${sha}/check-runs`,
      { per_page: 100 },
    ).catch(() => ({ body: null }))
    return checksOf(body?.check_runs ?? [])
  }

  /**
   * The comments and reviews written on the notified threads since the cursor.
   * Two repo-wide endpoints and one call per pull request, then everything that
   * belongs to a thread nobody was told about is dropped — a repository's
   * comments since a timestamp are broader than the threads it notified on, and
   * the inbox is for what arrived rather than for what merely happened.
   */
  private async commentDrafts(
    repo: string,
    threads: Map<number, Notification>,
    since: Date,
  ): Promise<EntityDraft[]> {
    const query = { since: since.toISOString(), per_page: 100 }
    const [issues, pulls] = await Promise.all([
      // One covers the comments on issues *and* on pull requests, which are the
      // same thing to GitHub; the other only the ones left on a line of a diff.
      this.get<Comment[]>(`/repos/${repo}/issues/comments`, query).catch(() => ({ body: null })),
      this.get<Comment[]>(`/repos/${repo}/pulls/comments`, query).catch(() => ({ body: null })),
    ])

    const drafts: EntityDraft[] = []
    for (const comment of [...(issues.body ?? []), ...(pulls.body ?? [])]) {
      const number = numberIn(comment.issue_url ?? comment.pull_request_url ?? comment.html_url)
      if (number === null || !threads.has(number)) continue
      drafts.push({
        id: commentId(comment.html_url, `comment-${comment.id}`),
        parentId: threadId(repo, number),
        values: {
          type: 'github/comment',
          text: comment.body ?? '',
          'github/author': comment.user?.login ?? null,
          'github/url': comment.html_url ?? null,
          'github/reviewState': null,
        },
      })
    }

    for (const [number, notification] of threads) {
      // Only a pull request has reviews; asking an issue for its own is a 404
      // per notification, which is a request spent saying nothing.
      if (notification.subject?.type !== 'PullRequest') continue
      const { body: reviews } = await this.get<Review[]>(
        `/repos/${repo}/pulls/${number}/reviews`,
        { per_page: 100 },
        // A review is not a comment and is not in either list above, and the
        // endpoint has no `since`, so the filtering is done here.
      ).catch(() => ({ body: null }))
      for (const review of reviews ?? []) {
        const at = review.submitted_at ? Date.parse(review.submitted_at) : 0
        if (at < since.getTime()) continue
        drafts.push({
          id: commentId(review.html_url, `pullrequestreview-${review.id}`),
          parentId: threadId(repo, number),
          values: {
            type: 'github/comment',
            text: review.body ?? '',
            'github/author': review.user?.login ?? null,
            'github/url': review.html_url ?? null,
            'github/reviewState': review.state?.toLowerCase() ?? null,
          },
        })
      }
    }
    return drafts
  }

  /**
   * The pull requests nobody sent a notification about. Subscription is what a
   * notification depends on, so a repository you are not watching can hold an
   * open pull request of yours that never arrives here — this is the sweep for
   * those, run rarely because nothing about it is urgent.
   */
  private async mine(): Promise<void> {
    if (Date.now() - this.askedMine < MINE_MS) return
    this.askedMine = Date.now()
    const answer = (await runIntegrationTool('github.listMyPullRequests', {
      state: 'open',
      limit: 50,
    }).catch(() => null)) as {
      pullRequests?: {
        number: number
        title: string
        url: string
        state?: string
        isDraft?: boolean
        author?: { login?: string }
        repository?: { nameWithOwner?: string }
      }[]
    } | null

    const drafts: EntityDraft[] = []
    for (const pull of answer?.pullRequests ?? []) {
      const repo = pull.repository?.nameWithOwner
      if (!repo) continue
      drafts.push({
        id: threadId(repo, pull.number),
        values: {
          type: 'github/pullRequest',
          text: pull.title,
          'github/url': pull.url,
          'github/state': pull.isDraft ? 'draft' : (pull.state?.toLowerCase() ?? 'open'),
          'github/author': pull.author?.login ?? null,
          'github/repo': repo,
          // Deliberately not written: there was no notification, so there is no
          // reason, and blanking one a notification did put there would lose it.
        },
      })
    }
    await this.write(drafts)
  }

  private async write(drafts: EntityDraft[]): Promise<void> {
    if (!drafts.length) return
    const built = await this.pensive()
    if ('problem' in built) throw new Error(built.problem)
    await new EntityWriter(built.pensive, 'github').write(drafts)
  }
}
