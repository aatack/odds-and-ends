import type { IntegrationStatus } from '@conswap/common/types'
import type { Context, Integration } from '../context.js'
import { readJson, writeJson } from '../database.js'
import { addEvent } from '../lifecycle.js'
import { run, runJson } from '../shell.js'
import { getTopic, updateTopic, upsertTopic } from '../topics.js'

interface Notification {
  id: string
  reason: string
  updated_at: string
  subject: { title: string; url: string | null; type: string }
  repository: { full_name: string }
}

interface SearchedPullRequest {
  number: number
  title: string
  url: string
  updatedAt: string
  repository: { nameWithOwner: string }
}

interface PullRequestDetail {
  number: number
  title: string
  url: string
  state: string
  isDraft: boolean
  mergedAt: string | null
  headRefName: string
  reviewDecision: string | null
  statusCheckRollup: { name?: string; status?: string; conclusion?: string; state?: string }[] | null
}

/** What we last said about a PR, so that only changes become events. */
interface PullRequestMemory {
  state: string
  checks: string
  review: string
  draft: boolean
}

function checksVerdict(rollup: PullRequestDetail['statusCheckRollup']): string {
  if (!rollup || rollup.length === 0) return 'none'
  let pending = false
  for (const check of rollup) {
    const conclusion = (check.conclusion ?? check.state ?? '').toUpperCase()
    const status = (check.status ?? '').toUpperCase()
    if (status && status !== 'COMPLETED') pending = true
    else if (['FAILURE', 'TIMED_OUT', 'CANCELLED', 'ACTION_REQUIRED', 'ERROR'].includes(conclusion)) return 'failing'
    else if (conclusion === '' || conclusion === 'PENDING') pending = true
  }
  return pending ? 'running' : 'passing'
}

const reasons: Record<string, string> = {
  review_requested: 'your review was requested',
  mention: 'you were mentioned',
  assign: 'it was assigned to you',
  comment: 'somebody commented',
  ci_activity: 'CI reported back',
  state_change: 'it was opened or closed',
  subscribed: 'there was activity',
  team_mention: 'your team was mentioned',
  author: 'somebody replied to you',
}

export class GithubIntegration implements Integration {
  readonly name = 'github'

  private timer: NodeJS.Timeout | null = null
  private running = false
  private state: IntegrationStatus['state']
  private detail: string
  private lastRunAt: string | null = null

  constructor(private readonly context: Context) {
    this.state = context.config.github.enabled ? 'idle' : 'disabled'
    this.detail = context.config.github.enabled ? 'not started' : 'turned off'
  }

  status(): IntegrationStatus {
    return {
      name: this.name,
      enabled: this.context.config.github.enabled,
      state: this.state,
      detail: this.detail,
      lastRunAt: this.lastRunAt,
    }
  }

  start(): void {
    if (!this.context.config.github.enabled || this.timer) return
    this.timer = setInterval(() => void this.poll(), this.context.config.github.pollSeconds * 1000)
    void this.poll()
  }

  stop(): void {
    if (this.timer) clearInterval(this.timer)
    this.timer = null
  }

  private async poll(): Promise<void> {
    if (this.running) return
    this.running = true
    this.state = 'polling'
    try {
      const whoami = await run('gh', ['auth', 'status'], { timeoutMs: 20_000 })
      if (whoami.code !== 0) throw new Error('gh is not logged in')
      const seen = await this.readNotifications()
      const tracked = await this.readPullRequests()
      this.state = 'idle'
      this.detail = `${tracked} pull requests, ${seen} notifications`
      this.lastRunAt = new Date().toISOString()
    } catch (error) {
      this.state = 'error'
      this.detail = String(error instanceof Error ? error.message : error)
      this.context.log('github', 'poll failed', this.detail)
    } finally {
      this.running = false
    }
  }

  private topicFor(repo: string, number: number, title: string, url: string): string {
    const id = `github:${repo}#${number}`
    const { created } = upsertTopic(this.context.db, {
      id,
      type: 'github_pull_request',
      text: `${repo}#${number} ${title}`,
      metadata: { repo, number, url, provider: 'github' },
      open: false,
    })
    if (!created) {
      const topic = getTopic(this.context.db, id)
      const wanted = `${repo}#${number} ${title}`
      if (topic && topic.text !== wanted) updateTopic(this.context.db, id, { text: wanted })
    }
    return id
  }

  private async readNotifications(): Promise<number> {
    const notifications = await runJson<Notification[]>(
      'gh',
      ['api', '-X', 'GET', 'notifications', '-f', 'all=false', '--paginate'],
      { timeoutMs: 60_000 },
    )
    if (!notifications) return 0
    const seen = new Set(readJson<string[]>(this.context.db, 'github:notifications', []))
    let filed = 0
    for (const notification of notifications) {
      const key = `${notification.id}:${notification.updated_at}`
      if (seen.has(key)) continue
      seen.add(key)
      const number = Number.parseInt((notification.subject.url ?? '').split('/').pop() ?? '', 10)
      if (!Number.isFinite(number)) continue
      const topicId = this.topicFor(
        notification.repository.full_name,
        number,
        notification.subject.title,
        `https://github.com/${notification.repository.full_name}/pull/${number}`,
      )
      addEvent(this.context, topicId, {
        type: 'github_event',
        text: `${reasons[notification.reason] ?? notification.reason} — ${notification.subject.title}`,
        metadata: {
          repo: notification.repository.full_name,
          number,
          reason: notification.reason,
          url: `https://github.com/${notification.repository.full_name}/pull/${number}`,
        },
      })
      filed += 1
    }
    writeJson(this.context.db, 'github:notifications', [...seen].slice(-800))
    return filed
  }

  private async readPullRequests(): Promise<number> {
    const mine =
      (await runJson<SearchedPullRequest[]>(
        'gh',
        ['search', 'prs', '--author=@me', '--state=open', '--limit=30', '--json', 'number,title,url,updatedAt,repository'],
        { timeoutMs: 60_000 },
      )) ?? []
    const reviewing =
      (await runJson<SearchedPullRequest[]>(
        'gh',
        [
          'search',
          'prs',
          '--review-requested=@me',
          '--state=open',
          '--limit=20',
          '--json',
          'number,title,url,updatedAt,repository',
        ],
        { timeoutMs: 60_000 },
      )) ?? []

    const byKey = new Map<string, SearchedPullRequest>()
    for (const pull of [...mine, ...reviewing]) byKey.set(`${pull.repository.nameWithOwner}#${pull.number}`, pull)

    const memory = readJson<Record<string, PullRequestMemory>>(this.context.db, 'github:pulls', {})
    for (const [key, pull] of byKey) {
      const repo = pull.repository.nameWithOwner
      const detail = await runJson<PullRequestDetail>(
        'gh',
        [
          'pr',
          'view',
          String(pull.number),
          '--repo',
          repo,
          '--json',
          'number,title,url,state,isDraft,mergedAt,headRefName,reviewDecision,statusCheckRollup',
        ],
        { timeoutMs: 45_000 },
      )
      if (!detail) continue
      const topicId = this.topicFor(repo, detail.number, detail.title, detail.url)
      updateTopic(this.context.db, topicId, {
        metadata: {
          repo,
          number: detail.number,
          url: detail.url,
          branch: detail.headRefName,
          reviewDecision: detail.reviewDecision,
          checks: checksVerdict(detail.statusCheckRollup),
          draft: detail.isDraft,
        },
      })

      const current: PullRequestMemory = {
        state: detail.mergedAt ? 'merged' : detail.state.toLowerCase(),
        checks: checksVerdict(detail.statusCheckRollup),
        review: (detail.reviewDecision ?? 'none').toLowerCase(),
        draft: detail.isDraft,
      }
      const previous = memory[key]
      memory[key] = current
      if (!previous) continue
      if (previous.checks !== current.checks && current.checks !== 'running') {
        addEvent(this.context, topicId, {
          type: 'github_event',
          text: current.checks === 'passing' ? 'CI is green' : `CI is ${current.checks}`,
          metadata: { repo, number: detail.number, url: detail.url, kind: 'checks' },
        })
      }
      if (previous.review !== current.review && current.review !== 'none') {
        addEvent(this.context, topicId, {
          type: 'github_event',
          text: `review: ${current.review.replace(/_/g, ' ')}`,
          metadata: { repo, number: detail.number, url: detail.url, kind: 'review' },
        })
      }
      if (previous.state !== current.state) {
        addEvent(this.context, topicId, {
          type: 'github_event',
          text: current.state === 'merged' ? 'merged' : `${current.state}`,
          metadata: { repo, number: detail.number, url: detail.url, kind: 'state' },
        })
      }
    }
    writeJson(this.context.db, 'github:pulls', memory)
    return byKey.size
  }
}
