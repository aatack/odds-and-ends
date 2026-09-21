import type { IntegrationStatus } from '@conswap/common/types'
import type { Context, Integration } from '../context.js'
import { run } from '../shell.js'

/**
 * GitHub is not a source of topics. A pull request is something a topic of mine
 * can be waiting on, not a thing that needs me in its own right: polling it for
 * notifications filed hundreds of branches nobody asked about. So all this does
 * is say whether `gh` will answer when a blocker asks it something.
 *
 * The waiting itself lives in `blockers/definitions.ts` — CI, review, merge.
 */
export class GithubIntegration implements Integration {
  readonly name = 'github'

  private timer: NodeJS.Timeout | null = null
  private running = false
  private state: IntegrationStatus['state']
  private detail: string
  private lastRunAt: string | null = null

  constructor(private readonly context: Context) {
    this.state = context.config.github.enabled ? 'idle' : 'disabled'
    this.detail = context.config.github.enabled ? 'not checked yet' : 'turned off'
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
    this.timer = setInterval(() => void this.check(), this.context.config.github.checkSeconds * 1000)
    void this.check()
  }

  stop(): void {
    if (this.timer) clearInterval(this.timer)
    this.timer = null
  }

  /** Nothing here writes to the database; it only reports what `gh` says. */
  private async check(): Promise<void> {
    if (this.running) return
    this.running = true
    try {
      const result = await run('gh', ['auth', 'status'], { timeoutMs: 20_000 })
      const output = `${result.stdout}\n${result.stderr}`
      if (result.code === 0) {
        const account = /account (\S+)/.exec(output)?.[1]
        this.state = 'idle'
        this.detail = account ? `signed in as ${account}` : 'signed in'
      } else {
        this.state = 'error'
        this.detail = 'gh is not signed in'
      }
      this.lastRunAt = new Date().toISOString()
    } finally {
      this.running = false
    }
  }
}
