import { mkdtempSync, rmSync } from 'fs'
import { tmpdir } from 'os'
import { join } from 'path'
import { afterAll, beforeAll, describe, expect, it } from 'vitest'
import type { FastifyInstance } from 'fastify'
import { buildApp } from '../src/app'
import { ConfigDb } from '../src/config'
import { Registry } from '../src/registry'
import { INTEGRATION_TOOLS } from '../src/integrations/index'
import { pullRequestArgs } from '../src/integrations/github'
import {
  idSet,
  keepInFeed,
  parseRef,
  parseUserId,
  recentQuery,
  slackError,
} from '../src/integrations/slack'

// The endpoint and the argument parsing. Nothing here reaches GitHub, Slack or
// Claude — the handlers themselves are only exercised by hand.

const ADMIN = 'admin-secret'
let dir: string
let db: ConfigDb
let app: FastifyInstance

const headers = { authorization: `Bearer ${ADMIN}`, 'content-type': 'application/json' }

beforeAll(async () => {
  dir = mkdtempSync(join(tmpdir(), 'eg-integrations-'))
  db = new ConfigDb(join(dir, 'config.db'))
  app = buildApp({ db, registry: new Registry(db), adminToken: ADMIN })
  await app.ready()
})

afterAll(async () => {
  await app.close()
  db.close()
  rmSync(dir, { recursive: true, force: true })
})

describe('the tool list', () => {
  it('needs the admin token', async () => {
    expect((await app.inject({ method: 'GET', url: '/tools' })).statusCode).toBe(401)
  })

  it('lists every registered tool with a JSON Schema for its arguments', async () => {
    const listed = (await app.inject({ method: 'GET', url: '/tools', headers })).json()
    expect(listed.map((t: { id: string }) => t.id).sort()).toEqual(
      INTEGRATION_TOOLS.map((t) => t.id).sort(),
    )
    for (const tool of listed) {
      expect(tool.args.type).toBe('object')
      expect(tool.safety).toBe('dangerous')
    }
  })

  it('covers GitHub, git, Claude, Slack and the terminal', () => {
    const prefixes = new Set(INTEGRATION_TOOLS.map((t) => t.id.split('.')[0]))
    expect([...prefixes].sort()).toEqual(['claude', 'git', 'github', 'slack', 'terminal'])
  })
})

describe('runTool', () => {
  const run = (payload: unknown) =>
    app.inject({ method: 'POST', url: '/runTool', headers, payload }).then((r) => r.json())

  it('needs the admin token', async () => {
    const r = await app.inject({ method: 'POST', url: '/runTool', payload: { tool: 'x' } })
    expect(r.statusCode).toBe(401)
  })

  it('refuses a tool it does not have', async () => {
    expect(await run({ tool: 'github.nope', args: {} })).toMatchObject({
      status: 'error',
      message: expect.stringContaining('github.nope'),
    })
  })

  it('refuses a call with no tool named', async () => {
    expect(await run({ args: {} })).toMatchObject({ status: 'error' })
  })

  it('validates arguments before running anything', async () => {
    const out = await run({ tool: 'github.getPullRequest', args: {} })
    expect(out).toMatchObject({ status: 'error', message: expect.stringContaining('pullRequest') })
  })

  it('rejects null for a required argument, the way sources do', async () => {
    const out = await run({ tool: 'slack.sendMessage', args: { channel: null, text: 'hi' } })
    expect(out).toMatchObject({ status: 'error', message: expect.stringContaining('channel') })
  })
})

describe('naming a pull request', () => {
  it('passes a URL through', () => {
    expect(pullRequestArgs('https://github.com/o/r/pull/12')).toEqual([
      'https://github.com/o/r/pull/12',
    ])
  })

  it('splits the owner/repo#number shorthand', () => {
    expect(pullRequestArgs('o/r#12')).toEqual(['12', '--repo', 'o/r'])
    expect(pullRequestArgs('o/r/12')).toEqual(['12', '--repo', 'o/r'])
  })

  it('refuses a bare number, which has no repo to resolve against', () => {
    expect(() => pullRequestArgs('12')).toThrow(/URL/)
  })
})

describe('naming a place in Slack', () => {
  it('reads a message permalink', () => {
    expect(parseRef('https://acme.slack.com/archives/C0123ABCD/p1712345678000100')).toEqual({
      channel: 'C0123ABCD',
      ts: '1712345678.000100',
    })
  })

  it('keeps the thread a permalink names', () => {
    expect(
      parseRef(
        'https://acme.slack.com/archives/C0123ABCD/p1712345678000100?thread_ts=1712345600.000100&cid=C0123ABCD',
      ),
    ).toEqual({
      channel: 'C0123ABCD',
      ts: '1712345678.000100',
      threadTs: '1712345600.000100',
    })
  })

  it('reads a channel link as the conversation, with no message', () => {
    expect(parseRef('https://acme.slack.com/archives/C0123ABCD')).toEqual({ channel: 'C0123ABCD' })
  })

  it('reads the channel:timestamp pair', () => {
    expect(parseRef('C0123ABCD:1712345678.000100')).toEqual({
      channel: 'C0123ABCD',
      ts: '1712345678.000100',
    })
  })

  it('treats every kind of conversation alike', () => {
    for (const id of ['C0123ABCD', 'D0123ABCD', 'G0123ABCD', 'U0123ABCD', '#general']) {
      expect(parseRef(id)).toEqual({ channel: id })
    }
  })

  it('complains about a link that is not Slack’s', () => {
    expect(() => parseRef('https://example.com/hello')).toThrow(/Slack link/)
  })
})

describe('naming somebody in Slack', () => {
  it('reads the id the other tools hand back', () => {
    expect(parseUserId('U0123ABCD')).toBe('U0123ABCD')
    expect(parseUserId('  U0123ABCD  ')).toBe('U0123ABCD')
  })

  it('reads a mention out of a message’s text', () => {
    expect(parseUserId('<@U0123ABCD>')).toBe('U0123ABCD')
    expect(parseUserId('<@U0123ABCD|alex>')).toBe('U0123ABCD')
  })

  it('takes an enterprise id and a bot id too', () => {
    expect(parseUserId('W0123ABCD')).toBe('W0123ABCD')
    expect(parseUserId('B0123ABCD')).toBe('B0123ABCD')
  })

  it('refuses a handle, which the Web API cannot look up', () => {
    expect(() => parseUserId('@alex')).toThrow(/not a Slack user id/)
    expect(() => parseUserId('Alex Atack')).toThrow(/not a Slack user id/)
  })

  it('refuses a conversation id, which names a place and not a person', () => {
    for (const id of ['C0123ABCD', 'D0123ABCD', 'G0123ABCD', '#general']) {
      expect(() => parseUserId(id)).toThrow(/not a Slack user id/)
    }
  })
})

describe('the recent-messages query', () => {
  it('is modifiers only — there is no text to search for', () => {
    expect(recentQuery('2026-07-24', null)).toBe('after:2026-07-24')
  })

  it('excludes you by handle', () => {
    expect(recentQuery('2026-07-24', 'alex')).toBe('after:2026-07-24 -from:@alex')
  })

  it('always carries a positive term, so it is never a bare negation', () => {
    for (const handle of [null, 'alex']) {
      expect(recentQuery('2026-07-24', handle).startsWith('after:')).toBe(true)
    }
  })
})

describe('what the feed leaves out', () => {
  const filters = { joined: new Set(['C_IN', 'C_MUTED', 'D_DM']), muted: new Set(['C_MUTED']) }
  const both = { unjoined: false, muted: false }
  const keep = (id: string | null | undefined, opts = both) => keepInFeed(id, filters, opts)

  it('keeps a conversation you are in', () => {
    expect(keep('C_IN')).toBe(true)
    expect(keep('D_DM')).toBe(true)
  })

  it('drops a public channel you never joined', () => {
    expect(keep('C_STRANGER')).toBe(false)
  })

  it('drops a muted conversation even though you are in it', () => {
    expect(keep('C_MUTED')).toBe(false)
  })

  it('lets either filter be turned off on its own', () => {
    expect(keep('C_STRANGER', { unjoined: true, muted: false })).toBe(true)
    expect(keep('C_MUTED', { unjoined: false, muted: true })).toBe(true)
    // Muted still wins when only the membership filter is relaxed.
    expect(keep('C_MUTED', { unjoined: true, muted: false })).toBe(false)
  })

  it('keeps a match Slack gave no channel for', () => {
    expect(keep(undefined)).toBe(true)
    expect(keep(null)).toBe(true)
  })

  it('keeps everything when there is nothing to filter against', () => {
    expect(keepInFeed('C_STRANGER', null, both)).toBe(true)
  })
})

describe('what a failed Slack call says', () => {
  it('names the scope Slack asked for, and what the token has', () => {
    const said = slackError('users.conversations', {
      ok: false,
      error: 'missing_scope',
      needed: 'im:read',
      provided: 'search:read,chat:write',
    })
    expect(said).toContain('im:read')
    expect(said).toContain('search:read,chat:write')
    // The two things that are actually wrong when this happens.
    expect(said).toMatch(/User.*Token Scopes/)
    expect(said).toContain('reinstall')
  })

  it('falls back to the bare error when Slack names no scope', () => {
    expect(slackError('conversations.history', { ok: false, error: 'channel_not_found' })).toBe(
      'Slack conversations.history failed: channel_not_found',
    )
    expect(slackError('chat.postMessage', { ok: false })).toContain('unknown error')
  })
})

describe('reading a list of ids', () => {
  it('tolerates spacing and trailing separators', () => {
    expect([...idSet('C1, C2 ,C3,')]).toEqual(['C1', 'C2', 'C3'])
  })

  it('is empty for nothing at all', () => {
    for (const raw of [undefined, '', '  ', ',,']) expect(idSet(raw).size).toBe(0)
  })
})
