import { randomBytes } from 'crypto'
import { existsSync } from 'fs'
import { homedir } from 'os'
import { basename, dirname, join, resolve } from 'path'
import { z } from 'zod'
import type { ToolDef } from '../../../src/core/source/index'
import { directory, ok, run, said } from './exec'

// Git, through the `git` CLI, which must be on the server's PATH.
//
// Every tool takes the directory to run in, because that is what names the
// repository: there is no "current" one here, and two worktrees of the same
// repository are two different places to be standing.
//
// Worktrees are why this exists. Something that is going to change a repository —
// a Claude session, most likely — gets a checkout of its own, works there, pushes
// a branch, and hands the path back to be removed. So `createWorktree` is the one
// tool that decides *where*: everything else is told.

const GIT = 'git'

/**
 * Where worktrees are made. One directory off the home directory, so that a
 * stray one is obvious and the whole lot can be swept away by hand if it comes
 * to that.
 */
const WORKTREES = join(homedir(), '.pensive-worktrees')

/** Long enough that a collision is a curiosity, short enough to read out. */
const ID_LENGTH = 6

const ALPHABET = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'

/** Fetching and pushing cross a network, and a big repository takes its time. */
const NETWORK_MS = 5 * 60_000

/**
 * Git told there is nobody at the keyboard. Without this, a push to a repository
 * wanting credentials — or anything that would open an editor — waits for an
 * answer that is never coming and is killed on the timeout, which reports the
 * wait rather than the reason for it.
 */
const NONINTERACTIVE = { GIT_TERMINAL_PROMPT: '0', GIT_EDITOR: 'true' }

/**
 * Six characters of `a-zA-Z0-9`. Bytes at or above the last whole multiple of the
 * alphabet are thrown away rather than folded back in with `%`, which would make
 * the first handful of letters slightly likelier than the rest.
 */
function shortId(): string {
  const ceiling = 256 - (256 % ALPHABET.length)
  let id = ''
  while (id.length < ID_LENGTH) {
    for (const byte of randomBytes(ID_LENGTH)) {
      if (byte >= ceiling) continue
      id += ALPHABET[byte % ALPHABET.length]
      if (id.length === ID_LENGTH) break
    }
  }
  return id
}

/** A worktree path nothing is using yet. */
function freePath(): string {
  for (let attempt = 0; attempt < 10; attempt++) {
    const path = join(WORKTREES, shortId())
    if (!existsSync(path)) return path
  }
  throw new Error(`Couldn't find an unused name in ${WORKTREES}`)
}

/** The branch checked out in `cwd`, or null when the head is detached. */
async function branchAt(cwd: string): Promise<string | null> {
  const { stdout } = await ok(GIT, ['rev-parse', '--abbrev-ref', 'HEAD'], { cwd })
  const name = stdout.trim()
  return name && name !== 'HEAD' ? name : null
}

/**
 * The main worktree of whatever repository `cwd` belongs to. `--git-common-dir`
 * is the one directory every worktree of a repository shares, so its parent is
 * the checkout the others were made from — and that is where a removal is run,
 * rather than from inside the directory being deleted.
 *
 * It comes back relative when `cwd` *is* the main worktree, hence the resolve.
 */
async function mainWorktree(cwd: string): Promise<string> {
  const { stdout } = await ok(GIT, ['rev-parse', '--git-common-dir'], { cwd })
  return dirname(resolve(cwd, stdout.trim()))
}

const pathArg = (what: string): z.ZodString => z.string().min(1).describe(what)

export const GIT_TOOLS: ToolDef[] = [
  {
    id: 'git.createWorktree',
    name: 'Create a worktree',
    description: [
      `Make a new worktree of a repository under \`${WORKTREES}\`, named with a`,
      'six-character id, and hand back the full path to it. The branch is given the',
      'same name as the directory, so the id names both.',
      '',
      '`from` is what to branch off — `origin/master`, usually. The checkout’s',
      'default remote is fetched first, since a remote-tracking ref is only worth',
      'branching from if it is current. Without one the branch starts from whatever',
      'the given checkout has at `HEAD`, which is what a checkout sitting on a',
      'feature branch will quietly hand you.',
      '',
      'Nothing is shared with the checkout it came from but the repository itself,',
      'so work in here disturbs nothing.',
    ].join('\n'),
    safety: 'dangerous',
    args: z.object({
      path: pathArg('A checkout of the repository to branch from — `~/repos/x` works'),
      from: z
        .string()
        .optional()
        .describe('Ref to branch off, e.g. `origin/master`. Fetched first. Omit for `HEAD`'),
    }),
    handler: async ({ path, from }) => {
      const repo = directory(path)
      const worktree = freePath()
      const branch = basename(worktree)
      // Only when there is a start-point to be current about. A fetch is the one
      // thing here that crosses a network, and a worktree off `HEAD` has nothing
      // to gain by waiting for one.
      if (from) {
        await ok(GIT, ['fetch', '--quiet'], { cwd: repo, env: NONINTERACTIVE, timeoutMs: NETWORK_MS })
      }
      // `-b` explicitly, rather than letting git name the branch after the
      // directory as it does when neither is given: with a start-point that is the
      // only way to land on a branch instead of a detached `HEAD`, and without one
      // the result is exactly what it always was.
      await ok(GIT, ['worktree', 'add', '-b', branch, worktree, ...(from ? [from] : [])], {
        cwd: repo,
        env: NONINTERACTIVE,
      })
      return { path: worktree, id: branch, branch, from: from ?? null }
    },
  },

  {
    id: 'git.removeWorktree',
    name: 'Remove a worktree',
    description: [
      'Delete a worktree and the directory it lives in, once nothing needs it.',
      '',
      'Refused while the worktree has changes or untracked files in it, unless',
      '`force` says otherwise — and refused outright on the main checkout of a',
      'repository, which is not a worktree anything here made.',
      '',
      'The branch outlives the worktree, so it is deleted too where git considers',
      'that safe: a branch whose commits have gone nowhere is left alone.',
    ].join('\n'),
    safety: 'dangerous',
    args: z.object({
      path: pathArg('The worktree to remove — the path `git.createWorktree` returned'),
      force: z
        .boolean()
        .default(false)
        .describe('Remove it even though it has changes or untracked files in it'),
    }),
    handler: async ({ path, force }) => {
      const worktree = directory(path)
      const repo = await mainWorktree(worktree)
      // Asked before the removal, since afterwards there is nowhere to ask from.
      const branch = await branchAt(worktree)
      await ok(GIT, ['worktree', 'remove', ...(force ? ['--force'] : []), worktree], { cwd: repo })
      // `-d` rather than `-D`: it refuses a branch that isn't fully merged, so work
      // that hasn't been pushed or merged anywhere is never what gets tidied away.
      // A refusal is not a failure of the removal, which has already happened.
      const cleaned = branch
        ? (await run(GIT, ['branch', '-d', branch], { cwd: repo })).exitCode === 0
        : false
      return { removed: worktree, repository: repo, branch, branchDeleted: cleaned }
    },
  },

  {
    id: 'git.pull',
    name: 'Git pull',
    description: [
      'Fast-forward the checkout at `path` to whatever its upstream has.',
      '',
      '`--ff-only`: there is nobody here to resolve a merge, and a merge commit is',
      'not something a tool should invent — a branch that has diverged says so and',
      'stops, leaving the checkout as it was.',
    ].join('\n'),
    safety: 'dangerous',
    args: z.object({ path: pathArg('The checkout to pull into — `~/repos/x` works') }),
    handler: async ({ path }) => {
      const cwd = directory(path)
      const result = await ok(GIT, ['pull', '--ff-only'], {
        cwd,
        env: NONINTERACTIVE,
        timeoutMs: NETWORK_MS,
      })
      return { branch: await branchAt(cwd), output: said(result) }
    },
  },

  {
    id: 'git.commitAll',
    name: 'Commit everything',
    description: [
      'Stage everything in the checkout at `path` — modifications, deletions and',
      'untracked files alike — and commit it under `message`.',
      '',
      'A checkout with nothing outstanding is **not** an error: this is the sweep',
      'that runs behind an agent which was supposed to commit its own work, so',
      'finding nothing to do is the good outcome. `committed` says which happened.',
    ].join('\n'),
    safety: 'dangerous',
    args: z.object({
      path: pathArg('The checkout to commit in — `~/repos/x` works'),
      message: z.string().min(1).describe('The commit message'),
    }),
    handler: async ({ path, message }) => {
      const cwd = directory(path)
      await ok(GIT, ['add', '--all'], { cwd })
      // Asked after staging and read as porcelain, so this says the same thing
      // whatever git version and locale the server happens to have.
      const { stdout } = await ok(GIT, ['status', '--porcelain'], { cwd })
      const branch = await branchAt(cwd)
      if (!stdout.trim()) return { committed: false, branch, commit: null }
      const result = await ok(GIT, ['commit', '--message', message], { cwd, env: NONINTERACTIVE })
      const { stdout: head } = await ok(GIT, ['rev-parse', 'HEAD'], { cwd })
      return { committed: true, branch, commit: head.trim(), output: said(result) }
    },
  },

  {
    id: 'git.push',
    name: 'Git push',
    description: [
      'Push the checkout at `path` to `origin`, tracking it there.',
      '',
      'Naming a `branch` makes one first and switches to it, so what is pushed and',
      'what is checked out do not part company. Without one, whatever is checked',
      'out is pushed to a branch of the same name.',
      '',
      'Nothing here can answer a password prompt, so a remote that wants one fails',
      'rather than hanging: use SSH, or a credential helper.',
    ].join('\n'),
    safety: 'dangerous',
    args: z.object({
      path: pathArg('The checkout to push from — `~/repos/x` works'),
      branch: z.string().optional().describe('Create this branch and push that instead'),
    }),
    handler: async ({ path, branch }) => {
      const cwd = directory(path)
      if (branch) await ok(GIT, ['checkout', '-b', branch], { cwd })
      // `HEAD` rather than the branch's own name, so this reads the same whatever
      // the branch is called; `--set-upstream` so that a branch pushed for the
      // first time is tracked, which is what makes a later pull on it mean
      // anything.
      const result = await ok(GIT, ['push', '--set-upstream', 'origin', 'HEAD'], {
        cwd,
        env: NONINTERACTIVE,
        timeoutMs: NETWORK_MS,
      })
      return { branch: await branchAt(cwd), output: said(result) }
    },
  },

  {
    id: 'git.checkout',
    name: 'Git checkout',
    description: [
      'Switch the checkout at `path` to an existing branch — `master`, usually, to',
      'get back to where you started.',
      '',
      'Only ever an existing branch: making one is `git.push`’s, where there is',
      'somewhere to put it. Changes that would be overwritten stop the switch, so',
      'nothing is lost to one.',
      '',
      '**A branch can only be checked out in one worktree at a time**, so asking a',
      'worktree for `master` is refused while the main checkout has it — git says',
      'which worktree holds it. Switch the checkout that owns the branch, or make',
      'the worktree a different one; there is no way to have it in both, and',
      'nothing here pretends otherwise.',
    ].join('\n'),
    safety: 'dangerous',
    args: z.object({
      path: pathArg('The checkout to switch — `~/repos/x` works'),
      branch: z.string().min(1).describe('Branch to switch to, e.g. `master`'),
    }),
    handler: async ({ path, branch }) => {
      const cwd = directory(path)
      const result = await ok(GIT, ['checkout', branch], { cwd })
      return { branch: await branchAt(cwd), output: said(result) }
    },
  },
]
