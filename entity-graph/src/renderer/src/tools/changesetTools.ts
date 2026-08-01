import { str } from '../../../core/entity'
import { createEntity, link, readEntities, readOutline, writeValue } from '../source/entity'
import { runIntegration } from './integrationTools'
import type { ArgSpec, CallInfo, ToolSpec } from './types'

// A changeset: one piece of work, held open across a worktree, a branch, a Claude
// session and a pull request, and written down as an entity so the notes can
// point at it and it can point back.
//
// Two gestures, and the second is the one that does the work. `shift+k` names a
// changeset and starts it; `k` says something else to it. Everything between —
// the fetch, the branch, the session, the commit, the push, the pull request — is
// composed here out of the server's integrations, because none of those steps is
// a thing the user did and none of them belongs in the log on its own.
//
// The tools are the app's rather than the store's, though the store could define
// them: the composition below is a page of decisions about what happens when a
// step half-works, which is a poor fit for an `execute` value edited a line at a
// time in an inspector.

/** Every changeset hangs off this, so there is one place to find them all. */
const CHANGESETS_ID = '@changesets'

/**
 * The tree of rules an agent is given, read fresh on the turn that starts a
 * session. A store-specific id in the app's source, which is not lovely — but the
 * alternative is a tool that has to be told where its own instructions live every
 * time it runs, and there is exactly one answer.
 */
const RULES_ID = 'c2765f0c-6428-4347-b9d6-cba4744ea0a6'

/** What a worktree is branched off when nothing in the context says otherwise. */
const DEFAULT_BASE = 'origin/master'

/**
 * Said at the end of every prompt, first and follow-up alike. The session is the
 * only thing standing in the worktree, so it is the only thing that can commit;
 * `git.commitAll` behind it is a sweep for what it forgot, not the plan.
 */
const PUBLISH = [
  '',
  '---',
  'When you are done: commit what you changed, push the branch, and open a pull',
  'request for it if there is not one already.',
].join('\n')

/** How Claude's answers are marked apart from the prompts they answer. */
const SAID_BY_CLAUDE = '*Claude:*'

const text = (v: unknown): string => String(v ?? '').trim()

/** A value off an entity that has to be there for the next step to mean anything. */
function required(values: Record<string, unknown>, key: string, changesetId: string): string {
  const value = str(values[key])
  if (!value) throw new Error(`Changeset ${changesetId} has no \`${key}\` on it`)
  return value
}

// --- The changeset entity ----------------------------------------------------

interface Changeset {
  id: string
  values: Record<string, unknown>
}

/**
 * The changeset, read from the store rather than the cache. A script and a tool
 * composing several writes are in the same position: one answer, no second look,
 * and nowhere to put "not yet".
 */
async function readChangeset(changesetId: string): Promise<Changeset> {
  const entity = (await readEntities([changesetId]))[changesetId]
  const values = entity?.values ?? {}
  if (str(values.type) !== 'changeset') {
    throw new Error(`${changesetId} isn't a changeset — is the cursor inside one?`)
  }
  return { id: changesetId, values }
}

/**
 * The changeset a call is aimed at. Named outright if the palette was given one,
 * and otherwise the `changesetId` folded out of the notes around the cursor,
 * which is what makes `k` work from anywhere inside a changeset's notes without
 * anything being selected in particular.
 */
function changesetOf(args: Record<string, unknown>, call: CallInfo): string {
  const named = text(args.changesetId) || text(call.context.values.changesetId)
  if (!named) {
    throw new Error(
      'No changeset here — `changesetId` is written on the notes a changeset was started from',
    )
  }
  return named
}

// --- The system prompt -------------------------------------------------------

/**
 * What a session is told before it is told anything else, built on the turn that
 * starts it and never again — a resumed conversation already has the system
 * prompt it began with, so there would be nothing to do with a second one.
 *
 * The rules come out of the store on every start rather than being baked in here,
 * which is the whole point of keeping them as notes: editing the tree changes what
 * the next session is told. Read through the *source*, not the cache, since
 * nothing has necessarily ever looked at that tree in this window.
 */
async function systemPrompt(changeset: Changeset, rootId: string | null): Promise<string> {
  const rules = await readOutline(RULES_ID).catch(() => '')
  const name = str(changeset.values.text)
  return [
    'You are working on a changeset: one piece of work in a worktree of its own,',
    'tracked as a note in a graph you can read and write over the `pensive-notes`',
    'MCP tools.',
    '',
    `Changeset: \`${changeset.id}\`${name ? ` — ${name}` : ''}`,
    ...(rootId
      ? [
          `The notes describing this work are under entity \`${rootId}\`. Read them`,
          'before you start, and write anything worth keeping back to them.',
        ]
      : ['This changeset was started without notes behind it; the prompt is all there is.']),
    ...(rules ? ['', rules] : []),
  ].join('\n')
}

// --- Publishing --------------------------------------------------------------

/** What a pull request is called, and what it says. */
async function description(changeset: Changeset, rootId: string | null): Promise<string> {
  const notes = rootId ? await readOutline(rootId).catch(() => '') : ''
  return [`Changeset \`${changeset.id}\``, ...(notes ? ['', notes] : [])].join('\n')
}

/**
 * Get whatever the session did out of the worktree and onto a pull request.
 *
 * Every step of this is meant to be a no-op: the session was asked to commit and
 * push and raise the pull request itself, and mostly does. This is what catches
 * the turn where it didn't — a stray file, a branch never pushed, a session that
 * did the work and stopped short of saying so.
 */
async function publish(changeset: Changeset, message: string): Promise<Record<string, unknown>> {
  const path = required(changeset.values, 'worktree', changeset.id)
  const branch = required(changeset.values, 'branch', changeset.id)

  const committed = (await runIntegration('git.commitAll', { path, message })) as {
    committed: boolean
  }
  await runIntegration('git.push', { path })

  const found = (await runIntegration('github.pullRequestForBranch', { path, branch })) as {
    pullRequest: { url?: string; number?: number } | null
  }
  if (found.pullRequest?.url) {
    return { swept: committed.committed, pullRequest: found.pullRequest.url, raised: false }
  }

  const rootId = str(changeset.values.rootId) ?? null
  const raised = (await runIntegration('github.createPullRequest', {
    path,
    title: str(changeset.values.text) ?? changeset.id,
    body: await description(changeset, rootId),
  })) as { url: string; number: number | null }
  return { swept: committed.committed, pullRequest: raised.url, raised: true }
}

// --- Prompting ---------------------------------------------------------------

interface Prompted {
  /** What Claude said. */
  result: string
  /** The publish step's account of itself, or why it couldn't run. */
  published: Record<string, unknown>
}

/**
 * One turn: start the session if it hasn't been started, say the thing, write
 * both halves down, and then sweep up after it.
 *
 * The session id is written on the changeset the first time it is prompted and
 * not when the changeset is made, and the order matters twice over. It is how
 * this knows whether to build a system prompt — there is no second chance at
 * one — and it means a changeset that was never prompted carries no claim to a
 * conversation that doesn't exist.
 */
async function prompt(changeset: Changeset, ask: string, under: string): Promise<Prompted> {
  const path = required(changeset.values, 'worktree', changeset.id)
  const rootId = str(changeset.values.rootId) ?? null

  const existing = str(changeset.values.sessionId)
  // Derived from the changeset, so the same changeset always names the same
  // conversation and nothing has to be minted or remembered. The CLI hashes
  // anything that isn't a uuid into one, and scopes it to the directory, so this
  // is a name rather than an identifier.
  const sessionId = existing ?? `changeset-${changeset.id}`
  const system = existing ? null : await systemPrompt(changeset, rootId)

  // Written before the answer comes back, so a session that runs for an hour
  // leaves the question on screen the whole time rather than nothing at all.
  const noteId = await createEntity({ text: ask }, under)

  const answer = (await runIntegration('claude.runPrompt', {
    path,
    sessionId,
    prompt: `${ask}\n${PUBLISH}`,
    ...(system ? { systemPrompt: system } : {}),
  })) as { result?: string }

  // And the session id only once there is a session: the presence of this value
  // is what says the conversation has a system prompt already, so writing it
  // ahead of a turn that then fails would cost the *next* attempt its rules.
  if (!existing) await writeValue(changeset.id, 'sessionId', sessionId)

  const said = text(answer.result) || '(said nothing)'
  await createEntity({ text: `${SAID_BY_CLAUDE} ${said}` }, noteId)

  // Everything above is now written down, which is why this can be allowed to
  // fail without taking the turn with it: a repository with no remote would
  // otherwise end every prompt in an error, having done all of the actual work.
  // The message says what happened; the log keeps the rest.
  let published: Record<string, unknown>
  try {
    published = await publish(changeset, ask)
    if (published.pullRequest) {
      await writeValue(changeset.id, 'pullRequest', published.pullRequest)
    }
  } catch (e) {
    published = { error: e instanceof Error ? e.message : String(e) }
  }
  return { result: said, published }
}

/** What the toast says about a turn: what came back, and what got out. */
const report = (published: Record<string, unknown>): string => {
  if (typeof published.error === 'string') return `answered, but couldn't publish: ${published.error}`
  if (published.raised) return `answered, and raised ${published.pullRequest}`
  return published.pullRequest ? `answered, and pushed to ${published.pullRequest}` : 'answered'
}

// --- The tools ---------------------------------------------------------------

/**
 * Optional, though the tool cannot run without one. A required argument is
 * refused before `run` is reached, and the context is only *laid into* arguments
 * by a gesture — a script calling this has its changeset in the context it passed
 * and nowhere else. So the check belongs in {@link changesetOf}, which can look
 * in both places and say something useful when neither has one.
 */
const changesetArg: ArgSpec = {
  name: 'changesetId',
  label: 'Changeset id',
  kind: 'entity',
  fromContext: 'changesetId',
  optional: true,
}

export const CHANGESET_TOOLS: ToolSpec[] = [
  {
    id: 'changeset.create',
    label: 'New changeset',
    aliases: ['start work', 'dispatch', 'worktree', 'agent', 'implement these notes'],
    hint: 'Changeset',
    scope: 'frame',
    reach: 'external',
    mutates: true,
    keys: [{ key: 'k', shift: true }],
    args: [
      { name: 'name', label: 'Changeset name', placeholder: 'What this piece of work is' },
      {
        name: 'repo',
        label: 'Repository path',
        fromContext: 'repo',
        placeholder: '~/repos/something',
      },
      {
        name: 'rootId',
        label: 'Notes to work from',
        kind: 'entity',
        fromContext: 'entityId',
        optional: true,
      },
      {
        name: 'instructions',
        label: 'Anything else to say',
        optional: true,
        placeholder: 'Left empty: implement the notes',
      },
    ],
    run: async (args, call) => {
      const name = text(args.name)
      if (!name) throw new Error('A changeset needs a name')
      const repo = text(args.repo)
      if (!repo) throw new Error('Which repository? Give the path to a checkout of it')
      const rootId = text(args.rootId) || null
      // Not an argument: a fifth prompt for something that is `origin/master`
      // every time is worse than a value written once on the notes above.
      const from = str(call.context.values.base) ?? DEFAULT_BASE

      const worktree = (await runIntegration('git.createWorktree', { path: repo, from })) as {
        path: string
        branch: string
      }

      // One write rather than seven, and the id it comes back with *is* the
      // changeset id — there is no second identifier to keep in step, and the
      // link from `@changesets` is made on the way in.
      const changesetId = await createEntity(
        {
          text: name,
          type: 'changeset',
          repo,
          worktree: worktree.path,
          branch: worktree.branch,
          base: from,
          ...(rootId ? { rootId } : {}),
        },
        CHANGESETS_ID,
      )

      if (rootId) {
        // Both ways round, so the changeset is visible from the notes and the
        // notes from the changeset. The query's cycle guard is what makes that
        // safe: an entity that is already its own ancestor is not descended into.
        await link(changesetId, rootId)
        await link(rootId, changesetId)
        // On the *root*, not the changeset: this is what folds into the context of
        // everything underneath, and so what `k` finds from anywhere in the notes.
        await writeValue(rootId, 'changesetId', changesetId)
      }

      const changeset = await readChangeset(changesetId)
      const ask =
        text(args.instructions) ||
        (rootId ? `Implement the notes under \`${rootId}\`.` : `Get started on: ${name}`)
      const { published } = await prompt(changeset, ask, rootId ?? changesetId)

      return {
        data: { changesetId, worktree: worktree.path, branch: worktree.branch, published },
        message: `${name}: ${report(published)}`,
      }
    },
  },

  {
    id: 'changeset.prompt',
    label: 'Prompt changeset',
    aliases: ['ask', 'follow up', 'continue', 'claude', 'say'],
    hint: 'Changeset',
    scope: 'frame',
    reach: 'external',
    mutates: true,
    keys: [{ key: 'k' }],
    args: [
      { name: 'prompt', label: 'What to say', placeholder: 'Asked of this changeset’s session' },
      changesetArg,
    ],
    run: async (args, call) => {
      const ask = text(args.prompt)
      if (!ask) throw new Error('Nothing to say')
      const changeset = await readChangeset(changesetOf(args, call))
      // Under the row the cursor is on, which is where the conversation belongs —
      // an answer about one part of the notes hangs off that part. Cold from the
      // palette there is no row, so the changeset itself takes them.
      const under = text(call.context.values.entityId) || changeset.id
      const { published } = await prompt(changeset, ask, under)
      return { data: { changesetId: changeset.id, published }, message: report(published) }
    },
  },
]
