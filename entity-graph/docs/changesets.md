# Changesets

A **changeset** is one piece of work, held open across a git worktree, a branch,
a Claude session and a pull request, and written down as an entity so that the
notes can point at it and it can point back. It is how a change to a codebase is
made from inside the outliner: write the notes, press a key, talk to what happens
next.

Two gestures, both in `tools/changesetTools.ts`:

| key | tool | what it does |
| --- | --- | --- |
| `shift+k` | **New changeset** | names a piece of work, cuts a worktree for it, and sends the first prompt |
| `k` | **Prompt changeset** | says something else to a changeset that already exists |

## The entity

`type: changeset`, hanging off the reserved entity **`@changesets`**, so there is
one place to find them all.

| value | what it is |
| --- | --- |
| `text` | what the changeset is called. Also the pull request's title |
| `type` | `changeset` |
| `repo` | the checkout it was cut from |
| `worktree` | the full path of the worktree on this machine |
| `branch` | the branch in it — also the worktree's own id |
| `base` | what the branch was cut from, `origin/master` unless told otherwise |
| `rootId` | the notes it was started from, absent for a changeset with none |
| `sessionId` | the conversation, written when there *is* one. See below |
| `pullRequest` | the URL, once anything has been pushed |

**Two ids, and both are written down.** The changeset's id is the entity's, minted
by the store. The worktree's is git's — six characters, and the branch name too.
Nothing derives one from the other, which is why the path and the branch are both
values on the entity rather than something to reconstruct.

### Which way the links run

Three links, and they are the whole reason this is an entity rather than a record:

- `@changesets` → the changeset, written as it is created.
- the changeset → the notes it was started from, **and** the notes → the
  changeset. Both, so the changeset shows up under the notes you were reading and
  the notes show up under the changeset when you come at it from the list. The
  query's cycle guard is what makes that safe — an entity already in its own
  ancestry is not descended into — so the pair reads as two views rather than a
  loop.

`changesetId` is then written on the **root entity**, not on the changeset. That
is the one that matters: values fold down the path a call is made from, so a
`changesetId` on the notes is in the context of everything underneath them, and
`k` finds the changeset from any row in the subtree without anything having to be
selected in particular.

## A turn

Both tools end in the same place, which is the interesting part.

1. **The system prompt, once.** A session with no `sessionId` on its changeset has
   never been started, so this turn builds one: the changeset's name and id, the
   id of the notes to read, and the rules under
   `c2765f0c-6428-4347-b9d6-cba4744ea0a6` read **out of the store** and pasted in.
   Out of the store because nothing on screen has necessarily ever looked at that
   tree, and out of the store *every time* because that is the point of keeping
   rules as notes: edit the tree and the next session is told something else.
2. **The prompt goes down as a note** under the row it was asked from, before the
   answer comes back — a session that runs for an hour should leave the question
   on screen the whole time.
3. **The session runs**, for as long as it takes; there is no ceiling.
4. **`sessionId` is written** — now, and not in step 1. Its absence is what says
   "this conversation still needs a system prompt", so writing it ahead of a turn
   that then fails would cost the *next* attempt its rules.
5. **The answer goes down** as a child of the prompt, prefixed `*Claude:*`.
6. **The sweep runs**: commit whatever is loose with the prompt as its message,
   push, and raise a pull request if the branch hasn't got one. Then the URL is
   written onto the changeset.

Every prompt already ends with an instruction to commit, push and raise a pull
request, so step 6 is meant to find nothing to do. It is there for the turn where
the session did the work and stopped short of saying so.

**Step 6 is allowed to fail without taking the turn with it.** By the time it
runs, the worktree has the changes and both halves of the conversation are
written down; a repository with no remote would otherwise end every prompt in an
error having done all of the actual work. What went wrong goes in the toast and
in the activity log.

## What it is composed of

Nothing here is new machinery. The tools call the server's integrations directly
(`runIntegration`, in `tools/integrationTools.ts`) rather than through the tool
machine, because none of the individual steps is a thing the user *did*: a fetch
and a `git add` have no business in the activity log, the toasts or the undo
stack.

- `git.createWorktree` with `from: origin/master` — fetches, then branches.
- `claude.runPrompt` with a `systemPrompt` on the turn that starts a session.
- `git.commitAll`, `git.push`.
- `github.pullRequestForBranch`, then `github.createPullRequest` if that found
  nothing. Both are told the worktree rather than a repo, since `gh` can read the
  `owner/repo` off its remote.

See [`server/docs/integrations.md`](../server/docs/integrations.md) for all of
them.

They are the app's tools rather than the store's, though `@tools` could define
them: the composition above is a page of decisions about what happens when a step
half-works, which is a poor fit for an `execute` value edited a line at a time
through an inspector.

## Reaching the notes from a worktree

A worktree is a directory `claude` has never seen, so a session in one gets none
of the MCP servers configured against the repository it came from — and an agent
pointed at `rootId` with no way to read it is no use at all.

The fix is a **`.mcp.json` at the repository root**, which a worktree inherits
along with every other tracked file. It cannot hold the token, so it expands two
variables out of the environment, which the session gets from the server that
spawned it:

```
PENSIVE_MCP_URL=http://127.0.0.1:4000/<sourceId>/mcp
PENSIVE_MCP_TOKEN=<the source token>
```

Both go in `server/.env`, which is gitignored. `.claude/settings.json` carries
`enableAllProjectMcpServers`, because a project-scoped server is normally
approved once by hand and a `--print` session has nobody to ask.

Any repository you want to run changesets against needs the same two files.

## Known edges

- **Overlapping prompts don't queue.** Two turns against one changeset are two
  `claude --resume` processes against one transcript, and nothing here
  serialises them. One at a time until they do.
- **A turn in flight lives in a worker.** Closing the window or pressing Stop
  loses the note-writing, though not the session — the work is in the worktree
  and the conversation resumes under the same id.
- **`base` is read from the context**, not asked for: write `base` on a note
  above the work to cut branches from something other than `origin/master`.
  Likewise `repo`, which saves typing the checkout path every time.
