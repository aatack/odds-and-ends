# Changesets

A **changeset** is one piece of work, held open across a git worktree, a branch,
a Claude session and a pull request, and written down as an entity so that the
notes can point at it and it can point back. It is how a change to a codebase is
made from inside the outliner: write the notes, press a key, talk to what happens
next.

**The tools themselves are not in this repository.** They are notes under
`@tools` in the store — see [`user-tools.md`](./user-tools.md) — which is what
lets them be edited in the app rather than rebuilt. This file is about the half
that *is* here: what the app and the server provide for them to be written in
terms of, and what a definition has to get right.

| key | tool | what it does |
| --- | --- | --- |
| `shift+k` | **New changeset** | names a piece of work and cuts a worktree for it |
| `k` | **Prompt changeset** | says something to it, and writes both halves down |
| — | **Publish changeset** | commits, pushes and raises a pull request |

Creating one does *not* prompt it. The two gestures stay separate so that
`shift+k` returns as soon as the worktree exists, and the first thing you say is
said the same way as everything after it.

## The entity

`type: changeset`, hanging off the reserved entity **`@changesets`**, so there is
one place to find them all.

| value | what it is |
| --- | --- |
| `text` | what the changeset is called. Also the pull request's title |
| `type` | `changeset` |
| `open` | `true` as it is created: a changeset is a piece of work, so it is a task |
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

1. **The system prompt, once.** A changeset with no `sessionId` has never been
   prompted, so this turn builds one: the changeset's name and id, the id of the
   notes to read, and a tree of rules pasted in whole. Those are read with
   `entity.outline`, which goes to the **store** — nothing on screen has
   necessarily ever looked at that tree — and read *every time a session starts*,
   which is the point of keeping rules as notes rather than as a constant.
2. **The prompt goes down as a note** under the row it was asked from, before the
   answer comes back: a session that runs for an hour should leave the question on
   screen the whole time.
   
   Both ids go out **with every turn**, appended to the prompt, and they are the
   two things that change from one turn to the next: the row the key was pressed
   on, which is the difference between "do this bit" and "do something in here
   somewhere", and the note the prompt landed in, so the session has somewhere to
   write back that isn't the root of everything. The system prompt names `rootId`
   once and never again, which is not enough on its own.
3. **The note is given a pill that watches the turn.** `[@tool:<noteId>](Claude)`
   is appended to it and the same id is passed to the session's call as
   `$callId`, so the question carries a clock while the session runs and says how
   it ended after. It is two writes rather than one — the id doesn't exist until
   the note does.
4. **The session runs**, for as long as it takes; there is no ceiling.
5. **`sessionId` is written** — now, and not in step 1. Its absence is what says
   "this conversation still needs a system prompt", so writing it ahead of a turn
   that then fails would cost the *next* attempt its rules.
6. **The answer is thrown away**, and the prompt says so. Anything worth keeping
   was written into the notes by the session itself; a reply pasted under the
   question as well only said it again, at length, in a voice nobody else in the
   tree uses.
7. **Publish**: commit whatever is loose with the prompt as its message, push, and
   raise a pull request if the branch hasn't got one. Then the URL goes onto the
   changeset.

Every prompt already ends with an instruction to commit, push and raise a pull
request, so step 7 is meant to find nothing to do. It is there for the turn where
the session did the work and stopped short of saying so.

**Step 7 is allowed to fail without taking the turn with it.** By the time it
runs, the worktree has the changes and the question and its outcome are written
down; a repository with no remote would otherwise end every prompt in an error
having done all of the actual work.

## What the repository provides

Nothing here is specific to changesets. Each of these is a tool a definition can
reach through the `tool` façade, by its id.

From the server's integrations
([`server/docs/integrations.md`](../server/docs/integrations.md)):

- `git.createWorktree`, taking a `from` — fetches, then branches off it.
- `git.commitAll`, `git.push`.
- `github.pullRequestForBranch`, then `github.createPullRequest` if that found
  nothing. Both are told the worktree rather than a repo, since `gh` reads the
  `owner/repo` off its remote.
- `claude.runPrompt`, with no time limit and an optional `systemPrompt` read on
  the turn that starts a conversation.

From the app:

- `entity.create` hands back **the id of what it made**, and takes a `values` map
  — so a changeset is one write rather than seven, and the turn's note has an id
  to be named by.
- `entity.outline` reads a branch as markdown **through the store**, for the rules
  and for a pull request's description.
- `entity.get`, `entity.link`, `entity.value.set` for the rest.
- **`$callId`**, passed alongside any tool's arguments, names the call — which is
  what lets `[@tool:<id>](Claude)` in a note watch the turn that note asked for.
  See [`user-tools.md`](./user-tools.md#naming-a-call).

## Writing the definitions

Things a body has to get right, none of which the sandbox will warn about:

- **No `await`.** Calls through `tool` are synchronous by construction. A body
  marked `async` returns a promise, which comes back as nothing.
- **Reach tools by id**, `tool['git.createWorktree']({…})`, rather than by the
  camel case of a label — the labels are prose and the ids are not.
- **Read the context, don't declare an argument for it.** A definition's
  arguments get no `fromContext`, but the folded context is right there:
  `context.entityId` is the selected row, `context.changesetId` the changeset in
  scope, `context.repo` and `context.base` whatever the notes above said. Every
  one of those saves a field in the palette.
- **`execute` is an expression evaluating to a function**, applied to the declared
  arguments positionally.
- **Run "Reload your tools" after editing one.** Definitions are read when the
  source opens.

## Known edges

- **Overlapping prompts don't queue.** Two turns against one changeset are two
  `claude --resume` processes against one transcript, and nothing serialises
  them. One at a time until something does.
- **A turn in flight lives in a worker.** Closing the window or pressing Stop
  loses the note-writing, though not the session — the work is in the worktree
  and the conversation resumes under the same id.
- **A worktree gets no MCP servers by default**, being a directory `claude` has
  never seen. An agent pointed at `rootId` needs whatever configuration lets it
  read the store; that is a machine-level concern and nothing here sets it up.
