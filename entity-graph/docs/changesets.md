# Changesets

A **changeset** is one piece of work, held open across a git worktree, a branch,
a Claude session and a pull request. It is how a change to a codebase is made
from inside the outliner: write the notes, press a key, talk to what happens
next.

**The tools themselves are not in this repository.** They are notes under
`@tools` in the store — see [`user-tools.md`](./user-tools.md) — which is what
lets them be edited in the app rather than rebuilt. This file is about the half
that *is* here: what the app and its integrations provide for them to be written in
terms of, and what a definition has to get right.

| key | tool | what it does |
| --- | --- | --- |
| `k` | **Prompt Claude** (`changeset.prompt`) | says something to the session on the frame's root; a new one gets a worktree |
| `shift+k` | **Prompt Claude anonymously** (`changeset.promptAnonymously`) | the same, but a new one runs in an empty scratch directory |
| — | **Run Claude session** (`changeset.run`) | what both keys call |
| — | **Publish changeset** (`changeset.publish`) | commits, pushes and raises a pull request |

## The session lives on the notes

There is no entity for a changeset. The **root entity of the frame** the key was
pressed in holds the whole of it, as one value, `claudeSession`. So any row in
that frame talks to the same conversation, and the notes are where you come back
to it.

| key in `claudeSession` | what it is |
| --- | --- |
| `name` | the root's text, first line, as the session started. Also the pull request's title |
| `anonymous` | `true` for a session with no worktree |
| `repo` | the checkout the worktree was cut from — `repo` in the context |
| `worktree` | the full path of the worktree on this machine |
| `branch` | the branch in it — also the worktree's own id |
| `base` | what the branch was cut from — `base` in the context, `origin/master` otherwise |
| `sessionId` | the conversation, written when there *is* one. See below |
| `pullRequest` | the URL, once anything has been pushed |

**The first turn makes the session.** It is written on the root, and the root is
given `open: true`, so the notes come up in the stack until the work is done.

**The key picks the kind of a new session only.** A root has one session, and a
turn never turns one kind into the other: `shift+k` on a root with a worktree
session, or `k` on an anonymous one, is an error rather than a second
conversation. Blank `claudeSession` to start again.

**An anonymous session has no directory written down.** `claude.runPrompt` is
called with no `path`, and runs in an empty scratch directory named for the
session id — the same one every turn, which is what lets it resume. It is told
to make no code changes, and there is nothing to publish or clean up.

**Notes from before this** carry `changesetId`, naming a `type: changeset` entity
under `@changesets`. The first turn on such a root copies that entity's worktree,
branch and session into `claudeSession`, and carries on with them.

### The buttons

`changeset` is a type like any other, written in the store under `@types` — see
[`types.md`](./types.md) — and its `actions` name four more tools under `@tools`,
one button each on every changeset row. They are the gestures that have nothing
to do with the conversation: what to do with the branch once it exists.

| button | tool | what it does |
| --- | --- | --- |
| **Check out** | `changeset.checkout` | stand the *original* repo on the commit the worktree is at |
| **Check out master** | `changeset.checkoutMaster` | put it back on the branch `base` names, and pull |
| **Merge** | `changeset.merge` | merge the pull request, and tick the changeset off |
| **Open PR** | `changeset.openPullRequest` | open the pull request in the browser |

None of them declares an argument. Each asks `changeset.here` which session it
is on: the row itself when that is an old changeset or holds a `claudeSession`,
then the frame's root, then the `changesetId` folded down from old notes. That is
what makes them work from the palette as well as from a button.

**Check out is detached**, and has to be: the worktree is still holding the
branch, and git will only let one checkout have it. So `git.checkout` is asked
for the branch with `detach`, which takes the commit and leaves the branch where
it is — the change can be looked at, built and run in the checkout it came from
without the session losing its own.

**Check out master pulls afterwards.** Going back is half of what it is for; the
other half is being current again once the pull request has merged, and a
checkout on its own is not that. The pull is `--ff-only`, so a branch that has
diverged says so rather than inventing a merge.

The branch it switches to is `base` with the remote stripped — `origin/master` →
`master` — rather than the word `master` written in, so a changeset cut from
somewhere else goes back to where it came from.

Merging is the one thing that finishes a changeset, and nothing else watches for
it, so the button that merges is also the one that writes `open: false` — on the
notes holding the session. The
branch is left behind on purpose: the worktree still holds it, and having `gh`
delete it out from under one is not a tidy-up.

## A turn

1. **The system prompt, once.** A session with no `sessionId` has never been
   prompted, so this turn builds one: whether it has a worktree, the id and name
   of the notes to read, and a tree of rules pasted in whole. Those are read with
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
   write back that isn't the root of everything. The system prompt names the root
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
   raise a pull request if the branch hasn't got one. Then the URL goes into
   `claudeSession`. An anonymous session skips this.

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

From the app's integrations ([`docs/integrations.md`](./integrations.md)):

- `git.createWorktree`, taking a `from` — fetches, then branches off it.
- `git.commitAll`, `git.push`, `git.pull`.
- `git.checkout`, with `detach` for the branch a worktree is already holding.
- `github.pullRequestForBranch`, then `github.createPullRequest` if that found
  nothing. Both are told the worktree rather than a repo, since `gh` reads the
  `owner/repo` off its remote.
- `github.mergePullRequest`, which the **Merge** button is.
- `claude.runPrompt`, with no time limit, an optional `systemPrompt` read on
  the turn that starts a conversation, and a scratch directory when no `path` is
  given.

From the app:

- `entity.create` hands back **the id of what it made**, so the turn's note has an
  id to be named by.
- `entity.outline` reads a branch as markdown **through the store**, for the rules
  and for a pull request's description.
- `entity.get`, `entity.link`, `entity.value.set` for the rest.
- `link.open`, which hands a URL to the desktop's own browser rather than opening
  a window of this app on it.
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
  `context.entityId` is the selected row, `context.rootId` the frame's root,
  `context.repo` and `context.base` whatever the notes above said. Every
  one of those saves a field in the palette.
- **`execute` is an expression evaluating to a function**, applied to the declared
  arguments positionally.
- **Run "Reload your tools" after editing one.** Definitions are read when the
  source opens.

## Known edges

- **Overlapping prompts don't queue.** Two turns against one session are two
  `claude --resume` processes against one transcript, and nothing serialises
  them. One at a time until something does.
- **A turn in flight lives in a worker.** Closing the window or pressing Stop
  loses the note-writing, though not the session — the work is in the worktree
  and the conversation resumes under the same id.
- **A worktree gets no MCP servers by default**, being a directory `claude` has
  never seen, and a scratch directory is the same. An agent pointed at the notes
  needs whatever configuration lets it read the store; that is a machine-level concern and nothing here sets it up.
