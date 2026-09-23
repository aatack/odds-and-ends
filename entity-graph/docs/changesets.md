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
| `k` | **Prompt Claude** (`changeset.prompt`) | asks the selected note of the session on the path; a new one gets a worktree |
| `shift+k` | **Prompt Claude anonymously** (`changeset.promptAnonymously`) | the same, but a new one runs in an empty scratch directory |
| — | **Run Claude session** (`changeset.run`) | what both keys call |
| — | **Publish changeset** (`changeset.publish`) | commits, pushes and raises a pull request |

## The session lives on the notes

There is no entity for a changeset. A new session is written on the **root
entity of the frame** the key was pressed in, as one value, `claudeSession`. A
turn carries on whichever session is nearest on the path down to the selected
row — the one the folded context shows — so any row under it talks to the same
conversation, and the notes are where you come back to it.

| key in `claudeSession` | what it is |
| --- | --- |
| `name` | the root's text, first line, as the session started. Also the pull request's title |
| `anonymous` | `true` for a session with no worktree |
| `repo` | the checkout the worktree was cut from — `repo` in the context |
| `worktree` | the full path of the worktree on this machine |
| `branch` | the branch in it — also the worktree's own id |
| `base` | what the branch was cut from — `base` in the context, `origin/master` otherwise |
| `sessionId` | the conversation, written when there *is* one: the id `claude.runPrompt` made up for it on the first turn |
| `pullRequest` | the URL, once anything has been pushed |

**The first turn makes the session.** It is written on the root. Every turn then
gives the notes holding the session `open: { toolCall: <note id> }` — the turn's
own call — so they come up in the stack once the turn is over, and not while
there is nothing to do but wait for it. See [`open.ts`](../src/core/open.ts).

**The key picks the kind of a new session only.** With a session on the path,
`k` and `shift+k` do the same thing: carry it on. Without one, `k` wants a
repository — `repo` from the context, or else asked for, with the repositories
recent calls were given to pick from — and `shift+k` passes none, which makes
the session anonymous. Blank `claudeSession` to start again.

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
2. **The prompt is the selected note's text.** Nothing is typed: press `k` on
   each of a list of things to be done. Its id, its text and the frame root's id
   go out **with every turn**, appended to the prompt, so the session knows which
   note it is answering and hangs its reply under it. The system prompt names the
   notes once and never again, which is not enough on its own.
3. **The note is given a pill that watches the turn.** `[@tool:<noteId>](Claude)`
   is appended to it — replacing the last turn's — and the same id is passed to
   the session's call as `$callId`, so the question carries a clock while the
   session runs and says how it ended after.
4. **The session runs**, for as long as it takes; there is no ceiling.
5. **`sessionId` is written** — now, and not in step 1, as the id the run
   hands back. Its absence is what says
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
  the turn that starts a conversation, a scratch directory when no `path` is
  given, and a new session under a fresh id when no `sessionId` is.

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
- **Read the context, don't declare an argument for it** — unless it has to be
  asked for when the context is silent, as `k`'s `repo` is; then see
  `fromContext`, `unlessContext` and `recent` in
  [`user-tools.md`](./user-tools.md#arguments). The folded context is right there:
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
