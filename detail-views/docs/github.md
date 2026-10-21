# GitHub

`modules/github/` — `view.ts` (pure: ids, badge, name, actions, order),
`github.ts` (queries and writes through `gh`).

## Reads

- Only `gh api graphql` queries; `query()` refuses a mutation. `gh` runs from
  the temp dir.
- `github` (`github.home`) `children`, fresh 2 min: my open PRs (search), each
  summary written at 0 and marked `loaded.self`; the list replaces its links.
- `github:pr:<url>` `self` 10 min, `children` 1 min: one full query, joined if
  both parts ask at once, writes both flags. Description, comments and reviews
  are written at `createdAt` (and so are their links from the PR); state,
  title, checks at 0. Only failing and pending checks get rows.
- Reloading every PR every few minutes is acceptable.
- **The watch** (`Core.pollGitHub`, every minute from `Core.start`): loads
  the list, and every PR in an open topic's chat, each only once it is no
  longer fresh (so the list every 2 min, a PR every 10). Without it, only
  what is on screen loads, and the topics mode never shows the list: it went
  stale for hours. The plan for a notifications poll is in `refetch.md`.

## What stands in the way (`blockers` in `view.ts`)

Every view of a PR leads with **one** thing: the first of its blockers, as a
quiet chip where a dot would be (row, pill, and so the overview's heading).
Once it's done, the next one shows. Each has whose move it is (`me`, `them`,
`none`); mine is the same chip, a touch heavier.

The order is my workflow. Merged or closed first, whoever's it is.

- **Mine:** approve it myself ("Approve it": on GitHub *or* here, the same
  rule as the Approve button, `approvedMine`); then CI failing, conflicts,
  changes requested (to fix); then someone else's review ("Awaiting …");
  then CI running; then merging: "Merge it" until auto-merge is on (which
  approving turns on), "Auto-merging" once it is.
- **Someone else's:** mine until I approve it ("Review it", or "Review
  requested" when asked); "You requested changes" (theirs) once I asked for
  changes, until they ask me again; then "Approved", only waiting for it to
  be merged.

What doesn't change what I do is not shown: unresolved threads, behind its
base, a draft (approving mine marks it ready), anything on someone else's PR
once I've approved it.

From GitHub, per PR: `reviewDecision`, `latestReviews`, `reviewRequests`,
`reviewThreads.isResolved`, `mergeable`, `mergeStateStatus`, the check rollup,
`isDraft`, `autoMergeRequest`.

## Previews

PRs in repos listed in `previews` (`view.ts`) have a preview deployment per
PR number (`theengineeringco/branch-demo`: `https://pr-<n>.preview.theeng.co/demo`).
An open one's overview starts with a **Preview** link: hovering it peeks at
the running branch.

## Writes

**Approve is derived, not remembered.** On someone else's PR it is done when
GitHub's latest reviews include mine (`approvedByMe`). On mine, GitHub won't
let me approve, so "approved" is GitHub's review decision (`review ===
'APPROVED'`) or my own approval here (`github.localApproval`); approving marks
a draft ready for review (`gh pr ready`), then turns on auto-merge. Once it is
approved and auto-merge is on, **Approve** becomes **Bump** (the same key,
`p`): it writes nothing, and puts the PR's line (below) on the clipboard
again, to pass on. Approved without auto-merge (on GitHub, say), it is
**Auto-merge**. Both come from the latest PR load. On my own PR these ask
nothing (`Action.prompt` null): the line is copied at once, then it is done.

**Fix** (mine, while CI fails, it has conflicts or changes were requested;
no key): every Claude session in the PR's context (`sessionsInContext`: a
task topic, or a Claude module session) is asked to fix it on its branch and
commit, not push (`Core.fixPr`). A topic gets it as a message and dispatches
at once, unless Claude is still answering there (then it goes with the
next). Disabled when its context has none. Pushing stays mine (a topic's
**Push**).

Only the allowlist in `checkWrite`: approve someone else's PR; on mine,
approve locally (an owned `github.localApproval` child), mark a draft ready
for review (never back to a draft), and turn on auto-merge; close mine and
delete the branch. Each is started by me: approving someone else's and
closing mine are confirmed with Enter (each takes an optional comment);
approving mine is done at once.

Approving mine puts a line naming the PR on the clipboard, at once, to pass on: `[#1234](<PR url>) Name - [preview](<preview url>)`, the preview
only for a repo with previews (`prLine`). Any action can do this: its
`Action.copy` is copied when it succeeds, through `Api.copy` (Electron's
clipboard on the desktop, the browser's on the phone).

The buttons are on a PR's overview wherever it is drawn, a peek included: a
button there opens the prompt in the peek (`acting.peek`), which stays open
until the action is confirmed or given up. The keys (`p`, `X`) act on the
view's root only.
