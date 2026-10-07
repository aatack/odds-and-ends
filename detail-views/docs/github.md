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
- Reloading every PR every few minutes is acceptable; GitHub has no watch here.
  The plan for a notifications poll is in `refetch.md`.

## What stands in the way (`blockers` in `view.ts`)

A PR's views lead with what is between it and being merged, most pressing
first, each with whose move it is (`me`, `them`, `none`). The first one is the
badge, the pill's coloured word and the row's chip; the overview lists them
all, mine filled.

In order: merged / closed (and nothing else); draft; my review (mine:
approving it here; theirs: reviewing it, "requested of you" when asked);
changes requested (mine: mine to answer; theirs: on them, "you requested
changes" if it was me); unresolved review threads; others' reviews awaited
(with who was asked); CI failing; merge conflicts (`mergeable` or
`mergeStateStatus: DIRTY`); behind its base (`BEHIND`); CI running; and once
clear: "will auto-merge", "ready: merge it" (mine), "approved by you" or
"ready" (theirs).

From GitHub, per PR: `reviewDecision`, `latestReviews`, `reviewRequests`,
`reviewThreads.isResolved`, `mergeable`, `mergeStateStatus`, the check rollup,
`isDraft`, `autoMergeRequest`.

## Writes

**Approve is derived, not remembered.** On someone else's PR it is done when
GitHub's latest reviews include mine (`approvedByMe`). On mine, GitHub won't
let me approve, so "approved" is GitHub's review decision (`review ===
'APPROVED'`) or my own approval here (`github.localApproval`); approving turns
on auto-merge, so the button is off once approved and auto-merge is on. Both
come from the latest PR load.

Only the allowlist in `checkWrite`: approve someone else's PR; on mine,
approve locally (an owned `github.localApproval` child) and turn on
auto-merge; close mine and delete the branch. Each is started by me and
confirmed with Enter.
