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

Every view of a PR leads with **one** thing: the first of its blockers, as a
quiet chip where a dot would be (row, pill, and so the overview's heading).
Once it's done, the next one shows. Each has whose move it is (`me`, `them`,
`none`); mine is the same chip, a touch heavier.

The order is the one I asked for: merged / closed; not reviewed by me (mine:
not approved, on GitHub *or* here, the same rule as the Approve button,
`approvedMine`; theirs: not approved by me, "review requested" when asked);
waiting on someone else's review (and who); CI failing; conflicts; CI
running; then clear ("auto-merging", "ready to merge", "approved", "ready").
Slotted in: a draft first; changes requested and unresolved threads after
awaited reviews; behind its base after conflicts.

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
'APPROVED'`) or my own approval here (`github.localApproval`); approving turns
on auto-merge, so the button is off once approved and auto-merge is on. Both
come from the latest PR load.

Only the allowlist in `checkWrite`: approve someone else's PR; on mine,
approve locally (an owned `github.localApproval` child) and turn on
auto-merge; close mine and delete the branch. Each is started by me and
confirmed with Enter.
