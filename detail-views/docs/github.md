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

## Writes

Only the allowlist in `checkWrite`: approve someone else's PR; on mine,
approve locally (an owned `github.localApproval` child) and turn on
auto-merge; close mine and delete the branch. Each is started by me and
confirmed with Enter.
