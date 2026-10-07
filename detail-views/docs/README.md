# Docs

How detail-views is built and why. `CLAUDE.md` at the root holds the rules;
these hold the reasoning and the algorithms behind them. Read the one for the
area you are changing before changing it, and update it in the same commit.

| File | Covers |
|-|-|
| [architecture.md](architecture.md) | The layers, what runs where, and the path from a service to the screen |
| [events.md](events.md) | The event model, the two stores, timestamps, migrations |
| [frontend-cache.md](frontend-cache.md) | The entity cache: reading, invalidating, loading from services |
| [presentation.md](presentation.md) | Module views, `focusOf`, the bounded walk, ordering |
| [slack.md](slack.md) | Slack: lists, the history batch, the watch, cursors, threads |
| [claude.md](claude.md) | **Claude**: requirements and key decisions (ASD-STE100) |
| [github.md](github.md) | GitHub: what loads, and the PR writes |
| [mobile.md](mobile.md) | **The phone**: setting it up, using it, how it works, security |
| [decisions.md](decisions.md) | Decisions made, with the alternatives turned down |
| [refetch.md](refetch.md) | Refetch strategy per item type, and what is still to do (ASD-STE100) |
