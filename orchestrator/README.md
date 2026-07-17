# Orchestrator

A local desktop tool for managing the things I'm working on — GitHub pull
requests, agents, tickets — and the relationships between them. It's an
**Electron + React + TypeScript** app backed by a local **SQLite** database.

It lives in the `local-helpers` monorepo alongside `pr-review-helper`, but is a
**separate project** with its own dependencies. (The PR helper is a Vite web app;
this is an Electron desktop app.)

> The current landing page is an **example** built on a placeholder `task` item
> type, so the visual style and the architecture can be reviewed end to end. See
> [`AGENTS.md`](./AGENTS.md) for the design principles and conventions.

## Architecture

- **Items model** — a central `items` table keyed by `(type, id)` with shared
  `created_at` / `archived_at`; a `links` table for directional links between any
  two items; and per-type tables (`tasks`) for type-specific data.
- **State** in three tiers: local `useState`, session Jotai atoms (the loaded DB
  snapshot, the action log), and persisted state (SQLite for real data;
  `localStorage` for throwaway UI state like the current selection).
- **Actions** — every mutation goes through a registry (`src/renderer/actions/`):
  each action has a name, a Zod param schema, and a run function. Actions are
  logged to an audit trail; async ones track pending/failed and can be re-run
  with their original arguments. The same registry backs the UI, hotkeys, and the
  ⌘K command palette.
- **Reusable components** — one `TextEditor`, plus `ItemPill` / `ItemCard` /
  `ItemDetail`, whose type-specific bits come from a single `ItemView` registry.

```
src/main      Electron main — window, SQLite, IPC handlers
src/preload   contextBridge exposing the typed API to the renderer
src/shared    types + IPC contract (shared by main & renderer)
src/renderer  React app (state / actions / helpers / components / views)
```

## Run

Prerequisites: **Node 20+** and a C toolchain (`better-sqlite3` is a native
module — on Debian/Ubuntu: `sudo apt install build-essential python3`). From this
directory:

```bash
npm install      # also rebuilds better-sqlite3 for Electron (postinstall)
npm run dev      # launches the Electron app with hot reload
```

If the native module ever complains after an Electron upgrade, rebuild it:

```bash
npm run rebuild
```

The SQLite database is created on first launch under Electron's per-user data
directory (`app.getPath("userData")/orchestrator.db`) and seeded with a few
example tasks.

## Scripts

| Script | Does |
|--------|------|
| `npm run dev` | Electron app + Vite hot reload |
| `npm run build` | Production build (`out/`) |
| `npm start` | Preview the production build |
| `npm run type-check` | `tsc --noEmit` for main/preload and renderer |
| `npm test` | Vitest |
| `npm run rebuild` | Rebuild `better-sqlite3` against the installed Electron |

## What the landing page shows

- **Inbox** (left): `ItemCard`s for each task — draggable, and drop targets
  (drop one card on another to link them).
- **Detail** (centre): the selected item in `ItemDetail`, with an inline
  `TextEditor` for the title/description, status controls, a **Debug** panel
  (linked items as pills + metadata), archive/restore, and drop-to-link.
- **Pills** and an **expand-in-place** card, to show those variants.
- **Activity** (right): the live action log — pending/failed counts, with
  **Retry** on failed actions.
- **⌘K**: the command palette, running actions straight from the registry.
