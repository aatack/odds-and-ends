# Frontend state model

The renderer's state model, as refactored. This document records the decisions
made while turning the design notes into code — particularly the points the notes
left open, so the source of truth can be updated from here.

The organising rule is **state / tools / views are separate layers**, each
depending only on the ones above it:

```
source/   the transport seam (call a source tool, as a given user)
state/    latent, serialisable state + pure derivations + the query cache
tools/    every user-triggerable command, and the pending-call state machine
keys/     key routing (top level only)
views/    React — reads derived state, renders, forwards gestures to tools
```

Nothing in `state/` or `tools/` imports React or touches the DOM, so the app can
in principle be driven headlessly: point `source/transport` at a source, retain a
frame, and run tools by id.

## Vocabulary

A **tool** is anything the user can invoke through the command palette — not
just the small subset of actions the source exposes over its tool API. Moving
the selection, opening a tab and toggling the theme are all tools, alongside
`writeValue`-backed ones. Each declares its arguments once, and both hotkeys and
the palette dispatch through that single declaration, so they cannot drift.

A tool's `reach` says how far it reaches: `ui` (frontend state only), `source`
(reads/writes the entity store) or `external` (touches the outside world).

A **call** is one invocation of a tool: a generated `callId`, the tool's id, the
argument values so far, and an immutable snapshot of the context it started in.

## Argument values

Each argument value is a discriminated union rather than a bare value, because
"not supplied yet" and "use the tool's default" are genuinely different states,
and the source's tool contract already spends `null` on the latter
(`stripNulls` in `core/source/types.ts`):

```ts
type ArgValue =
  | { kind: 'empty' }              // not supplied yet
  | { kind: 'default' }            // use the tool's default (serialises to null)
  | { kind: 'value'; value: unknown }
```

State stores **parsed values**, not the raw text the user typed, so validation
happens when the user presses Tab or Enter rather than at execution time. The
palette keeps the in-progress text in local component state and writes the
parsed value through on every keystroke that parses; a keystroke that doesn't
parse leaves the last good value in state and only reports the error when the
user tries to advance. That way a call abandoned mid-word still resumes with
what was typed.

## Arguments and the context

Every argument declares whether it may be filled from the call's context
(`fromContext: 'entityId'`, etc.). Nothing is auto-filled by name alone — an
opt-out-by-default flat fold would prefill "Create child of entity"'s `text`
from the parent's own text and then, because of auto-skip, run without ever
showing it.

The context is assembled once when the call starts and never changes:

- **Folded entity values.** The frame stack's root entities (outermost first)
  then the selection path within the top frame (root → leaf), each entity's
  values folded into one map so later entries win — the selected entity's values
  therefore take precedence. `null` values are skipped, not folded.
- **Positional keys**, layered on top: `entityId` (the selected entity),
  `parentId`, `rootId`, `frameId`, `tabId`, `groupId`. These are what arguments
  actually reference, and they take precedence over any same-named entity value.
- **Extras**, highest precedence: what a right-click supplies (the entity under
  the cursor), which need not be the current selection.

Because entity values come from the query cache, an outer frame that isn't
mounted contributes only whatever is already cached — the context is
best-effort by design, and positional keys never depend on it.

## Display

A pending call is displayed in one of two ways, not three: a palette, which is
centred when it has no anchor and reads as a context menu when anchored at the
cursor; or hidden, in which case a toast in the corner names the tool and the
argument being waited on.

The toast is deliberately not a full editor — it's a guide for the case where a
hotkey has started something that needs another input (link, move). Its maximise
button switches the call to `palette` display with no anchor, so minimising and
re-maximising returns to the centre rather than the original cursor position.

## Argument navigation

- **Tab** advances to the next argument not filled from the context. It still
  lands on arguments filled from a default. It never runs the tool, so on the
  last argument it is a no-op.
- **Shift+Tab** steps back one argument at a time, including over ones the
  context filled, so they can be inspected. From the first argument it returns
  to the tool list by clearing the call's tool id — that is the only way back.
- **Enter** commits the current argument, jumps to the next empty one, and runs
  the tool once nothing is empty.
- A call whose every argument is satisfied on start runs immediately, with no
  confirmation, regardless of the tool's reach. Deliberate for now.
- When the palette is hidden and the tool's own hotkey is pressed again, the
  argument being waited on is filled from the *live* context — this is what makes
  "press `x`, select the new parent, press `x` again" fall out of the general
  model rather than being a special case.

## The call log

One list, not two: cancelled and settled calls differ only by their `outcome`
(`cancelled` / `success` / `error`), which keeps resume-then-recancel updating a
single entry in place. Each records `startedAt` (in the immutable context) and
`settledAt`, and a `fromCallId` when it was resumed or rerun from another call,
so the trail reads as a history rather than a set of duplicates.

Retention: a **cancelled** call is kept whenever the tool takes arguments (a
cancelled argument-less call carries no information). A **settled** call is kept
only when the tool's reach is `external`. Reads and writes against the entity
store are far too frequent, and their results far too large, to persist — a
single `query` result would exhaust the localStorage quota, which
`persistentAtom` swallows silently. Every call still *produces* a result: that
is how errors and confirmations reach the toast layer. Only retention is
filtered. No pruning or garbage collection yet.

## Layout state

Tab groups, tabs and frames are id-keyed collections in one persisted blob.
Groups additionally keep an explicit `groupOrder`, since they are laid out as
columns and a record has no order; the selected group's *index* is derived from
it. Whether the selected group is expanded is one boolean at the top level, not
one per group.

A tab owns its frame stack, the frames popped off it (so a pop can be undone),
and its collapsed set. Collapse is therefore per tab and keyed by entity id,
while selection is per frame and keyed by *path* — so the same entity appearing
twice in one frame can be selected in one place but folds in both. That
asymmetry is intended.

A frame holds its root entity id, the latent selection path, nullable find text,
a per-entity max-depth map, and any in-progress edit.

- The **selection path** is latent and never overwritten by the resolved one.
  Resolution strips trailing ids until the path exists in the rendered rows,
  defaulting to `[rootId]`; because it is only ever derived, re-expanding a
  collapsed ancestor restores the original selection.
- The **edit state** (in-place edit or create, plus the draft text) is
  persisted, so a half-typed entity survives a reload.
- **Per-entity max depth** is stored but not yet honoured: the `query` tool
  takes a single `maxDepth`, so the root entity's entry is passed through and
  the rest is provisioned for a later server change.
- Which query a frame uses is still to be designed; frames are all entity views
  for now.
- Scroll position is not tracked.

Canvases are gone. The `View` union collapsed back to a single entity view.

Nothing cached lives in latent state: query results, entity display names and
code-run output are runtime-only and rebuild on load, at the cost of a beat of
jank on tab labels.

The layout is not scoped per source. Pointing the app at a different source
leaves frames rooted at ids that source has never heard of, and they render
empty — which is the intended behaviour, not a bug to guard against.

## Keys

There is exactly one `keydown` listener, at the top level, and it acts on global
state. It resolves in this order:

1. A pending call with `palette` display owns the keyboard — the palette's own
   input handles it.
2. A pending call with hidden display consumes Escape (cancel) and the pending
   tool's own hotkey (fill the waiting argument from the live context).
   Everything else falls through, so the selection can still be moved to choose
   a target.
3. When the event targets a text field, only Escape and modifier combos
   continue; bare keys belong to the field.
4. Otherwise the scope chain: the focused **frame**, then its **tab group**, then
   the **app**. The first enabled tool whose binding matches wins, which is how
   Escape means "cancel this edit" in a frame and "cancel this call" at the app
   level without either knowing about the other.
