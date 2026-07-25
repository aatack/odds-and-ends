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
frame, and run tools by id. (That isn't aspirational — the layers have been
exercised in node with nothing but a `localStorage` stub.)

One seam is worth naming: direct manipulation — clicking a row, dragging a tab,
typing in the in-place editor — calls a named mutator in `state/actions.ts` rather
than a tool, because routing a mouse gesture through the call machine would put
noise in the log for no gain. Anything invocable by key or palette, or worth
recording, is a tool, and tools are written in terms of the same mutators.

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

It follows that the toast can only serve an argument that is *pointed at*: there
is nowhere in it to type. So a hotkey whose outstanding argument is typed rather
than picked opens the palette instead, even though it was started from the
keyboard.

Taking over the pending slot records whatever was in it, so right-clicking
part-way through a link doesn't lose the link. A call that runs straight through
never touches the slot at all, which is what lets the selection be moved while a
link waits for its far end.

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

## Undo

Undo is destructive at the store rather than compensating: `popEvents` takes the
most recent event, and anything within 100ms of it, off the database and returns
it, and the returned events go on a stack. Redo writes them back verbatim, so the
store ends up as it was — the original timestamps and authors, not the edit
re-applied at the current time.

The 100ms window is what makes a step a *user action* rather than an event: 
creating an entity writes its values and the link to its parent at the same
instant, and they must come off together. The flip side is that two actions in
quick succession collapse into one step, which is correct but surprising if you
drive the app faster than a person can.

Consequences worth knowing:

- **Undo has a horizon.** The store never gives up an event older than five
  minutes (`POP_AGE_LIMIT_MS`), so past that an edit is settled and `popEvents`
  comes back empty. Since undo deletes rather than compensates, the limit caps
  how much of a store's history a client — or a held-down `⌘Z` — can take off.
  Redo is unaffected: a step already on the stack can be written back whenever.
- **The stack is the only copy of those events.** Nothing can reconstruct them,
  which is why it's latent state and persisted despite looking like history — and
  why there is no "clear undo history" tool: it would destroy data, not tidy a
  list.
- **Any write that didn't come from the stack clears it.** Those events are no
  longer the store's most recent, so replaying them would land them *after* the
  newer edit. The tools that work on the stack opt out with `preservesUndo`; the
  raw debug panel, which writes events directly rather than through a tool, clears
  it by hand.
- **A step records the source it came off**, and redo refuses a step belonging to
  another source. Everything else about pointing the app at a different source is
  harmless; injecting one store's events into another would invent entities there.
- `popEvents` is absent from a source that can't remove events (a read-only
  wrapper), so the client can tell undo is unavailable by the tool's absence.

`⌘Z`/`⌘Y` are handed to a focused text field rather than routed, along with the
other editing combos — inside an in-place edit, undo should mean the typing.

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

A frame holds its root entity id, which way its query reads, the latent selection
path, its filters (nullable find text, a sections-only flag), a per-entity
max-depth map, and any in-progress edit.

- The **selection path** is latent and never overwritten by the resolved one.
  Resolution strips trailing ids until the path exists in the rendered rows,
  defaulting to `[rootId]`; because it is only ever derived, re-expanding a
  collapsed ancestor restores the original selection. While pages are still
  outstanding an unfound path is left alone rather than stripped, so a deep
  selection isn't snapped to the root every time a large tree reloads.
- The **edit state** (in-place edit or create, plus the draft text) is
  persisted, so a half-typed entity survives a reload.
- **Find text** is the find field: null while the frame isn't filtering, a
  string — empty included — while it is. `frame.find` takes no argument and
  only flips null to `''`; the field that appears edits the state directly from
  then on, so there is no draft anywhere to keep in step. Run against a field
  already open it changes nothing and merely asks for the caret (below), so the
  key means the same thing whatever state the frame is in. Enter blurs the
  field, handing bare keys back to navigation; the text stays. Matching rows keep
  their ancestors, so the tree still reads.
- **Sections only** is the other filter: keep the section rows and the frame's
  root, and drop everything else — the tree as a table of contents. Unlike find
  it does *not* keep non-matching ancestors, since the point is to see the
  sections and nothing else; rows keep their real depth, so a section nested
  inside an ordinary entity still reads as nested. Applied after find, so the two
  compose. Both are filters over the rows, not different queries, and both show
  in the frame's top-right corner — the find field, and a pill for anything with
  nothing to configure.
- **Direction** is the first thing about a frame's *query* rather than its rows:
  `out` follows outbound links, `in` follows inbound ones, so the same traversal
  answers "what links to this?". It is part of the request key, so flipping it
  refetches. A reversed frame draws the same tree upside down, which the tools
  that edit the link between a row and the row above it have to know: creating,
  unlinking and moving all ask the frame the call came from which way round the
  link runs. A pushed frame does not inherit it — whether a query type should
  carry into a new frame is still an open question.
- **Per-entity max depth** is stored but not yet honoured: the `query` tool
  takes a single `maxDepth`, so the root entity's entry is passed through and
  the rest is provisioned for a later server change.
- What else a frame's query could be — beyond its root, its depth and its
  direction — is still to be designed.
- Scroll position is not tracked. The view keeps the selected row, and the row
  being typed into, scrolled 30% in from the edge; the row being typed into is
  also mounted whatever the virtual window says, or its box would never take the
  caret.

Canvases are gone. The `View` union collapsed back to a single entity view.

Nothing cached lives in latent state: query results, entity display names and
code-run output are runtime-only and rebuild on load, at the cost of a beat of
jank on tab labels. Names are the one thing kept past the rows they came from —
still runtime, but never pruned, since dropping an inactive tab's rows shouldn't
turn its label back into a uuid.

**Focus requests** (`state/focusRequest.ts`) are the one thing that travels
upward. A tool has no DOM and no components, so when it needs the keyboard to
land somewhere it names a field — `find:<frameId>` — and the component owning
that field answers through `useFocusRequest`. It is a signal, not state: runtime
only, carrying a nonce so asking twice for the same field lands twice, and
cleared once taken so the next field of that name to mount doesn't inherit it.

A tool declares whether it `mutates`, which is what makes every open frame
refetch; a run can override that when it turns out there was nothing to write
(committing an editor that was already empty, say), so a blur doesn't cost a
round of queries.

One tool takes an argument it could have inferred: `edit.commit` names its frame,
because a blur can arrive after the click that moved focus somewhere else, and the
write must still go to the frame that was being edited.

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
3. The activity log, while open, keeps Escape: it dismisses itself, and the
   press goes no further.
4. When the event targets a text field, only Escape and modifier combos
   continue; bare keys belong to the field.
5. Otherwise the scope chain: the focused **frame**, then its **tab group**, then
   the **app**. The first enabled tool whose binding matches wins, which is how
   Escape means "cancel this edit" in a frame and "cancel this call" at the app
   level without either knowing about the other.

Escape therefore has a pecking order without anything spelling one out: a
pending call, then the activity log, then an in-place edit, then a frame's find
field, then its sections filter — each step is just a tool that isn't `enabled`
unless there is something for it to do, declared in the order it should be tried.

Some of the keys the app wants belong to Electron's default menu, which consumes
an accelerator before the page sees the keystroke: ⌘/Ctrl+W (close a tab, not the
window) and ⌘/Ctrl+Z (the app's own undo). `src/main/index.ts` therefore builds
the menu by hand without those items — so a new app-level binding has to be
checked against it, not just against the tool registry.
