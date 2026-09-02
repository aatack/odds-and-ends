# 3D modelling — agent & contributor guide

An Electron + React + TypeScript app for describing 3D models as graphs of
transforms. [`README.md`](./README.md) is what it does; this is how it is built.

## Layers

```
src/core        the model, and everything pure about it
  values.ts       the value types that flow along an edge
  geometry.ts     vectors, triangulation, the surfaces built on them
  transforms.ts   the built-in vocabulary: sockets in, sockets out, a function between
  graph.ts        a model, and the transform it becomes when another model uses it
  evaluate.ts     a model's every node evaluated, or the reason it has none
  scene.ts        any value reduced to triangles, polylines and points
  glb.ts          a mesh written out as binary glTF
  seed.ts         what is in the store the first time the app opens
  api.ts          the write operations, and the seam onto the desktop
src/main        Electron main: the window, the sqlite store, the IPC handlers
src/preload     the contextBridge exposing `window.modelling`
src/renderer/src
  state/          latent state, pure reducers, pure derivations — no React, no DOM
  hooks.ts        the only place React meets the state layer
  commands.ts     everything the user can invoke, and the one key router
  components/     the three panes and the pieces they are made of
test/           all of the above driven headlessly (`npm test`)
```

Each layer depends only on the ones above it. **`state/` is in
`tsconfig.node.json` on purpose**: it is checked against the ES libraries alone,
so reaching for React or the DOM from it fails to compile. That is what keeps
`npm test` able to drive the whole app with nothing rendered.

- **Latent state only.** The store holds the models, which model is open, what
  is selected, and the current notice. The evaluation, the preview scene, the
  palette, what a socket is worth and whether a connection is allowed are all
  *derivations* (`state/derive.ts`) and are never written back.
- **Views render and forward.** A component takes what it draws and hands
  gestures back as actions. Anything invocable is a command, not an inline
  handler.
- **One key listener**, in `App`, walking `commands.ts` and running the first
  enabled command whose binding matches. A button and a hotkey are the same
  command said twice, so they cannot drift.
- **Persistence is injected** (`Persistence` in `core/api.ts`) and never waited
  on: the store is the truth on screen and the file catches up. A test passes an
  array and asserts the operations.

## Decisions worth knowing

- **A transform is data.** Sockets, params and a pure function. That is why a
  user's model can be one (`modelDef`) and why a *scripted* one could be later:
  it would only have to supply the same three things. Nothing else in the app
  knows which kind it is holding.
- **Nothing is cached between evaluations.** A model is small; recomputing all
  of it on every edit is what keeps the preview honest and leaves no
  invalidation to get wrong. If that ever stops being true, memoise in
  `evaluate.ts` — not by storing results in the store.
- **The 2D plane is the ground.** `(x, y)` lifts to `(x, 0, -y)`; height is +Y.
  Every conversion goes through `geometry.lift`, so the choice is made once.
- **glTF 2.0 is the output format**, written by hand in `core/glb.ts`. A mesh
  here is triangles with a colour each, which is exactly what a glTF primitive
  with `COLOR_0` is, and every open-source renderer reads it. Writing it
  ourselves costs a few dozen lines and no dependency, and it can be tested with
  no DOM. Vertex colours go out **linear**; the preview converts the same way,
  which is why the two agree.
- **sqlite is wasm, not native.** A native binding has to be rebuilt against
  Electron's ABI on every install — a toolchain, a postinstall step, and a
  binary that then no longer loads under plain node, which would take the store
  tests with it. The models are kilobytes, so keeping the database in memory and
  writing the file out after a change costs nothing.
- **Selection belongs to the store, not to React Flow.** Both holding it races:
  the controlled `selected` prop lands back on the canvas before the store has
  been told, and the two take turns undoing each other. `onNodesChange` applies
  the select changes; nothing listens to `onSelectionChange`.
- **No `<input type="color">`.** It opens a popup the page cannot see or
  control — drawn *inside* the window on Linux — and it takes stray clicks, so a
  colour can change itself. `editors.tsx` picks off a saturation/value plane
  with a hue strip, holding the hue itself because black has none to read back.
- **A drag listens on the window.** Listeners on the element under the cursor
  miss moves even with pointer capture, and a drag has to survive leaving the
  box it started in. Both the path pad and the viewer's handles do it this way.
- **A view that fits its contents freezes while they are dragged.** The path
  pad scales to the shape; growing that under a moving cursor sends the point
  chasing outwards. Same reason the point is tracked by the offset it was
  grabbed at rather than snapped to the cursor.
- **A solid comes out grey, and Extrude sweeps along a path.** Colour is
  `Paint`'s and nothing else's; asking a generator for one over-specifies it.
  The sweep carries frames along the path by parallel transport so the
  cross-section doesn't spin, and the first frame is chosen to agree with
  `lift`, which is what makes a straight path up the plain extrusion.
- **A drag is not an edit yet.** `moveNode` only moves; `commitNode` writes,
  once, when the drag ends.

## Design language

Clean and quiet, in the manner of Linear or Apple: generous whitespace,
restrained colour, one muted accent (`brand`), tone and spacing rather than
borders. Every colour is a token in `src/renderer/src/index.css`.

**Hard rules:**

- **No motion.** No transitions, no animations, anywhere. A global rule in
  `index.css` disables them, so the ban holds even if a utility slips in — but
  don't reach for `transition-*` or `animate-*` in the first place.
- **Prefer no borders.** A hairline `ring-line`, a whisper of `shadow-xs`, or a
  change of background does the work.
- **Focus rings only for the keyboard** (`focus-visible:`).
- The preview draws a frame when the camera moves or the model changes, and
  never otherwise. There is no idle render loop.

## Checking your work

`npm test` is the fast one and covers most of what you would otherwise click
through: the geometry, the evaluator, the exporter's container, the editing
rules, and the sqlite store against a real file. `npm run typecheck` covers
both projects. `npm run build` catches what only the bundler sees.

To look at the app, ask — starting it takes over a window on the user's desktop.
If you do need to see it, `npx electron . --no-sandbox` after a build, and
`webContents.capturePage()` from a throwaway main script is how to get a
screenshot without a display server that cooperates.

## Not done yet

- **A scripting language.** Deliberately not ruled out: a scripted transform is
  a `TransformDef` whose `evaluate` runs a sandbox, and it would need nothing
  else. Values are already plain JSON for this reason.
- **Boolean geometry.** `Combine` pools triangles; it is not a union.
- **Undo.** Every edit is already a small `WriteOp`, which is the half of it
  that usually gets in the way.
- **Editing a 3D path in the viewer.** Its points are typed on the node; only a
  2D/3D *point* constant can be dragged in three dimensions so far.
