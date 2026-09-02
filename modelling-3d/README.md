# 3D modelling

Build a model by saying what it *is* rather than by pushing vertices: a graph of
semantically meaningful transforms over primitives. Because the description is a
graph and not a mesh, one model describes a whole class of things — change the
number of sides and every column in the colonnade changes with it.

Three panes:

- **Navigator** — your models, and the vocabulary they are built from. Both drag
  onto the canvas; a model dropped into another model becomes a node in it.
- **Builder** — the open model as nodes and edges. Each node is a transform,
  each edge carries a value from an output to an input.
- **Preview** — whatever is selected, in 3D, redrawn as you edit.

## Running it

```sh
npm install
npm run dev          # or: npm run dev:no-sandbox
```

`dev:no-sandbox` is the one to use if Electron complains that
`chrome-sandbox` is not owned by root — which it is on an unprivileged
checkout.

```sh
npm test             # the model, the evaluator, the exporter, the editing rules, the store
npm run typecheck    # both projects
npm run build        # production bundles into out/
```

Your models live in `models.sqlite` under the app's data directory
(`~/.config/modelling-3d` on Linux). It is an ordinary sqlite file — open it
with the CLI and have a look.

## Using it

| Gesture | What happens |
| --- | --- |
| Drag from the navigator | A node of that transform, where you dropped it |
| Drag between handles | A connection, if the types agree and it makes no loop |
| Drag an **input** handle onto empty space | A constant of that type, already holding what the socket was worth |
| Click a node | It is selected, and the preview shows its output |
| Ctrl/⌘-click | Adds to the selection; the preview overlays them |
| Click empty canvas | Nothing selected — the preview shows every node nothing reads |
| Drag a point on a 2D path | The shape changes, and everything downstream with it |
| Double-click a model in the navigator | Rename it |
| Drag in the preview | Orbit; scroll to zoom, right-drag to pan. The vertical axis stays vertical |

| Key | |
| --- | --- |
| `Delete` / `Backspace` | Delete the selected nodes |
| `Escape` | Select nothing |
| `F` | Frame what is shown |
| `⌘/Ctrl+A` | Select every node |
| `⌘/Ctrl+N` | New model |
| `⌘/Ctrl+E` | Export the preview as glTF |

## The vocabulary

Values are `number`, `text`, `2D point`, `3D point`, `colour`, `2D path`,
`3D path` and `mesh` — a mesh being a bag of triangles, each with its own
colour, and nothing else.

Most work starts in 2D, because a shape is easier to say flat: rectangles,
polygons, circles, stars and lines, moved, turned and scaled, then lifted into
3D by **Fill**, **Extrude**, **Revolve** or **Loft**, or converted point for
point. From there the mesh operations — translate, rotate, scale, paint,
combine, mirror, repeat, radial array — do the rest. **The 2D plane is the
ground**: a 2D point `(x, y)` lifts to `(x, 0, -y)`, and height runs along +Y.

A model of your own becomes a transform as soon as it has ports: drop an
**In** or **Out** port node on its canvas, name it, and the model's sockets are
those ports in the order they read down the page.

## Export

glTF 2.0, binary (`.glb`), written into your downloads folder. It carries the
triangles with a colour per vertex and two directional lights matching the
preview's, so Blender, f3d, Godot, three.js or a browser will all show what you
were looking at. **Open** hands the file to whatever your desktop opens `.glb`
with; **Show** reveals it in the file manager.

See [AGENTS.md](./AGENTS.md) for how the code is arranged.
