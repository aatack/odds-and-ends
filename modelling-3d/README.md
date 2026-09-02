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
| Right-click empty canvas | A search box: type a name, press enter, and it lands there |
| Drag between handles | A connection, if the types agree and it makes no loop |
| Drag an **input** handle onto empty space | A constant of that type, already holding what the socket was worth |
| Drag an **output** handle onto empty space | The same search box, offering only what can take that value, and joining it up |
| Click a node | It is selected, and the preview shows its output |
| Ctrl/⌘-click | Adds to the selection; the preview overlays them |
| Click empty canvas | Nothing selected — the preview shows every node nothing reads |
| Drag a point on a 2D path | The shape changes, and everything downstream with it |
| Double-click the pad, alt-click a point | Add a point where you clicked; take one away |
| Double-click a model in the navigator | Rename it |
| Drag in the preview | Orbit. Scroll to zoom, right-drag to pan; the vertical axis stays vertical |
| Drag a point in the preview | Moves a 2D/3D point constant along the ground — hold shift to move it vertically |
| Scroll the canvas | Pans; ⌘/ctrl-scroll or pinch zooms |

| Key | |
| --- | --- |
| `⌘/Ctrl+F` | Find a transform in the navigator |
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

**Extrude sweeps along a path**, not up by a number: leave its path alone and
you get a straight extrusion of height 1, give it a **Line in 3D** and you say
how far and in which direction, and give it a bent path and the shape bends
with it. **A solid comes out grey** — colour is **Paint**'s job, so a shape
doesn't have to know what colour it is before it is allowed to be a shape.

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
