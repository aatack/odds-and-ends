# Diagrams

A note saying `type: diagram` holds a picture as well as text. The app draws a
pannable canvas above the note, as wide as the note is and as tall as the note
says; the shapes on it are the entity's own values.

`diagram` is one of the types the store *serves* rather than holds — like `type`
and `tool`, its schema comes back with any read of that id whether or not anybody
wrote it (`core/builtins.ts`), so the shape of a value is spelled out for a reader
who has the store and no source code.

## Shapes are values, not children

Every shape is one value on the entity, under a key beginning `diagram/`:

```json
{
  "text": "How the ingest fits together",
  "aspectRatio": 1.778,
  "diagram/1": { "shape": "rectangle", "x": 40, "y": 40, "width": 160, "height": 64, "text": "Ingest" },
  "diagram/2": { "shape": "rectangle", "x": 320, "y": 40, "width": 160, "height": 64, "text": "Index" },
  "diagram/3": { "shape": "arrow", "from": "diagram/1", "to": "diagram/2", "text": "batched" },
  "diagram/4": { "shape": "text", "x": 40, "y": 160, "width": 200, "height": 28, "text": "nightly" }
}
```

Values rather than linked sub-entities, for two reasons that both matter:

- **A diagram still has notes under it.** Its children are its children — the
  outline reads through a diagram exactly as it reads through any other row, and
  nothing has to be told apart from the drawing.
- **One shape is one event.** Moving a rectangle writes `diagram/1` and nothing
  else, so a drag is a small append rather than a rewrite of the picture, and two
  people nudging different boxes do not overwrite each other.

Three shapes, and every field but `shape` may be left out:

| `shape` | what it is | fields |
| --- | --- | --- |
| `rectangle` | a box with a hairline round it | `x`, `y`, `width`, `height`, `text` |
| `text` | the same, with nothing drawn round it | `x`, `y`, `width`, `height`, `text` |
| `arrow` | a line with a head on it | `from`, `to`, `text` |

Coordinates are the canvas's own, positive y downwards. Neither the origin nor
the extent means anything: the view is panned and zoomed over whatever is there,
and a diagram whose shapes all sit at x = 4000 opens looking at them.

An arrow's `from` and `to` are each **either** the key of another shape, which the
arrow then follows as that shape moves, **or** a bare `{ "x": …, "y": … }`. A key
without the prefix (`"1"`) is read as one with it. An arrow naming a shape the
diagram no longer has is left undrawn rather than drawn to the origin — a value
comes off an entity by being written `null`, so a dangling end is the ordinary
aftermath of a removal.

`aspectRatio` is how wide the canvas is against its height — a number, or `"16:9"`
written as a ratio. It defaults to 16:9. The height follows from the width the
note has, so nothing measures anything; dragging the strip along the bottom of the
canvas is what writes it.

`core/diagram.ts` is the whole reading of all of this, and has no dependencies, so
the desktop app, the phone and the server agree about what a `diagram/…` key says.

## The canvas

`components/Diagram.tsx`. [React Flow](https://reactflow.dev) does the parts of a
canvas nobody should write twice — the viewport, the dragging, the connection
gesture, the arrow heads — and none of the parts that are this app's:

- **The values are the picture.** The flow's nodes are a copy of the shapes, so a
  drag can move a box before anything has been written; the write happens when the
  drag ends and the picture is redrawn from what came back.
- **Every change is a tool.** Dragging a rectangle and typing "add a rectangle"
  into the palette are the same write. See below.
- **It holds no keys.** React Flow's own key handling is switched off wholesale —
  `deleteKeyCode`, `selectionKeyCode` and the rest set to null — because there is
  one key listener in this app and `tools/dispatch.ts` owns it. The two keys that
  *do* mean something different over a canvas are tools like any other; see
  `DIAGRAM_SELECTION_TOOLS` below.
- **The wheel is not the zoom.** A canvas inside an outline that swallowed the
  scroll would be a hole in the page. The wheel scrolls the tree; zooming is on the
  controls in the corner and on a pinch.
- **The look is ours.** The nodes are our components in our type, and the few
  surfaces the library owns — the dotted field, the zoom buttons, the connection
  dots, the resize handles — are re-pointed at the app's tokens in `index.css`, so
  a diagram is light or dark with everything else.

What a pointer can do, all of it deliberately basic:

| gesture | what happens |
| --- | --- |
| drag a shape | moves it |
| drag its corners, once selected | resizes it |
| double click it | types into it, in the app's own text control |
| drag from a dot on a box's side to another box | an arrow tied to both |
| the same, let go over the canvas | an arrow to that point, whose end is a dot to drag |
| drag a loose end onto a box | the end takes hold of the box and follows it |
| drag the strip under the canvas | changes the height, and writes `aspectRatio` |
| the controls in the top right | add a rectangle, a text box or an arrow; remove what is selected |
| Backspace or Delete, with a shape selected | removes the selected shapes |
| Enter, with a shape selected | types into it |
| fold the row | puts the canvas away and leaves the text |

Detaching an end that has taken hold of a box is not a gesture: write the value.

A diagram folds whether or not it has anything under it, because what folding one
puts away is the canvas. That is how a page of diagrams reads as an outline
rather than as a wall of pictures.

## What is selected, and the two keys

Backspace over a rectangle should remove the rectangle and not the note, and Enter
over one should type into it rather than start a new note underneath. Both are
ordinary tools — there is one key listener in this app — so what the canvas has
selected has to be something a tool can ask about, and `state/diagram.ts` is where
it is kept: one canvas at a time, runtime only, released when the row unmounts.

Two conditions before either key means anything: the canvas has a selection, *and*
the frame's own cursor is on that diagram's row. A canvas keeps its selection
while the cursor walks away from it, and without the second half a Backspace three
rows below would rub out a rectangle. With nothing selected both keys mean what
they always meant.

`DIAGRAM_SELECTION_TOOLS` is those two, and they sit at the very front of the
registry: the router hands a press to the first tool that binds the key and says
it applies, so a tool taking a key off another has to be found before it. The rest
of the diagram tools sit further down, where they read.

## The tools

Every one of them writes a single value, and each is in the command palette and
the right-click menu like anything else — so a script can draw a diagram the way a
pointer does.

| id | what it writes |
| --- | --- |
| `diagram.rectangle.add` | a new `rectangle`, at `x`/`y` or stepped down the canvas |
| `diagram.text.add` | a new `text` |
| `diagram.arrow.add` | a new `arrow`; `from` and `to` are each a key or a point |
| `diagram.shape.set` | one whole shape, as a drag left it — unlisted |
| `diagram.shape.remove` | `null` over one key |
| `diagram.aspectRatio.set` | the canvas's proportions |
| `diagram.selection.remove` | `null` over every selected key — Backspace, Delete |
| `diagram.selection.edit` | types into the selected shape — Enter |
| `create.diagram` | a child note of the selection, typed `diagram` |

Every argument on these carries a `hasDefault` rather than being `optional`, and
that is the difference between a gesture that works and one that stops to ask: an
*empty* argument opens the palette however unimportant it is, and only a defaulted
one lets a call run straight through. A dragged arrow that asked for its
coordinates in a dialog would be no gesture at all.

A new key is the next number up over whatever the entity already holds —
`diagram/3` is something a person can name in an arrow's `from`, which a uuid is
not. Two clients adding a shape at the same instant would choose the same number
and the later write would win: that is one lost rectangle, and cheap to redraw.
