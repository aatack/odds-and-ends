# Tools you write in the graph

A note under the reserved entity `@tools` can describe a tool of the app. Once it
does, it is a tool like any other: it lists in the command palette, it can hold a
key, other scripts can call it by name, and what it did shows in the activity log.

Its body runs in the same QuickJS sandbox a `type: code` entity runs in, with the
same `tool` façade — so a tool you wrote reaches everything in the registry: the
frame tools, the writes, the server's integrations. That is why this is a frontend
feature. The server can *list* the same definitions (`core/source/userTools.ts`,
so they appear over MCP) but has nothing to run one with, since most of what a
tool would want to do only exists in the app.

## Getting one going

Run **New tool of your own** and give it a name. That creates the note under
`@tools`, hangs a `type: code` child off it for the body, and opens the inspector
on it so you can fill in the rest. Write the body in the code child — you can press
play on it to try it out — then run **Reload your tools** and it is in the palette.

## The fields

Values on the note itself. Two are required; everything else has a default.

| Value | Type | Required | Default | What it does |
| --- | --- | --- | --- | --- |
| `name` | string | **yes** | — | The tool's id, and how a script names it: `tool.greet(…)`. |
| `script` | string | **yes**, unless a `type: code` child supplies it | — | The body. |
| `text` | string | no | — | The note's own text. Read as the palette label. |
| `label` | string | no | `text`, then `name` | Palette label, if the note's text isn't the right one. |
| `description` | string | no | — | Matched by the palette's search. |
| `arguments` | JSON Schema object | no | no arguments | Becomes the argument prompts. |
| `scope` | `frame` \| `group` \| `app` | no | `app` | Which part of the focus chain a key resolves against. |
| `reach` | `ui` \| `source` \| `external` | no | `external` | How far it reaches, and so whether its calls are kept in the log. |
| `key` | string | no | none | A binding: `g`, `shift+g`, `mod+shift+j`. `mod` is Ctrl or ⌘. |
| `mutates` | boolean | no | `false` | Rarely needed: a body can only write *through* a write tool, and each of those refreshes the frames on its own way out. |
| `safety` | `pure` \| `safe-mutating` \| `dangerous` | no | `dangerous` | Read by the *server* only, for its capability filters. |

`arguments` is a JSON Schema object, the same shape the server publishes for its
integrations, which is what lets one mapping serve both:

```json
{
  "type": "object",
  "properties": {
    "who": { "type": "string", "description": "Who to greet" },
    "loudly": { "type": "boolean", "default": false }
  },
  "required": ["who"]
}
```

A property's `type` decides how the field is parsed (`string`, `number`,
`integer`, `boolean`; an `enum` becomes a picker; anything else is entered as
JSON). `required` decides whether the call can run without it. A `default` means
leaving the field alone sends `null`, which is the contract's "use the default".

## Where the body goes

Either on the note as `script`, or — better, for anything longer than a line — as
a child marked `type: code`, whose `text` is the body. That child is an ordinary
code entity: you can edit it in place and press play on it, which is how you debug
a tool before binding a key to it. The first such child wins.

## What the body sees

- `context` — the folded call context of wherever the tool was invoked from, with
  the arguments laid in on top. So a `who` argument reads as `context.who`, and
  `context.args.who` too if you'd rather be explicit about which is which.
- `tool` — the whole registry, by the camel case of a tool's label or by its id.
  Calls are synchronous; no `await`.
- `console` — logged to the devtools console, prefixed with the tool's name.

Whatever the body evaluates to is the tool's result: it lands in the activity log,
and a script that called the tool gets it back. A one-line summary of it becomes
the toast.

```js
// Greet someone on the selected note, and hand back what was written.
const greeting = `${context.loudly ? 'HELLO' : 'Hello'}, ${context.who}`
tool.setEntityValue(context.entityId, 'text', greeting)
greeting
```

## Reloading

Definitions are read once, when the source opens. After editing one, run **Reload
your tools** — it re-reads `@tools` and rebuilds that half of the registry. It is
a deliberate step rather than something a write triggers: a definition is edited a
value at a time, and rebuilding on every keystroke would mean binding half a key
and running half a body.

## Limits

- A key that collides with one of the app's own loses. Declared tools trail the
  built-ins in the registry, and the router takes the first tool in a scope that
  binds the key — so a store can't rebind `d` out from under you.
- The same goes for ids: a definition named after a built-in is unreachable by
  that name. Two definitions sharing a name keep the first in outline order.
- The sandbox is the code runner's, and inherits its v0 caveats: one script at a
  time, and Stop kills the worker, so stopping one run aborts any other in flight.
