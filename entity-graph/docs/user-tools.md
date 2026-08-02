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
`@tools` with a stub `execute` on it, and opens the inspector so you can fill it
in. Write the body, declare any `arguments`, then run **Reload your tools** and it
is in the palette.

## The fields

Values on the note itself. Two are required; everything else has a default.

The note's own `text` is **not** read. That is what the outline shows, and it is a
different job — a definition should be able to sit under a heading of "Slack
things" without the palette calling the tool that.

| Value | Type | Required | Default | What it does |
| --- | --- | --- | --- | --- |
| `name` | string | **yes** | — | What the tool is called: the palette label. |
| `execute` | string | **yes** | — | The body: an expression evaluating to a function. See below. |
| `id` | string | no | `name` | What a script reaches it by: `tool.greet(…)`. |
| `description` | string | no | — | Matched by the palette's search. |
| `arguments` | list | no | no arguments | What it takes, one entry per argument. See below. |
| `scope` | `frame` \| `group` \| `app` | no | `app` | Which part of the focus chain a key resolves against. |
| `reach` | `ui` \| `source` \| `external` | no | `external` | How far it reaches, and so whether its calls are kept in the log. |
| `key` | string | no | none | A binding: `g`, `shift+g`, `mod+shift+j`. `mod` is Ctrl or ⌘. |
| `mutates` | boolean | no | `false` | Rarely needed: a body can only write *through* a write tool, and each of those refreshes the frames on its own way out. |
| `safety` | `pure` \| `safe-mutating` \| `dangerous` | no | `dangerous` | Read by the *server* only, for its capability filters. |

## Arguments

A list, one entry per argument, in the order you want to be asked for them:

```json
[
  { "name": "who", "type": "string", "required": true },
  { "name": "loudly", "type": "boolean" },
  { "name": "payload", "type": "" }
]
```

| Key | Meaning |
| --- | --- |
| `name` | Required. What `run` receives it under, and what the body reads off `context`. An entry naming nothing is skipped. |
| `type` | How the field is parsed: `string`, `number`, `integer`, `boolean`, `entity`. Empty or absent means the value is entered as JSON. |
| `required` | Absent means `false`. A call won't run with a required argument outstanding. |
| `options` | A list. Makes it a picker, whatever `type` says. |
| `description` | Shown as the field's placeholder. |
| `default` | See the gotcha below. |

The **label** is derived from the name — `pullRequest` is prompted for as "Pull
request". An argument that needs nothing but a name can be the name on its own:
`["who", "what"]`. Two entries sharing a name keep the first.

`type: "entity"` gives you the entity-id field, which the palette can fill by
pointing rather than typing. Outside the app it is an ordinary string.

### The `default` gotcha

A `default` does **not** mean the app fills that value in. It means the field shows
as "default", and leaving it alone sends `null` — the contract's "use the default",
which the tool is then meant to apply. Nothing applies it for you, so:

```js
const times = context.times ?? 1
```

You want that `??` regardless: an optional argument left blank is dropped before
the body runs, so it arrives as `undefined` rather than `null`.

### Behind the scenes

The list is converted to JSON Schema in `core/toolArguments.ts` — that is the form
the palette derives its prompts from, and the form the server publishes to MCP. A
definition that already holds a schema object is passed through untouched, so
anything written the long way keeps working.

`arguments` written as *text* is parsed rather than shrugged at, since a value
field holding a list is easy to leave as a string by accident. If it doesn't parse,
the tool still loads — taking no arguments — and **Reload your tools** says so.

### When a tool asks for nothing

If your tool runs the moment you pick it, prompting for none of the arguments you
declared, then `arguments` didn't read as a list. In the inspector, the type beside
each key says which it is: **JSON** is a list, **text** is a string. Two ways out —
rewrite the value through the inspector's *New key* row (which parses what you type
as JSON), or leave it as text, since that is now parsed too.

The symptom is worth recognising because it doesn't look like a declaration
problem: the body is called with `undefined` for every parameter, so `first +
second` comes back as `NaN`, which the log shows as `null`.

## The body

`execute` is a string holding **an expression that evaluates to a function**. The
function is called with the declared arguments, positionally, in the order
`arguments` lists them:

```js
(who, loudly) => {
  const greeting = `${loudly ? 'HELLO' : 'Hello'}, ${who}`
  tool.setEntityValue(context.entityId, 'text', greeting)
  return greeting
}
```

Whatever it returns is the tool's result: it lands in the activity log, a script
that called the tool gets it back, and a one-line summary of it becomes the toast.

Because `execute` holds a string, the inspector edits it as **raw multi-line text**
rather than as escaped JSON — so it reads like code while you write it. That is the
whole reason **New tool of your own** writes a stub in rather than leaving the value
off: an absent value has no shape, and would come back as a one-line JSON field.

Add a parameter whenever you add an argument, in the same order. An argument the
user left blank arrives as `undefined`, the same as a parameter that wasn't passed.

### What else is in scope

- `context` — the folded call context of wherever the tool was invoked from, with
  the arguments laid in on top. `context.entityId` is the selected entity; a `who`
  argument is also reachable as `context.who` or `context.args.who`, though the
  parameter is the point.
- `tool` — the whole registry, by a tool's `id` or by the camel case of its name.
  Calls are synchronous; no `await`. That includes the tools you wrote: one may
  call another, and a `type: code` entity may call any of them.
  `tool.getEntity(id)` is the one to reach for when you need an entity a script
  hasn't been handed — it asks the store and waits, rather than reading the cache,
  which answers with whatever it happens to have.
- `console` — logged to the devtools console, prefixed with the tool's id.

Don't make it `async`. The sandbox has no promise support, by design — that is what
buys the synchronous `tool` calls — so a returned promise comes back as nothing.

### Naming a call

A call gets a uuid nobody outside the machine ever sees. Pass `$callId` alongside
the arguments and it gets yours instead:

```js
const watching = tool['claude.runPrompt']({
  path: worktree,
  prompt: ask,
  sessionId: session,
  $callId: noteId,
})
```

Only the object form can carry one — there is nowhere to put a name in a
positional call — and `$callId` is taken off before the rest is read as arguments,
so a tool that has no such argument is none the wiser. An object left with nothing
else in it was a name and not arguments, so `tool.reloadTools({ $callId: id })` is
a call with no arguments rather than one with an object for its first.

Naming a call is also what puts it in the **activity log**, which a script's calls
otherwise stay out of. Nothing names a call except to point at it afterwards, and
`[@tool:callId](label)` in a note's text is what points: write the note first, and
it shows the call counting up while it runs and how it ended after.

### The older shape

A `script` value still works: statements rather than an expression, reading their
arguments off `context` and handing back whatever the last of them evaluates to.
`execute` is the one to write. A note carrying both uses `execute`.

An earlier version of this looked for a `type: code` child to use as the body.
That's gone — one place to look beats two.

## Reloading

Definitions are read once, when the source opens. After editing one, run **Reload
your tools** — it re-reads `@tools` and rebuilds that half of the registry. It is
a deliberate step rather than something a write triggers: a definition is edited a
value at a time, and rebuilding on every keystroke would mean binding half a key
and running half a body.

It is also how you find out what the store thinks you wrote. It reports what it
took and what it passed over:

- `Nothing is linked under @tools` — the note exists but isn't a child of `@tools`.
  Link it with **Link entity from…**, naming `@tools` as the source.
- `2 tools: greet, add` — both loaded.
- `1 tool: greet. Skipped n7 (no \`execute\`)` — the note is linked and was read,
  but isn't a tool yet. The reason names the value that's missing.

A tool that loaded but does the wrong thing is a different problem: look in the
activity log, which keeps the result of every run, and in the devtools console,
where anything the body logged appears under the tool's name.

## Limits

- A key that collides with one of the app's own loses. Declared tools trail the
  built-ins in the registry, and the router takes the first tool in a scope that
  binds the key — so a store can't rebind `d` out from under you.
- The same goes for ids: a definition taking a built-in's id is unreachable by it.
  Two definitions sharing an id keep the first in outline order.
- Nesting is bounded at eight. A script calling a tool that calls a tool is fine;
  a tool that calls itself without end stops with an error saying so, rather than
  taking the window with it. Stop kills every worker, so stopping one run aborts
  any other in flight.
- The app and the server disagree about what makes a note tool-shaped. The app
  wants a `name` and a body; the server wants a `name`, a `description` and an
  `arguments`, and doesn't look for a body at all — it can't run one. So a tool
  with nothing but a name and a body works here and never appears over MCP. Give
  it a `description` and an `arguments` if you want it in both places.
