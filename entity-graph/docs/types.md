# Types

An entity's `type` value names another entity. That entity is a **type**, and it
describes the entities that name it: which values they are expected to hold, what
can be done with them, and what is computed for them as they load.

A type is an entity like any other, so all of this is written in the store rather
than in this repository. What is here is the three keys that are read by name, the
one type the store supplies whether or not anybody wrote it, and where each of
them is acted on.

## What a type holds

| value | what it is |
| --- | --- |
| `schema` | JSON Schema for the values an instance holds |
| `actions` | name → TypeScript, run when a button of that name is pressed |
| `events` | a script run once per instance as it loads, with that instance as its context |

`core/schema.ts` is the whole reading of them — `schemaOf`, `fieldsOf`,
`actionsOf`, `checkValue` — and has no dependencies, so the desktop app, the phone
and the server agree about what a type says.

**A type lends its instances nothing.** It used to: a key the type defined and the
entity didn't was taken from the type, one level deep, in the cache's rollup. That
is gone. A schema says what an entity *should* hold, which is a thing to draw an
empty box for and check a value against — not a value the entity silently has, and
not something a reader has to know to subtract before believing what an entity
says.

A type's own id is its name: `type` holds the id of the type entity and the pill
on a row draws that string verbatim, so `code`, `file`, `changeset` and
`github/pullRequest` are entity ids rather than an enum. That is also why a type is
written to a *named* id rather than a minted uuid.

## `schema`

JSON Schema, in the ordinary notation, describing the entity as an object:

```json
{
  "type": "object",
  "properties": {
    "worktree": { "type": "string", "description": "The full path on this machine" },
    "base": { "type": "string", "description": "What the branch was cut from" },
    "pullRequest": { "type": "string", "description": "The URL, once anything is pushed" }
  },
  "required": ["worktree"]
}
```

`properties` is the field list, **in the order it was written** — a schema is read
as a form, and the order the keys were typed in is the only ordering anyone
intended.

The details panel (`components/EntityInspector`) draws a box per field whether or
not the entity has written one, with the field's shape and description beside it,
and marks a value that doesn't fit in the warning tone while writing it anyway.

**Checking is soft, always.** `checkValue` answers *why* a value doesn't fit and
nothing acts on the answer beyond saying so. A store where a schema could refuse a
value would be a store where a schema written after the fact locks its own
entities out — and the entities were there first.

It understands the keywords a hand-written field description actually uses —
`type`, `enum`, `const`, the string/number/array bounds, `required` and nested
`properties` — and ignores everything else rather than treating it as a failure. A
`null` value is never wrong: null is how a value comes *off* an entity in an
append-only store, so a required field that has been cleared is an empty field
rather than a wrong one.

## `actions`

A dictionary of name → the TypeScript run when a button of that name is pressed:

```json
{ "Merge": "tool['github.mergePullRequest'](context.pullRequest)" }
```

Every row of that type wears one button per action, drawn after its text
(`views/Editor`, `RowActions`). Pressing one runs `entity.action`, which finds the
body on the type and runs it in the same QuickJS sandbox a `type: code` entity uses
— so it reaches the whole tool registry through `tool.…`, and `context` is folded
along the row the button sits on rather than wherever the keyboard is. The action's
own name is in the context too, so one body shared by several buttons can tell
which was pressed.

The tool is an ordinary tool: the call is the user's, it toasts what it did, and
it is kept in the activity log. It can be run from the palette by naming the entity
and the action, which is also how an action is tried out before it has a row.

## `events`

A script run once per session for each instance, with that instance's values as
its context, whose return value is a list of events added to the client's cache and
never written to the store. This is how an entity shows something it doesn't hold —
the text of a Slack message, the branches on a repo.

It lives on the type rather than on the instance, which is what makes one script
serve every entity of a kind. `docs/frontend-state.md` has the rules it runs
under; the short version is that it waits for the type, runs only for an entity
something has actually asked for, and is not re-run by a refresh.

### The script

Statements rather than an expression: the last one evaluated is what the script
returned. `context` is the instance's own values with its `entityId` laid in on
top, so a script can name the entity it is running for. `tool.…` is the whole
registry, called synchronously — the sandbox has no promises, so nothing is
`async` and nothing is `await`ed.

```js
const pr = tool['github.getPullRequest'](context.url)
;[
  { key: 'text', value: pr.title },
  { key: 'state', value: pr.state },
]
```

### The events it returns

`derivedEvents` in `core/cache.ts` reads them, and reads them loosely: a bare
object counts as a list of one, and anything it doesn't recognise is dropped
rather than thrown over — a script that logs and returns nothing has still done
its job.

| what it returns | what that is |
| --- | --- |
| `{ key, value }` | that value on the instance itself — the common case by far |
| `{ entityId, key, value }` | the same on some other entity: a repo giving its branches their text |
| `{ sourceId, destinationId, action }` | a link. `0` adds it, `1` removes it |

`timestamp` defaults to 0 and `author` to `derived`, which is what keeps a
derived value *behind* a real one: a script may write over a key the entity
already holds and the entity's own value still wins. There is nothing to do to
arrange that, and no honest reason to pass a timestamp of your own.

## The `type` type

The entity `type` is the type of types, and the store serves it rather than holding
it: `core/builtins.ts` hands its values back with every read of that id, timestamped
0 and authored `builtin`, so anything written to those keys wins and a fresh store
still knows what a type is.

The events go in at `readWithBuiltins` in `core/source/defaultTools.ts` — the one
read every other read is built on — so a client's cache, a `query`, and an agent
over MCP all see the same thing, and none of them has to know it was never written
down. A dump of the whole store (`readEvents` with no ids) leaves them out: that is
what *is* written down.

Its schema is the three keys above, which is what makes the details panel of a type
a form for writing one.

## Writing a type

By hand, or over MCP, which tells an agent the same thing (`server/src/mcp.ts`):

1. **Choose the id, which is the name** — `github/pullRequest`. Writing a value to
   an id nothing has been written to is how an entity gets a name rather than a
   uuid.
2. Set `type` to `type`, `text` to what it is called, and `schema` to the JSON
   Schema.
3. Link it under `@types`, where types are collected — a reserved entity in the
   manner of `@tools` and `@changesets`. Nothing in the code reads it; it is where
   to look.
4. Set `type` on the entities that are of it.
