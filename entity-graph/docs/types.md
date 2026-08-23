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
| `actions` | the ids of the tools every instance wears as a button |
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

A list of tool ids, in the order they should be drawn:

```json
["changeset.checkout", "changeset.merge", "changeset.openPullRequest"]
```

Every row of that type wears one button per action, and the button *is* the tool:
pressing it runs that tool along the row, so `context.entityId` is the row it was
pressed on and the folded context is the one that row's path gives. A body reads
what it needs off the context and declares no arguments — an argument nothing
filled would stop the press to ask for it.

**They are drawn by being written.** `views/Editor` appends `[@button:<id>]()` to
the row's text before rendering it, and the inline-button field
(`components/EntityMarkdown`) does the rest, so there is no second way of laying a
button out beside a row. Inline after the text while it is one line; in a
paragraph of its own when it isn't. It follows that a row not drawing its text as
markdown — one being edited, a code row, a file's bytes — doesn't draw them
either.

The point of a tool rather than a script on the type is everywhere else it then
is. The same action is in the command palette, in the right-click menu (which is
the palette, seeded with the row under the cursor), reachable from another script
as `tool['changeset.merge']()`, and editable where every other tool is — under
`@tools`, or in `tools/` if it is the app's. A type names it; it doesn't own it.

## `events`

A script run once per session for each instance, with that instance's values as
its context, whose return value is a list of events added to the client's cache and
never written to the store. This is how an entity shows something it doesn't hold —
the text of a Slack message, the branches on a repo.

It lives on the type rather than on the instance, which is what makes one script
serve every entity of a kind. `docs/frontend-state.md` has the rules it runs
under; the short version is that it waits for the type, runs only for an entity
something has actually asked for, and is not re-run by a refresh.

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
