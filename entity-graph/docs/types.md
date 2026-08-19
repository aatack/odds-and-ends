# Types

An entity's `type` value names another entity. That entity is a **type**, and it
describes the entities that name it: which values they are expected to hold, what
can be done with them, and what is computed for them as they load.

A type is an entity like any other, so all of this is written in the store rather
than in this repository. What is here is the three keys that are read by name, the
four types the store supplies whether or not anybody wrote them, and where each of
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

## The types the store serves

Four types are the *app's*, in that the app reads their fields by name, and the
store serves them rather than holding them: `core/builtins.ts` hands their values
back with every read of those ids, timestamped 0 and authored `builtin`, so
anything written to one of those keys wins and a fresh store still knows what they
are.

| type | what the app does with it |
| --- | --- |
| `type` | `schema`, `actions`, `events` — the three keys above |
| `tool` | a tool of the app, written in the store: `name`, `execute`, `arguments`, … (see [`user-tools.md`](./user-tools.md)) |
| `code` | `text` is a script, run from the row's play button |
| `file` | `mimeType`, and a caption in `text`; the bytes are in the resource store under the same id |

They hang under **`@types`**, which is served too — as links, so they can be found
by reading the outline rather than only by knowing an id. A type someone writes goes
under the same heading, and a real link-removal event takes any of them out again,
being later than the served one.

**The rule this follows: anything the app gives special meaning to is described
here.** A field read by name somewhere in `src/` and nowhere in a schema is a field
only the source can tell you about — which is no use to an agent reading the store
over MCP, and not much use to the details panel either. So a new special field
means a line in `core/builtins.ts`, and one that isn't there yet is a gap rather
than a decision.

Two things are deliberately not in it. The values *every* entity has — `text`,
`section`, `open`, `type`, `mimeType` — belong to no type, and are described where
they are read (`core/entity.ts`, and the MCP instructions). And a type whose fields
are read by something *written in the store* rather than by the app — `changeset`,
whose values are read by the tools under `@tools` — is the store's to describe, in
the store.

The events go in at `readWithBuiltins` in `core/source/defaultTools.ts` — the one
read every other read is built on — so a client's cache, a `query`, and an agent
over MCP all see the same thing, and none of them has to know it was never written
down. A dump of the whole store (`readEvents` with no ids) leaves them out: that is
what *is* written down.

A served link comes back when *either* end was asked for, since one link is a
parent's child and a child's inbound link — the same event read from two sides.

## Writing a type

By hand, or over MCP, which tells an agent the same thing (`server/src/mcp.ts`):

1. **Choose the id, which is the name** — `github/pullRequest`. Writing a value to
   an id nothing has been written to is how an entity gets a name rather than a
   uuid.
2. Set `type` to `type`, `text` to what it is called, and `schema` to the JSON
   Schema.
3. Link it under `@types`, where types are collected — a reserved entity in the
   manner of `@tools` and `@changesets`. Nothing in the code reads it; it is where
   to look, and the served types are already there.
4. Set `type` on the entities that are of it.
