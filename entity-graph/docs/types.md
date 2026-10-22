# Custom types

An entity's `type` value names another entity. When that entity says
`type: type`, it is a **custom type**, and the entities naming it are its
instances.

A type's id is its name. To make a `recipe` type, write values to the id
`recipe` (writing to an id nothing has been written to is how an entity gets a
name instead of a uuid), and set `type: recipe` on each recipe.

## The `type` type

The entity `type` is the type of types. The store serves it, so it exists in
every store whether or not anybody wrote it (`core/builtins.ts`). Its values are
timestamped 0 and authored `builtin`, so anything written to the same keys wins.

`type` is its own type, and its `schema` describes the fields below. Read
`type` (`get_details` over MCP) to see them.

## Fields of a type

| field | value | what the app does with it |
| --- | --- | --- |
| `text` | string | What the type is called. |
| `schema` | JSON Schema | Describes the values an instance holds. |
| `onLoad` | string: a JavaScript function | Run as each instance is read into the client's cache. |

### `schema`

Ordinary JSON Schema, usually an object with `properties`:

```json
{
  "type": "object",
  "properties": {
    "servings": { "type": "integer", "description": "How many it feeds" },
    "source": { "type": "string", "description": "Where it came from" }
  },
  "required": ["servings"]
}
```

Properties are read in the order written. A schema describes; it does not
enforce. A value that does not fit is still written — the form marks its line and
says why (`problemOf` in `components/schema/schema.ts`).

Two extension keywords say how a string is entered, which JSON Schema has no
word for; to everything else it is an ordinary string:

- `"x-multiline": true` draws it as a growing text box rather than one line.
- `"x-monospace": true` sets it in monospace, for code.

A field whose value is itself a schema says so in the standard way, with a
`$ref` to the meta-schema: `{ "$ref": "https://json-schema.org/draft/2020-12/schema" }`.
The form edits it with the schema builder. The `type` type's own `schema` field
is one.

### `onLoad`

The source of a function, `(entity) => [...]`:

```js
(entity) => [
  { key: 'text', value: `Recipe for ${entity.servings}` },
]
```

- **When.** Once per instance per session, after the instance's own events are in
  the client's cache and its type has loaded. A refresh does not run it again.
- **Where.** In the app's QuickJS sandbox (`helpers/codeRunner`). `entity` is the
  instance's values, with `entityId` laid in. `tool.<name>(...)` calls any tool
  of the app, synchronously: there are no promises and no `await`. `console.log`
  goes to the devtools console.
- **What it returns.** A list of events (a single object counts as a list of one).
  They go into the client's cache only and are **never written to the store**:
  - `{ key, value }` sets a value on the instance.
  - `{ entityId, key, value }` sets a value on another entity.
  - `{ sourceId, destinationId }` links two entities (`action: 1` unlinks).

  Each is timestamped 0 unless it says otherwise, so a value written to the store
  always wins over one `onLoad` returned.
- **Failure.** A function that throws leaves the instance as it was. The cache
  records the reason (`lazyError`), and the inspector's cache tab shows it.

`core/cache.ts` runs it (`startLazy`) and `helpers/onLoad.ts` is the bridge to the
sandbox.

## Writing a type, step by step

1. Choose the id, which is the name: `recipe`.
2. On it, set `type` to `type`, `text` to what it is called, and `schema` to the
   JSON Schema. Add `onLoad` if instances need values computed as they load.
3. Set `type: recipe` on each instance.
