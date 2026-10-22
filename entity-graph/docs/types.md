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

And one says how a value that holds others is edited: `"x-popout": true` on an
object, a list, a union, a schema or a JSON value shows it as one line saying
what it holds, which opens the whole form for it in a dialog.

A field whose value is itself a schema says so in the standard way, with a
`$ref` to the meta-schema: `{ "$ref": "https://json-schema.org/draft/2020-12/schema" }`.
The form edits it with the schema builder. The `type` type's own `schema` field
is one.

### Custom editors

A field can be drawn with an editor of the host's own instead of the form's
control. The schema names it — `"x-editor": "colour"` — and the code says what
the name means, by passing `editors` to `SchemaForm` or `SchemaBuilder`
(`components/schema/editors.tsx` is the whole of it, with examples):

```tsx
const editors: Editors = {
  colour: {
    name: 'Colour picker',      // what the builder offers it as
    kinds: ['string'],          // the kinds it can edit; absent means all
    tall: false,                // true if it takes more than one line
    render: ({ value, onChange, schema, label }) => (
      <input type="color" value={String(value ?? '#000000')}
             onChange={(e) => onChange(e.target.value)} />
    ),
  },
}

<SchemaForm schema={schema} value={value} setValue={setValue} editors={editors} />
```

- **In place.** The editor replaces the control on the right of the field's line;
  the line's label, its warning and a list item's cross are still the form's.
- **In a dialog.** With `"x-popout": true` as well, the line keeps its pencil and
  the editor is drawn in the dialog.
- **Code rather than a control.** An editor that runs something — a picker, a tool
  call — draws a button and calls `onChange` with what comes back. `onChange`
  can be called whenever, so `async` is fine.
- **Inherited.** Editors registered on a form or builder reach every form and
  builder inside it: a pop-out's dialog, a schema builder in a form, the builder's
  own option lines. `EditorsProvider` registers them once, high up.
- **Missing editors.** A schema naming an editor nobody registered gets the
  ordinary control. Nothing breaks.
- **Choosing one.** The builder offers **Editor** in a field's menu when any
  registered editor suits the field's kind, and the choice is made with the form,
  from those editors by name. Changing the field's kind takes the editor off.

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
