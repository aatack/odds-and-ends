# Chats

A **chat** is a note you talk to a tool through. The note is the conversation:
its children are the messages in it, oldest first, and one value on it —
`chat` — says which tool every message is sent to.

Nothing about that is a new kind of thing in the store. A chat is a note, a
message is a note under it, and sending one is a tool call like any other. What
the app adds is a place to have the conversation and the shape of the exchange:
what you said, then what it said.

## What a chat holds

| value | what it is |
| --- | --- |
| `chat` | the tool every message is sent to, by its id or the camel case of its name |
| `type` | `chat`, which is what draws the form in the inspector and the pill on the row |

Anything else on the note is the *standing* half of the conversation — which
repo, which channel, which session — and is passed to the tool along with each
message. See [Calling the tool](#calling-the-tool).

`chat` is a builtin type (`core/builtins.ts`), like `type`, `tool` and
`diagram`: the store serves it whether or not anybody wrote it, so a fresh store
knows what a chat is and the schema can't drift from the code that reads it.

## What a message holds

Nothing at all, beyond the `text` that was said. A message is an ordinary note —
it renders as markdown with the same inline fields every row has, it can be
right-clicked for the tool list, and it can be opened as its own outline.

One exception, and it is the app's own:

| value | what it is |
| --- | --- |
| `owner` | who said it, when that is not whoever wrote the event |

A message you typed carries none: its author is the person at the keyboard,
which is the honest answer and the one the store already had. A **reply** carries
the id of the tool that answered, because the app wrote that note too and its
events are authored by whoever is signed in. Without it, the answer would come
back looking like something you had said.

That is also what decides the sides of the panel: a message whose owner is the
current author floats right, and everything else floats left. So a conversation
on a shared pensive reads correctly without anyone doing anything — your
colleague's messages are on the left because they are theirs.

## Where a chat is

On a **tab**, as a list of entity ids and nothing more (`TabState.chats`). Each
one is a small round picture in the tab's bottom right corner, floating over the
rows the way the filter pills float over them at the other corner — so opening a
chat never reflows what you were reading.

The picture is drawn from the id and stored nowhere
(`helpers/identicon.ts`): a 5×5 grid mirrored about its middle column, in one
muted hue, both decided by the id. The same chat therefore looks the same in
every tab and every window, forever, and a chat gets a face without anybody
choosing one.

Three tools, all in `tools/chatTools.ts`:

- **Add chat to tab** (`chat.add`) — what makes a chat a chat. Until a tab
  carries it, a note with a `chat` value is a note with a value on it. Run it on
  the note: right-click, or the palette.
- **Remove chat from tab** (`chat.remove`) — takes it back out. Also the link at
  the bottom of an open panel.
- **Send a chat message** (`chat.send`) — the whole of what a chat does.

Which one is open is on the tab too (`openChatId`), one at a time: the panel sits
over the rows, and two of them would be a window manager.

## Sending

Type a line in the box at the bottom of the panel and press Enter (Shift+Enter
for a newline). That runs `chat.send`, which does three things in order:

1. **Writes what you typed** as a child of the chat. Before the call, not after,
   so it is on screen while the answer is being waited for — and so the author on
   it is you.
2. **Calls the tool** named in the chat's `chat` value, the way a script would.
3. **Writes what it answered** as the next child, carrying `owner`.

A tool that answers with nothing writes no third note: a blank message in a
conversation reads as one that failed to render. The toast says so instead.

`chat.send` is a tool rather than something the panel does for itself, for the
reasons every gesture in this app is one — it is worth recording, its errors
reach the toast layer like every other call's, and it is then callable from a
script, so a chat that talks to itself is a loop somebody can write rather than a
feature somebody has to ask for.

### Calling the tool

The tool is called with **the chat's own values for whatever arguments it
declares**, with `text` — the message — laid over the top.

Filtered rather than passed whole, and this is worth knowing when writing the
tool on the other end: a tool handed a key it doesn't declare refuses the call
outright (`argsFromCall` in `tools/args.ts`), and a chat carries `text`, `type`
and `chat` that are the app's business and no tool's. So:

- A chat holding `repo: '~/repos/thing'` and a tool declaring a `repo` argument
  gets it, every message, without anyone retyping it.
- A chat holding `repo` and a tool that declares no `repo` argument is not
  passed one, and nothing fails.
- The message always wins over the note's own `text`, which is the chat's
  *title* and not something anybody meant to send.

A tool that declares no `text` argument never sees the message. That is not
checked — a tool is free to be written that way — but it is almost always a
mistake, and the symptom is a chat that answers the same thing every time.

Whatever the tool returns is the reply. A string is the message itself; anything
else is written as the JSON it is, in a fenced block, since a tool that was not
written to be talked to still answers *something* and burying it would be worse.

## Writing one

By hand, or over MCP, which tells an agent the same thing (`get_details` on
`chat`):

1. **Create a note** wherever the conversation belongs — chats are notes and go
   where notes go.
2. Set `type` to `chat` and `chat` to the id of the tool to send to.
3. Set anything else the tool needs and that doesn't change per message.
4. In the app, run **Add chat to tab** on it.

A tool of your own under `@tools` is the usual far end; see
[`user-tools.md`](./user-tools.md). Declare a `text` argument, return a string,
and it is a correspondent.

An agent can write the note and link it; it cannot add it to a tab, which is the
app's own. Say which you have done.
