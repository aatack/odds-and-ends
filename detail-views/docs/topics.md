# Topics mode

Written in ASD-STE100. An experiment, after chat apps. Code: `src/core/modules/topics/` (`view.ts` is pure, `topics.ts` writes and dispatches), `src/renderer/src/views/Topics.tsx`.

## Requirements

1. The UI has two modes. The topics mode is the default. The classic UI (the modules in the sidebar) is behind a flag: start the app with `DETAIL_VIEWS_UI=classic`. The main process gives the flag to the window as `?ui=classic` (`main.tsx` reads it).
2. Each mode keeps its own stack and selections (`detail-views.topics.state` and `detail-views.state` in local storage), so one mode does not change the other.
3. There is one global list of topics. Its root entity is `topics` (type `topics.home`). Each topic is an owned item of type `topic`, linked under the root. The ID of the topic is its entity ID. The name of the topic is its `text`.
4. On the left is the list of topics, in the place of the sidebar. Each topic shows its name and the time that it was last updated (`updatedAt`). While Claude works in it, it shows the Claude mark and how long Claude has worked. The most recently updated topic is first (`byRecent`).
5. At the top of the list is a box: type a name and press Enter to make a topic (`topicCreate`). The new topic opens.
6. A click on a topic opens its chat in the rest of the window. The box of the chat gets the keyboard. Until I open a topic, the rest of the window is empty.
7. The chat is the chat page of the Claude module (`ChatPage` in `Claude.tsx`), with no panel on the right: rows as bubbles, the box at the bottom, and the rows selectable as in a tree (the same keys, Tab to the box).
8. Enter in the box adds a message to the chat and sends nothing. A message is a note (an item with no type) under the topic. My messages are bubbles on the right. Notes under them show as lines.
9. **Ctrl+Enter** (in the box or on the rows) dispatches: if the box has text, it becomes a message first. Then all the messages since the last dispatch go to Claude as one prompt, separated by blank lines (`unsent`, `topicDispatch`). A response item (`claude.response`, `running`) goes under the topic after them, as a bubble on the left. It shows how long Claude has worked, then Claude's answer (or the error), as in the Claude module (`ask` in `claude.ts`).
10. Ctrl+Enter is disabled when there is nothing to send. The core refuses it too ("Nothing new to send").
11. The topic is the Claude session: its ID is the session ID. The first dispatch starts the session (`--session-id`); later dispatches resume it (`--resume`). The topic keeps `started` and `runningSince`. Dispatches of one topic run one at a time, in sequence.
12. A dispatch runs in the directory in the `cwd` of the topic. If the topic has no `cwd`, the first dispatch makes a new temporary directory and keeps it as `cwd` (authored `claude`, so undo does not remove it), so later dispatches resume the conversation there.
13. (Later: a dropdown to choose the directory of a topic.)

## Key decisions

- **A message is a note, not a new type.** All the item tools work on it (`e`, Enter, `b`, Backspace), and the MCP server reads it as any note.
- **The response marks the dispatch.** The messages to send are the notes after the newest response. No other marker is necessary.
- **Topics is also a module.** Its root is in `moduleViews`, so the classic UI shows it in the sidebar, and a headless caller can use it. The classic UI shows a topic as its chat too (`Detail`).
- **The phone stays on the classic UI.** It can open a topic and dispatch (the tool has a label), but it cannot make a topic.
