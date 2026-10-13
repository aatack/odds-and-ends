# Topics mode

Written in ASD-STE100. An experiment, after chat apps. Code: `src/core/modules/topics/` (`view.ts` is pure, `topics.ts` writes and dispatches), `src/renderer/src/views/Topics.tsx`.

## Requirements

1. The UI has two modes. The topics mode is the default. The classic UI (the modules in the sidebar) is behind a flag: start the app with `DETAIL_VIEWS_UI=classic`. The main process gives the flag to the window as `?ui=classic` (`main.tsx` reads it).
2. Each mode keeps its own stack and selections (`detail-views.topics.state` and `detail-views.state` in local storage), so one mode does not change the other.
3. There is one global list of topics. Its root entity is `topics` (type `topics.home`). Each topic is an owned item of type `topic`, linked under the root. The ID of the topic is its entity ID. The name of the topic is its `text`.
4. On the left is the list of topics, in the place of the sidebar. Each topic shows its name and the time that it was last updated (`updatedAt`). Each is a light line, as a module is in the classic sidebar: muted text, its name darkening on a fill under the pointer; the open topic is on the selection's fill (`--select`). While Claude works in it, it shows the Claude mark and how long Claude has worked. The most recently updated topic is first (`byRecent`).
5. At the top of the list is a box: type a name and press Enter to make a topic (`topicCreate`). The new topic opens. **Ctrl+.**, from anywhere (typing too), opens a form for the name instead (`topic.new`, a `TopicDialog`): Enter makes the topic and opens it, Escape gives up.
6. A click on a topic opens its chat in the rest of the window. The box of the chat is closed, so the keys are free for other tools. Until I open a topic, the rest of the window is empty.
7. The chat is the chat page of the Claude module (`ChatPage` in `Claude.tsx`), `flat`, with no panel on the right: a flat list of the items directly under the topic, and the box at the bottom. My messages (and what I pasted) are lines on the left, across the width, with no bubble and no mark. Claude's answers are bubbles with the Claude mark beside them, a wash of Claude's colour (`--claude-soft`) inside an edge of it (`--claude-line`), and a small shadow. The mark lines up with the start of my messages, so Claude's bubbles are indented past it. Rows of one voice (mine, or Claude's) are close together (`joined`); between voices the gap is larger. The scroll bar is near the right edge of the window, with space between it and the messages. Nothing in the chat is selectable: a click selects nothing, and the cursor keys and the item tools (Enter, `e`, `x`, …) do nothing there (`Session.selected` is null on a topic).
8. The box shows only while it has the keyboard (`ChatPage` `onDemand`). Until then, nothing is in its place. Enter opens the box (`topic.prompt`), as Tab does. Enter in the box when it is empty closes it (`topic.leave`), as Escape does.
9. Enter in the box (with text) adds a message to the chat, sends nothing, and closes the box. A message is a note (an item with no type) under the topic.
10. **Ctrl+Enter** (in the box or not) dispatches: if the box has text, it becomes a message first, and the box closes. Then all the messages since the last dispatch go to Claude as one prompt, separated by blank lines (`unsent`, `topicDispatch`). A response item (`claude.response`, `running`) goes under the topic after them, as a bubble on the left. It shows how long Claude has worked, then Claude's answer (or the error), as in the Claude module (`ask` in `claude.ts`).
11. Ctrl+Enter is disabled when there is nothing to send. The core refuses it too ("Nothing new to send"). While Claude answers (a response under the topic is `running`, `answering`), there is never a second dispatch: the core refuses it ("Claude is still answering"), and Ctrl+Enter only adds the text in the box as a message, which goes with the next dispatch.
12. The topic is the Claude session: its ID is the session ID. The first dispatch starts the session (`--session-id`); later dispatches resume it (`--resume`). The topic keeps `started` and `runningSince`. Dispatches of one topic run one at a time, in sequence.
13. A dispatch runs in the directory in the `cwd` of the topic. If the topic has no `cwd`, the first dispatch makes a new temporary directory and keeps it as `cwd` (authored `claude`, so undo does not remove it), so later dispatches resume the conversation there.
14. At the top right of the chat is a small button, **Context**. While I hover it, or after I press it (until I press it again), a card under it shows what the topic runs in (`TopicContext`). Later, this becomes a list of items pinned to the topic.
    - The directory (`cwd`), as a dropdown (`DirectorySelect`, without its worktree options): a directory I used before, or one I type. Choosing one moves the topic there (`claudeSetDirectory`, as for a Claude session); its conversation goes with it.
    - **New worktree**, when the directory is in a git repo (one of the remembered `repos`), or is a worktree (then made from its `origin`): moves the topic to a new worktree on a branch of its own.
    - The worktree's branch, as a pill. When the repo's `origin` remote is on GitHub (`github`, `owner/name`, kept when the worktree is made), it is the branch's item (`github.branch`): hover to peek, click to open it.
    - Not while Claude answers: the core refuses to move a topic then.
15. A paste outside a text box, while a topic is open and its box is closed, goes into the topic as what it is (`paste.ts`, `topicPaste`, `topicAttach`):
    - A file (a copied file, a screenshot): kept under the app's data directory (`files/<id>/<name>`), as an item of type `file` (its `text` the name, `path`, `mime`, `size`) under the topic.
    - A link to a PR, a Slack message (`…slack.com/archives/<channel>/p<ts>`) or a GitHub branch (`github.com/<owner>/<repo>/tree/<branch>`): that item, linked under the topic. A PR and a Slack message load themselves. A branch (`github.branch`, id `github:branch:<owner>/<repo>:<branch>`) is all in its id and links to its page.
    - Any other text (another link too): a message with that text.
    - What is pasted goes with the next dispatch, in its place among the messages: a file as `[File: <path>]`, an item as `[Item: <id>]` (Claude reads it through the app's MCP server) (`promptPart`).

## Key decisions

- **A message is a note, not a new type.** The MCP server reads it as any note. The chat does not select it, so the item tools do not work on it there.
- **The chat is flat.** No notes under messages, so Enter needs no meaning on the rows: it moves the keyboard between the box and the chat.
- **The response marks the dispatch.** The messages to send are the notes after the newest response. No other marker is necessary.
- **Topics is also a module.** Its root is in `moduleViews`, so the classic UI shows it in the sidebar, and a headless caller can use it. The classic UI shows a topic as its chat too (`Detail`).
- **The phone stays on the classic UI.** It can open a topic and dispatch (the tool has a label), but it cannot make a topic.
