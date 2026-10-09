# Claude module

Written in ASD-STE100. Code: `src/core/modules/claude/` (`view.ts` is pure, `claude.ts` runs `claude -p`).

## Requirements

1. The Claude module is a module in the sidebar. Its root entity is `claude`.
2. Each session is an item of type `claude.session`.
3. The ID of the session item is the ID of the Claude session. The app makes the ID and gives it to Claude (`--session-id`).
4. A session has a name.
5. A session has an optional working directory, and a worktree checkbox. The form shows the checkbox only when the directory field has text.
6. If there is no working directory, make a new temporary directory. Run the agent there.
7. If there is a working directory and no worktree, run the agent in that directory.
8. If there is a working directory and a worktree, make a git worktree of the repo of that directory. Run the agent in the worktree.
9. Do not put a worktree in a repo. No repo must commit it.
10. A worktree session has a new branch. Make its name from the session name and the session ID. Keep the branch name on the session item.
11. Keep these on the session item: the directory where it runs (`cwd`), the worktree directory (`worktree`), the branch (`branch`), the repo (`repo`).
12. **Shift+K** opens a form: name, directory, worktree. Enter starts the session. Escape cancels. Shift+K works in all views.
13. Link the new session item only under the Claude root. Do not link it to other items (for now). When it starts, push the view of the session.
14. Keep each directory that I enter on the Claude root (`cwds`). Give these as options in the next form. Put the last one in the field.
15. The core has two functions: create a session (`claudeCreate`), and prompt a session (`claudePrompt`: a session ID and a text).
16. A session is a chat. Its children are its prompts and responses, in sequence. The session is apart from the notes. (Later, a session can refer to items in the notes.)
17. When I prompt a session, put a prompt item under the session. Then put an empty response item under the session, after the prompt.
17a. Send Claude only my text. One exception: if the session has a context, the first prompt starts with the IDs of the other items in it: `[Session context: abc123, xyz456]`, then a blank line (`contextLine`). The prompt item keeps only my text. The MCP server's instructions tell Claude what the line means, and that it can read the items.
17b. **k** on an item puts a reference to it in the next prompt of the session in the side panel: `[](entity:<ID>)` and a space, after the text that is there. The text cursor goes to the end of the box. If the side panel does not show a session, k opens the nearest session in context with the path (as Ctrl+Enter finds it) in the side panel first. k focuses the prompt box, so k and then Enter sends. k works only when it finds a session. Markdown shows `[](entity:<ID>)` as the pill of the item, everywhere (also in answers).
18. When Claude answers, put the answer in the response item.
18a. Keep all of the answer in the response item, as one text. Do not split it into notes. (`b` splits it, if I want.)
19. If Claude fails, put an error message in the response item instead of the answer.
20. While Claude works, the response item shows how long it has run.
20a. While Claude works, the session pill also shows how long the answer has run. The session keeps `runningSince` (when the answer started; `null` when it is done).
21. The view of a session is the chat: the session at the top, then the prompts and responses, the newest at the bottom. A prompt has a faint blue fill (the `accent` tint). A response has no fill.
21b. The session stays at the top of the view. It does not scroll. A thin line is below it. The prompts and responses start at the bottom and go up. When the view opens, it is at the bottom. When the chat is at the bottom, it stays at the bottom when items come.
21c. There is a space between messages: two thirds of a line. The same space is after the last message. Messages have no bullet.
21e. The prompt box grows as I type, to 40% of the screen height. Then it scrolls.
21d. While the prompt box has the keyboard, the chat does not show the selected row.
21a. Below the chat is a text box for the next prompt. When the view opens, the box has the keyboard. Enter sends the prompt. Shift+Enter makes a new line. Escape gives the keyboard back to the chat.
22. Do not stream the answer. Show it when Claude is done.
23. Use the model Opus 5.5 (`claude-opus-5-5`).
24. Use Claude Code auto mode for permissions (`--permission-mode auto`).

## Key decisions

- **Items are owned.** Sessions, prompts and responses go in the owned store. They do not go in the cache store, because the cache store is cleared each week.
- **Claude writes as `claude`.** Undo (Ctrl+Z) removes only my own edits (author `me`). Thus, undo does not remove an answer.
- **One prompt at a time for each session.** Prompts wait in sequence, in the order that I write them.
- **First prompt, then resume.** The first prompt uses `--session-id <ID>`. Each prompt after it uses `--resume <ID>`. The session item keeps `started: true` after the first answer.
- **Worktree location.** `<app data directory>/worktrees/<repo>-<first 8 characters of ID>`. The app data directory is `~/.config/detail-views`. It is not in a repo.
- **Branch name.** `claude/<name as a slug>-<first 8 characters of ID>`, made from the remote's default branch (`origin/HEAD`, else `master`), fetched just before. The local checkout is not changed. If the fetch fails, the session is not made.
- **Temporary directory.** `mkdtemp(<system temp>/claude-…)`.
- **Command.** `claude -p <prompt> --output-format json --model claude-opus-5-5 --permission-mode <mode> (--session-id | --resume) <ID> --allowedTools mcp__detail-views --mcp-config <the app's MCP server>`, in the session directory. The last two are there only while the app's MCP server runs (`docs/mcp.md`). The answer is `result`. The cost is kept as `cost`.
- **Permission mode is per session** (`permissionMode`). New sessions get `auto`.
- **Answer is one item.** The response item keeps all of the answer as its `text`. The app does not split it. `b` can split it into notes (`src/core/outline.ts`); those notes keep the type `claude.response`.
- **Order.** The link to the response is 1 ms after the link to the prompt. Thus, the response is always after its prompt.
- **The prompt box is the view's composer.** The session's module view gives `compose: 'claude'`, and the module's `submit` prompts the session. Thus, the box uses the same path as other composers (`Core.submit`), on the desktop and on the phone. The draft is kept for each session.
- **The chat is a tree.** It is the usual tree, not a `Detail` view. Thus, the keys work on prompts and responses as on all items (`e`, Enter, `b`, `c`…).
- **Errors.** A failure of `claude` (a non-zero exit, or `is_error` in its JSON) sets the response item: `text` and `error` are the full message, `running` is false. The message gives the program, the directory, the exit code, and all that the program wrote. The view shows all of it.
- **Paths.** `~` is the home directory. The app expands it, because a program that the app starts does not. The form refuses a directory that does not exist.
- **The `claude` program.** The app looks for it on the `PATH`, then in `~/.local/bin` and `~/.claude/local`. An app that starts from a launcher can have a short `PATH`.
- **ENOENT.** Node gives `spawn <program> ENOENT` both when the program is missing and when the working directory is missing. The app finds which one it is and says so.
- **Phone.** Shift+K is a tool with a label. Thus, it is also a button on the phone. The prompt box is the same on the phone.
- **Look.** Sessions and responses start with the Claude mark (`src/renderer/src/assets/claude.svg`, from Simple Icons, CC0-1.0), in the `--claude` colour (terracotta). A prompt has the `accent` tint. A response has no tint (`error` if it failed). Thus, my words and Claude's words are different at a glance.

## Items

| Type | Is | Under |
|-|-|-|
| `claude.home` | The module root. Keeps `cwds`. | The sidebar |
| `claude.session` | A session. `text` is its name. | `claude` |
| `claude.prompt` | What I asked. `session` is its session. | The session |
| `claude.response` | The answer, or the error. `running` while Claude works. | The session, after its prompt |

## Not done

- No streaming of the answer.
- A session is not attached to items in the notes. Claude is not told which item I am at.
- No link from a session to the PR of its branch.
- No form to change the permission mode or the model of a session.
