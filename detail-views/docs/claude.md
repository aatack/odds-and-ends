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
12. **Shift+K** opens a form: name, directory, worktree. Enter starts the session. Escape cancels.
13. The new session item goes under the selected item. The selected item gets `claudeSessionId: <session ID>`.
14. Keep each directory that I enter on the Claude root (`cwds`). Give these as options in the next form. Put the last one in the field.
15. **k** opens a text box under the selected item. This is the same box as a new note, with a different action when I write it.
16. To find the session for **k**, look along the selection path, from the selected item to the root of the view. Use the first item that is a session, or that has `claudeSessionId`. If there is no such item, show an error toast.
17. When I write the prompt, put a prompt item under the selected item. Also link the prompt item under the session item.
18. When Claude answers, put the answer in an item under the prompt item. The structure is: `[item + session] → [prompt] → [response]`.
19. If Claude fails, put an error message in the response item instead of the answer.
20. While Claude works, the response item shows how long it has run.
21. After each prompt, if the branch in the session directory has a GitHub PR, link the PR item under the session item. (A "branch" here is a PR. The app has no branch items.)
22. Do not stream the answer. Show it when Claude is done.
23. Use the model Opus 5.5 (`claude-opus-5-5`).
24. Use Claude Code auto mode for permissions (`--permission-mode auto`).

## Key decisions

- **Items are owned.** Sessions, prompts and responses go in the owned store. They do not go in the cache store, because the cache store is cleared each week.
- **Claude writes as `claude`.** Undo (Ctrl+Z) removes only my own edits (author `me`). Thus, undo does not remove an answer.
- **One prompt at a time for each session.** Prompts wait in sequence, in the order that I write them.
- **First prompt, then resume.** The first prompt uses `--session-id <ID>`. Each prompt after it uses `--resume <ID>`. The session item keeps `started: true` after the first answer.
- **Worktree location.** `<app data directory>/worktrees/<repo>-<first 8 characters of ID>`. The app data directory is `~/.config/detail-views`. It is not in a repo.
- **Branch name.** `claude/<name as a slug>-<first 8 characters of ID>`, made from the current commit of the repo.
- **Temporary directory.** `mkdtemp(<system temp>/claude-…)`.
- **Command.** `claude -p <prompt> --output-format json --model claude-opus-5-5 --permission-mode <mode> (--session-id | --resume) <ID>`, in the session directory. The answer is `result`. The cost is kept as `cost`.
- **Permission mode is per session** (`permissionMode`). New sessions get `auto`.
- **PR look-up is a read.** `git rev-parse --abbrev-ref HEAD` and `git remote get-url origin` give the branch and the repo. A GraphQL query through `gh` finds the PR. The app does not write to GitHub for this.
- **Errors.** A failure of `claude` (a non-zero exit, or `is_error` in its JSON) sets the response item: `text` and `error` are the message, `running` is false.
- **Phone.** Shift+K and k are tools with labels. Thus, they are also buttons on the phone.
- **Look.** Sessions and responses start with the Claude mark (`src/renderer/src/assets/claude.svg`, from Simple Icons, CC0-1.0), in the `--claude` colour (terracotta). A response has a terracotta rail and a faint fill. A prompt has an accent (indigo) rail and a faint fill. Thus, my words and Claude's words are different at a glance.

## Items

| Type | Is | Under |
|-|-|-|
| `claude.home` | The module root. Keeps `cwds`. | The sidebar |
| `claude.session` | A session. `text` is its name. | The item it started from, and `claude` |
| `claude.prompt` | What I asked. `session` is its session. | The item I asked from, and the session |
| `claude.response` | The answer, or the error. `running` while Claude works. | Its prompt |

## Not done

- No streaming of the answer.
- No branch items. A branch without a PR gets no link.
- No form to change the permission mode or the model of a session.
