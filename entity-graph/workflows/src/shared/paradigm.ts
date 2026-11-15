// How the app works, in sections, written once for two readers: a person (the window's Home page, as markdown, with
// buttons to the places it names and the keys that do each thing) and an agent connected over MCP (the server's
// instructions, and the `paradigm` action), which is told the tool calls instead.
//
// In the person's text, a link to `#<page>` is drawn as a button that goes there (`items`, `bots`, `types`,
// `integrations`, `log`, `phone`, `mcp`), and `#next` as one that does Next; `code` is drawn as a key.

export type Reader = 'human' | 'agent'

export function guide(reader: Reader): string {
  /** The same point, as each reader acts on it. */
  const say = (human: string, agent: string) => (reader === 'human' ? human : agent)
  return `## What this app is for

${say(
  `YAWN shows you the one thing that needs your attention next, with what you need to give your input on it beside it. You give it, then hand the thing on to whoever moves it next: a person, a bot, or a script. Everything else waits with them, not with you.

Press [Next](#next) (\`Ctrl+D\`) to open the next thing that is your turn. Read its chat and what is pinned beside it, write what you have to say, assign it on, and press Next again.`,
  `The app shows a person the one thing that needs their attention next, with the context to give their input; then they hand it on. You are one of those it is handed to: you are run on a topic assigned to you, read it (get_item), act on it, and hand it on (assign, or \`assign\` in your update) to whoever moves it next. Leave a person only what nobody else can answer.`,
)}

## Topics

A topic is the one kind of thing people write: something said, a thing to be done, a note in a tree. Its text's first line is its name. Its status is open (a thing to be done, or a checkbox), closed, or none (a comment). ${say(
  `Everything else in the app (a link, a file, a bot) is an item too, shown in a topic or beside it.`,
  `The tools say item for anything in the database. Its status is \`open\`: true while open, false once closed, left out for none.`,
)}

## Who has it: assignees and owners

- Anything open can be **assigned** to people, bots and scripts, one or several. ${say(
  `Assign from the top right of a topic, from its right-click menu (*Assign to*), or by writing \`@name\` in its chat.`,
  `The assign action (an \`assignees\` link from the item), or \`assign\` in your update. People (list_people) sign in by username.`,
)}
- An item is a person's turn when it is assigned to them. What is assigned to you is your list, and Next walks it: the longest held first, skipping what you have looked at in the last half hour. ${say(
  `Your [Assignments](#items) (\`Ctrl+L\`) shows it, then what you own, then what is pinned to you.`,
  `A person's search lists what is assigned to them, then what they own (items_waiting_on, next_for_human).`,
)}
- Topics, bots and scripts have an **owner**: whoever made it, if a person; else the owner of what it is in. It is who the item goes back to when a bot lets go of it${say('', ' (an `owns` link; set_owner changes it)')}.
- Topics don't depend on each other. To wait on something, hand the topic to a script (below).
- Anything can be **closed**${say(` (*Close* in its right pane, or \`Ctrl+Enter\` twice in its chat)`, ' (close_item)')}: it floats free, its owner and assignees taken off it. Reopening it${say('', ' (reopen_item)')} brings it back with them.

## Comments, notes and pins

A topic holds two lists, and has things pinned beside it.

- **Comments** are its chat: what is said in it, in order. It is the history of the topic: who said what, what was added, who it was handed to. ${say(
  `Type in the box at the bottom; \`Tab\` changes what is added (a comment, an open topic to split work off, a script…).`,
  `To make something appear in a topic's chat, add it in the topic (add_items): a comment (a topic with just text), a script, an open topic to split work off (it shows as a card here and blocks nothing; \`detached: true\` files it on its own).`,
)}
- **Notes** are a tree of topics: what the topic knows, kept in order and edited in place, rather than said over time. A plan, a checklist (open notes are its boxes), a write-up to comment on point by point. ${say(
  `A topic's display says which of the two its view opens on: press its header to switch. In the tree, \`Enter\` adds a note, \`?\` an open one, \`/\` a heading, \`e\` edits, \`w\` and \`s\` move up and down.`,
  `A topic's \`display\` says which its view shows; add_items with \`via: notes\` adds to the tree.`,
)}
- **Pins** are what is useful in actioning a topic, shown beside it: an open topic made in it (pinned as it is made), a pull request or preview link, a file, the topic it came from. ${say(
  `Right-click anything and choose *Pin to*.`,
  `The pin action, or \`pin: true\` on an item in add_items. get_item lists its pins and where it is pinned (pinnedIn).`,
)} Attach content where it will be wanted later, rather than only mentioning it.

## Bots and scripts

- A **bot** is a Claude session with a name and instructions. ${say(`See them, and what each is doing, on [Bots](#bots).`, `list_bots lists them.`)} Bots and scripts run one item at a time: every minute, each is run on the first item assigned to it (the longest held first) that it has not run on in the last 30 minutes (its cycle), or that was assigned to it again since. Writing in a bot's own chat assigns the bot to itself.
- A **script** is a bot without a model: JavaScript, with a name, given topics the same way. Use one to wait on something, or for anything repetitive that needs no LLM (a snooze, a check every 10 minutes until a branch merges). Make one once and give it every topic of its kind. It costs nothing to run. The app runs the scripts the person signed in owns, or bots they own do. A script owned by an item type is offered in the right pane of every item of that type${say(
  '.',
  ` (set_owner to "topic", "bot"…). run_script runs code once, now. See item_types for a script's exact shape.`,
)}

## The kinds of item

- **topic**: what is said, a thing to be done, a note.
- **bot**: a Claude session that works on what is assigned to it.
- **script**: a bot without a model, running JavaScript.
- **user**: a person who signs in.
- **link**: a web page; a URL in a note becomes one.
- **github_pr**: a GitHub pull request, with its state and checks.
- **file**: a file on this machine, shown whole.
- **slack_message**, **slack_channel**: a Slack message and its thread, or a channel over a stretch of time.

${say(`[Types](#types) lists them in full, with the actions each has.`, `item_types describes each in full: what it is for, the params it takes, and what it resolves with.`)}

## The world outside

Changing anything outside this app (posting, merging, sending) is unsafe: ${say(
  `bots do it only when a person has asked them to. GitHub and Slack are set up on [Integrations](#integrations).`,
  `do it only when a person has asked for it. The app's settings that bots may change (how many bots and scripts run at once, integrations' setup such as the GitHub default repo): settings lists them, set_setting changes one. Secrets and how the app is reached are not among them.`,
)}

${say(
  `## Getting around

| Keys | What they do |
| --- | --- |
| \`Ctrl+D\` | [Next](#next): the next thing that is your turn |
| \`Ctrl+L\` | [Assignments](#items); again, its search line |
| \`Ctrl+F\` | Find in the view |
| \`Ctrl+W\`, \`Alt+Left\` | Back |
| \`Alt+Right\` | Forward |
| \`Enter\` | Send what is typed in a chat |
| \`Tab\` | Change what the chat adds |
| \`Ctrl+Enter\` twice | Close the topic |
| Right-click | Open, copy, pin, assign or act on anything |

Everything that has changed, and who changed it, is on the [Edit log](#log). The app also runs on your phone: see [Phone](#phone).`,
  `## Finding work

items_waiting_on (a username, bot, a bot's name, or a blocker type), next_for_human, list_items, get_item. activity_log and edit_log say what has happened.`,
)}
`
}

/** What an agent connected over MCP is told: the server's instructions, and the `paradigm` action. */
export const PARADIGM = guide('agent')
