// How the app works: a guide for each page that has one (Home, Integrations, Types), each in sections, written once
// for two readers. A person reads it on the page, as markdown, with buttons to the places it names and the keys that
// do each thing. An agent is told the tool calls instead: Home's is the MCP server's instructions, and the `paradigm`
// action answers any of them, as the page's *For bots* shows it and its Copy copies it.
//
// In the person's text, a link to `#<page>` is drawn as a button that goes there (`items`, `bots`, `types`,
// `integrations`, `log`, `phone`, `mcp`), and `#next` as one that does Next; `code` is drawn as a key.

export type Reader = 'human' | 'agent'
export type Guide = 'home' | 'integrations' | 'types'
export const GUIDES: Guide[] = ['home', 'integrations', 'types']

export function guide(page: Guide, reader: Reader): string {
  /** The same point, as each reader acts on it. */
  const say = (human: string, agent: string) => (reader === 'human' ? human : agent)
  return page === 'home' ? home(say) : page === 'integrations' ? integrations(say) : types(say)
}

type Say = (human: string, agent: string) => string

function home(say: Say): string {
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
- Anything can be **closed**${say(` (*Close* in its right pane, or \`Ctrl+Enter\` twice in its chat)`, ' (close_item)')}. Closing means exactly this, and nothing else: its owner and all its assignees are taken off it, and its open is set to false. Nothing in it changes. A closed bot or script has no owner, so nothing runs it. ${say(
  `*Reopen* sets it open again and makes you its owner and assignee.`,
  `Only a person reopens an item.`,
)}

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

## Views

Every item can be looked at in a few ways; its display says which it opens on, and its type decides when it doesn't say.

- **Comments**: its chat. A topic opens here, and so does a script.
- **Notes**: its tree. For a plan or a write-up, set a topic's display to notes.
- **Focus**: its type's own view: a file shown whole, a pull request's page, a link, a Slack thread.
- **Assignments**: what is assigned to it, then what it owns, then what is pinned to it. A person or a bot opens here: your [Assignments](#items) is yours.
${say(
  `- **Debug** and **Edit log**: everything it holds, and every change to it.

Press a topic's header, or right-click anything and hover *Open*, to see it another way.`,
  `
get_item with \`size: focus\` reads an item as it opens: a chat as its messages, notes as an indented tree with ids, anything else as its values. set_value on \`display\` (\`comments\`, \`notes\`, \`focus\` or \`assignments\`) changes how a topic opens for everyone. Write a plan or a write-up to be commented on as notes, point by point, and show the topic as notes; say what you did in its comments.`,
)}

## Bots and scripts

- A **bot** is a Claude session with a name and instructions, and a folder of its own; both last from one run to the next. ${say(`See them, and what each is doing, on [Bots](#bots).`, `list_bots lists them.`)}
- A **script** is a bot without a model: JavaScript, with a name, given topics the same way. Use one to wait on something, or for anything repetitive that needs no LLM (a snooze, a check every 10 minutes until a branch merges). Make one once and give it every topic of its kind. It costs nothing to run. The app runs the scripts the person signed in owns, or bots they own do. A script owned by an item type is offered in the right pane of every item of that type${say(
  ' (see [Types](#types)).',
  ` (set_owner to "topic", "bot"…). run_script runs code once, now. See item_types for a script's exact shape.`,
)}

**When they wake.** Bots and scripts work one item at a time. Every minute, each is run on the first item assigned to it, the longest held first, that is due:

- it was just given it, or given it again by someone else;
- someone wrote in it while it was running on it (it didn't see that, so it keeps the topic and runs again);
- its cycle has passed since it last ran on it: 30 minutes unless its cycle minutes say otherwise, and never with 0;
- ${say(`you pressed *Wake now* on its page, or wrote in its own chat (which assigns it to itself).`, `it was woken from its page, or someone wrote in its own chat (which assigns it to itself).`)}

A run is told what is new in the item since it last ran on it. When it ends, the item goes where the run said; ${say(
  `if it said nowhere, it goes back to its owner.`,
  `with no \`assign\` in your update, it goes back to its owner. Assign it to yourself to keep it, and you come back to it after your cycle.`,
)}

## The kinds of item

- **topic**: what is said, a thing to be done, a note.
- **bot**: a Claude session that works on what is assigned to it.
- **script**: a bot without a model, running JavaScript.
- **user**: a person who signs in.
- **link**, **github_pr**, **file**, **slack_message**, **slack_channel**: things from outside, pulled in to sit in a topic or be pinned beside it.

${say(`[Types](#types) says more of each, with the actions each has.`, `The types guide (paradigm with \`page: types\`) says more of each; item_types gives each one's params in full.`)}

## The world outside

Changing anything outside this app (posting, merging, sending) is unsafe: ${say(
  `bots do it only when a person has asked them to. What the app reaches outside (GitHub, Slack, Claude, Deepgram) is on [Integrations](#integrations).`,
  `do it only when a person has asked for it. The integrations guide (paradigm with \`page: integrations\`) says what the app reaches outside and which tools are unsafe. The app's settings that bots may change (how many bots and scripts run at once, integrations' setup such as the GitHub default repo): settings lists them, set_setting changes one. Secrets and how the app is reached are not among them.`,
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

function integrations(say: Say): string {
  return `## What the app reaches outside

Each integration is set up once, ${say('on this page', 'by a person on the Integrations page')}. Reading through one is always fine; changing anything outside (a comment, a merge, an issue) happens only when a person has asked for it${say(', and bots are told so', ': those tools are marked unsafe')}.

## Claude

Runs the bots: each is a \`claude\` session on this machine${say(
  `. *Trust bot folders* marks the bots' folders, and the repos in them, as trusted, so Claude Code reads their \`.claude/settings.json\`.`,
  ', in a folder of its own, with this app as its "workflows" MCP server.',
)}

## GitHub

Pull requests, issues and the REST API, through the \`gh\` CLI on this machine, signed in as whoever \`gh\` is logged in as. A pull request's link in a note becomes a **github_pr** item, which shows its state and checks. The default repo is the one used when none is named.
${say(
  '',
  `
- Reading: github_pr_list, github_pr_view, github_pr_checks, github_pr_diff, github_issue_list, github_issue_view, github_api_get.
- Unsafe (only when asked): github_pr_comment, github_pr_merge, github_pr_automerge, github_issue_comment, github_issue_create.
- The default repo is one of the settings: settings lists it, set_setting changes it.`,
)}

## Slack

Read only: the app never posts. It reads with a person's user token, one per workspace. ${say(
  `Paste a Slack message's link into an empty chat input, or write it in a note, and it is pulled in as a **slack_message**, shown as Slack shows it, with its thread. A **slack_channel** is a channel over a stretch of time. Channels you follow show on your [Assignments](#items).`,
  `A Slack link in a person's note becomes a **slack_message** item by itself; add one with add_items (its link, or channel and ts), or a **slack_channel** (a channel from a time).
- Reading: slack_workspaces, slack_channels, slack_message, slack_messages, slack_messages_around, slack_channel_history, slack_thread, slack_replies, slack_search.`,
)}

## Deepgram

${say(
  "Speech to text for the chat input's record button: what you say is sent as you finish each sentence. It needs an API key from Deepgram's console.",
  "Speech to text for a person's record button in the chat input. Nothing for bots to call: a recording arrives as the person's comments.",
)}
${say(
  `
## Your phone

The same app, on your phone, over Tailscale: set it up on [Phone](#phone).`,
  '',
)}
`
}

function types(say: Say): string {
  return `## Items and their types

Everything in the app is an item, and every item has a type. What is the same for all of them: who made it and when, who owns it and who has it, whether it is open or closed, its comments, notes and pins, and the views it can be seen in. What differs is what it holds, how it is shown, and what can be done to it.

## The types

- **topic**: what people write: something said (a comment), a thing to be done (open), a note in a tree. It opens on its comments, unless its display says.
- **bot**: a Claude session with a name and instructions, working on what is assigned to it. It opens on its assignments.
- **script**: a bot without a model: JavaScript, run on what is assigned to it, or on a press. It opens on its comments.
- **user**: a person who signs in, by username. Things are assigned to people as to bots.
- **link**: a web page. A URL in a note becomes one, so the same page is one item wherever it is mentioned.
- **github_pr**: a GitHub pull request, from its link: its state, checks and comments.
- **file**: a file on this machine, shown whole: an image, text, a PDF or a Word document.
- **slack_message**: a Slack message, from its link, shown with its thread.
- **slack_channel**: a Slack channel over a stretch of time.

A bot, a script and a person stay open for as long as they last (removing one closes it); every other item is done as it is added, and a topic is open only when it says so.

## What can be done to each

Every type has the app's own actions (Close or Reopen, and the type's own), and then the scripts it owns: ${say(
  `each is offered in the right pane of every item of that type, to press. *New script* below makes one for a type.`,
  `set_owner a script to a type's name ("topic", "bot"…) and it is offered in the right pane of every item of that type, and run on that item when pressed, with it as \`item\`.`,
)}
${say(
  '',
  `
item_types gives each type's params (as JSON Schema), what it resolves with, and a script's exact shape. add_items makes any of them.`,
)}
`
}

/** What an agent connected over MCP is told: the server's instructions, and the `paradigm` action. */
export const PARADIGM = guide('home', 'agent')
