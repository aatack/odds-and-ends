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

// Home follows the workflow, which is the source of truth for the whole app: get the next thing, read what you need,
// act, hand it on. Each other guide sits under one step (Types under reading, Integrations under handing on), and
// what fits no step is in *Other things*, to move into a step or take out of the app.
function home(say: Say): string {
  return `${say(
    `YAWN shows you the next thing that needs you. It also gives you what you need to decide. You give your input, then you give the item to the next person, bot or script.`,
    `This app shows a person the next thing that needs them, with what they need to decide. They give their input, then they give the item to the next person, bot or script. You are one of the bots that items are given to. Your work follows the same steps.`,
  )}

Each part of the app helps with one of these four steps:

1. **Get the next thing.**
2. **Read what you need.**
3. **Act.**
4. **Hand it on, or close it.**

## 1. Get the next thing

An item is your turn when it is assigned to you. All the items assigned to you are your list.

${say(
  `Press [Next](#next) (\`Ctrl+D\`) to open the next item on your list. Next starts with the item that you have had for the longest time. It skips the items that you looked at in the last 30 minutes.

To see your full list, open [Assignments](#items) (\`Ctrl+L\`). It shows the items assigned to you first. Then it shows the items that you own, and the items pinned to you.`,
  `The app runs you on one topic at a time, from your list. Your prompt gives the topic's id, and tells you what is new in it since your last run. Read the topic with get_item.

To find other work, use items_waiting_on (a username, a bot's name, or a blocker type), next_for_human, list_items and get_item. A person's list shows the items assigned to them first, then the items that they own.`,
)}

## 2. Read what you need

Everything you need to decide is in the item, or pinned beside it. Most items are **topics**. A topic is what people write: something said, a thing to do, or a note in a tree. The first line of its text is its name. A topic is open (a thing to do, or a checkbox), closed, or neither (a comment).${say(
  '',
  ` In the tools, an item is anything in the database. Its status is \`open\`: true when open, false when closed, and not there for neither.`,
)}

A topic has three places to read:

- **Comments** are its chat. They are its history: who said what, what they added, and who they gave it to.
- **Notes** are a tree of topics. They show what the topic knows now, in order. Use them for a plan, a checklist (open notes are its boxes), or a write-up to comment on point by point.
- **Pins** show beside it. They are the items that help you act on it: a pull request, a link, a file, a split-off topic, or the topic it came from.

Each item opens in one **view**. Its display sets the view. If it has no display, its type sets the view.

- **Comments**: its chat. A topic or a script opens here.
- **Notes**: its tree.
- **Focus**: the view for its type. For example, a whole file, a pull request's page, or a Slack thread.
- **Assignments**: what is assigned to it, then what it owns, then what is pinned to it. A person or a bot opens here.
- **Pins**: what is pinned to it, and the items it is pinned to.

${say(
  `To see an item in a different view, press the header of the topic. Or right-click the item and go to *Open*.`,
  `get_item with \`size: focus\` reads an item as its view shows it. A chat gives its messages, notes give an indented tree with ids, and other items give their values. Set \`display\` with set_value (\`comments\`, \`notes\`, \`focus\`, \`assignments\` or \`pins\`) to change how a topic opens for everyone.`,
)}

Many types of item can be pinned or added to a topic: links, pull requests, files, Slack messages and channels, bots, scripts and people. ${say(
  `[Types](#types) tells you about each type.`,
  `The types guide (paradigm with \`page: types\`) tells you about each type. item_types gives the params of each type.`,
)}

## 3. Act

Do the work in the app when you can. Then the next person can see what you did.

- **Write in the chat.** ${say(
  `Type in the box at the bottom, and press \`Enter\` to send. Press \`Tab\` to add a different thing: an open topic, a script, and so on. You can also speak: the record button uses [Deepgram](#integrations).`,
  `Use add_items on the topic to add a comment (a topic with only text), a script, or an open topic.`,
)}
- **Change the notes.** ${say(
  `In the tree, \`Enter\` adds a note, \`?\` adds an open note, and \`/\` adds a heading. \`e\` edits a note. \`w\` and \`s\` move it up and down.`,
  `Use add_items with \`via: notes\` to add to the tree. Write a plan or a write-up as notes, one point to each note, and set the topic's display to notes. Then say what you did in its comments.`,
)}
- **Split off work.** Add an open topic in the chat. It is pinned to the topic as you make it. It does not block the topic.${say('', ' With `detached: true`, it goes in no topic.')}
- **Pin what will help later.** Attach the item itself, not only a mention of it. ${say(
  `Right-click any item and choose *Pin to*.`,
  `Use the pin action, or \`pin: true\` on an item in add_items. get_item lists an item's pins, and where it is pinned (pinnedIn).`,
)}
- **Run a type's scripts.** A script that a type owns shows in the right pane of each item of that type. ${say(
  `Press it to run it on the item.`,
  `To make one, set_owner a script to the type's name ("topic", "bot"…).`,
)}

Changes outside the app (a post, a merge, a message) are not safe. ${say(
  `Bots make them only when a person asks.`,
  `Make them only when a person asks you to.`,
)}

## 4. Hand it on, or close it

When you have done your part, give the item to the next person, bot or script. Do not keep an item that someone else can move on.${say(
  '',
  ' Leave a person only what nobody else can answer, as one direct question.',
)}

- **Assign it.** ${say(
  `Use the top right of the topic, or right-click it and choose *Assign to*. Or write \`@name\` in its chat.`,
  `Use the assign action (an \`assignees\` link from the item), or \`assign\` in your update. People sign in by username (list_people).`,
)} You can assign an item to more than one person, bot or script.
- **To a person**: it goes on their list, and Next shows it to them.
- **To a bot**: a bot is a Claude session with a name, instructions and a folder of its own. It does the work and then hands the item on. ${say(`[Bots](#bots) shows each bot and what it does now.`, `list_bots lists the bots.`)}
- **To a script**: a script is a bot without a model. It runs JavaScript, and it costs nothing to run. Use a script to wait for something: for example, a Slack message in a channel, or a pull request that merges. Also use one for a repeated task that needs no model. Make one script, and give it all the topics of that kind.${say(
  ' [Integrations](#integrations) shows what bots and scripts can see outside the app.',
  ' The integrations guide (paradigm with `page: integrations`) shows what bots and scripts can see outside the app. item_types gives the shape of a script. run_script runs code one time, now.',
)}

**When bots and scripts wake.** A bot or a script works on one item at a time. Each minute, the app runs it on the first item that is due, oldest first. An item is due when:

- someone gave it the item, or gave it again;
- someone wrote in the item while it was running on it;
- its cycle has passed since its last run on the item (30 minutes, or its cycle minutes; never with 0);
- ${say(`you pressed *Wake now* on its page, or wrote in its own chat.`, `a person woke it from its page, or wrote in its own chat.`)}

The app runs the bots and scripts that the person signed in owns, and the ones that their bots own. When a run ends, the item goes where the run said. ${say(
  `If the run did not say, the item goes back to its owner.`,
  `If your update has no \`assign\`, the item goes back to its owner. To keep it, assign it to yourself. You then come back to it after your cycle.`,
)}

**Owners.** Each topic, bot and script has an owner. The owner is the person who made it, or else the owner of the item it is in. The owner gets the item back when a bot lets go of it.${say('', ' The owner is an `owns` link. set_owner changes it.')}

**Close it** when there is nothing more to do. ${say(
  `Press *Close* in its right pane, or press \`Ctrl+Enter\` two times in its chat.`,
  `Use close_item, only when a person asks.`,
)} Closing removes its owner and all its assignees, and sets open to false. Nothing in it changes. A closed bot or script has no owner, so nothing runs it. ${say(
  `*Reopen* opens it again, and makes you its owner and assignee.`,
  `Only a person reopens an item.`,
)}

## Other things

These parts of the app do not fit one of the four steps yet. We will move each one into a step, or take it out of the app.

${say(
  `- The [Edit log](#log) shows each change, and who made it. The *Debug* view shows all that an item holds.
- The app also runs on your phone: see [Phone](#phone).
- Other apps can connect to YAWN as an MCP server: see [MCP](#mcp).
- Keys:

| Keys | What they do |
| --- | --- |
| \`Ctrl+D\` | [Next](#next): the next item on your list |
| \`Ctrl+L\` | [Assignments](#items); press again for its search line |
| \`Ctrl+F\` | Find in the view |
| \`Ctrl+W\`, \`Alt+Left\` | Back |
| \`Alt+Right\` | Forward |
| \`Enter\` | Send what you typed in a chat |
| \`Tab\` | Change what the chat adds |
| \`Ctrl+Enter\` two times | Close the topic |
| Right-click | Open, copy, pin, assign, or act on any item |`,
  `- activity_log and edit_log show what has happened.
- settings lists the app's settings that bots can change (for example, how many bots and scripts run at one time, or the GitHub default repo). set_setting changes one. Secrets, and how people reach the app, are not in the list.`,
)}
`
}

function integrations(say: Say): string {
  return `${say(
  'This guide is part of step 4 on [Home](#home), *Hand it on*. Bots and scripts use these integrations to watch and act outside the app: for example, a script that waits for a pull request to merge.',
  "This guide is part of step 4 of the paradigm, *Hand it on*. Bots and scripts use these integrations to watch and act outside the app: for example, a script that waits for a pull request to merge.",
)}

## What the app reaches outside

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
  return `${say(
  'This guide is part of step 2 on [Home](#home), *Read what you need*. It tells you about each type of item that you can find in a topic, or pinned beside it.',
  'This guide is part of step 2 of the paradigm, *Read what you need*. It tells you about each type of item that you can find in a topic, or pinned beside it.',
)}

## Items and their types

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
