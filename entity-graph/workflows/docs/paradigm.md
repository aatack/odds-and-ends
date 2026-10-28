# The paradigm

This page gives the idea of the app. It does not give the implementation.
For the implementation, see [`items.md`](./items.md).
The text uses ASD-STE100 Simplified Technical English.

## Purpose

- You have many tasks. They come from different sources, for example Slack, the
  calendar and GitHub.
- Bots can do much of this work.
- You push one button. The app shows the next task that needs you.
- Bots do all the other work.

## Tasks, events and items

- A **task** is one thing to do.
- A task has a list of **events**. The list is the history of the task.
- A task is also an event. Thus a task can contain other tasks. A bot can
  split a task into smaller tasks in this way.
- The database keeps each event as an **item**. "Item" is the general word for a
  thing in the database. A link of the type `contains` connects a task to each
  of its events.
- Some events are complete when they occur. Example: a note.
- Other events are **blockers**. A blocker waits until a condition occurs.
  Examples: a script finds that CI is complete, or a person approves an action.
- When a blocker is complete, it records its result.
- A task can have more than one blocker at the same time.
- A person or a bot can **have** a task. To give a task to a person or to a bot
  is to **assign** it. An assignment is a link from the task to the person or bot.
  It is not an event in the task.
- If you have a task and it has no blockers, it waits for you.
- A new task that you make is yours. A new task that a bot makes has nobody,
  unless the bot gives it to somebody.
- A task that contains an open task is blocked by it. Thus you do the smaller
  tasks before the task that contains them.

| Blocker | It waits for |
| --- | --- |
| Task | A task inside this task to close. |
| Approval | You to approve an action. |
| Script | A condition that a small program checks at set times. |

## The chat

- The app shows all of the events in a task as a chat, each as a card: notes,
  child tasks, scripts and approvals. Your events are on the right. Events from
  other people, bots and scripts are on the left. A bot that wants to show
  something in the chat adds it to the task as an event.
- A script card shows what the script waits for. If the script has no schedule,
  it shows a play button: push it to run the script. If the script has a
  schedule, the card shows how many times it ran and when it runs next. When the
  script is complete, the card shows a tick and the result.
- The app shows who has the item at the top right. Click it to give the item to
  more people or bots, or to take it from them.
- When you send text, it is a note in the item.
- Type "@" and select a bot or a person to give the item to them. When you send
  the note, they get the item and you do not have it.
- When the chat input is empty, push Enter to give the item to a bot or a person.
  The app selects who had the item last, or who you gave an item to last. Push
  Tab to select a different bot or person.

## Bots

- A bot is an item of its own. It has a name, instructions, a model and a folder.
  You make bots on the Bots page. Different bots can use different models. The
  model is opus if you do not select a different model.
- A bot has a chat of its own. Write in it to talk to the bot.
- A bot starts when an event occurs to the bot itself. Examples: somebody gives
  it an item, somebody changes its settings, or somebody writes in its chat.
- A bot also starts every 15 minutes, if events occurred in its items after its
  last run.
- A bot does one run at a time. Thus two bots do not work on the same thing at the
  same time by accident.
- A bot waits for a short time (1 minute, if you do not change it) after the last
  event before it starts. Thus you can give it many items quickly, and it does
  them all in one run.
- A bot has one conversation, for all of its items. The bot remembers its work.
  Its instructions are in its system prompt. After the conversation becomes
  shorter (compaction), the bot gets its instructions again.
- In each run, the bot gets all of its items. For each item, it gets the events
  that occurred after its last run. The first time that it sees an item, it gets
  all of the item.
- The bot must do the work on its items, and then give each item to the next
  person or bot. It can also keep an item.
- For each item, a bot can reply, add blockers, give the item to a person or to a
  different bot, or close the item.
- If the bot does not give a task to somebody, the task goes back to its owner:
  the person who made it. If the bot added a wait, it keeps the task.
- A bot does not close a task unless somebody tells it to close the task.
- The bot page shows the result of the last run of the bot.
- When a bot asks you a question, the question must be short and direct.

## Find work

- When you start the app, you write your username and a source. There is no
  password, because the app is on your computer. People are items too. If
  nobody has the username, the app makes a new person.
- A source is a file that holds the items. Write its path, for example
  `~/tasks/tasks.sqlite`. If the file does not exist, the app makes it.
- The app keeps the username and the source for the next start. Use the menu to
  sign out, or to close the source.
- Next shows the items that you have, and that nothing blocks. The item that
  nobody touched for the longest time comes first.

## Safety

- Some actions change things outside the app. For example, they send a message
  or merge a pull request. These actions are **unsafe**.
- Bots and other agents cannot use unsafe actions.
- Scripts that agents write cannot use unsafe actions.
- To do an unsafe action, an agent asks you for approval. You can do the action,
  change it first, or cancel it.
- Bots can also use a shell. Their instructions tell them not to do unsafe
  things with it.

## History

- The database (egdb) keeps the full history of each item: every value that it
  had, and every link. The app does not keep a different history.
- The database records the time of each change, and who or what did it. Thus
  the app keeps only whether an event is complete (`resolved`). The database
  records when it became complete, and who completed it.
- Click the title of a task to open its menu. Use the menu to debug the task.
- The debug page shows an item as the database keeps it: its values, its links,
  and who made it and when.
- Bots can read this history. They use it to find which processes are slow, where
  you had to change their work, and where you did work outside the app.
