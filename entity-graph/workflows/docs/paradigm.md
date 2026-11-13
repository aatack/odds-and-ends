# The paradigm

This page gives the idea of the app. It does not give the implementation.
For the implementation, see [`items.md`](./items.md).
The text uses ASD-STE100 Simplified Technical English.

## Purpose

- You have many tasks. They come from different sources, for example Slack, the
  calendar and GitHub.
- Bots can do much of this work.
- You push one button. The app shows the next item that needs you.
- Bots do all the other work.

## Topics and items

- "Item" is the general word for a thing in the database: a topic, an
  approval, a script, a bot, a person.
- A **topic** is the one thing that people write. It can be a comment, a
  task or a note. The first line of its text is its title.
- A topic can have a status. It is open, or it is closed. A topic with no
  status is a comment. An open topic is a task, or a checkbox in a list of
  notes. To close a topic is to set its status to closed.
- A topic has two lists of items:
  - Its **comments**. This is its chat: the history of the topic, in order.
  - Its **notes**. This is a tree of topics.
- A topic shows one of the two lists. Use the menu at the top of the topic to
  select which. The chat is the default.
- A link of the type `comments` or `notes` connects a topic to each item in
  that list.
- A topic can contain other open topics. A bot can split a task into smaller
  tasks in this way. A topic in a topic does not block it.
- Some items are complete when they occur. Example: a comment.
- Other items are **blockers**. A blocker waits until a condition occurs.
  Example: a person approves an action.
- When a blocker is complete, it records its result.
- A topic can have more than one blocker at the same time.
- A person, a bot or a script can **have** an item. To give an item to a
  person, a bot or a script is to **assign** it. Any item can be assigned. An
  assignment is a link of the type `assignees` from the item to the person, bot
  or script. It is not an item in the topic.
- If you have an item and it has no blockers, it waits for you.
- Topics, bots and scripts have an **owner**: a person or a bot. A link of the
  type `owns` goes from the owner to the item. If a person made the item, the
  owner is that person. If not, the owner is the owner of the topic that the
  item is in. If that topic has no owner, the owner is the owner of the bot that
  made the item.
- A new open topic that you make is yours. A new topic that a bot makes has
  nobody, unless the bot gives it to somebody.
- To wait for something, give the topic to a script.

| Blocker | It waits for |
| --- | --- |
| Approval | You to approve an action. |

## The chat

- The app shows the comments of a topic as a chat, each as a card: comments,
  open topics, scripts and approvals. Your items are on the right. Items from
  other people, bots and scripts are on the left. A bot that wants to show
  something in the chat adds it to the topic as an item.
- A script card shows what the script is, a play button (a person's press runs it now), when it runs next, and what
  it finished with.
- The top of each item shows the item (its icon and its title) and who has it.
  Click who has it to give the item to more people or bots, or to take it from
  them.
- When you send text, it is a comment in the topic.
- Type "@" and select a bot or a person to give the item to them. When you send
  the comment, they get the item and you do not have it.
- When the chat input is empty, push Enter to give the item to a bot or a person.
  The app selects who had the item last, or who you gave an item to last. Push
  Tab to select a different bot or person.

## Bots

- A bot is an item of its own. It has a name, instructions, a model and a folder.
  You make bots on the Bots page. Different bots can use different models. The
  model is opus if you do not select a different model.
- A bot has a chat of its own. Write in it to talk to the bot. When you write in
  it, the app gives the bot item to the bot.
- A bot works on one item at a time. A script does the same.
- Every minute, the app looks at each bot that does not run. It selects the
  first item that the bot has (the item that it had for the longest time
  first) that the bot did not work on in the last 30 minutes. The bot then
  works on that item.
- If somebody gives the item to the bot again, the bot works on it again at
  once. If a person writes in the item while the bot works on it, the bot works
  on it again at once.
- Push Wake now on the bot page to make the bot start at once.
- A bot does one run at a time. Thus two bots do not work on the same thing at the
  same time by accident.
- A bot has one conversation, for all of its items. The bot remembers its work.
  Its instructions are in its system prompt. After the conversation becomes
  shorter (compaction), the bot gets its instructions again.
- In each run, the bot gets the items in its item that occurred after it last
  worked on that item. The first time that it sees an item, it gets all of the
  item.
- The bot must do the work on its item, and then give the item to the next
  person or bot. It can also keep the item.
- For its item, a bot can reply, add blockers, give the item to a person or to a
  different bot, or close the item.
- If the bot does not give an item to somebody, the item goes back to its
  owner. A bot never keeps an item only to wait: it hands it to a script that
  gives it back.
- A bot does not close a topic unless somebody tells it to close the topic.
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
- The search page (Ctrl+L) shows two lists. The first list is the open items
  that you have. The second list is the open topics, bots and scripts that you
  own and do not have.
- Use the menu at the top right of the search page to see the search page of
  other people or bots.
- Next (Ctrl+D) shows the next item that you have, and that nothing blocks.
  It does not show the item on the screen. It does not show items that you
  looked at in the last 30 minutes.
- History (Ctrl+T) lists the items that Back goes through, the nearest first.
  Type to search it. Pick an item to go back to it.

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
  the app keeps only the status of a topic (`open`), and whether a blocker is
  complete (`resolved`). The database records when it changed, and who changed
  it.
- Click the title of an item to open its menu. Use the menu to debug the item.
- The debug page shows an item as the database keeps it: its values, its links,
  and who made it and when.
- Bots can read this history. They use it to find which processes are slow, where
  you had to change their work, and where you did work outside the app.
