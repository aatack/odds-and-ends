# The paradigm

This page gives the idea of the app. It does not give the implementation.
For the implementation, see [`events.md`](./events.md).
The text uses ASD-STE100 Simplified Technical English.

## Purpose

- You have many tasks. They come from different sources, for example Slack, the
  calendar and GitHub.
- Bots can do much of this work.
- You push one button. The app shows the next task that needs you.
- Bots do all the other work.

## Items and events

- An **item** is one task.
- An item has a list of **events**. The list is the history of the item.
- An item is also an event. Thus an item can contain other items. A bot can
  split a task into smaller items in this way.
- Some events are complete when they occur. Examples: a message, or an action.
- Other events are **blockers**. A blocker waits until a condition occurs.
  Examples: CI finishes, or a person replies on Slack.
- When a blocker is complete, it records its result.
- An item can have more than one blocker at the same time.
- You or a bot can **have** an item. An "assign" event gives an item to you or to a
  bot, or takes it away.
- If you have an item and it has no blockers, it waits for you. If nobody has an
  item, you have it.
- An item that contains an open item is blocked by it. Thus you do the smaller
  items before the item that contains them.

| Blocker | It waits for |
| --- | --- |
| GitHub | An event on a pull request, for example the end of the checks. |
| Slack | A reply to a message. |
| Item | An item inside this item to close. |
| Approval | You to approve an action. |
| After | Another event to be complete. Then it adds more events. |
| Script | A condition that a small program checks at set times. |

## Chains

- An "after" blocker waits for a different event. Then it adds new events.
- It can use a script to select the new events from the result of that event.

Example:

1. A bot changes some code. Then it adds a GitHub blocker for the checks.
2. The bot also adds an "after" blocker for the GitHub blocker.
3. If the checks fail, the "after" blocker adds a bot blocker, and a bot repairs the code.
4. If the checks pass, the "after" blocker adds nothing. The item then waits for you.

## The chat

- The app shows the events of an item as a chat. Your messages are on the right.
  Messages from bots and other sources are on the left.
- The app shows who has the item, and the blockers that are not complete, above
  the chat input. Each blocker shows how long it has blocked the item.
- When you send a message, it is a note in the item.
- Type "@" and select a bot to give the item to the bot. When you send the
  message, the bot gets the item and you do not have it.
- When the chat input is empty, push Enter to give the item to a bot. The app
  selects the bot that had the item last, or the bot that you used last. Push Tab
  to select a different bot.
- When you type, push Tab to select a different action. For example, you can
  wait for a pull request to merge.

## Bots

- A bot is an item of its own. It has a name, instructions, a model and a folder.
  You make bots on the Bots page. Different bots can use different models.
- A bot starts when an event occurs in an item that it has, or in an item that it
  watches. It does one run at a time. Thus two bots do not work on the same
  thing at the same time by accident.
- A bot can wait for a short time after the last event before it starts. Thus
  you can give it many items quickly, and it does them all in one run.
- A bot has one conversation, for all of its items. The bot remembers its work.
  It gets its instructions again at each run.
- In each run, the bot gets the events that occurred after its last run, for
  each item. The first time that it sees an item, it gets all of the item.
- For each item, a bot can reply to you, add blockers, give the item to you or to
  a different bot, or close the item.
- When the bot finishes, it gives the item back to you. If the bot added a wait,
  it keeps the item and starts again when the wait is complete.
- When a bot asks you a question, the question must be short and direct.

## Find work

- Next shows the items that you have, and that nothing blocks. The item that
  nobody touched for the longest time comes first.
- "Handed lately" shows which items went to you or to a bot recently.
- "Lost track" shows the items that you made or had, where nothing occurred for
  some hours. Use it to find items that you forgot.

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

- The app keeps the full history of each item: every event and its result.
- The app records the time of each event, and who or what did it.
- Bots can read this history. They use it to find which processes are slow, where
  you had to change their work, and where you did work outside the app.
