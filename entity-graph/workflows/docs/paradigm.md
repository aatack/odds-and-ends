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
  Examples: a bot replies, CI finishes, or a person replies on Slack.
- When a blocker is complete, it records its result.
- An item can have more than one blocker at the same time.
- If an item has no blockers, it waits for you.
- An item that contains an open item is blocked by it. Thus you do the smaller
  items before the item that contains them.

| Blocker | It waits for |
| --- | --- |
| Bot | A bot to read the item and reply. |
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
- The app shows blockers that are not complete above the chat input. Each blocker
  shows how long it has blocked the item.
- When you send a message, a bot replies by default. Push Tab to select a
  different action. For example, you can send only a message, or wait for a pull
  request to merge.

## Bots

- Each item has its own bot conversation. The bot remembers the item.
- A new item inside an item starts with a copy of the conversation of the item
  that contains it. Thus its bot knows why the item exists.
- When a bot starts again, it gets all events that occurred after its last start.
- A bot can reply to you, add blockers, or close the item.
- When a bot asks you a question, the question must be short and direct.

## Safety

- Some actions change things outside the app. For example, they send a message
  or merge a pull request. These actions are **unsafe**.
- Bots cannot use unsafe actions.
- Scripts that bots write cannot use unsafe actions.
- To do an unsafe action, a bot asks you for approval. You can do the action,
  change it first, or cancel it.

## History

- The app keeps the full history of each item: every event and its result.
- The app records the time of each event, and who or what did it.
- Bots can read this history. They use it to find which processes are slow, where
  you had to change their work, and where you did work outside the app.
