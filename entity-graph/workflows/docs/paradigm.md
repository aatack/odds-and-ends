# The paradigm

This page gives the idea of the app. It does not give the implementation.
For the implementation, see [`handoffs.md`](./handoffs.md).
The text uses ASD-STE100 Simplified Technical English.

## Purpose

- You have many tasks. They come from different sources, for example Slack, the
  calendar and GitHub.
- Bots can do much of this work.
- You push one button. The app shows the next task that needs your decision.
- Bots do all the other work.

## Items

- An **item** is one task.

## Handoffs

- An item has a **queue of handoffs**.
- A handoff tells what must occur next. It also tells who or what does it.
- The first handoff in the queue is active. The item waits for it.
- When the active handoff is complete, it records its **result**. Then the next
  handoff becomes active.
- The next handoff can use the result of the one before it. This is how you chain
  handoffs.
- If the queue is empty, the item comes back to you.

There are six types of handoff:

| Type | The item waits for |
| --- | --- |
| Human | Your decision. The handoff asks one direct question. |
| Bot | A bot to do the work in its instructions. |
| GitHub | An event on a pull request, for example the end of the checks. |
| Slack | A reply to a message. |
| Items | Other items to close. |
| Script | Nothing. A script reads the result before it and selects the next handoffs. |

## Example

1. A bot changes some code. It cannot continue until the checks are complete.
2. The bot sets three handoffs: a GitHub wait, then a script, then nothing more.
3. The GitHub wait records the result of the checks.
4. If the checks failed, the script gives the item to a bot to repair the code.
5. If the checks passed, the script asks you: "Merge this pull request?"

## Bots

- A bot works on one item at a time.
- When it stops, it gives the next handoffs.
- A bot gives you an item only when nobody else can do the work.
- When a bot gives you an item, it asks one direct question with no unnecessary words.
- The app keeps the text that the bot writes, but does not show it unless you ask.

## Safety

- Some actions change things outside the app. For example, they send a message
  or merge a pull request. These actions are **unsafe**.
- Bots cannot use unsafe actions.
- Scripts that bots write cannot use unsafe actions.
- To do an unsafe action, a bot asks you for approval. You can do the action,
  change it first, or do a different thing.

## History

- The app keeps the full history of each item: every handoff, its result, and the
  actions between them.
- The app records the time of each event. For example: when a handoff was set,
  when it started, when a bot started and stopped, when you first saw it, and
  when it was complete. It also records who or what did each thing.
- The app shows the history as a chat. What you typed is on the right. What other
  persons, bots and scripts set is on the left.
- Bots can read this history. They use it to find which processes are slow, where
  you had to change their work, and where you did work outside the app.
