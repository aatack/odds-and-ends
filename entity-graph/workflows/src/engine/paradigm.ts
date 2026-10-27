// The paradigm, as short as it can be: what an agent connected over MCP is told
// (the server's instructions) and can read again (the `paradigm` action).

export const PARADIGM = `How this works:
- A task is a thing to be done (an item is any thing in the database; the tools say item). Everything in it is an event (a contains link from it), oldest first: its history, and its chat. To make something appear in a task's chat, add it as an event in the task (add_events): a note to say something, a script, a child task.
- Some events just happen (a note). Blockers wait: an approval, or a child task to close.
- Items are assigned to people, bots and scripts (the assign action: an assigned_to link from the item). People (list_people) sign in by username. A bot is an item of its own (list_bots), with a chat of its own: it wakes when an item is assigned to it, its settings change or someone writes in its chat, and every 15 minutes while its items have news; it does them all in one run, one run at a time.
- An item is a person's turn when it is assigned to them and nothing blocks it but approvals. Otherwise it is waiting, and they need not look.
- To move an item on: add events to it (add_events). Say something with a note; hand it on with assign. To wait on something, or for anything repetitive that needs no LLM (a snooze, a check every 10 minutes until a branch merges), hand the task to a script: add a script event with wake_at and owns [the task]; it runs its JavaScript on its own wake times, says things in any tasks, and hands the tasks on when it is done. It costs nothing to run. run_script runs code once, now. See event_types for the script's exact shape.
- To split work off, add a child item: it blocks its parent until it closes. To file unrelated work, add an item with detached: true.
- Changing anything outside this app (posting, merging, sending) is unsafe: only people do that.
- Finding work: items_waiting_on (a username, bot, a bot's name, or a blocker type), next_for_human, list_items, get_item. event_types describes every event.`
