// The paradigm, as short as it can be: what an agent connected over MCP is told
// (the server's instructions) and can read again (the `paradigm` action).

export const PARADIGM = `How this works:
- An item is a thing to be done. Everything in it is an event, oldest first: its history.
- Some events just happen (a note). Blockers wait: an approval, a child item to close, a script to say it is done.
- Items are assigned to people and to bots (the assign action: an assigned_to link from the item). People (list_people) sign in by username. A bot is an item of its own (list_bots), with a chat of its own: it wakes when an item is assigned to it, its settings change or someone writes in its chat, and every 15 minutes while its items have news; it does them all in one run, one run at a time.
- An item is a person's turn when it is assigned to them and nothing blocks it but approvals. Otherwise it is waiting, and they need not look.
- To move an item on: add events to it (add_events). Say something with a note; hand it on with assign; wait on something with a script. A script that never says it is done is a loop until cancelled.
- To split work off, add a child item: it blocks its parent until it closes. To file unrelated work, add an item with detached: true.
- Changing anything outside this app (posting, merging, sending) is unsafe: only people do that.
- Finding work: items_waiting_on (a username, bot, a bot's name, or a blocker type), next_for_human, list_items, get_item. event_types describes every event.`
