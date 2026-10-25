// The paradigm, as short as it can be: what an agent connected over MCP is told
// (the server's instructions) and can read again (the `paradigm` action).

export const PARADIGM = `How this works:
- An item is a thing to be done. Everything in it is an event, oldest first: its history.
- Some events just happen (a message). Blockers wait: CI to finish, an approval, a child item to close, a script to say it is done.
- Items are assigned to people and to bots (the assign action: an assigned_to link from the item). People (list_people) sign in by username. A bot is an item of its own (list_bots): it wakes when an item is assigned to it or its settings change, and every 15 minutes while its items have news; it does them all in one run, one run at a time.
- An item is a person's turn when it is assigned to them and nothing blocks it but approvals. Otherwise it is waiting, and they need not look.
- To move an item on: add events to it (add_events). Say something with a message; hand it on with assign; wait on something with a blocker; chain with an after (wait for one event, then add others). A script that never says it is done is a loop (e.g. a message for a bot every hour) until cancelled.
- To split work off, add a child item: it blocks its parent until it closes. To file unrelated work, add an item with detached: true.
- Changing anything outside this app (posting, merging, sending) is unsafe: ask for it with an approval event, which a person runs (unsafe_actions lists them).
- Directives (the directives action) are the operator's notes on what they are working on at the moment: read the latest for the broad picture.
- Finding work: items_waiting_on (a username, bot, a bot's name, or a blocker type), next_for_human, list_items, get_item, recently_assigned, stale_items. event_types describes every event.`
