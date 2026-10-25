// The paradigm, as short as it can be: what an agent connected over MCP is told
// (the server's instructions) and can read again (the `paradigm` action).

export const PARADIGM = `How this works:
- An item is a thing to be done. Everything in it is an event, oldest first: its history.
- Some events just happen (a message). Blockers wait: CI to finish, an approval, a child item to close, a script to say it is done.
- Items are assigned (assign events) to the human or to bots. A bot is an item of its own (list_bots): it wakes whenever anything happens in an item assigned to it, or that it watches, and does them one run at a time.
- An item is the human's turn when it is assigned to them (or to nobody) and nothing blocks it but approvals. Otherwise it is waiting, and the human need not look.
- To move an item on: add events to it (add_events). Say something with a message; hand it on with an assign; wait on something with a blocker; chain with an after (wait for one event, then add others). A script that never says it is done is a loop (e.g. a message for a bot every hour) until cancelled.
- To split work off, add a child item: it blocks its parent until it closes. To file unrelated work, add an item with detached: true.
- Changing anything outside this app (posting, merging, sending) is unsafe: ask for it with an approval event, which the human runs (unsafe_actions lists them).
- Directives (the directives action) are the operator's notes on what they are working on at the moment: read the latest for the broad picture.
- Finding work: items_waiting_on (human, bot, a bot's name, or a blocker type), next_for_human, list_items, get_item, recently_assigned, stale_items. event_types describes every event.`
