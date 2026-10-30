// The paradigm, as short as it can be: what an agent connected over MCP is told
// (the server's instructions) and can read again (the `paradigm` action).

export const PARADIGM = `How this works:
- A task is a thing to be done (an item is any thing in the database; the tools say item). Everything in it is an item (a contains link from it), oldest first: its history, and its chat. To make something appear in a task's chat, add it as an item in the task (add_items): a note to say something, a script, a child task.
- Some items just happen (a note). Blockers wait: an approval. Tasks don't depend on each other: to wait on something, hand the task to a script.
- Items are assigned to people, bots and scripts (the assign action: an assigned_to link from the item). People (list_people) sign in by username. A bot is an item of its own (list_bots), with a chat of its own: it wakes when anything affects it (a task assigned to it, news in its tasks or chat, its settings), a debounce after, and every cycle (15 minutes) while it has tasks; it does them all in one run, one run at a time.
- An item is a person's turn when it is assigned to them and nothing blocks it but approvals. Otherwise it is waiting, and they need not look.
- To move an item on: add items to it (add_items). Say something with a note; hand it on with assign. To wait on something, or for anything repetitive that needs no LLM (a snooze, a check every 10 minutes until a branch merges), assign the task to a script: a bot without a model (it has a name and is given topics the same way; it wakes when given one and on its cycle, runs its JavaScript, keeps its state on the topics, and hands them on). Make one once and give it every topic of its kind. It costs nothing to run. run_script runs code once, now. See item_types for the script's exact shape.
- Keep traceability: link_items what a new task came from into it, and the new task into where it came from.
- To split work off, add a task in this one (it shows as a card here, and blocks nothing), or with detached: true to file it on its own.
- Changing anything outside this app (posting, merging, sending) is unsafe: only people do that.
- The search page (the Tasks view and every picker) shows sections that are configurable: search_sections reads them, set_search_sections changes them (built-in sections, queries over items, or JavaScript functions answering rows).
- Finding work: items_waiting_on (a username, bot, a bot's name, or a blocker type), next_for_human, list_items, get_item. item_types describes every item.`
