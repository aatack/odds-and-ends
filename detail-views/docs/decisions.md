# Decisions

Newest last. Each says what was chosen, why, and what was turned down.

1. **Event sourcing, from entity-graph.** Entities are rolled-up events.
   *Why:* local overrides of fetched data are just later events; one model for
   owned and fetched data. *Turned down:* the earlier row-per-entity store with
   `expires_at`.
2. **Two databases.** An append-only owned log and a disposable cache store.
   *Why:* fetched data can be thrown away weekly without risk to mine. The old
   single file is kept, read-only, for reverting.
3. **The cache store keeps one event per value and per link.** *Why:* a reload
   states what is true now; piling up history nothing needs makes rollups slow.
4. **Fetched events at the time they happened; undated ones at 0.** *Why:*
   ordering by `updatedAt` then means "newest real activity", loads never
   reorder, and my edits always win over undated data.
5. **Render only from the frontend cache.** Loading is something the cache
   does on reading; the core never pushes data to a view. *Why:* the UI never
   waits; one copy of each entity.
6. **Loads are per part (`self` / `children`) and marked on the entity.**
   *Why:* a row needs less than an open item; flags in the cache store clear
   with the data.
7. **Revisit everything the cache has read, not only what is on screen.**
   *Why:* anything looked at stays current. One freshness per part for now.
8. **Pure presentation shared with the renderer.** `ModuleView` replaced
   core-side `present`/`order`. *Why:* the renderer computes what it shows from
   its own cache; headless callers run the same code.
9. **Bounded walks.** Focus views walk at most `focusLimit` children.
   *Why:* never render unbounded data. Currently 3000 because Slack's workspace
   is sorted after the walk.
10. **Slack: no unread counts.** *Why:* tracking them needed a call per
    conversation and was wrong as soon as I read elsewhere.
11. **Slack: order by the rollup's newest event, in the frontend.** Not by a
    stored `latestTs`, not by load time.
12. **Slack: lists + one search batch + a search poll; no per-conversation
    loading.** *Why:* ~600 calls took minutes; search covers every
    conversation in ~10 calls. *Turned down:* `client.counts`/`client.boot`
    (private, may break or breach terms); Socket Mode (needs an app-level token
    and app settings; still an option for instant updates and edits); polling
    each channel.
13. **Slack: history further back only on demand**, globally (next 1000 by
    search) or per conversation (100 by history) or per thread (whole).
    Cursors are timestamps, not Slack's expiring `next_cursor`.
14. **Slack: a conversation's history starts at the older of its own cursor
    and the global one**, since everything after the global cursor is cached.
15. **Slack: threads are listed in the workspace**, ordered by newest reply;
    replies don't bump their conversation.
16. **Slack: only DMs, group DMs and private channels join the list from
    search**; public ones come with the list reload (search sees channels I'm
    not in).
17. **Poll status on its own entity** (`slack:watch`), so a 15 s poll re-reads
    one tiny entity rather than the workspace.
18. **Clocks and in-flight state live in the session**, not in view timers.
19. **GitHub: reload PRs on freshness.** A few minutes is fine; no watch yet.
20. **Hide a chat by unlinking it from the workspace** (owned event), not by a
    mute flag. *Why:* it is the event model's own way to say "not under this
    for me"; it outlasts every reload. *Turned down:* `interest` values with
    auto-rules (more machinery; would also have bumped the chat's sort time).
21. **A thread is listed iff its conversation is listed.** *Why:* hiding a
    conversation should hide its threads, and threads from channels I'm not in
    should never appear. Hence lists load before polling starts.
