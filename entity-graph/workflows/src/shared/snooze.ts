// The Snooze script's source: given a topic ("[id]() 2d", or assigned it), it keeps the topic until then, and hands it
// back to whoever snoozed it. Its state is on each topic (sleepUntil, snoozedBy). The scripts' docs show it as their example.
export const SNOOZE_SOURCE = `const out = []
const now = Date.parse(context.now)
for (const t of context.topics) {
  const v = (await tools.get_item({ item_id: t.id })).values
  if (!v.sleepUntil) {
    const m = (await mentions(t.id))[0]
    const n = parseFloat(m?.text) || 1
    const unit = { m: 6e4, h: 36e5, d: 864e5 }[(m?.text.match(/[mhd]/) || ['h'])[0]]
    await tools.set_value({ item_id: t.id, key: 'sleepUntil', value: now + n * unit })
    await tools.set_value({ item_id: t.id, key: 'snoozedBy', value: m?.by ?? null })
  } else if (v.sleepUntil <= now) {
    await tools.set_value({ item_id: t.id, key: 'sleepUntil', value: null })
    out.push({ item: t.id, to: [v.snoozedBy || 'aatack'] })
  }
}
return { assign: out }`
