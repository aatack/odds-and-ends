// What the chat input can send. Each turns the typed text into events; Tab steps
// through them in the input. Whoever is `[id]()`-referred to in the text is
// assigned the item as it is sent (Engine.send), and the input offers "assign to
// @someone" in place of these while it is empty.

import type { ItemSpec, PresetInfo } from '../shared/types'
import { SLACK_LINK } from '../shared/slack'

export interface Preset extends PresetInfo {
  build: (text: string) => ItemSpec[]
}

/** A note, with every Slack message link in it pulled in as a slack_message after it (a link alone is just the message). */
const note = (text: string): ItemSpec[] => {
  const links = [...text.matchAll(new RegExp(SLACK_LINK, 'g'))].map((m) => m[0])
  const rest = links.reduce((t, l) => t.replace(l, ''), text).trim()
  const said: ItemSpec[] = rest ? [{ type: 'note', params: { text: links.length ? text.trim() : rest } }] : []
  return [...said, ...links.map((link): ItemSpec => ({ type: 'slack_message', params: { link } }))]
}

export const PRESETS: Preset[] = [
  {
    id: 'note',
    name: 'Note',
    hint: 'a note in this task; @ someone to hand them the task',
    icon: 'note',
    build: note,
  },
]
