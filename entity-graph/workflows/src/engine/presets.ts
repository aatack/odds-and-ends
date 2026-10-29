// What the chat input can send. Each turns the typed text into items; Tab steps
// through them in the input. Whoever is `[id]()`-referred to in the text is
// assigned the item as it is sent (Engine.send), and the input offers "assign to
// @someone" in place of these while it is empty.

import type { ItemSpec, PresetInfo } from '../shared/types'

export interface Preset extends PresetInfo {
  build: (text: string) => ItemSpec[]
}

/** A note. The URLs in it are pulled in by the engine as it is added (Engine.addNow). */
const note = (text: string): ItemSpec[] => (text.trim() ? [{ type: 'note', params: { text: text.trim() } }] : [])

export const PRESETS: Preset[] = [
  {
    id: 'note',
    name: 'Note',
    hint: 'a note in this task; @ someone to hand them the task',
    icon: 'note',
    build: note,
  },
]
