// What the chat input can send. Each turns the typed text into events; Tab steps
// through them in the input. Whoever is `[id]()`-referred to in the text is
// assigned the item as it is sent (Engine.send), and the input offers "assign to
// @someone" in place of these while it is empty.

import type { EventSpec, PresetInfo } from '../shared/types'

export interface Preset extends PresetInfo {
  build: (text: string) => EventSpec[]
}

export const PRESETS: Preset[] = [
  {
    id: 'note',
    name: 'Note',
    hint: 'a note in this task; @ someone to hand them the task',
    icon: 'note',
    build: (text) => (text.trim() ? [{ type: 'note', params: { text: text.trim() } }] : []),
  },
]
