// What the chat input can send. Each turns the typed text into events; Tab steps
// through them in the input. Bots `[id]()`-referred to in the text are assigned
// the item as it is sent (Engine.send), and the input offers "assign to @bot"
// in place of these while it is empty.

import type { EventSpec, PresetInfo } from '../shared/types'

export interface Preset extends PresetInfo {
  build: (text: string) => EventSpec[]
}

const said = (text: string): EventSpec[] => (text.trim() ? [{ type: 'message', params: { text: text.trim() } }] : [])

export const PRESETS: Preset[] = [
  // The default: a note in the item. @ a bot in it to hand the item to the bot.
  {
    id: 'message',
    name: 'Note',
    hint: 'a note in this task; @ a bot to hand it the task',
    icon: 'message',
    build: said,
  },
  {
    id: 'pr-merged',
    name: 'PR merged',
    hint: "a pull request's title: wait until it is merged",
    icon: 'github',
    build: (text) => [{ type: 'github', params: { query: text.trim(), until: 'merged' } }],
  },
  {
    id: 'pr-checks',
    name: 'PR checks',
    hint: "a pull request's title: wait for its checks to finish",
    icon: 'github',
    build: (text) => [{ type: 'github', params: { query: text.trim(), until: 'checks' } }],
  },
]
