// What the chat input can send. Each turns the typed text into events; Tab steps
// through them in the input.

import type { EventSpec, PresetInfo } from '../shared/types'

export interface Preset extends PresetInfo {
  build: (text: string) => EventSpec[]
}

const said = (text: string): EventSpec[] => (text.trim() ? [{ type: 'message', params: { text: text.trim() } }] : [])

export const PRESETS: Preset[] = [
  // The default: what is typed is only said; Enter with nothing typed asks a bot to take it from there.
  {
    id: 'message',
    name: 'Message',
    hint: 'just a message; Enter on an empty line runs a bot',
    icon: 'message',
    whenEmpty: { name: 'Bot', hint: 'Enter runs a bot on everything new', icon: 'bot' },
    build: (text) => (text.trim() ? said(text) : [{ type: 'bot', params: {} }]),
  },
  {
    id: 'bot',
    name: 'Bot',
    hint: 'a message, then a bot takes it up',
    icon: 'bot',
    build: (text) => [...said(text), { type: 'bot', params: {} }],
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
