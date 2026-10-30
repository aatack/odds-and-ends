import { atomWithStorage, createJSONStorage } from 'jotai/utils'
import type { Overview } from '../../shared/types'

/** Someone a task can be assigned to: a person (by username) or a bot (by id). */
export interface Someone {
  id: string
  name: string
  bot: boolean
}

/** Every person, then every bot. */
export const everyone = (o: Pick<Overview, 'users' | 'bots'> & Partial<Pick<Overview, 'scripts'>>): Someone[] => [
  ...o.users.map((u) => ({ id: u.id, name: u.name, bot: false })),
  ...o.bots.map((b) => ({ id: b.id, name: b.name, bot: true })),
  // Scripts with a name are given topics as bots are.
  ...(o.scripts ?? []).filter((s) => s.named).map((s) => ({ id: s.id, name: s.name, bot: true })),
]

/** Someone, for reading: "you" for whoever is signed in, a bot as @name, a person by name; a bot's run (`bot:<id>`) as its bot. */
export function nameOf(o: Pick<Overview, 'users' | 'bots' | 'me'> & Partial<Pick<Overview, 'scripts'>>, id: string): string {
  const plain = id.startsWith('bot:') ? id.slice(4) : id
  if (plain === o.me.id) return 'you'
  const bot = o.bots.find((b) => b.id === plain)
  if (bot) return `@${bot.name}`
  const script = o.scripts?.find((s) => s.id === plain)
  if (script) return script.name
  return o.users.find((u) => u.id === plain)?.name ?? plain
}

/** Whether someone is a person (not a bot). */
export const isPerson = (o: Pick<Overview, 'users'>, id: string): boolean => o.users.some((u) => u.id === id)

const kept = createJSONStorage<string | null>(() => localStorage)

/** Who is signed in to this window: their username, kept in local storage. */
export const userAtom = atomWithStorage<string | null>('workflows.user', null, kept, { getOnInit: true })

/** Which source is open in this window: its path as typed, kept in local storage. */
export const sourceAtom = atomWithStorage<string | null>('workflows.source', null, kept, { getOnInit: true })
