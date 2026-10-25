import { atomWithStorage, createJSONStorage } from 'jotai/utils'
import type { Overview } from '../../shared/types'

/** Someone a task can be assigned to: a person (by username) or a bot (by id). */
export interface Someone {
  id: string
  name: string
  bot: boolean
}

/** Every person, then every bot. */
export const everyone = (o: Pick<Overview, 'users' | 'bots'>): Someone[] => [
  ...o.users.map((u) => ({ id: u.id, name: u.name, bot: false })),
  ...o.bots.map((b) => ({ id: b.id, name: b.name, bot: true })),
]

/** Someone, for reading: "you" for whoever is signed in, a bot as @name, a person by name; a bot's run (`bot:<id>`) as its bot. */
export function nameOf(o: Pick<Overview, 'users' | 'bots' | 'me'>, id: string): string {
  const plain = id.startsWith('bot:') ? id.slice(4) : id
  if (plain === o.me.id) return 'you'
  const bot = o.bots.find((b) => b.id === plain)
  if (bot) return `@${bot.name}`
  return o.users.find((u) => u.id === plain)?.name ?? plain
}

/** Whether someone is a person (not a bot). */
export const isPerson = (o: Pick<Overview, 'users'>, id: string): boolean => o.users.some((u) => u.id === id)

/** Who is signed in to this window: their username, kept in local storage. */
export const userAtom = atomWithStorage<string | null>('workflows.user', null, createJSONStorage(() => localStorage), { getOnInit: true })
