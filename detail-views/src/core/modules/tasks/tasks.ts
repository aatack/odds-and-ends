import type { Module } from '../module.ts'
import { tasksView } from './view.ts'

/** The author of everything I write: the owned store's own events. */
export const me = 'me'

/** Owned data only. Notes are made by the core's generic `create`, like anywhere else. */
export function tasksModule(): Module {
  return { view: tasksView }
}
