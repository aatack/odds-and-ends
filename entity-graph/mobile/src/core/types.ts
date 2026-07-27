// The shapes this app shares with the desktop one, re-exported from the model
// they both read: `../../../src/core`.
//
// These used to be copies, so that this app could build with nothing of the
// Electron project in its graph. That held while the only thing crossing the
// wire was a query result. It stopped holding when both clients started keeping
// their own event cache and running the traversal themselves — at which point
// the *rollup* has to agree exactly, or the two apps disagree about what the
// store says. A shared type is one thing; a shared fold over events is another,
// and once that is shared the type may as well come with it.
//
// What is imported is only the dependency-free part of the model — no Electron,
// no node, no zod. See `../../AGENTS.md`.

export type { AppEvent, LinkAction, LinkEvent, ValueEvent } from '../../../src/core/events'
export type { Entity, EntitySummary, LinkDirection } from '../../../src/core/entity'
export type { ResourceRecord } from '../../../src/core/source/permissions'

/**
 * One entry of the source's tool list (`GET /:sourceId/tools`). Still this app's
 * own: the server derives it from zod, and pulling zod into a phone bundle to
 * name four strings would be a poor trade.
 */
export interface ToolMeta {
  id: string
  name: string
  description: string
  safety: string
  args: Record<string, unknown>
}
