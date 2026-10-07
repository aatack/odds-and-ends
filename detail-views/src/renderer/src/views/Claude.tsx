import { memo, useContext } from 'react'
import type { Entity } from '../../../core/types.ts'
import type { OverviewProps, PillProps, RowProps } from './kindTypes.ts'
import { Text } from './messages.tsx'
import { ClockContext, HeaderPill, Highlight } from './primitives.tsx'

const text = (data: Record<string, unknown>, fallback = ''): string =>
  typeof data.text === 'string' && data.text ? data.text : fallback

export function ClaudeHomePill(props: PillProps) {
  return (
    <span className="item-name">
      <Highlight text={text(props.entity.data, 'Claude')} find={props.findText} />
    </span>
  )
}

// --- Sessions ------------------------------------------------------------------------

export function SessionPill(props: PillProps) {
  return (
    <span className="item-name">
      <Highlight text={text(props.entity.data, 'Session')} find={props.findText} />
    </span>
  )
}

/** A session in a list: its name, then where it runs. */
export const SessionRow = memo(function SessionRow(props: RowProps) {
  const { data } = props.entity
  return (
    <span className="line">
      <Highlight text={text(data, 'Session')} find={props.findText} />{' '}
      <span className="muted">{String(data.branch ?? data.cwd ?? '')}</span>
    </span>
  )
})

/** A session heading its view: where it runs; its prompts (and their answers) are the tree. */
export function SessionOverview(props: OverviewProps) {
  const { data } = props.entity
  return (
    <>
      <div className="title">
        <HeaderPill entity={props.entity} />
      </div>
      <div className="facts muted">
        <span>{String(data.cwd ?? '')}</span>
        {data.branch ? <span>{String(data.branch)}</span> : null}
        {data.worktree ? <span>worktree</span> : null}
        <span>{String(data.permissionMode ?? '')} mode</span>
      </div>
    </>
  )
}

// --- Prompts and responses ---------------------------------------------------------------

export function PromptPill(props: PillProps) {
  return (
    <span className="item-name">
      <Text text={text(props.entity.data)} find={props.findText} inline />
    </span>
  )
}

/** What I asked. */
export const PromptRow = memo(function PromptRow(props: RowProps) {
  return (
    <span className="prompt">
      <Text text={text(props.entity.data)} find={props.findText} />
    </span>
  )
})

export function PromptOverview(props: OverviewProps) {
  return (
    <div className="prompt">
      <Text text={text(props.entity.data)} find={props.findText} />
    </div>
  )
}

/** `1m 05s`: how long a prompt has been running. */
function elapsed(ms: number): string {
  const seconds = Math.max(0, Math.floor(ms / 1000))
  const minutes = Math.floor(seconds / 60)
  return minutes ? `${minutes}m ${String(seconds % 60).padStart(2, '0')}s` : `${seconds}s`
}

/** Claude's answer: how long it has been working, the error in its place, or the answer in full. */
function Response(props: { entity: Entity; findText?: string }) {
  const { data } = props.entity
  const now = useContext(ClockContext)
  if (data.running) return <span className="muted">Claude is working… {elapsed(now - props.entity.createdAt)}</span>
  if (typeof data.error === 'string' && data.error) return <span className="error-text">{data.error}</span>
  return <Text text={text(data)} find={props.findText} />
}

export const ResponseRow = memo(function ResponseRow(props: RowProps) {
  return <Response entity={props.entity} findText={props.findText} />
})

export function ResponseOverview(props: OverviewProps) {
  return <Response entity={props.entity} findText={props.findText} />
}

export function ResponsePill(props: PillProps) {
  const { data } = props.entity
  return (
    <span className="item-name">
      {data.running ? 'Claude is working…' : <Text text={text(data).split('\n')[0]} find={props.findText} inline />}
    </span>
  )
}
