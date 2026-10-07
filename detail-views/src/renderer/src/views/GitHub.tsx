import { memo } from 'react'
import { authorColour } from '../format.ts'
import { hotkeyOf } from '../tools.ts'
import type { OverviewProps, PillProps, RowProps } from './kindTypes.ts'
import { MessageBody, MessageRow, Text } from './messages.tsx'
import { Button, HeaderPill, Highlight, Link } from './primitives.tsx'

/** A coloured dot for where CI is; the word rides in the tooltip. */
function Dot(props: { outcome: unknown }) {
  const outcome = typeof props.outcome === 'string' ? props.outcome : 'none'
  return <span className={`dot ${outcome}`} title={outcome} />
}

export function GitHubHomePill(props: PillProps) {
  return (
    <span className="item-name">
      <Highlight text={String(props.entity.data.text ?? 'GitHub')} find={props.findText} />
    </span>
  )
}

// --- Pull requests -----------------------------------------------------------------

interface Blocker {
  id: string
  label: string
  turn: 'me' | 'them' | 'none'
  tone: string
}

const blockersOf = (data: Record<string, unknown>): Blocker[] => (Array.isArray(data.blockers) ? (data.blockers as Blocker[]) : [])

/**
 * What's in a PR's way first, as a quiet chip in its colour, where the dot
 * used to be. The others wait their turn: once this is done, the next shows.
 */
function PrStatus(props: { data: Record<string, unknown> }) {
  const [first] = blockersOf(props.data)
  if (!first) return <span className="status-chip gray">…</span>
  return (
    <span
      className={`status-chip ${first.tone}${first.turn === 'me' ? ' mine' : ''}`}
      title={first.turn === 'me' ? 'Yours to do' : first.turn === 'them' ? 'Waiting on someone else' : undefined}
    >
      {first.label}
    </span>
  )
}

/** A PR in a list: what's in its way, its name, and where it lives. */
export const PrRow = memo(function PrRow(props: RowProps) {
  const { data } = props.entity
  return (
    <span className={`line${data.draft ? ' muted' : ''}`}>
      <PrStatus data={data} /> <Text text={String(data.text ?? data.url)} find={props.findText} inline />{' '}
      <span className="muted">{String(data.repo ?? '').split('/').pop()}</span>
    </span>
  )
})

/** A PR named in passing: what's in its way, and its name. */
export function PrPill(props: PillProps) {
  const { data } = props.entity
  return (
    <>
      <PrStatus data={data} />
      <span className="item-name">
        {data.name ? <Text text={String(data.name)} find={props.findText} inline /> : (props.fallback ?? String(data.url ?? props.entity.id))}
      </span>
    </>
  )
}

/**
 * A PR heading its view: what's in its way and its name (its pill), what can
 * be done, then the rest quietly. Its checks and discussion are the tree below.
 */
export function PrOverview(props: OverviewProps) {
  const { entity, view } = props
  const data = entity.data
  const counts = (data.counts ?? {}) as Record<string, number>
  const checkSummary = (['failing', 'pending', 'skipped', 'passing'] as const)
    .filter((kind) => counts[kind])
    .map((kind) => `${counts[kind]} ${kind}`)
    .join(' · ')
  return (
    <>
      <div className="title">
        {typeof data.preview === 'string' && (
          // First, so hovering it is the quickest way to try the branch.
          <Link href={data.preview}>Preview</Link>
        )}
        <HeaderPill entity={entity} />
      </div>
      {props.interactive && view.actions.length > 0 && (
        <div className="facts">
          {view.actions.map((action) => (
            <Button
              key={action.id}
              busy={props.working?.includes(action.id)}
              active={props.acting === action.id}
              disabled={action.disabled}
              label={action.label}
              hotkey={hotkeyOf(`action.${action.id}`)}
              onClick={() => props.onAction?.(action.id)}
            />
          ))}
        </div>
      )}
      {data.state ? (
        <div className="facts muted">
          <span>{String(data.repo ?? '')}</span>
          {data.url ? (
            <Link href={String(data.url)} page>
              on GitHub
            </Link>
          ) : null}
          <span>
            {String(data.head)} → {String(data.base)}
          </span>
          <span>
            +{String(data.additions)} −{String(data.deletions)} in {String(data.files)} {data.files === 1 ? 'file' : 'files'}
          </span>
          {checkSummary && (
            <span>
              <Dot outcome={data.checks} /> {checkSummary}
            </span>
          )}
          {data.autoMerge ? <span>auto-merge on</span> : null}
          {data.locallyApproved ? <span>approved by you here</span> : null}
        </div>
      ) : null}
    </>
  )
}

// --- Checks ------------------------------------------------------------------------

export function CheckPill(props: PillProps) {
  return (
    <>
      <Dot outcome={props.entity.data.outcome} />
      <span className="item-name">
        <Highlight text={String(props.entity.data.text ?? props.entity.data.name)} find={props.findText} />
      </span>
    </>
  )
}

export const CheckRow = memo(function CheckRow(props: RowProps) {
  const { data } = props.entity
  const name = String(data.text ?? data.name)
  const started = Number(data.startedAt)
  const ended = Number(data.completedAt) || Date.now() / 1000
  const minutes = started ? Math.max(1, Math.round((ended - started) / 60)) : null
  return (
    <span className="line-row">
      <span className="line">
        <Dot outcome={data.outcome} />{' '}
        {data.url ? (
          <Link href={String(data.url)}>
            <Highlight text={name} find={props.findText} />
          </Link>
        ) : (
          <Highlight text={name} find={props.findText} />
        )}
      </span>
      {minutes !== null && <span className="muted">{minutes}m</span>}
    </span>
  )
})

/** A check on its own: what it is and where its run is. */
export function CheckOverview(props: OverviewProps) {
  const data = props.entity.data
  return (
    <>
      <div className="title">
        <HeaderPill entity={props.entity} />
      </div>
      <div className="facts">
        <span>{String(data.outcome)}</span>
        {data.url ? (
          <Link href={String(data.url)} page>
            run
          </Link>
        ) : null}
      </div>
    </>
  )
}

// --- Descriptions, comments, reviews --------------------------------------------------

/** Grouped with the one above, like chat. */
export const GhItemRow = memo(function GhItemRow(props: RowProps) {
  return <MessageRow {...props} always={props.parent?.type !== 'github.pr'} />
})

export function GhItemPill(props: PillProps) {
  const { data } = props.entity
  return (
    <span className="item-name">
      <span style={{ color: authorColour(String(data.author)), fontWeight: 700 }}>{String(data.author ?? '')}</span>{' '}
      {String(data.verdict ?? data.kind ?? '')}
    </span>
  )
}

export function GhItemOverview(props: OverviewProps) {
  return <MessageBody entity={props.entity} author onOpen={props.onOpen} onImage={props.onImage} findText={props.findText} />
}

export function LocalApprovalPill() {
  return (
    <>
      <span className="badge tick green">✓</span>
      <span className="item-name">approved by me</span>
    </>
  )
}
