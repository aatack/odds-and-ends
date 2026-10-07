import { memo } from 'react'
import type { Badge as BadgeData } from '../../../core/types.ts'
import { authorColour } from '../format.ts'
import { hotkeyOf } from '../tools.ts'
import type { OverviewProps, PillProps, RowProps } from './kindTypes.ts'
import { MessageBody, MessageRow } from './messages.tsx'
import { Badge, Button, HeaderPill, Highlight, Link } from './primitives.tsx'

const reviewWords: Record<string, string> = {
  APPROVED: 'approved',
  CHANGES_REQUESTED: 'changes requested',
  REVIEW_REQUIRED: 'review required',
}

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

/** A PR in a list: its badge, where it lives, and its name. */
export const PrRow = memo(function PrRow(props: RowProps) {
  const { data } = props.entity
  return (
    <span className={`line${data.draft ? ' muted' : ''}`}>
      <Badge badge={data.badge as BadgeData | null | undefined} />{' '}
      <span className="muted">{String(data.repo ?? '').split('/').pop()}</span> <Highlight text={String(data.text ?? data.url)} find={props.findText} />
    </span>
  )
})

export function PrPill(props: PillProps) {
  const { data } = props.entity
  return (
    <>
      <Badge badge={data.badge as BadgeData | null | undefined} />
      <span className="item-name">
        {data.name ? <Highlight text={String(data.name)} find={props.findText} /> : (props.fallback ?? String(data.url ?? props.entity.id))}
      </span>
    </>
  )
}

/** A pull request heading its view: where it stands, and what can be done to it. Its checks and discussion are the tree. */
export function PrOverview(props: OverviewProps) {
  const { entity, view } = props
  const data = entity.data
  const counts = (data.counts ?? {}) as Record<string, number>
  const review = reviewWords[String(data.review)]
  const checkSummary = (['failing', 'pending', 'skipped', 'passing'] as const)
    .filter((kind) => counts[kind])
    .map((kind) => `${counts[kind]} ${kind}`)
    .join(' · ')
  return (
    <>
      <div className="title">
        <HeaderPill entity={entity} />
      </div>
      {data.state ? (
        <div className="facts">
          <span className="muted">{String(data.repo ?? '')}</span>
          {data.url ? (
            <Link href={String(data.url)} page>
              on GitHub
            </Link>
          ) : null}
          <span>{data.state === 'OPEN' ? (data.draft ? 'draft' : 'open') : String(data.state).toLowerCase()}</span>
          {review && <span className={`verdict ${String(data.review).toLowerCase()}`}>{review}</span>}
          {data.locallyApproved ? <span className="verdict approved">approved by me</span> : null}
          {data.autoMerge ? <span className="verdict approved">auto-merge on</span> : null}
          {data.mergeable === 'CONFLICTING' && <span className="verdict changes_requested">conflicts</span>}
          {checkSummary && (
            <span>
              <Dot outcome={data.checks} /> {checkSummary}
            </span>
          )}
          <span className="muted">
            {String(data.head)} → {String(data.base)}
          </span>
          <span className="muted">
            +{String(data.additions)} −{String(data.deletions)} in {String(data.files)} {data.files === 1 ? 'file' : 'files'}
          </span>
          {props.interactive &&
            view.actions.map((action) => (
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
