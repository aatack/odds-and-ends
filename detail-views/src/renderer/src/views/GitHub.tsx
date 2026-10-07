import { memo } from 'react'
import type { Badge as BadgeData } from '../../../core/types.ts'
import { authorColour } from '../format.ts'
import type { PillProps, RowProps } from './kindTypes.ts'
import { MessageBody, MessageRow, showsAuthor, showsTime } from './messages.tsx'
import { Badge, Button, Composer, HeaderPill, Link, Row, Status } from './primitives.tsx'
import type { FocusProps } from './types.ts'

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

/** My open pull requests, most recently active first. */
export function GitHubHome(props: FocusProps) {
  const { focus } = props
  return (
    <div className="pane">
      <div className="list">
        {focus.children.map((child, index) => (
          <PrRow
            key={child.id}
            entity={child}
            selected={index === props.cursor}
            onSelect={props.onSelect}
            onOpen={props.onOpen}
            onImage={props.onImage}
          />
        ))}
      </div>
      <Status error={focus.error} />
    </div>
  )
}

/** A PR in a list: its badge, where it lives, and its title. */
export const PrRow = memo(function PrRow(props: RowProps) {
  const { data } = props.entity
  return (
    <Row id={props.entity.id} selected={props.selected} className={data.draft ? 'draft' : ''} onSelect={props.onSelect}>
      <Badge badge={data.badge as BadgeData | null | undefined} />
      <span className="grow">
        <span className="muted">{String(data.repo ?? '').split('/').pop()}</span> {String(data.name ?? data.url)}
      </span>
    </Row>
  )
})

export function PrPill(props: PillProps) {
  const { data } = props.entity
  return (
    <>
      <Badge badge={data.badge as BadgeData | null | undefined} />
      <span className="item-name">
        {data.name ? String(data.name) : (props.fallback ?? String(data.url ?? props.entity.id))}
      </span>
    </>
  )
}

export function CheckPill(props: PillProps) {
  return (
    <>
      <Dot outcome={props.entity.data.outcome} />
      <span className="item-name">{String(props.entity.data.name)}</span>
    </>
  )
}

/** A check on its own: what it is and where its run is. */
export function CheckFull(props: FocusProps) {
  const data = props.focus.entity?.data ?? {}
  return (
    <div className="pane">
      {props.headed !== false && (
        <div className="title">
          <HeaderPill entity={props.focus.entity} />
        </div>
      )}
      <div className="facts">
        <span>{String(data.outcome)}</span>
        {data.url ? (
          <Link href={String(data.url)} page>
            run
          </Link>
        ) : null}
      </div>
    </div>
  )
}

/** A comment, review or description outside its PR: says who and when. */
export function GhItemRow(props: RowProps) {
  return <MessageRow {...props} author time />
}

export function GhItemPill(props: PillProps) {
  const { data } = props.entity
  return (
    <span className="item-name">
      <span style={{ color: authorColour(String(data.author)), fontWeight: 700 }}>{String(data.author ?? '')}</span>{' '}
      {String(data.verdict ?? data.kind ?? '')}
    </span>
  )
}

/** A comment, review or description on its own. */
export function GhItemFull(props: FocusProps) {
  const { entity } = props.focus
  if (!entity) return null
  return (
    <div className="pane">
      <div className="list">
        <MessageBody entity={entity} author time onOpen={props.onOpen} onImage={props.onImage} />
      </div>
    </div>
  )
}

export function LocalApprovalPill() {
  return (
    <>
      <span className="badge tick green">✓</span>
      <span className="item-name">approved by me</span>
    </>
  )
}

export function GitHubHomePill() {
  return <span className="item-name">GitHub</span>
}

function plural(count: number, word: string): string {
  return `${count} ${word}`
}

/** A pull request: where it stands, the checks that need me, then the discussion. */
export function GitHubPr(props: FocusProps) {
  const { focus } = props
  const data = focus.entity?.data ?? {}
  const counts = (data.counts ?? {}) as Record<string, number>
  const discussion = focus.children.filter((child) => child.type === 'github.item')
  const firstItem = focus.children.length - discussion.length
  const review = reviewWords[String(data.review)]
  const checkSummary = (['failing', 'pending', 'skipped', 'passing'] as const)
    .filter((kind) => counts[kind])
    .map((kind) => plural(counts[kind], kind))
    .join(' · ')
  return (
    <div className="pane">
      {props.headed !== false && (
        <div className="title">
          <HeaderPill entity={focus.entity} />
        </div>
      )}
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
            +{String(data.additions)} −{String(data.deletions)} in {String(data.files)}{' '}
            {data.files === 1 ? 'file' : 'files'}
          </span>
          {focus.actions.map((action) => (
            <Button
              key={action.id}
              busy={props.working?.includes(action.id)}
              active={props.acting === action.id}
              disabled={action.disabled}
              label={action.label}
              onClick={() => props.onAction?.(action.id)}
            />
          ))}
        </div>
      ) : null}
      <div className="list">
        {focus.children.map((child, index) =>
          child.type === 'github.check' ? (
            <CheckRow
              key={child.id}
              entity={child}
              selected={index === props.cursor}
              onSelect={props.onSelect}
              onOpen={props.onOpen}
              onImage={props.onImage}
            />
          ) : (
            <MessageRow
              key={child.id}
              entity={child}
              author={index === firstItem || showsAuthor(discussion, index - firstItem)}
              time={index === firstItem || showsTime(discussion, index - firstItem)}
              selected={index === props.cursor}
              onSelect={props.onSelect}
              onOpen={props.onOpen}
              onImage={props.onImage}
            />
          ),
        )}
        {focus.loading && <div className="loading">Loading…</div>}
      </div>
      <Status error={focus.error} />
      {props.acting && props.composing && (
        <Composer
          kind="action"
          placeholder={focus.actions.find((action) => action.id === props.acting)?.prompt}
          draft={props.draft}
          composing={props.composing}
          onDraft={props.onDraft}
          onCompose={props.onCompose}
        />
      )}
    </div>
  )
}

export const CheckRow = memo(function CheckRow(props: RowProps) {
  const { data } = props.entity
  const name = String(data.name)
  const started = Number(data.startedAt)
  const ended = Number(data.completedAt) || Date.now() / 1000
  const minutes = started ? Math.max(1, Math.round((ended - started) / 60)) : null
  return (
    <Row id={props.entity.id} selected={props.selected} onSelect={props.onSelect}>
      <Dot outcome={data.outcome} />
      <span className="grow">{data.url ? <Link href={String(data.url)}>{name}</Link> : name}</span>
      {minutes !== null && <span className="muted">{minutes}m</span>}
    </Row>
  )
})
