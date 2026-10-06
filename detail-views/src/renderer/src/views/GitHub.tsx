import { memo } from 'react'
import type { Entity } from '../../../core/types.ts'
import { MessageRow, showsAuthor, showsTime } from './messages.tsx'
import { Link, Row, Status } from './primitives.tsx'
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
          <PrRow key={child.id} entity={child} selected={index === props.cursor} onSelect={props.onSelect} />
        ))}
      </div>
      <Status error={focus.error} />
    </div>
  )
}

const PrRow = memo(function PrRow(props: { entity: Entity; selected: boolean; onSelect(id: string): void }) {
  const { data } = props.entity
  const review = reviewWords[String(data.review)]
  return (
    <Row id={props.entity.id} selected={props.selected} className={data.draft ? 'draft' : ''} onSelect={props.onSelect}>
      <Dot outcome={data.checks} />
      <span className="grow">
        <span className="muted">
          {String(data.repo ?? '').split('/').pop()}#{String(data.number ?? '')}
        </span>{' '}
        {String(data.title ?? data.url)}
      </span>
      {review && <span className={`verdict ${String(data.review).toLowerCase()}`}>{review}</span>}
    </Row>
  )
})

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
      <div className="title">
        <span className="muted">
          {String(data.repo ?? '')}#{String(data.number ?? '')}
        </span>{' '}
        {data.url ? <Link href={String(data.url)} page>
            {String(data.title ?? data.url)}
          </Link> : null}
      </div>
      {data.state ? (
        <div className="facts">
          <span>{data.state === 'OPEN' ? (data.draft ? 'draft' : 'open') : String(data.state).toLowerCase()}</span>
          {review && <span className={`verdict ${String(data.review).toLowerCase()}`}>{review}</span>}
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
        </div>
      ) : null}
      <div className="list">
        {focus.children.map((child, index) =>
          child.type === 'github.check' ? (
            <CheckRow key={child.id} entity={child} selected={index === props.cursor} onSelect={props.onSelect} />
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
    </div>
  )
}

const CheckRow = memo(function CheckRow(props: { entity: Entity; selected: boolean; onSelect(id: string): void }) {
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
