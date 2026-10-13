import { memo, useContext, useState } from 'react'
import { branchEntityId } from '../../../core/modules/github/view.ts'
import type { Entity } from '../../../core/types.ts'
import type { OverviewProps, PillProps, RowProps } from './kindTypes.ts'
import { Button, HeaderPill, Highlight, ItemContext, ItemPill } from './primitives.tsx'

// A path chosen for a topic: a directory, or a new worktree made from one.
// The newest under a topic is where its next dispatch runs.

const text = (data: Record<string, unknown>, key: string): string | null => (typeof data[key] === 'string' && data[key] ? (data[key] as string) : null)

function FolderMark() {
  return <span className="folder-mark" role="img" aria-label="Path" />
}

export function PathPill(props: PillProps) {
  return (
    <span className="item-name">
      <FolderMark /> <Highlight text={String(props.entity.data.text ?? '')} find={props.findText} />
    </span>
  )
}

/** A worktree's branch, as its item when its repo is on GitHub. */
function Branch(props: { entity: Entity }) {
  const branch = text(props.entity.data, 'branch')
  const github = text(props.entity.data, 'github')
  if (!branch) return null
  return github ? (
    <ItemPill id={branchEntityId(github, branch)} fallback={branch} />
  ) : (
    <span className="item-pill">
      <span className="item-name">{branch}</span>
    </span>
  )
}

/** What a path is besides its directory: a worktree (and of what), and its branch. */
function Facts(props: { entity: Entity }) {
  const origin = text(props.entity.data, 'origin')
  return (
    <>
      {text(props.entity.data, 'worktree') && <span className="muted">worktree{origin ? ` of ${origin}` : ''}</span>}
      <Branch entity={props.entity} />
    </>
  )
}

export const PathRow = memo(function PathRow(props: RowProps) {
  return (
    <span className="line path-line">
      <PathPill entity={props.entity} findText={props.findText} />
      <Facts entity={props.entity} />
    </span>
  )
})

export function PathOverview(props: OverviewProps) {
  return (
    <>
      <div className="title">
        <HeaderPill entity={props.entity} />
      </div>
      <div className="facts">
        <Facts entity={props.entity} />
      </div>
    </>
  )
}

/** The directories I have used (most recent first), and which are in a git repo, from the Claude root. */
export function useRemembered(): { cwds: string[]; repos: string[] } {
  const { item } = useContext(ItemContext)
  const root = item('claude')?.data ?? {}
  return { cwds: Array.isArray(root.cwds) ? (root.cwds as string[]) : [], repos: Array.isArray(root.repos) ? (root.repos as string[]) : [] }
}

/**
 * Each directory I have used: pressing it chooses it as it is, and, for one
 * in a git repo, **Worktree** chooses a new worktree of it. `chosen` marks
 * the choice where it is only being made (the new-topic form).
 */
export function PathOptions(props: { cwds: string[]; repos: string[]; chosen?: { cwd: string; worktree: boolean } | null; disabled?: string | false; onPick(cwd: string, worktree: boolean): void }) {
  if (!props.cwds.length) return null
  const is = (cwd: string, worktree: boolean) => props.chosen?.cwd === cwd && props.chosen.worktree === worktree
  return (
    <div className="path-options">
      {props.cwds.map((cwd) => (
        <div key={cwd} className="path-option">
          <Button label={cwd} active={is(cwd, false)} disabled={props.disabled} onClick={() => props.onPick(cwd, false)} />
          {props.repos.includes(cwd) && <Button label="Worktree" active={is(cwd, true)} disabled={props.disabled} onClick={() => props.onPick(cwd, true)} />}
        </div>
      ))}
    </div>
  )
}

/**
 * A topic's top right: a small **Path** button. While it is hovered, or after
 * it is pressed (until pressed again), a card under it: a box to type a
 * directory in (Enter adds it), and the directories I have used, each with a
 * worktree button. Whatever is picked goes into the topic as a path.
 */
export function PathPicker(props: { props: OverviewProps }) {
  const chat = props.props
  const { cwds, repos } = useRemembered()
  const [hovered, setHovered] = useState(false)
  const [pinned, setPinned] = useState(false)
  // While the box has the keyboard, the card stays.
  const [typing, setTyping] = useState(false)
  const busy = chat.working?.includes('path') ?? false
  const disabled = busy && 'Adding a path'
  return (
    <div className="topic-context" onMouseEnter={() => setHovered(true)} onMouseLeave={() => setHovered(false)}>
      <Button label="Path" busy={busy} busyLabel="Adding…" active={pinned} onClick={() => setPinned(!pinned)} />
      {(hovered || pinned || typing) && (
        <div className="topic-context-card">
          <input
            className="find"
            data-field="path"
            placeholder="Directory"
            spellCheck={false}
            value={chat.pathDraft ?? ''}
            onChange={(event) => chat.onPathDraft?.(event.target.value)}
            onFocus={() => setTyping(true)}
            onBlur={() => setTyping(false)}
          />
          <PathOptions cwds={cwds} repos={repos} disabled={disabled} onPick={(cwd, worktree) => chat.onAddPath?.(cwd, worktree)} />
        </div>
      )}
    </div>
  )
}
