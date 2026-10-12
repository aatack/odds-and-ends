import { memo, useContext, useEffect, useLayoutEffect, useMemo, useRef } from 'react'
import type { Entity } from '../../../core/types.ts'
import { shortTime } from '../format.ts'
import { inertRows, type ShownRow } from '../state.ts'
import { hotkeyOf } from '../tools.ts'
import type { OverviewProps, PillProps, RowProps, Tint } from './kindTypes.ts'
import { Text } from './messages.tsx'
import { noteClass } from './Notes.tsx'
import { Button, ClockContext, Composer, HeaderPill, Highlight, ItemContext, KindsContext, Status } from './primitives.tsx'
import { TextBox, ViewPane } from './View.tsx'

/** The Claude mark (src/renderer/src/assets/claude.svg), in Claude's colour. */
export function ClaudeMark(props: { large?: boolean }) {
  return <span className={`claude-mark${props.large ? ' large' : ''}`} role="img" aria-label="Claude" />
}

const text = (data: Record<string, unknown>, fallback = ''): string =>
  typeof data.text === 'string' && data.text ? data.text : fallback

export function ClaudeHomePill(props: PillProps) {
  return (
    <span className="item-name">
      <ClaudeMark />
      <Highlight text={text(props.entity.data, 'Claude')} find={props.findText} />
    </span>
  )
}

// --- Sessions ------------------------------------------------------------------------

/** A session's name, then how long it has been answering, while it is. */
export function SessionPill(props: PillProps) {
  const { data } = props.entity
  const now = useContext(ClockContext)
  return (
    <span className="item-name">
      <ClaudeMark /> <Highlight text={text(data, 'Session')} find={props.findText} />
      {typeof data.runningSince === 'number' && <span className="muted"> {elapsed(now - data.runningSince)}</span>}
    </span>
  )
}

/** A session in a list: its name, then where it runs. */
export const SessionRow = memo(function SessionRow(props: RowProps) {
  const { data } = props.entity
  return (
    <span className="line session">
      <ClaudeMark /> <Highlight text={text(data, 'Session')} find={props.findText} />{' '}
      <span className="muted">{String(data.branch ?? data.cwd ?? '')}</span>
    </span>
  )
})

/** A session heading its view: where it runs; its chat (each prompt, then its answer) is the tree. */
export function SessionOverview(props: OverviewProps) {
  const { data } = props.entity
  return (
    <>
      <div className="title session">
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

/** What I asked (tinted `accent` by its type: my voice). */
export const PromptRow = memo(function PromptRow(props: RowProps) {
  return <Text text={text(props.entity.data)} find={props.findText} />
})

export function PromptOverview(props: OverviewProps) {
  return <Text text={text(props.entity.data)} find={props.findText} />
}

/** A prompt's tint: mine, a faint blue, as in a chat. A response has none, unless it failed. */
export const promptTint = (): Tint => 'accent'
export const responseTint = (entity: Entity): Tint | null => (typeof entity.data.error === 'string' && entity.data.error ? 'error' : null)

/** `1m 05s`: how long a prompt has been running. */
function elapsed(ms: number): string {
  const seconds = Math.max(0, Math.floor(ms / 1000))
  const minutes = Math.floor(seconds / 60)
  return minutes ? `${minutes}m ${String(seconds % 60).padStart(2, '0')}s` : `${seconds}s`
}

/** Claude's answer (tinted by its type): how long it has been working, the error in its place, or the answer in full. */
function Response(props: { entity: Entity; findText?: string }) {
  const { data } = props.entity
  const now = useContext(ClockContext)
  const failed = typeof data.error === 'string' && data.error
  return (
    <div className="response">
      <ClaudeMark />
      <div className={`response-body ${noteClass(data)}`}>
        {data.running ? (
          <span className="muted">Working… {elapsed(now - props.entity.createdAt)}</span>
        ) : failed ? (
          <span className="error-text">{failed}</span>
        ) : (
          <Text text={text(data)} find={props.findText} />
        )}
      </div>
    </div>
  )
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
      <ClaudeMark /> {data.running ? 'Working…' : <Text text={text(data).split('\n')[0]} find={props.findText} inline />}
    </span>
  )
}

// --- The chat pages ----------------------------------------------------------------
//
// An experiment, after chat apps: the module is a list of chats, and a chat is
// bubbles, mine on the right, with what is in its context beside it. Only in
// the main view; a peek or the side panel shows the usual tree.

type EntityRow = Extract<ShownRow, { kind: 'entity' }>

const childRows = (props: OverviewProps): EntityRow[] =>
  props.shown.rows.filter((row): row is EntityRow => row.kind === 'entity' && row.row.depth === 1)

/** The Claude module: every chat, the most recently active first. A click opens one. */
export function ChatListDetail(props: OverviewProps) {
  const chats = childRows(props)
  return (
    <div className="pane chat-list">
      <div className="title">
        <HeaderPill entity={props.entity} />
        {props.onNewChat && <Button label="New chat" hotkey={hotkeyOf('claude.new')} onClick={props.onNewChat} />}
      </div>
      <div className="chat-cards">
        {chats.map((shown) => (
          <ChatCard key={shown.key} shown={shown} onOpen={props.onOpen} />
        ))}
      </div>
    </div>
  )
}

/** A chat in the list: its name and when it was last active, then its newest message (or how long Claude has been on it). */
const ChatCard = memo(function ChatCard(props: { shown: EntityRow; onOpen(id: string): void }) {
  const { entity } = props.shown.row
  const { data } = entity
  const now = useContext(ClockContext)
  const ref = useRef<HTMLDivElement>(null)
  useEffect(() => {
    if (props.shown.selected) ref.current?.scrollIntoView({ block: 'nearest' })
  }, [props.shown.selected])
  const running = typeof data.runningSince === 'number'
  const last = typeof data.lastText === 'string' ? data.lastText.split('\n').find((line) => line.trim()) ?? '' : ''
  return (
    <div ref={ref} className={`chat-card${props.shown.selected ? ' selected' : ''}`} onClick={() => props.onOpen(entity.id)}>
      <div className="chat-card-top">
        <ClaudeMark /> <span className="chat-card-name">{text(data, 'Session')}</span>
        <span className="chat-card-time muted">{running ? `${elapsed(now - (data.runningSince as number))}` : shortTime(String(entity.updatedAt / 1000))}</span>
      </div>
      <div className="chat-card-last muted">
        {running ? 'Working…' : last ? <>{data.lastMine ? 'You: ' : ''}<Text text={last} inline /></> : String(data.cwd ?? '')}
      </div>
    </div>
  )
})

/**
 * A chat: its prompts and answers as bubbles (mine on the right), the prompt
 * box below with the keyboard; to the right, the chat's own overview and the
 * overview of each item in its context, or one of them opened there. Tab in
 * the box goes on to the next chat waiting for me.
 */
export function ChatDetail(props: OverviewProps) {
  const { entity, view } = props
  const messages = childRows(props)
  // Opens at the bottom, and stays there as messages come, while it is there.
  const ref = useRef<HTMLDivElement>(null)
  const inner = useRef<HTMLDivElement>(null)
  const atEnd = useRef(true)
  useLayoutEffect(() => {
    const el = ref.current
    if (el && atEnd.current) el.scrollTop = el.scrollHeight
  })
  useEffect(() => {
    const el = ref.current
    const content = inner.current
    if (!el || !content) return
    const observer = new ResizeObserver(() => {
      if (atEnd.current) el.scrollTop = el.scrollHeight
    })
    observer.observe(content)
    return () => observer.disconnect()
  }, [])
  const input = props.shown.rows.find((row) => row.kind === 'input')
  return (
    <div className="pane chat-page">
      <div className="chat-main">
        <div
          ref={ref}
          className="bubbles"
          onScroll={(event) => {
            const el = event.currentTarget
            atEnd.current = el.scrollTop + el.clientHeight >= el.scrollHeight - 4
            if (el.scrollTop < 200) props.onNearEnd?.()
          }}
        >
          <div ref={inner} className="bubbles-inner">
            {messages.map((shown) => (
              <Bubble key={shown.key} shown={shown} props={props} />
            ))}
            {input && (
              <div className="bubble-line">
                <div className="bubble note">
                  <TextBox edit={props.edit} onEditDraft={props.onEditDraft} />
                </div>
              </div>
            )}
          </div>
        </div>
        <Status error={view.error} />
        <Composer
          key={entity.id}
          kind="claude"
          field="prompt"
          autoFocus
          placeholder="Prompt"
          draft={props.draft}
          composing={props.composing}
          onDraft={props.onDraft}
          onCompose={props.onCompose}
        />
      </div>
      <aside className="chat-context">
        <ChatContext props={props} />
      </aside>
    </div>
  )
}

/** A message as a bubble: a prompt on the right, an answer on the left. */
function Bubble(props: { shown: EntityRow; props: OverviewProps }) {
  const { shown } = props
  const { entity } = shown.row
  const mine = entity.type === 'claude.prompt'
  const failed = typeof entity.data.error === 'string' && entity.data.error
  const kind = mine ? 'mine' : entity.type === 'claude.response' ? (failed ? 'theirs failed' : 'theirs') : 'note'
  return (
    <div className={`bubble-line ${mine ? 'right' : 'left'}`}>
      <div className={`bubble ${kind}`}>
        {shown.editing ? (
          <TextBox edit={props.props.edit} onEditDraft={props.props.onEditDraft} />
        ) : entity.type === 'claude.response' ? (
          <Answer entity={entity} />
        ) : (
          <Text text={text(entity.data)} />
        )}
      </div>
    </div>
  )
}

/** Claude's answer in a bubble: how long it has been working, the error in its place, or the answer in full. */
function Answer(props: { entity: Entity }) {
  const { data } = props.entity
  const now = useContext(ClockContext)
  const failed = typeof data.error === 'string' && data.error
  if (data.running) return <span className="muted">Working… {elapsed(now - props.entity.createdAt)}</span>
  if (failed) return <span className="error-text">{failed}</span>
  return <Text text={text(data)} />
}

/**
 * The chat's right pane: its own overview, then each item in its context as
 * an overview, framed apart. A click on one opens it here, as its whole view,
 * with an arrow back.
 */
function ChatContext(props: { props: OverviewProps }) {
  const chat = props.props
  const opened = chat.contextView ?? null
  const shown = useMemo(() => (opened ? inertRows(opened) : null), [opened])
  if (opened && shown) {
    const back = () => chat.onContextOpen?.(null)
    return (
      <div className="context-open">
        <div className="context-open-bar">
          <Button quiet label="←" title="Back to the context" onClick={back} />
          <HeaderPill entity={opened.root} />
        </div>
        <ViewPane
          {...chat}
          view={opened}
          shown={shown}
          interactive={false}
          focused={false}
          prompt={undefined}
          acting={null}
          edit={null}
          picking={null}
          find={null}
          working={undefined}
          onSelect={() => {}}
          onAction={undefined}
          onClose={back}
          onNearEnd={undefined}
        />
      </div>
    )
  }
  const peers = Array.isArray(chat.entity.data.contextPeers) ? (chat.entity.data.contextPeers as string[]) : []
  return (
    <>
      <div className="context-card chat-own">
        <SessionOverview {...chat} />
      </div>
      {peers.map((id) => (
        <ContextCard key={id} id={id} props={chat} />
      ))}
    </>
  )
}

/** An item in the chat's context, as its overview, framed apart from the others. A click opens it in the pane. */
function ContextCard(props: { id: string; props: OverviewProps }) {
  const kinds = useContext(KindsContext)!
  const { item } = useContext(ItemContext)
  const entity = item(props.id)
  if (!entity) return <div className="context-card muted">…</div>
  const Overview = kinds[entity.type].Overview
  // Its overview alone: no tree, no actions, nothing of the chat's own.
  const view = { ...props.props.view, root: entity, rows: [], actions: [], older: false, compose: null, error: null, loading: false }
  return (
    <div className="context-card" onClick={() => props.props.onContextOpen?.(entity.id)}>
      <Overview
        {...props.props}
        view={view}
        shown={{ rows: [], keys: [], selectedPath: [], selectedIndex: -1 }}
        interactive={false}
        onAction={undefined}
        onOlder={undefined}
        onPause={undefined}
        entity={entity}
        findText={undefined}
      />
    </div>
  )
}
