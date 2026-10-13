import { memo, useContext, useEffect, useLayoutEffect, useRef, type ReactNode } from 'react'
import { createPortal } from 'react-dom'
import { components, type InputProps } from 'react-select'
import CreatableSelect from 'react-select/creatable'
import type { Entity } from '../../../core/types.ts'
import { shortTime } from '../format.ts'
import { contextWindow, type ChatMode, type ShownRow } from '../state.ts'
import { hotkeyOf } from '../tools.ts'
import type { OverviewProps, PillProps, RowProps, Tint } from './kindTypes.ts'
import { Text } from './messages.tsx'
import { noteClass } from './Notes.tsx'
import { Button, ClockContext, Composer, HeaderPill, Highlight, ItemContext, KindsContext, SideSlotContext, Status, useEdgeResize } from './primitives.tsx'
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
        {props.interactive && props.onSetDirectory ? <DirectorySelect {...props} /> : <span>{String(data.cwd ?? '')}</span>}
        {data.branch ? <span>{String(data.branch)}</span> : null}
        {data.worktree ? <span>worktree{data.origin ? ` of ${String(data.origin)}` : ''}</span> : null}
        <span>{String(data.permissionMode ?? '')} mode</span>
      </div>
    </>
  )
}

interface DirectoryOption {
  cwd: string
  worktree: boolean
  /** The option for a path typed in, not yet one of the remembered ones. */
  __isNew__?: boolean
}

/** The data-field of the dropdown's text box, so keys meant for other boxes leave it alone. */
function DirectoryInput(props: InputProps<DirectoryOption, false>) {
  return <components.Input {...props} {...{ 'data-field': 'directory' }} />
}

/**
 * Where a session runs, as a dropdown: each directory I have used (a new
 * one typed in), and, for one in a git repo, a new worktree of it too.
 * Picking one moves the session there, its conversation with it. Not while
 * Claude is answering.
 */
function DirectorySelect(props: OverviewProps) {
  const { data } = props.entity
  const { item } = useContext(ItemContext)
  const root = item('claude')?.data ?? {}
  const cwds = Array.isArray(root.cwds) ? (root.cwds as string[]) : []
  const repos = new Set(Array.isArray(root.repos) ? (root.repos as string[]) : [])
  const options: DirectoryOption[] = cwds.flatMap((cwd) => [{ cwd, worktree: false }, ...(repos.has(cwd) ? [{ cwd, worktree: true }] : [])])
  const current: DirectoryOption = { cwd: String(data.cwd ?? ''), worktree: false }
  const running = typeof data.runningSince === 'number'
  const busy = props.working?.includes('directory') ?? false
  const choose = (option: DirectoryOption | null) => {
    if (option && !(option.cwd === data.cwd && !option.worktree)) props.onSetDirectory?.(option.cwd, option.worktree)
  }
  return (
    <span className="dir-select-wrap" title={running ? 'Claude is answering: wait for it to finish' : undefined}>
      <CreatableSelect<DirectoryOption, false>
        unstyled
        classNamePrefix="dir-select"
        className="dir-select"
        value={current}
        options={options}
        isDisabled={running || busy}
        isLoading={busy}
        placeholder="Directory"
        menuPortalTarget={document.body}
        components={{ Input: DirectoryInput }}
        getOptionValue={(option) => `${option.cwd}#${option.worktree}`}
        getOptionLabel={(option) => option.cwd}
        formatOptionLabel={(option, meta) =>
          meta.context === 'value' ? (
            option.cwd
          ) : option.__isNew__ ? (
            <>Use {option.cwd}</>
          ) : option.worktree ? (
            <span className="dir-option">
              <span className="dir-option-tag">new worktree</span> {option.cwd}
            </span>
          ) : (
            option.cwd
          )
        }
        getNewOptionData={(input) => ({ cwd: input, worktree: false, __isNew__: true })}
        isValidNewOption={(input) => Boolean(input.trim()) && !cwds.includes(input.trim())}
        onChange={choose}
        onCreateOption={(input) => choose({ cwd: input.trim(), worktree: false })}
      />
    </span>
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
export function elapsed(ms: number): string {
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
  return (
    <ChatPage
      props={props}
      field="prompt"
      placeholder={modeNames[props.chatMode ?? 'prompt']}
      bubble={(row) => (row.entity.type === 'claude.prompt' ? 'mine' : row.entity.type === 'claude.response' ? 'theirs' : null)}
      side={<ChatPanel props={props} />}
    />
  )
}

/** Which rows of a chat are bubbles: mine on the right, theirs on the left; the rest are lines across it. */
export type BubbleOf = (row: EntityRow['row']) => 'mine' | 'theirs' | null

/**
 * A chat page: every row under its root as bubbles and lines, selectable as
 * in a tree, newest at the bottom, and its box below with the keyboard
 * (`field` names it for the keys); `side` beside it, if anything. A `flat`
 * one shows only the rows directly under its root, and none is selectable.
 * A box `onDemand` takes the keyboard only when asked (Enter), not when
 * the page opens. `corner` sits at its top right. A flat chat's rows sit closer
 * together while the same voice goes on (`joined`): mine, then Claude's.
 */
export function ChatPage(page: {
  props: OverviewProps
  field: string
  placeholder: string
  bubble: BubbleOf
  side?: ReactNode
  flat?: boolean
  onDemand?: boolean
  corner?: ReactNode
  /** Just above the box (a topic's waits). */
  above?: ReactNode
  /** The box sends somewhere outside the app: tinted as a warning, with where on its right. */
  outward?: ReactNode
  /** Beside a row, on its right, shown while it is hovered (a Slack message's waits). */
  aside?: (row: EntityRow['row']) => ReactNode
}) {
  const { props } = page
  const { entity, view } = props
  // Everything under the chat, as the tree walks it (notes under notes too), the root apart; flat, only what is directly under it.
  const rows = props.shown.rows.filter((row) => (page.flat ? row.kind === 'entity' && row.row.depth === 1 : row.kind === 'input' || row.row.depth > 0))
  // Typing in the box, the chat marks no row; picking rows, the box is dimmed.
  const prompting = props.composing && !props.acting
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
  return (
    <div className={`pane chat-page${prompting ? ' prompting' : ''}${page.flat ? ' flat' : ''}`}>
      <div className="chat-main">
        {page.corner && <div className="chat-corner">{page.corner}</div>}
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
            {rows.map((shown, at) =>
              shown.kind === 'input' ? (
                <div key={shown.key} className="chat-row selected" style={chatIndent(shown.depth)}>
                  <div className="note-line">
                    <NoteMark />
                    <div className="note-line-text">
                      <TextBox edit={props.edit} onEditDraft={props.onEditDraft} />
                    </div>
                  </div>
                </div>
              ) : (
                <ChatRow
                  key={shown.key}
                  shown={shown}
                  props={props}
                  prompting={prompting}
                  bubble={page.bubble}
                  flat={page.flat}
                  joined={page.flat === true && at > 0 && voiceOf(rows[at - 1], page.bubble) === voiceOf(shown, page.bubble)}
                  aside={page.aside?.(shown.row)}
                />
              ),
            )}
          </div>
        </div>
        <Status error={view.error} />
        {page.above}
        <div className={`chat-box${page.outward ? ' outward' : ''}`}>
          <Composer
            key={entity.id}
            kind={view.compose ?? 'claude'}
            field={page.field}
            autoFocus={!page.onDemand}
            placeholder={page.placeholder}
            draft={props.draft}
            composing={props.composing}
            onDraft={props.onDraft}
            onCompose={props.onCompose}
          />
          {page.outward && <div className="chat-box-where">{page.outward}</div>}
        </div>
      </div>
      {page.side}
    </div>
  )
}

/** Whose a row of a chat is: Claude's (its bubbles) or mine (everything else). */
const voiceOf = (shown: ShownRow, bubble: BubbleOf): 'theirs' | 'mine' => (shown.kind === 'entity' && bubble(shown.row) === 'theirs' ? 'theirs' : 'mine')

const modeNames: Record<ChatMode, string> = { prompt: 'Prompt', note: 'Note' }

/** Rows under a message or a note are indented under it, as in a tree. */
const chatIndent = (depth: number) => ({ paddingLeft: (depth - 1) * 22 })

/** The note mark (src/renderer/src/assets/note.svg). */
function NoteMark() {
  return <span className="note-mark" role="img" aria-label="Note" />
}

/**
 * A row of the chat, selectable like any row (a click, or the keys while the
 * box doesn't have them), marked by a rail at its left that keeps clear of
 * the bubbles' fills: a prompt as a bubble on the right, an answer on the
 * left, a note (or anything else) across the width, in full, with its mark
 * and its fold mark when it has rows under it.
 */
function ChatRow(props: { shown: EntityRow; props: OverviewProps; prompting: boolean; bubble: BubbleOf; flat?: boolean; joined?: boolean; aside?: ReactNode }) {
  const { shown } = props
  const { entity, depth } = shown.row
  const kinds = useContext(KindsContext)!
  const ref = useRef<HTMLDivElement>(null)
  const marked = shown.selected && !props.prompting && !props.flat
  useEffect(() => {
    if (marked) ref.current?.scrollIntoView({ block: 'nearest' })
  }, [marked])
  const editing = shown.editing ? <TextBox edit={props.props.edit} onEditDraft={props.props.onEditDraft} /> : null
  const frame = (content: ReactNode) => (
    <div ref={ref} className={`chat-row${marked ? ' selected' : ''}${props.joined ? ' joined' : ''}`} style={chatIndent(depth)} onMouseDown={props.flat ? undefined : () => props.props.onSelect(shown.row.path)}>
      {content}
      {props.aside && <div className="chat-aside">{props.aside}</div>}
    </div>
  )
  const fold = shown.row.hasChildren && !props.flat ? <span className="chat-fold">{shown.row.open ? '▾' : '▸'}</span> : null
  const bubble = props.bubble(shown.row)
  if (bubble) {
    const mine = bubble === 'mine'
    const failed = typeof entity.data.error === 'string' && entity.data.error
    return frame(
      <div className={`bubble-line ${mine ? 'right' : 'left'}`}>
        {!mine && fold}
        {!mine && <ClaudeMark />}
        <div className={`bubble ${mine ? 'mine' : failed ? 'theirs failed' : 'theirs'}`}>
          {editing ?? (entity.type === 'claude.response' ? <Answer entity={entity} /> : <Text text={text(entity.data)} />)}
        </div>
        {mine && fold}
      </div>,
    )
  }
  const Row = kinds[entity.type].Row
  return frame(
    <div className="note-line">
      {/* A flat chat's lines are all mine: no mark says so. */}
      {props.flat ? null : (fold ?? (entity.type === 'note' ? <NoteMark /> : <span className="chat-fold" />))}
      <div className="note-line-text">
        {editing ??
          (entity.type === 'note' ? (
            <Text text={text(entity.data)} />
          ) : (
            <Row entity={entity} parent={shown.row.parent} above={shown.row.above} onOpen={props.props.onOpen} onImage={props.props.onImage} />
          ))}
      </div>
    </div>,
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
 * The chat's panel on the right, beside the main view and as tall as it, as
 * the side panel is (its left edge drags to resize it, the same width as
 * that one): the chat's own overview at the top, then each item in its
 * context, a rule between them, each opening out to its whole view in place.
 * It scrolls when they are taller than it.
 */
function ChatPanel(props: { props: OverviewProps }) {
  const chat = props.props
  const slot = useContext(SideSlotContext)
  const resize = useEdgeResize(chat.sideWidth ?? null, chat.onSideResize)
  if (!slot) return null
  const peers = Array.isArray(chat.entity.data.contextPeers) ? (chat.entity.data.contextPeers as string[]) : []
  // Opened out here but not in its context (a note of the chat's): first, above the context.
  const others = Object.keys(chat.contextViews ?? {}).filter((id) => !peers.includes(id))
  return createPortal(
    <aside ref={resize.ref} className={`side chat-side${resize.dragging ? ' resizing' : ''}`} style={resize.style}>
      <div className="side-resize" {...resize.edge} />
      <div className="chat-side-body">
        <div className="chat-own">
          <SessionOverview {...chat} />
        </div>
        {[...others, ...peers].map((id) => (
          <ContextItem key={id} id={id} props={chat} />
        ))}
      </div>
    </aside>,
    slot,
  )
}

/**
 * An item in the chat's context: its overview, with a fold mark; opened out
 * (the mark, or a click on it), its whole view in its place, as a peek shows
 * it.
 */
function ContextItem(props: { id: string; props: OverviewProps }) {
  const chat = props.props
  const kinds = useContext(KindsContext)!
  const { item } = useContext(ItemContext)
  const opened = chat.contextViews?.[props.id] ?? null
  // Opened out, it is a window of its own, as a peek is: a press gives it the keys, and its rows the cursor.
  const key = contextWindow(props.id)
  const windows = chat.windows
  const shown = windows?.shown[key] ?? null
  const entity = item(props.id)
  const toggle = () => chat.onContextToggle?.(props.id)
  if (!entity) return <div className="context-item muted">…</div>
  const mark = (
    <span
      className="context-mark"
      title={opened ? 'Fold' : 'Open out'}
      onClick={(event) => {
        event.stopPropagation()
        toggle()
      }}
    >
      {opened ? '▾' : '▸'}
    </span>
  )
  if (opened && shown) {
    return (
      <div
        className="context-item open"
        onPointerDown={(event) => {
          // Its own window, not the main view the panel is drawn from.
          event.stopPropagation()
          windows?.handlers.onFocus(key)
        }}
      >
        {mark}
        <div className="context-body">
          <ViewPane
            {...chat}
            view={opened}
            shown={shown}
            interactive={false}
            focused={windows?.focus === key}
            prompt={undefined}
            acting={null}
            edit={windows?.edit ?? null}
            picking={null}
            find={null}
            working={undefined}
            onSelect={windows?.handlers.onSelect ?? (() => {})}
            onToggle={windows?.handlers.onToggle}
            onEditDraft={windows?.handlers.onEditDraft ?? chat.onEditDraft}
            onCommitEdit={windows?.handlers.onCommitEdit ?? chat.onCommitEdit}
            onCancelEdit={windows?.handlers.onCancelEdit ?? chat.onCancelEdit}
            onAction={undefined}
            onClose={toggle}
            onNearEnd={undefined}
          />
        </div>
      </div>
    )
  }
  const Overview = kinds[entity.type].Overview
  // Its overview alone: no tree, no actions, nothing of the chat's own.
  const view = { ...chat.view, root: entity, rows: [], actions: [], older: false, compose: null, error: null, loading: false }
  return (
    <div className="context-item" onClick={toggle}>
      {mark}
      <div className="context-body">
        <Overview
          {...chat}
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
    </div>
  )
}
