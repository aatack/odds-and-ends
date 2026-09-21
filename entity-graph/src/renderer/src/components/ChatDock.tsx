import React, { useEffect, useRef, useState } from 'react'
import { Loading02, X } from '@untitledui/icons'
import { EntityMarkdown } from './EntityMarkdown'
import { PillWrapper } from './EntityPill'
import { Identicon } from './ui/Identicon'
import { TextEditor } from './ui/TextEditor'
import { CHAT_OWNER_KEY, CHAT_TOOL_KEY } from '../../../core/builtins'
import { str } from '../../../core/entity'
import { cn } from '../helpers/cn'
import { currentUser } from '../source/transport'
import * as A from '../state/actions'
import { useCallRunning, useEntityLabel, useGetEntities, useLayoutState } from '../state/hooks'
import { tabChats } from '../state/types'
import { runTool } from '../tools/call'

// The conversations a tab is keeping: a little round picture each, down the
// bottom right corner, and one panel at a time beside them. Deliberately the
// shape everybody already knows from a messenger, because a chat has no other
// affordances worth inventing.
//
// It owns no domain state. The list of chats is on the tab, the messages are the
// chat entity's children, and sending is `chat.send` — so what is here is the
// drawing of those three and nothing else.

/** How much of the conversation is on screen before it starts scrolling. */
const BODY = 'max-h-80'

export function ChatDock({ tabId }: { tabId: string }): React.JSX.Element | null {
  const layout = useLayoutState()
  const tab = layout.tabs[tabId]
  const chats = tabChats(tab)
  if (!chats.length) return null

  // Only if it is still one of the tab's, so a chat removed in another window —
  // or by an undo of the layout — doesn't leave its panel behind.
  const openId = tab?.openChatId && chats.includes(tab.openChatId) ? tab.openChatId : null

  return (
    // The frame keeps its full height under this: a chat floats over the rows
    // the way the filter pills float over them at the other corner, so opening
    // one never reflows what you were reading.
    <div className="pointer-events-none absolute bottom-3 right-3 z-20 flex items-end gap-2">
      {openId && (
        <ChatPanel
          key={openId}
          chatId={openId}
          onClose={() => A.openChat(tabId, null)}
          onRemove={() => A.removeChat(tabId, openId)}
        />
      )}
      <div className="pointer-events-auto flex flex-col-reverse gap-2">
        {chats.map((chatId) => (
          <ChatButton
            key={chatId}
            chatId={chatId}
            open={chatId === openId}
            onClick={() => A.openChat(tabId, chatId === openId ? null : chatId)}
          />
        ))}
      </div>
    </div>
  )
}

/**
 * One chat, shut: its picture. Wrapped as a pill so it carries the entity
 * gestures every other mention of an entity does — right-click it for the tool
 * list (which is where **Remove chat from tab** is), middle-click it to open the
 * conversation as an ordinary outline in a new tab.
 */
function ChatButton({
  chatId,
  open,
  onClick,
}: {
  chatId: string
  open: boolean
  onClick: () => void
}): React.JSX.Element {
  const label = useEntityLabel(chatId)
  return (
    <PillWrapper id={chatId} title={label}>
      <button
        onClick={onClick}
        aria-label={label}
        className={cn(
          'rounded-full shadow-lg focus:outline-none focus-visible:ring-2 focus-visible:ring-brand-500/40',
          // Tone rather than a border, and the ring is the only thing that says
          // which one is open — there being a panel beside it already does.
          open && 'ring-2 ring-brand-200',
        )}
      >
        <Identicon id={chatId} size={34} />
      </button>
    </PillWrapper>
  )
}

/** One chat, open: who it is, what has been said, and a box to say more in. */
function ChatPanel({
  chatId,
  onClose,
  onRemove,
}: {
  chatId: string
  onClose: () => void
  onRemove: () => void
}): React.JSX.Element {
  const get = useGetEntities()
  const label = useEntityLabel(chatId)
  // Asking is what loads it; the children are the messages, in link order, which
  // is the order they were said in.
  const chat = get([chatId])[chatId]
  const messageIds = chat.outboundLinks
  const messages = get(messageIds)
  // Who *this* window is writing as. The same answer every write stamps on its
  // events, which is what the sides of the conversation are decided against.
  const me = currentUser()

  const [draft, setDraft] = useState('')
  const [callId, setCallId] = useState<string | null>(null)
  const sending = useCallRunning(callId)

  const named = str(chat.values[CHAT_TOOL_KEY])
  const bottom = useRef<HTMLDivElement>(null)

  // Stay at the bottom as the conversation grows, which is where a chat is read
  // from. No smooth scrolling: there is no motion anywhere in this app.
  useEffect(() => {
    bottom.current?.scrollIntoView({ block: 'end' })
  }, [messageIds.length, sending])

  const send = (): void => {
    const text = draft.trim()
    if (!text) return
    // Cleared before the call rather than after it: the message is the store's
    // now, and a box that stays full while it is being sent invites a second
    // press of the same line.
    setDraft('')
    setCallId(runTool('chat.send', { extra: { entityId: chatId, chatText: text } }))
  }

  return (
    <div className="pointer-events-auto flex w-80 flex-col overflow-hidden rounded-xl bg-white shadow-lg">
      <div className="flex items-center gap-2 bg-gray-50 px-3 py-2">
        <Identicon id={chatId} size={22} />
        <PillWrapper id={chatId} className="min-w-0 flex-1">
          <span className="truncate font-serif text-[13px] text-gray-900">{label}</span>
        </PillWrapper>
        <button
          className="shrink-0 text-gray-400 hover:text-gray-700 focus:outline-none"
          onClick={onClose}
          aria-label="Close chat"
        >
          <X size={13} />
        </button>
      </div>

      <div className={cn('flex flex-col gap-2 overflow-y-auto px-3 py-3', BODY)}>
        {!named && (
          // Said rather than left to fail on the first Enter: a chat with no tool
          // behind it is a note somebody has half written, and the fix is a value
          // on it.
          <p className="text-[12px] text-gray-400">
            Nothing to send to — this note has no <code>{CHAT_TOOL_KEY}</code> value naming a tool.
            Set one, and then say something.
          </p>
        )}
        {messageIds.length === 0 && named && (
          <p className="text-[12px] text-gray-400">Nothing said yet.</p>
        )}
        {messageIds.map((id) => (
          <Message key={id} chatId={chatId} id={id} entity={messages[id]} me={me} />
        ))}
        {sending && (
          <span className="flex items-center gap-1.5 self-start text-[12px] text-gray-400">
            <Loading02 size={11} />
            Waiting
          </span>
        )}
        <div ref={bottom} />
      </div>

      <div className="bg-gray-50 px-3 py-2">
        <TextEditor
          eager
          value={draft}
          setValue={setDraft}
          placeholder={named ? 'Say something…' : 'No tool to send to'}
          // Enter sends rather than committing the box, so the built-in handling
          // is taken over; Shift+Enter is left alone and puts in a newline, the
          // way it does in every other message box.
          onKeyDown={(e) => {
            if (e.key !== 'Enter' || e.shiftKey) return
            e.preventDefault()
            send()
          }}
          className="block font-serif text-[13px] leading-5 text-gray-900"
        />
      </div>

      {/* Quiet, and at the bottom, because taking a chat out of a tab is the one
          thing here that isn't part of having the conversation. */}
      <button
        className="px-3 pb-2 text-left text-[11px] text-gray-400 hover:text-gray-700 focus:outline-none"
        onClick={onRemove}
      >
        Remove from this tab
      </button>
    </div>
  )
}

/**
 * One message. Rendered by the ordinary entity renderer — a message is a note
 * like any other, markdown, inline fields and all — on a bubble that says which
 * side of the conversation it came from.
 *
 * Whose it is, is whoever wrote it, unless the note says otherwise: a reply
 * carries `owner` naming the tool that answered, because the app wrote that note
 * too and its events are authored by the person at the keyboard.
 */
function Message({
  chatId,
  id,
  entity,
  me,
}: {
  chatId: string
  id: string
  entity: { values: Record<string, unknown>; createdBy: string } | undefined
  me: string
}): React.JSX.Element {
  const text = str(entity?.values.text) ?? ''
  const owner = str(entity?.values[CHAT_OWNER_KEY]) ?? entity?.createdBy ?? ''
  const mine = owner === me

  return (
    <div
      // The message publishes itself, so the global handlers reach it: a reply
      // can be right-clicked for the tool list or middle-clicked into a tab of
      // its own, exactly as a row can.
      data-entity-id={id}
      data-parent-id={chatId}
      className={cn(
        'max-w-[85%] rounded-lg px-2.5 py-1.5',
        mine ? 'self-end bg-brand-50' : 'self-start bg-gray-100',
      )}
    >
      {text ? (
        <EntityMarkdown
          entityId={id}
          path={[chatId, id]}
          text={text}
          className="block font-serif text-[13px] leading-5 text-gray-900"
        />
      ) : (
        <span className="block font-serif text-[13px] italic leading-5 text-gray-400">Empty</span>
      )}
    </div>
  )
}
