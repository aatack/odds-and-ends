import { CHAT_OWNER_KEY, CHAT_TOOL_KEY } from '../../../core/builtins'
import { str } from '../../../core/entity'
import { createEntity, readEntities } from '../source/entity'
import * as A from '../state/actions'
import { focusOf, getLayout } from '../state/store'
import { tabChats } from '../state/types'
import { entityArg, requireId } from './entityTools'
import type { CallInfo, ToolSpec } from './types'

// Chats: a note whose children are the messages in it, kept in the corner of a
// tab and talked to through a tool the note names.
//
// There is very little to any of this, which is deliberate. A chat is not a new
// kind of thing in the store — it is notes under a note, written by the ordinary
// write tools — and the conversation is not a new kind of call: `chat.send`
// looks up a tool the user already had and calls it the way a script would. What
// the app adds is the shape of the exchange (what you said, then what it said)
// and somewhere to have it.

/**
 * What the tool said, as a message. A string is the message itself; anything
 * else is shown as the JSON it is, since a tool that was not written to be
 * talked to still answers *something* and burying it would be worse than a
 * fenced block.
 */
function saidBy(value: unknown): string {
  if (typeof value === 'string') return value
  if (value == null) return ''
  return ['```json', JSON.stringify(value, null, 2), '```'].join('\n')
}

/**
 * The tab a chat goes in or out of: the one the call was born in, falling back
 * to whatever is in focus now. Not an argument — a tab is not something anybody
 * names by typing its uuid, and the only honest answer is the one you are
 * looking at.
 */
const tabOf = (call: CallInfo): string | null => call.context.tabId ?? focusOf(getLayout()).tabId

export const CHAT_TOOLS: ToolSpec[] = [
  {
    // The one gesture that makes a chat a chat: until a tab carries it, a note
    // with a `chat` value is a note with a value on it. Aimed from a row, so the
    // usual way in is a right-click on the note itself.
    id: 'chat.add',
    label: 'Add chat to tab',
    aliases: ['chat', 'message', 'conversation', 'messenger', 'talk to'],
    hint: 'Chat',
    scope: 'frame',
    reach: 'ui',
    args: [entityArg('chatId', 'Chat id')],
    run: ({ chatId }, call) => {
      const chat = requireId(chatId, 'Chat id')
      const tab = tabOf(call)
      if (!tab) throw new Error('No tab to put it in')
      A.addChat(tab, chat)
    },
  },
  {
    id: 'chat.remove',
    label: 'Remove chat from tab',
    aliases: ['close chat', 'hide chat', 'stop chatting'],
    hint: 'Chat',
    scope: 'frame',
    reach: 'ui',
    // Only offered where there is one to take away, so the palette of a tab with
    // no chats in it isn't half about chats.
    enabled: () => tabChats(getLayout().tabs[focusOf(getLayout()).tabId ?? '']).length > 0,
    args: [entityArg('chatId', 'Chat id')],
    run: ({ chatId }, call) => {
      const chat = requireId(chatId, 'Chat id')
      const tab = tabOf(call)
      if (!tab) throw new Error('No tab to take it out of')
      A.removeChat(tab, chat)
    },
  },
  {
    // Pressing Enter in a chat's box, and the whole of what a chat *does*.
    //
    // A tool rather than something the panel does for itself, for the reasons
    // every gesture in this app is one: it is worth recording (a message sent is
    // exactly the sort of thing the activity log is for), its errors belong in
    // the toast layer like every other call's, and it is then callable from a
    // script — a chat that talks to itself is a loop somebody can write rather
    // than a feature somebody has to ask for.
    //
    // The chat is named the way every tool names an entity — `entityId` — so a
    // right-click on the note sends to it and the panel says which chat it is by
    // overriding that key. The message's own key is `chatText` and not `text`:
    // `text` is on every entity in the fold, so an argument named after it would
    // arrive filled with whatever row the cursor happened to be on.
    id: 'chat.send',
    label: 'Send a chat message',
    aliases: ['chat', 'message', 'say', 'reply', 'ask'],
    hint: 'Chat',
    scope: 'frame',
    // The tool on the far end is usually one that leaves the app — a model, a
    // service — and a conversation is worth looking back at either way.
    reach: 'external',
    mutates: true,
    args: [
      entityArg('chatId', 'Chat id'),
      { name: 'text', label: 'Message', fromContext: 'chatText' },
    ],
    run: async ({ chatId, text }, call) => {
      const id = requireId(chatId, 'Chat id')
      const message = String(text ?? '').trim()
      if (!message) throw new Error('Nothing to send')

      // The store rather than the cache. This is a script's position — one
      // answer, no second chance to look — and the values are what the tool on
      // the far end is about to be called with.
      const chat = (await readEntities([id]))[id]
      const named = str(chat?.values[CHAT_TOOL_KEY])
      if (!named) {
        throw new Error(
          `${str(chat?.values.text) ?? id} has no \`${CHAT_TOOL_KEY}\` value naming a tool to send to`,
        )
      }

      // Imported here rather than at the top: the registry is built out of this
      // file, so a static import of either of these is a cycle through the array
      // below. The code runner reaches the call machine the same way.
      const [{ callToolByName }, { findToolByName, nearestToolNames }] = await Promise.all([
        import('./call'),
        import('./registry'),
      ])
      const tool = findToolByName(named)
      if (!tool) {
        const nearest = nearestToolNames(named)
        throw new Error(
          `No tool called "${named}"${nearest.length ? `. Did you mean ${nearest.join(', ')}?` : ''}`,
        )
      }

      // Said before the answer is waited for, so it is on screen while the
      // waiting happens — and so the author on it is the person who typed it,
      // which is what puts it on the right of the panel.
      await createEntity({ text: message }, id)

      // The chat's own values for whatever the tool declares, with the message
      // laid over the top. Filtered rather than passed whole because a tool
      // handed a key it doesn't declare refuses the call outright (see
      // `argsFromCall`), and a chat carries `text`, `type` and `chat` that are
      // the app's business and no tool's. So the chat holds the standing half of
      // the conversation and each message supplies the rest.
      const args: Record<string, unknown> = { text: message }
      for (const arg of tool.args ?? []) {
        const held = chat?.values[arg.name]
        if (arg.name !== 'text' && held != null) args[arg.name] = held
      }

      const answer = saidBy(await callToolByName(named, [args], call.context))
      // A tool that answered with nothing has still been sent to, and a blank
      // note in the conversation would read as a message that failed to render.
      // This one *is* worth saying out loud, since there is nothing on screen to
      // say it: silence from the panel would read as a call that never went.
      if (!answer) return { message: `${tool.label} said nothing` }
      // `owner` is what keeps the reply on the left: everything the app writes is
      // authored by whoever is at the keyboard, so without it the answer would
      // come back looking like something the user had said.
      await createEntity({ text: answer, [CHAT_OWNER_KEY]: tool.id }, id)
      // No message, deliberately. The answer is already on screen, two inches
      // above where the toast would land, and a chat that toasts every reply over
      // itself is a chat you cannot read while you are having it.
      return { data: answer }
    },
  },
]
