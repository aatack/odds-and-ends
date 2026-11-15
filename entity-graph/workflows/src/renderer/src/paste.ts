// Pasting, the same everywhere, by what has the focus (one listener on the document: `usePaste`).
//
// - In a text box, text goes in as it is; a file is kept as a file item in no topic, and `[name](id)` goes in.
// - Anywhere else, the nearest place that lists items (`pasteHere`: a chat, the notes, Pinned, Pinned in) around what
//   was last pressed takes it: the backend makes what was pasted the item it looks like (`inferPasted`, the one place
//   that says) and puts it there.

import { useEffect } from 'react'
import type { Pasted, PasteTarget } from '../../shared/types'
import { api } from './useWorkflows'

type Act = (fn: () => Promise<unknown>) => Promise<void>

/** Marks an element as a place that lists items, where what is pasted goes (`target`). */
export const pasteHere = (target: PasteTarget) => ({ 'data-paste': JSON.stringify(target) })

/** A file given to the window, as the backend takes it. */
export const asFile = async (f: File): Promise<Pasted> => {
  const bytes = new Uint8Array(await f.arrayBuffer())
  let bin = ''
  for (let i = 0; i < bytes.length; i += 0x8000) bin += String.fromCharCode(...bytes.subarray(i, i + 0x8000))
  return { file: { name: f.name || 'pasted', base64: btoa(bin) } }
}

const typedIn = (el: Element | null): el is HTMLElement =>
  el instanceof HTMLElement && (el.matches('textarea, input') || el.isContentEditable)

// What was pressed last: a place that takes no focus (a chat's background) still says where a paste goes.
let pressed: Element | null = null

/** Listens for pastes on the whole document while mounted; what goes wrong is shown as `act` shows it. */
export function usePaste(act: Act) {
  useEffect(() => {
    const down = (e: MouseEvent) => (pressed = e.target as Element)
    const paste = (e: ClipboardEvent) => {
      const data = e.clipboardData
      if (!data) return
      const files = [...data.files]
      const at = e.target instanceof Element ? e.target : null
      if (typedIn(at)) {
        if (!files.length) return
        e.preventDefault()
        void act(async () => {
          for (const f of files) {
            const id = await api().paste({ into: null }, await asFile(f))
            at.focus()
            document.execCommand('insertText', false, `[${(f.name || 'pasted').replace(/[[\]]/g, '')}](${id})`)
          }
        })
        return
      }
      const from = at && at !== document.body ? at : pressed
      const place = from?.closest<HTMLElement>('[data-paste]') ?? from?.querySelector<HTMLElement>('[data-paste]')
      const text = data.getData('text')
      if (!place?.dataset.paste || (!files.length && !text.trim())) return
      e.preventDefault()
      const target = JSON.parse(place.dataset.paste) as PasteTarget
      void act(async () => {
        if (!files.length) await api().paste(target, { text })
        for (const f of files) await api().paste(target, await asFile(f))
      })
    }
    document.addEventListener('mousedown', down, true)
    document.addEventListener('paste', paste)
    return () => {
      document.removeEventListener('mousedown', down, true)
      document.removeEventListener('paste', paste)
    }
  }, [act])
}
