import { typing } from './dispatch.ts'
import type { Session } from './session.ts'

/** A file's content, base64, for the core to keep. */
function base64(file: File): Promise<string> {
  return new Promise((resolve, reject) => {
    const reader = new FileReader()
    reader.onload = () => resolve(String(reader.result).replace(/^data:[^,]*,/, ''))
    reader.onerror = () => reject(reader.error)
    reader.readAsDataURL(file)
  })
}

/**
 * The only paste listener. Outside any text box, on a topic, what is pasted
 * goes into it as what it is (`Session.paste`, `Session.attach`): each file
 * as a file item, a link to something the app knows as that item, any other
 * text as a message. Anywhere else, a paste is the browser's.
 */
export function installPaste(session: Session): () => void {
  const listener = (event: ClipboardEvent) => {
    const data = event.clipboardData
    if (!data || typing(event.target) || !session.canPaste()) return
    const files = [...data.files]
    const text = data.getData('text/plain')
    if (!files.length && !text.trim()) return
    event.preventDefault()
    for (const file of files) void base64(file).then((content) => session.attach({ name: file.name, mime: file.type, data: content }))
    if (!files.length) void session.paste(text)
  }
  window.addEventListener('paste', listener)
  return () => window.removeEventListener('paste', listener)
}
