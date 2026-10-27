// Recording into the chat: the microphone, streamed to Deepgram's live transcription, each finished utterance
// handed back to be sent as a note, as if typed. The key comes from the backend (Integrations, or
// DEEPGRAM_API_KEY); a browser WebSocket can't set headers, so it goes as Deepgram's `token` subprotocol.

import { useCallback, useEffect, useRef, useState } from 'react'
import { api } from './useWorkflows'

const LISTEN =
  'wss://api.deepgram.com/v1/listen?model=nova-3&smart_format=true&interim_results=true&endpointing=400&utterance_end_ms=1500&vad_events=true'

interface DeepgramMessage {
  type?: string
  is_final?: boolean
  speech_final?: boolean
  channel?: { alternatives?: { transcript?: string }[] }
}

/** Whether Deepgram is set up (a key kept, or DEEPGRAM_API_KEY): asked again whenever a chat input is shown. */
export function useHasDeepgram(): boolean {
  const [has, setHas] = useState(false)
  useEffect(() => {
    let live = true
    void api().hasDeepgram().then((h) => live && setHas(h))
    return () => void (live = false)
  }, [])
  return has
}

export type Recording = {
  /** Recording now (or starting). */
  on: boolean
  /** What is being said and not yet sent: the finished words, then Deepgram's guess at the rest. */
  hearing: string
  error: string | null
  start: () => void
  stop: () => void
}

/** A recorder: `onUtterance` gets each finished utterance (a pause ends one), and what is left when it stops. */
export function useRecorder(onUtterance: (text: string) => void): Recording {
  const [on, setOn] = useState(false)
  const [hearing, setHearing] = useState('')
  const [error, setError] = useState<string | null>(null)
  const parts = useRef<{ done: string[]; guess: string }>({ done: [], guess: '' })
  const live = useRef<{ socket: WebSocket; media: MediaRecorder; stream: MediaStream } | null>(null)
  const said = useRef(onUtterance)
  said.current = onUtterance

  const show = () => setHearing([...parts.current.done, parts.current.guess].filter(Boolean).join(' '))
  const flush = useCallback(() => {
    const text = parts.current.done.join(' ').trim()
    parts.current.done = []
    if (text) said.current(text)
  }, [])

  const stop = useCallback(() => {
    const l = live.current
    live.current = null
    setOn(false)
    if (!l) return
    if (l.media.state !== 'inactive') l.media.stop()
    l.stream.getTracks().forEach((t) => t.stop())
    // Deepgram sends what it has left, then closes.
    if (l.socket.readyState === WebSocket.OPEN) l.socket.send(JSON.stringify({ type: 'CloseStream' }))
    setTimeout(() => {
      if (l.socket.readyState !== WebSocket.CLOSED) l.socket.close()
      flush()
      parts.current.guess = ''
      setHearing('')
    }, 1500)
  }, [flush])

  const start = useCallback(() => {
    if (live.current) return
    setError(null)
    setOn(true)
    parts.current = { done: [], guess: '' }
    void (async () => {
      try {
        const key = await api().deepgramKey()
        if (!key) throw new Error('No Deepgram key: add one on the Integrations page.')
        const stream = await navigator.mediaDevices.getUserMedia({ audio: true })
        const socket = new WebSocket(LISTEN, ['token', key])
        const media = new MediaRecorder(stream, { mimeType: 'audio/webm;codecs=opus' })
        live.current = { socket, media, stream }
        media.ondataavailable = (e) => e.data.size && socket.readyState === WebSocket.OPEN && socket.send(e.data)
        socket.onopen = () => media.start(250)
        socket.onmessage = (m) => {
          const msg = JSON.parse(String(m.data)) as DeepgramMessage
          if (msg.type === 'UtteranceEnd') return flush()
          if (msg.type !== 'Results') return
          const text = msg.channel?.alternatives?.[0]?.transcript?.trim() ?? ''
          if (msg.is_final) {
            if (text) parts.current.done.push(text)
            parts.current.guess = ''
            if (msg.speech_final) flush()
          } else parts.current.guess = text
          show()
        }
        socket.onerror = () => setError('Deepgram could not be reached (is the key right?).')
        socket.onclose = (e) => {
          if (live.current?.socket === socket) {
            if (e.code !== 1000) setError(`Deepgram closed the stream${e.reason ? `: ${e.reason}` : ''}.`)
            stop()
          }
        }
      } catch (e) {
        setError((e as Error).message)
        stop()
      }
    })()
  }, [flush, stop])

  // Leaving the task stops the recording; what was said is still sent.
  useEffect(() => () => stop(), [stop])
  return { on, hearing, error, start, stop }
}
