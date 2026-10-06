// Speech to text as it is spoken: the microphone, streamed to Deepgram's live
// API, handed back a sentence at a time.
//
// The socket is here rather than in the main process because this side holds the
// microphone, and the main process — Node 20, under this Electron — has no
// WebSocket client to hold one with. So the key comes over on a channel of its
// own (`transcriptionKey`), and the browser's WebSocket carries it as a
// subprotocol, which is how Deepgram takes a key from a client that cannot set
// headers.
//
// Nothing here knows about entities. What a sentence becomes is the caller's.

const LISTEN_URL =
  'wss://api.deepgram.com/v1/listen?' +
  new URLSearchParams({
    model: 'nova-3',
    smart_format: 'true',
    punctuate: 'true',
    // Interim results are what the pill shows while somebody is still talking;
    // finals are what is written down.
    interim_results: 'true',
    // How long a pause ends an utterance, and the backstop for when the audio is
    // too noisy for that pause to be heard.
    endpointing: '300',
    utterance_end_ms: '1000',
  }).toString()

/** How often a chunk of audio goes out. Small enough that interims feel live. */
const CHUNK_MS = 250

/** How long a closing socket may take to hand back its last words. */
const CLOSE_GRACE_MS = 3000

// --- The reading of Deepgram's messages, which is pure ---------------------

/** What has been heard of the utterance in progress. */
export interface Heard {
  /** Final segments of the current utterance, not yet written down. */
  finals: string[]
  /** The segment still being guessed at. */
  interim: string
}

export const NOTHING_HEARD: Heard = { finals: [], interim: '' }

/**
 * One message from the socket, applied. Deepgram sends a segment several times
 * as it firms up (`is_final: false`), then once for good (`is_final: true`); an
 * utterance is several final segments, ended by `speech_final` — or by an
 * `UtteranceEnd` message when the pause was never clean enough to call. Ending
 * one hands back what was said, as sentences.
 */
export function hear(heard: Heard, message: unknown): { heard: Heard; said: string[] } {
  const m = message as {
    type?: string
    is_final?: boolean
    speech_final?: boolean
    channel?: { alternatives?: { transcript?: string }[] }
  }
  if (m?.type === 'UtteranceEnd') return flush(heard)
  if (m?.type !== 'Results') return { heard, said: [] }
  const text = (m.channel?.alternatives?.[0]?.transcript ?? '').trim()
  if (!m.is_final) return { heard: { ...heard, interim: text }, said: [] }
  const next = { finals: text ? [...heard.finals, text] : heard.finals, interim: '' }
  return m.speech_final ? flush(next) : { heard: next, said: [] }
}

/** End the utterance in progress, whatever state it is in. */
export function flush(heard: Heard): { heard: Heard; said: string[] } {
  const text = [...heard.finals, heard.interim].join(' ').trim()
  return { heard: NOTHING_HEARD, said: sentences(text) }
}

/** What is being said right now, for showing rather than keeping. */
export const hearing = (heard: Heard): string => [...heard.finals, heard.interim].join(' ').trim()

/**
 * A run of speech, a sentence per entry. Deepgram punctuates, so this is a split
 * after `.`, `?` or `!` and whatever closes a quote behind one. It will split
 * after "Mr." too; a transcript line is a line to be read, not a sentence to be
 * parsed, and one broken in two reads fine.
 */
export function sentences(text: string): string[] {
  return text
    .split(/(?<=[.?!]["')\]]*)\s+/)
    .map((s) => s.trim())
    .filter(Boolean)
}

// --- The stream --------------------------------------------------------------

export interface TranscriptionHandlers {
  /** A sentence heard for good. */
  onSentence: (text: string) => void
  /** What is being said right now, or '' when nobody is. */
  onHearing: (text: string) => void
  /** The stream ended without being asked to, and why. */
  onFailure: (message: string) => void
}

export interface LiveTranscription {
  /** Stop listening. Whatever was still being said is handed over first. */
  stop: () => Promise<void>
}

/**
 * Start listening. Resolves once the microphone is open and the socket is up,
 * so a caller can say "live" and mean it; rejects with a sentence worth showing
 * when either is refused.
 */
export async function startLiveTranscription(
  key: string,
  handlers: TranscriptionHandlers,
): Promise<LiveTranscription> {
  let stream: MediaStream
  try {
    stream = await navigator.mediaDevices.getUserMedia({ audio: true })
  } catch (e) {
    throw new Error(`Could not open the microphone: ${e instanceof Error ? e.message : String(e)}`)
  }
  const release = (): void => stream.getTracks().forEach((t) => t.stop())

  let socket: WebSocket
  try {
    socket = await open(key)
  } catch (e) {
    release()
    throw e
  }

  let heard = NOTHING_HEARD
  let stopping = false
  const say = (said: string[]): void => said.forEach(handlers.onSentence)

  socket.onmessage = (event) => {
    let message: unknown
    try {
      message = JSON.parse(String(event.data))
    } catch {
      return
    }
    const step = hear(heard, message)
    heard = step.heard
    say(step.said)
    handlers.onHearing(hearing(heard))
  }

  // One recorder per socket, started once the socket is open: the first chunk
  // carries the container's header, and Deepgram reads the format from it.
  const recorder = new MediaRecorder(stream, { mimeType: 'audio/webm;codecs=opus' })
  recorder.ondataavailable = (event) => {
    if (event.data.size > 0 && socket.readyState === WebSocket.OPEN) socket.send(event.data)
  }
  recorder.start(CHUNK_MS)

  const closed = new Promise<void>((resolve) => {
    socket.onclose = (event) => {
      if (recorder.state !== 'inactive') recorder.stop()
      release()
      const rest = flush(heard)
      heard = rest.heard
      say(rest.said)
      handlers.onHearing('')
      if (!stopping) {
        handlers.onFailure(
          `Transcription stopped: ${event.reason || `the connection closed (${event.code})`}`,
        )
      }
      resolve()
    }
  })

  return {
    stop: async () => {
      if (stopping) return closed
      stopping = true
      if (recorder.state !== 'inactive') recorder.stop()
      // Asked to finish rather than cut off, so the words still in flight come
      // back as finals before the socket closes. Closed anyway if they don't.
      if (socket.readyState === WebSocket.OPEN) socket.send(JSON.stringify({ type: 'CloseStream' }))
      const grace = setTimeout(() => socket.close(), CLOSE_GRACE_MS)
      await closed
      clearTimeout(grace)
    },
  }
}

/** The socket, open. A refused key closes it before it opens, and says little. */
function open(key: string): Promise<WebSocket> {
  return new Promise((resolve, reject) => {
    const socket = new WebSocket(LISTEN_URL, ['token', key])
    socket.onopen = () => {
      socket.onerror = null
      resolve(socket)
    }
    socket.onerror = () => {
      reject(new Error('Deepgram refused the connection. Check DEEPGRAM_API_KEY in .env'))
    }
  })
}
