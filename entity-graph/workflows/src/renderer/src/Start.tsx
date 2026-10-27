import { useEffect, useRef, useState } from 'react'
import { inBrowser, tokenFromLink } from './phone'
import { api } from './useWorkflows'

/** What the app needs before anything else: who you are, and which source (a file) to work on. */
export interface Session {
  user: string
  source: string
}

/**
 * Opens a source and signs in: the source's backend is started (or found
 * running), and the person is made if there is nobody by that username yet.
 * Answers the session as it is to be kept.
 */
export async function begin(s: Session & { token?: string }): Promise<Session> {
  if (s.token !== undefined) api().setToken?.(s.token)
  const file = await api().openSource(s.source)
  const user = await api().signIn(s.user)
  api().setUser(user.id)
  // In a browser the source is the backend's, which it says.
  return { user: user.id, source: inBrowser ? file : s.source }
}

// A link from the Phone page brings its token: taken as the page loads, before anything asks.
const linked = inBrowser ? tokenFromLink() : null

/**
 * The first screen: a username and a source, both needed. In a browser (the phone), a username and the token from
 * the Phone page instead: the source is the backend's. The source is a file
 * path (`~/` is the home folder); a file that is not there is made. Both are
 * kept in local storage, so the app opens straight onto them next time. With
 * both kept already, it opens them as it shows.
 */
export function Start({ kept, onBegin }: { kept: Partial<Session>; onBegin: (s: Session) => void }) {
  const [user, setUser] = useState(kept.user ?? '')
  const [source, setSource] = useState(kept.source ?? '')
  const [token, setToken] = useState(() => linked ?? api().token?.() ?? '')
  const [error, setError] = useState<string | null>(null)
  const [busy, setBusy] = useState(false)
  const go = async (s: Session) => {
    setError(null)
    setBusy(true)
    try {
      onBegin(await begin(inBrowser ? { ...s, token } : s))
    } catch (e) {
      setError((e as Error).message.replace(/^Error invoking remote method '\w+': (Error: )?/, ''))
      setBusy(false)
    }
  }
  // Both kept: open them at once.
  const tried = useRef(false)
  useEffect(() => {
    if (tried.current || !kept.user || !(inBrowser ? token : kept.source)) return
    tried.current = true
    void go({ user: kept.user, source: kept.source ?? '' })
  })
  const ready = user.trim() && (inBrowser ? token.trim() : source.trim()) && !busy
  const field = (label: string, value: string, set: (v: string) => void, placeholder: string, autoFocus: boolean, secret = false) => (
    <label className="start-field">
      <span className="muted">{label}</span>
      <div className="builder-row compact">
        <div className="builder-fields">
          <input
            autoFocus={autoFocus}
            type={secret ? 'password' : 'text'}
            autoCapitalize="off"
            autoCorrect="off"
            spellCheck={false}
            value={value}
            placeholder={placeholder}
            aria-label={label}
            onChange={(e) => set(e.target.value)}
            onKeyDown={(e) => e.key === 'Enter' && ready && void go({ user, source })}
          />
        </div>
      </div>
    </label>
  )
  return (
    <div className="sign-in">
      <div className="page">
        <h1>YAWN</h1>
        {error && <div className="error">{error}</div>}
        <section className="card">
          {field('Username', user, setUser, 'e.g. alex', !kept.user)}
          {inBrowser ? (
            <>
              {field('Token', token, setToken, 'From the Phone page, on the machine YAWN runs on', !!kept.user, true)}
              <p className="muted">The token is on the Phone page of the app on the machine it runs on. Someone new is made as they first sign in.</p>
            </>
          ) : (
            <>
              {field('Source', source, setSource, 'e.g. ~/tasks/tasks.sqlite', !!kept.user)}
              <p className="muted">A source that is not there is made. Someone new is made as they first sign in.</p>
            </>
          )}
          <div className="buttons">
            <button data-ui className="primary" disabled={!ready} onClick={() => void go({ user, source })}>
              {busy ? 'Opening…' : 'Open'}
            </button>
          </div>
        </section>
      </div>
    </div>
  )
}
