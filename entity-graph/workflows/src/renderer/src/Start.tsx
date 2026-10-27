import { useEffect, useRef, useState } from 'react'
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
export async function begin(s: Session): Promise<Session> {
  await api().openSource(s.source)
  const user = await api().signIn(s.user)
  api().setUser(user.id)
  return { user: user.id, source: s.source }
}

/**
 * The first screen: a username and a source, both needed. The source is a file
 * path (`~/` is the home folder); a file that is not there is made. Both are
 * kept in local storage, so the app opens straight onto them next time. With
 * both kept already, it opens them as it shows.
 */
export function Start({ kept, onBegin }: { kept: Partial<Session>; onBegin: (s: Session) => void }) {
  const [user, setUser] = useState(kept.user ?? '')
  const [source, setSource] = useState(kept.source ?? '')
  const [error, setError] = useState<string | null>(null)
  const [busy, setBusy] = useState(false)
  const go = async (s: Session) => {
    setError(null)
    setBusy(true)
    try {
      onBegin(await begin(s))
    } catch (e) {
      setError((e as Error).message.replace(/^Error invoking remote method '\w+': (Error: )?/, ''))
      setBusy(false)
    }
  }
  // Both kept: open them at once.
  const tried = useRef(false)
  useEffect(() => {
    if (tried.current || !kept.user || !kept.source) return
    tried.current = true
    void go({ user: kept.user, source: kept.source })
  })
  const ready = user.trim() && source.trim() && !busy
  const field = (label: string, value: string, set: (v: string) => void, placeholder: string, autoFocus: boolean) => (
    <label className="start-field">
      <span className="muted">{label}</span>
      <div className="builder-row compact">
        <div className="builder-fields">
          <input
            autoFocus={autoFocus}
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
          {field('Source', source, setSource, 'e.g. ~/tasks/tasks.sqlite', !!kept.user)}
          <p className="muted">A source that is not there is made. Someone new is made as they first sign in.</p>
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
