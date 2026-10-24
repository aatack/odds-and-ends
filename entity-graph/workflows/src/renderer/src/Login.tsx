// Phone-only: the one screen before the app, shown when there is no token (or
// after a 401). One token field and Connect; the token is verified by a real
// call and then kept in localStorage. A `#token=…` fragment (from the Phone
// page's connect link) fills it in, is stripped from the URL, and connects.

import { useCallback, useEffect, useState } from 'react'
import { api } from './useWorkflows'
import { clearToken, setToken } from './transport/http'

export function Login({ onConnected }: { onConnected: () => void }) {
  const [token, setField] = useState('')
  const [error, setError] = useState<string | null>(null)
  const [busy, setBusy] = useState(false)

  // Keep the token only if it works: store it, make one call, roll back on failure.
  const connect = useCallback(
    async (secret: string) => {
      const trimmed = secret.trim()
      if (!trimmed) return
      setBusy(true)
      setError(null)
      setToken(trimmed)
      try {
        await api().overview()
        onConnected()
      } catch {
        clearToken()
        setError('That token did not work. Copy it again from the Phone page on your laptop.')
      } finally {
        setBusy(false)
      }
    },
    [onConnected],
  )

  // A connect link carries the token in the fragment: take it, clear it from the URL, connect.
  useEffect(() => {
    const match = location.hash.match(/token=([^&]+)/)
    if (!match) return
    const secret = decodeURIComponent(match[1])
    history.replaceState(null, '', location.pathname + location.search)
    setField(secret)
    void connect(secret)
  }, [connect])

  return (
    <div className="login">
      <form
        className="login-box"
        onSubmit={(e) => {
          e.preventDefault()
          void connect(token)
        }}
      >
        <h1>Workflows</h1>
        <p className="muted">Paste the token from the Phone page on your laptop.</p>
        {error && <div className="error">{error}</div>}
        <input
          className="login-field"
          type="password"
          inputMode="text"
          autoCapitalize="off"
          autoCorrect="off"
          spellCheck={false}
          aria-label="Token"
          placeholder="Token"
          value={token}
          onChange={(e) => setField(e.target.value)}
        />
        <button data-ui className="primary" type="submit" disabled={busy || !token.trim()}>
          {busy ? 'Connecting…' : 'Connect'}
        </button>
      </form>
    </div>
  )
}
