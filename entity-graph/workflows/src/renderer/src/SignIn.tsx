import { useEffect, useState } from 'react'
import type { User } from '../../shared/types'
import { api } from './useWorkflows'

/**
 * Who is using the app: a username, typed or picked. There is no password, as
 * the app is local; a username is the id of the person's item. A new person is
 * made here too.
 */
export function SignIn({ onSignIn }: { onSignIn: (u: User) => void }) {
  const [users, setUsers] = useState<User[]>([])
  const [username, setUsername] = useState('')
  const [name, setName] = useState('')
  const [making, setMaking] = useState(false)
  const [error, setError] = useState<string | null>(null)
  useEffect(() => void api().users().then(setUsers, (e: Error) => setError(e.message)), [])
  const go = async () => {
    setError(null)
    try {
      onSignIn(making ? await api().saveUser(username, name) : await api().signIn(username))
    } catch (e) {
      setError((e as Error).message.replace(/^Error invoking remote method '\w+': (Error: )?/, ''))
    }
  }
  return (
    <div className="sign-in">
      <div className="page">
        <h1>{making ? 'A new person' : 'Who are you?'}</h1>
        {error && <div className="error">{error}</div>}
        <section className="card">
          <div className="builder-row compact">
            <div className="builder-fields">
              <input
                autoFocus
                value={username}
                placeholder="Username"
                aria-label="Username"
                onChange={(e) => setUsername(e.target.value)}
                onKeyDown={(e) => e.key === 'Enter' && void go()}
              />
            </div>
          </div>
          {making && (
            <div className="builder-row compact">
              <div className="builder-fields">
                <input value={name} placeholder="Name (optional)" aria-label="Name" onChange={(e) => setName(e.target.value)} onKeyDown={(e) => e.key === 'Enter' && void go()} />
              </div>
            </div>
          )}
          <div className="buttons">
            <button data-ui className="primary" disabled={!username.trim()} onClick={() => void go()}>
              {making ? 'Make them, and sign in' : 'Sign in'}
            </button>
            <button data-ui onClick={() => setMaking(!making)}>
              {making ? 'Sign in instead' : 'Someone new'}
            </button>
          </div>
        </section>
        {users.length > 0 && !making && (
          <section className="card">
            <h2>People</h2>
            <div className="list">
              {users.map((u) => (
                <div key={u.id} className="row" onClick={() => setUsername(u.id)}>
                  <span className="row-title">{u.name}</span>
                  <span className="muted">{u.id}</span>
                </div>
              ))}
            </div>
          </section>
        )}
      </div>
    </div>
  )
}
