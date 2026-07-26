import React, { useState } from 'react'
import { Button } from '../components/ui/Button'
import { Field, Input } from '../components/ui/Field'
import {
  connect,
  normaliseBaseUrl,
  type Connection,
} from '../source/connection'
import { toast } from '../state/toast'

// Where a phone is told which source it is looking at. Used twice: as the first-run
// screen, and inside the settings sheet to point it somewhere else.
//
// Everything is entered here rather than discovered, because there is nothing to
// discover from: the desktop app keeps a list of servers and admin tokens in its
// main process and can enumerate a server's sources, and this app deliberately has
// none of that. One source, named explicitly, with a token that reaches only it.

const BLANK: Connection = { baseUrl: '', sourceId: '', token: '', author: 'mobile' }

export function ConnectForm({
  initial,
  onDone,
  submitLabel = 'Connect',
}: {
  initial?: Connection | null
  onDone?: () => void
  submitLabel?: string
}): React.JSX.Element {
  const [draft, setDraft] = useState<Connection>(initial ?? BLANK)
  const [busy, setBusy] = useState(false)
  const [error, setError] = useState<string | null>(null)

  const set = (key: keyof Connection) => (e: React.ChangeEvent<HTMLInputElement>) =>
    setDraft((d) => ({ ...d, [key]: e.target.value }))

  const submit = async (): Promise<void> => {
    setBusy(true)
    setError(null)
    const candidate: Connection = {
      baseUrl: normaliseBaseUrl(draft.baseUrl),
      sourceId: draft.sourceId.trim(),
      token: draft.token.trim(),
      author: draft.author.trim() || 'mobile',
    }
    try {
      // Checked before it is saved: a connection that doesn't work is worse than no
      // connection, because the app then looks broken rather than unconfigured.
      await connect(candidate)
      toast('Connected')
      onDone?.()
    } catch (e) {
      setError(e instanceof Error ? e.message : String(e))
    } finally {
      setBusy(false)
    }
  }

  return (
    <div className="flex flex-col gap-3.5">
      <Field label="Server" hint="The address the laptop serves on — include the port.">
        <Input
          value={draft.baseUrl}
          onChange={set('baseUrl')}
          placeholder="http://192.168.1.20:4000"
          inputMode="url"
          autoCapitalize="none"
          autoCorrect="off"
          spellCheck={false}
        />
      </Field>
      <Field label="Source" hint="The id of the source to open.">
        <Input
          value={draft.sourceId}
          onChange={set('sourceId')}
          placeholder="flow"
          autoCapitalize="none"
          autoCorrect="off"
          spellCheck={false}
        />
      </Field>
      <Field label="Token" hint="A token issued for that source.">
        <Input
          value={draft.token}
          onChange={set('token')}
          placeholder="…"
          type="password"
          autoCapitalize="none"
          autoCorrect="off"
          spellCheck={false}
        />
      </Field>
      <Field label="Author" hint="Recorded against everything written from this phone.">
        <Input value={draft.author} onChange={set('author')} placeholder="mobile" />
      </Field>
      {error && <p className="text-[13.5px] text-error-600">{error}</p>}
      <Button tone="primary" block disabled={busy} onClick={() => void submit()}>
        {busy ? 'Checking…' : submitLabel}
      </Button>
    </div>
  )
}
