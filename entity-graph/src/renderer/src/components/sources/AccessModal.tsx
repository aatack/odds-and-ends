import React, { useCallback, useEffect, useState } from 'react'
import { Trash03 } from '@untitledui/icons'
import type { SourceNode, SourceToken } from '../../../../core/client'
import { Button } from '../ui/Button'
import { CopyButton } from '../ui/CopyButton'
import { IconButton } from '../ui/IconButton'
import { Input } from '../ui/Input'
import { Modal } from '../ui/Modal'
import { Switch } from '../ui/Switch'
import type { SourceGraphActions } from '../../views/useSourceGraph'

// Who may reach one published node, and how.
//
// A token is issued to a *person*, and the name is the whole point: every write
// that arrives with it is recorded as that author, whatever the client claims.
// So the list reads as a list of people rather than of secrets — and pausing one
// refuses it without forgetting whose it was, where revoking takes it off for
// good.

const SECTION = 'text-[11px] font-medium uppercase tracking-[0.09em] text-gray-500'

export function AccessModal({
  node,
  actions,
  onClose,
}: {
  node: SourceNode
  actions: SourceGraphActions
  onClose: () => void
}): React.JSX.Element {
  const [tokens, setTokens] = useState<SourceToken[]>([])
  const [name, setName] = useState('')
  const [issued, setIssued] = useState<SourceToken | null>(null)
  const [error, setError] = useState<string | null>(null)

  const read = useCallback(async () => {
    try {
      setTokens(await actions.tokens(node.id))
    } catch (e) {
      setError(e instanceof Error ? e.message : String(e))
    }
  }, [actions, node.id])

  useEffect(() => {
    void read()
  }, [read])

  const issue = async (): Promise<void> => {
    setError(null)
    try {
      // Shown once, in full, beside the row it made: a token is only useful
      // while it is on the clipboard, and there is nothing to gain by hiding it
      // from the person who just asked for it.
      setIssued(await actions.issueToken(node.id, name))
      setName('')
      await read()
    } catch (e) {
      setError(e instanceof Error ? e.message : String(e))
    }
  }

  return (
    <Modal title={`Access to ${node.label}`} onClose={onClose} size="wide">
      <div className="space-y-4">
        <div className="space-y-2">
          <p className={SECTION}>Tokens</p>
          {tokens.length === 0 ? (
            <p className="text-[13px] text-gray-400">
              Nobody can reach this yet. Issue a token below.
            </p>
          ) : (
            <div className="space-y-1">
              {tokens.map((token) => (
                <div key={token.token} className="flex items-center gap-2 rounded-md bg-gray-50 px-3 py-2">
                  <p className="w-32 shrink-0 truncate text-[13px] text-gray-900">{token.name}</p>
                  <p className="min-w-0 flex-1 truncate font-mono text-xs text-gray-400">
                    {token.token.slice(0, 8)}…
                  </p>
                  <Switch
                    label={`Allow ${token.name}`}
                    checked={!token.paused}
                    onChange={async (on) => {
                      await actions.pauseToken(token.token, !on)
                      await read()
                    }}
                  />
                  <CopyButton value={token.token} title={`Copy ${token.name}’s token`} />
                  <IconButton
                    title={`Revoke ${token.name}’s token`}
                    onClick={async () => {
                      await actions.revokeToken(token.token)
                      await read()
                    }}
                  >
                    <Trash03 size={16} />
                  </IconButton>
                </div>
              ))}
            </div>
          )}

          <div className="flex items-end gap-2">
            <label className="min-w-0 flex-1 space-y-1">
              <span className="block text-[11px] font-medium uppercase tracking-[0.06em] text-gray-500">
                Issue a token to
              </span>
              <Input
                value={name}
                onChange={(e) => setName(e.target.value)}
                onKeyDown={(e) => e.key === 'Enter' && void issue()}
                placeholder="their name — writes are recorded as this"
              />
            </label>
            <Button variant="primary" onClick={() => void issue()} disabled={!name.trim()}>
              Create
            </Button>
          </div>

          {issued && (
            <div className="flex items-start gap-1 rounded-md bg-gray-50 px-3 py-2">
              <p className="min-w-0 flex-1 break-all font-mono text-[11px] leading-snug text-gray-600">
                {issued.token}
              </p>
              <CopyButton value={issued.token} title="Copy the new token" />
            </div>
          )}

          {error && <p className="text-[13px] text-error-600">{error}</p>}
        </div>
      </div>
    </Modal>
  )
}
