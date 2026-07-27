import React, { useState } from 'react'
import { Button } from '../components/ui/Button'
import { Field, Select } from '../components/ui/Field'
import { Sheet } from '../components/ui/Sheet'
import { disconnect } from '../source/connection'
import { useAtomValue, useConnection, useUndoStack, useView } from '../state/hooks'
import { themeAtom } from '../state/store'
import { closeSheet } from '../state/ui'
import { dispatch } from '../tools/dispatch'
import type { Theme } from '../state/types'
import { ConnectForm } from './ConnectForm'

// Connection, appearance, and the two facts worth being able to check: how deep the
// undo stack is, and what the app thinks it is connected to.

export function SettingsSheet(): React.JSX.Element {
  const connection = useConnection()
  const undo = useUndoStack()
  const view = useView()
  const theme = useAtomValue(themeAtom)
  const [editing, setEditing] = useState(false)

  return (
    <Sheet title="Settings" onClose={closeSheet}>
      <div className="flex flex-col gap-5 pt-1 pb-3">
        {editing ? (
          <ConnectForm
            initial={connection}
            submitLabel="Save"
            onDone={() => {
              setEditing(false)
              closeSheet()
            }}
          />
        ) : (
          <section className="flex flex-col gap-2">
            <h3 className="text-[11px] font-semibold tracking-wide text-gray-400 uppercase">
              Source
            </h3>
            <dl className="rounded-xl bg-gray-100 px-3.5 py-3 text-[13.5px]">
              <Row label="Server" value={connection?.baseUrl ?? '—'} />
              <Row label="Source" value={connection?.sourceId ?? '—'} />
              <Row label="Author" value={connection?.author ?? '—'} />
            </dl>
            <div className="flex gap-2">
              <Button tone="plain" className="flex-1" onClick={() => setEditing(true)}>
                Change
              </Button>
              <Button
                tone="danger"
                className="flex-1"
                onClick={() => {
                  closeSheet()
                  disconnect()
                }}
              >
                Disconnect
              </Button>
            </div>
          </section>
        )}

        <section className="flex flex-col gap-2">
          <h3 className="text-[11px] font-semibold tracking-wide text-gray-400 uppercase">
            Appearance
          </h3>
          <Field label="Theme">
            <Select value={theme} onChange={(e) => themeAtom.set(e.target.value as Theme)}>
              <option value="system">Follow the system</option>
              <option value="light">Light</option>
              <option value="dark">Dark</option>
            </Select>
          </Field>
        </section>

        <section className="flex flex-col gap-2">
          <h3 className="text-[11px] font-semibold tracking-wide text-gray-400 uppercase">
            State
          </h3>
          <dl className="rounded-xl bg-gray-100 px-3.5 py-3 text-[13.5px]">
            <Row label="Undo steps held" value={String(undo.length)} />
            <Row label="Levels deep" value={String(view.stack.length)} />
            <Row label="Rows folded" value={String(view.collapsed.length)} />
          </dl>
          <Button
            tone="plain"
            block
            onClick={() => {
              closeSheet()
              dispatch('app.reload')
            }}
          >
            Re-read what this source can do
          </Button>
        </section>
      </div>
    </Sheet>
  )
}

function Row({ label, value }: { label: string; value: string }): React.JSX.Element {
  return (
    <div className="flex gap-3 py-0.5">
      <dt className="shrink-0 text-gray-500">{label}</dt>
      <dd className="min-w-0 flex-1 truncate text-right text-gray-800">{value}</dd>
    </div>
  )
}
