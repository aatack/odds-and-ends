import React, { useState } from 'react'
import { Button } from '../components/ui/Button'
import { Field, Input, Select, TextArea } from '../components/ui/Field'
import { Sheet } from '../components/ui/Sheet'
import { closeSheet } from '../state/ui'
import { dispatch } from '../tools/dispatch'
import { toolById } from '../tools/registry'
import { argsOf, kindOf, type ArgSpec } from '../tools/types'

// The form a tool opens when it still needs something typed.
//
// Every declared argument is shown, not only the missing ones: an argument the
// context filled — the entity you are standing on — is exactly the one you might
// want to change, and hiding it would make the sheet a mystery. The desktop app's
// palette does the same thing with Shift+Tab.
//
// Values are parsed on submit rather than per keystroke: on a phone, an error
// appearing under a field while a thumb is still mid-word is noise.

const parse = (spec: ArgSpec, raw: string): unknown => {
  const kind = kindOf(spec)
  if (kind === 'number') {
    const n = Number(raw)
    if (Number.isNaN(n)) throw new Error(`${spec.label} must be a number`)
    return n
  }
  if (kind === 'json') {
    try {
      return JSON.parse(raw)
    } catch {
      // A bare word is what someone typing a value actually means most of the time,
      // and `"a"` is a nuisance to type on a phone keyboard.
      return raw
    }
  }
  return raw
}

export function ArgSheet({
  toolId,
  args,
}: {
  toolId: string
  args: Record<string, unknown>
}): React.JSX.Element | null {
  const tool = toolById(toolId)
  const specs = tool ? argsOf(tool) : []
  const [raw, setRaw] = useState<Record<string, string>>(() =>
    Object.fromEntries(
      specs.map((spec) => {
        const given = args[spec.name]
        return [spec.name, given == null ? '' : typeof given === 'string' ? given : JSON.stringify(given)]
      }),
    ),
  )
  const [error, setError] = useState<string | null>(null)

  if (!tool) return null

  const submit = (): void => {
    const values: Record<string, unknown> = {}
    try {
      for (const spec of specs) {
        const text = raw[spec.name] ?? ''
        if (text === '') {
          if (!spec.optional) throw new Error(`${spec.label} is required`)
          continue
        }
        values[spec.name] = parse(spec, text)
      }
    } catch (e) {
      setError(e instanceof Error ? e.message : String(e))
      return
    }
    closeSheet()
    dispatch(toolId, values)
  }

  return (
    <Sheet
      title={tool.label}
      onClose={closeSheet}
      footer={
        <Button tone="primary" block onClick={submit}>
          {tool.label}
        </Button>
      }
    >
      <div className="flex flex-col gap-3 pt-1 pb-2">
        {specs.map((spec) => (
          <Field key={spec.name} label={spec.label}>
            {kindOf(spec) === 'select' ? (
              <Select
                value={raw[spec.name] ?? ''}
                onChange={(e) => setRaw((r) => ({ ...r, [spec.name]: e.target.value }))}
              >
                <option value="">—</option>
                {(spec.options ?? []).map((option) => (
                  <option key={option} value={option}>
                    {option}
                  </option>
                ))}
              </Select>
            ) : kindOf(spec) === 'text' ? (
              <TextArea
                value={raw[spec.name] ?? ''}
                placeholder={spec.placeholder}
                onChange={(e) => setRaw((r) => ({ ...r, [spec.name]: e.target.value }))}
              />
            ) : (
              <Input
                value={raw[spec.name] ?? ''}
                placeholder={spec.placeholder}
                inputMode={kindOf(spec) === 'number' ? 'numeric' : undefined}
                autoCapitalize={kindOf(spec) === 'entity' ? 'none' : 'sentences'}
                autoCorrect={kindOf(spec) === 'entity' ? 'off' : undefined}
                onChange={(e) => setRaw((r) => ({ ...r, [spec.name]: e.target.value }))}
              />
            )}
          </Field>
        ))}
        {error && <p className="text-[13px] text-error-600">{error}</p>}
      </div>
    </Sheet>
  )
}
