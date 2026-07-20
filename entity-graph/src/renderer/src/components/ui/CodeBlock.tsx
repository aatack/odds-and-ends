import React, { useEffect, useState } from 'react'
import { Highlight, themes } from 'prism-react-renderer'
import type { CodeRunState } from '../../views/useCodeRunner'
import { cn } from '../../helpers/cn'

// Track the `.dark` class on <html> so the syntax theme flips with the app's
// theme toggle. Cheap and reactive — one observer per mounted block.
function useIsDark(): boolean {
  const [dark, setDark] = useState(() => document.documentElement.classList.contains('dark'))
  useEffect(() => {
    const el = document.documentElement
    const obs = new MutationObserver(() => setDark(el.classList.contains('dark')))
    obs.observe(el, { attributes: true, attributeFilter: ['class'] })
    return () => obs.disconnect()
  }, [])
  return dark
}

function formatValue(value: unknown): string {
  if (typeof value === 'string') return value
  try {
    return JSON.stringify(value, null, 2) ?? String(value)
  } catch {
    return String(value)
  }
}

function outputLines(run: CodeRunState): { text: string; error: boolean } | null {
  if (run.status === 'running') return { text: 'Running…', error: false }
  if (run.status === 'error') {
    const parts = [...run.logs, run.error]
    return { text: parts.join('\n'), error: true }
  }
  const parts = [...run.logs]
  if (run.hasResult) parts.push(formatValue(run.result))
  if (parts.length === 0) return { text: '(no output)', error: false }
  return { text: parts.join('\n'), error: false }
}

export interface CodeBlockProps {
  code: string
  /** Local run state for this entity, if it has been run this session. */
  run?: CodeRunState
}

/**
 * A `type: code` entity's body: TypeScript source on a faint grey ground in Fira
 * Code, with its (non-persisted) run output pinned below a hairline. Wide lines
 * scroll horizontally inside the block rather than wrapping or widening the row.
 */
export function CodeBlock({ code, run }: CodeBlockProps): React.JSX.Element {
  const dark = useIsDark()
  const output = run ? outputLines(run) : null

  return (
    <div className="my-px w-full min-w-0 overflow-hidden rounded-md bg-gray-100 shadow-xs">
      <div className="overflow-x-auto">
        <Highlight code={code} language="tsx" theme={dark ? themes.vsDark : themes.vsLight}>
          {({ tokens, getLineProps, getTokenProps }) => (
            <pre
              className="w-max min-w-full px-2.5 py-1.5 font-mono text-[11.5px] leading-[1.45]"
              style={{ background: 'transparent', margin: 0 }}
            >
              {tokens.map((line, i) => (
                <div key={i} {...getLineProps({ line })}>
                  {line.map((token, k) => (
                    <span key={k} {...getTokenProps({ token })} />
                  ))}
                </div>
              ))}
            </pre>
          )}
        </Highlight>
      </div>

      {output && (
        <>
          <div className="border-t border-gray-200" />
          <div className="overflow-x-auto">
            <pre
              className={cn(
                'w-max min-w-full whitespace-pre px-2.5 py-1.5 font-mono text-[11.5px] leading-[1.45]',
                output.error ? 'text-red-600' : 'text-gray-500',
              )}
            >
              {output.text}
            </pre>
          </div>
        </>
      )}
    </div>
  )
}
