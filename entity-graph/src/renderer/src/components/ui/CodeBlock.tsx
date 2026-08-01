import React, { useEffect, useState } from 'react'
import { Highlight, themes } from 'prism-react-renderer'
import type { CodeRunState } from '../../helpers/codeRunner'
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

// The three things that make a code surface look like one. Pulled out as
// constants because the editable counterpart (`CodeEditor`) lays a textarea
// *over* the highlighted text, and the two only line up if they are set in the
// same type at the same padding — one place to change, so they can't drift.

/** The faint ground code sits on. */
export const CODE_GROUND = 'rounded-md bg-gray-100 shadow-xs'
/** The type it is set in. */
export const CODE_TYPE = 'font-mono text-[11.5px] leading-[1.45]'
/** The inset between the ground and the first glyph. */
export const CODE_PAD = 'px-2.5 py-1.5'

/**
 * Highlighted source, and nothing else — no ground, no padding of its own beyond
 * {@link CODE_PAD}. `wrap` is what the editor needs: long lines fold instead of
 * running off the side, which is how a textarea behaves and therefore the only
 * way the two layers can agree on where the lines are.
 */
export function CodeText({
  code,
  language = 'tsx',
  wrap = false,
}: {
  code: string
  language?: string
  wrap?: boolean
}): React.JSX.Element {
  const dark = useIsDark()
  return (
    <Highlight code={code} language={language} theme={dark ? themes.vsDark : themes.vsLight}>
      {({ tokens, getLineProps, getTokenProps }) => (
        <pre
          className={cn(
            CODE_TYPE,
            CODE_PAD,
            // Wrapping: the same rules a textarea applies to itself. Not
            // wrapping: as wide as the widest line, for a parent that scrolls.
            wrap ? 'whitespace-pre-wrap break-words' : 'w-max min-w-full',
          )}
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
  )
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
  /** Prism language. Defaults to TypeScript — what a `type: code` entity is. */
  language?: string
}

/**
 * A `type: code` entity's body: TypeScript source on a faint grey ground in Fira
 * Code, with its (non-persisted) run output pinned below a hairline. Wide lines
 * scroll horizontally inside the block rather than wrapping or widening the row.
 *
 * Also the surface a fenced block inside markdown renders on, minus the run
 * output — a fence and a code entity should look like the same thing.
 */
export function CodeBlock({ code, run, language = 'tsx' }: CodeBlockProps): React.JSX.Element {
  const output = run ? outputLines(run) : null

  return (
    <div className={cn('my-px w-full min-w-0 overflow-hidden', CODE_GROUND)}>
      <div className="overflow-x-auto">
        <CodeText code={code} language={language} />
      </div>

      {output && (
        <>
          <div className="border-t border-gray-200" />
          <div className="overflow-x-auto">
            <pre
              className={cn(
                'w-max min-w-full whitespace-pre',
                CODE_TYPE,
                CODE_PAD,
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
