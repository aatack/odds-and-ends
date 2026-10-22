import React, { useEffect, useState } from 'react'
import { Highlight, themes } from 'prism-react-renderer'
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

// The three things that make a code surface look like one.

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

export interface CodeBlockProps {
  code: string
  /** Prism language. Defaults to TypeScript. */
  language?: string
}
/**
 * A fenced block inside markdown: source on a faint grey ground in Fira Code.
 * Wide lines scroll horizontally inside the block rather than wrapping or
 * widening the row.
 */
export function CodeBlock({ code, language = 'tsx' }: CodeBlockProps): React.JSX.Element {
  return (
    <div className={cn('my-px w-full min-w-0 overflow-hidden', CODE_GROUND)}>
      <div className="overflow-x-auto">
        <CodeText code={code} language={language} />
      </div>
    </div>
  )
}
