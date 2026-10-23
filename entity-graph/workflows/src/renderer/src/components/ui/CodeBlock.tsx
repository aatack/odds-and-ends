import React from 'react'
import { Highlight, themes } from 'prism-react-renderer'
import { cn } from '../../helpers/cn'

// Highlighted source on a faint grey ground in Fira Code, as entity-graph draws
// it. Wide lines scroll inside the block rather than widening what holds it.

export function CodeBlock({ code, language = 'tsx' }: { code: string; language?: string }): React.JSX.Element {
  return (
    <div className="my-px w-full min-w-0 overflow-hidden rounded-md bg-gray-100 shadow-xs">
      <div className="overflow-x-auto">
        <Highlight code={code} language={language} theme={themes.vsLight}>
          {({ tokens, getLineProps, getTokenProps }) => (
            <pre
              className={cn('font-mono text-[11.5px] leading-[1.45]', 'px-2.5 py-1.5', 'w-max min-w-full')}
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
    </div>
  )
}
