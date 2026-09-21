import { useEffect, useRef } from 'react'
import type { ReactNode } from 'react'
import type { OverlayItem } from '../overlay'
import { keyLabel, tools } from '../tools'
import type { Tool } from '../tools'
import type { OverlayModel } from '../hooks'
import { Key } from './primitives'

interface OverlayProps {
  overlay: OverlayModel | null
  onQuery(query: string): void
  onIndex(index: number): void
  onChoose(item: OverlayItem): void
  onClose(): void
}

export function Overlay({ overlay, onQuery, onIndex, onChoose, onClose }: OverlayProps): ReactNode {
  const input = useRef<HTMLInputElement>(null)

  useEffect(() => {
    input.current?.focus()
  }, [overlay?.kind])

  if (!overlay) return null

  return (
    <div className="fixed inset-0 z-40 flex items-start justify-center bg-black/25 pt-[14vh]" onMouseDown={onClose}>
      <div
        onMouseDown={(event) => event.stopPropagation()}
        style={{ boxShadow: 'var(--shadow)' }}
        className="w-[560px] max-w-[92vw] overflow-hidden rounded-xl border border-line-strong bg-panel"
      >
        <div className="flex items-center gap-2 border-b border-line px-3.5 py-2.5">
          <span className="shrink-0 text-[11.5px] text-faint">{overlay.title}</span>
          {overlay.kind !== 'help' && (
            <input
              ref={input}
              value={overlay.query}
              placeholder={overlay.placeholder}
              onChange={(event) => onQuery(event.target.value)}
              className="min-w-0 flex-1 text-[13.5px] placeholder:text-faint"
            />
          )}
        </div>

        {overlay.kind === 'help' ? (
          <Help />
        ) : (
          <div className="scroller max-h-[46vh] overflow-y-auto py-1">
            {overlay.items.length === 0 ? (
              <div className="px-3.5 py-3 text-[12.5px] text-faint">nothing matches</div>
            ) : (
              overlay.items.map((item, index) => (
                <button
                  key={item.id}
                  onMouseEnter={() => onIndex(index)}
                  onClick={() => onChoose(item)}
                  className={`flex w-full items-center gap-2 px-3.5 py-[7px] text-left ${
                    index === overlay.index ? 'bg-accent-soft' : ''
                  }`}
                >
                  <span className="min-w-0 flex-1 truncate text-[13px]">{item.title}</span>
                  {item.hint && <span className="shrink-0 text-[11px] text-faint">{item.hint}</span>}
                  {item.key && <Key>{item.key}</Key>}
                </button>
              ))
            )}
          </div>
        )}
      </div>
    </div>
  )
}

const sections: Tool['section'][] = ['Move around', 'Do something', 'Put it down', 'The app']

function Help(): ReactNode {
  return (
    <div className="scroller max-h-[60vh] overflow-y-auto px-3.5 py-3">
      {sections.map((section) => (
        <div key={section} className="mb-4 last:mb-0">
          <div className="mb-1.5 text-[10.5px] font-semibold tracking-wide text-faint uppercase">{section}</div>
          {tools
            .filter((tool) => tool.section === section && tool.scope !== 'overlay' && tool.bindings.length > 0)
            .map((tool) => (
              <div key={tool.id} className="flex items-baseline gap-2 py-[3px]">
                <span className="flex w-[74px] shrink-0 gap-1">
                  {tool.bindings.slice(0, 2).map((binding, index) => (
                    <Key key={index}>{keyLabel(binding)}</Key>
                  ))}
                </span>
                <span className="text-[12.5px]">{tool.title}</span>
                {tool.hint && <span className="text-[11px] text-faint">{tool.hint}</span>}
              </div>
            ))}
        </div>
      ))}
    </div>
  )
}
