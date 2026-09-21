import type { ReactNode } from 'react'

export function Key({ children }: { children: ReactNode }): ReactNode {
  return <span className="key">{children}</span>
}

/** Status reads as a dot and a word, never as a filled or outlined badge. */
export function Status({
  children,
  color,
  title,
}: {
  children: ReactNode
  color?: string
  title?: string
}): ReactNode {
  return (
    <span title={title} className="inline-flex items-center gap-1.5 text-[11.5px] text-muted">
      {color && <Dot color={color} />}
      {children}
    </span>
  )
}

export function Separator(): ReactNode {
  return <span className="text-faint">·</span>
}

export function Dot({ color }: { color: string }): ReactNode {
  return <span className="inline-block size-[6px] shrink-0 rounded-full" style={{ background: color }} />
}

export function Empty({ title, children }: { title: string; children?: ReactNode }): ReactNode {
  return (
    <div className="flex h-full flex-col items-center justify-center gap-2 px-8 text-center">
      <div className="text-[15px] font-medium">{title}</div>
      <div className="max-w-[420px] text-[12.5px] text-muted">{children}</div>
    </div>
  )
}
