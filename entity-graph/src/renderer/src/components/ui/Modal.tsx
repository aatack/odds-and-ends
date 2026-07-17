import type { ReactNode } from 'react'
import { X } from '@untitledui/icons'
import { IconButton } from './IconButton'

// A centred overlay with a click-away backdrop and a soft-edged card. The one
// dialog shell for the whole app, so every modal reads the same.
export function Modal({
  title,
  onClose,
  wide = false,
  children,
}: {
  title: ReactNode
  onClose: () => void
  wide?: boolean
  children: ReactNode
}): React.JSX.Element {
  return (
    <div
      className="fixed inset-0 z-50 flex items-start justify-center overflow-y-auto bg-gray-950/30 p-6"
      onClick={onClose}
    >
      <div
        className={`my-6 w-full ${wide ? 'max-w-2xl' : 'max-w-md'} rounded-xl bg-white shadow-lg`}
        onClick={(e) => e.stopPropagation()}
      >
        <div className="flex items-center justify-between px-5 py-3.5">
          <p className="text-[13px] font-semibold text-gray-900">{title}</p>
          <IconButton title="Close" onClick={onClose}>
            <X size={16} />
          </IconButton>
        </div>
        <div className="space-y-4 px-5 pb-5">{children}</div>
      </div>
    </div>
  )
}
