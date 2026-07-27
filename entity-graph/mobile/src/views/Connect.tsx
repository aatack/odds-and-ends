import React from 'react'
import { ConnectForm } from './ConnectForm'

// The first-run screen. Deliberately the whole screen rather than a sheet: there is
// nothing behind it to see.

export function Connect(): React.JSX.Element {
  return (
    <div className="h-full overflow-y-auto bg-gray-50 pt-[calc(var(--inset-top)+2.5rem)] pb-[calc(var(--inset-bottom)+2rem)]">
      <div className="mx-auto flex max-w-md flex-col gap-6 px-5">
        <div>
          <h1 className="text-[22px] font-semibold tracking-tight text-gray-900">Entity Graph</h1>
          <p className="mt-1.5 text-[14px] leading-relaxed text-gray-500">
            Point this at a source and it will read and write the same graph the desktop app
            does. The details are kept on this phone only.
          </p>
        </div>
        <ConnectForm />
        <p className="text-[12.5px] leading-relaxed text-gray-400">
          A link of the form <code className="font-mono">…/#connect=…</code> fills all of this
          in, which beats typing a token with a thumb — see the README for how to make one.
        </p>
      </div>
    </div>
  )
}
