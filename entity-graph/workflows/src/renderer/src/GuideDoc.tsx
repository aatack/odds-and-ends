import { useAtom } from 'jotai'
import { atomWithStorage } from 'jotai/utils'
import { Check, Copy } from 'lucide-react'
import { useMemo, useState } from 'react'
import { guide, type Guide, type Reader } from '../../shared/paradigm'
import { GoContext, Markdown } from './components/ui/Markdown'

/** Which reader the guides are shown for: kept, and the same on every page that has one. */
const readerAtom = atomWithStorage<Reader>('workflows.home.reader', 'human')

/**
 * A page's guide (shared/paradigm.ts), at its top. *For people*: as markdown, with buttons to the places it names and
 * its keys drawn as keys. *For bots*: the markdown itself, as the `paradigm` tool answers it, to read or copy.
 */
export function GuideDoc({ page, onGo }: { page: Guide; onGo: (place: string) => void }) {
  const [reader, setReader] = useAtom(readerAtom)
  const [copied, setCopied] = useState(false)
  const text = useMemo(() => guide(page, reader), [page, reader])
  const copy = () =>
    void navigator.clipboard.writeText(guide(page, 'agent')).then(() => {
      setCopied(true)
      setTimeout(() => setCopied(false), 1500)
    })
  return (
    <div className="home-doc">
      <div className="home-readers" role="tablist">
        {(['human', 'agent'] as Reader[]).map((r) => (
          <button key={r} data-ui role="tab" aria-selected={r === reader} className={r === reader ? 'chosen' : ''} onClick={() => setReader(r)}>
            {r === 'human' ? 'For people' : 'For bots'}
          </button>
        ))}
        <button data-ui className="home-copy" title="Copy what bots are told: the paradigm tool's answer" onClick={copy}>
          {copied ? <Check className="icon" /> : <Copy className="icon" />}
          {copied ? 'Copied' : 'Copy for bots'}
        </button>
      </div>
      {reader === 'human' ? (
        <GoContext.Provider value={onGo}>
          <Markdown text={text} />
        </GoContext.Provider>
      ) : (
        <pre className="home-source">{text}</pre>
      )}
    </div>
  )
}
