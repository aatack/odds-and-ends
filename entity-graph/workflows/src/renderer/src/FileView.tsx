// A file item, at its two sizes: its card in the chat (an image inline, anything else as its name), and on its own
// (an image to zoom and pan; text, edited in place when it is a file on this machine; a PDF or Word document as it
// looks; with a switch between the text and how it looks for what is both, such as markdown). Its bytes come
// through the backend (fileData, fileText), as a blob here, so the phone shows them as the window does.

import { Download, FileText, Pencil, RefreshCw, ZoomIn, ZoomOut, Maximize } from 'lucide-react'
import { lazy, Suspense, useCallback, useEffect, useState } from 'react'
import { TransformComponent, TransformWrapper } from 'react-zoom-pan-pinch'
import type { ItemInfo } from '../../shared/types'
import { CodeBlock } from './components/ui/CodeBlock'
import { Markdown } from './components/ui/Markdown'
import { api } from './useWorkflows'

const PdfPreview = lazy(() => import('./FilePreviews').then((m) => ({ default: m.PdfPreview })))
const DocxPreview = lazy(() => import('./FilePreviews').then((m) => ({ default: m.DocxPreview })))

type Kind = 'image' | 'pdf' | 'docx' | 'video' | 'audio' | 'other'

const ext = (name: string) => /\.([^./]+)$/.exec(name)?.[1]?.toLowerCase() ?? ''

/** What it is shown as, from its type and name. */
function kindOf(info: ItemInfo): Kind {
  const mime = String(info.params.mime ?? '')
  if (mime.startsWith('image/') && mime !== 'image/svg+xml') return 'image'
  if (mime === 'application/pdf') return 'pdf'
  if (ext(String(info.params.name ?? '')) === 'docx') return 'docx'
  if (mime.startsWith('video/')) return 'video'
  if (mime.startsWith('audio/')) return 'audio'
  return 'other'
}

/** Text that also has a look of its own: the switch between them is offered. */
const PREVIEWS: Record<string, 'markdown' | 'html' | 'svg' | 'csv'> = { md: 'markdown', markdown: 'markdown', html: 'html', htm: 'html', svg: 'svg', csv: 'csv' }

/** Prism's name for a file's language, from its name; plain text when it knows none. */
const LANG: Record<string, string> = {
  ts: 'tsx', tsx: 'tsx', mts: 'tsx', cts: 'tsx', js: 'jsx', jsx: 'jsx', mjs: 'jsx', cjs: 'jsx', json: 'json', py: 'python', rs: 'rust', go: 'go',
  css: 'css', scss: 'css', html: 'markup', htm: 'markup', xml: 'markup', svg: 'markup', md: 'markdown', markdown: 'markdown', yml: 'yaml', yaml: 'yaml',
  sh: 'bash', bash: 'bash', sql: 'sql', c: 'c', h: 'c', cpp: 'cpp', java: 'java', kt: 'kotlin', swift: 'swift', rb: 'ruby',
}

export const fileSize = (n: unknown): string => {
  const b = Number(n)
  if (!Number.isFinite(b) || n === undefined || n === null) return ''
  return b < 1024 ? `${b} B` : b < 1024 ** 2 ? `${(b / 1024).toFixed(1)} KB` : `${(b / 1024 ** 2).toFixed(1)} MB`
}

/** The file's bytes as a blob URL, while shown; `again` fetches them afresh. */
function useBlob(info: ItemInfo, on = true): { url: string | null; error: string | null; again: () => void } {
  const [url, setUrl] = useState<string | null>(null)
  const [error, setError] = useState<string | null>(null)
  const [n, setN] = useState(0)
  useEffect(() => {
    if (!on) return
    let gone = false
    let made: string | null = null
    setError(null)
    api()
      .fileData(info.id)
      .then(({ mime, base64 }) => {
        if (gone) return
        const bytes = Uint8Array.from(atob(base64), (c) => c.charCodeAt(0))
        made = URL.createObjectURL(new Blob([bytes], { type: mime }))
        setUrl(made)
      })
      .catch((e: Error) => !gone && setError(e.message))
    return () => {
      gone = true
      if (made) URL.revokeObjectURL(made)
    }
  }, [info.id, on, n])
  return { url, error, again: useCallback(() => setN((x) => x + 1), []) }
}

/** In the chat: an image as itself; anything else as its name and size, which opens it. */
export function FileCard({ info, onOpen }: { info: ItemInfo; onOpen: (id: string) => void }) {
  const image = kindOf(info) === 'image' || String(info.params.mime) === 'image/svg+xml'
  const { url, error } = useBlob(info, image)
  const name = String(info.params.name ?? 'file')
  return (
    <div className="file-card">
      {image && url ? (
        <button data-ui className="file-thumb" onClick={() => onOpen(info.id)} title={name}>
          <img src={url} alt={name} />
        </button>
      ) : (
        <button data-ui className="link file-chip" onClick={() => onOpen(info.id)}>
          <FileText className="icon" />
          <span className="file-name">{name}</span>
          <span className="muted">{fileSize(info.params.size)}</span>
        </button>
      )}
      {info.params.title ? <Markdown text={String(info.params.title)} /> : null}
      {error && <span className="error-text">{error}</span>}
    </div>
  )
}

/** On its own, filling the view. */
export function FileFocus({ info, mobile = false }: { info: ItemInfo; mobile?: boolean }) {
  const kind = kindOf(info)
  const name = String(info.params.name ?? 'file')
  const [text, setText] = useState<Awaited<ReturnType<ReturnType<typeof api>['fileText']>> | null>(null)
  const [error, setError] = useState<string | null>(null)
  const [n, setN] = useState(0)
  useEffect(() => {
    let gone = false
    api()
      .fileText(info.id)
      .then((t) => !gone && setText(t))
      .catch((e: Error) => !gone && setError(e.message))
    return () => {
      gone = true
    }
  }, [info.id, n])
  const isText = text?.text !== null && text?.text !== undefined
  const preview = PREVIEWS[ext(name)]
  const [plain, setPlain] = useState(false)
  // Its bytes: for what is drawn from them, and for Download.
  const blob = useBlob(info, !!text && !text.missing)
  const again = () => {
    setN((x) => x + 1)
    blob.again()
  }

  const where = text?.path ?? (info.params.hash ? 'kept with the app' : '')
  return (
    <div className={`file-focus ${mobile ? 'mobile' : ''}`}>
      <div className="file-bar">
        <span className="muted file-where" title={where}>
          {where}
          {text?.size !== null && text?.size !== undefined ? ` · ${fileSize(text.size)}` : ''}
        </span>
        {isText && preview && (
          <div className="segmented">
            <button data-ui className={!plain ? 'on' : ''} onClick={() => setPlain(false)}>
              Preview
            </button>
            <button data-ui className={plain ? 'on' : ''} onClick={() => setPlain(true)}>
              Text
            </button>
          </div>
        )}
        <button data-ui className="link" title="Read it again" onClick={again}>
          <RefreshCw className="icon" />
        </button>
        {blob.url && (
          <a className="link" href={blob.url} download={name} title="Download">
            <Download className="icon" />
          </a>
        )}
      </div>
      {(error || blob.error) && <div className="error">{error ?? blob.error}</div>}
      {text?.missing && <div className="error">The file is not there any more.</div>}
      <div className="file-body">
        {!text ? (
          <p className="muted">Reading…</p>
        ) : isText && (plain || !preview) ? (
          <TextFile itemId={info.id} name={name} text={text.text!} editable={text.editable} onSaved={again} />
        ) : isText && preview ? (
          <TextPreview kind={preview} text={text.text!} url={blob.url} />
        ) : kind === 'image' ? (
          blob.url && <ZoomImage url={blob.url} name={name} />
        ) : kind === 'pdf' ? (
          blob.url && (
            <Suspense fallback={<p className="muted">Opening…</p>}>
              <PdfPreview url={blob.url} />
            </Suspense>
          )
        ) : kind === 'docx' ? (
          blob.url && (
            <Suspense fallback={<p className="muted">Opening…</p>}>
              <DocxPreview url={blob.url} />
            </Suspense>
          )
        ) : kind === 'video' ? (
          blob.url && <video className="file-media" src={blob.url} controls />
        ) : kind === 'audio' ? (
          blob.url && <audio src={blob.url} controls />
        ) : (
          <p className="muted">There is no preview of {name}: download it to open it.</p>
        )}
      </div>
    </div>
  )
}

/** An image to zoom (the wheel, a pinch, the buttons) and pan (dragging); double-click zooms in. */
function ZoomImage({ url, name }: { url: string; name: string }) {
  return (
    <TransformWrapper minScale={0.1} maxScale={40} centerOnInit limitToBounds={false} doubleClick={{ step: 0.7 }} wheel={{ step: 0.15 }}>
      {({ zoomIn, zoomOut, resetTransform }) => (
        <div className="zoom">
          <div className="zoom-buttons">
            <button data-ui title="Zoom in" onClick={() => zoomIn()}>
              <ZoomIn className="icon" />
            </button>
            <button data-ui title="Zoom out" onClick={() => zoomOut()}>
              <ZoomOut className="icon" />
            </button>
            <button data-ui title="Fit" onClick={() => resetTransform()}>
              <Maximize className="icon" />
            </button>
          </div>
          <TransformComponent wrapperClass="zoom-area" contentClass="zoom-content">
            <img src={url} alt={name} className="zoom-img" />
          </TransformComponent>
        </div>
      )}
    </TransformWrapper>
  )
}

/** Text as text, highlighted; a file on this machine can be edited and saved over. */
function TextFile({ itemId, name, text, editable, onSaved }: { itemId: string; name: string; text: string; editable: boolean; onSaved: () => void }) {
  const [draft, setDraft] = useState<string | null>(null)
  const [error, setError] = useState<string | null>(null)
  const save = async () => {
    setError(null)
    try {
      await api().saveFileText(itemId, draft!)
      setDraft(null)
      onSaved()
    } catch (e) {
      setError((e as Error).message)
    }
  }
  if (draft !== null)
    return (
      <div className="file-text">
        <div className="file-edit-bar">
          <button data-ui className="primary" onClick={() => void save()}>
            Save
          </button>
          <button data-ui onClick={() => setDraft(null)}>
            Cancel
          </button>
          {error && <span className="error-text">{error}</span>}
        </div>
        <textarea
          className="file-editor"
          value={draft}
          spellCheck={false}
          autoFocus
          onChange={(e) => setDraft(e.target.value)}
          onKeyDown={(e) => {
            if ((e.ctrlKey || e.metaKey) && e.key === 's') {
              e.preventDefault()
              void save()
            }
            if (e.key === 'Escape') setDraft(null)
          }}
        />
      </div>
    )
  return (
    <div className="file-text">
      {editable && (
        <div className="file-edit-bar">
          <button data-ui onClick={() => setDraft(text)} title="Edit the file on this machine">
            <Pencil className="icon" /> Edit
          </button>
        </div>
      )}
      <CodeBlock code={text} language={LANG[ext(name)] ?? 'text'} />
    </div>
  )
}

/** How text looks: markdown drawn, HTML in a frame that runs nothing, an SVG as an image, a CSV as a table. */
function TextPreview({ kind, text, url }: { kind: 'markdown' | 'html' | 'svg' | 'csv'; text: string; url: string | null }) {
  if (kind === 'markdown') return <div className="file-markdown"><Markdown text={text} /></div>
  if (kind === 'html') return <iframe className="file-html" sandbox="" srcDoc={text} title="Preview" />
  if (kind === 'svg') return url ? <ZoomImage url={url} name="SVG" /> : null
  const rows = parseCsv(text)
  return (
    <div className="file-csv">
      <table className="params">
        <tbody>
          {rows.slice(0, 2000).map((r, i) => (
            <tr key={i}>
              {r.map((c, j) => (i === 0 ? <th key={j}>{c}</th> : <td key={j}>{c}</td>))}
            </tr>
          ))}
        </tbody>
      </table>
      {rows.length > 2000 && <p className="muted">The first 2000 rows of {rows.length}.</p>}
    </div>
  )
}

/** CSV's rows: quoted fields may hold commas, newlines and doubled quotes. */
export function parseCsv(text: string): string[][] {
  const rows: string[][] = []
  let row: string[] = []
  let field = ''
  let quoted = false
  for (let i = 0; i < text.length; i++) {
    const c = text[i]
    if (quoted) {
      if (c === '"' && text[i + 1] === '"') (field += '"'), i++
      else if (c === '"') quoted = false
      else field += c
    } else if (c === '"') quoted = true
    else if (c === ',') row.push(field), (field = '')
    else if (c === '\n' || c === '\r') {
      if (c === '\r' && text[i + 1] === '\n') i++
      row.push(field), rows.push(row), (row = []), (field = '')
    } else field += c
  }
  if (field || row.length) row.push(field), rows.push(row)
  return rows
}

/** Reads files given to the window (dropped or pasted) and keeps each as a file item in the task. */
export async function uploadFiles(itemId: string, files: File[]): Promise<void> {
  for (const f of files) {
    const bytes = new Uint8Array(await f.arrayBuffer())
    let bin = ''
    for (let i = 0; i < bytes.length; i += 0x8000) bin += String.fromCharCode(...bytes.subarray(i, i + 0x8000))
    await api().uploadFile(itemId, f.name || 'pasted', btoa(bin))
  }
}
