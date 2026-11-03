// The heavier previews, loaded only when one is opened: a PDF (pdf.js, through react-pdf) and a Word document
// (docx-preview). Both read the file's blob URL.

import { renderAsync } from 'docx-preview'
import { useEffect, useRef, useState } from 'react'
import { Document, Page, pdfjs } from 'react-pdf'
import 'react-pdf/dist/Page/TextLayer.css'
import 'react-pdf/dist/Page/AnnotationLayer.css'

pdfjs.GlobalWorkerOptions.workerSrc = new URL('pdfjs-dist/build/pdf.worker.min.mjs', import.meta.url).toString()

/** Every page, the width of the view (up to 900px), the text selectable. */
export function PdfPreview({ url }: { url: string }) {
  const [pages, setPages] = useState(0)
  const box = useRef<HTMLDivElement>(null)
  const [width, setWidth] = useState(800)
  useEffect(() => {
    const el = box.current
    if (!el) return
    const seen = new ResizeObserver(() => setWidth(Math.min(900, el.clientWidth - 16)))
    seen.observe(el)
    return () => seen.disconnect()
  }, [])
  return (
    <div className="file-pdf" ref={box}>
      <Document file={url} onLoadSuccess={(d) => setPages(d.numPages)} loading={<p className="muted">Opening…</p>} error={<p className="error-text">It could not be read as a PDF.</p>}>
        {Array.from({ length: pages }, (_, i) => (
          <Page key={i} pageNumber={i + 1} width={width} className="file-pdf-page" />
        ))}
      </Document>
    </div>
  )
}

/** A Word document (.docx), laid out as its pages. */
export function DocxPreview({ url }: { url: string }) {
  const box = useRef<HTMLDivElement>(null)
  const [error, setError] = useState<string | null>(null)
  useEffect(() => {
    let gone = false
    void (async () => {
      try {
        const data = await (await fetch(url)).blob()
        if (!gone && box.current) await renderAsync(data, box.current, undefined, { inWrapper: true, ignoreLastRenderedPageBreak: true })
      } catch (e) {
        if (!gone) setError(`It could not be read as a Word document: ${(e as Error).message}`)
      }
    })()
    return () => {
      gone = true
      if (box.current) box.current.innerHTML = ''
    }
  }, [url])
  return (
    <div className="file-docx">
      {error && <p className="error-text">{error}</p>}
      <div ref={box} />
    </div>
  )
}
