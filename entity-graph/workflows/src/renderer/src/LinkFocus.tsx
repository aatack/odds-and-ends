import { ExternalLink } from 'lucide-react'
import type { ItemInfo } from '../../shared/types'

/**
 * A link's focus: the page itself, the whole width of the view, under a line with its address (which opens it in the
 * browser). In the app, a webview: the page top-level in a session of the links' own, kept on disk, so signing in
 * (Google, GitHub) works and lasts (main/index.ts). On the phone, a frame: a page that forbids being framed is blank.
 */
export function LinkFocus({ info }: { info: ItemInfo }) {
  const url = String(info.params.url ?? info.id)
  return (
    <div className="link-focus">
      <div className="link-focus-bar">
        <a href={url} target="_blank" rel="noreferrer" title="Open in the browser">
          {url}
        </a>
        <ExternalLink className="icon" style={{ width: 13, height: 13, flex: 'none' }} />
      </div>
      {window.workflows ? (
        <webview key={url} src={url} partition="persist:links" allowpopups={'true' as unknown as boolean} title={String(info.params.title ?? url)} />
      ) : (
        <iframe key={url} src={url} title={String(info.params.title ?? url)} sandbox="allow-scripts allow-same-origin allow-forms allow-popups" />
      )}
    </div>
  )
}
