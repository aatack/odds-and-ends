import { ExternalLink } from 'lucide-react'
import type { ItemInfo } from '../../shared/types'

/**
 * A link's focus: the page itself, loaded in a frame the whole width of the view, under a line with its address (which
 * opens it in the browser). The app drops the headers that would forbid framing it (main/index.ts); a page that needs
 * a login the app doesn't have shows as it would to someone signed out.
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
      <iframe key={url} src={url} title={String(info.params.title ?? url)} sandbox="allow-scripts allow-same-origin allow-forms allow-popups" />
    </div>
  )
}
