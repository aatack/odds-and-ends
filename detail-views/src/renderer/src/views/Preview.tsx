import type { Preview as PreviewState } from '../state.ts'

const width = 720
const height = 480
const gap = 6

/** Below the link if it fits, else above; kept inside the window. */
function place(anchor: PreviewState['anchor']): { left: number; top: number } {
  const below = anchor.y + anchor.height + gap
  const top = below + height <= window.innerHeight ? below : Math.max(gap, anchor.y - gap - height)
  const left = Math.min(Math.max(gap, anchor.x), window.innerWidth - width - gap)
  return { left, top }
}

/** A live, interactive page beside a hovered link. */
export function Preview(props: {
  preview: PreviewState
  onEnter(): void
  onLeave(): void
  onOpen(url: string): void
}) {
  const { url } = props.preview
  return (
    <div className="preview" style={{ ...place(props.preview.anchor), width, height }} onMouseEnter={props.onEnter} onMouseLeave={props.onLeave}>
      <div className="preview-bar">
        <span className="grow">{url}</span>
        <button onClick={() => props.onOpen(url)}>Open</button>
      </div>
      <webview key={url} src={url} partition="persist:preview" className="preview-page" />
    </div>
  )
}
