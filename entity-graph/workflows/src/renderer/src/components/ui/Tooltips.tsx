import { useEffect, useLayoutEffect, useRef, useState } from 'react'
import { createPortal } from 'react-dom'

// The one tooltip for the app, drawn as its popovers are, in place of the
// browser's own. Mounted once: any element's `title` is its tip, so components
// keep writing `title=` as they always have. On hover the title moves to
// `data-tip` (so the browser's never shows); React sets `title` again only when
// it changes, and the next hover moves that one too. It shows after a short
// delay, beside the element (below it, or above where there is no room), and
// goes on leaving, pressing, a key, scrolling or a blur.

const DELAY = 450
const GAP = 6
const EDGE = 8

type Tip = { text: string; box: DOMRect }

function tipOf(el: Element): string | null {
  const title = el.getAttribute('title')
  if (title) {
    el.setAttribute('data-tip', title)
    el.removeAttribute('title')
    if (!el.hasAttribute('aria-label') && !el.textContent?.trim()) el.setAttribute('aria-label', title)
  }
  return el.getAttribute('data-tip')
}

export function Tooltips(): React.JSX.Element | null {
  const [tip, setTip] = useState<Tip | null>(null)
  const [place, setPlace] = useState<React.CSSProperties | null>(null)
  const ref = useRef<HTMLDivElement>(null)

  useEffect(() => {
    let current: Element | null = null
    let timer: ReturnType<typeof setTimeout> | undefined
    const hide = (): void => {
      clearTimeout(timer)
      current = null
      setTip(null)
    }
    const onOver = (e: PointerEvent): void => {
      if (e.pointerType === 'touch') return
      const target = e.target instanceof Element ? e.target.closest('[title], [data-tip]') : null
      if (target === current) return
      hide()
      // An iframe's title names it for a screen reader; it is not a tip.
      if (!target || target instanceof HTMLIFrameElement) return
      const text = tipOf(target)
      if (!text) return
      current = target
      timer = setTimeout(() => {
        if (current === target && target.isConnected) setTip({ text, box: target.getBoundingClientRect() })
      }, DELAY)
    }
    const onOut = (e: PointerEvent): void => {
      if (current && !(e.relatedTarget instanceof Node && current.contains(e.relatedTarget))) hide()
    }
    document.addEventListener('pointerover', onOver)
    document.addEventListener('pointerout', onOut)
    document.addEventListener('pointerdown', hide, true)
    document.addEventListener('keydown', hide, true)
    document.addEventListener('scroll', hide, true)
    window.addEventListener('blur', hide)
    return () => {
      clearTimeout(timer)
      document.removeEventListener('pointerover', onOver)
      document.removeEventListener('pointerout', onOut)
      document.removeEventListener('pointerdown', hide, true)
      document.removeEventListener('keydown', hide, true)
      document.removeEventListener('scroll', hide, true)
      window.removeEventListener('blur', hide)
    }
  }, [])

  // Placed before paint, once its size is known: centred under the element, kept on screen.
  useLayoutEffect(() => {
    if (!tip || !ref.current) {
      setPlace(null)
      return
    }
    const { width, height } = ref.current.getBoundingClientRect()
    const { box } = tip
    const below = box.bottom + GAP + height <= window.innerHeight - EDGE
    const left = Math.min(Math.max(EDGE, box.left + box.width / 2 - width / 2), window.innerWidth - EDGE - width)
    setPlace({ left, top: below ? box.bottom + GAP : Math.max(EDGE, box.top - GAP - height) })
  }, [tip])

  if (!tip) return null
  return createPortal(
    <div ref={ref} role="tooltip" className="tooltip popover" style={place ?? { left: -9999, top: -9999 }}>
      {tip.text}
    </div>,
    document.body,
  )
}
