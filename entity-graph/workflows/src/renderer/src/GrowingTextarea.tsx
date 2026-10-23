import { forwardRef, useImperativeHandle, useLayoutEffect, useRef, type TextareaHTMLAttributes } from 'react'

/** Most lines a text box grows to before it scrolls. */
const MAX_LINES = 8

/** A text box one line tall that grows with its text, up to MAX_LINES, with no handle to drag. */
export const GrowingTextarea = forwardRef<HTMLTextAreaElement, TextareaHTMLAttributes<HTMLTextAreaElement>>(function GrowingTextarea(props, outer) {
  const ref = useRef<HTMLTextAreaElement>(null)
  useImperativeHandle(outer, () => ref.current!)
  useLayoutEffect(() => {
    const el = ref.current
    if (!el) return
    const style = getComputedStyle(el)
    const line = parseFloat(style.lineHeight) || 20
    const pad = parseFloat(style.paddingTop) + parseFloat(style.paddingBottom)
    el.style.height = 'auto'
    el.style.height = `${Math.min(el.scrollHeight, line * MAX_LINES + pad)}px`
  }, [props.value])
  return <textarea {...props} ref={ref} rows={1} className={`growing ${props.className ?? ''}`} />
})
