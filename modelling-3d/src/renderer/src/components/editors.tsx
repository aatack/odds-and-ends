/**
 * Editing a literal in place on a node.
 *
 * Every value type that can be typed or dragged has an editor here, and the
 * builder shows one wherever a socket is unconnected — so a constant is edited
 * where it is used rather than in a panel somewhere else. Two of them are
 * direct manipulation rather than fields: a colour is picked off a plane, and a
 * 2D path has its points dragged around a pad.
 */

import { useRef, useState } from 'react'
import type { Colour, Path2, Path3, ValueType, Vec2, Vec3 } from '@core/values'
import { fromHex, fromHsv, toHex, toHsv, vec2 } from '@core/values'
import { NumberField, TextField, cn } from './ui'

export function ValueEditor({
  type,
  value,
  onChange,
}: {
  type: ValueType
  value: unknown
  onChange: (value: unknown) => void
}) {
  switch (type) {
    case 'number':
      return <NumberField value={value as number} onChange={onChange} />
    case 'text':
      return (
        <TextField
          className="nodrag"
          value={(value as string) ?? ''}
          onChange={(event) => onChange(event.target.value)}
        />
      )
    case 'vec2': {
      const v = value as Vec2
      return (
        <div className="flex gap-1">
          <NumberField value={v.x} onChange={(x) => onChange(vec2(x, v.y))} />
          <NumberField value={v.y} onChange={(y) => onChange(vec2(v.x, y))} />
        </div>
      )
    }
    case 'vec3': {
      const v = value as Vec3
      return (
        <div className="flex gap-1">
          <NumberField value={v.x} onChange={(x) => onChange({ ...v, x })} />
          <NumberField value={v.y} onChange={(y) => onChange({ ...v, y })} />
          <NumberField value={v.z} onChange={(z) => onChange({ ...v, z })} />
        </div>
      )
    }
    case 'colour':
      return <ColourField value={value as Colour} onChange={onChange} />
    case 'path2':
      return <PathPad value={value as Path2} onChange={onChange} />
    case 'path3':
      return <Path3Summary value={value as Path3} />
    default:
      return null
  }
}

// ---------------------------------------------------------------------------
// Colour
// ---------------------------------------------------------------------------

/**
 * Where a pointer landed inside an element, as a fraction of it, clamped so a
 * drag that leaves the box still says something sensible.
 */
function fraction(element: Element, event: { clientX: number; clientY: number }): Vec2 {
  const box = element.getBoundingClientRect()
  const clamp = (v: number): number => (v < 0 ? 0 : v > 1 ? 1 : v)
  return vec2(clamp((event.clientX - box.left) / box.width), clamp((event.clientY - box.top) / box.height))
}

/** Follow a pointer until it is let go, reporting where it is each time. */
function trackPointer(
  event: React.PointerEvent,
  element: Element,
  report: (at: Vec2) => void,
): void {
  event.preventDefault()
  event.stopPropagation()
  report(fraction(element, event))
  const target = event.currentTarget as HTMLElement
  target.setPointerCapture(event.pointerId)
  const move = (moved: PointerEvent): void => report(fraction(element, moved))
  const stop = (): void => {
    target.releasePointerCapture(event.pointerId)
    target.removeEventListener('pointermove', move)
    target.removeEventListener('pointerup', stop)
    target.removeEventListener('pointercancel', stop)
  }
  target.addEventListener('pointermove', move)
  target.addEventListener('pointerup', stop)
  target.addEventListener('pointercancel', stop)
}

/**
 * Saturation and value off a plane, hue off a strip, and a hex field for when
 * you know exactly what you want.
 *
 * Not `<input type="color">`: that opens a popup the page cannot see or
 * control — on Linux it is drawn *inside* the window and takes stray clicks,
 * which is how a colour changes itself.
 *
 * The hue is held here as well as in the colour, because black and grey have no
 * hue to read back: dragging the value to zero and up again would otherwise
 * lose which colour you were on.
 */
function ColourField({ value, onChange }: { value: Colour; onChange: (value: Colour) => void }) {
  const [open, setOpen] = useState(false)
  const measured = toHsv(value)
  const [hue, setHue] = useState(measured.h)
  const plane = useRef<HTMLDivElement>(null)
  const strip = useRef<HTMLDivElement>(null)

  // A colour arriving from elsewhere (typed hex, another edit) sets the hue,
  // but only when it actually has one.
  const h = measured.s > 0.001 && measured.v > 0.001 ? measured.h : hue
  const hex = toHex(value)

  return (
    <div className="nodrag">
      <div className="flex items-center gap-1.5">
        <button
          onClick={() => setOpen((was) => !was)}
          style={{ background: hex }}
          className="h-6 w-6 shrink-0 rounded ring-1 ring-line"
          title={open ? 'Close the picker' : 'Pick a colour'}
        />
        <input
          value={hex}
          spellCheck={false}
          onChange={(event) => {
            const text = event.target.value.trim()
            if (/^#?([0-9a-f]{6}|[0-9a-f]{3})$/i.test(text)) {
              const next = fromHex(text)
              onChange(next)
              setHue(toHsv(next).h)
            }
          }}
          className="h-6 w-full min-w-0 rounded bg-sunken px-1.5 font-mono text-[11px] hover:bg-line/70 focus:bg-line/70"
        />
      </div>

      {open && (
        <div className="mt-1.5">
          <div
            ref={plane}
            onPointerDown={(event) =>
              trackPointer(event, plane.current!, (at) =>
                onChange(fromHsv({ h, s: at.x, v: 1 - at.y })),
              )
            }
            className="relative h-24 w-full rounded"
            style={{
              background: `linear-gradient(to top, #000, rgba(0,0,0,0)), linear-gradient(to right, #fff, ${toHex(
                fromHsv({ h, s: 1, v: 1 }),
              )})`,
            }}
          >
            <span
              className="pointer-events-none absolute h-3 w-3 -translate-x-1/2 -translate-y-1/2 rounded-full ring-2 ring-white"
              style={{
                left: `${measured.s * 100}%`,
                top: `${(1 - measured.v) * 100}%`,
                background: hex,
                boxShadow: '0 0 0 1px rgba(0,0,0,0.35)',
              }}
            />
          </div>

          <div
            ref={strip}
            onPointerDown={(event) =>
              trackPointer(event, strip.current!, (at) => {
                const next = at.x * 360
                setHue(next)
                onChange(fromHsv({ h: next, s: Math.max(measured.s, 0.02), v: Math.max(measured.v, 0.02) }))
              })
            }
            className="relative mt-1.5 h-3 w-full rounded"
            style={{
              background:
                'linear-gradient(to right, #f00 0%, #ff0 17%, #0f0 33%, #0ff 50%, #00f 67%, #f0f 83%, #f00 100%)',
            }}
          >
            <span
              className="pointer-events-none absolute top-1/2 h-4 w-1.5 -translate-x-1/2 -translate-y-1/2 rounded-sm bg-white"
              style={{ left: `${(h / 360) * 100}%`, boxShadow: '0 0 0 1px rgba(0,0,0,0.35)' }}
            />
          </div>
        </div>
      )}
    </div>
  )
}

// ---------------------------------------------------------------------------
// Paths
// ---------------------------------------------------------------------------

function Path3Summary({ value }: { value: Path3 }) {
  return (
    <div className="rounded bg-sunken px-1.5 py-1 text-[11px] text-muted">
      {value.points.length} points{value.closed ? ', closed' : ''} — flatten it to edit
    </div>
  )
}

const PAD = 160

/**
 * The points of a 2D path, dragged directly on a pad.
 *
 * Double-clicking empty space puts a point where you clicked, on whichever edge
 * it lands nearest; alt-clicking or right-clicking a point takes it away. Every
 * gesture stops where it is caught, because React Flow is listening for the
 * same ones behind it.
 */
function PathPad({ value, onChange }: { value: Path2; onChange: (value: Path2) => void }) {
  const svg = useRef<SVGSVGElement>(null)
  const points = value.points

  const extent =
    Math.max(0.5, ...points.flatMap((p) => [Math.abs(p.x), Math.abs(p.y)])) * 1.2
  const view = { min: -extent, size: extent * 2 }
  const unit = extent / 50

  /** Pad coordinates from a pointer, in the path's own space. */
  function at(event: { clientX: number; clientY: number }): Vec2 | null {
    const element = svg.current
    if (!element) return null
    const matrix = element.getScreenCTM()
    if (!matrix) return null
    const point = new DOMPoint(event.clientX, event.clientY).matrixTransform(matrix.inverse())
    return vec2(point.x, -point.y)
  }

  function drag(index: number, event: React.PointerEvent): void {
    event.stopPropagation()
    if (event.altKey || event.button === 2) {
      remove(index)
      return
    }
    if (event.button !== 0) return
    const target = event.currentTarget as SVGElement
    target.setPointerCapture(event.pointerId)
    const move = (moved: PointerEvent): void => {
      const here = at(moved)
      if (here) onChange({ ...value, points: points.map((p, k) => (k === index ? here : p)) })
    }
    const stop = (): void => {
      target.releasePointerCapture(event.pointerId)
      target.removeEventListener('pointermove', move)
      target.removeEventListener('pointerup', stop)
      target.removeEventListener('pointercancel', stop)
    }
    target.addEventListener('pointermove', move)
    target.addEventListener('pointerup', stop)
    target.addEventListener('pointercancel', stop)
  }

  function remove(index: number): void {
    if (points.length > 2) onChange({ ...value, points: points.filter((_, k) => k !== index) })
  }

  /** Put a point where it was clicked, on whichever edge that is nearest. */
  function insert(event: React.MouseEvent): void {
    event.stopPropagation()
    const here = at(event)
    if (!here || points.length === 0) return
    let best = points.length
    let closest = Infinity
    const last = value.closed ? points.length : points.length - 1
    for (let i = 0; i < last; i++) {
      const a = points[i]
      const b = points[(i + 1) % points.length]
      const middle = vec2((a.x + b.x) / 2, (a.y + b.y) / 2)
      const distance = (middle.x - here.x) ** 2 + (middle.y - here.y) ** 2
      if (distance < closest) {
        closest = distance
        best = i + 1
      }
    }
    const next = [...points]
    next.splice(best, 0, here)
    onChange({ ...value, points: next })
  }

  const outline = points.map((p) => `${p.x},${-p.y}`).join(' ')

  return (
    <div className="nodrag nowheel" onContextMenu={(event) => event.preventDefault()}>
      <svg
        ref={svg}
        width={PAD}
        height={PAD}
        viewBox={`${view.min} ${view.min} ${view.size} ${view.size}`}
        onDoubleClick={insert}
        onPointerDown={(event) => event.stopPropagation()}
        className="w-full rounded bg-sunken"
      >
        <line x1={view.min} y1="0" x2={view.min + view.size} y2="0" stroke="#e0e0e6" strokeWidth={unit} />
        <line x1="0" y1={view.min} x2="0" y2={view.min + view.size} stroke="#e0e0e6" strokeWidth={unit} />
        {value.closed ? (
          <polygon points={outline} fill="rgba(97,105,217,0.12)" stroke="#6169d9" strokeWidth={unit * 1.6} />
        ) : (
          <polyline points={outline} fill="none" stroke="#6169d9" strokeWidth={unit * 1.6} />
        )}
        {points.map((p, index) => (
          <circle
            key={index}
            cx={p.x}
            cy={-p.y}
            r={unit * 4}
            fill="#ffffff"
            stroke="#4c53c4"
            strokeWidth={unit * 1.4}
            onPointerDown={(event) => drag(index, event)}
            onDoubleClick={(event) => event.stopPropagation()}
          />
        ))}
      </svg>
      <div className="flex items-center gap-2 pt-1 text-[10px] text-faint">
        <span className="grow">{points.length} points · drag, double-click to add, alt-click to remove</span>
        <button
          onClick={() => onChange({ ...value, closed: !value.closed })}
          className={cn('shrink-0 rounded px-1', value.closed ? 'text-brand-600' : 'hover:text-muted')}
          title="Whether the last point joins back to the first"
        >
          {value.closed ? 'closed' : 'open'}
        </button>
      </div>
    </div>
  )
}
