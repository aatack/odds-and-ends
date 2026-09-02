/**
 * Editing a literal in place on a node.
 *
 * Every value type that can be typed or dragged has an editor here, and the
 * builder shows one wherever a socket is unconnected — so a constant is edited
 * where it is used rather than in a panel somewhere else. A 2D path is the
 * interesting one: its points are dragged around a pad, which is the intuitive
 * way to say what shape you meant.
 */

import { useRef, useState } from 'react'
import type { Colour, Path2, Path3, ValueType, Vec2, Vec3 } from '@core/values'
import { fromHex, toHex, vec2 } from '@core/values'
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

/** A few tones worth reaching for before mixing one. */
const SWATCHES = [
  '#d9d4c7',
  '#a8a29a',
  '#6f7278',
  '#3a3d42',
  '#b0553f',
  '#c08a4a',
  '#7c9a6d',
  '#4f7f8f',
  '#5b5f9e',
  '#8a5f7d',
  '#ffffff',
  '#1f2024',
]

/**
 * Written rather than picked. `<input type="color">` opens a popup the page
 * cannot see or control — on Linux it is drawn *inside* the window and takes
 * stray clicks, which is how a colour changes itself. A swatch, a hex field
 * and three channels are all this needs and none of it leaves the page.
 */
function ColourField({ value, onChange }: { value: Colour; onChange: (value: Colour) => void }) {
  const [open, setOpen] = useState(false)
  const hex = toHex(value)
  const channel = (key: 'r' | 'g' | 'b') => (
    <input
      key={key}
      type="range"
      min={0}
      max={255}
      value={Math.round(value[key] * 255)}
      onChange={(event) => onChange({ ...value, [key]: Number(event.target.value) / 255 })}
      className="nodrag h-3 w-full accent-brand-600"
      title={key.toUpperCase()}
    />
  )

  return (
    <div className="nodrag">
      <div className="flex items-center gap-1.5">
        <button
          onClick={() => setOpen((was) => !was)}
          style={{ background: hex }}
          className="h-5 w-5 shrink-0 rounded ring-1 ring-line"
          title="Mix a colour"
        />
        <input
          value={hex}
          spellCheck={false}
          onChange={(event) => {
            const text = event.target.value.trim()
            if (/^#?[0-9a-f]{6}$/i.test(text) || /^#?[0-9a-f]{3}$/i.test(text)) {
              onChange(fromHex(text))
            }
          }}
          className="h-6 w-full min-w-0 rounded bg-sunken px-1.5 font-mono text-[11px] hover:bg-line/70 focus:bg-line/70"
        />
      </div>
      {open && (
        <div className="mt-1 rounded bg-sunken p-1.5">
          <div className="grid grid-cols-6 gap-1 pb-1.5">
            {SWATCHES.map((swatch) => (
              <button
                key={swatch}
                onClick={() => onChange(fromHex(swatch))}
                style={{ background: swatch }}
                className="h-4 rounded-sm ring-1 ring-line"
                title={swatch}
              />
            ))}
          </div>
          {(['r', 'g', 'b'] as const).map(channel)}
        </div>
      )}
    </div>
  )
}

function Path3Summary({ value }: { value: Path3 }) {
  return (
    <div className="rounded bg-sunken px-1.5 py-1 text-[11px] text-muted">
      {value.points.length} points{value.closed ? ', closed' : ''} — flatten it to edit
    </div>
  )
}

const PAD = 132

/**
 * The points of a 2D path, dragged directly. Double-clicking the pad puts a
 * new point on the nearest edge, and alt-clicking one takes it away.
 */
function PathPad({ value, onChange }: { value: Path2; onChange: (value: Path2) => void }) {
  const svg = useRef<SVGSVGElement>(null)
  const points = value.points

  const extent = Math.max(
    1,
    ...points.flatMap((p) => [Math.abs(p.x), Math.abs(p.y)]),
  ) * 1.15
  const view = { min: -extent, size: extent * 2 }

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
    if (event.altKey) {
      if (points.length > 2) onChange({ ...value, points: points.filter((_, k) => k !== index) })
      return
    }
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
    }
    target.addEventListener('pointermove', move)
    target.addEventListener('pointerup', stop)
  }

  /** Put a point on whichever edge the click landed nearest. */
  function insert(event: React.MouseEvent): void {
    const here = at(event)
    if (!here || points.length === 0) return
    let best = 0
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
    <div className="nodrag nowheel">
      <svg
        ref={svg}
        width={PAD}
        height={PAD}
        viewBox={`${view.min} ${view.min} ${view.size} ${view.size}`}
        onDoubleClick={insert}
        className="rounded bg-sunken"
      >
        <line x1={view.min} y1="0" x2={view.min + view.size} y2="0" stroke="#e0e0e6" strokeWidth={extent / 90} />
        <line x1="0" y1={view.min} x2="0" y2={view.min + view.size} stroke="#e0e0e6" strokeWidth={extent / 90} />
        {value.closed ? (
          <polygon points={outline} fill="rgba(97,105,217,0.12)" stroke="#6169d9" strokeWidth={extent / 60} />
        ) : (
          <polyline points={outline} fill="none" stroke="#6169d9" strokeWidth={extent / 60} />
        )}
        {points.map((p, index) => (
          <circle
            key={index}
            cx={p.x}
            cy={-p.y}
            r={extent / 22}
            fill="#ffffff"
            stroke="#4c53c4"
            strokeWidth={extent / 70}
            onPointerDown={(event) => drag(index, event)}
          />
        ))}
      </svg>
      <div className={cn('pt-1 text-[10px] text-faint')}>
        drag a point · double-click to add · alt-click to remove
      </div>
    </div>
  )
}
