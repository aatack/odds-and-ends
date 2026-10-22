import React from 'react'
import type { Editors } from '../schema'

// Editors that need nothing of the app: only the value they are handed.

/** Black or white, whichever reads on a background of `hex` (by its luminance). */
function inkOn(hex: string): string {
  const [r, g, b] = [1, 3, 5].map((i) => {
    const c = parseInt(hex.slice(i, i + 2), 16) / 255
    return c <= 0.03928 ? c / 12.92 : ((c + 0.055) / 1.055) ** 2.4
  })
  return 0.2126 * r + 0.7152 * g + 0.0722 * b > 0.179 ? '#000000' : '#ffffff'
}

/**
 * A control drawn in place (colour), one built of buttons (stars), and one that
 * runs code and hands a value back (roll).
 */
export const BASIC_EDITORS: Editors = {
  colour: {
    name: 'Colour picker',
    kinds: ['string'],
    // The whole box is the colour, with its hex on top in whichever of black or
    // white reads on it. The native picker lies invisibly over all of it, so a
    // press anywhere opens it.
    render: ({ value, onChange }) => {
      const hex = typeof value === 'string' && /^#[0-9a-f]{6}$/i.test(value) ? value : null
      return (
        <label
          className="relative flex h-8 w-full items-center px-2.5 font-mono text-[12.5px]"
          style={{ background: hex ?? undefined, color: hex ? inkOn(hex) : undefined }}
        >
          <span className={hex ? undefined : 'text-gray-400'}>{hex ?? 'Pick a colour'}</span>
          <input
            type="color"
            className="absolute inset-0 h-full w-full opacity-0"
            value={hex ?? '#000000'}
            onChange={(e) => onChange(e.target.value)}
          />
        </label>
      )
    },
  },
  stars: {
    name: 'Stars',
    kinds: ['integer', 'number'],
    render: ({ value, onChange }) => (
      <div className="flex px-1.5">
        {[1, 2, 3, 4, 5].map((n) => (
          <button
            key={n}
            className={`px-0.5 text-[15px] ${typeof value === 'number' && value >= n ? 'text-brand-600' : 'text-gray-300'}`}
            onClick={() => onChange(value === n ? 0 : n)}
          >
            ★
          </button>
        ))}
      </div>
    ),
  },
  roll: {
    name: 'Dice roll',
    kinds: ['integer', 'number'],
    render: ({ value, onChange }) => (
      <button
        className="px-2.5 text-left text-[13px] text-gray-700"
        // Stands in for any code that comes back with a value later.
        onClick={async () => {
          await new Promise((r) => setTimeout(r, 200))
          onChange(1 + Math.floor(Math.random() * 6))
        }}
      >
        {typeof value === 'number' ? `Rolled ${value} — roll again` : 'Roll'}
      </button>
    ),
  },
}
