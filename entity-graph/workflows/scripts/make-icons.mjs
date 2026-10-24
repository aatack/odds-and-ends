// Generates the PWA icons as PNGs with Node built-ins only (zlib for the pixel
// stream). A solid dark square (the app background) with a rounded brand-tone
// mark centred in it — plain, but the design's own two tones. Run with:
//   node scripts/make-icons.mjs
// Output goes to src/renderer/public, which Vite copies into out/renderer.

import { deflateSync } from 'node:zlib'
import { writeFileSync, mkdirSync } from 'node:fs'
import { join, dirname } from 'node:path'
import { fileURLToPath } from 'node:url'

const BG = [0x0e, 0x13, 0x1c, 0xff] // --color-gray-50 in dark mode: the body background
const MARK = [0x5b, 0x63, 0xd3, 0xff] // --color-brand-600: the app's one accent

const crcTable = Array.from({ length: 256 }, (_, n) => {
  let c = n
  for (let k = 0; k < 8; k++) c = c & 1 ? 0xedb88320 ^ (c >>> 1) : c >>> 1
  return c >>> 0
})
const crc32 = (buf) => {
  let c = 0xffffffff
  for (const b of buf) c = crcTable[(c ^ b) & 0xff] ^ (c >>> 8)
  return (c ^ 0xffffffff) >>> 0
}

const chunk = (type, data) => {
  const len = Buffer.alloc(4)
  len.writeUInt32BE(data.length)
  const typed = Buffer.concat([Buffer.from(type, 'ascii'), data])
  const crc = Buffer.alloc(4)
  crc.writeUInt32BE(crc32(typed))
  return Buffer.concat([len, typed, crc])
}

// A size×size RGBA PNG: background, with a centred rounded square of the mark
// tone taking up `markFrac` of the canvas (smaller for maskable, to stay inside
// the safe zone). `radiusFrac` rounds the mark's corners.
function png(size, markFrac, radiusFrac = 0.22) {
  const inset = Math.round((size * (1 - markFrac)) / 2)
  const lo = inset
  const hi = size - inset
  const r = Math.round((hi - lo) * radiusFrac)
  const inMark = (x, y) => {
    if (x < lo || x >= hi || y < lo || y >= hi) return false
    const cx = x < lo + r ? lo + r : x >= hi - r ? hi - r : x
    const cy = y < lo + r ? lo + r : y >= hi - r ? hi - r : y
    return (x - cx) ** 2 + (y - cy) ** 2 <= r * r || (x >= lo + r && x < hi - r) || (y >= lo + r && y < hi - r)
  }
  const raw = Buffer.alloc((size * 4 + 1) * size)
  let p = 0
  for (let y = 0; y < size; y++) {
    raw[p++] = 0 // filter: none
    for (let x = 0; x < size; x++) {
      const [rr, gg, bb, aa] = inMark(x, y) ? MARK : BG
      raw[p++] = rr
      raw[p++] = gg
      raw[p++] = bb
      raw[p++] = aa
    }
  }
  const ihdr = Buffer.alloc(13)
  ihdr.writeUInt32BE(size, 0)
  ihdr.writeUInt32BE(size, 4)
  ihdr[8] = 8 // bit depth
  ihdr[9] = 6 // colour type: RGBA
  const sig = Buffer.from([137, 80, 78, 71, 13, 10, 26, 10])
  return Buffer.concat([sig, chunk('IHDR', ihdr), chunk('IDAT', deflateSync(raw)), chunk('IEND', Buffer.alloc(0))])
}

const dir = join(dirname(fileURLToPath(import.meta.url)), '..', 'src', 'renderer', 'public')
mkdirSync(dir, { recursive: true })
const write = (name, buf) => (writeFileSync(join(dir, name), buf), console.log(`${name}  ${buf.length} bytes`))
write('icon-192.png', png(192, 0.7))
write('icon-512.png', png(512, 0.7))
write('maskable-512.png', png(512, 0.52)) // smaller mark: stays inside the maskable safe zone
write('apple-touch-icon.png', png(180, 0.72))
