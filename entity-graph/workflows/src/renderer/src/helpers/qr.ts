// A QR code for some text: byte mode, error correction level M, the smallest version it fits. After Project
// Nayuki's QR Code generator (MIT), cut down to what is needed here. No DOM, so the tests check it against qrencode.

const ECC_PER_BLOCK = [-1, 10, 16, 26, 18, 24, 16, 18, 22, 22, 26, 30, 22, 22, 24, 24, 28, 28, 26, 26, 26, 26, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28]
const BLOCKS = [-1, 1, 1, 1, 2, 2, 4, 4, 4, 5, 5, 5, 8, 9, 9, 10, 10, 11, 13, 14, 16, 17, 17, 18, 20, 21, 23, 25, 26, 28, 29, 31, 33, 35, 37, 38, 40, 43, 45, 47, 49]

const bit = (x: number, i: number) => ((x >>> i) & 1) !== 0

function rawModules(ver: number): number {
  let n = (16 * ver + 128) * ver + 64
  if (ver >= 2) {
    const align = Math.floor(ver / 7) + 2
    n -= (25 * align - 10) * align - 55
    if (ver >= 7) n -= 36
  }
  return n
}
const dataCodewords = (ver: number) => Math.floor(rawModules(ver) / 8) - ECC_PER_BLOCK[ver] * BLOCKS[ver]

function gfMul(x: number, y: number): number {
  let z = 0
  for (let i = 7; i >= 0; i--) {
    z = (z << 1) ^ ((z >>> 7) * 0x11d)
    z ^= ((y >>> i) & 1) * x
  }
  return z & 0xff
}

function rsDivisor(degree: number): number[] {
  const r = new Array<number>(degree).fill(0)
  r[degree - 1] = 1
  let root = 1
  for (let i = 0; i < degree; i++) {
    for (let j = 0; j < r.length; j++) {
      r[j] = gfMul(r[j], root)
      if (j + 1 < r.length) r[j] ^= r[j + 1]
    }
    root = gfMul(root, 0x02)
  }
  return r
}

function rsRemainder(data: number[], divisor: number[]): number[] {
  const r = divisor.map(() => 0)
  for (const b of data) {
    const factor = b ^ (r.shift() as number)
    r.push(0)
    divisor.forEach((d, i) => (r[i] ^= gfMul(d, factor)))
  }
  return r
}

/** The modules, dark as true, row by row; `mask` (0–7) forces a mask, else the one with the least penalty is used. */
export function encodeQr(text: string, mask?: number): boolean[][] {
  const bytes = [...new TextEncoder().encode(text)]
  let ver = 1
  for (; ; ver++) {
    if (ver > 40) throw new Error('Too long for a QR code.')
    if (4 + (ver <= 9 ? 8 : 16) + 8 * bytes.length <= dataCodewords(ver) * 8) break
  }
  // The data: byte mode, its length, the bytes, then a terminator and padding.
  const bits: number[] = []
  const put = (v: number, n: number) => {
    for (let i = n - 1; i >= 0; i--) bits.push((v >>> i) & 1)
  }
  put(4, 4)
  put(bytes.length, ver <= 9 ? 8 : 16)
  for (const b of bytes) put(b, 8)
  const capacity = dataCodewords(ver) * 8
  put(0, Math.min(4, capacity - bits.length))
  put(0, (8 - (bits.length % 8)) % 8)
  for (let pad = 0xec; bits.length < capacity; pad ^= 0xec ^ 0x11) put(pad, 8)
  const data: number[] = []
  for (let i = 0; i < bits.length; i += 8) data.push(bits.slice(i, i + 8).reduce((a, b) => (a << 1) | b, 0))

  // Error correction, block by block, interleaved.
  const blocks = BLOCKS[ver]
  const eccLen = ECC_PER_BLOCK[ver]
  const raw = Math.floor(rawModules(ver) / 8)
  const shortBlocks = blocks - (raw % blocks)
  const shortLen = Math.floor(raw / blocks)
  const divisor = rsDivisor(eccLen)
  const all: number[][] = []
  for (let i = 0, k = 0; i < blocks; i++) {
    const dat = data.slice(k, k + shortLen - eccLen + (i < shortBlocks ? 0 : 1))
    k += dat.length
    const ecc = rsRemainder(dat, divisor)
    if (i < shortBlocks) dat.push(0)
    all.push([...dat, ...ecc])
  }
  const codewords: number[] = []
  for (let i = 0; i < all[0].length; i++)
    for (let j = 0; j < all.length; j++) if (i !== shortLen - eccLen || j >= shortBlocks) codewords.push(all[j][i])

  // The function patterns, then the codewords in their zigzag, then the mask.
  const size = ver * 4 + 17
  const modules = Array.from({ length: size }, () => new Array<boolean>(size).fill(false))
  const fixed = Array.from({ length: size }, () => new Array<boolean>(size).fill(false))
  const set = (x: number, y: number, dark: boolean) => {
    modules[y][x] = dark
    fixed[y][x] = true
  }
  for (let i = 0; i < size; i++) {
    set(6, i, i % 2 === 0)
    set(i, 6, i % 2 === 0)
  }
  for (const [cx, cy] of [[3, 3], [size - 4, 3], [3, size - 4]])
    for (let dy = -4; dy <= 4; dy++)
      for (let dx = -4; dx <= 4; dx++) {
        const d = Math.max(Math.abs(dx), Math.abs(dy))
        const x = cx + dx
        const y = cy + dy
        if (x >= 0 && x < size && y >= 0 && y < size) set(x, y, d !== 2 && d !== 4)
      }
  if (ver > 1) {
    const n = Math.floor(ver / 7) + 2
    const step = Math.floor((ver * 8 + n * 3 + 5) / (n * 4 - 4)) * 2
    const at = [6]
    for (let p = size - 7; at.length < n; p -= step) at.splice(1, 0, p)
    for (let i = 0; i < n; i++)
      for (let j = 0; j < n; j++) {
        if ((i === 0 && j === 0) || (i === 0 && j === n - 1) || (i === n - 1 && j === 0)) continue
        for (let dy = -2; dy <= 2; dy++) for (let dx = -2; dx <= 2; dx++) set(at[i] + dx, at[j] + dy, Math.max(Math.abs(dx), Math.abs(dy)) !== 1)
      }
  }
  const format = (m: number) => {
    // Level M is 00.
    const d = m
    let r = d
    for (let i = 0; i < 10; i++) r = (r << 1) ^ ((r >>> 9) * 0x537)
    const b = ((d << 10) | r) ^ 0x5412
    for (let i = 0; i <= 5; i++) set(8, i, bit(b, i))
    set(8, 7, bit(b, 6))
    set(8, 8, bit(b, 7))
    set(7, 8, bit(b, 8))
    for (let i = 9; i < 15; i++) set(14 - i, 8, bit(b, i))
    for (let i = 0; i < 8; i++) set(size - 1 - i, 8, bit(b, i))
    for (let i = 8; i < 15; i++) set(8, size - 15 + i, bit(b, i))
    set(8, size - 8, true)
  }
  format(0)
  if (ver >= 7) {
    let r = ver
    for (let i = 0; i < 12; i++) r = (r << 1) ^ ((r >>> 11) * 0x1f25)
    const b = (ver << 12) | r
    for (let i = 0; i < 18; i++) {
      const a = size - 11 + (i % 3)
      const c = Math.floor(i / 3)
      set(a, c, bit(b, i))
      set(c, a, bit(b, i))
    }
  }
  let i = 0
  for (let right = size - 1; right >= 1; right -= 2) {
    if (right === 6) right = 5
    for (let v = 0; v < size; v++)
      for (let j = 0; j < 2; j++) {
        const x = right - j
        const y = ((right + 1) & 2) === 0 ? size - 1 - v : v
        if (!fixed[y][x] && i < codewords.length * 8) {
          modules[y][x] = bit(codewords[i >>> 3], 7 - (i & 7))
          i++
        }
      }
  }

  const masked = (m: number) => {
    const out = modules.map((row) => [...row])
    for (let y = 0; y < size; y++)
      for (let x = 0; x < size; x++) {
        if (fixed[y][x]) continue
        const flip = [
          (x + y) % 2 === 0,
          y % 2 === 0,
          x % 3 === 0,
          (x + y) % 3 === 0,
          (Math.floor(x / 3) + Math.floor(y / 2)) % 2 === 0,
          ((x * y) % 2) + ((x * y) % 3) === 0,
          (((x * y) % 2) + ((x * y) % 3)) % 2 === 0,
          (((x + y) % 2) + ((x * y) % 3)) % 2 === 0,
        ][m]
        if (flip) out[y][x] = !out[y][x]
      }
    return out
  }
  const withFormat = (m: number) => {
    const out = masked(m)
    const keep = modules.map((row) => [...row])
    format(m)
    for (let y = 0; y < size; y++) for (let x = 0; x < size; x++) if (fixed[y][x]) out[y][x] = modules[y][x]
    for (let y = 0; y < size; y++) modules[y] = keep[y]
    return out
  }
  if (mask !== undefined) return withFormat(mask)
  let best: boolean[][] = []
  let least = Infinity
  for (let m = 0; m < 8; m++) {
    const q = withFormat(m)
    const p = penalty(q)
    if (p < least) {
      least = p
      best = q
    }
  }
  return best
}

/** The spec's penalties for runs, blocks and balance (not finder-like patterns): any mask reads; this picks a tidy one. */
function penalty(q: boolean[][]): number {
  const size = q.length
  let p = 0
  for (let a = 0; a < size; a++)
    for (const line of [q[a], q.map((row) => row[a])]) {
      let run = 1
      for (let i = 1; i <= size; i++) {
        if (i < size && line[i] === line[i - 1]) run++
        else {
          if (run >= 5) p += run - 2
          run = 1
        }
      }
    }
  let dark = 0
  for (let y = 0; y < size; y++)
    for (let x = 0; x < size; x++) {
      if (q[y][x]) dark++
      if (x < size - 1 && y < size - 1 && q[y][x] === q[y][x + 1] && q[y][x] === q[y + 1][x] && q[y][x] === q[y + 1][x + 1]) p += 3
    }
  return p + Math.floor(Math.abs(dark * 20 - size * size * 10) / (size * size)) * 10
}
