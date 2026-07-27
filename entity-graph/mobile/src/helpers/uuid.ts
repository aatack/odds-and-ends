/**
 * A v4 uuid, without `crypto.randomUUID`.
 *
 * That function is restricted to secure contexts, and this app is served over
 * plain HTTP from a laptop on the LAN — so on the very setup it is designed for
 * it would be `undefined`. `crypto.getRandomValues` carries no such restriction,
 * hence the hand-rolled version; `Math.random` is the last resort, which no
 * browser this runs on should ever need.
 */
export function uuid(): string {
  const bytes = new Uint8Array(16)
  if (typeof crypto !== 'undefined' && crypto.getRandomValues) {
    crypto.getRandomValues(bytes)
  } else {
    for (let i = 0; i < 16; i++) bytes[i] = Math.floor(Math.random() * 256)
  }
  // Version 4, variant 1, as the spec pins those bits.
  bytes[6] = (bytes[6] & 0x0f) | 0x40
  bytes[8] = (bytes[8] & 0x3f) | 0x80
  const hex = [...bytes].map((b) => b.toString(16).padStart(2, '0')).join('')
  return `${hex.slice(0, 8)}-${hex.slice(8, 12)}-${hex.slice(12, 16)}-${hex.slice(16, 20)}-${hex.slice(20)}`
}
