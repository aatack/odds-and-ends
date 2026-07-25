// Bytes to base64. The source's tools speak JSON, so anything binary crosses as
// a string; `btoa` needs one character per byte, which is what the chunking
// below builds without spreading a whole file into one argument list.

const CHUNK = 0x8000

export function bytesToBase64(bytes: Uint8Array): string {
  let binary = ''
  for (let i = 0; i < bytes.length; i += CHUNK) {
    binary += String.fromCharCode(...bytes.subarray(i, i + CHUNK))
  }
  return btoa(binary)
}

export const blobToBase64 = async (blob: Blob): Promise<string> =>
  bytesToBase64(new Uint8Array(await blob.arrayBuffer()))

export function base64ToBytes(data: string): Uint8Array<ArrayBuffer> {
  const binary = atob(data)
  const bytes = new Uint8Array(new ArrayBuffer(binary.length))
  for (let i = 0; i < binary.length; i++) bytes[i] = binary.charCodeAt(i)
  return bytes
}

export const base64ToBlob = (data: string, mimeType: string): Blob =>
  new Blob([base64ToBytes(data)], { type: mimeType })
