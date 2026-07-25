// Putting things on the system clipboard.
//
// Text is unconditional. Images are not: the clipboard takes a very short list of
// types, and PNG is the only bitmap on it, so anything else is re-encoded first.
// That is a real conversion, not a relabelling — a JPEG copied out of here
// arrives as a PNG, losing nothing but its compression.

export const copyText = (text: string): Promise<void> => navigator.clipboard.writeText(text)

/** Re-encode an image as PNG, the one bitmap format the clipboard accepts. */
async function toPng(blob: Blob): Promise<Blob> {
  const bitmap = await createImageBitmap(blob)
  const canvas = new OffscreenCanvas(bitmap.width, bitmap.height)
  const context = canvas.getContext('2d')
  if (!context) throw new Error('Could not re-encode the image')
  context.drawImage(bitmap, 0, 0)
  bitmap.close()
  return canvas.convertToBlob({ type: 'image/png' })
}

/** Copy an image, converting it if it isn't already a PNG. */
export async function copyImage(blob: Blob): Promise<void> {
  const png = blob.type === 'image/png' ? blob : await toPng(blob)
  await navigator.clipboard.write([new ClipboardItem({ 'image/png': png })])
}
