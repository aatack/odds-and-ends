/**
 * Where a Slack image's bytes come from, by the ref a message gave it. The
 * window has a protocol for it; the phone asks the server. Set once, by the
 * entry, before anything renders.
 */
let source = (ref: string): string => `slack-image://${ref}`

export function setImageSource(next: (ref: string) => string): void {
  source = next
}

export function imageSrc(ref: string): string {
  return source(ref)
}
