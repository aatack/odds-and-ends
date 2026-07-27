/**
 * Copy text, on a page that is very likely not a secure context.
 *
 * `navigator.clipboard` is gated on HTTPS, and this app is served over plain HTTP
 * from a laptop on the LAN — so the modern API is simply absent exactly where the
 * app runs. The fallback is the old hidden-textarea trick, which still works in
 * every mobile browser as long as it happens inside a user gesture (it does: every
 * copy here starts with a tap).
 */
export async function copyText(text: string): Promise<void> {
  if (navigator.clipboard?.writeText) {
    try {
      await navigator.clipboard.writeText(text)
      return
    } catch {
      // Fall through: permission refused, or an insecure context that exposes the
      // API but rejects it.
    }
  }

  const area = document.createElement('textarea')
  area.value = text
  // Off-screen but focusable, and `readOnly` so a mobile keyboard doesn't appear.
  area.setAttribute('readonly', '')
  area.style.position = 'fixed'
  area.style.top = '-1000px'
  area.style.opacity = '0'
  document.body.appendChild(area)
  area.select()
  area.setSelectionRange(0, text.length)
  const ok = document.execCommand('copy')
  document.body.removeChild(area)
  if (!ok) throw new Error('The browser refused to copy')
}
