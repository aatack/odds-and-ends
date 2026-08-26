// The other side of the transport seam: the machine this is running on, rather
// than the open source — the filesystem, the system clipboard, and the browser.
// Each of them takes something out of the app and hands it to the desktop, and
// all of them are kept here so the tool layer never touches `window`.

const api = window.entityGraph

/**
 * Write bytes into the downloads folder under a name that isn't already taken,
 * and say where they went. There is no dialog and no choosing the directory:
 * clicking a file in the tree means "give me this", and a dialog defaulting to a
 * name that already exists is one careless Enter away from destroying the
 * original.
 */
export const saveFile = (name: string, data: string): Promise<string> =>
  api.saveFile(name, data)

/**
 * Put bytes on the system clipboard as a file, so they paste as one. The bytes
 * land in a temporary file on the way — the clipboard can only point at
 * something that exists — and that path is what comes back.
 */
export const copyFile = (name: string, data: string): Promise<string> =>
  api.copyFile(name, data)

/**
 * Open a URL in the desktop's own browser. The main process is where the scheme
 * is checked — `http`, `https` and `mailto` and nothing else — since that is the
 * side with anything to lose by opening something else.
 */
export const openExternal = (url: string): Promise<void> => api.openExternal(url)

/**
 * What is on the clipboard, as text. The other half — putting text *on* it — is
 * `helpers/clipboard`, and lives there because the browser will do it: only the
 * read side is behind a permission prompt, so only the read side comes through
 * the main process.
 */
export const readClipboardText = (): Promise<string> => api.readClipboardText()

/**
 * Show a file or directory where it lives, with the item selected in whatever
 * the desktop uses for a file manager. `~` is expanded on the other side, since
 * that is where the home directory is known; the absolute path comes back.
 */
export const revealPath = (path: string): Promise<string> => api.revealPath(path)

/**
 * A plausible extension for bytes that arrived without a name — from the
 * clipboard, say. Only for something short and word-like after the slash, so an
 * exotic mime type ends up with no extension rather than a silly one.
 */
function extensionFor(mimeType: string): string {
  const subtype = mimeType.split('/')[1]?.split('+')[0]?.replace(/[^a-z0-9]/gi, '') ?? ''
  return subtype && subtype.length <= 5 ? `.${subtype}` : ''
}

/**
 * What to call a resource once it leaves the app: the name it came with, and
 * failing that the entity's own id under an extension guessed from its type.
 */
export const fileNameFor = (
  resource: { name: string | null; mimeType: string },
  id: string,
): string => resource.name ?? `${id}${extensionFor(resource.mimeType)}`
