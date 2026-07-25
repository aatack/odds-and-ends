// The other side of the transport seam: the local filesystem, rather than the
// open source. One capability so far — putting a resource's bytes in the
// downloads folder — kept here so the tool layer never touches `window`.

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
