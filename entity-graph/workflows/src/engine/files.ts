// What a `file` item holds: a file kept with the source (`<source>.data/files/<sha256>`, an upload or a bot's copy),
// or a file on this machine it refers to (`path`), read as it is now. The window and the phone read them through
// the backend (fileData, fileText), so neither needs to reach the disk.

import { createHash } from 'node:crypto'
import { existsSync, mkdirSync, readFileSync, statSync, writeFileSync } from 'node:fs'
import { basename, extname, isAbsolute, join } from 'node:path'

/** The largest file read whole (into the window, or a stored copy). */
export const FILE_LIMIT = 50_000_000

const MIME: Record<string, string> = {
  '.png': 'image/png', '.jpg': 'image/jpeg', '.jpeg': 'image/jpeg', '.gif': 'image/gif', '.webp': 'image/webp', '.svg': 'image/svg+xml',
  '.bmp': 'image/bmp', '.ico': 'image/x-icon', '.avif': 'image/avif',
  '.pdf': 'application/pdf',
  '.docx': 'application/vnd.openxmlformats-officedocument.wordprocessingml.document',
  '.mp4': 'video/mp4', '.webm': 'video/webm', '.mov': 'video/quicktime', '.mp3': 'audio/mpeg', '.wav': 'audio/wav', '.ogg': 'audio/ogg', '.m4a': 'audio/mp4',
  '.md': 'text/markdown', '.markdown': 'text/markdown', '.html': 'text/html', '.htm': 'text/html', '.csv': 'text/csv', '.json': 'application/json',
  '.txt': 'text/plain', '.log': 'text/plain',
}

/** A file's type, from its name: what the browser would call it, octet-stream when nothing is known. */
export function mimeOf(name: string): string {
  return MIME[extname(name).toLowerCase()] ?? (TEXT_EXTS.has(extname(name).toLowerCase()) ? 'text/plain' : 'application/octet-stream')
}

/** Names that are text whatever their type says: source code, config and the like. */
const TEXT_EXTS = new Set(
  ('.ts .tsx .js .jsx .mjs .cjs .mts .cts .py .rs .go .java .kt .rb .php .c .h .cc .cpp .hpp .cs .swift .sh .bash .zsh .fish .ps1 .sql .css .scss ' +
    '.less .xml .yaml .yml .toml .ini .cfg .conf .env .gitignore .dockerfile .lua .r .jl .ex .exs .erl .hs .ml .scala .vue .svelte .tex .rst .org ' +
    '.diff .patch .lock .graphql .proto .tf .nix .zig')
    .split(' '),
)

/** Text: shown as text (and, when it refers to a file here, edited in place). */
export function isText(name: string, mime: string): boolean {
  return mime.startsWith('text/') || mime === 'application/json' || mime === 'image/svg+xml' || TEXT_EXTS.has(extname(name).toLowerCase())
}

export class Files {
  constructor(private dir: string) {}

  /** Keeps bytes with the source, under their hash (once, however often given). */
  keep(bytes: Buffer): string {
    if (bytes.length > FILE_LIMIT) throw new Error(`That file is over ${FILE_LIMIT / 1e6} MB.`)
    const hash = createHash('sha256').update(bytes).digest('hex')
    mkdirSync(this.dir, { recursive: true })
    const at = join(this.dir, hash)
    if (!existsSync(at)) writeFileSync(at, bytes)
    return hash
  }

  /** A file item's params as kept: `path` is checked (and, with `copy`, kept and dropped); name and type filled in. */
  prepare(p: Record<string, unknown>): Record<string, unknown> {
    const { copy, ...out } = p
    if (out.hash) {
      if (!/^[0-9a-f]{64}$/.test(String(out.hash)) || !existsSync(join(this.dir, String(out.hash)))) throw new Error(`No file is kept as ${out.hash}.`)
    } else {
      const path = String(out.path ?? '')
      if (!path) throw new Error('A file item needs a path (a file on this machine).')
      if (!isAbsolute(path)) throw new Error(`${path} is not an absolute path.`)
      if (!existsSync(path) || !statSync(path).isFile()) throw new Error(`There is no file at ${path}.`)
      out.name ??= basename(path)
      if (copy) {
        const bytes = readFileSync(path)
        out.hash = this.keep(bytes)
        out.size = bytes.length
        delete out.path
      }
    }
    out.name ??= 'file'
    out.mime ??= mimeOf(String(out.name))
    return out
  }

  /** Where a file item's bytes are: its kept copy, else the file it refers to. */
  where(p: Record<string, unknown>): string {
    if (p.hash) return join(this.dir, String(p.hash))
    if (p.path) return String(p.path)
    throw new Error('This file item holds no file.')
  }

  read(p: Record<string, unknown>): Buffer {
    const at = this.where(p)
    if (!existsSync(at)) throw new Error(p.hash ? 'Its kept copy is gone.' : `There is no file at ${at} any more.`)
    if (statSync(at).size > FILE_LIMIT) throw new Error(`It is over ${FILE_LIMIT / 1e6} MB, too large to show.`)
    return readFileSync(at)
  }

  /** How big it is now, and whether it is still there. */
  stat(p: Record<string, unknown>): { size: number | null; missing: boolean } {
    try {
      return { size: statSync(this.where(p)).size, missing: false }
    } catch {
      return { size: null, missing: true }
    }
  }

  /** Writes text over the file a file item refers to: never a kept copy, which stays as it was given. */
  saveText(p: Record<string, unknown>, text: string): void {
    if (p.hash || !p.path) throw new Error('Only a file it refers to on this machine can be edited: this one is a kept copy.')
    writeFileSync(String(p.path), text)
  }
}
