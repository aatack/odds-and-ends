// Secrets for a backend that runs without Electron, and so without its
// safeStorage: AES-256-GCM, under a key kept in a file of its own beside the
// store (readable by its owner only). Values from before the backend ran on its
// own, encrypted with safeStorage (`safe:`), can only be read by the app, which
// hands them back decrypted (see `legacySecrets` in api.ts).

import { createCipheriv, createDecipheriv, randomBytes } from 'node:crypto'
import { chmodSync, existsSync, readFileSync, writeFileSync } from 'node:fs'
import { join } from 'node:path'
import { plainSecrets, type Secrets } from '../engine/settings'

export function keyFileSecrets(dir: string): Secrets {
  const file = join(dir, 'secret.key')
  if (!existsSync(file)) writeFileSync(file, randomBytes(32).toString('base64'), { mode: 0o600 })
  chmodSync(file, 0o600)
  const key = Buffer.from(readFileSync(file, 'utf8').trim(), 'base64')
  return {
    encrypt: (plain) => {
      const iv = randomBytes(12)
      const cipher = createCipheriv('aes-256-gcm', key, iv)
      const body = Buffer.concat([cipher.update(plain, 'utf8'), cipher.final()])
      return `aes:${Buffer.concat([iv, cipher.getAuthTag(), body]).toString('base64')}`
    },
    decrypt: (stored) => {
      if (stored.startsWith('safe:')) throw new Error('This secret was saved by an older app: open the app once, or add it again.')
      if (!stored.startsWith('aes:')) return plainSecrets.decrypt(stored)
      const raw = Buffer.from(stored.slice(4), 'base64')
      const decipher = createDecipheriv('aes-256-gcm', key, raw.subarray(0, 12))
      decipher.setAuthTag(raw.subarray(12, 28))
      return Buffer.concat([decipher.update(raw.subarray(28)), decipher.final()]).toString('utf8')
    },
  }
}
