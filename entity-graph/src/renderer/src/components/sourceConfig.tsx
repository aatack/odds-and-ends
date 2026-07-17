import React from 'react'
import type { SourceConfig, SourceType } from '../../../core/client'
import { Field } from './ui/Field'
import { Input } from './ui/Input'
import { Select } from './ui/Select'

export const SOURCE_TYPES: SourceType[] = ['sqlite', 'combined', 'frozen', 'filter', 'remote']
export const SAFETIES = ['pure', 'safe-mutating', 'dangerous'] as const

/** Type-aware config inputs for all five source types. */
export function ConfigFields({
  type,
  cfg,
  set,
}: {
  type: SourceType
  cfg: Record<string, string>
  set: (k: string, v: string) => void
}): React.JSX.Element {
  const field = (key: string, label: string, placeholder = '', mono = true): React.JSX.Element => (
    <Field label={label}>
      <Input
        mono={mono}
        value={cfg[key] ?? ''}
        onChange={(e) => set(key, e.target.value)}
        placeholder={placeholder}
      />
    </Field>
  )

  switch (type) {
    case 'sqlite':
      return <>{field('path', 'DB file path', '/path/to/store.db')}{field('defaultAuthor', 'Default author (optional)', 'anonymous', false)}</>
    case 'combined':
      return field('children', 'Child source ids (comma-separated)', 'a, b, c')
    case 'frozen':
      return <>{field('child', 'Child source id', 'source-id')}{field('beforeTs', 'Before timestamp (Unix ms)', '1700000000000')}</>
    case 'filter':
      return (
        <>
          {field('child', 'Child source id', 'source-id')}
          {field('allow', 'Allow tool ids (comma-separated, optional)', 'readEvents, query')}
          {field('deny', 'Deny tool ids (comma-separated, optional)', 'writeValue')}
          <Field label="Max safety (optional)">
            <Select value={cfg.maxSafety ?? ''} onChange={(e) => set('maxSafety', e.target.value)}>
              <option value="">(none)</option>
              {SAFETIES.map((s) => <option key={s} value={s}>{s}</option>)}
            </Select>
          </Field>
        </>
      )
    case 'remote':
      return <>{field('url', 'Remote source URL', 'http://host:4000/other-source')}{field('token', 'Remote token (optional)', '')}</>
  }
}

// ---------------------------------------------------------------------------
// (De)serialisation between flat form state and SourceConfig
// ---------------------------------------------------------------------------

export function flattenConfig(c?: SourceConfig): Record<string, string> {
  if (!c) return {}
  switch (c.type) {
    case 'sqlite': return { path: c.path, defaultAuthor: c.defaultAuthor ?? '' }
    case 'combined': return { children: c.children.join(', ') }
    case 'frozen': return { child: c.child, beforeTs: String(c.beforeTs) }
    case 'filter': return {
      child: c.child,
      allow: (c.allow ?? []).join(', '),
      deny: (c.deny ?? []).join(', '),
      maxSafety: c.maxSafety ?? '',
    }
    case 'remote': return { url: c.url, token: c.token ?? '' }
  }
}

const csv = (s?: string): string[] =>
  (s ?? '').split(',').map((x) => x.trim()).filter(Boolean)

export function buildConfig(type: SourceType, cfg: Record<string, string>): SourceConfig {
  switch (type) {
    case 'sqlite':
      if (!cfg.path?.trim()) throw new Error('path is required')
      return { type, path: cfg.path.trim(), ...(cfg.defaultAuthor?.trim() ? { defaultAuthor: cfg.defaultAuthor.trim() } : {}) }
    case 'combined':
      return { type, children: csv(cfg.children) }
    case 'frozen':
      if (!cfg.child?.trim()) throw new Error('child is required')
      return { type, child: cfg.child.trim(), beforeTs: Number(cfg.beforeTs || 0) }
    case 'filter': {
      if (!cfg.child?.trim()) throw new Error('child is required')
      const allow = csv(cfg.allow)
      const deny = csv(cfg.deny)
      return {
        type,
        child: cfg.child.trim(),
        ...(allow.length ? { allow } : {}),
        ...(deny.length ? { deny } : {}),
        ...(cfg.maxSafety ? { maxSafety: cfg.maxSafety as (typeof SAFETIES)[number] } : {}),
      }
    }
    case 'remote':
      if (!cfg.url?.trim()) throw new Error('url is required')
      return { type, url: cfg.url.trim(), ...(cfg.token?.trim() ? { token: cfg.token.trim() } : {}) }
  }
}
