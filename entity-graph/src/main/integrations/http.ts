// Outbound HTTP, for the integrations with no command-line front end. Thin on
// purpose: one request, JSON back, and an error that quotes what the service
// actually said rather than "request failed".

export interface JsonRequest {
  method?: 'GET' | 'POST' | 'PUT' | 'DELETE'
  headers?: Record<string, string>
  /** Sent as a JSON body. */
  body?: unknown
  /** Appended as a query string; `undefined` values are dropped. */
  query?: Record<string, string | number | boolean | undefined>
}

/** The service answered, but not with what was asked for. */
export class HttpError extends Error {
  constructor(
    public status: number,
    public body: string,
  ) {
    super(`HTTP ${status}: ${body.slice(0, 500) || '(empty response)'}`)
    this.name = 'HttpError'
  }
}

const withQuery = (url: string, query: JsonRequest['query']): string => {
  if (!query) return url
  const params = new URLSearchParams()
  for (const [key, value] of Object.entries(query)) {
    if (value !== undefined) params.set(key, String(value))
  }
  const q = params.toString()
  return q ? `${url}${url.includes('?') ? '&' : '?'}${q}` : url
}

export async function fetchJson<T>(url: string, req: JsonRequest = {}): Promise<T> {
  const res = await fetch(withQuery(url, req.query), {
    method: req.method ?? (req.body === undefined ? 'GET' : 'POST'),
    headers: {
      Accept: 'application/json',
      ...(req.body !== undefined ? { 'Content-Type': 'application/json' } : {}),
      ...req.headers,
    },
    body: req.body !== undefined ? JSON.stringify(req.body) : undefined,
  })
  const text = await res.text()
  if (!res.ok) throw new HttpError(res.status, text)
  if (!text) return undefined as T
  try {
    return JSON.parse(text) as T
  } catch {
    throw new HttpError(res.status, text)
  }
}

/**
 * A form-encoded POST returning JSON. Every Slack Web API method accepts this,
 * where JSON bodies are only honoured by some of them — so one shape covers the
 * whole API.
 */
export async function postForm<T>(
  url: string,
  form: Record<string, string | number | boolean | undefined>,
  headers: Record<string, string> = {},
): Promise<T> {
  const body = new URLSearchParams()
  for (const [key, value] of Object.entries(form)) {
    if (value !== undefined) body.set(key, String(value))
  }
  const res = await fetch(url, {
    method: 'POST',
    headers: { 'Content-Type': 'application/x-www-form-urlencoded; charset=utf-8', ...headers },
    body,
  })
  const text = await res.text()
  if (!res.ok) throw new HttpError(res.status, text)
  try {
    return JSON.parse(text) as T
  } catch {
    throw new HttpError(res.status, text)
  }
}
