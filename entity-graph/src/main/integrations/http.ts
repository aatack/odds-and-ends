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

/** A request whose answer includes what the service said *about* the answer. */
export interface JsonResponse<T> {
  status: number
  /** Null when the service answered with no body — a 304, a 204. */
  body: T | null
  headers: Headers
}

/**
 * One request, with the status and the headers kept. Most callers only want the
 * body and reach for {@link fetchJson}; this is for the ones where the envelope
 * is the point — a conditional GET that answers `304`, a `Link` header that says
 * there is another page, a poll interval the service is asking to be obeyed.
 *
 * A status the caller named in `expect` comes back as an ordinary answer rather
 * than a throw, since "nothing has changed" is an answer.
 */
export async function fetchJsonResponse<T>(
  url: string,
  req: JsonRequest & { expect?: number[] } = {},
): Promise<JsonResponse<T>> {
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
  if (!res.ok && !req.expect?.includes(res.status)) throw new HttpError(res.status, text)
  if (!text) return { status: res.status, body: null, headers: res.headers }
  try {
    return { status: res.status, body: JSON.parse(text) as T, headers: res.headers }
  } catch {
    throw new HttpError(res.status, text)
  }
}

export async function fetchJson<T>(url: string, req: JsonRequest = {}): Promise<T> {
  const { body } = await fetchJsonResponse<T>(url, req)
  return body as T
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
