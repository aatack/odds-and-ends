import type { ActionRequest, ActionResult, QueueView, ServerStatus, Topic, TopicDetail, TopicId } from './types'

/**
 * The only thing in the app that knows there is a server. Swapping this out is
 * how the same logic would run against an in-process backend, or a phone.
 */
export interface Transport {
  get<T>(path: string): Promise<T>
  post<T>(path: string, body: unknown): Promise<T>
  subscribe(listener: (event: { revision: number; topics: TopicId[]; queue: boolean }) => void): () => void
}

export class HttpTransport implements Transport {
  constructor(private readonly base: string) {}

  async get<T>(path: string): Promise<T> {
    const response = await fetch(`${this.base}${path}`)
    if (!response.ok) throw new Error(`${path} came back ${response.status}`)
    return (await response.json()) as T
  }

  async post<T>(path: string, body: unknown): Promise<T> {
    const response = await fetch(`${this.base}${path}`, {
      method: 'POST',
      headers: { 'content-type': 'application/json' },
      body: JSON.stringify(body),
    })
    if (!response.ok) throw new Error(`${path} came back ${response.status}`)
    return (await response.json()) as T
  }

  subscribe(listener: (event: { revision: number; topics: TopicId[]; queue: boolean }) => void): () => void {
    const source = new EventSource(`${this.base}/stream`)
    source.onmessage = (message: MessageEvent<string>) => {
      try {
        listener(JSON.parse(message.data) as { revision: number; topics: TopicId[]; queue: boolean })
      } catch {
        // A malformed frame is not worth tearing the stream down for.
      }
    }
    return () => source.close()
  }
}

export class Client {
  constructor(readonly transport: Transport) {}

  queue(): Promise<QueueView> {
    return this.transport.get<QueueView>('/queue')
  }

  topic(id: TopicId, expanded: TopicId[]): Promise<TopicDetail> {
    const query = expanded.length > 0 ? `?expanded=${expanded.map(encodeURIComponent).join(',')}` : ''
    return this.transport.get<TopicDetail>(`/topics/${encodeURIComponent(id)}${query}`)
  }

  search(query: string): Promise<{ topics: Topic[] }> {
    return this.transport.get<{ topics: Topic[] }>(`/topics?query=${encodeURIComponent(query)}`)
  }

  status(): Promise<ServerStatus> {
    return this.transport.get<ServerStatus>('/status')
  }

  act(request: ActionRequest): Promise<ActionResult> {
    return this.transport.post<ActionResult>('/actions', request)
  }
}
