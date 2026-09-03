import Store from 'electron-store'

export interface AppConfig {
  /** Who writes made from this window are recorded as. */
  user: string
}

/**
 * The app's own settings, and now nearly nothing: what used to be here — every
 * server, every saved token, which source was open — is the graph of pensives,
 * which is a SQLite file of its own (`pensive/graph.db` under `userData`).
 */
export const store = new Store<AppConfig>({ defaults: { user: 'anonymous' } })
