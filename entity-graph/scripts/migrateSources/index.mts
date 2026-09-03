// One-off: turn the old server configuration into a graph of pensive nodes.
//
// Before this refactor, a source was a row in a server's config DB and a server
// was a child process the app spawned. Both are gone; what is left is a drawing
// the app owns. Rather than retyping every path, this reads the old
// configuration where the app left it and writes the equivalent nodes:
//
// - one `sqlite` node per old sqlite source, with its path made absolute
// - one `combined` node over all of them, writing to whichever source was open
// - that combiner plugged into the desktop node, so the outliner opens on it
//
// It refuses to run over a graph that already has nodes in it, so it cannot
// silently double anything. Nothing is deleted: the old config files are left
// exactly where they are.
//
//   npm run rebuild:node && npx tsx scripts/migrateSources/index.mts [databasesDir]
//   npm run rebuild:electron        # …and put better-sqlite3 back for the app
//
// `databasesDir` is where the old relative paths were resolved against —
// `server/databases` in the checkout the app was running from. Defaults to
// `server/databases` beside this repository, which is where it used to be.

import Database from 'better-sqlite3'
import { existsSync, readFileSync } from 'fs'
import { homedir } from 'os'
import { isAbsolute, join, resolve } from 'path'
import { GraphDb, DESKTOP_NODE_ID } from '../../src/main/pensive/graph'

/** Where Electron keeps this app's own files, per platform. */
function userData(): string {
  const home = homedir()
  if (process.platform === 'darwin') return join(home, 'Library', 'Application Support', 'entity-graph')
  if (process.platform === 'win32') return join(process.env.APPDATA ?? home, 'entity-graph')
  return join(process.env.XDG_CONFIG_HOME ?? join(home, '.config'), 'entity-graph')
}

interface OldConfig {
  servers?: { id: string; label: string }[]
  currentSource?: { serverId: string; sourceId: string; label: string } | null
}

interface OldSource {
  id: string
  label: string
  path: string
}

/**
 * Every sqlite source in one old server's config DB, in the order it was made.
 * A DB that turns out not to be one is skipped rather than fatal — a half-set-up
 * server should not stop the rest from coming across.
 */
function sourcesOf(configDb: string): OldSource[] {
  const db = new Database(configDb, { readonly: true })
  try {
    const rows = db
      .prepare<[], { id: string; label: string; type: string; config_json: string }>(
        'SELECT id, label, type, config_json FROM sources ORDER BY created_at',
      )
      .all()
    return rows
      .filter((r) => r.type === 'sqlite')
      .map((r) => ({
        id: r.id,
        label: r.label,
        path: (JSON.parse(r.config_json) as { path: string }).path,
      }))
  } catch (e) {
    console.warn(`- ${configDb}: ${e instanceof Error ? e.message : String(e)}, skipped`)
    return []
  } finally {
    db.close()
  }
}

function main(): void {
  const root = userData()
  const databases = resolve(
    process.argv[2] ?? join(import.meta.dirname ?? '.', '..', '..', 'server', 'databases'),
  )

  const configPath = join(root, 'config.json')
  if (!existsSync(configPath)) {
    console.error(`Nothing to migrate: ${configPath} isn't there.`)
    process.exit(1)
  }
  const old = JSON.parse(readFileSync(configPath, 'utf8')) as OldConfig

  const graph = new GraphDb(join(root, 'pensive', 'graph.db'))
  const existing = graph.nodes().filter((n) => n.id !== DESKTOP_NODE_ID)
  if (existing.length) {
    console.error(
      `${existing.length} node(s) are already drawn. Delete them, or delete ` +
        `${join(root, 'pensive', 'graph.db')}, and run this again.`,
    )
    process.exit(1)
  }

  // One column of sqlite nodes, in the order the sources were made.
  let y = 120
  const added: { oldId: string; nodeId: string; label: string }[] = []
  for (const server of old.servers ?? []) {
    const configDb = join(root, 'servers', server.id, 'config.db')
    if (!existsSync(configDb)) {
      console.warn(`- ${server.label}: no config DB at ${configDb}, skipped`)
      continue
    }
    for (const source of sourcesOf(configDb)) {
      const path = isAbsolute(source.path) ? source.path : join(databases, source.path)
      if (!existsSync(path)) console.warn(`  (${source.label}: ${path} isn't there yet)`)
      const node = graph.addNode({
        label: source.label,
        x: -520,
        y,
        config: { kind: 'sqlite', path },
      })
      added.push({ oldId: source.id, nodeId: node.id, label: source.label })
      console.log(`+ ${source.label} → ${path}`)
      y += 200
    }
  }

  if (!added.length) {
    console.error('No sqlite sources found. Nothing written.')
    process.exit(1)
  }

  // The arrangement the old app could not express: all of them read as one, with
  // edits going where the source that was open used to take them.
  const openId = old.currentSource?.sourceId
  const writeTo = added.find((a) => a.oldId === openId) ?? added[0]
  const combined = graph.addNode({
    label: 'Everything',
    x: -240,
    y: 120,
    config: { kind: 'combined', writeTo: writeTo.nodeId },
  })
  for (const { nodeId } of added) graph.addEdge(nodeId, combined.id)
  graph.addEdge(combined.id, DESKTOP_NODE_ID)
  graph.close()

  console.log(
    `\nDrew ${added.length} store(s) into "Everything", writing to ${writeTo.label}, ` +
      `and plugged that into the app. Open the Sources page to check it.`,
  )
}

main()
