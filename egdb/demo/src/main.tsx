import { Egdb, EgdbClient } from 'egdb-client'
import { EgdbProvider } from 'egdb-client/react'
import { createRoot } from 'react-dom/client'
import { App } from './App'

const client = new EgdbClient({ url: '/api' })
const root = createRoot(document.getElementById('root')!)

const branches = await client.branches().catch(() => null)
const main = branches?.find((b) => b.name === 'main')
if (!main) {
  root.render(<p style={{ font: '14px system-ui', padding: 24 }}>No server or no "main" branch: start egdb-server and run <code>npm run seed</code>.</p>)
} else {
  const egdb = new Egdb({ client, branch: main.id, author: 'demo', onError: (e) => console.error(e) })
  root.render(
    <EgdbProvider egdb={egdb}>
      <App />
    </EgdbProvider>,
  )
}
