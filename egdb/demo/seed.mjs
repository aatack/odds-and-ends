// Fill an egdb server with an outline to try the demo on:
//   node seed.mjs [url] [depth] [fanout]
// It makes a branch called "main" (or uses it) and writes a tree under `root`.

// It calls the HTTP API directly, so it needs no build of the client.

const url = process.argv[2] ?? 'http://127.0.0.1:7878'
const depth = Number(process.argv[3] ?? 5)
const fanout = Number(process.argv[4] ?? 8)
const call = async (path, body) => {
  const response = await fetch(url + path, body ? { method: 'POST', headers: { 'content-type': 'application/json' }, body: JSON.stringify(body) } : {})
  if (!response.ok) throw new Error(`${path}: ${await response.text()}`)
  return response.json()
}
const op = {
  set: (entity, key, value) => ({ set: { entity, key, value } }),
  link: (source, destination, link_type, sort) => ({ link: { source, destination, link_type, sort } }),
}

const branches = await call('/branches')
const branch = branches.find((b) => b.name === 'main')?.id ?? (await call('/create_branch', { name: 'main', parents: [] })).branch

const words = ['plan', 'draft', 'review', 'ship', 'fix', 'idea', 'notes', 'budget', 'design', 'test', 'call', 'read']
let batch = [op.set('root', 'type', 'list'), op.set('root', 'text', 'Everything')]
let count = 1
async function flush() {
  if (batch.length === 0) return
  await call('/write', { branch, author: 'seed', ops: batch })
  batch = []
}
async function grow(id, level) {
  if (level === depth) return
  for (let i = 0; i < fanout; i++) {
    const kid = `${id}.${i}`
    const leaf = level + 1 === depth || i % 3 === 2
    const text = `${words[(count * 7) % words.length]} ${kid.slice(5) || 'root'}`
    batch.push(op.set(kid, 'type', leaf ? 'task' : 'list'), op.set(kid, 'text', text), op.link(id, kid, 'child', { explicit: i }))
    count++
    if (batch.length > 3000) await flush()
    if (!leaf) await grow(kid, level + 1)
  }
}
await grow('root', 0)
await flush()
console.log(`branch ${branch}: ${count} entities under root`)
