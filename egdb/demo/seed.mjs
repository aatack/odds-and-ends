// Fill an egdb server with an outline to try the demo on:
//   node seed.mjs [url] [depth] [fanout]
// It makes a branch called "main" (or uses it) and writes a tree under `root`.

import { EgdbClient, op } from 'egdb-client'

const url = process.argv[2] ?? 'http://127.0.0.1:7878'
const depth = Number(process.argv[3] ?? 5)
const fanout = Number(process.argv[4] ?? 8)
const client = new EgdbClient({ url })

const branches = await client.branches()
const branch = branches.find((b) => b.name === 'main')?.id ?? (await client.createBranch('main'))

const words = ['plan', 'draft', 'review', 'ship', 'fix', 'idea', 'notes', 'budget', 'design', 'test', 'call', 'read']
let batch = [op.set('root', 'type', 'list'), op.set('root', 'text', 'Everything')]
let count = 1
async function flush() {
  if (batch.length === 0) return
  await client.write({ branch, author: 'seed', ops: batch })
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
