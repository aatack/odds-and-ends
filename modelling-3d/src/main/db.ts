/**
 * The store: one sqlite file holding every model, node and edge.
 *
 * Rows rather than a blob per model, so an edit says exactly what it changed
 * and the file is something you can open with the sqlite CLI. A node's literals
 * are the one thing kept as JSON, because their shape is the value type's
 * business and not the schema's.
 *
 * The engine is sqlite compiled to wasm rather than a native binding. A native
 * one has to be rebuilt against Electron's ABI on every install — a toolchain,
 * a postinstall step, and a binary that then no longer loads under plain node,
 * which would take these tests with it. The models are kilobytes, so the cost
 * of the wasm build (the database lives in memory and the whole file is written
 * out after a change) is nothing, and `npm install` stays instant.
 */

import { renameSync, writeFileSync } from 'fs'
import { readFile } from 'fs/promises'
import initSqlJs, { type Database } from 'sql.js'
import type { WriteOp } from '../core/api'
import type { Model, Models } from '../core/graph'
import { seedModels } from '../core/seed'

const SCHEMA = `
create table if not exists models (
  id    text primary key,
  name  text not null,
  ord   real not null
);
create table if not exists nodes (
  id        text primary key,
  model_id  text not null,
  transform text not null,
  x         real not null,
  y         real not null,
  data      text not null
);
create table if not exists edges (
  id            text primary key,
  model_id      text not null,
  source        text not null,
  source_output text not null,
  target        text not null,
  target_input  text not null
);
create index if not exists nodes_by_model on nodes(model_id);
create index if not exists edges_by_model on edges(model_id);
`

/** How long a burst of edits is allowed to gather before the file is written. */
const FLUSH_AFTER = 250

export class Store {
  private timer: ReturnType<typeof setTimeout> | null = null

  private constructor(
    private db: Database,
    private file: string,
  ) {}

  static async open(file: string): Promise<Store> {
    const SQL = await initSqlJs()
    const existing = await readFile(file).catch(() => null)
    const db = existing ? new SQL.Database(existing) : new SQL.Database()
    const store = new Store(db, file)
    db.run(SCHEMA)
    if (store.isEmpty()) {
      store.seed()
      store.flush()
    }
    return store
  }

  private isEmpty(): boolean {
    return this.rows<{ n: number }>('select count(*) as n from models')[0].n === 0
  }

  private seed(): void {
    this.apply(
      Object.values(seedModels()).flatMap((model): WriteOp[] => [
        { kind: 'model.create', model: { ...model, nodes: {}, edges: {} } },
        ...Object.values(model.nodes).map(
          (node): WriteOp => ({ kind: 'node.put', modelId: model.id, node }),
        ),
        ...Object.values(model.edges).map(
          (edge): WriteOp => ({ kind: 'edge.put', modelId: model.id, edge }),
        ),
      ]),
    )
  }

  private rows<T>(sql: string): T[] {
    const statement = this.db.prepare(sql)
    const out: T[] = []
    while (statement.step()) out.push(statement.getAsObject() as T)
    statement.free()
    return out
  }

  load(): Models {
    const models: Models = {}
    for (const row of this.rows<{ id: string; name: string; ord: number }>(
      'select * from models order by ord',
    )) {
      models[row.id] = { id: row.id, name: row.name, order: row.ord, nodes: {}, edges: {} }
    }
    for (const row of this.rows<{
      id: string
      model_id: string
      transform: string
      x: number
      y: number
      data: string
    }>('select * from nodes')) {
      const model = models[row.model_id]
      if (!model) continue
      model.nodes[row.id] = {
        id: row.id,
        transform: row.transform,
        x: row.x,
        y: row.y,
        data: parse(row.data),
      }
    }
    for (const row of this.rows<{
      id: string
      model_id: string
      source: string
      source_output: string
      target: string
      target_input: string
    }>('select * from edges')) {
      const model = models[row.model_id]
      if (!model) continue
      model.edges[row.id] = {
        id: row.id,
        source: row.source,
        sourceOutput: row.source_output,
        target: row.target,
        targetInput: row.target_input,
      }
    }
    return models
  }

  /** A burst of edits as one transaction, so a half-wired node never lands. */
  apply(ops: WriteOp[]): void {
    if (ops.length === 0) return
    this.db.run('begin')
    try {
      for (const op of ops) this.applyOne(op)
      this.db.run('commit')
    } catch (error) {
      this.db.run('rollback')
      throw error
    }
    this.schedule()
  }

  private applyOne(op: WriteOp): void {
    const db = this.db
    switch (op.kind) {
      case 'model.create':
        db.run('insert or replace into models (id, name, ord) values (?, ?, ?)', [
          op.model.id,
          op.model.name,
          op.model.order,
        ])
        return
      case 'model.rename':
        db.run('update models set name = ? where id = ?', [op.name, op.id])
        return
      case 'model.delete':
        db.run('delete from nodes where model_id = ?', [op.id])
        db.run('delete from edges where model_id = ?', [op.id])
        db.run('delete from models where id = ?', [op.id])
        return
      case 'node.put':
        db.run(
          'insert or replace into nodes (id, model_id, transform, x, y, data) values (?, ?, ?, ?, ?, ?)',
          [op.node.id, op.modelId, op.node.transform, op.node.x, op.node.y, JSON.stringify(op.node.data)],
        )
        return
      case 'node.move':
        db.run('update nodes set x = ?, y = ? where id = ?', [op.x, op.y, op.id])
        return
      case 'node.data':
        db.run('update nodes set data = ? where id = ?', [JSON.stringify(op.data), op.id])
        return
      case 'node.delete':
        db.run('delete from edges where source = ? or target = ?', [op.id, op.id])
        db.run('delete from nodes where id = ?', [op.id])
        return
      case 'edge.put':
        db.run(
          'insert or replace into edges (id, model_id, source, source_output, target, target_input) values (?, ?, ?, ?, ?, ?)',
          [op.edge.id, op.modelId, op.edge.source, op.edge.sourceOutput, op.edge.target, op.edge.targetInput],
        )
        return
      case 'edge.delete':
        db.run('delete from edges where id = ?', [op.id])
        return
    }
  }

  private schedule(): void {
    if (this.timer) return
    this.timer = setTimeout(() => {
      this.timer = null
      this.flush()
    }, FLUSH_AFTER)
  }

  /** Write the database out, through a temporary file so a crash can't halve it. */
  flush(): void {
    if (this.timer) {
      clearTimeout(this.timer)
      this.timer = null
    }
    const temporary = `${this.file}.writing`
    writeFileSync(temporary, Buffer.from(this.db.export()))
    renameSync(temporary, this.file)
  }

  close(): void {
    this.flush()
    this.db.close()
  }
}

function parse(text: string): Record<string, unknown> {
  try {
    const value = JSON.parse(text)
    return value && typeof value === 'object' ? (value as Record<string, unknown>) : {}
  } catch {
    return {}
  }
}

export type { Model }
