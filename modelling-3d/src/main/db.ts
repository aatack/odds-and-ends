/**
 * The store: one sqlite file holding every model, node and edge.
 *
 * Rows rather than a blob per model, so an edit writes only what changed and
 * the file stays something you can open and read with the sqlite CLI. A node's
 * literals are the one thing kept as JSON, because their shape is the value
 * type's business and not the schema's.
 */

import Database from 'better-sqlite3'
import type { Model, Models } from '../core/graph'
import type { WriteOp } from '../core/api'
import { seedModels } from '../core/seed'

const SCHEMA = `
create table if not exists models (
  id    text primary key,
  name  text not null,
  ord   real not null
);
create table if not exists nodes (
  id        text primary key,
  model_id  text not null references models(id) on delete cascade,
  transform text not null,
  x         real not null,
  y         real not null,
  data      text not null
);
create table if not exists edges (
  id            text primary key,
  model_id      text not null references models(id) on delete cascade,
  source        text not null,
  source_output text not null,
  target        text not null,
  target_input  text not null
);
create index if not exists nodes_by_model on nodes(model_id);
create index if not exists edges_by_model on edges(model_id);
`

export class Store {
  private db: Database.Database

  constructor(file: string) {
    this.db = new Database(file)
    this.db.pragma('journal_mode = WAL')
    this.db.pragma('foreign_keys = ON')
    this.db.exec(SCHEMA)
    if (this.isEmpty()) this.seed()
  }

  private isEmpty(): boolean {
    const row = this.db.prepare('select count(*) as n from models').get() as { n: number }
    return row.n === 0
  }

  private seed(): void {
    const models = seedModels()
    this.apply(
      Object.values(models).flatMap((model): WriteOp[] => [
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

  load(): Models {
    const models: Models = {}
    for (const row of this.db.prepare('select * from models order by ord').all() as {
      id: string
      name: string
      ord: number
    }[]) {
      models[row.id] = { id: row.id, name: row.name, order: row.ord, nodes: {}, edges: {} }
    }
    for (const row of this.db.prepare('select * from nodes').all() as {
      id: string
      model_id: string
      transform: string
      x: number
      y: number
      data: string
    }[]) {
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
    for (const row of this.db.prepare('select * from edges').all() as {
      id: string
      model_id: string
      source: string
      source_output: string
      target: string
      target_input: string
    }[]) {
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
    const run = this.db.transaction((batch: WriteOp[]) => {
      for (const op of batch) this.applyOne(op)
    })
    run(ops)
  }

  private applyOne(op: WriteOp): void {
    const db = this.db
    switch (op.kind) {
      case 'model.create':
        db.prepare('insert or replace into models (id, name, ord) values (?, ?, ?)').run(
          op.model.id,
          op.model.name,
          op.model.order,
        )
        return
      case 'model.rename':
        db.prepare('update models set name = ? where id = ?').run(op.name, op.id)
        return
      case 'model.delete':
        db.prepare('delete from nodes where model_id = ?').run(op.id)
        db.prepare('delete from edges where model_id = ?').run(op.id)
        db.prepare('delete from models where id = ?').run(op.id)
        return
      case 'node.put':
        db.prepare(
          'insert or replace into nodes (id, model_id, transform, x, y, data) values (?, ?, ?, ?, ?, ?)',
        ).run(op.node.id, op.modelId, op.node.transform, op.node.x, op.node.y, JSON.stringify(op.node.data))
        return
      case 'node.move':
        db.prepare('update nodes set x = ?, y = ? where id = ?').run(op.x, op.y, op.id)
        return
      case 'node.data':
        db.prepare('update nodes set data = ? where id = ?').run(JSON.stringify(op.data), op.id)
        return
      case 'node.delete':
        db.prepare('delete from edges where source = ? or target = ?').run(op.id, op.id)
        db.prepare('delete from nodes where id = ?').run(op.id)
        return
      case 'edge.put':
        db.prepare(
          'insert or replace into edges (id, model_id, source, source_output, target, target_input) values (?, ?, ?, ?, ?, ?)',
        ).run(op.edge.id, op.modelId, op.edge.source, op.edge.sourceOutput, op.edge.target, op.edge.targetInput)
        return
      case 'edge.delete':
        db.prepare('delete from edges where id = ?').run(op.id)
        return
    }
  }

  close(): void {
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
