/**
 * The seam between the renderer and everything outside it.
 *
 * Writes are a list of small operations rather than a method each, which keeps
 * the IPC surface to two calls, lets a burst (dropping a node and wiring it up)
 * commit in one transaction, and makes the store's effects something a test can
 * simply collect in an array.
 */

import type { GraphEdge, GraphNode, Model, Models } from './graph'

export type WriteOp =
  | { kind: 'model.create'; model: Model }
  | { kind: 'model.rename'; id: string; name: string }
  | { kind: 'model.delete'; id: string }
  | { kind: 'node.put'; modelId: string; node: GraphNode }
  | { kind: 'node.move'; id: string; x: number; y: number }
  | { kind: 'node.data'; id: string; data: Record<string, unknown> }
  | { kind: 'node.delete'; id: string }
  | { kind: 'edge.put'; modelId: string; edge: GraphEdge }
  | { kind: 'edge.delete'; id: string }

/** Somewhere the models can be kept. Nothing on screen waits on it. */
export interface Persistence {
  write(ops: WriteOp[]): void
}

/** For tests and for the moment before a store is connected to anything. */
export const noPersistence: Persistence = { write: () => {} }

export interface ModellingAPI {
  /** Everything in the store, read once at startup. */
  load(): Promise<Models>
  write(ops: WriteOp[]): Promise<void>
  /**
   * Write a `.glb` into the user's downloads folder under a name that is not
   * taken, returning the path it went to.
   */
  saveModel(name: string, glb: Uint8Array): Promise<string>
  /** Show a saved file in the desktop's file manager. */
  revealFile(path: string): Promise<void>
  /** Hand a saved file to whatever the desktop opens `.glb` with. */
  openFile(path: string): Promise<void>
}

export type { GraphEdge, GraphNode, Model, Models }
