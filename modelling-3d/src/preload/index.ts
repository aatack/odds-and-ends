import { contextBridge, ipcRenderer } from 'electron'
import type { ModellingAPI, WriteOp } from '../core/api'

const api: ModellingAPI = {
  load: () => ipcRenderer.invoke('models:load'),
  write: (ops: WriteOp[]) => ipcRenderer.invoke('models:write', ops),
  saveModel: (name, glb) => ipcRenderer.invoke('models:save', name, glb),
  revealFile: (path) => ipcRenderer.invoke('file:reveal', path),
  openFile: (path) => ipcRenderer.invoke('file:open', path),
}

contextBridge.exposeInMainWorld('modelling', api)

declare global {
  interface Window {
    modelling: ModellingAPI
  }
}
