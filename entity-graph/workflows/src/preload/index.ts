import { contextBridge, ipcRenderer } from 'electron'
import { API_METHODS, type WorkflowsAPI } from '../shared/types'

// Every call goes to the backend, through the main process.
const calls = Object.fromEntries(API_METHODS.map((name) => [name, (...args: unknown[]) => ipcRenderer.invoke('rpc', name, args)]))

const on = (channel: string) => (cb: () => void) => {
  const listener = () => cb()
  ipcRenderer.on(channel, listener)
  return () => ipcRenderer.removeListener(channel, listener)
}

const api = { ...calls, onBack: on('back'), onForward: on('forward'), onChange: on('changed') } as unknown as WorkflowsAPI

contextBridge.exposeInMainWorld('workflows', api)
