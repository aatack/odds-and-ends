import { contextBridge, ipcRenderer } from 'electron'
import { API_METHODS, type WorkflowsAPI } from '../shared/types'

// Every call goes to the backend, through the main process, as whoever is signed in here.
let user: string | null = null
const calls = Object.fromEntries(API_METHODS.map((name) => [name, (...args: unknown[]) => ipcRenderer.invoke('rpc', name, args, user)]))

const on = (channel: string) => (cb: () => void) => {
  const listener = () => cb()
  ipcRenderer.on(channel, listener)
  return () => ipcRenderer.removeListener(channel, listener)
}

const setUser = (u: string | null) => {
  user = u
}

const api = { ...calls, setUser, onBack: on('back'), onForward: on('forward'), onChange: on('changed') } as unknown as WorkflowsAPI

contextBridge.exposeInMainWorld('workflows', api)
