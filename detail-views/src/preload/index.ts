import { contextBridge, ipcRenderer } from 'electron'

contextBridge.exposeInMainWorld('core', {
  invoke: (name: string, args: unknown) => ipcRenderer.invoke('invoke', name, args),
  slackSend: (args: { topic: string; on: string; text: string }) => ipcRenderer.invoke('slackSend', args),
  openExternal: (url: string) => ipcRenderer.invoke('openExternal', url),
  copy: (text: string) => ipcRenderer.invoke('copy', text),
  onChange: (listener: (changed: string[] | null) => void) => {
    const handler = (_event: unknown, changed: string[] | null) => listener(changed)
    ipcRenderer.on('changed', handler)
    return () => ipcRenderer.off('changed', handler)
  },
})
