import { contextBridge, ipcRenderer } from 'electron'

contextBridge.exposeInMainWorld('core', {
  invoke: (name: string, args: unknown) => ipcRenderer.invoke('invoke', name, args),
  openExternal: (url: string) => ipcRenderer.invoke('openExternal', url),
  onChange: (listener: () => void) => {
    const handler = () => listener()
    ipcRenderer.on('changed', handler)
    return () => ipcRenderer.off('changed', handler)
  },
})
