import { contextBridge, ipcRenderer } from 'electron'
import type { WorkflowsAPI } from '../shared/types'

const call = (name: string) => (...args: unknown[]) => ipcRenderer.invoke(name, ...args)

const api: WorkflowsAPI = {
  overview: call('overview') as WorkflowsAPI['overview'],
  detail: call('detail') as WorkflowsAPI['detail'],
  catalog: call('catalog') as WorkflowsAPI['catalog'],
  next: call('next') as WorkflowsAPI['next'],
  create: call('create') as WorkflowsAPI['create'],
  handOff: call('handOff') as WorkflowsAPI['handOff'],
  approve: call('approve') as WorkflowsAPI['approve'],
  close: call('close') as WorkflowsAPI['close'],
  handledOutside: call('handledOutside') as WorkflowsAPI['handledOutside'],
  takeOver: call('takeOver') as WorkflowsAPI['takeOver'],
  pollSources: call('pollSources') as WorkflowsAPI['pollSources'],
  integrations: call('integrations') as WorkflowsAPI['integrations'],
  addSlack: call('addSlack') as WorkflowsAPI['addSlack'],
  removeSlack: call('removeSlack') as WorkflowsAPI['removeSlack'],
  mcp: call('mcp') as WorkflowsAPI['mcp'],
  regenerateMcpToken: call('regenerateMcpToken') as WorkflowsAPI['regenerateMcpToken'],
  setMcpPort: call('setMcpPort') as WorkflowsAPI['setMcpPort'],
  onChange: (cb) => {
    const listener = () => cb()
    ipcRenderer.on('changed', listener)
    return () => ipcRenderer.removeListener('changed', listener)
  },
}

contextBridge.exposeInMainWorld('workflows', api)
