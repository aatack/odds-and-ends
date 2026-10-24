import { contextBridge, ipcRenderer } from 'electron'
import type { WorkflowsAPI } from '../shared/types'

const call = (name: string) => (...args: unknown[]) => ipcRenderer.invoke(name, ...args)

const api: WorkflowsAPI = {
  overview: call('overview') as WorkflowsAPI['overview'],
  detail: call('detail') as WorkflowsAPI['detail'],
  eventDetail: call('eventDetail') as WorkflowsAPI['eventDetail'],
  catalog: call('catalog') as WorkflowsAPI['catalog'],
  next: call('next') as WorkflowsAPI['next'],
  viewed: call('viewed') as WorkflowsAPI['viewed'],
  skipped: call('skipped') as WorkflowsAPI['skipped'],
  create: call('create') as WorkflowsAPI['create'],
  send: call('send') as WorkflowsAPI['send'],
  approve: call('approve') as WorkflowsAPI['approve'],
  cancel: call('cancel') as WorkflowsAPI['cancel'],
  close: call('close') as WorkflowsAPI['close'],
  attach: call('attach') as WorkflowsAPI['attach'],
  directives: call('directives') as WorkflowsAPI['directives'],
  addDirective: call('addDirective') as WorkflowsAPI['addDirective'],
  setPriority: call('setPriority') as WorkflowsAPI['setPriority'],
  summarise: call('summarise') as WorkflowsAPI['summarise'],
  customActions: call('customActions') as WorkflowsAPI['customActions'],
  saveCustomAction: call('saveCustomAction') as WorkflowsAPI['saveCustomAction'],
  removeCustomAction: call('removeCustomAction') as WorkflowsAPI['removeCustomAction'],
  runCustomAction: call('runCustomAction') as WorkflowsAPI['runCustomAction'],
  generateCustomAction: call('generateCustomAction') as WorkflowsAPI['generateCustomAction'],
  handledOutside: call('handledOutside') as WorkflowsAPI['handledOutside'],
  pollSources: call('pollSources') as WorkflowsAPI['pollSources'],
  integrations: call('integrations') as WorkflowsAPI['integrations'],
  addSlack: call('addSlack') as WorkflowsAPI['addSlack'],
  removeSlack: call('removeSlack') as WorkflowsAPI['removeSlack'],
  mcp: call('mcp') as WorkflowsAPI['mcp'],
  regenerateMcpToken: call('regenerateMcpToken') as WorkflowsAPI['regenerateMcpToken'],
  setMcpPort: call('setMcpPort') as WorkflowsAPI['setMcpPort'],
  onBack: (cb) => {
    const listener = () => cb()
    ipcRenderer.on('back', listener)
    return () => ipcRenderer.removeListener('back', listener)
  },
  onForward: (cb) => {
    const listener = () => cb()
    ipcRenderer.on('forward', listener)
    return () => ipcRenderer.removeListener('forward', listener)
  },
  onChange: (cb) => {
    const listener = () => cb()
    ipcRenderer.on('changed', listener)
    return () => ipcRenderer.removeListener('changed', listener)
  },
}

contextBridge.exposeInMainWorld('workflows', api)
