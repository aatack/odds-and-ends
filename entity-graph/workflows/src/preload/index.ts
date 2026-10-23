import { contextBridge, ipcRenderer } from 'electron'
import type { WorkflowsAPI } from '../shared/types'

const call = (name: string) => (...args: unknown[]) => ipcRenderer.invoke(name, ...args)

const api: WorkflowsAPI = {
  overview: call('overview') as WorkflowsAPI['overview'],
  detail: call('detail') as WorkflowsAPI['detail'],
  next: call('next') as WorkflowsAPI['next'],
  create: call('create') as WorkflowsAPI['create'],
  dispatch: call('dispatch') as WorkflowsAPI['dispatch'],
  execute: call('execute') as WorkflowsAPI['execute'],
  close: call('close') as WorkflowsAPI['close'],
  handledOutside: call('handledOutside') as WorkflowsAPI['handledOutside'],
  setState: call('setState') as WorkflowsAPI['setState'],
  takeOver: call('takeOver') as WorkflowsAPI['takeOver'],
  pollSources: call('pollSources') as WorkflowsAPI['pollSources'],
  onChange: (cb) => {
    const listener = () => cb()
    ipcRenderer.on('changed', listener)
    return () => ipcRenderer.removeListener('changed', listener)
  },
}

contextBridge.exposeInMainWorld('workflows', api)
