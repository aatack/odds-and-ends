import { contextBridge, ipcRenderer } from "electron";
import type { OrchestratorApi } from "../shared/api";

const api: OrchestratorApi = {
  snapshot: () => ipcRenderer.invoke("db:snapshot"),
  task: {
    create: (input) => ipcRenderer.invoke("task:create", input),
    update: (id, patch) => ipcRenderer.invoke("task:update", { id, patch }),
  },
  item: {
    setArchived: (ref, at) => ipcRenderer.invoke("item:setArchived", { ref, at }),
  },
  link: {
    create: (source, dest) => ipcRenderer.invoke("link:create", { source, dest }),
    delete: (id) => ipcRenderer.invoke("link:delete", { id }),
  },
};

contextBridge.exposeInMainWorld("api", api);
