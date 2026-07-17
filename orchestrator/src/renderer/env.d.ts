/// <reference types="vite/client" />

import type { OrchestratorApi } from "../shared/api";

declare global {
  interface Window {
    api: OrchestratorApi;
  }
}

export {};
