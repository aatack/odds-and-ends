import { contextBridge } from 'electron'

const api = { serverUrl: process.env.CONSWAP_SERVER_URL ?? 'http://127.0.0.1:4319' }

contextBridge.exposeInMainWorld('conswap', api)

export type ConswapBridge = typeof api
