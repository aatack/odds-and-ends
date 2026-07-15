import type { EntityGraphAPI } from '../../preload/index'

declare global {
  interface Window {
    entityGraph: EntityGraphAPI
  }
}

export {}
