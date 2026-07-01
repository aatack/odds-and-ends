/** 0=add, 1=remove, 2=move forward (toward index 0), 3=move backward (toward end) */
export type LinkAction = 0 | 1 | 2 | 3

export interface BaseEvent {
  timestamp: number // Unix ms
  author: string
}

export interface ValueEvent extends BaseEvent {
  type: 'value'
  entityId: string
  key: string
  value: unknown // any JSON-serialisable value
}

export interface LinkEvent extends BaseEvent {
  type: 'link'
  sourceId: string
  destinationId: string
  action: LinkAction
}

export type AppEvent = ValueEvent | LinkEvent
