import type { ComponentType } from 'react'
import { SlackConversation, SlackHome, SlackThread } from './Slack.tsx'
import { Status } from './primitives.tsx'
import { Generic, Tasks } from './Tasks.tsx'
import type { FocusProps } from './types.ts'

/** The focus view for each entity type. Unknown types fall back to Generic. */
const views: Record<string, ComponentType<FocusProps>> = {
  'slack.home': SlackHome,
  'slack.conversation': SlackConversation,
  'slack.message': SlackThread,
  'tasks.home': Tasks,
  task: Tasks,
}

export function Focus(props: FocusProps) {
  if (!props.focus.entity) return <div className="pane centred"><Status error={props.focus.error} /></div>
  const View = (props.focus.entity && views[props.focus.entity.type]) ?? Generic
  return <View {...props} />
}
