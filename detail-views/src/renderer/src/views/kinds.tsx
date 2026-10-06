import { CheckFull, CheckPill, CheckRow, GhItemFull, GhItemPill, GhItemRow, GitHubHome, GitHubHomePill, GitHubPr, LocalApprovalPill, PrPill, PrRow } from './GitHub.tsx'
import type { Kinds } from './kindTypes.ts'
import {
  ConversationPill,
  ConversationRow,
  MessagePill,
  SlackConversation,
  SlackHome,
  SlackHomePill,
  SlackMessageRow,
  SlackThread,
  UserPill,
} from './Slack.tsx'
import { Generic, GenericRow, TaskPill, TaskRow, Tasks, TasksHomePill } from './Tasks.tsx'

/**
 * Every item type's three views. Typed against `itemTypes` in core, so a type
 * without all three fails the type check.
 */
export const kinds: Kinds = {
  'slack.home': { Full: SlackHome, Row: GenericRow, Pill: SlackHomePill },
  'slack.conversation': { Full: SlackConversation, Row: ConversationRow, Pill: ConversationPill },
  'slack.message': { Full: SlackThread, Row: SlackMessageRow, Pill: MessagePill },
  'slack.user': { Full: Generic, Row: GenericRow, Pill: UserPill },
  'github.home': { Full: GitHubHome, Row: GenericRow, Pill: GitHubHomePill },
  'github.pr': { Full: GitHubPr, Row: PrRow, Pill: PrPill },
  'github.check': { Full: CheckFull, Row: CheckRow, Pill: CheckPill },
  'github.item': { Full: GhItemFull, Row: GhItemRow, Pill: GhItemPill },
  'github.localApproval': { Full: Generic, Row: GenericRow, Pill: LocalApprovalPill },
  'tasks.home': { Full: Tasks, Row: GenericRow, Pill: TasksHomePill },
  task: { Full: Tasks, Row: TaskRow, Pill: TaskPill },
}
