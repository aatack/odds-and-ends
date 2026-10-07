import { CheckOverview, CheckPill, CheckRow, GhItemOverview, GhItemPill, GhItemRow, GitHubHomePill, LocalApprovalPill, PrOverview, PrPill, PrRow } from './GitHub.tsx'
import type { Kinds } from './kindTypes.ts'
import { HomePill, NoteOverview, NotePill, NoteRow, PillOverview, PillRow, TaskOverview, TaskPill, TaskRow } from './Notes.tsx'
import {
  ConversationOverview,
  ConversationPill,
  ConversationRow,
  MessageOverview,
  MessagePill,
  SlackHomeOverview,
  SlackHomePill,
  SlackMessageRow,
  UserPill,
  WatchPill,
} from './Slack.tsx'

/**
 * Every item type's views. Typed against `itemTypes` in core, so a type
 * without its pill, overview and row fails the type check. `Detail` is
 * optional: without it a view is the overview and the tree.
 */
export const kinds: Kinds = {
  'slack.home': { Pill: SlackHomePill, Overview: SlackHomeOverview, Row: PillRow },
  'slack.conversation': { Pill: ConversationPill, Overview: ConversationOverview, Row: ConversationRow },
  'slack.message': { Pill: MessagePill, Overview: MessageOverview, Row: SlackMessageRow },
  'slack.user': { Pill: UserPill, Overview: PillOverview, Row: PillRow },
  'slack.watch': { Pill: WatchPill, Overview: PillOverview, Row: PillRow },
  'github.home': { Pill: GitHubHomePill, Overview: PillOverview, Row: PillRow },
  'github.pr': { Pill: PrPill, Overview: PrOverview, Row: PrRow },
  'github.check': { Pill: CheckPill, Overview: CheckOverview, Row: CheckRow },
  'github.item': { Pill: GhItemPill, Overview: GhItemOverview, Row: GhItemRow },
  'github.localApproval': { Pill: LocalApprovalPill, Overview: PillOverview, Row: PillRow },
  'tasks.home': { Pill: HomePill, Overview: PillOverview, Row: PillRow },
  task: { Pill: TaskPill, Overview: TaskOverview, Row: TaskRow },
  note: { Pill: NotePill, Overview: NoteOverview, Row: NoteRow },
}
