// The item types. What each takes and resolves with is the contract
// docs/items.md sets out; keep the two in step.

import { z } from 'zod'
import type { ItemType } from './integrations'
import { slackChannelParams, slackMessageParams } from './slack'


/** What a topic's first items (and an older build's task's `items`) look like. */
const firstItems = z.array(z.object({ type: z.string(), params: z.record(z.unknown()), ref: z.string().optional() }))

export const TOPIC_DOC =
  'A topic: what is said, a thing to be done, or a note in a tree; one type for all of them. `text` is what it says (its first line is ' +
  'its name). `open` is its status: true while open (a thing to be done, or a checkbox), false once closed, left out for none (a comment). ' +
  'It holds two lists: its comments (its chat: what is said in it, in order) and its notes (a tree of topics); `display` says which its ' +
  'view shows. What you add in a topic goes in its comments. Pinned items show beside it. Its owner (whoever made it, if a person; else the ' +
  "owner of the topic it is in) is who it goes back to. To split work off, add an open topic (`open: true`) with its first comments in " +
  '`comments` and who has it in `assignees`. The old types `task` (title, items) and `note` (text) are taken as topics.'

export const topic: ItemType = {
  type: 'topic',
  description: TOPIC_DOC,
  params: {
    text: z.string().describe('What it says; its first line is its name.'),
    open: z.boolean().optional().describe('Its status: true while open, false once closed; left out, none.'),
    section: z.boolean().optional().describe('A heading, in a tree of notes.'),
    display: z.enum(['comments', 'notes']).optional().describe('What its view shows: its comments (a chat; the default) or its notes (a tree).'),
    comments: firstItems.optional().describe('Comments to put in it as it is made. Not kept in its values.'),
    notes: firstItems.optional().describe('Notes to put in it as it is made. Not kept in its values.'),
    key: z.string().optional().describe('Identifies it, so that it is added only once.'),
    detached: z.boolean().optional().describe('Put it in no topic, rather than the one it is added in: for work unrelated to it.'),
    assignees: z
      .array(z.string())
      .optional()
      .describe('Who has it from the start: people by username, bots by name or id. Not kept in its values: they are links.'),
  },
  data: '—',
  blocking: false,
  bots: true,
  describe: (p) => String(p.text ?? ''),
}

/**
 * An older build's types, as what is added today: a task is an open topic (its title its text, its items its comments),
 * a note a topic. Answers the spec as it is, for any other type.
 */
export function asTopic(s: { type: string; params: Record<string, unknown> }): { type: string; params: Record<string, unknown> } {
  if (s.type === 'task') {
    const { title, items, events, ...rest } = s.params ?? {}
    return { type: 'topic', params: { text: rest.text ?? title, open: rest.open ?? true, ...rest, ...((items ?? events) ? { comments: items ?? events } : {}) } }
  }
  if (s.type === 'note') return { type: 'topic', params: s.params ?? {} }
  return s
}

export const approval: ItemType = {
  type: 'approval',
  description:
    'An unsafe action for the human to approve, change, or turn down. While only approvals are open, the item is the human\'s turn.',
  params: {
    action: z.string().describe('The unsafe action, by name: see unsafe_actions.'),
    inputs: z.record(z.unknown()).describe('Its inputs, filled in as far as you can. The human may change them.'),
    reason: z.string().optional().describe('Why, in a line.'),
  },
  data: '{ ran: true, inputs, changed: string[], output }',
  blocking: true,
  human: true,
  bots: true,
  describe: (p) => String(p.reason ?? `Run ${p.action}?`),
  said: (p, d) => `Ran ${p.action}${(d.changed as string[] | undefined)?.length ? ` (changed ${(d.changed as string[]).join(', ')})` : ''}`,
}

/** What a script's run may return; null (or nothing) is `{}`. */
export const scriptResult = z
  .object({
    messages: z.array(z.object({ item: z.string().optional(), text: z.string() })).optional(),
    assign: z.array(z.object({ item: z.string(), to: z.array(z.string()) })).optional(),
    state: z.unknown().optional(),
  })
  .nullable()

export const SCRIPT_DOC =
  'A script: a bot without a model, running JavaScript. Give it a name, and topics as you would a bot (assign it, or @ it in a ' +
  'note: "[id]() 2d"); one script serves every topic it is given. Use one for repetitive work that needs no LLM: a snooze, ' +
  'watching a PR or a thread, a weekly chore. It costs nothing to run.\n' +
  '- Make it on its own: add_items with no item_id.\n' +
  '- It runs as a bot does: every minute the app gives it the first topic assigned to it (the longest held first) that it has not run ' +
  'on in the last cycle_minutes (30), or that was assigned to it again since. A run handles that one topic.\n' +
  '- Keep state on the topics it works on (set_value: any key, any value), and read it back with get_item (values).\n' +
  '- `source` is the body of an async function: `context` and `tools` are in scope; it may await and return. context is the ' +
  'script (id, name, its values, state, runs) with topics: [{ id, title }] (the topic it is run on), now (ISO time) and trigger. ' +
  'tools.<name>(args) calls any MCP tool; unsafe ones only when a person presses its play button. mentions(topicId, who = itself) ' +
  'answers the notes in a topic that mention it, newest first, each with text (what follows the mention); refs(itemId, type?) ' +
  'answers the items in an item, of one type if given.\n' +
  '- It returns (all optional): { messages: [{ item, text }] (item: its own chat if left out), assign: [{ item, to: [who] }], ' +
  'state (for its next run) }. It never ends on its own: a run that throws says so in its chat, and it runs again on its cycle.'

export const script: ItemType = {
  type: 'script',
  description: SCRIPT_DOC,
  params: {
    name: z.string().optional().describe('What it is called: assign to it or @ it by this.'),
    source: z.string().describe('The body of an async function, with context and tools in scope.'),
    description: z.string().optional().describe('What it does, in a line.'),
    cycle_minutes: z.number().min(0).optional().describe('How long before it runs on a topic again that it still holds: 30 if left out.'),
  },
  data: 'none: it runs till a person cancels it (older scripts may have { result, runs, logs } or { error }, from when they could end)',
  blocking: true,
  bots: true,
  describe: (p) => String(p.name ?? p.description ?? 'Script'),
  said: (p, d) => (d.error ? `The script failed: ${d.error}` : `${p.name ?? p.description ?? 'Script'}: done`),
}

export const bot: ItemType = {
  type: 'bot',
  description:
    'A bot: a name, instructions, and how it runs. It sits in no item, and is open while it lasts (cancelling it deletes it). ' +
    'Items are assigned to it (the assign action). Every minute, it is run on the first item assigned to it (the longest held first) ' +
    'that it has not run on in the last cycle_minutes (30), or that was assigned to it again since: one item a run.',
  params: {
    name: z.string().min(1).describe('What it is called: people @ it by this name.'),
    instructions: z.string().optional().describe('What it is for and how it works: given with every run.'),
    provider: z.string().optional().describe("Who runs it: 'claude' (the default)."),
    model: z.string().optional().describe('The model, as the provider names it (opus, sonnet, haiku or a full id): opus if left out.'),
    cycle_minutes: z.number().min(0).optional().describe('How long before it runs on an item again that it still holds: 30 if left out.'),
    folder: z.string().optional().describe('Its working folder; its own, under the app\'s, if left out.'),
    session_id: z.string().optional().describe("Its Claude session, set by the app once it first runs. Don't set it."),
  },
  data: '{ cancelled: true }, once deleted',
  blocking: true,
  bots: true,
  describe: (p) => `@${p.name}`,
}

export const SEARCH_DOC =
  "The search page's sections: one list for the whole app (the Tasks view, and every picker: @ in the chat input, and the rest). " +
  'It is kept on the one `search` item, which sits in no task; search_sections reads it, set_search_sections replaces it. Each section, in order:\n' +
  "- { kind: 'builtin', id, label? }: one of the finder's own: viewed (recently viewed), needsYou, withBots, waiting, closed (today).\n" +
  "- { kind: 'query', label, filter, limit? }: a filter, as egdb's find takes it ({ eq: { key, value } }, ne, lt/le/gt/ge, contains, " +
  'contains_ignore_case, prefix, in: { key, values }, is_null, not_null, and: [], or: [], not), over every item\'s values (type, ' +
  'title, text, description, resolved, the rest of its params) and these made from its links: parent (the task it sits in), assignees ' +
  '(who has it: eq matches any one), created_by, created_at. Rows are the items that match, the newest first.\n' +
  "- { kind: 'function', label, source }: JavaScript, run as a script's body is (context and tools in scope, no unsafe tools), with " +
  'context.text the filter typed so far; it returns rows: [{ id, type, left, right? }] (left and right in markdown; id is what Enter opens).\n' +
  'The label heads the section. What is typed filters every section at once; a function runs again as it changes.\n' +
  "Example: [{ kind: 'builtin', id: 'needsYou' }, { kind: 'query', label: 'Open scripts', filter: { and: [{ eq: { key: 'type', value: 'script' } }, " +
  "{ eq: { key: 'resolved', value: false } }] } }]"

export const search: ItemType = {
  type: 'search',
  description: SEARCH_DOC,
  params: { sections: z.array(z.record(z.unknown())).describe('The sections, in order: see the description.') },
  data: '—',
  blocking: false,
  bots: false,
  describe: () => 'Search page sections',
}

export const user: ItemType = {
  type: 'user',
  description:
    'A person who uses the app: its id is their username, which they sign in with. It sits in no item, and is open while it lasts. ' +
    'Items are assigned to people as to bots.',
  params: {
    username: z.string().regex(/^[a-z0-9][a-z0-9_.-]*$/, 'Lower-case letters, digits, dots, dashes and underscores.').describe('Their username: the id of this item.'),
    name: z.string().optional().describe('What they are called, if not their username.'),
  },
  data: '{ cancelled: true }, once removed',
  blocking: true,
  bots: true,
  describe: (p) => String(p.name || p.username),
}


export const link: ItemType = {
  type: 'link',
  description:
    "A web page, pulled into the task: a URL in a note becomes one (its id is the URL), so [url]() refers to it. Made by hand, " +
    'give it the url and, if you like, a title.',
  params: { url: z.string().regex(/^https?:\/\//, 'A web address: http(s)://…'), title: z.string().optional().describe('What it is, in a line; the URL if left out.') },
  data: '—',
  blocking: false,
  bots: true,
  describe: (p) => String(p.title || p.url),
}



/** When, to the minute, from a Slack ts or any time slack.ts reads. */
const at = (when: unknown) => {
  const s = String(when)
  const ms = /^\d{9,10}(\.\d+)?$/.test(s) ? Number(s) * 1000 : Date.parse(s)
  return Number.isNaN(ms) ? s : new Date(ms).toLocaleString([], { dateStyle: 'medium', timeStyle: 'short' })
}

export const slackMessage: ItemType = {
  type: 'slack_message',
  description:
    'A Slack message, pulled into the task: from its link (or its channel and ts). The chat shows who said it and what, read from Slack as it ' +
    "is shown; its view shows the thread it is in. A Slack link in a person's note is pulled in this way by itself.",
  params: slackMessageParams,
  data: '—',
  blocking: false,
  bots: true,
  describe: (p) => (p.link ? `Slack message ${p.link}` : `Slack message ${p.ts} in ${p.channel}`),
}

export const slackChannel: ItemType = {
  type: 'slack_channel',
  description: 'A Slack channel over a range of time, pulled into the task: its view shows the messages, with their threads.',
  params: slackChannelParams,
  data: '—',
  blocking: false,
  bots: true,
  describe: (p) => `Slack ${p.channel}, ${at(p.oldest)} to ${p.latest ? at(p.latest) : 'now'}`,
}

export const file: ItemType = {
  type: 'file',
  description:
    'A file, shown in the chat: an image inline, anything else as its name; its view shows it whole (an image to zoom, text, a PDF or Word ' +
    'document as it looks). Give the absolute path of a file on this machine: it refers to that file as it is when looked at (and a person ' +
    'may edit it, if it is text). With copy: true a copy is kept with the app instead, which lasts whatever happens to the file.',
  params: {
    path: z.string().optional().describe('The absolute path of a file on this machine.'),
    copy: z.boolean().optional().describe('Keep a copy now, rather than referring to the file. Not kept in its values.'),
    name: z.string().optional().describe("What it is called; the file's name if left out."),
    title: z.string().optional().describe('A line to show with it.'),
    hash: z.string().optional().describe('Set by the app: the kept copy, by its sha256.'),
    size: z.number().optional().describe('Set by the app: a kept copy\'s size, in bytes.'),
    mime: z.string().optional().describe("Set by the app, from its name: the file's type."),
  },
  data: '—',
  blocking: false,
  bots: true,
  describe: (p) => String(p.title || p.name || p.path || 'File'),
}

export const ITEM_TYPES: ItemType[] = [topic, approval, script, link, file, search, slackMessage, slackChannel, bot, user]
