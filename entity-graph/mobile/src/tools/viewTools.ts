import * as A from '../state/actions'
import { loadMore, refreshQueries } from '../state/query'
import { getView } from '../state/store'
import { ROOT_ID, topLevel } from '../state/types'
import { openSheet } from '../state/ui'
import { asId, requireId } from './context'
import { needsRow, type ToolSpec } from './types'

// Tools that move around and change what is on screen, without touching the store.
// These are what the desktop app splits between its frame tools and its layout
// tools; with one view and one stack there is nothing to split.

export const VIEW_TOOLS: ToolSpec[] = [
  {
    id: 'view.open',
    label: 'Open',
    aliases: ['drill in', 'focus', 'zoom', 'go to'],
    hint: 'Navigate',
    args: [{ name: 'entityId', label: 'Entity id', kind: 'entity', fromContext: 'entityId' }],
    enabled: needsRow,
    run: ({ entityId }) => A.pushLevel(requireId(entityId, 'Entity id')),
  },
  {
    id: 'view.openById',
    label: 'Open an id…',
    aliases: ['go to', 'paste id', 'jump'],
    hint: 'Navigate',
    args: [{ name: 'entityId', label: 'Entity id', kind: 'entity', placeholder: 'Paste an id' }],
    run: ({ entityId }) => A.pushLevel(requireId(entityId, 'Entity id')),
  },
  {
    id: 'view.back',
    label: 'Back',
    aliases: ['up', 'out', 'pop'],
    hint: 'Navigate',
    enabled: () => getView().stack.length > 1,
    run: () => A.popLevel(),
  },
  {
    id: 'view.root',
    label: 'Go to the index',
    aliases: ['home', 'top', 'root'],
    hint: 'Navigate',
    enabled: () => {
      const s = getView()
      return s.stack.length > 1 || topLevel(s).rootId !== ROOT_ID
    },
    run: () => A.resetTo(ROOT_ID),
  },
  {
    id: 'view.crumbs',
    label: 'Where am I?',
    aliases: ['crumbs', 'trail', 'path', 'history'],
    hint: 'Navigate',
    listed: false,
    run: () => openSheet({ kind: 'crumbs' }),
  },
  {
    // A property of the level, not a different kind of view: the same tree, read
    // the other way round.
    id: 'view.reverse',
    label: 'Follow inbound links',
    aliases: ['backlinks', 'references', 'what links here', 'parents', 'reverse', 'flip'],
    hint: 'Navigate',
    run: () => {
      const reversed = topLevel(getView()).direction === 'in'
      A.setDirection(reversed ? 'out' : 'in')
      return { message: reversed ? 'Following outbound links' : 'Following inbound links' }
    },
  },
  {
    id: 'view.refresh',
    label: 'Refresh',
    aliases: ['reload', 'refetch', 'sync'],
    hint: 'View',
    run: () => refreshQueries(),
  },

  // --- Folding --------------------------------------------------------------
  {
    id: 'view.collapse',
    label: 'Collapse',
    aliases: ['fold', 'close', 'hide children'],
    hint: 'View',
    args: [{ name: 'entityId', label: 'Entity id', kind: 'entity', fromContext: 'entityId' }],
    enabled: (ctx) => ctx.row?.hasChildren === true && !ctx.row.collapsed,
    run: ({ entityId }) => A.setCollapsed(asId(entityId), true),
  },
  {
    id: 'view.expand',
    label: 'Expand',
    aliases: ['unfold', 'open children', 'show'],
    hint: 'View',
    args: [{ name: 'entityId', label: 'Entity id', kind: 'entity', fromContext: 'entityId' }],
    enabled: (ctx) => ctx.row?.collapsed === true,
    run: ({ entityId }) => A.setCollapsed(asId(entityId), false),
  },
  {
    id: 'view.collapseAll',
    label: 'Collapse everything',
    aliases: ['fold all', 'close all'],
    hint: 'View',
    run: (_args, ctx) => {
      // Read the rows rather than the store: what is folded is a statement about
      // what is on screen. The level's own root is left out — it always expands, so
      // folding it would just empty the screen.
      const folded = new Set(getView().collapsed)
      for (const row of ctx.rows) if (row.hasChildren && row.id !== ctx.rootId) folded.add(row.id)
      A.setCollapsedSet([...folded])
    },
  },
  {
    id: 'view.expandAll',
    label: 'Expand everything',
    aliases: ['unfold all', 'open all'],
    hint: 'View',
    enabled: () => getView().collapsed.length > 0,
    run: () => A.setCollapsedSet([]),
  },

  // --- Filters --------------------------------------------------------------
  {
    id: 'view.find',
    label: 'Find',
    aliases: ['search', 'filter'],
    hint: 'View',
    run: () => {
      // Empty string, not null: that is what puts the field on screen. Run against
      // a field already open it changes nothing, so the tool means "find" whatever
      // state the view is in.
      if (getView().find == null) A.setFind('')
    },
  },
  {
    id: 'view.find.clear',
    label: 'Clear the search',
    aliases: ['unfilter', 'show all'],
    hint: 'View',
    enabled: () => getView().find != null,
    run: () => A.setFind(null),
  },
  {
    id: 'view.sections',
    label: 'Sections only',
    aliases: ['outline', 'headings', 'contents', 'toc'],
    hint: 'View',
    run: () => {
      const on = !getView().sectionsOnly
      A.setSectionsOnly(on)
      return { message: on ? 'Showing sections only' : 'Showing everything' }
    },
  },
  {
    // A tool rather than a bare call so the scroll sentinel at the end of the list
    // and the action sheet reach the same code.
    id: 'view.loadMore',
    label: 'Load more rows',
    aliases: ['next page', 'more'],
    hint: 'View',
    listed: false,
    // No `enabled`: the sentinel at the end of the list fires this whenever it is
    // on screen, which for a short outline is always, and `loadMore` already knows
    // when there is nothing to ask for.
    run: () => loadMore(),
  },
]
