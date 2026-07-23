import React, { useEffect, useMemo } from 'react'
import { EDITOR_ACTIONS } from '../actions/editorActions'
import { hotkeyHint, matchAction } from '../actions/keys'
import type { Command } from '../components/CommandPalette'
import { showToast } from '../components/ui/Toast'
import { emitReload } from '../helpers/reloadBus'
import { isEditableTarget, useHotkeys } from '../helpers/useHotkeys'
import type { EditorActions } from '../views/useEditor'
import { LAYOUT_ACTIONS, type LayoutController } from './layoutActions'
import { TabGroupView } from './TabGroupView'
import { useLayout } from './useLayout'

export interface LayoutProps {
  actions: EditorActions
  onDebugEntity: (entityId: string) => void
  /** Publish the layout + focused-editor palette commands to the app shell. */
  onRegisterCommands: (commands: Command[] | null) => void
}

/**
 * The VS Code-style shell: tab groups laid out side by side, each showing the
 * top frame of its active tab. Owns the two window-level key handlers — layout
 * actions, and editor actions routed to the focused frame — and publishes both
 * command sets to the palette.
 */
export function Layout({ actions, onDebugEntity, onRegisterCommands }: LayoutProps): React.JSX.Element {
  const layout = useLayout()
  const { state, groups, controller, runFocusedEditorAction } = layout

  // Layout hotkeys (m, alt+arrows, d, …). ignoreEditable keeps bare keys from
  // firing while the user types in the palette or an in-place edit.
  useHotkeys(LAYOUT_ACTIONS, controller, { ignoreEditable: true })

  // Editor hotkeys are bound once here and routed to whichever frame is focused,
  // rather than in each editor (which would fire in every mounted editor at once).
  useEffect(() => {
    const onKey = (e: KeyboardEvent): void => {
      if (isEditableTarget(e.target)) return
      const action = matchAction(EDITOR_ACTIONS, e)
      if (!action) return
      e.preventDefault()
      runFocusedEditorAction(action.id)
    }
    window.addEventListener('keydown', onKey)
    return () => window.removeEventListener('keydown', onKey)
  }, [runFocusedEditorAction])

  // Both registries feed the one command palette — editor actions first, since
  // they act on the frame the user is looking at. Context commands come first of
  // all: they take an `entityId`, so a right-click over an entity pre-fills them.
  const commands = useMemo<Command[]>(() => {
    const contextCmds = buildContextCommands(controller, actions, onDebugEntity)
    const editorCmds = EDITOR_ACTIONS.filter((a) => a.palette !== false).map((a) => ({
      id: `editor.${a.id}`,
      label: a.label,
      aliases: a.aliases,
      hint: hotkeyHint(a.keys),
      run: () => runFocusedEditorAction(a.id),
    }))
    const layoutCmds = LAYOUT_ACTIONS.filter((a) => a.palette !== false).map((a) => ({
      id: `layout.${a.id}`,
      label: a.label,
      aliases: a.aliases,
      hint: hotkeyHint(a.keys) ?? a.hint,
      run: () => a.run(controller),
    }))
    return [...contextCmds, ...editorCmds, ...layoutCmds]
  }, [controller, actions, onDebugEntity, runFocusedEditorAction])

  useEffect(() => {
    onRegisterCommands(commands)
    return () => onRegisterCommands(null)
  }, [commands, onRegisterCommands])

  const visible = state.solo
    ? groups.filter((g) => g.group.id === state.focusedGroupId)
    : groups

  return (
    <div className="flex h-full w-full">
      {visible.map((rg) => (
        <TabGroupView
          key={rg.group.id}
          rg={rg}
          frames={state.frames}
          names={layout.names}
          focused={rg.group.id === state.focusedGroupId}
          actions={actions}
          onSelectTab={(t) => layout.selectTab(rg.group.id, t)}
          onFocus={() => layout.focusGroup(rg.group.id)}
          onCloseTab={(t) => layout.closeTab(rg.group.id, t)}
          onNewTab={() => layout.newTab(rg.group.id)}
          registerHandle={layout.registerHandle}
          updateView={layout.updateView}
          updateCanvasCam={layout.updateCanvasCam}
          reportName={layout.reportName}
        />
      ))}
    </div>
  )
}

// Commands that act on a specific entity by id, rather than on the current
// selection — the parameterised counterparts to the selection-based editor
// actions. Fields are auto-populated from the palette context: a right-click over
// an entity supplies `entityId` and (where it has a parent) `parentId`, so the
// clicked entity flows straight into `entityId`/`sourceId`/`childId` fields and
// its parent into `fromParentId`; the multi-argument ones then step through what's
// left. Mutations bump the reload bus so every mounted editor re-queries — the
// change didn't come through any one editor's own useEditor, so nothing else
// would refresh it.
function buildContextCommands(
  controller: LayoutController,
  actions: EditorActions,
  onDebugEntity: (entityId: string) => void,
): Command[] {
  const afterWrite = (p: Promise<unknown>): void => {
    p.then(emitReload).catch((e) =>
      showToast({ message: e instanceof Error ? e.message : String(e), variant: 'error' }),
    )
  }
  // Tri-state select → the stored value.
  const parseTri = (v: string, on: unknown, off: unknown, clear: unknown): unknown =>
    v === 'on' ? on : v === 'off' ? off : clear
  return [
    {
      id: 'ctx.open-entity',
      label: 'Open entity',
      aliases: ['focus', 'drill in', 'push frame'],
      fields: [{ name: 'entityId', label: 'Entity id' }],
      run: (v) => controller.focusEntity(v.entityId),
    },
    {
      id: 'ctx.debug-entity',
      label: 'Debug entity',
      aliases: ['inspect', 'info', 'raw'],
      fields: [{ name: 'entityId', label: 'Entity id' }],
      run: (v) => onDebugEntity(v.entityId),
    },
    {
      id: 'ctx.rename-entity',
      label: 'Rename entity',
      aliases: ['edit text', 'set text', 'change'],
      fields: [
        { name: 'entityId', label: 'Entity id' },
        { name: 'text', label: 'New text' },
      ],
      run: (v) => afterWrite(actions.writeText(v.entityId, v.text)),
    },
    {
      id: 'ctx.create-child',
      label: 'Create child of entity',
      aliases: ['add', 'new', 'insert'],
      fields: [
        { name: 'entityId', label: 'Parent id' },
        { name: 'text', label: 'Child text' },
      ],
      run: (v) => afterWrite(actions.createChild(v.entityId, v.text)),
    },
    {
      id: 'ctx.link-entities',
      label: 'Link entity to…',
      aliases: ['connect', 'relate', 'reference'],
      fields: [
        { name: 'sourceId', label: 'Source id', fromContext: 'entityId' },
        { name: 'destId', label: 'Destination id' },
      ],
      run: (v) => afterWrite(actions.linkEntities(v.sourceId, v.destId)),
    },
    {
      id: 'ctx.unlink-entity',
      label: 'Unlink entity from parent',
      aliases: ['remove', 'detach', 'disconnect'],
      fields: [
        { name: 'childId', label: 'Entity id', fromContext: 'entityId' },
        { name: 'parentId', label: 'Parent id' },
      ],
      run: (v) => afterWrite(actions.unlink(v.parentId, v.childId)),
    },
    {
      id: 'ctx.move-entity',
      label: 'Move entity to…',
      aliases: ['reparent', 'relocate'],
      fields: [
        { name: 'entityId', label: 'Entity id' },
        { name: 'fromParentId', label: 'From parent id', fromContext: 'parentId' },
        { name: 'toParentId', label: 'To parent id' },
      ],
      run: (v) => afterWrite(actions.moveEntity(v.entityId, v.fromParentId, v.toParentId)),
    },
    {
      id: 'ctx.set-section',
      label: 'Set entity section',
      aliases: ['heading', 'header', 'title'],
      fields: [
        { name: 'entityId', label: 'Entity id' },
        { name: 'section', label: 'Section', kind: 'select', options: ['on', 'off'] },
      ],
      run: (v) => afterWrite(actions.writeValue(v.entityId, 'section', parseTri(v.section, true, null, null))),
    },
    {
      id: 'ctx.set-checkbox',
      label: 'Set entity checkbox',
      aliases: ['todo', 'task', 'done', 'open', 'check'],
      fields: [
        { name: 'entityId', label: 'Entity id' },
        { name: 'open', label: 'Checkbox', kind: 'select', options: ['on', 'off', 'none'] },
      ],
      // on = open box (true), off = ticked (false), none = plain bullet (null).
      run: (v) => afterWrite(actions.writeValue(v.entityId, 'open', parseTri(v.open, true, false, null))),
    },
    {
      id: 'ctx.close-panel',
      label: 'Close panel',
      aliases: ['remove node', 'remove panel'],
      fields: [{ name: 'entityId', label: 'Entity id' }],
      run: (v) => controller.closePanelById(v.entityId),
    },
  ]
}
