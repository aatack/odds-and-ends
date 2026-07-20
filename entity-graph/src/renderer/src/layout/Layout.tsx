import React, { useEffect, useMemo } from 'react'
import { EDITOR_ACTIONS } from '../actions/editorActions'
import { hotkeyHint, matchAction } from '../actions/keys'
import type { Command } from '../components/CommandPalette'
import { isEditableTarget, useHotkeys } from '../helpers/useHotkeys'
import type { EditorActions } from '../views/useEditor'
import { LAYOUT_ACTIONS } from './layoutActions'
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
  // they act on the frame the user is looking at.
  const commands = useMemo<Command[]>(() => {
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
    return [...editorCmds, ...layoutCmds]
  }, [controller, runFocusedEditorAction])

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
          onDebugEntity={onDebugEntity}
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
