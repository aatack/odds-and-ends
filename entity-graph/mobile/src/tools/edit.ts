import * as A from '../state/actions'
import { cachedValues, childOrder } from '../state/query'
import { getView } from '../state/store'
import { last, topLevel, type EditState } from '../state/types'
import { createEntity, link, writeValue } from '../source/entity'

// The in-place editor's commit path, factored out of the tools that use it: one
// writes and closes, the other writes and opens the next line.

export interface Committed {
  /** The edit as it was when it was committed. */
  edit: EditState
  /** The new entity's id, when the edit was a create that actually wrote. */
  created?: string
  mutated: boolean
}

/**
 * Write whatever is in the editor.
 *
 * `captured` is for the caller that has already moved the editor on (chained
 * entry): the box on screen is the *next* line by then, so the state must not be
 * cleared and the edit to write has to be handed in.
 *
 * An empty box means "never mind", whichever mode it is in: creating writes no
 * entity, and editing leaves the text as it was rather than blanking it — clearing
 * text deliberately is what `entity.rename` is for. A draft identical to what is
 * already stored writes nothing either, which matters here because every blur
 * commits and a phone blurs a field for all sorts of reasons.
 */
export async function commitEdit(captured?: EditState): Promise<Committed | null> {
  const state = getView()
  const edit = captured ?? state.edit
  if (!edit) return null
  if (state.edit === edit) A.setEdit(null)

  const subject = last(edit.path)
  if (!subject || !edit.draft.trim()) return { edit, mutated: false }

  if (edit.mode === 'edit') {
    if (cachedValues(subject)?.text === edit.draft) return { edit, mutated: false }
    await writeValue(subject, 'text', edit.draft)
    return { edit, mutated: true }
  }

  const values = { text: edit.draft, ...edit.values }

  // A row below another means "links to it" in a reversed level, so the new entity
  // is linked the other way round — otherwise it would be created out of sight of
  // the level that asked for it.
  if (topLevel(state).direction === 'in') {
    const created = await createEntity(values, null)
    await link(created, subject)
    return { edit, created, mutated: true }
  }

  const created = await createEntity(values, subject, {
    siblings: childOrder(subject),
    after: edit.after,
  })
  A.selectPath([...edit.path, created])
  return { edit, created, mutated: true }
}
