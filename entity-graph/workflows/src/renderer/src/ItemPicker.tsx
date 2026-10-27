import type { Item } from '../../shared/types'
import { Modal } from './components/ui/Modal'
import { ItemsList } from './ItemsList'
import { PICKER_SECTIONS, type Recent, type SectionId } from './sections'

/** One pick, which closes the picker; or a multiselect, whose `value` is what is selected and `setValue` changes it. */
type Picking = { multiple?: false; onPick: (id: string) => void } | { multiple: true; value: string[]; setValue: (ids: string[]) => void }

/**
 * Picks an item (or a bot, or a person), in a modal over the blurred window: the
 * finder (`ItemsList`), whose Enter (or a click) picks the row rather than
 * opening it. Tab makes a new item with the typed title, and picks that. A modal
 * has less room than the Tasks view, so its sections show six rows each by
 * default. With `multiple`, what is selected shows first, ticked: picking a row
 * adds it, or, if it is selected, takes it off.
 */
export function ItemPicker(
  props: {
    title: string
    items: Item[]
    recent: Recent
    sections?: SectionId[]
    perSection?: number
    verb: string
    /** Tab: with what is typed. Left out, Tab does nothing. */
    onTab?: (text: string) => void
    onClose: () => void
    /** Bots and people to offer first, in a section of their own. */
    bots?: { id: string; name: string; bot?: boolean }[]
  } & Picking,
) {
  const { title, items, recent, sections = PICKER_SECTIONS, perSection = 6, verb, onTab, onClose, bots } = props
  const pick = (id: string) => {
    if (!props.multiple) return props.onPick(id)
    props.setValue(props.value.includes(id) ? props.value.filter((v) => v !== id) : [...props.value, id])
  }
  return (
    <Modal title={title} size="large" blur onClose={onClose}>
      <div className="min-h-0 flex-1 overflow-y-auto">
        <ItemsList
          items={items}
          recent={recent}
          sections={sections}
          perSection={perSection}
          verb={verb}
          onOpen={pick}
          onTab={onTab}
          bots={bots}
          selected={props.multiple ? props.value : undefined}
        />
      </div>
    </Modal>
  )
}
