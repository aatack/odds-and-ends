import type { Item } from '../../shared/types'
import { Modal } from './components/ui/Modal'
import { ItemsList } from './ItemsList'
import type { Recent, SectionId } from './sections'

/** One pick, which closes the picker; or a multiselect, whose `value` is what is selected and `setValue` changes it. */
type Picking = { multiple?: false; onPick: (id: string) => void } | { multiple: true; value: string[]; setValue: (ids: string[]) => void }

/**
 * Picks an item (or a bot, or a person), in a modal over the blurred window: the
 * finder (`ItemsList`), the same as everywhere, except that Enter (or a click) picks
 * the row rather than opening it. Tab makes a new item with the typed title, and
 * picks that. With `multiple`, what is selected shows first, ticked: picking a row
 * adds it, or, if it is selected, takes it off.
 */
export function ItemPicker(
  props: {
    title: string
    items: Item[]
    recent: Recent
    /** Left out, the search page's as configured. */
    sections?: SectionId[]
    verb: string
    /** Tab: with what is typed. Left out, Tab does nothing. */
    onTab?: (text: string) => void
    onClose: () => void
    /** The people and bots to offer, last, in a section of their own. Left out, everyone. */
    bots?: { id: string; name: string; bot?: boolean }[]
  } & Picking,
) {
  const { title, items, recent, sections, verb, onTab, onClose, bots } = props
  const pick = (id: string) => {
    if (!props.multiple) return props.onPick(id)
    props.setValue(props.value.includes(id) ? props.value.filter((v) => v !== id) : [...props.value, id])
  }
  return (
    <Modal title={title} size="large" blur onClose={onClose}>
      {/* The finder the modal's width: its search line stays at the top, and the rows below it scroll (`.picker` in styles.css). */}
      <div className="picker min-h-0 flex-1">
        <ItemsList
          items={items}
          recent={recent}
          sections={sections}
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
