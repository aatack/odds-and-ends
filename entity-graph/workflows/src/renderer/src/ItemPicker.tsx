import type { Item } from '../../shared/types'
import { Modal } from './components/ui/Modal'
import { ItemsList } from './ItemsList'
import { PICKER_SECTIONS, type Recent, type SectionId } from './sections'

/**
 * Picks an item, in a modal over the blurred window: the finder (`ItemsList`),
 * whose Enter (or a click) calls back with the item rather than opening it.
 * Tab makes a new item with the typed title, and picks that. A modal has less
 * room than the Tasks view, so its sections show six rows each by default.
 */
export function ItemPicker(props: {
  title: string
  items: Item[]
  recent: Recent
  sections?: SectionId[]
  perSection?: number
  verb: string
  onPick: (id: string) => void
  onCreate: (title: string) => void
  onClose: () => void
  /** Bots and people to offer first, in a section of their own. */
  bots?: { id: string; name: string; bot?: boolean }[]
}) {
  const { title, items, recent, sections = PICKER_SECTIONS, perSection = 6, verb, onPick, onCreate, onClose, bots } = props
  return (
    <Modal title={title} size="large" blur onClose={onClose}>
      <div className="min-h-0 flex-1 overflow-y-auto">
        <ItemsList items={items} recent={recent} sections={sections} perSection={perSection} verb={verb} onOpen={onPick} onCreate={onCreate} bots={bots} />
      </div>
    </Modal>
  )
}
