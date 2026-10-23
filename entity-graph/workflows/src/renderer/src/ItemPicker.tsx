import type { Item } from '../../shared/types'
import { Modal } from './components/ui/Modal'
import { ItemsList } from './ItemsList'

/**
 * Picks an item, in a modal over the blurred window: the items view itself,
 * whose Enter (or a click) calls back with the item rather than opening it.
 * Tab makes a new item with the typed title, and picks that.
 */
export function ItemPicker(props: {
  title: string
  items: Item[]
  verb: string
  onPick: (id: string) => void
  onCreate: (title: string) => void
  onClose: () => void
}) {
  const { title, items, verb, onPick, onCreate, onClose } = props
  return (
    <Modal title={title} size="large" blur onClose={onClose}>
      <div className="min-h-0 flex-1 overflow-y-auto">
        <ItemsList items={items} verb={verb} onOpen={onPick} onCreate={onCreate} />
      </div>
    </Modal>
  )
}
