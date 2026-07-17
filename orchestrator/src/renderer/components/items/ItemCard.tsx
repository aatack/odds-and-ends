import { useState, type DragEvent } from "react";
import type { ItemRef } from "../../../shared/types";
import { runAction } from "../../actions";
import { readItemDrag, startItemDrag } from "../../helpers/drag";
import { cn } from "../../helpers/cn";
import { useResolvedItem } from "./useItem";

// A compact, type-specific overview of an item — the kind of thing shown as a
// single (possibly nested) bullet in a list. It renders *content only*: no
// border, background or selection highlight, expanding to fill the available
// width and growing vertically as its content needs. Laying it out, selecting
// it on click and highlighting the selection are the caller's job.
//
// The card is draggable (drag it onto another item to link them) and a drop
// target (dropping an item here links this item → the dropped one). It doesn't
// handle click-to-select; the type-specific body may add its own buttons.
export function ItemCard({ refItem }: { refItem: ItemRef }) {
  const { view, data } = useResolvedItem(refItem);
  const [dropping, setDropping] = useState(false);
  const Body = view.CardBody;

  const onDrop = (e: DragEvent) => {
    e.preventDefault();
    setDropping(false);
    const dragged = readItemDrag(e);
    if (dragged) runAction("link.create", { source: refItem, dest: dragged });
  };

  return (
    <div
      draggable
      onDragStart={(e) => startItemDrag(e, refItem)}
      onDragOver={(e) => {
        e.preventDefault();
        setDropping(true);
      }}
      onDragLeave={() => setDropping(false)}
      onDrop={onDrop}
      className={cn(
        "w-full",
        dropping && "rounded-md ring-1 ring-inset ring-brand-300",
      )}
    >
      {Body && data ? (
        <Body data={data} />
      ) : (
        <div className="text-sm text-gray-500">No data for this item.</div>
      )}
    </div>
  );
}
