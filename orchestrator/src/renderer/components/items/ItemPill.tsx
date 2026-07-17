import { X } from "@untitledui/icons";
import type { ItemRef } from "../../../shared/types";
import { runAction } from "../../actions";
import { startItemDrag } from "../../helpers/drag";
import { cn } from "../../helpers/cn";
import { useResolvedItem } from "./useItem";

// One-line item overview. Draggable (to link elsewhere); clicking opens the
// item in the current view. Truncates gracefully in tight spaces.
export function ItemPill({
  refItem,
  onRemove,
  className,
}: {
  refItem: ItemRef;
  onRemove?: () => void;
  className?: string;
}) {
  const { summary, view } = useResolvedItem(refItem);
  return (
    <span
      className={cn(
        "inline-flex max-w-full items-center gap-1 rounded-md bg-gray-100 py-0.5 pl-2 pr-1 text-xs text-gray-700",
        className,
      )}
    >
      <span
        draggable
        onDragStart={(e) => startItemDrag(e, refItem)}
        onClick={() => runAction("selection.select", { ref: refItem })}
        className="min-w-0 truncate"
        title={summary.title}
      >
        <span className="mr-1 font-medium text-gray-400">{view.label}</span>
        <span className="font-serif">{summary.title}</span>
      </span>
      {onRemove && (
        <button
          onClick={onRemove}
          className="rounded-sm p-0.5 text-gray-400 hover:bg-gray-200 hover:text-gray-600"
          aria-label="Remove"
        >
          <X size={12} />
        </button>
      )}
    </span>
  );
}
