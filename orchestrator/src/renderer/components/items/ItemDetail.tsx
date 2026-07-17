import { useState, type DragEvent } from "react";
import type { ItemRef } from "../../../shared/types";
import { runAction } from "../../actions";
import { readItemDrag } from "../../helpers/drag";
import { cn } from "../../helpers/cn";
import { absoluteTime, relativeTime } from "../../helpers/time";
import { Badge } from "../ui/Badge";
import { Button } from "../ui/Button";
import { ItemPill } from "./ItemPill";
import { useLinkedItems, useResolvedItem } from "./useItem";

// Large detail view for a single item. The header, debug section, archival and
// drop-to-link behaviour are common to every type; the body is type-specific.
// Dropping an item here links it (source = this item, dest = the dropped one).
export function ItemDetail({
  refItem,
  embedded = false,
}: {
  refItem: ItemRef;
  embedded?: boolean;
}) {
  const { item, view, data, summary } = useResolvedItem(refItem);
  const linked = useLinkedItems(refItem);
  const [debug, setDebug] = useState(false);
  const [dropping, setDropping] = useState(false);

  const archived = item?.archivedAt != null;
  const Body = view.DetailBody;

  const onDrop = (e: DragEvent) => {
    e.preventDefault();
    setDropping(false);
    const dragged = readItemDrag(e);
    if (dragged) runAction("link.create", { source: refItem, dest: dragged });
  };

  return (
    <div
      onDragOver={(e) => {
        e.preventDefault();
        setDropping(true);
      }}
      onDragLeave={() => setDropping(false)}
      onDrop={onDrop}
      className={cn(
        "flex flex-col gap-4 rounded-xl",
        !embedded && "bg-white p-5 shadow-xs",
        dropping && "ring-1 ring-inset ring-brand-300",
      )}
    >
      <div className="flex items-center gap-2">
        <Badge color={view.color}>{view.label}</Badge>
        {summary.status && (
          <Badge dot color={summary.status.color}>
            {summary.status.label}
          </Badge>
        )}
        {archived && <Badge color="gray">Archived</Badge>}
        <div className="flex-1" />
        <Button size="sm" variant="tertiary" onClick={() => setDebug((v) => !v)}>
          {debug ? "Hide debug" : "Debug"}
        </Button>
        {archived ? (
          <Button size="sm" onClick={() => runAction("item.unarchive", { ref: refItem })}>
            Restore
          </Button>
        ) : (
          <Button size="sm" onClick={() => runAction("item.archive", { ref: refItem })}>
            Archive
          </Button>
        )}
      </div>

      {Body && data ? <Body data={data} /> : <div className="text-sm text-gray-500">No data for this item.</div>}

      {debug && (
        <div className="flex flex-col gap-3 rounded-lg bg-gray-50 p-4 text-xs text-gray-500">
          <div>
            <div className="mb-1.5 font-semibold text-gray-700">
              Links ({linked.length})
            </div>
            {linked.length === 0 ? (
              <div>No links. Drag an item onto this view to link it.</div>
            ) : (
              <div className="flex flex-wrap gap-1.5">
                {linked.map(({ link, ref, direction }) => (
                  <ItemPill
                    key={link.id}
                    refItem={ref}
                    className={direction === "incoming" ? "opacity-70" : undefined}
                    onRemove={() =>
                      runAction("link.delete", { source: link.source, dest: link.dest })
                    }
                  />
                ))}
              </div>
            )}
          </div>
          {item && (
            <div className="flex flex-col gap-0.5">
              <div>
                <span className="font-medium text-gray-700">id</span> {item.type}:{item.id}
              </div>
              <div title={absoluteTime(item.createdAt)}>
                <span className="font-medium text-gray-700">created</span>{" "}
                {relativeTime(item.createdAt)}
              </div>
              {item.archivedAt != null && (
                <div title={absoluteTime(item.archivedAt)}>
                  <span className="font-medium text-gray-700">archived</span>{" "}
                  {relativeTime(item.archivedAt)}
                </div>
              )}
            </div>
          )}
        </div>
      )}
    </div>
  );
}
