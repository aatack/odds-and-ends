import type { DragEvent } from "react";
import type { ItemRef } from "../../shared/types";

const MIME = "application/x-item-ref";

export function startItemDrag(e: DragEvent, ref: ItemRef): void {
  e.dataTransfer.setData(MIME, JSON.stringify(ref));
  e.dataTransfer.effectAllowed = "link";
}

export function readItemDrag(e: DragEvent): ItemRef | null {
  const raw = e.dataTransfer.getData(MIME);
  if (!raw) return null;
  try {
    return JSON.parse(raw) as ItemRef;
  } catch {
    return null;
  }
}
