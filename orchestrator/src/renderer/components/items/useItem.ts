import { useAtomValue } from "jotai";
import { linksAtom, snapshotAtom } from "../../state/store";
import { sameRef, type Item, type ItemRef, type Link } from "../../../shared/types";
import { itemViews, type ItemSummary, type ItemView } from "./itemViews";

export interface ResolvedItem {
  ref: ItemRef;
  item?: Item;
  view: ItemView;
  data: unknown;
  summary: ItemSummary;
}

export function useResolvedItem(ref: ItemRef): ResolvedItem {
  const snapshot = useAtomValue(snapshotAtom);
  const view = itemViews[ref.type];
  const item = snapshot.items.find((i) => sameRef(i, ref));
  const data = view.data(snapshot, ref.id);
  const summary = data
    ? view.summary(data)
    : { title: `${ref.type}:${ref.id}` };
  return { ref, item, view, data, summary };
}

export interface LinkedItem {
  link: Link;
  ref: ItemRef;
  direction: "outgoing" | "incoming";
}

export function useLinkedItems(ref: ItemRef): LinkedItem[] {
  const links = useAtomValue(linksAtom);
  return links.flatMap((link): LinkedItem[] => {
    if (sameRef(link.source, ref))
      return [{ link, ref: link.dest, direction: "outgoing" }];
    if (sameRef(link.dest, ref))
      return [{ link, ref: link.source, direction: "incoming" }];
    return [];
  });
}
