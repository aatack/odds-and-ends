import { useAtomValue } from "jotai";
import { Moon01, Sun } from "@untitledui/icons";
import { itemsAtom, selectedRefAtom, tasksAtom, themeAtom } from "../state/store";
import { sameRef, type ItemRef } from "../../shared/types";
import { runAction } from "../actions";
import { cn } from "../helpers/cn";
import { Button } from "../components/ui/Button";
import { ItemCard } from "../components/items/ItemCard";
import { ItemDetail } from "../components/items/ItemDetail";
import { ActionLog } from "../components/ActionLog";

// The landing page — an example wiring of the reusable components against the
// placeholder "task" item type. The selected item is opened in the detail view
// at the top; every task is listed below as its own row, each rendering an
// ItemCard for its content. The row (not the card) owns click-to-select and the
// selection highlight.
export function Home({ onOpenPalette }: { onOpenPalette: () => void }) {
  const items = useAtomValue(itemsAtom);
  const tasks = useAtomValue(tasksAtom);
  const selected = useAtomValue(selectedRefAtom);
  const theme = useAtomValue(themeAtom);

  const taskRefs: ItemRef[] = tasks.map((t) => ({ type: "task", id: t.itemId }));
  const isArchived = (ref: ItemRef) =>
    items.find((i) => sameRef(i, ref))?.archivedAt != null;
  const active = taskRefs.filter((r) => !isArchived(r));

  return (
    <div className="flex h-full min-h-0 flex-col">
      <header className="flex items-center gap-3 border-b border-gray-100 bg-white/80 px-6 py-3 backdrop-blur">
        <h1 className="text-[15px] font-semibold tracking-tightish text-gray-900">
          Orchestrator
        </h1>
        <span className="rounded-md bg-gray-100 px-1.5 py-0.5 text-[10px] font-medium uppercase tracking-wider text-gray-500">
          local
        </span>
        <div className="flex-1" />
        <Button
          variant="tertiary"
          size="sm"
          onClick={() => runAction("theme.toggle", {})}
          className="px-1.5"
          aria-label={theme === "dark" ? "Switch to light mode" : "Switch to dark mode"}
          title={theme === "dark" ? "Switch to light mode" : "Switch to dark mode"}
        >
          {theme === "dark" ? <Sun size={16} /> : <Moon01 size={16} />}
        </Button>
        <Button variant="secondary" size="sm" onClick={onOpenPalette}>
          Actions
          <kbd className="ml-1 rounded bg-gray-200 px-1 text-[10px] text-gray-500">Ctrl P</kbd>
        </Button>
        <Button variant="primary" size="sm" onClick={() => runAction("task.create", {})}>
          New task
        </Button>
      </header>

      <div className="flex min-h-0 flex-1">
        <main className="flex min-w-0 flex-1 flex-col gap-6 overflow-y-auto p-6">
          <section className="min-h-0">
            <div className="mb-2 text-[11px] font-medium uppercase tracking-[0.09em] text-gray-500">
              Detail
            </div>
            {selected ? (
              <ItemDetail refItem={selected} />
            ) : (
              <p className="px-1 py-8 text-center text-[13px] text-gray-400">
                Select a task from the list, or create one.
              </p>
            )}
          </section>

          <section>
            <div className="mb-2 text-[11px] font-medium uppercase tracking-[0.09em] text-gray-500">
              Tasks
            </div>
            {active.length === 0 ? (
              <p className="px-1 text-sm text-gray-400">Nothing here yet.</p>
            ) : (
              <div className="flex flex-col gap-0.5">
                {active.map((ref) => (
                  <TaskRow
                    key={ref.id}
                    refItem={ref}
                    selected={selected != null && sameRef(selected, ref)}
                  />
                ))}
              </div>
            )}
          </section>
        </main>

        <aside className="hidden w-80 shrink-0 bg-white xl:block">
          <ActionLog />
        </aside>
      </div>
    </div>
  );
}

// A single row in the task list. Clicking selects the item and the row shows
// the selection highlight — both responsibilities the ItemCard deliberately
// leaves to whatever lays it out.
function TaskRow({ refItem, selected }: { refItem: ItemRef; selected: boolean }) {
  return (
    <div
      onClick={() => runAction("selection.select", { ref: refItem })}
      className={cn(
        "rounded-lg px-2 py-1 transition-colors",
        selected ? "bg-gray-100" : "hover:bg-gray-100/70",
      )}
    >
      <ItemCard refItem={refItem} />
    </div>
  );
}
