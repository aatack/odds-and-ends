import { ipcMain } from "electron";
import type { ItemRef, TaskInput, TaskPatch } from "../shared/types";
import * as repo from "./repository";

export function registerIpc(): void {
  ipcMain.handle("db:snapshot", () => repo.snapshot());

  ipcMain.handle("task:create", (_e, input: TaskInput) => repo.createTask(input));
  ipcMain.handle("task:update", (_e, arg: { id: string; patch: TaskPatch }) =>
    repo.updateTask(arg.id, arg.patch),
  );

  ipcMain.handle("item:setArchived", (_e, arg: { ref: ItemRef; at: number | null }) =>
    repo.setItemArchived(arg.ref, arg.at),
  );

  ipcMain.handle("link:create", (_e, arg: { source: ItemRef; dest: ItemRef }) =>
    repo.createLink(arg.source, arg.dest),
  );
  ipcMain.handle("link:delete", (_e, arg: { id: number }) => repo.deleteLink(arg.id));
}
