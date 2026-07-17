import { z } from "zod";
import { themeAtom } from "../state/store";
import { defineAction } from "./registry";

// The colour theme is user-specific throwaway state (like selection), so it is
// applied synchronously and kept out of the audit log. Flipping it here rather
// than in the header keeps every state change flowing through an action.
export const toggleThemeAction = defineAction({
  name: "theme.toggle",
  params: z.object({}),
  log: false,
  palette: { label: "Toggle dark mode", defaultParams: {} },
  run: (_p, ctx) => {
    ctx.store.set(themeAtom, (t) => (t === "dark" ? "light" : "dark"));
  },
});
