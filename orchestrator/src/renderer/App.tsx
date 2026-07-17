import { useEffect } from "react";
import { useAtom, useAtomValue } from "jotai";
import { runAction } from "./actions";
import { CommandPalette } from "./components/CommandPalette";
import { paletteOpenAtom, themeAtom } from "./state/store";
import { Home } from "./views/Home";

export function App() {
  const [paletteOpen, setPaletteOpen] = useAtom(paletteOpenAtom);
  const theme = useAtomValue(themeAtom);

  useEffect(() => {
    runAction("snapshot.refresh", {});
  }, []);

  useEffect(() => {
    document.documentElement.classList.toggle("dark", theme === "dark");
  }, [theme]);

  useEffect(() => {
    const onKey = (e: KeyboardEvent) => {
      if (e.ctrlKey && e.key.toLowerCase() === "p") {
        e.preventDefault();
        setPaletteOpen((v) => !v);
      }
    };
    window.addEventListener("keydown", onKey);
    return () => window.removeEventListener("keydown", onKey);
  }, [setPaletteOpen]);

  return (
    <>
      <Home onOpenPalette={() => setPaletteOpen(true)} />
      <CommandPalette open={paletteOpen} onClose={() => setPaletteOpen(false)} />
    </>
  );
}
