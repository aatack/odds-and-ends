import { StrictMode } from "react";
import { createRoot } from "react-dom/client";
import { Provider } from "jotai";
import { store } from "./state/store";
import { App } from "./App";
import "./actions";
// Self-hosted fonts (bundled, no network): Geist for UI chrome, Source Serif 4
// with optical sizing for user-written prose. Imported before styles.css so the
// @font-face rules exist when our token layer references the families.
import "@fontsource-variable/geist/index.css";
import "@fontsource-variable/source-serif-4/opsz.css";
import "@fontsource-variable/source-serif-4/opsz-italic.css";
import "./styles.css";

createRoot(document.getElementById("root")!).render(
  <StrictMode>
    <Provider store={store}>
      <App />
    </Provider>
  </StrictMode>,
);
