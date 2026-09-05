/**
 * The page: one figure, one staircase, one table. It is a single file of HTML
 * with the history baked into it, so opening it needs nothing but the server.
 */

import type { Step } from "./debt.ts";

export interface View {
  now: number;
  debt: number;
  steps: Step[];
}

export function page(view: View): string {
  const data = JSON.stringify(view).replace(/</g, "\\u003c");
  return `<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Running debt</title>
<style>${STYLE}</style>
</head>
<body>
<main class="viz-root">
  <header>
    <p class="label">Running debt</p>
    <p class="hero"><span id="hero">&mdash;</span> <span class="unit">km</span></p>
    <p class="sub" id="sub"></p>
  </header>

  <figure>
    <figcaption>What is owed, since the first penalty. It grows by half every Sunday at 4am; running and cycling pay it down.</figcaption>
    <div id="chart" class="chart"></div>
  </figure>

  <details>
    <summary>Every step, as a table</summary>
    <table id="table">
      <thead><tr><th>When</th><th>What</th><th class="n">Change</th><th class="n">Debt after</th></tr></thead>
      <tbody></tbody>
    </table>
  </details>
</main>
<script type="application/json" id="view">${data}</script>
<script type="module">${SCRIPT}</script>
</body>
</html>
`;
}

const STYLE = `
:root {
  color-scheme: light;
  --surface-1: #fcfcfb;
  --plane: #f9f9f7;
  --text-primary: #0b0b0b;
  --text-secondary: #52514e;
  --text-muted: #898781;
  --grid: #e1e0d9;
  --axis: #c3c2b7;
  --border: rgba(11, 11, 11, 0.1);
  --series-1: #2a78d6;
}
@media (prefers-color-scheme: dark) {
  :root {
    color-scheme: dark;
    --surface-1: #1a1a19;
    --plane: #0d0d0d;
    --text-primary: #ffffff;
    --text-secondary: #c3c2b7;
    --text-muted: #898781;
    --grid: #2c2c2a;
    --axis: #383835;
    --border: rgba(255, 255, 255, 0.1);
    --series-1: #3987e5;
  }
}
* { box-sizing: border-box; }
body {
  margin: 0;
  background: var(--plane);
  color: var(--text-primary);
  font: 15px/1.5 system-ui, -apple-system, "Segoe UI", sans-serif;
}
.viz-root { max-width: 61rem; margin: 0 auto; padding: 2.5rem 1.5rem 4rem; }
header { margin-bottom: 1.75rem; }
p { margin: 0; }
.label { color: var(--text-secondary); font-size: 0.875rem; }
.hero { font-size: 3.25rem; font-weight: 600; line-height: 1.1; letter-spacing: -0.02em; margin-top: 0.25rem; }
.hero .unit { font-size: 1.25rem; font-weight: 500; color: var(--text-secondary); }
.sub { color: var(--text-secondary); font-size: 0.9375rem; margin-top: 0.375rem; }
figure { margin: 0; background: var(--surface-1); border: 1px solid var(--border); border-radius: 10px; padding: 1.25rem 1.25rem 0.75rem; }
figcaption { color: var(--text-secondary); font-size: 0.875rem; margin-bottom: 0.75rem; max-width: 44rem; }
.chart { position: relative; }
svg { display: block; width: 100%; touch-action: none; }
.tick { fill: var(--text-muted); font-size: 12px; font-variant-numeric: tabular-nums; }
.end-label { fill: var(--text-primary); font-size: 13px; font-weight: 600; }
.tooltip {
  position: absolute; pointer-events: none; opacity: 0; transition: opacity 90ms;
  background: var(--surface-1); border: 1px solid var(--border); border-radius: 8px;
  padding: 0.5rem 0.625rem; box-shadow: 0 4px 14px rgba(0, 0, 0, 0.12);
  font-size: 0.8125rem; white-space: nowrap; transform: translate(-50%, -100%);
}
.tooltip.on { opacity: 1; }
.tooltip .value { font-weight: 600; font-size: 1rem; display: block; }
.tooltip .when { color: var(--text-secondary); }
.tooltip .cause { color: var(--text-secondary); display: flex; align-items: center; gap: 0.375rem; margin-top: 0.25rem; }
.key { display: inline-block; width: 12px; height: 2px; border-radius: 1px; background: var(--series-1); }
details { margin-top: 1.75rem; }
summary { cursor: pointer; color: var(--text-secondary); font-size: 0.875rem; }
table { border-collapse: collapse; width: 100%; margin-top: 0.875rem; font-size: 0.875rem; }
th, td { text-align: left; padding: 0.375rem 0.75rem 0.375rem 0; border-bottom: 1px solid var(--grid); }
th { color: var(--text-muted); font-weight: 500; }
.n { text-align: right; font-variant-numeric: tabular-nums; }
`;

const SCRIPT = `
const view = JSON.parse(document.getElementById("view").textContent);
const NS = "http://www.w3.org/2000/svg";
const PAD = { top: 18, right: 72, bottom: 28, left: 44 };

const when = new Intl.DateTimeFormat("en-GB", { timeZone: "Europe/London",
  day: "numeric", month: "short", hour: "2-digit", minute: "2-digit", hourCycle: "h23" });
const day = new Intl.DateTimeFormat("en-GB", { timeZone: "Europe/London", day: "numeric", month: "short" });
const month = new Intl.DateTimeFormat("en-GB", { timeZone: "Europe/London", month: "short" });

const km = (n) => n.toFixed(2).replace(/\\.?0+$/, "") + " km";
const signed = (n) => (n > 0 ? "+" : n < 0 ? "\\u2212" : "") + km(Math.abs(n));

function describe(step) {
  if (step.cause === "growth") return "Sunday, half again";
  if (step.cause === "penalty") return "Penalty";
  return (step.cause === "run" ? "Run" : "Cycle") + ", " + km(step.km);
}

// The staircase as points: each step holds its level until the next one lands.
const points = [];
for (const step of view.steps) {
  points.push({ at: step.at, debt: step.before, step });
  points.push({ at: step.at, debt: step.after, step });
}
points.push({ at: view.now, debt: view.debt, step: view.steps[view.steps.length - 1] });

const first = view.steps[0].at;
const last = view.now;
const highest = Math.max(...points.map((p) => p.debt));
const bottom = Math.min(0, ...points.map((p) => p.debt));
const top = Math.max(highest * 1.08, highest + 1);

function ticks(low, high) {
  const span = high - low || 1;
  const size = Math.pow(10, Math.floor(Math.log10(span / 4)));
  const step = [1, 2, 2.5, 5, 10].map((m) => m * size).find((s) => span / s <= 5) ?? size * 10;
  const out = [];
  for (let value = Math.ceil(low / step) * step; value <= high + 1e-9; value += step) out.push(value);
  return out;
}

function months(from, to) {
  const out = [];
  const cursor = new Date(from);
  cursor.setUTCDate(1);
  cursor.setUTCHours(0, 0, 0, 0);
  while (cursor.getTime() <= to) {
    if (cursor.getTime() >= from) out.push(cursor.getTime());
    cursor.setUTCMonth(cursor.getUTCMonth() + 1);
  }
  return out;
}

const node = (name, attributes, text) => {
  const element = document.createElementNS(NS, name);
  for (const [key, value] of Object.entries(attributes)) element.setAttribute(key, value);
  if (text !== undefined) element.textContent = text;
  return element;
};

/** The level the staircase is at then, and the step that put it there. */
function at(time) {
  const moment = Math.max(first, Math.min(last, time));
  let found = points[0];
  for (const point of points) if (point.at <= moment) found = point;
  return { at: moment, debt: found.debt, step: found.step };
}

const chart = document.getElementById("chart");
const tooltip = document.createElement("div");
tooltip.className = "tooltip";
tooltip.setAttribute("role", "status");
chart.appendChild(tooltip);

let drawn = null;
let drawnWidth = 0;

function draw() {
  const width = chart.clientWidth || 880;
  if (width === drawnWidth) return;
  drawnWidth = width;
  const height = Math.max(240, Math.min(380, Math.round(width * 0.42)));
  const x = (time) => PAD.left + ((time - first) / (last - first)) * (width - PAD.left - PAD.right);
  const y = (debt) => height - PAD.bottom - ((debt - bottom) / (top - bottom)) * (height - PAD.top - PAD.bottom);

  const svg = node("svg", { viewBox: \`0 0 \${width} \${height}\`, height, role: "img",
    "aria-label": \`Running debt from \${day.format(first)} to now, currently \${km(view.debt)}.\` });

  for (const value of ticks(bottom, top)) {
    svg.appendChild(node("line", { x1: PAD.left, x2: width - PAD.right, y1: y(value), y2: y(value),
      stroke: value === 0 ? "var(--axis)" : "var(--grid)", "stroke-width": 1 }));
    svg.appendChild(node("text", { x: PAD.left - 8, y: y(value) + 4, "text-anchor": "end", class: "tick" }, String(value)));
  }
  svg.appendChild(node("text", { x: PAD.left, y: height - PAD.bottom + 18, "text-anchor": "middle", class: "tick" },
    day.format(first)));
  for (const time of months(first, last)) {
    if (x(time) - PAD.left < 40) continue;
    svg.appendChild(node("line", { x1: x(time), x2: x(time), y1: PAD.top, y2: height - PAD.bottom,
      stroke: "var(--grid)", "stroke-width": 1 }));
    svg.appendChild(node("text", { x: x(time), y: height - PAD.bottom + 18, "text-anchor": "middle", class: "tick" },
      month.format(time)));
  }

  const line = points.map((p, i) => \`\${i ? "L" : "M"}\${x(p.at).toFixed(1)} \${y(p.debt).toFixed(1)}\`).join(" ");
  svg.appendChild(node("path", {
    d: \`\${line} L\${x(last).toFixed(1)} \${y(bottom).toFixed(1)} L\${x(first).toFixed(1)} \${y(bottom).toFixed(1)} Z\`,
    fill: "var(--series-1)", "fill-opacity": 0.1 }));
  svg.appendChild(node("path", { d: line, fill: "none", stroke: "var(--series-1)", "stroke-width": 2,
    "stroke-linejoin": "round", "stroke-linecap": "round" }));

  const crosshair = node("line", { x1: 0, x2: 0, y1: PAD.top, y2: height - PAD.bottom,
    stroke: "var(--axis)", "stroke-width": 1, opacity: 0 });
  const marker = node("circle", { cx: 0, cy: 0, r: 5, fill: "var(--series-1)",
    stroke: "var(--surface-1)", "stroke-width": 2, opacity: 0 });
  svg.append(crosshair, marker);

  svg.appendChild(node("circle", { cx: x(last), cy: y(view.debt), r: 4.5,
    fill: "var(--series-1)", stroke: "var(--surface-1)", "stroke-width": 2 }));
  svg.appendChild(node("text", { x: x(last) + 10, y: y(view.debt) + 4, class: "end-label" }, km(view.debt)));

  svg.addEventListener("pointermove", (event) => {
    const box = svg.getBoundingClientRect();
    const across = ((event.clientX - box.left) / box.width) * width;
    const point = at(first + ((across - PAD.left) / (width - PAD.left - PAD.right)) * (last - first));
    crosshair.setAttribute("x1", x(point.at));
    crosshair.setAttribute("x2", x(point.at));
    crosshair.setAttribute("opacity", 1);
    marker.setAttribute("cx", x(point.at));
    marker.setAttribute("cy", y(point.debt));
    marker.setAttribute("opacity", 1);
    label(point, x(point.at), y(point.debt));
  });
  svg.addEventListener("pointerleave", () => {
    tooltip.classList.remove("on");
    crosshair.setAttribute("opacity", 0);
    marker.setAttribute("opacity", 0);
  });

  if (drawn) drawn.remove();
  drawn = svg;
  chart.appendChild(svg);
}

function label(point, left, above) {
  tooltip.replaceChildren();
  const value = document.createElement("span");
  value.className = "value";
  value.textContent = km(point.debt) + " owed";
  const stamp = document.createElement("span");
  stamp.className = "when";
  stamp.textContent = when.format(point.at);
  const cause = document.createElement("span");
  cause.className = "cause";
  const key = document.createElement("span");
  key.className = "key";
  cause.append(key, document.createTextNode(
    describe(point.step) + " \\u00b7 " + signed(point.step.change) + ", " + day.format(point.step.at)));
  tooltip.append(value, stamp, cause);
  tooltip.style.left = left + "px";
  tooltip.style.top = Math.max(tooltip.offsetHeight, above - 12) + "px";
  tooltip.classList.add("on");
}

const body = document.querySelector("#table tbody");
for (const step of [...view.steps].reverse()) {
  const row = document.createElement("tr");
  for (const [text, kind] of [[when.format(step.at), ""], [describe(step), ""],
    [signed(step.change), "n"], [km(step.after), "n"]]) {
    const cell = document.createElement("td");
    cell.className = kind;
    cell.textContent = text;
    row.appendChild(cell);
  }
  body.appendChild(row);
}

document.getElementById("hero").textContent = view.debt.toFixed(1);
document.getElementById("sub").textContent =
  "Owed now \\u00b7 first incurred " + day.format(first) + " \\u00b7 " + view.steps.length + " changes since";

draw();
new ResizeObserver(draw).observe(chart);
`;
