/**
 * The page: one figure, one staircase, one table you can write back through. It
 * is a single file of HTML with the history baked into it, so opening it needs
 * nothing but the server.
 */

import type { Step } from "./debt.ts";

export interface View {
  now: number;
  debt: number;
  maintenance: number;
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
    <p class="sub" id="upkeep"></p>
  </header>

  <figure>
    <div id="chart" class="chart"></div>
  </figure>

  <details id="steps">
    <summary>Steps</summary>

    <form id="add" autocomplete="off">
      <select name="kind" aria-label="What you did">
        <option value="cycle">Cycle</option>
        <option value="run">Run</option>
        <option value="penalty">Penalty</option>
      </select>
      <input name="km" type="number" step="0.01" min="0.01" placeholder="km" aria-label="Distance in kilometres" required>
      <input name="at" type="datetime-local" aria-label="When, in UK time" required>
      <button type="submit">Add</button>
      <span class="error" id="error" role="alert"></span>
    </form>

    <table id="table">
      <thead><tr>
        <th>When</th><th>What</th><th class="n">Change</th><th class="n">Debt after</th><th class="c">Forgive</th>
      </tr></thead>
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
  --critical: #d03b3b;
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
    --critical: #d03b3b;
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
figure { margin: 0; background: var(--surface-1); border: 1px solid var(--border); border-radius: 10px; padding: 1rem 1.25rem 0.75rem; }
.chart { position: relative; }
svg { display: block; width: 100%; touch-action: none; }
.tick { fill: var(--text-muted); font-size: 12px; font-variant-numeric: tabular-nums; }
.axis-title { fill: var(--text-secondary); font-size: 12px; }
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
form { display: flex; flex-wrap: wrap; gap: 0.5rem; align-items: center; margin-top: 1rem; }
select, input, button {
  font: inherit; font-size: 0.875rem; color: var(--text-primary); background: var(--surface-1);
  border: 1px solid var(--border); border-radius: 6px; padding: 0.3125rem 0.5rem;
}
input[name="km"] { width: 6rem; }
button { cursor: pointer; font-weight: 500; }
button:disabled { cursor: progress; opacity: 0.6; }
.error { color: var(--critical); font-size: 0.8125rem; }
table { border-collapse: collapse; width: 100%; margin-top: 0.875rem; font-size: 0.875rem; }
th, td { text-align: left; padding: 0.375rem 0.75rem 0.375rem 0; border-bottom: 1px solid var(--grid); }
th { color: var(--text-muted); font-weight: 500; }
.n { text-align: right; font-variant-numeric: tabular-nums; }
.c { text-align: center; width: 5rem; padding-right: 0; }
tr.spared td { color: var(--text-muted); }
`;

const SCRIPT = `
let view = JSON.parse(document.getElementById("view").textContent);
const NS = "http://www.w3.org/2000/svg";
const PAD = { top: 18, right: 72, bottom: 28, left: 62 };
const ZONE = "Europe/London";

const when = new Intl.DateTimeFormat("en-GB", { timeZone: ZONE,
  day: "numeric", month: "short", hour: "2-digit", minute: "2-digit", hourCycle: "h23" });
const day = new Intl.DateTimeFormat("en-GB", { timeZone: ZONE, day: "numeric", month: "short" });
const month = new Intl.DateTimeFormat("en-GB", { timeZone: ZONE, month: "short" });
const field = new Intl.DateTimeFormat("en-CA", { timeZone: ZONE, year: "numeric", month: "2-digit",
  day: "2-digit", hour: "2-digit", minute: "2-digit", hourCycle: "h23" });

const km = (n) => n.toFixed(2).replace(/\\.?0+$/, "") + " km";
const signed = (n) => (n > 0 ? "+" : n < 0 ? "\\u2212" : "") + km(Math.abs(n));

/** What the UK clocks say, as a datetime-local field wants it. */
function clock(instant) {
  const found = {};
  for (const part of field.formatToParts(instant)) found[part.type] = part.value;
  return \`\${found.year}-\${found.month}-\${found.day}T\${found.hour}:\${found.minute}\`;
}

function describe(step) {
  if (step.cause === "growth") return step.forgiven ? "Sunday, forgiven" : "Sunday, half again";
  if (step.cause === "penalty") return "Penalty";
  return (step.cause === "run" ? "Run" : "Cycle") + ", " + km(step.km);
}

const chart = document.getElementById("chart");
const tooltip = document.createElement("div");
tooltip.className = "tooltip";
tooltip.setAttribute("role", "status");
chart.appendChild(tooltip);

let shape = null;
let drawn = null;

/** The staircase as points: each step holds its level until the next one lands. */
function derive() {
  const points = [];
  for (const step of view.steps) {
    points.push({ at: step.at, debt: step.before, step });
    points.push({ at: step.at, debt: step.after, step });
  }
  points.push({ at: view.now, debt: view.debt, step: view.steps[view.steps.length - 1] });
  const highest = Math.max(...points.map((p) => p.debt));
  shape = {
    points,
    first: view.steps[0].at,
    last: view.now,
    bottom: 0,
    top: Math.max(highest * 1.08, highest + 1),
  };
}

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
  const moment = Math.max(shape.first, Math.min(shape.last, time));
  let found = shape.points[0];
  for (const point of shape.points) if (point.at <= moment) found = point;
  return { at: moment, debt: found.debt, step: found.step };
}

function draw() {
  const { points, first, last, bottom, top } = shape;
  const width = chart.clientWidth || 880;
  const height = Math.max(240, Math.min(380, Math.round(width * 0.42)));
  const x = (time) => PAD.left + ((time - first) / (last - first)) * (width - PAD.left - PAD.right);
  const y = (debt) => height - PAD.bottom - ((debt - bottom) / (top - bottom)) * (height - PAD.top - PAD.bottom);

  const svg = node("svg", { viewBox: \`0 0 \${width} \${height}\`, height, role: "img",
    "aria-label": \`Running debt from \${day.format(first)} to now, currently \${km(view.debt)}.\` });

  for (const value of ticks(bottom, top)) {
    svg.appendChild(node("line", { x1: PAD.left, x2: width - PAD.right, y1: y(value), y2: y(value),
      stroke: value === 0 ? "var(--axis)" : "var(--grid)", "stroke-width": 1 }));
    svg.appendChild(node("text", { x: PAD.left - 8, y: y(value) + 4, "text-anchor": "end", class: "tick" },
      String(value)));
  }
  svg.appendChild(node("text", { x: 14, y: (PAD.top + height - PAD.bottom) / 2, class: "axis-title",
    "text-anchor": "middle", transform: \`rotate(-90 14 \${(PAD.top + height - PAD.bottom) / 2})\` }, "debt [km]"));

  svg.appendChild(node("text", { x: PAD.left, y: height - PAD.bottom + 18, "text-anchor": "middle", class: "tick" },
    day.format(first)));
  for (const time of months(first, last)) {
    if (x(time) - PAD.left < 40) continue;
    svg.appendChild(node("line", { x1: x(time), x2: x(time), y1: PAD.top, y2: height - PAD.bottom,
      stroke: "var(--grid)", "stroke-width": 1 }));
    svg.appendChild(node("text", { x: x(time), y: height - PAD.bottom + 18, "text-anchor": "middle", class: "tick" },
      month.format(time)));
  }

  for (const step of view.steps) {
    if (!step.forgiven) continue;
    svg.appendChild(node("line", { x1: x(step.at), x2: x(step.at), y1: PAD.top, y2: height - PAD.bottom,
      stroke: "var(--text-muted)", "stroke-width": 1, "stroke-dasharray": "3 4" }));
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

function table() {
  body.replaceChildren();
  for (const step of [...view.steps].reverse()) {
    const row = document.createElement("tr");
    if (step.forgiven) row.className = "spared";
    for (const [text, kind] of [[when.format(step.at), ""], [describe(step), ""],
      [signed(step.change), "n"], [km(step.after), "n"]]) {
      const cell = document.createElement("td");
      cell.className = kind;
      cell.textContent = text;
      row.appendChild(cell);
    }
    const last = document.createElement("td");
    last.className = "c";
    if (step.cause === "growth") {
      const box = document.createElement("input");
      box.type = "checkbox";
      box.checked = step.forgiven;
      box.setAttribute("aria-label", "Forgive the increase on " + day.format(step.at));
      box.addEventListener("change", () => {
        box.disabled = true;
        post("/forgive", { at: step.at, forgiven: box.checked }).finally(() => { box.disabled = false; });
      });
      last.appendChild(box);
    }
    row.appendChild(last);
    body.appendChild(row);
  }
}

function header() {
  document.getElementById("hero").textContent = view.debt.toFixed(1);
  document.getElementById("sub").textContent =
    "Owed now \\u00b7 first incurred " + day.format(view.steps[0].at) +
    " \\u00b7 " + view.steps.length + " changes since";
  document.getElementById("upkeep").textContent =
    km(view.maintenance) + " on the bike each week just to hold it there";
}

const form = document.getElementById("add");
const problem = document.getElementById("error");

async function post(path, sent) {
  problem.textContent = "";
  try {
    const answer = await fetch(path, {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify(sent),
    });
    const got = await answer.json();
    if (!answer.ok) throw new Error(got.error ?? "That did not work");
    view = got;
    render();
  } catch (error) {
    problem.textContent = error.message;
  }
}

form.kind.addEventListener("change", () => {
  const penalty = form.kind.value === "penalty";
  form.km.disabled = penalty;
  form.km.required = !penalty;
  if (penalty) form.km.value = "";
});

form.addEventListener("submit", (event) => {
  event.preventDefault();
  form.querySelector("button").disabled = true;
  post("/events", { kind: form.kind.value, km: form.km.value, at: form.at.value })
    .then(() => { form.km.value = ""; form.at.value = clock(Date.now()); })
    .finally(() => { form.querySelector("button").disabled = false; });
});

function render() {
  derive();
  draw();
  table();
  header();
}

let width = 0;
form.at.value = clock(Date.now());
render();
new ResizeObserver(() => {
  if (chart.clientWidth === width) return;
  width = chart.clientWidth;
  draw();
}).observe(chart);
`;
