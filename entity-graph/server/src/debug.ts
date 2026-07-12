import type { FastifyInstance } from 'fastify'

const PAGE = /* html */ `<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8" />
<meta name="viewport" content="width=device-width, initial-scale=1" />
<title>Source debug</title>
<style>
  :root { color-scheme: light dark; font-family: system-ui, sans-serif; }
  body { margin: 0 auto; max-width: 900px; padding: 1.5rem; line-height: 1.4; }
  h1 { font-size: 1.2rem; }
  .muted { opacity: 0.7; }
  .token { display: flex; gap: 0.5rem; margin: 1rem 0; }
  .token input { flex: 1; padding: 0.4rem; font-family: monospace; }
  .tool { border: 1px solid color-mix(in srgb, currentColor 25%, transparent); border-radius: 8px; padding: 0.75rem 1rem; margin: 0.75rem 0; }
  .tool h3 { margin: 0 0 0.25rem; font-size: 1rem; display: flex; gap: 0.5rem; align-items: baseline; }
  .badge { font-size: 0.7rem; padding: 0.1rem 0.4rem; border-radius: 999px; border: 1px solid currentColor; text-transform: uppercase; letter-spacing: 0.03em; }
  .pure { color: #2e7d32; } .safe-mutating { color: #b8860b; } .dangerous { color: #c62828; }
  textarea { width: 100%; box-sizing: border-box; font-family: monospace; min-height: 4rem; padding: 0.4rem; }
  pre { background: color-mix(in srgb, currentColor 8%, transparent); padding: 0.6rem; border-radius: 6px; overflow: auto; white-space: pre-wrap; word-break: break-word; }
  button { padding: 0.35rem 0.8rem; cursor: pointer; }
  summary { cursor: pointer; }
</style>
</head>
<body>
<h1>Source debug: <code id="sid"></code></h1>
<p class="muted">Calls <code>/{source}/tools</code> and <code>/{source}/call</code> with the bearer token below.</p>
<div class="token">
  <input id="token" type="password" placeholder="source bearer token" autocomplete="off" />
  <button id="load">Load tools</button>
</div>
<div id="tools"></div>
<script>
  const base = location.pathname.replace(/\\/debug\\/?$/, '');
  document.getElementById('sid').textContent = base.replace(/^\\//, '');
  const tokenEl = document.getElementById('token');
  const key = 'eg-token-' + base;
  tokenEl.value = localStorage.getItem(key) || '';
  function headers() {
    const h = { 'content-type': 'application/json' };
    if (tokenEl.value) h['authorization'] = 'Bearer ' + tokenEl.value;
    return h;
  }
  function skeleton(schema) {
    const props = (schema && schema.properties) || {};
    const out = {};
    for (const k of Object.keys(props)) out[k] = null;
    return out;
  }
  async function load() {
    localStorage.setItem(key, tokenEl.value);
    const container = document.getElementById('tools');
    container.innerHTML = 'Loading…';
    const res = await fetch(base + '/tools', { headers: headers() });
    if (!res.ok) { container.innerHTML = '<pre>Error ' + res.status + ': ' + (await res.text()) + '</pre>'; return; }
    const tools = await res.json();
    container.innerHTML = '';
    for (const t of tools) {
      const el = document.createElement('div');
      el.className = 'tool';
      el.innerHTML =
        '<h3>' + t.name + ' <code>' + t.id + '</code> <span class="badge ' + t.safety + '">' + t.safety + '</span></h3>' +
        '<div class="muted">' + (t.description || '') + '</div>' +
        '<details><summary>schema</summary><pre>' + JSON.stringify(t.args, null, 2) + '</pre></details>' +
        '<textarea></textarea><div><button>Call</button></div><pre style="display:none"></pre>';
      const ta = el.querySelector('textarea');
      ta.value = JSON.stringify(skeleton(t.args), null, 2);
      const out = el.querySelector('pre:last-of-type');
      el.querySelector('button').onclick = async () => {
        out.style.display = 'block';
        out.textContent = 'Calling…';
        let args;
        try { args = ta.value.trim() ? JSON.parse(ta.value) : {}; }
        catch (e) { out.textContent = 'Invalid JSON: ' + e.message; return; }
        const r = await fetch(base + '/call', { method: 'POST', headers: headers(), body: JSON.stringify({ tool: t.id, args }) });
        out.textContent = JSON.stringify(await r.json(), null, 2);
      };
      container.appendChild(el);
    }
    if (!tools.length) container.innerHTML = '<p class="muted">No tools exposed.</p>';
  }
  document.getElementById('load').onclick = load;
  if (tokenEl.value) load();
</script>
</body>
</html>`

/** GET /:sourceId/debug — an interactive per-source tool console. */
export function registerDebug(app: FastifyInstance): void {
  app.get('/:sourceId/debug', async (_req, reply) => {
    return reply.type('text/html').send(PAGE)
  })
}
