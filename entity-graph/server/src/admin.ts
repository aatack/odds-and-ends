import type { FastifyInstance } from 'fastify'

const PAGE = /* html */ `<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8" />
<meta name="viewport" content="width=device-width, initial-scale=1" />
<title>Sources admin</title>
<style>
  :root { color-scheme: light dark; font-family: system-ui, sans-serif; }
  body { margin: 0 auto; max-width: 960px; padding: 1.5rem; line-height: 1.4; }
  h1 { font-size: 1.3rem; } h2 { font-size: 1.05rem; margin-top: 1.5rem; }
  .muted { opacity: 0.7; } .err { color: #c62828; } .ok { color: #2e7d32; }
  .row { display: flex; gap: 0.5rem; align-items: center; flex-wrap: wrap; }
  input, textarea, select { font-family: inherit; padding: 0.35rem; box-sizing: border-box; }
  input[type=password], input.grow { flex: 1; font-family: monospace; }
  textarea { width: 100%; font-family: monospace; min-height: 6rem; }
  button { padding: 0.3rem 0.7rem; cursor: pointer; }
  .card { border: 1px solid color-mix(in srgb, currentColor 25%, transparent); border-radius: 8px; padding: 0.75rem 1rem; margin: 0.6rem 0; }
  .card h3 { margin: 0 0 0.3rem; font-size: 1rem; }
  .badge { font-size: 0.7rem; padding: 0.1rem 0.4rem; border-radius: 999px; border: 1px solid currentColor; }
  pre { background: color-mix(in srgb, currentColor 8%, transparent); padding: 0.5rem; border-radius: 6px; overflow: auto; white-space: pre-wrap; word-break: break-word; margin: 0.3rem 0; }
  code { font-family: monospace; }
  table { border-collapse: collapse; width: 100%; font-size: 0.85rem; }
  td, th { text-align: left; padding: 0.2rem 0.4rem; border-bottom: 1px solid color-mix(in srgb, currentColor 15%, transparent); }
  .toks { margin-top: 0.5rem; }
</style>
</head>
<body>
<h1>Sources admin</h1>
<div class="row">
  <input id="admin" type="password" class="grow" placeholder="admin token (leave blank if server has none)" autocomplete="off" />
  <button id="load">Load sources</button>
</div>
<p id="status" class="muted"></p>

<div id="sources"></div>

<h2 id="formTitle">Create source</h2>
<div class="row" style="margin-bottom:0.4rem">
  <input id="fId" placeholder="id (e.g. notes)" />
  <input id="fLabel" placeholder="label" />
  <select id="tpl">
    <option value="">insert template…</option>
    <option value="sqlite">sqlite</option>
    <option value="combined">combined</option>
    <option value="frozen">frozen</option>
    <option value="filter">filter (readonly)</option>
    <option value="remote">remote</option>
  </select>
</div>
<textarea id="fConfig" placeholder='{"type":"sqlite","path":"./data/notes.db"}'></textarea>
<div class="row" style="margin-top:0.4rem">
  <button id="save">Save</button>
  <button id="cancel" style="display:none">Cancel edit</button>
</div>

<script>
  var TEMPLATES = {
    sqlite: { type: 'sqlite', path: './data/ID.db', defaultAuthor: 'me' },
    combined: { type: 'combined', children: ['sourceA', 'sourceB'] },
    frozen: { type: 'frozen', child: 'sourceA', beforeTs: 0 },
    filter: { type: 'filter', child: 'sourceA', maxSafety: 'pure' },
    remote: { type: 'remote', url: 'http://host:4000/otherId', token: '' },
  };
  var $ = function (id) { return document.getElementById(id); };
  var key = 'eg-admin-token';
  $('admin').value = localStorage.getItem(key) || '';
  var editingId = null;

  function headers(json) {
    var h = {};
    if ($('admin').value) h['authorization'] = 'Bearer ' + $('admin').value;
    if (json) h['content-type'] = 'application/json';
    return h;
  }
  function status(msg, cls) {
    var s = $('status'); s.textContent = msg || ''; s.className = cls || 'muted';
  }
  async function api(method, url, body) {
    var opts = { method: method, headers: headers(!!body) };
    if (body) opts.body = JSON.stringify(body);
    var res = await fetch(url, opts);
    var text = await res.text();
    var data = text ? JSON.parse(text) : null;
    if (!res.ok) throw new Error((data && data.error) || ('HTTP ' + res.status));
    return data;
  }

  async function load() {
    localStorage.setItem(key, $('admin').value);
    try {
      var sources = await api('GET', '/admin/sources');
      render(sources);
      status(sources.length + ' source(s)', 'ok');
    } catch (e) { status(e.message, 'err'); $('sources').innerHTML = ''; }
  }

  function render(sources) {
    var c = $('sources'); c.innerHTML = '';
    sources.forEach(function (s) {
      var el = document.createElement('div');
      el.className = 'card';
      el.innerHTML =
        '<h3>' + s.id + ' <span class="badge">' + s.type + '</span> <span class="muted">' + (s.label || '') + '</span></h3>' +
        '<pre></pre>' +
        '<div class="row">' +
          '<button data-a="edit">Edit</button>' +
          '<button data-a="del">Delete</button>' +
          '<button data-a="tok">Tokens</button>' +
          '<a href="/' + s.id + '/debug" target="_blank">debug page ↗</a>' +
        '</div><div class="toks"></div>';
      el.querySelector('pre').textContent = JSON.stringify(s.config, null, 2);
      el.querySelector('[data-a=edit]').onclick = function () { startEdit(s); };
      el.querySelector('[data-a=del]').onclick = function () { del(s.id); };
      el.querySelector('[data-a=tok]').onclick = function () { toggleTokens(s.id, el.querySelector('.toks')); };
      c.appendChild(el);
    });
  }

  function startEdit(s) {
    editingId = s.id;
    $('formTitle').textContent = 'Edit source: ' + s.id;
    $('fId').value = s.id; $('fId').disabled = true;
    $('fLabel').value = s.label || '';
    $('fConfig').value = JSON.stringify(s.config, null, 2);
    $('cancel').style.display = '';
    window.scrollTo(0, document.body.scrollHeight);
  }
  function cancelEdit() {
    editingId = null;
    $('formTitle').textContent = 'Create source';
    $('fId').value = ''; $('fId').disabled = false;
    $('fLabel').value = ''; $('fConfig').value = '';
    $('cancel').style.display = 'none';
  }

  async function save() {
    var config;
    try { config = JSON.parse($('fConfig').value); }
    catch (e) { status('config is not valid JSON: ' + e.message, 'err'); return; }
    try {
      if (editingId) {
        await api('PUT', '/admin/sources/' + editingId, { label: $('fLabel').value, config: config });
      } else {
        if (!$('fId').value) { status('id is required', 'err'); return; }
        await api('POST', '/admin/sources', { id: $('fId').value, label: $('fLabel').value, config: config });
      }
      cancelEdit(); load();
    } catch (e) { status(e.message, 'err'); }
  }

  async function del(id) {
    if (!confirm('Delete source "' + id + '"?')) return;
    try { await api('DELETE', '/admin/sources/' + id); load(); }
    catch (e) { status(e.message, 'err'); }
  }

  async function toggleTokens(id, box) {
    if (box.dataset.open === '1') { box.dataset.open = '0'; box.innerHTML = ''; return; }
    box.dataset.open = '1';
    try {
      var toks = await api('GET', '/admin/sources/' + id + '/tokens');
      var rows = toks.map(function (t) {
        return '<tr><td><code>' + t.token + '</code></td><td>' + (t.label || '') + '</td>' +
          '<td>' + (t.revoked ? 'revoked' : 'live') + '</td>' +
          '<td>' + (t.revoked ? '' : '<button data-rev="' + t.token + '">revoke</button>') + '</td></tr>';
      }).join('');
      box.innerHTML =
        '<table><thead><tr><th>token</th><th>label</th><th>state</th><th></th></tr></thead><tbody>' +
        (rows || '<tr><td colspan="4" class="muted">no tokens</td></tr>') + '</tbody></table>' +
        '<div class="row" style="margin-top:0.4rem"><input placeholder="new token label (optional)" class="tlabel" />' +
        '<button class="issue">Issue token</button></div>';
      box.querySelectorAll('[data-rev]').forEach(function (b) {
        b.onclick = async function () {
          try { await api('DELETE', '/admin/tokens/' + b.getAttribute('data-rev')); box.dataset.open = '0'; toggleTokens(id, box); }
          catch (e) { status(e.message, 'err'); }
        };
      });
      box.querySelector('.issue').onclick = async function () {
        try {
          var r = await api('POST', '/admin/sources/' + id + '/tokens', { label: box.querySelector('.tlabel').value });
          status('issued token for "' + id + '": ' + r.token, 'ok');
          box.dataset.open = '0'; toggleTokens(id, box);
        } catch (e) { status(e.message, 'err'); }
      };
    } catch (e) { status(e.message, 'err'); }
  }

  $('load').onclick = load;
  $('save').onclick = save;
  $('cancel').onclick = cancelEdit;
  $('tpl').onchange = function () {
    var t = TEMPLATES[this.value];
    if (t) {
      var copy = JSON.parse(JSON.stringify(t));
      if (this.value === 'sqlite' && $('fId').value) copy.path = './data/' + $('fId').value + '.db';
      $('fConfig').value = JSON.stringify(copy, null, 2);
    }
    this.value = '';
  };
  if ($('admin').value) load();
</script>
</body>
</html>`

/** GET /admin — a minimal source management console. */
export function registerAdmin(app: FastifyInstance): void {
  app.get('/admin', async (_req, reply) => {
    return reply.type('text/html').send(PAGE)
  })
}
