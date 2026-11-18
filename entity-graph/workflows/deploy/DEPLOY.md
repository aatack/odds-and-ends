# Deploying YAWN on a server

This runs the workflows backend on an always-on Linux server, so the phone and
the laptop keep working with your laptop off, and Claude Code runs there for the
bots. The server is reached only over **Tailscale** — a private network between
your own devices — so it is not open to the public internet.

Everything here is in plain steps. You do not need to know systemd or Linux well.
Copy the commands as shown. Lines starting with `sudo` run as the administrator.

The kit that does the work lives in `deploy/`:
`install.sh` (sets the server up), `workflows.service` (keeps the backend running),
`backup.sh` + its timer (a daily copy), and `doctor.sh` (checks everything is OK).

---

## 1. Make a server at Hetzner

1. Make an account at <https://www.hetzner.com/cloud>.
2. Create a new server (Hetzner calls it a "Cloud server"):
   - **Image:** Ubuntu 24.04.
   - **Location:** a US one — **Hillsboro, OR** or **Ashburn, VA**.
   - **Type:** a plan with **4 GB RAM or more**, such as the CPX21 or CCX13 class.
     (The Rust build needs about 2 GB; the bots want headroom.) **Check the
     console for current prices** — they change.
   - **IPv4:** optional. You reach the box over Tailscale, not its public IP, so
     you can leave the public IPv4 off to save a little (keep it if unsure).
   - **SSH key:** add your SSH public key so you can log in.
   - **Firewall:** do **not** open any ports. Tailscale is the only way in.
     Keep the Hetzner firewall closed to inbound traffic except SSH (port 22) —
     or, once Tailscale is up, close SSH too and reach it over Tailscale.
3. Create the server and note its public IP (if you kept one) or its name.

## 2. Log in and install

From your laptop's terminal:

```sh
ssh root@<server-ip>
git clone <your repo url> workflows-src
cd workflows-src
sudo entity-graph/workflows/deploy/install.sh
```

The installer is **idempotent** — running it again is safe. It installs the
packages, Node 22, Rust (for the build), Tailscale and the Claude Code CLI; makes
a `workflows` user; copies the checkout to `/opt/workflows`; builds the app and
the store; writes `/etc/workflows/env` with a **generated phone token**; and
starts the backend. It does **not** run `tailscale up` — you do that next. When
it finishes it prints a checklist; follow it.

## 3. Turn on Tailscale and serve the app

```sh
sudo tailscale up
```

This prints a link. Open it in a browser and sign in to your Tailscale account
(the same one your laptop and phone use). In the Tailscale **admin console**,
turn on **MagicDNS** and **HTTPS certificates** (both under DNS settings). Then:

```sh
sudo tailscale serve --bg --https=443 http://127.0.0.1:47392
```

This gives the backend a real HTTPS address like
`https://<server-name>.<your-tailnet>.ts.net/`. `tailscale serve status` shows it.

## 4. Let Claude run (for the bots)

The bots run Claude Code on the server. Either log in interactively:

```sh
sudo -u workflows -H claude login
```

…or put an API key in the env file instead and restart:

```sh
sudo nano /etc/workflows/env      # add: ANTHROPIC_API_KEY=sk-ant-...
sudo systemctl restart workflows
```

Find your phone token (you need it to sign in) with:

```sh
sudo grep PHONE /etc/workflows/env
```

## 5. First sign-in

Your address is `https://<server-name>.<your-tailnet>.ts.net/` (from step 3).

- **Laptop (Desktop app):** open YAWN, and on the Start screen set
  **Source** = that URL and **Token** = the phone token from step 4. The app
  remembers the token, so next time it opens straight away.
- **Phone:** make sure the Tailscale app is installed, signed in to the same
  tailnet and **switched on**. Open the URL in the phone's browser, paste the
  token, sign in with your username, then **Add to Home Screen** so it installs
  as an app.

## 6. Moving your existing store to the server

If you already have a store on your laptop and want the server to take it over:

1. **Stop the laptop's backend first** — quit YAWN (or stop any
   `npm run backend`). Two backends must never run on the same file.
2. Copy the store file **and** its data folder (especially `secret.key` and the
   `bots` folder — without `secret.key` the stored secrets cannot be read) to the
   server. From the laptop, with `rsync` over Tailscale:

   ```sh
   rsync -a ~/tasks/tasks.sqlite      root@<server>:/var/lib/workflows/store.sqlite
   rsync -a ~/tasks/tasks.sqlite.data/ root@<server>:/var/lib/workflows/store.sqlite.data/
   ```

3. On the server, give them to the service user, then restart:

   ```sh
   sudo chown -R workflows:workflows /var/lib/workflows
   sudo systemctl restart workflows
   ```

4. From now on the **laptop uses the server** (Source = the URL, step 5). Do not
   run a backend on the old laptop file again — point the laptop at the server.

## 7. Updating

The bots can change the code, but to pull a new version yourself:

```sh
cd /opt/workflows
sudo -u workflows git pull
sudo entity-graph/workflows/deploy/install.sh --update
```

`--update` rebuilds the app and the store from the checkout and restarts the
service. (The `git pull` is yours to run; the installer does the rest.)

## 8. Backup and restore

A copy runs **daily** on its own (the `workflows-backup.timer`). Each copy is a
consistent snapshot of the store plus `secret.key` and the bots' folders, kept in
`/var/backups/workflows` (the last 14). Run one now with:

```sh
sudo /opt/workflows/entity-graph/workflows/deploy/backup.sh
```

These copies sit on the **same box**, so they survive a bad write, not a dead
disk. Copy them off too: `rclone`/`scp` the newest `.tar.gz` elsewhere, or take a
**Hetzner snapshot** in the console. **Keep `secret.key`** — the stored secrets
cannot be read without it.

To restore: stop the service, unpack a backup into `/var/lib/workflows`, fix
ownership, start again:

```sh
sudo systemctl stop workflows
sudo tar -xzf /var/backups/workflows/workflows-<stamp>.tar.gz -C /tmp/restore
sudo cp -a /tmp/restore/store.sqlite       /var/lib/workflows/store.sqlite
sudo cp -a /tmp/restore/data/secret.key    /var/lib/workflows/store.sqlite.data/secret.key
sudo cp -a /tmp/restore/data/bots          /var/lib/workflows/store.sqlite.data/
sudo chown -R workflows:workflows /var/lib/workflows
sudo systemctl start workflows
```

## 9. Security notes

- **The phone token is full owner access.** Anyone with it is you. Treat it like
  a password; it lives in `/etc/workflows/env` (readable only by root and the
  `workflows` user).
- **The tailnet is the gate.** The backend listens only on loopback; only
  `tailscale serve` exposes it, and only to devices on your tailnet. Keep the
  Hetzner firewall closed; do not open ports.
- **Bots run Claude in auto mode on this server**, with the server's files and
  any token in `/etc/workflows/env`. Keep that file **minimal** — only what the
  bots actually need. The **unsafe-actions rule still holds**: bots never get
  unsafe actions.
- Updating the backend's flags or ports? Update `deploy/` in the same change —
  the unit and this guide must match the backend.

## 10. Troubleshooting

- Run the checker: `sudo /opt/workflows/entity-graph/workflows/deploy/doctor.sh`
  — it prints OK/FAIL for node, the store binary, the build, Claude, Tailscale,
  the service and `/health`.
- Watch the logs live: `sudo journalctl -u workflows -f`.
- Restart it: `sudo systemctl restart workflows`.

## 11. The entity-graph app is not deployable here yet

This kit is for the **workflows** app. The separate **entity-graph** phone app
(`entity-graph/mobile`) talks to a **"broadcast node" that lives inside the
entity-graph desktop Electron app** — see `entity-graph/mobile/README.md`. There
is **no standalone server** for it today, so it cannot be put on this VPS. To
deploy it too, someone would first have to build a **headless broadcast host**
(the broadcast node running without the desktop app), the way the workflows
backend was made headless. Until then, run the entity-graph phone app against the
desktop app on your laptop as its README describes.
