// Telegram, spoken directly to the Bot API over `fetch`. There is no library
// here on purpose: Looper is meant to be something you can drop into a directory
// and run, so it has nothing to install, and the four calls it needs (getMe,
// sendMessage, sendPhoto/sendDocument, getUpdates) are a few lines each.
//
// Long polling means no webhook and no public URL: the process asks Telegram for
// updates and Telegram holds the request open until something arrives.

import { readFile } from "node:fs/promises";
import { basename, extname } from "node:path";

/** One message from you, as Looper cares about it. */
export interface Incoming {
  /** Telegram's update id; the offset for the next poll is this plus one. */
  updateId: number;
  /** When Telegram stamped it, in milliseconds. */
  at: number;
  text: string;
}

interface Update {
  update_id: number;
  message?: { text?: string; date?: number; chat?: { id?: number | string } };
}

const imageExtensions = new Set([".png", ".jpg", ".jpeg", ".gif", ".webp"]);

/** Telegram's ceiling on one text message. */
const messageLimit = 4096;

export class Telegram {
  private readonly base: string;
  /** The one chat Looper talks to and listens to. Empty only during setup. */
  private readonly chatId: string;

  constructor(token: string, chatId: string) {
    // TELEGRAM_API_BASE exists so the tests can stand a fake Bot API in front of
    // the real one; in normal use it is unset.
    const api = process.env.TELEGRAM_API_BASE ?? "https://api.telegram.org";
    this.base = `${api}/bot${token}`;
    this.chatId = chatId;
  }

  /** The bot's own username, which doubles as a check that the token works. */
  async whoAmI(): Promise<string> {
    const me = (await this.call("getMe", {})) as { username?: string };
    return me.username ?? "unknown";
  }

  /**
   * Send text, split across messages when it is too long for one. Sent as plain
   * text — no parse_mode — because the agent writes markdown that Telegram's
   * parser rejects half the time, and a rejected message is worse than an
   * unstyled one.
   */
  async send(text: string): Promise<void> {
    for (const chunk of chunkText(text.trim() || "(empty message)", messageLimit)) {
      await this.call("sendMessage", { chat_id: this.chatId, text: chunk });
    }
  }

  /**
   * Send a file from disk, as a photo when it looks like an image and a document
   * otherwise. Photos display inline, which is the point when the agent is
   * showing you a screenshot; documents arrive byte-for-byte, which is the point
   * for anything else.
   */
  async sendFile(path: string, caption?: string): Promise<void> {
    const body = new FormData();
    body.set("chat_id", this.chatId);
    if (caption) body.set("caption", caption.slice(0, 1024));
    const bytes = await readFile(path);
    const file = new File([new Uint8Array(bytes)], basename(path));
    const photo = imageExtensions.has(extname(path).toLowerCase());
    body.set(photo ? "photo" : "document", file);
    await this.upload(photo ? "sendPhoto" : "sendDocument", body);
  }

  /**
   * Ask for updates newer than `offset`, waiting up to `seconds` for one to
   * arrive. Returns them raw; `poll` is the filtered version.
   */
  async pollAny(offset: number, seconds: number): Promise<Update[]> {
    return (await this.call(
      "getUpdates",
      { offset, timeout: seconds, allowed_updates: ["message"] },
      (seconds + 20) * 1000
    )) as Update[];
  }

  /**
   * The same poll, keeping only text messages from the configured chat. Anything
   * else — a sticker, a photo, another chat — still advances the offset, so it is
   * consumed once and never seen again.
   */
  async poll(offset: number, seconds: number): Promise<{ messages: Incoming[]; offset: number }> {
    const updates = await this.pollAny(offset, seconds);
    const messages: Incoming[] = [];
    let next = offset;
    for (const update of updates) {
      next = Math.max(next, update.update_id + 1);
      const message = update.message;
      if (!message?.text) continue;
      if (String(message.chat?.id) !== this.chatId) continue;
      messages.push({
        updateId: update.update_id,
        at: (message.date ?? 0) * 1000,
        text: message.text,
      });
    }
    return { messages, offset: next };
  }

  private async call(method: string, params: unknown, timeoutMs = 30_000): Promise<unknown> {
    const response = await fetch(`${this.base}/${method}`, {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify(params),
      signal: AbortSignal.timeout(timeoutMs),
    });
    return readResult(method, response);
  }

  private async upload(method: string, body: FormData): Promise<unknown> {
    const response = await fetch(`${this.base}/${method}`, {
      method: "POST",
      body,
      signal: AbortSignal.timeout(120_000),
    });
    return readResult(method, response);
  }
}

async function readResult(method: string, response: Response): Promise<unknown> {
  const payload = (await response.json().catch(() => null)) as
    | { ok?: boolean; result?: unknown; description?: string }
    | null;
  if (!payload?.ok) {
    const reason = payload?.description ?? `HTTP ${response.status}`;
    throw new Error(`Telegram ${method} failed: ${reason}`);
  }
  return payload.result;
}

/**
 * Split text into pieces no longer than `limit`, breaking at blank lines, then
 * newlines, then wherever it must. Only the last resort cuts a line in half.
 */
export function chunkText(text: string, limit: number): string[] {
  if (text.length <= limit) return [text];
  const chunks: string[] = [];
  let rest = text;
  while (rest.length > limit) {
    const window = rest.slice(0, limit);
    const at = Math.max(window.lastIndexOf("\n\n"), window.lastIndexOf("\n"));
    const cut = at > limit * 0.5 ? at : limit;
    chunks.push(rest.slice(0, cut).trimEnd());
    rest = rest.slice(cut).trimStart();
  }
  if (rest) chunks.push(rest);
  return chunks;
}

/**
 * Watch for a message sent to a bot and hand back the chat it came from. Used
 * during setup so nobody has to go and find their numeric id: you message the
 * bot, and the id comes from the message. Whatever is already queued is drained
 * first, so a stale message from somewhere else can't be mistaken for the answer.
 */
export async function detectChatId(token: string, timeoutMs = 5 * 60_000): Promise<string> {
  if (!token) throw new Error("A bot token is needed before a chat can be detected.");
  const api = new Telegram(token, "");
  const username = await api.whoAmI();
  console.log(`\nMessage @${username} on Telegram now — anything will do.`);

  let offset = 0;
  for (const update of await api.pollAny(0, 0)) offset = Math.max(offset, update.update_id + 1);

  const deadline = Date.now() + timeoutMs;
  while (Date.now() < deadline) {
    for (const update of await api.pollAny(offset, 25)) {
      offset = Math.max(offset, update.update_id + 1);
      const id = update.message?.chat?.id;
      if (id !== undefined) {
        console.log(`Found chat ${id}.`);
        return String(id);
      }
    }
  }
  throw new Error("No message arrived; run Looper again, or set TELEGRAM_CHAT_ID by hand.");
}
