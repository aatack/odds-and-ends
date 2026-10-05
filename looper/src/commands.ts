// What you can say to Looper itself, as opposed to the agent, and how long it
// waits for you to finish talking before it starts a session.
//
// Messages are debounced: a session does not start while you are still typing,
// so a burst of messages arrives as one thought rather than being split across
// two sessions. `/wait` asks for a longer pause before the next session, for
// when there is a lot to say; `/go` starts one straight away.

/** A message that is an instruction to Looper rather than something for the agent. */
export type Command = "go" | "wait" | "start";

/**
 * The command a message is, if it is one: the whole message, in any case, with
 * the `@botname` Telegram adds in group chats allowed. `/start` is there so the
 * message Telegram sends when you first open a bot is not handed to the agent.
 * Anything else that starts with a slash is an ordinary message.
 */
export function readCommand(text: string): Command | null {
  const match = /^\/(go|wait|start)(?:@\w+)?$/i.exec(text.trim());
  return match ? (match[1].toLowerCase() as Command) : null;
}

export interface Waiting {
  now: number;
  /** When the next session is due anyway. */
  until: number;
  /** When you last sent anything, command or message; 0 for never. */
  lastHeard: number;
  /** How long you must have been quiet before a session may start. */
  quiet: number;
  /** Messages the agent has not seen and that have not had a turn at waking it. */
  fresh: number;
  /** Whether a fresh message should bring the session forward. */
  wakeOnMessage: boolean;
  /** Whether you have said /go. */
  go: boolean;
}

/**
 * What ends a wait, or null while it should go on. `/go` ends it whatever else is
 * true. Otherwise nothing starts while you are mid-burst — not even a session
 * that is due — and once you have been quiet for long enough, a fresh message
 * starts one early, and a due one starts on time.
 */
export function nextMove(waiting: Waiting): "go" | "message" | "due" | null {
  if (waiting.go) return "go";
  if (waiting.now - waiting.lastHeard < waiting.quiet) return null;
  if (waiting.now >= waiting.until) return "due";
  if (waiting.wakeOnMessage && waiting.fresh > 0) return "message";
  return null;
}
