// The parts of handling your messages that are decisions rather than plumbing:
// which messages are commands, when a wait ends, and what a message says to the
// agent. All pure, so they run anywhere, with no fake Telegram or `claude`.

import { test } from "node:test";
import assert from "node:assert/strict";
import { nextMove, readCommand } from "../src/commands.ts";
import type { Waiting } from "../src/commands.ts";
import { readMessage } from "../src/telegram.ts";

test("a command is the whole message, in any case, with or without the bot's name", () => {
  assert.equal(readCommand("/go"), "go");
  assert.equal(readCommand(" /WAIT "), "wait");
  assert.equal(readCommand("/go@aatack_scout_bot"), "go");
  assert.equal(readCommand("/start"), "start");
  // Anything more than the command is a message, and so is any other slash.
  assert.equal(readCommand("/go and use sqlite"), null);
  assert.equal(readCommand("/etc/hosts is wrong"), null);
  assert.equal(readCommand("go"), null);
});

const minute = 60_000;
const base: Waiting = {
  now: 100 * minute,
  until: 200 * minute,
  lastHeard: 0,
  quiet: minute,
  fresh: 0,
  wakeOnMessage: true,
  go: false,
};

test("a burst of messages is waited out before a session starts", () => {
  // Thirty seconds after the last of them: still waiting for you.
  assert.equal(nextMove({ ...base, fresh: 3, lastHeard: base.now - 30_000 }), null);
  // A minute of quiet: the session starts early, with all three.
  assert.equal(nextMove({ ...base, fresh: 3, lastHeard: base.now - minute }), "message");
});

test("a session that falls due while you are typing waits for you to finish", () => {
  const due = { ...base, until: base.now - 1 };
  assert.equal(nextMove({ ...due, fresh: 1, lastHeard: base.now - 10_000 }), null);
  assert.equal(nextMove({ ...due, fresh: 1, lastHeard: base.now - minute }), "due");
  assert.equal(nextMove({ ...due, lastHeard: 0 }), "due");
});

test("/wait's longer quiet holds the session back for longer", () => {
  const waiting = { ...base, quiet: 5 * minute, fresh: 2 };
  assert.equal(nextMove({ ...waiting, lastHeard: base.now - 2 * minute }), null);
  assert.equal(nextMove({ ...waiting, lastHeard: base.now - 5 * minute }), "message");
});

test("/go starts a session at once, whatever else is true", () => {
  assert.equal(nextMove({ ...base, go: true, quiet: 5 * minute, lastHeard: base.now }), "go");
  assert.equal(nextMove({ ...base, go: true, wakeOnMessage: false }), "go");
});

test("without a message, nothing starts before the session is due", () => {
  assert.equal(nextMove(base), null);
  // And a message does not cut short a wait that it must not, like a usage cap.
  assert.equal(nextMove({ ...base, fresh: 1, wakeOnMessage: false }), null);
});

test("every kind of message reaches the agent as something it can read", () => {
  assert.equal(readMessage({ text: "use sqlite" }), "use sqlite");
  assert.equal(
    readMessage({ caption: "this screen is wrong", photo: [{}] }),
    "[The user sent a photo. Looper cannot show it to you.] this screen is wrong"
  );
  assert.equal(
    readMessage({ voice: {} }),
    "[The user sent a voice message. Looper cannot show it to you.]"
  );
  assert.equal(
    readMessage({ text: "use postgres" }, true),
    "[The user changed an earlier message to this text.] use postgres"
  );
  assert.equal(readMessage({}), null);
});
