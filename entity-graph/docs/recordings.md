# Recordings

A **recording** is a meeting transcribed as it is spoken, with notes kept of it
while it goes on. The notes are the point: they are on screen during the
meeting, filling in as people talk, so gaps can be seen and filled while the
people who can fill them are still in the room.

## What one holds

`type: recording` on a note, and a `recording` value naming its two children:

| child | what it is |
| --- | --- |
| `transcriptId` | **Transcript**: one note per sentence heard, in order |
| `notesId` | **Notes**: a section the notes are kept under |

Both are made by **Start a recording** and found only through that value, so
they can be renamed or moved freely. `recording` is a builtin type
(`core/builtins.ts`), so an agent over MCP can read which child is which.

## The two loops

The store joins them; neither calls the other.

1. **Listening.** The microphone streams to Deepgram's live API
   (`helpers/liveTranscription.ts`). Each sentence it hears for good is written
   under the transcript as an **open task**. What is still being said shows in
   the pill and is not written.
2. **Structuring.** Every 20 seconds, the transcript's open lines are handed to
   `claude.runPrompt` (Sonnet, no directory), which folds them into the notes
   over MCP. Once it answers, the app ticks the lines off.

So the open lines *are* the queue. A failed pass, a pause or a restart loses
nothing heard: the lines stay open until a pass takes them.

- **One pass at a time.** Two sessions writing one set of notes would undo each
  other, so a tick with a pass in flight is skipped.
- **A new session every six passes.** A resumed session reads the whole
  conversation again on every turn; the notes are the memory worth keeping, so a
  fresh session given the rules again is cheaper and loses nothing.
- **Pausing** stops the microphone. The loop runs on until the lines already
  heard are in the notes, then stops.

The rules the session is given are in `systemPrompt` in
`tools/recordingTools.ts`.

## Tools

| tool | what it does |
| --- | --- |
| **Start a recording** (`recording.start`) | a recording under the selected note, listening at once |
| **Pause or resume the recording** (`recording.toggle`) | the pill's button |
| **Bring the recording's notes up to date** (`recording.structure`) | one pass now |

The last two act on the recording named, or else on the one that is live.

## The pill

A recording row draws a large pill under its text (`components/RecordingPill.tsx`)
with the pause button and what is being heard. Whether the microphone is open is
a fact about this window, not the note, so it lives in a runtime atom
(`state/recordings.ts`): a recording seen after a restart, or anywhere else, reads
as paused.

## The key

`DEEPGRAM_API_KEY` in `.env`. The socket is in the renderer — that is where the
microphone is, and the main process has no WebSocket client on Electron 31's
Node — so the key is handed over on its own IPC channel (`transcription:key`),
never as a tool: a tool is listed in the palette and reachable from a script.
Deepgram takes it as a WebSocket subprotocol.

## Known edges

- **Only the microphone.** The other side of a call is heard only as far as the
  microphone hears the speakers.
- **Open lines are tasks.** For the seconds before a pass takes them, the
  transcript's lines are open tasks like any other, and the walk to the next
  task can land on one.
- **The session needs the notes over MCP.** It runs in a scratch directory, so
  it gets whatever MCP servers `claude` has at user level.
