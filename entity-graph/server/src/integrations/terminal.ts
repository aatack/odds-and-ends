import { platform } from 'os'
import { z } from 'zod'
import type { ToolDef } from '../../../src/core/source/index'
import { detach, directory, onPath } from './exec'

// A terminal, standing where you already are. The gesture every changeset wants
// and none of the others gives it: a worktree is a directory on this machine, and
// what anybody does with a directory is open a shell in it.
//
// Which terminal is deliberately not something to configure twice. `$TERMINAL`
// decides it where it is set — that being the variable the desktops already agree
// on — and otherwise the first of the usual ones that is installed. None of them
// has to be told where to start: the directory goes to `spawn`, so this needs to
// know nothing about each one's flag for it.
//
// It is the one integration that starts something and lets go. See `detach`.

/**
 * Tried in order, and the first one installed wins. `x-terminal-emulator` leads
 * because on Debian and Ubuntu it is whatever the desktop settled on, which is a
 * better answer than any name below it; `xterm` trails because it is the one
 * every X install has and nobody chose.
 */
const TERMINALS = [
  'x-terminal-emulator',
  'gnome-terminal',
  'konsole',
  'xfce4-terminal',
  'ghostty',
  'kitty',
  'alacritty',
  'wezterm',
  'foot',
  'xterm',
]

/** macOS has one answer and no PATH question: `open` hands it to the desktop. */
const MAC_TERMINAL = 'Terminal'

export const TERMINAL_TOOLS: ToolDef[] = [
  {
    id: 'terminal.open',
    name: 'Open a terminal',
    description: [
      'Open a terminal window on this machine, standing in `path`. `~/repos/x`',
      'works, and the tool hands back the absolute directory it used along with the',
      'terminal it started.',
      '',
      'It returns as soon as the window exists rather than when it closes — the',
      'window outlives the call, and the server has let go of it by then, so',
      'stopping the server leaves it open.',
      '',
      'On macOS this is `Terminal`. Elsewhere it is `$TERMINAL` if that is set, and',
      `otherwise the first of \`${TERMINALS.join('`, `')}\` that is installed. There is`,
      'nothing to configure beyond that variable.',
    ].join('\n'),
    safety: 'dangerous',
    args: z.object({
      path: z.string().min(1).describe('The directory to open it in — `~/repos/x` works'),
    }),
    handler: async ({ path }) => {
      const cwd = directory(path)
      if (platform() === 'darwin') {
        await detach('open', ['-a', MAC_TERMINAL, cwd])
        return { path: cwd, terminal: MAC_TERMINAL }
      }
      const chosen = [process.env.TERMINAL, ...TERMINALS].find((name) => !!name && onPath(name))
      if (!chosen) {
        throw new Error(
          `No terminal on this server's PATH — tried ${TERMINALS.join(', ')}. Set $TERMINAL`,
        )
      }
      await detach(chosen, [], { cwd })
      return { path: cwd, terminal: chosen }
    },
  },
]
