// The notes server, reached directly rather than through the agent, and only to
// find out whether it is there. Everything else Looper does with notes it does by
// handing the agent an MCP server and letting it read its own task.
//
// It is worth its own check because the agent cannot report this failure well.
// `--strict-mcp-config` means the server the agent gets is the one named here and
// nothing else, so a stale url leaves the wake with no notes at all — no task to
// read, nowhere to write — and every wake fails the same way until someone
// notices. One `initialize` on the terminal costs nothing and says which of the
// two things is wrong.

/** The MCP handshake, sent once, for its answer's sake. */
const handshake = {
  jsonrpc: "2.0",
  id: 1,
  method: "initialize",
  params: {
    protocolVersion: "2025-06-18",
    capabilities: {},
    clientInfo: { name: "looper", version: "0.1.0" },
  },
};

/**
 * Ask the notes server who it is. Returns its name, or throws with a reason
 * written for whoever has to fix it.
 */
export async function whichNotes(url: string, token: string): Promise<string> {
  let response: Response;
  try {
    response = await fetch(url, {
      method: "POST",
      headers: {
        authorization: `Bearer ${token}`,
        "content-type": "application/json",
        // Streamable HTTP servers may answer either way, so both are accepted and
        // the body is read as text and unwrapped below.
        accept: "application/json, text/event-stream",
      },
      body: JSON.stringify(handshake),
      signal: AbortSignal.timeout(10_000),
    });
  } catch (error) {
    // A refused connection is the common one, and it is worth naming: it means
    // the server has moved or is not running, not that anything is misconfigured
    // about the token.
    const reason = (error as Error).message;
    throw new Error(
      /timed out|abort/i.test(reason)
        ? `nothing answered at ${url} within 10s`
        : `nothing is listening at ${url} (${reason})`
    );
  }
  if (response.status === 401 || response.status === 403) {
    throw new Error(`${url} refused the bearer token (HTTP ${response.status})`);
  }
  if (!response.ok) {
    throw new Error(`${url} answered HTTP ${response.status}`);
  }
  const name = readServerName(await response.text());
  if (!name) throw new Error(`${url} answered, but not as an MCP server`);
  return name;
}

/**
 * The server's name out of an `initialize` reply, whether it came back as plain
 * JSON or as a single SSE event.
 */
function readServerName(body: string): string | null {
  const json = body.trimStart().startsWith("{")
    ? body
    : (body.match(/^data: (.*)$/m)?.[1] ?? null);
  if (!json) return null;
  try {
    const payload = JSON.parse(json) as {
      result?: { serverInfo?: { name?: string } };
    };
    return payload.result?.serverInfo?.name ?? null;
  } catch {
    return null;
  }
}
