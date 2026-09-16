import React, { useState } from 'react'
import { HelpCircle } from '@untitledui/icons'
import type { NodeConfig, NodeStatus, SourceNode } from '../../../../core/client'
import { relativeTime } from '../../helpers/time'
import { Field } from '../ui/Field'
import { IconButton } from '../ui/IconButton'
import { Modal } from '../ui/Modal'
import type { SourceGraphActions } from '../../views/useSourceGraph'
import { DraftInput } from './DraftInput'

// The body of the two nodes that *do* something rather than being something.
//
// A feed holds secrets, which is why they are here and not in a note: this graph
// is the app's own file, not a pensive, so a token written on a node is not a
// token published with the store. Nothing here is ever copied into one.
//
// Each field is a thing to go and fetch from somewhere else, so each has a line
// under the help icon saying where — the questions are all "which page of
// api.slack.com?" and "which scope?", and answering them in a browser tab that
// isn't this one is where the time goes.

type FeedConfig = Extract<NodeConfig, { kind: 'slackEvents' } | { kind: 'githubEvents' }>

/** How far a feed has read, said in the tense somebody would say it in. */
function readTo(config: FeedConfig): string | null {
  const at =
    config.kind === 'slackEvents'
      ? Number(config.cursor) * 1000
      : config.cursor
        ? Date.parse(config.cursor)
        : NaN
  return Number.isFinite(at) && at > 0 ? `Read up to ${relativeTime(at)}` : null
}

export function FeedBody({
  node,
  config,
  status,
  actions,
}: {
  node: SourceNode
  config: FeedConfig
  status: NodeStatus | undefined
  actions: SourceGraphActions
}): React.JSX.Element {
  const [helping, setHelping] = useState(false)
  const set = (patch: Partial<FeedConfig>): void =>
    void actions.updateNode(node.id, { config: { ...config, ...patch } as NodeConfig })

  return (
    <>
      {config.kind === 'slackEvents' ? (
        <>
          <Field label="User token">
            <DraftInput
              mono
              type="password"
              value={config.userToken}
              placeholder="xoxp-…"
              onCommit={(userToken) => set({ userToken })}
            />
          </Field>
          <Field label="App token">
            <DraftInput
              mono
              type="password"
              value={config.appToken}
              placeholder="xapp-… (optional)"
              onCommit={(appToken) => set({ appToken })}
            />
          </Field>
          <Field label="Never write">
            <DraftInput
              mono
              value={config.muted}
              placeholder="C0123ABCD, D0456EFGH"
              onCommit={(muted) => set({ muted })}
            />
          </Field>
        </>
      ) : (
        <Field label="Token">
          <DraftInput
            mono
            type="password"
            value={config.token}
            placeholder="empty — use `gh auth token`"
            onCommit={(token) => set({ token })}
          />
        </Field>
      )}

      <div className="flex items-start gap-1">
        <p className="min-w-0 flex-1 text-xs text-gray-400">
          {node.paused
            ? // Said here as well as in the line below, because for these two it
              // means something different: not "calls are refused" but "nothing
              // is running", and the catch-up on restart is the reassurance.
              'Not running. Starting it again reads everything said in the meantime.'
            : (status?.activity ?? readTo(config) ?? 'Waiting to start.')}
        </p>
        <span className="nodrag nopan">
          <IconButton title="Where these come from" onClick={() => setHelping(true)}>
            <HelpCircle size={16} />
          </IconButton>
        </span>
      </div>

      {helping && (
        <Modal
          title={config.kind === 'slackEvents' ? 'Setting up Slack' : 'Setting up GitHub'}
          onClose={() => setHelping(false)}
        >
          {config.kind === 'slackEvents' ? <SlackHelp /> : <GithubHelp />}
          <Inbox />
        </Modal>
      )}
    </>
  )
}

/** One field's worth of instructions: what it is, and where it comes from. */
function Help({ label, children }: { label: string; children: React.ReactNode }): React.JSX.Element {
  return (
    <div className="space-y-1">
      <p className="text-[13px] font-semibold text-gray-900">{label}</p>
      <div className="space-y-1.5 text-[13px] text-gray-600">{children}</div>
    </div>
  )
}

const Mono = ({ children }: { children: React.ReactNode }): React.JSX.Element => (
  <code className="rounded bg-gray-50 px-1 font-mono text-xs text-gray-700">{children}</code>
)

/** The same for both, and the thing somebody will look for first. */
function Inbox(): React.JSX.Element {
  return (
    <Help label="Where it all lands">
      <p>
        Everything new is linked under the entity <Mono>@inbox</Mono>, in whatever store is plugged
        into this node. Nothing creates that note for you — link it in wherever you want it with{' '}
        <b>Link entity to…</b> and the id, and the inbox fills up underneath it.
      </p>
    </Help>
  )
}

function SlackHelp(): React.JSX.Element {
  return (
    <>
      <Help label="User token">
        <p>
          A <Mono>xoxp-…</Mono> token from your own Slack app at <Mono>api.slack.com/apps</Mono> →
          OAuth &amp; Permissions → <b>User</b> Token Scopes. It sees what you see, DMs and private
          channels included. A bot token will not do: Slack&rsquo;s search is user-token only, under
          any scope.
        </p>
        <p>
          Scopes: <Mono>search:read</Mono>, <Mono>channels:history</Mono>, <Mono>groups:history</Mono>,{' '}
          <Mono>im:history</Mono>, <Mono>mpim:history</Mono>, <Mono>channels:read</Mono>,{' '}
          <Mono>groups:read</Mono>, <Mono>im:read</Mono>, <Mono>mpim:read</Mono>,{' '}
          <Mono>users:read</Mono>, <Mono>reactions:read</Mono>.
        </p>
        <p>
          A new scope does nothing until the app is reinstalled to the workspace, and reinstalling
          issues a new token — so copy the new one in here afterwards.
        </p>
      </Help>
      <Help label="App token">
        <p>
          A <Mono>xapp-…</Mono> token with <Mono>connections:write</Mono>, made on the app&rsquo;s
          Basic Information page. It buys Socket Mode: the same messages arrive seconds after they
          are sent rather than on the next poll. Leave it empty and the node polls only, which loses
          nothing but the promptness.
        </p>
        <p>
          Subscribe the events under <b>on behalf of users</b> rather than the bot list:{' '}
          <Mono>message.channels</Mono>, <Mono>message.groups</Mono>, <Mono>message.im</Mono>,{' '}
          <Mono>message.mpim</Mono>, <Mono>reaction_added</Mono>, <Mono>reaction_removed</Mono>. A
          workspace admin may have to approve the app first.
        </p>
      </Help>
      <Help label="Never write">
        <p>
          Conversation ids to leave out, comma-separated — the <Mono>C…</Mono> / <Mono>D…</Mono> id
          from a channel&rsquo;s link. Everything else you can see is written, muted or not: what is
          worth reading later is not the same question as what is worth a red dot now.
        </p>
      </Help>
    </>
  )
}

function GithubHelp(): React.JSX.Element {
  return (
    <>
      <Help label="Token">
        <p>
          A token with the <Mono>notifications</Mono> scope. Leave the field empty and the node uses
          whatever <Mono>gh</Mono> is signed in as on this machine, which is the ordinary case — add
          the scope to it with:
        </p>
        <p>
          <Mono>gh auth refresh --scopes notifications</Mono>
        </p>
      </Help>
      <Help label="What it reads">
        <p>
          Your notifications, which is one call covering every repository. A notification only comes
          for a thread you are subscribed to, so your own open pull requests are swept up separately
          every quarter of an hour.
        </p>
      </Help>
    </>
  )
}
