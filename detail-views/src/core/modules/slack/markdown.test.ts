import assert from 'node:assert/strict'
import { test } from 'node:test'
import { slackToMarkdown } from './markdown.ts'

const resolve = {
  user: (id: string) => (id === 'U1' ? { name: 'Sam', target: 'slack:conv:D1' } : { name: 'Priya', target: null }),
  channel: (id: string) => (id === 'C1' ? 'slack:conv:C1' : null),
}
const md = (text: string) => slackToMarkdown(text, resolve)

test('bold, italic, strike', () => {
  assert.equal(md('*bold* _it_ ~gone~'), '**bold** _it_ ~~gone~~')
  assert.equal(md('2 * 3 * 4'), '2 * 3 * 4')
})

test('mentions and channels become mention links', () => {
  assert.equal(md('hi <@U1> and <@U2|priya>'), 'hi [Sam](mention:U1/slack:conv:D1) and [Priya](mention:U2/)')
  assert.equal(md('see <#C1|eng> or <#C9|x>'), 'see [#eng](mention:C1/slack:conv:C1) or [#x](mention:C9/)')
  assert.equal(md('<!here> look'), '**@here** look')
})

test('links', () => {
  assert.equal(md('<https://a.com|the site> <https://b.com>'), '[the site](https://a.com) <https://b.com>')
})

test('code is left as written, entities decoded', () => {
  assert.equal(md('run `a *b* &lt;c&gt;` now'), 'run `a *b* <c>` now')
  assert.equal(md('look:\n```const x = *y*\n```'), 'look:\n```\nconst x = *y*\n```\n')
})

test('quotes, bullets, emoji, stray markup', () => {
  assert.equal(md('&gt; quoted\n• one\n• two'), '> quoted\n- one\n- two')
  assert.equal(md('#1 priority :rocket:'), '\\#1 priority 🚀')
  assert.equal(md('a &lt;b&gt; &amp; c'), 'a \\<b> & c')
})
