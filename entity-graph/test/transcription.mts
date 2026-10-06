// The reading of Deepgram's live messages: segments firm up, finals gather into
// an utterance, and an utterance ends — on `speech_final`, or on `UtteranceEnd`
// when the pause was never clean enough to call — as sentences.
//
//   npm test

import assert from 'node:assert/strict'

const { hear, flush, hearing, sentences, NOTHING_HEARD } = await import(
  '../src/renderer/src/helpers/liveTranscription'
)

const result = (transcript: string, isFinal: boolean, speechFinal = false): unknown => ({
  type: 'Results',
  is_final: isFinal,
  speech_final: speechFinal,
  channel: { alternatives: [{ transcript }] },
})

// Sentences split after the stop, and keep a closing quote with the sentence.
assert.deepEqual(sentences('We ship Friday. Does that work? "Yes." Great'), [
  'We ship Friday.',
  'Does that work?',
  '"Yes."',
  'Great',
])
assert.deepEqual(sentences('   '), [])

// An interim is shown and not kept.
let step = hear(NOTHING_HEARD, result('we ship', false))
assert.deepEqual(step.said, [])
assert.equal(hearing(step.heard), 'we ship')

// A final replaces the interim and waits for the utterance to end.
step = hear(step.heard, result('We ship Friday.', true))
assert.deepEqual(step.said, [])
assert.equal(hearing(step.heard), 'We ship Friday.')

// `speech_final` ends it, with everything final so far.
step = hear(step.heard, result('Does that work?', true, true))
assert.deepEqual(step.said, ['We ship Friday.', 'Does that work?'])
assert.equal(hearing(step.heard), '')

// `UtteranceEnd` ends one too, and an empty final changes nothing.
step = hear(NOTHING_HEARD, result('Next item.', true))
step = hear(step.heard, result('', true))
step = hear(step.heard, { type: 'UtteranceEnd' })
assert.deepEqual(step.said, ['Next item.'])

// Anything else is ignored.
step = hear(NOTHING_HEARD, { type: 'Metadata' })
assert.equal(step.heard, NOTHING_HEARD)

// Stopping keeps even what was only guessed at: it will not be heard again.
assert.deepEqual(flush({ finals: ['One.'], interim: 'two' }).said, ['One.', 'two'])

console.log('transcription: ok')
