// A library of custom editors for the schema form and builder (see
// `../schema/editors.tsx` for what an editor is). Kept apart from the schema
// components, which know nothing of the app: an editor here may read the app's
// state, and adding one never touches the form or the builder.
//
// `EDITORS` is all of them, to hand to a form or a builder as `editors`, or to
// register once with `EditorsProvider`.

import type { Editors } from '../schema'
import { BASIC_EDITORS } from './basic'
import { TAB_EDITORS } from './tabs'

export { BASIC_EDITORS } from './basic'
export { TAB_EDITORS } from './tabs'

export const EDITORS: Editors = { ...BASIC_EDITORS, ...TAB_EDITORS }
