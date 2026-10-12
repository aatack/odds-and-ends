import { electronApi } from './api.ts'
import { browserEnvironment } from './environment.ts'
import { mount } from './root.tsx'
import { Session } from './session.ts'

/**
 * The window: the core over IPC. The topics mode by default; the classic UI
 * (the modules) behind a flag, `DETAIL_VIEWS_UI=classic`, which the main
 * process passes on as `?ui=classic`.
 */
const topics = new URLSearchParams(location.search).get('ui') !== 'classic'
mount(new Session(electronApi(), browserEnvironment(), { topics }), { phone: false })
