import { electronApi } from './api.ts'
import { browserEnvironment } from './environment.ts'
import { mount } from './root.tsx'
import { Session } from './session.ts'

/** The window: the core over IPC. */
mount(new Session(electronApi(), browserEnvironment()), { phone: false })
