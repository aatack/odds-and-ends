// Stylesheets are the bundler's business: in a test run, an imported `.css`
// file is an empty module rather than a syntax error. Loaded with `--import`.
// tsx compiles the tests to CommonJS, so the hook is on `require` as well as on
// the ES module loader.
import Module, { register } from 'node:module'

Module._extensions['.css'] = () => {}

register(
  'data:text/javascript,' +
    encodeURIComponent(`
      export async function load(url, context, next) {
        if (url.endsWith('.css')) return { format: 'module', source: '', shortCircuit: true }
        return next(url, context)
      }
    `),
)
