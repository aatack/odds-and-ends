import type { ObjectSchema } from '../../shared/types'
import { Markdown } from './components/ui/Markdown'

/** A schema's fields as a table: name (with ? when it may be left out), type, and what it is. */
export function Params({ schema }: { schema: ObjectSchema }) {
  const params = Object.entries(schema.properties ?? {})
  if (!params.length) return null
  return (
    <table className="params">
      <tbody>
        {params.map(([name, p]) => (
          <tr key={name}>
            <td>
              <code>{name}</code>
              {schema.required?.includes(name) ? '' : '?'}
            </td>
            <td className="muted">{p.enum ? p.enum.join(' | ') : Array.isArray(p.type) ? p.type.join(' | ') : (p.type ?? 'any')}</td>
            <td>{p.description && <Markdown text={p.description} />}</td>
          </tr>
        ))}
      </tbody>
    </table>
  )
}
