/**
 * An error as a caller should read it. A zod failure is the case worth handling:
 * its `message` is a JSON dump of every issue, which is no use to anyone at the
 * other end of an HTTP call or an MCP tool.
 *
 * Duck-typed rather than `instanceof ZodError`, so this file stays free of zod
 * and can be reached from either side of the HTTP seam.
 */
export function formatError(e: unknown): string {
  const issues = (e as { issues?: { path: (string | number)[]; message: string }[] } | null)?.issues
  if (Array.isArray(issues)) {
    return issues.map((i) => `${i.path.join('.') || '(root)'}: ${i.message}`).join('; ')
  }
  return e instanceof Error ? e.message : String(e)
}
