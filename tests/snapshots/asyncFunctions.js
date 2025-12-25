// @filename: main.ts
export async function noTypeParams() {
  return 'hi'
}
export async function awaitedUnwraps() {
  const t = await noTypeParams()
  if (typeof t === 'string') {
    return true
  }
  return t
}
