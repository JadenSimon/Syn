/// <reference lib="es2015" />

export async function noTypeParams() {
    return 'hi'
}

export async function awaitedUnwraps() {
    const t = await noTypeParams()
    if (typeof t === 'string') {
        return true as const
    }
    return t
}
