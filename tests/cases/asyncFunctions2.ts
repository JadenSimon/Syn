/// <reference lib="es2015" />

let q = 0
export async function foo() {
    const x: Promise<number> = {} as any
    const y: Promise<string> = {} as any
    if (q === 0) {
        return x
    }
    return y
}

type Box<out T> = { value: T }
export async function foo2() {
    const x: Box<number> = {} as any
    const y: Box<string> = {} as any
    if (q === 0) {
        return x
    }
    return y
}

