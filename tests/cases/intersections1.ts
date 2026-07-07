type A = { prop: string }
type B = { prop: string | number }
export type C = (A & B)['prop']

export function f() {
    const x: C = 'a' // FIXME: we should not emit error here
    if (typeof x === 'string') {
        return x
    }
    throw new Error('not a string')
}

type D<T> = T extends { x: infer U } ? U : never

export function f2() {
    const x: D<{ y: number } & { x: 1 }> = 'a'
    const x2: D<{ y: number } & { x: 1 }> = 1
}
