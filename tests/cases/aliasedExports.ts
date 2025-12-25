// @filename: mod1.ts
type MyType = { bar: number }
export function foo() {
    return { bar: 1 } as MyType
}

export type MyType2 = { bar2: number }
export function foo2(): MyType2 {
    return { bar2: 1 }
}

// @filename: mod2.ts
export * from './mod1'

// @filename: mod3.ts
import { foo, foo2 } from './mod2'

export const results = foo()

// this should show as `import('./mod1').foo2`
export const results2 = foo2()

