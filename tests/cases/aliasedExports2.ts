// @filename: mod1.ts
export function foo() {
    return { bar: 1 }
}
export function foo2() {
    return 'i am foo2'
}

// @filename: mod2.ts
export { foo, foo2 as foo3 } from './mod1'

// @filename: mod3.ts
import { foo, foo3 } from './mod2'

export const results = foo()
export const results3 = foo3()

