// @filename: mod1.ts
export function foo() {
    return { bar: 1 }
}

// @filename: mod2.ts
export * as mod1 from './mod1'

// @filename: mod3.ts
import * as mod2 from './mod2'

export const results = mod2.mod1.foo()

