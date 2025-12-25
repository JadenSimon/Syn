// @filename: mod1.ts

export function foo() {
  return {
    bar: 1
  }
}
export function foo2() {
  return {
    bar2: 1
  }
}

// @filename: mod2.ts
export * from './mod1'

// @filename: mod3.ts
import { foo, foo2 } from './mod2'
export const results = foo()
export const results2 = foo2()
