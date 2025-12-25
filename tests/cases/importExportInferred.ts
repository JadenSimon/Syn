// @filename: foo.ts
export function foo() {
    return 1
}

// @filename: foo2.ts
import { foo } from './foo'

const x = 1
export function foo2(a: number) {
    return foo()
}

