// @filename: foo.ts
import { foo2 } from 'bar'

export function foo() {
    return foo2()
}

// @filename: node_modules/bar/index.d.ts
export declare function foo2(a: number): number;

// @filename: node_modules/bar/package.json
({
    "types": "./index.d.ts"
})