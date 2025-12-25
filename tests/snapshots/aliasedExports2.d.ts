// @filename: mod1.ts
export declare function foo(): { bar: number };
export declare function foo2(): string;

// @filename: mod2.ts
export { foo, foo2 as foo3 } from './mod1'

// @filename: mod3.ts
export declare const results: { bar: number }
export declare const results3: string
