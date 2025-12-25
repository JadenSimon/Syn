// @filename: mod1.ts
type MyType = { bar: number };
export declare function foo(): MyType;
export type MyType2 = { bar2: number };
export declare function foo2(): MyType2;

// @filename: mod2.ts
export * from './mod1'

// @filename: mod3.ts
export declare const results: { bar: number }
export declare const results2: import("./mod1").MyType2
