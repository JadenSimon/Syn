// @filename: mod1.ts
export declare function foo(): { bar: number };

// @filename: mod2.ts
export * as mod1 from './mod1'

// @filename: mod3.ts
export declare const results: { bar: number }
