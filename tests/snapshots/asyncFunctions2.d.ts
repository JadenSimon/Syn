// @filename: main.ts
export declare function foo(): Promise<number | string>;
type Box<out T> = { value: T };
export declare function foo2(): Promise<Box<number> | Box<string>>;
