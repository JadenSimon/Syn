// @filename: main.ts
type A = { prop: string };
type B = { prop: string | number };
export type C = (A & B)['prop'];
export declare function f(): string;
