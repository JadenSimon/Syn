// @filename: main.ts
type Renamed<T> = { [P in keyof T as `name_${P & string}`]: T[P] };
export type Renamed_1 = Renamed<{ x: string }>;
export type Renamed_2 = Renamed_1 extends { name_x: string } ? true : false;
type Prop<T> = T & string;
export type Prop_1 = Prop<'foo'>;
export declare function prop(): 1 | 0;
export declare function main(...args: string[]): string;
type Box<out T> = { value: T };
export type A = Box<string> | Box<number>;
export type B = A extends { value: infer U } ? U : never;
export declare const x: B
