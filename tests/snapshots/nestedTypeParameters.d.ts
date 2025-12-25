// @filename: main.ts
interface Shape<T> {
  method<U = T, V = never>(cb1: (v: T) => U, cb2?: () => V): Shape<U | V>
}
export declare const y: Shape<number | string>
export declare const y2: Shape<string>
export declare const y3: Shape<number>
export declare const y4: Shape<string>
