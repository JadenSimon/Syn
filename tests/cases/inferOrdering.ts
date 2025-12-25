declare function flat<T>(args: T[] | T[][]): T;
type Value = 1 | 2
declare const n: Value[] | Value[][]
export const x = flat(n)