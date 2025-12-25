export declare function f1(x: unknown): x is number
export declare function f2(x: unknown): asserts x is number
export declare function f3(number: unknown): asserts number is number
export declare function f4(number: unknown): asserts number
export declare function f5(fn: unknown): asserts fn is (x: any) => asserts x is true
export declare function f6<T, U extends T>(fn: (v: T) => v is U, arr: T[]): U[]