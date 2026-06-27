// @filename: main.syn
type X = [a?: X];
export declare function f(): X | undefined;
type X2<T> = [a?: X2<T>, T?];
export declare function f2(): X2<number> | undefined;
export declare function f3(): number | undefined;
export declare function f4(): number | undefined;
