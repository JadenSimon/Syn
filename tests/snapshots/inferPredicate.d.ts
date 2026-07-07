// @filename: main.syn
type X<T> = T extends (x: any) => x is infer U ? U : never;
type Y = X<(x: any) => x is boolean>;
export declare const x: Y
export declare function f1(): number;
