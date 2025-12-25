// @filename: main.ts
export declare const nested: (x?: (y?: (z: any) => any) => void) => (y?: (z: any) => any) => void
export declare const templated: (a: any, b: any, c: any, k: any) => string
export declare const templated2: (a: any, b?: string) => ''
export declare const templated3: (a: any, b?: string) => ''
export declare const taggedTemplate: (a: any, b?: void) => ''
export declare const regexp: (a?: {}) => {}
export declare const regexp2: (a?: {}) => {}
export declare const regexp3: (a?: {}) => {}
export declare const typed: (a: any) => boolean
export declare const conditional: (() => true) | false
export declare const conditional2: (() => true) | false
export declare const conditional3: { y2: any } | false
export declare const conditional4: { y2: any } | false
export declare const conditional5: (() => any) | false
export declare const parens1: () => any
export type Fn1 = () => true;
export type Fn2 = (() => true);
export type Fn3<T> = T extends () => infer U ? (a: U) => void : false;
export type Fn4<T> = T extends () => infer U ? ((a: U) => void) : false;
