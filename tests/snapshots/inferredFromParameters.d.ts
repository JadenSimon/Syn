// @filename: foo.ts
export declare function foo(): 'hi';
export declare function foo2(): 'hi';
export declare function foo3(): string;
export declare function foo4(): string;
export declare function foo5(): 'hi';
export declare function foo6(): 'hi';
export declare function _foo3<T>(a: T): T;
export declare function mapped1(): { hi: string };
export declare function _mapped2<T extends Record<string, any>>(a: T): { [P in keyof T]: { x: T[P] } };
export declare function mapped2(): { hi: { x: string } };
export declare function shaped<T extends number | boolean>(s: { t: T }): T;
export declare const shaped_1: boolean
export declare const shaped_2: number
export declare const f3: number
