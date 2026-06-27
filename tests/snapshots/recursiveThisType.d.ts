// @filename: main.syn
interface X {
  a: this
  b: () => this
  c: () => this extends { z: infer U } ? U : never
  d: () => this extends { z: infer U } ? [U, this] : never
}
export declare function f(): X;
export declare function f2(): () => X;
export declare function f3(): X;
interface X2 extends X {
  z: number
  zz: this
  f<T>(x: T): this & T
}
export declare function f4(): () => number;
export declare function f5(): [number, X2];
export declare function f6(): X2;
export declare function f7(): X2 & { a: number };
type Foo<T> = T extends { a: infer U } ? { b: U } : never;
interface X3 extends X {
  f(): Foo<this>
  f2(a: this): (typeof a)['f']
  f3(a: { b: number }): (typeof a)['b']
}
export declare function f9(): { b: X3 };
export declare function f10(): () => { b: X3 };
export declare function f11(): number;
