// @filename: main.syn
type Box<T> = {
  get: () => T, 
  set: (value: T) => void
};
export declare const bn1: Box<{ prop: number }>
export declare const bn2: Box<{ prop: number }>
export declare const bb1: Box<{ prop: boolean }>
export declare const bb2: Box<{ prop: boolean }>
