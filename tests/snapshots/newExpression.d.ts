// @filename: main.syn
interface Span {
  readonly start: number
}
export declare function getSpan(): undefined | Span;
export declare function getAllSpans(): Span[];
export declare function getSortedSpans(): Span[];
export declare function f1(): number;
export declare function f2(): { w: { a: 1 } };
export declare function f3(): boolean | number;
export declare function f4(): number;
