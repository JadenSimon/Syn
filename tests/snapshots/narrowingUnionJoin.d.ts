// @filename: main.syn
interface X extends Base {
  n: string
}
interface Y extends Base {
  n: number
}
type U = X | Y;
export declare function f(x: U): U;
