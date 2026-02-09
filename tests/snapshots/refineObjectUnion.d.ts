// @filename: main.syn
interface A {
  kind: 1
  data: string
}
interface B {
  kind: 2
  data: boolean
}
interface C {
  kind: 3
  data: number
}
interface D {
  kind: 4
  extra: number
}
type U = A | B | C | D;
export declare function refine(input: U): {
  kind: 1, 
  data: string
} | {
  kind: 2, 
  data: boolean
} | {
  kind: 3, 
  data: number
} | undefined;
export declare function refine2(input: U): {
  kind: 1, 
  data: string
} | {
  kind: 2, 
  data: boolean
} | {
  kind: 3, 
  data: number
} | 0 | undefined;
