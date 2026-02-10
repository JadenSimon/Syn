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
interface A2 {
  kind: '1'
  data: A | C
}
interface B2 {
  kind: '2'
  data: B
}
type U2 = A2 | B2;
export declare function refine(input: { x: U2 }): { data: C } | undefined;
