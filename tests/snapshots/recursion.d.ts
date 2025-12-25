// @filename: main.ts
type Node<T> = {
  value: T, 
  next: Node<T> | null
} & (T extends string ? { tag: "str" } : unknown);
export type Node1 = Node<string>;
export type Node2 = Node<number>;
export declare function f(): "str";
