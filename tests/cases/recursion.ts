type Node<T> =
    & { value: T; next: Node<T> | null }
    & (T extends string ? { tag: "str" } : unknown)

export type Node1 = Node<string>
export type Node2 = Node<number>

export function f() {
    const x = {} as Node1
    return x.tag
}

// this is emit is incorrect, we should not follow the inner alias
// export function f2() {
//     const x = {} as Node2
//     return x.next
// }