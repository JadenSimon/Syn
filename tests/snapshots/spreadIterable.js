// @filename: main.syn
const arr = []
const iter = arr[Symbol.iterator]()
const arr2 = ['a', ...iter]
export const x = () => arr2
