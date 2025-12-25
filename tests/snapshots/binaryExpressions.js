// @filename: main.ts
const isPrototypeProperty = true
const namespaceSymbol = {}
const createSymbolTable = () => ({
  symbols: {}
})
export const symbolTable = isPrototypeProperty ? (namespaceSymbol.members || (namespaceSymbol.members = createSymbolTable())) : (namespaceSymbol.exports || (namespaceSymbol.exports = createSymbolTable()))
let x
const f1 = (y) => ({
  y
})
const f2 = () => ({
  z: 1
})
export const q = false ? x ??= f1(f2()) : f2()
