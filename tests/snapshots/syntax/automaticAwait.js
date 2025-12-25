// @filename: main.syn
Object.defineProperty(exports, "__esModule", { value: true });
async function foo() {
  return 1
}
const v = await foo() + await foo()
if (v !== 2) {
  throw new Error(`not 2: ${v}`)
}
const p = foo()
if (!(typeof p === 'object' && p !== null && typeof p.then === 'function' && typeof p.catch === 'function')) {
  throw new Error(`not a promise?`)
}
let x
if (!(x === undefined)) {}
const o = {}
for (const [k, v] of Object.entries(o)) {}
const o2 = {
  *[Symbol.iterator]() {}
}
for (const x of o2) {}
