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
if (!((typeof p === 'object' && p !== null && typeof p.then === 'function' && typeof p.catch === 'function'))) {
  throw new Error(`not a promise!`)
}
const n = await p
function sync() {
  const p = foo()
  const p2 = foo()
  foo();
  foo();
  await p
}
async function _async() {
  const v = await foo()
  const p = foo()
  foo();
  await foo();
  sync();
  await p
  await v
  const v2 = await await foo()
}
function wrap(cb) {
  return cb()
}
function sync2() {
  return wrap(async () => {})
}
async function _async2() {
  await sync2();
  const p = await wrap(async () => {})
}
async function _async3() {
  const v = await wrap2(async () => {})
  const v2 = await (foo()).then(x => x + 1)
}
const m = new Map()
async function mapGet() {
  const p = m.get(1)
}
async function promises() {}
