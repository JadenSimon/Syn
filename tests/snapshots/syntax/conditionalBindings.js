// @filename: main.ts
function assert(a, b) {
  if (a !== b) {
    throw new Error(`${a} !== ${b}`)
  }
}
let c = 0
const arr = [1, 2, 3]
let _tmp_0
while (_tmp_0 = arr.pop()) {
  const x = _tmp_0
  c += x;
}
assert(c, 6);
