// @filename: main.syn
function assert(cond) {
  if (!cond) throw new Error("Assertion failed")
}
const mask = 0xffffffffn
const ten = 10n
function low(x) {
  return x & 0xffffffffn
}
function wide() {
  return 0x1_0000_0000n
}
assert(typeof mask === 'bigint');
assert(mask === 4294967295n);
assert(low(2n ** 40n + 7n) === 7n);
assert(ten * 2n === 20n);
assert(0b101n + 0o7n === 12n);
assert(wide() === mask + 1n);

// @filename: exports.syn
export const mask = 0xffffffffn
export function wide() {
  return 0x1_0000_0000n
}
