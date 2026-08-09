// @filename: main.syn
function assert(cond) {
  if (!cond) throw new Error("Assertion failed")
}
assert((__reify(__filename, 21))['a'] === __reify(__filename, 26));
assert((__reify(__filename, 32))['b'] === __reify(__filename, 37));
assert((__reify(__filename, 43))['a'] !== __reify(__filename, 50));
const structInfo = __reify(__filename, 70)
assert(Type.isArrayType(structInfo.b));
assert(structInfo.c[0] === __reify(__filename, 90));
