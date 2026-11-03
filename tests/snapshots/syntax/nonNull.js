// @filename: main.syn
function __nonNull(v, m) {
  if (v === undefined || v === null) throw new TypeError(m)
  return v
}
"use wrapped";
function assert(a, b) {
  if (a !== b) {
    throw new Error(`${a} !== ${b}`)
  }
}
function message(fn) {
  try {
    fn();
  } catch(e) {
    if (!(e instanceof Error)) e = new Error(e);
    return e instanceof TypeError ? e.message : 'wrong error'
  }
  return 'no error'
}
class Box {
  value;
}
const m = new Map([['a', {
  x: 1
}]])
function direct(k) {
  const v = m.get(k)
  if (v === undefined || v === null) throw new TypeError("m.get(k) was nullish")
  return v.x
}
function nested(k) {
  return __nonNull(m.get(k), "m.get(k) was nullish").x
}
let late
late = 0;
const zero = late
const box = new Box()
box.value = 2;
assert(__nonNull(zero, "zero was nullish"), 0);
assert(__nonNull(box.value, "box.value was nullish"), 2);
assert(direct('a'), 1);
assert(nested('a'), 1);
assert(message(() => direct('b')), 'm.get(k) was nullish');
assert(message(() => nested('b')), 'm.get(k) was nullish');
assert(message(() => __nonNull(m.get("q\""), "m.get(\"q\\\"\") was nullish")), 'm.get("q\\"") was nullish');
