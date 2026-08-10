// @filename: main.syn
const __assertU32 = v => {
  assert(Number.isInteger(v) && v >= 0 && v <= 4294967295);
  return v
}
const __wrapU32 = v => v >>> 0
const __wrapU8 = v => v & 255
"use checked";
function addu32(a, b) {
  return __assertU32(a + b)
}
function add(a, b) {
  "use wrapped";
  let c = 0
  const d = __wrapU32(c + 1)
  const d2 = __wrapU32(d + 1)
  const d3 = __wrapU32(d + d2)
  const d4 = __wrapU32(d3 + a)
  return __wrapU32(__wrapU32(a + b) + d4)
}
function addu8() {
  "use wrapped";
  let c = 0
  const d = __wrapU8(c + 1)
  const d2 = __wrapU8(d - 1)
}
function loop() {
  for (let i = 0; i < 100; i++) {}
}
const y = 1
console.log(y >= 0 && y <= 4294967295);
{
  const y = 1
  console.log(y >= -2147483648 && y <= 2147483647);
  console.log(true);
  console.log(y >= 0 && y <= 65535);
  console.log(Math.f16round(y) === y);
}
{
  const f = 0.1231232
  Math.f16round(f) === f;
  true;
  typeof f === 'number';
}
{
  const x = 1
  if (x <= 127) {
    true;
  }
}
