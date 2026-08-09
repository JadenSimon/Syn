// @filename: main.syn
"use checked";
function addu32(a, b) {
  return a + b
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
console.log(__reify(__filename, 104));
