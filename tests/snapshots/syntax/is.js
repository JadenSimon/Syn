// @filename: main.syn
Object.defineProperty(exports, "__esModule", { value: true });
const x = null
{
  const v = {
    kind: 'b',
    data: 'hi'
  }
  if ((typeof v.data === 'string')) {}
}
{
  const x = ['']
  if ((Array.isArray(x) && x.every((el) => typeof el === 'string'))) {}
}
{
  const x = ''
  if (((typeof x === 'string') || (typeof x === 'number'))) {}
}
{
  const x = ''
  if (((x === 2) || (x === 1))) {}
  ;(x === 'a');
}
{
  const x = ''
  if ((Array.isArray(x) && x.length === 1 && typeof x[0] === 'number')) {}
  if ((Array.isArray(x) && x.length === 2 && typeof x[0] === 'number' && typeof x[1] === 'string')) {}
  if ((Array.isArray(x) && x.length === 1 && typeof x[0] === 'object' && x[0] !== null && typeof x[0].a === 'string')) {}
  if ((Array.isArray(x))) {}
  if ((Array.isArray(x) && x.length >= 1 && typeof x[0] === 'number' && x.slice(1).every((el) => typeof el === 'string'))) {}
}
{
  const x = ''
  function __is0(x) {
    return typeof x === 'object' && x !== null && typeof x.x === 'string' && typeof x.y === 'string' && typeof x.z === 'string'
  }
  if ((__is0(x))) {}
}
{
  const x = ''
  class X {}
  if ((x instanceof X)) {}
  function __is1(x) {
    return x instanceof X && typeof x.y === 'number'
  }
  if ((__is1(x))) {}
}
{
  function __is2(x) {
    return typeof x === 'object' && x !== null && typeof x.z === 'number' && typeof x.x === 'number' && typeof x.y === 'number'
  }
  ;((Array.isArray(x) && x.every((el) => __is2(el))) || (__is2(x)));
}
{
  ;(typeof x === 'string' && /^foo.*bar$/.test(x));
  ;(typeof x === 'string' && /^foo(?:1\.1|bb|aa)bar$/.test(x));
}
{
  const X = {}
  X[X["a"] = 0] = "a"
  X[X["b"] = 1] = "b"
  function __is3(x) {
    return typeof x === 'object' && x !== null && x.a === 0 && x.b === 1
  }
  ;(__is3(x));
}
