// @filename: main.syn
Object.defineProperty(exports, "__esModule", { value: true });
exports.f = f;
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
    return typeof x === 'object' && x !== null && typeof x.y === 'string' && typeof x.x === 'string' && typeof x.z === 'string'
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
    return typeof x === 'object' && x !== null && typeof x.y === 'number' && typeof x.z === 'number' && typeof x.x === 'number'
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
  ;((x === 0) || (x === 1));
  ;(x === 0);
}
{
  const x2 = x
  const __switch = typeof x2 === 'object' && x2 !== null && typeof x2.y === 'number' ? 0 : x2 === 0 ? 1 : x2 === 1 ? 2 : x2 === 2 ? 3 : x2 === 3 ? 4 : x2 === 4 ? 5 : -1
  switch (__switch) {
    case 0: 
      throw x2.y
    case 1: 
    case 2: {
      x2;
      break
    }
    case 3: 
      throw x2
    case 4: 
      break
    case 5: {
      break
    }
  }
}
function f(cb) {
  const x = cb()
  const __switch = typeof x === 'object' && x !== null && typeof x.y === 'number' ? 0 : typeof x === 'object' && x !== null && typeof x.z === 'string' ? 1 : -1
  switch (__switch) {
    case 0: 
      return x.y
    case 1: 
      return x.z
  }
}
{
  function __is3(x) {
    return typeof x === 'object' && x !== null && (x.parent === null) || (__is3(x.parent)) && typeof x.id === 'number' && typeof x.code === 'string' && typeof x.name === 'string' && typeof x.sequence === 'number' && __is4(x.created_at)
  }
  function __is4(x) {
    return typeof x === 'object' && x !== null && typeof x.time === 'number' && typeof x.zone === 'number'
  }
  ;(__is3(x));
}
