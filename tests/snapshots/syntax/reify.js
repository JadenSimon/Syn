// @filename: main.syn
Object.defineProperty(exports, "__esModule", { value: true });
function stringy(t) {
  const r = {}
  for (const [k, v] of Object.entries(t)) {
    r[k] = (() => {
      switch (Type.kind(v)) {
        case 'literal': 
          return String(v)
        case 'object': 
          return '<object>'
        case 'intrinsic': 
          return v.description
        default: 
          return 'never'
      }
    })();
  }
  return r
}
console.log(stringy(__reify(__filename, 107)));
const f = __reify(__filename, 116, 114)
console.log(f(__reify(__filename, 124)));
