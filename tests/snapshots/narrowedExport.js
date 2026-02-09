// @filename: one.syn
Object.defineProperty(exports, "__esModule", { value: true });
exports.f = f;
function f(v) {
  if (typeof v === 'number') return v
  throw new Error(`not a number: ${v}`)
}

// @filename: two.syn
Object.defineProperty(exports, "__esModule", { value: true });
exports.x = void 0;
const one = require('./one')
const x = one.f(1)
exports.x = x;
