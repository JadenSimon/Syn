// @filename: main.syn
Object.defineProperty(exports, "__esModule", { value: true });
exports.f = f;
const x = {}
if (typeof x === null) {}
if (typeof x === undefined) {}
let y
if (y === null) {}
let z
if (typeof x === z) {}
let z2
if (typeof x === z2) {}
let init = 1
if (!init) {
  throw new Error("bad init")
}
function f(a = init) {
  return a
}
