// @filename: main.syn
Object.defineProperty(exports, "__esModule", { value: true });
exports.f = f;
const arr = [1]
const v = arr["\u0030"]
const o = {
  ['a\
b`']: 1
}
const x = o[`a\u0062\``]
function f() {
  return [v, x]
}
