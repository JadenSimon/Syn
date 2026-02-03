// @filename: main.syn
Object.defineProperty(exports, "__esModule", { value: true });
exports.f1 = f1;
exports.f2 = f2;
exports.f3 = f3;
exports.f5 = f5;
exports.v1 = exports.f4 = void 0;
function f1() {
  return 'hi'
}
function f2(cond) {
  if (cond) return 'bye'
  return 'hi'
}
function f3(cond) {
  if (cond) return 'bye'
}
const f4 = (cond) => cond ? 1 : 0
exports.f4 = f4;
let v1 = 1
exports.v1 = v1;
function f5(cond) {
  if (cond) return false
}
