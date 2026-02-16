// @filename: main.syn
Object.defineProperty(exports, "__esModule", { value: true });
exports.f = f;
const init = 1
if (!init) {
  throw new Error("bad init")
}
function f(a = init) {
  return a
}
