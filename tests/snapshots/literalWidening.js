// @filename: main.syn
Object.defineProperty(exports, "__esModule", { value: true });
exports.f = f;
exports.arr = exports.y3 = exports.y2 = exports.y1 = exports.x3 = exports.x2 = exports.x1 = void 0;
const FAILURE = "FAILURE"
function doWork() {
  return FAILURE
}
function isSuccess(result) {
  return !isFailure(result)
}
function isFailure(result) {
  return result === FAILURE
}
let result = doWork()
function f() {
  if (isSuccess(result)) {
    return result
  }
}
let x1 = widening('a')
exports.x1 = x1;
let x2 = widening(10)
exports.x2 = x2;
let x3 = widening(cond ? 'a' : 10)
exports.x3 = x3;
let y1 = nonWidening('a')
exports.y1 = y1;
let y2 = nonWidening(10)
exports.y2 = y2;
let y3 = nonWidening(cond ? 'a' : 10)
exports.y3 = y3;
const langCodes = []
const arr = map(langCodes, code => ({
  code
}))
exports.arr = arr;
