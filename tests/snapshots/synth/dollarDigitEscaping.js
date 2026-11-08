// @filename: main.syn
if (!Symbol.toComputation) {
  Object.defineProperty(Symbol, 'toComputation', {
    value: Symbol.for('toComputation')
  });
}
function __computation$(f, c) {
  f[Symbol.toComputation] = c;
  return f
}
const __meta$0 = {
  ambient: ['console']
}
function foo() {
  const $1 = 1
  const shorthand = {
    $1
  }
  return __computation$(function $2() {
    console.log($1);
  }, () => ['function $$2() {\n  console.log($0);\n}', [$1], __meta$0])
}
