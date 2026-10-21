// @filename: main.syn
if (!Symbol.toComputation) {
  Object.defineProperty(Symbol, 'toComputation', {
    value: Symbol.for('toComputation')
  });
}
function leaf() {
  return 1
}
leaf[Symbol.toComputation] = () => ['function leaf() {\n  return 1\n}', []];
export function exported() {
  return leaf()
}
exported[Symbol.toComputation] = () => ['function exported() {\n  return $0()\n}', [leaf]];
export const arrow = Object.assign(() => exported(), {
  [Symbol.toComputation]: () => ['() => $0()', [exported]]
})
function named() {
  return 2
}
named[Symbol.toComputation] = () => ['function named() {\n  return 2\n}', []];
const alias = named
export { alias as renamed }
function defaultCapture() {
  return 3
}
defaultCapture[Symbol.toComputation] = () => ['function defaultCapture() {\n  return 3\n}', []];
export default Object.assign(() => defaultCapture(), {
  [Symbol.toComputation]: () => ['() => $0()', [defaultCapture]]
});
