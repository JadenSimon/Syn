// @filename: main.syn
function leaf() {
  return 1
}
leaf[Symbol.for('toComputation')] = () => ['function leaf() {\n  return 1\n}', []];
export function exported() {
  return leaf()
}
exported[Symbol.for('toComputation')] = () => ['function exported() {\n  return $0()\n}', [leaf]];
export const arrow = Object.assign(() => exported(), {
  [Symbol.for('toComputation')]: () => ['() => $0()', [exported]]
})
function named() {
  return 2
}
named[Symbol.for('toComputation')] = () => ['function named() {\n  return 2\n}', []];
const alias = named
export { alias as renamed }
function defaultCapture() {
  return 3
}
defaultCapture[Symbol.for('toComputation')] = () => ['function defaultCapture() {\n  return 3\n}', []];
export default Object.assign(() => defaultCapture(), {
  [Symbol.for('toComputation')]: () => ['() => $0()', [defaultCapture]]
});
