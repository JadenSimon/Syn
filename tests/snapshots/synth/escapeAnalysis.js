// @filename: main.syn
const key = Symbol.for('toComputation')
function accept(fn) {
  return fn()
}
class Consumer {
  constructor(fn) {
    fn();
  }
  static {
    this[Symbol.for('toComputation')] = () => ['class Consumer {\n  constructor(fn) {\n    fn();\n  }\n}', []];
  }
}
function localOnly() {
  return 1
}
const localArrow = () => localOnly()
const localExpression = function() {
  return localArrow()
}
localExpression();
if ((localOnly)[key] || (localArrow)[key] || (localExpression)[key]) {
  throw new Error('direct calls must not escape')
}
function callArg() {
  return 2
}
callArg[Symbol.for('toComputation')] = () => ['function callArg() {\n  return 2\n}', []];
accept(callArg);
function newArg() {
  return 3
}
newArg[Symbol.for('toComputation')] = () => ['function newArg() {\n  return 3\n}', []];
new Consumer(newArg);
function arrayElement() {
  return 4
}
arrayElement[Symbol.for('toComputation')] = () => ['function arrayElement() {\n  return 4\n}', []];
const array = [arrayElement, Object.assign(() => 5, {
  [Symbol.for('toComputation')]: () => ['() => 5', []]
})]
function propertyValue() {
  return 6
}
propertyValue[Symbol.for('toComputation')] = () => ['function propertyValue() {\n  return 6\n}', []];
function shorthand() {
  return 7
}
shorthand[Symbol.for('toComputation')] = () => ['function shorthand() {\n  return 7\n}', []];
const object = {
  value: propertyValue,
  shorthand,
  inline: Object.assign(function() {
    return 8
  }, {
    [Symbol.for('toComputation')]: () => ['function() {\n  return 8\n}', []]
  })
}
function assigned() {
  return 9
}
assigned[Symbol.for('toComputation')] = () => ['function assigned() {\n  return 9\n}', []];
let target = localOnly
target = assigned;
if (!(callArg)[key] || !(newArg)[key] || !(arrayElement)[key] || !(propertyValue)[key] || !(shorthand)[key] || !(assigned)[key] || !(array[1])[key] || !(object.inline)[key]) {
  throw new Error('escaping functions need metadata')
}
function transitiveLeaf() {
  return 10
}
transitiveLeaf[Symbol.for('toComputation')] = () => ['function transitiveLeaf() {\n  return 10\n}', []];
function transitiveMiddle() {
  return transitiveLeaf()
}
transitiveMiddle[Symbol.for('toComputation')] = () => ['function transitiveMiddle() {\n  return $0()\n}', [transitiveLeaf]];
const alias = transitiveMiddle
const aliasAgain = alias
accept(Object.assign(() => aliasAgain(), {
  [Symbol.for('toComputation')]: () => ['() => $0()', [aliasAgain]]
}));
if (!(transitiveLeaf)[key] || !(transitiveMiddle)[key]) {
  throw new Error('captures through aliases need metadata')
}
function cycleA(n) {
  return n ? cycleB(n - 1) : 0
}
cycleA[Symbol.for('toComputation')] = () => ['function cycleA(n) {\n  return n ? $0(n - 1) : 0\n}', [cycleB]];
function cycleB(n) {
  return n ? cycleA(n - 1) : 0
}
cycleB[Symbol.for('toComputation')] = () => ['function cycleB(n) {\n  return n ? $0(n - 1) : 0\n}', [cycleA]];
const cycle = {
  cycleA
}
if (!(cycleB)[key]) throw new Error('capture cycles need metadata')
function localCounter() {
  let count = 0
  function increment() {
    return ++count
  }
  increment();
  return increment()
}
if (localCounter() !== 2) throw new Error('local mutation changed')
function factory() {
  let count = 0
  const c__count = [count]
  const increment = Object.assign(() => ++c__count[0], {
    [Symbol.for('toComputation')]: () => ['() => ++$0[0]', [c__count]]
  })
  return increment
}
const counter = factory()
if (!(counter)[key] || counter() !== 1 || counter() !== 2) {
  throw new Error('returned closure lost its capture')
}
const conciseFactory = () => Object.assign(() => 11, {
  [Symbol.for('toComputation')]: () => ['() => 11', []]
})
const returned = conciseFactory()
if (!(returned)[key]) throw new Error('concise arrow return must escape')
function conditionOnly() {
  return true
}
function choiceA() {
  return 12
}
choiceA[Symbol.for('toComputation')] = () => ['function choiceA() {\n  return 12\n}', []];
const choiceB = Object.assign(() => 13, {
  [Symbol.for('toComputation')]: () => ['() => 13', []]
})
const choice = conditionOnly() ? choiceA : choiceB
accept(choice);
if ((conditionOnly)[key] || !(choiceA)[key] || !(choiceB)[key]) {
  throw new Error('only conditional value arms escape')
}
function insideLocal() {
  function nested() {
    return 14
  }
  nested[Symbol.for('toComputation')] = () => ['function nested() {\n  return 14\n}', []];
  accept(nested);
  if (!(nested)[key]) throw new Error('nested escape was skipped')
}
insideLocal();
function paramLeaf() {
  return 15
}
paramLeaf[Symbol.for('toComputation')] = () => ['function paramLeaf() {\n  return 15\n}', []];
function paramDefault() {
  return paramLeaf()
}
paramDefault[Symbol.for('toComputation')] = () => ['function paramDefault() {\n  return $0()\n}', [paramLeaf]];
function withDefault(fn = paramDefault) {
  return fn()
}
function withInlineDefault(fn = Object.assign(() => 16, {
  [Symbol.for('toComputation')]: () => ['() => 16', []]
})) {
  if (!(fn)[key]) throw new Error('inline parameter initializer must escape')
  return fn()
}
function withBindingDefault({ fn = Object.assign(() => 17, {
  [Symbol.for('toComputation')]: () => ['() => 17', []]
}) } = {}) {
  if (!(fn)[key]) throw new Error('binding initializer must escape')
  return (fn)()
}
withDefault();
withInlineDefault();
withBindingDefault({});
if (!(paramDefault)[key] || !(paramLeaf)[key] || (withDefault)[key]) {
  throw new Error('parameter initializer and its captures must escape')
}
