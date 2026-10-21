// @filename: main.syn
'use strict';
if (!Symbol.toComputation) {
  Object.defineProperty(Symbol, 'toComputation', {
    value: Symbol.for('toComputation')
  });
}
const key = (Symbol).toComputation
const descriptor = Object.getOwnPropertyDescriptor(Symbol, 'toComputation')
if (!descriptor || key !== Symbol.for('toComputation') || descriptor.value !== key || descriptor.writable || descriptor.configurable || descriptor.enumerable) {
  throw new Error('Symbol.toComputation must be a frozen property using the registered key')
}
function first() {
  return 1
}
first[Symbol.toComputation] = () => ['function first() {\n  return 1\n}', []];
function second() {
  return 2
}
second[Symbol.toComputation] = () => ['function second() {\n  return 2\n}', []];
const functions = [first, second, Object.assign(() => 3, {
  [Symbol.toComputation]: () => ['() => 3', []]
})]
for (const fn of functions) {
  if (!(fn)[key]) throw new Error('function metadata must use Symbol.toComputation')
}
class Example {
  method() {
    return 4
  }
  get value() {
    return 5
  }
  static {
    this.prototype.method[Symbol.toComputation] = () => ['function method() {\n  return 4\n}', []];
    Object.getOwnPropertyDescriptor(this.prototype, 'value').get[Symbol.toComputation] = () => ['function value() {\n  return 5\n}', []];
    this[Symbol.toComputation] = () => ['class Example {\n  method() {\n    return 4\n  }\n  get value() {\n    return 5\n  }\n}', []];
  }
}
const getter = Object.getOwnPropertyDescriptor(Example.prototype, 'value').get
if (!(Example)[key] || !(Example.prototype.method)[key] || !(getter)[key]) {
  throw new Error('class metadata must use Symbol.toComputation')
}
