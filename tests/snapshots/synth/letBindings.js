// @filename: main.syn
function it(name, fn) {}
function expectEqual(actual, expected, message) {}
let c = 1
function fooFactory() {
  let x
  const foo = () => x
  x = c++;
  return foo
}
const foo1 = fooFactory()
const foo2 = fooFactory()
expectEqual(foo1(), 1);
expectEqual(foo2(), 2);
it('uses a different symbol binding id for each function', () => {
  expectEqual(foo1(), 1);
  expectEqual(foo2(), 2);
});
function sharedFactory() {
  let x
  const get = () => x
  const inc = () => void x++
  x = c++;
  function createIncTwo() {
    const two = 2
    return function() {
      x += two;
    }
  }
  return {
    get,
    inc,
    incTwo: createIncTwo()
  }
}
const shared = sharedFactory()
expectEqual(shared.get(), 3);
shared.inc();
expectEqual(shared.get(), 4);
it('shares scoped bindings across functions', () => {
  expectEqual(shared.get(), 4);
  shared.inc();
  expectEqual(shared.get(), 5);
});
it('shares scoped bindings with nested functions', () => {
  expectEqual(shared.get(), 4);
  shared.incTwo();
  expectEqual(shared.get(), 6);
});
function fib(n) {
  return n <= 1 ? n : doFib(n)
}
function doFib(n) {
  return fib(n - 2) + fib(n - 1)
}
expectEqual(fib(10), 55);
it('handles late bindings', () => {
  expectEqual(fib(10), 55);
});
const arr = []
function push(val) {
  arr.push(val);
}
function pop() {
  return arr.pop()
}
it('shares arrays across functions', () => {
  push(1);
  expectEqual(pop(), 1);
});
const fns = []
for (let i = 0; i < 2; i++) {
  fns[i] = () => i;
}
it('captures loop variable correctly in closures (constant)', () => {
  expectEqual(fns[0](), 0);
  expectEqual(fns[1](), 1);
});
const fns2 = []
const fns3 = []
for (let i = 0; i < 2; i++) {
  fns[i] = () => i;
  fns2[i] = () => i++;
}
it('captures loop variable correctly in closures (mutable)', () => {
  expectEqual(fns2[0](), 0);
  expectEqual(fns2[1](), 1);
  expectEqual(fns3[1](), 1);
  expectEqual(fns2[1](), 2);
});
