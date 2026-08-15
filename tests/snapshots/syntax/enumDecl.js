// @filename: main.syn
function assert(cond) {
  if (!cond) throw new Error("Assertion failed")
}
const Test = {
  one: 1,
  two: 2,
  four: 4
}
console.log(Test);
assert(Test.one === 1);
assert(Test.two === 2);
assert(Test.four === 4);
