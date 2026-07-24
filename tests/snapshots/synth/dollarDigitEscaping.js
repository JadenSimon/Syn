// @filename: main.syn
function foo() {
  const $1 = 1
  const shorthand = {
    $1
  }
  return Object.assign(function $2() {
    console.log($1);
  }, {
    [Symbol.for('toComputation')]: () => ['function $$2() {\n  console.log($0);\n}', [$1]]
  })
}
foo[Symbol.for('toComputation')] = () => ['function foo() {\n  const $$1 = 1\n  const shorthand = {\n    $1: $$1\n  }\n  return function $$2() {\n    console.log($$1);\n  }\n}', []];
