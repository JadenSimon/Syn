// @filename: main.syn
function foo(t) {
  if (t) throw new Error("no")
  const $1 = 1
  const shorthand = {
    $1
  }
  return Object.assign(function $2() {
    console.log($1);
    throw new Error('aa')
  }, {
    [Symbol.for('toComputation')]: () => ['function $$2() {\n  console.log($0);\n  throw new Error(\'aa\')\n}', [$1]]
  })
}
foo[Symbol.for('toComputation')] = () => ['function foo(t) {\n  if (t) throw new Error("no")\n  const $$1 = 1\n  const shorthand = {\n    $1: $$1\n  }\n  return function $$2() {\n    console.log($$1);\n    throw new Error(\'aa\')\n  }\n}', []];
try {
  foo(true);
} catch(e) {
  if (!(e instanceof Error)) e = new Error(e);
  console.log(e);
}
const x = foo()
x();
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbXSwibWFwcGluZ3MiOiJTQUNTLElBQUk7TUFDTCxTQUFBLElBQUE7UUFDRTtRQUNBO0lBQWM7O2dDQUNKO0lBQ1osUUFBUSxJQUFJO1VBQ04sSUFBSTs7aUhBSlI7OztBQUZEOztFQVNIO1FBQW1CO1FBSWpCLGFBQWEsUUFBUSxJQUFJLElBQUksTUFBTTtFQUpiLFFBQVEsSUFBSTs7TUFDcEMsSUFBSTtBQUNWIn0=
