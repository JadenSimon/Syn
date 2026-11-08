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
  ambient: ['console', 'Error']
}
function foo(t) {
  if (t) throw new Error("no")
  const $1 = 1
  const shorthand = {
    $1
  }
  return __computation$(function $2() {
    console.log($1);
    throw new Error('aa')
  }, () => ['function $$2() {\n  console.log($0);\n  throw new Error(\'aa\')\n}', [$1], __meta$0])
}
try {
  foo(true);
} catch(e) {
  if (!(e instanceof Error)) e = new Error(e);
  console.log(e);
}
const x = foo()
x();
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbXSwibWFwcGluZ3MiOiI7Ozs7Ozs7Ozs7OztTQUNTLElBQUk7TUFDTCxTQUFBLElBQUE7UUFDRTtRQUNBO0lBQWM7O2lDQUNKO0lBQ1osUUFBUSxJQUFJO1VBQ04sSUFBSTttRkFKUjs7O0VBT0o7UUFBbUI7UUFJakIsYUFBYSxRQUFRLElBQUksSUFBSSxNQUFNO0VBSmIsUUFBUSxJQUFJOztNQUNwQyxJQUFJO0FBQ1YifQ==
