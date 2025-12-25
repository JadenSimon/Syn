// @filename: main.ts
{
  const _stack = []
  try {
    __defer(_stack, () => console.log('hi'))
  } catch(_) {
    var _error = _, _hasError = true
  } finally {
    __callDispose(_stack, _error, _hasError)
  }
}
export default function foo() {}
function __knownSymbol(name, symbol) {
  return (symbol = Symbol[name]) ? symbol : Symbol.for("Symbol." + name);
}
function __callDispose(stack, error, hasError) {
  var E = typeof SuppressedError === "function" ? SuppressedError : function(e, s, m, _2) {
    return _2 = Error(m), _2.name = "SuppressedError", _2.error = e, _2.suppressed = s, _2;
  };
  var fail = (e) => error = hasError ? new E(e, error, "An error was suppressed during disposal") : (hasError = true, e);
  var next = (it) => {
    while (it = stack.pop()) {
      try {
        var result = it[1] && it[1].call(it[2]);
        if (it[0])
          return Promise.resolve(result).then(next, (e) => (fail(e), next()));
      } catch (e) {
        fail(e);
      }
    }
    if (hasError)
      throw error;
  };
  return next();
}
function __defer(stack, value, async) {
  if (typeof value !== "function")
      throw TypeError("Function expected");
  stack.push([async, value]);
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbIi9Vc2Vycy9qYWRlbnNpbW9uL1Byb2plY3RzL3N5bmFwc2UtdHlwZXNjcmlwdC1wcml2YXRlL3Rlc3RzL21haW4udHMiXSwibWFwcGluZ3MiOiI7OzswQkFDVSxRQUFROzs7Ozs7O3dCQUVNOzs7In0=
