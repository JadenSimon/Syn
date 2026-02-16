// @filename: main.syn
Object.defineProperty(exports, "__esModule", { value: true });
const _stack = []
try {
  __defer(_stack, () => console.log('test'))
  function assert(a, b) {
    if (a !== b) {
      throw new Error(`${a} !== ${b}`)
    }
  }
  let c = 0
  {
    c += 1;
    const _stack = []
    try {
      __defer(_stack, () => c -= 1)
      assert(c, 1);
    } catch(_) {
      var _error = _, _hasError = true
    } finally {
      __callDispose(_stack, _error, _hasError)
    }
  }
  assert(c, 0);
  function f1() {
    const _stack = []
    try {
      __defer(_stack, () => c -= 1)
      c += 1;
      assert(c, 1);
    } catch(_) {
      var _error = _, _hasError = true
    } finally {
      __callDispose(_stack, _error, _hasError)
    }
  }
  f1();
  f1();
  assert(c, 0);
  function f2() {
    const _stack = []
    try {
      __defer(_stack, () => c -= 1)
      c += 1;
      assert(c, 1);
      c += 1;
      __defer(_stack, () => {
        assert(c, 2);
        c -= 1;
      })
    } catch(_) {
      var _error = _, _hasError = true
    } finally {
      __callDispose(_stack, _error, _hasError)
    }
  }
  f2();
  assert(c, 0);
  let shouldDecrement = false
  function f3() {
    c += 1;
    const _stack = []
    try {
      __defer(_stack, () => {
        if (shouldDecrement) c -= 1;
      })
    } catch(_) {
      var _error = _, _hasError = true
    } finally {
      __callDispose(_stack, _error, _hasError)
    }
  }
  f3();
  assert(c, 1);
  shouldDecrement = true;
  c -= 1;
  f3();
  assert(c, 0);
  function maybe(ret = false) {
    if (!ret) return 
    return {
      v: 1
    }
  }
  {
    c += 1;
    const _stack = []
    try {
      __defer(_stack, () => {
        const _tmp_0 = maybe(true)
        if (_tmp_0 != null) {
          const { v } = _tmp_0
          c -= v;
        }
      })
      assert(c, 1);
    } catch(_) {
      var _error = _, _hasError = true
    } finally {
      __callDispose(_stack, _error, _hasError)
    }
  }
  assert(c, 0);
} catch(_) {
  var _error = _, _hasError = true
} finally {
  __callDispose(_stack, _error, _hasError)
}
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
