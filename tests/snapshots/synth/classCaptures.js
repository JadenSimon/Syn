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
  ambient: ['console']
}
function it(name, fn) {}
function expectEqual(actual, expected, message) {}
expectEqual[Symbol.toComputation] = () => ['function expectEqual(actual, expected, message) {}', []];
class A {
  foo() {
    return 'foo'
  }
  static {
    this.prototype.foo[Symbol.toComputation] = () => ['function foo() {\n  return \'foo\'\n}', []];
    this[Symbol.toComputation] = () => ['class A {\n  foo() {\n    return \'foo\'\n  }\n}', []];
  }
}
it('constructor', __computation$(() => {
  expectEqual(new A().foo(), 'foo');
}, () => ['() => {\n  $0(new $1().foo(), \'foo\');\n}', [expectEqual, A]]));
const B = class {
  foo() {
    return 'foo'
  }
  static {
    this.prototype.foo[Symbol.toComputation] = () => ['function foo() {\n  return \'foo\'\n}', []];
    this[Symbol.toComputation] = () => ['class {\n  foo() {\n    return \'foo\'\n  }\n}', []];
  }
}
it('anonymous class', __computation$(() => {
  expectEqual(new B().foo(), 'foo');
}, () => ['() => {\n  $0(new $1().foo(), \'foo\');\n}', [expectEqual, B]]));
const foo = new B().foo
it('anonymous class (method)', __computation$(() => {
  expectEqual(foo(), 'foo');
}, () => ['() => {\n  $0($1(), \'foo\');\n}', [expectEqual, foo]]));
const getValSym = Symbol.for('getVal')
{
  const suffix = '!'
  class D {
    constructor(val) {
      this.val = val;
    }
    getVal() {
      return `${this.val}${suffix}`
    }
    static {
      this.prototype.getVal[Symbol.toComputation] = () => ['function getVal() {\n  return `${this.val}${$0}`\n}', [suffix]];
      this[Symbol.toComputation] = () => ['class D {\n  constructor(val) {\n    this.val = val;\n  }\n  getVal() {\n    return `${this.val}${$0}`\n  }\n}', [suffix]];
    }
  }
  const d = new D('bar4')
  const getVal4 = d.getVal
  it('methods (captured)', __computation$(() => {
    expectEqual(getVal4.call({
      val: 'bar4'
    }), 'bar4!');
  }, () => ['() => {\n  $0($1.call({\n    val: \'bar4\'\n  }), \'bar4!\');\n}', [expectEqual, getVal4]]));
  class E {
    constructor(val) {
      this.val = val;
    }
    getVal(count) {
      return `${this.val}${suffix}`.repeat(count)
    }
    static {
      this.prototype.getVal[Symbol.toComputation] = () => ['function getVal(count) {\n  return `${this.val}${$0}`.repeat(count)\n}', [suffix]];
      this[Symbol.toComputation] = () => ['class E {\n  constructor(val) {\n    this.val = val;\n  }\n  getVal(count) {\n    return `${this.val}${$0}`.repeat(count)\n  }\n}', [suffix]];
    }
  }
  const e = new E('bar5')
  const getVal5 = e.getVal
  it('methods (captured with args)', __computation$(() => {
    expectEqual(getVal5.call({
      val: 'bar5'
    }, 2), 'bar5!bar5!');
  }, () => ['() => {\n  $0($1.call({\n    val: \'bar5\'\n  }, 2), \'bar5!bar5!\');\n}', [expectEqual, getVal5]]));
}
{
  const y = 2
  class F {
    static x = 1;
    static foo() {
      return this.x + y
    }
    static {
      this.foo[Symbol.toComputation] = () => ['function foo() {\n  return this.x + $0\n}', [y]];
      this[Symbol.toComputation] = () => ['class F {\n  static x = 1;\n  static foo() {\n    return this.x + $0\n  }\n}', [y]];
    }
  }
  it('static methods', __computation$(() => {
    expectEqual(F.foo(), 3);
  }, () => ['() => {\n  $0($1.foo(), 3);\n}', [expectEqual, F]]));
}
{
  class X {
    #foo = 1;
    static m() {
      const x = new this()
      x.#foo = 2;
      return x
    }
    m() {
      return this.#foo
    }
    m2() {
      return this.#foo.toString()
    }
    #privateMethod() {
      return this.#foo + 1
    }
    plusOne() {
      return this.#privateMethod()
    }
    static {
      this.m[Symbol.toComputation] = () => ['function m() {\n  const x = new this()\n  x.#foo = 2;\n  return x\n}', [], undefined, X];
      this.prototype.m[Symbol.toComputation] = () => ['function m() {\n  return this.#foo\n}', [], undefined, X];
      this.prototype.m2[Symbol.toComputation] = () => ['function m2() {\n  return this.#foo.toString()\n}', [], undefined, X];
      this.prototype.plusOne[Symbol.toComputation] = () => ['function plusOne() {\n  return this.#privateMethod()\n}', [], undefined, X];
      this[Symbol.toComputation] = () => ['class X {\n  #foo = 1;\n  static m() {\n    const x = new this()\n    x.#foo = 2;\n    return x\n  }\n  m() {\n    return this.#foo\n  }\n  m2() {\n    return this.#foo.toString()\n  }\n  #privateMethod() {\n    return this.#foo + 1\n  }\n  plusOne() {\n    return this.#privateMethod()\n  }\n}', []];
    }
  }
  const x = new X()
  it('private members', __computation$(() => {
    expectEqual(x.m(), 1);
  }, () => ['() => {\n  $0($1.m(), 1);\n}', [expectEqual, x]]));
  it('private members (nested)', __computation$(() => {
    expectEqual(x.m2(), '1');
  }, () => ['() => {\n  $0($1.m2(), \'1\');\n}', [expectEqual, x]]));
  it('private method', __computation$(() => {
    expectEqual(x.plusOne(), 2);
  }, () => ['() => {\n  $0($1.plusOne(), 2);\n}', [expectEqual, x]]));
}
let outer = 5
const c__outer = [outer]
const e = 1
class Z {
  z = 1;
  e = e;
  getFn() {
    c__outer[0] = 6;
    const f = __computation$(() => this.z, () => ['() => $0.z', [this]])
    return f
  }
  getFn2() {
    return __computation$(function() {
      console.log(c__outer[0]);
      return this
    }, () => ['function() {\n  console.log($0[0]);\n  return this\n}', [c__outer], __meta$0])
  }
  static getFn2() {
    return __computation$(function() {
      console.log(c__outer[0]);
      return this
    }, () => ['function() {\n  console.log($0[0]);\n  return this\n}', [c__outer], __meta$0])
  }
  [getValSym]() {
    return c__outer[0]
  }
  static [getValSym]() {
    return c__outer[0]
  }
  get x() {
    return this.e
  }
  static {
    this.prototype.getFn[Symbol.toComputation] = () => ['function getFn() {\n  $0[0] = 6;\n  const f = () => this.z\n  return f\n}', [c__outer]];
    this.prototype.getFn2[Symbol.toComputation] = () => ['function getFn2() {\n  return function() {\n    console.log($0[0]);\n    return this\n  }\n}', [c__outer], __meta$0];
    this.getFn2[Symbol.toComputation] = () => ['function getFn2() {\n  return function() {\n    console.log($0[0]);\n    return this\n  }\n}', [c__outer], __meta$0];
    this.prototype[getValSym][Symbol.toComputation] = () => ['function() {\n  return $0[0]\n}', [c__outer]];
    this[getValSym][Symbol.toComputation] = () => ['function() {\n  return $0[0]\n}', [c__outer]];
    Object.getOwnPropertyDescriptor(this.prototype, 'x').get[Symbol.toComputation] = () => ['function x() {\n  return this.e\n}', []];
    this[Symbol.toComputation] = () => ['class Z {\n  z = 1;\n  e = $0;\n  getFn() {\n    $1[0] = 6;\n    const f = () => this.z\n    return f\n  }\n  getFn2() {\n    return function() {\n      console.log($1[0]);\n      return this\n    }\n  }\n  static getFn2() {\n    return function() {\n      console.log($1[0]);\n      return this\n    }\n  }\n  [getValSym]() {\n    return $1[0]\n  }\n  static [getValSym]() {\n    return $1[0]\n  }\n  get x() {\n    return this.e\n  }\n}', [e, c__outer], __meta$0];
  }
}
