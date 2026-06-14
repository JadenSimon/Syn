// @filename: main.syn
var __template = ((c,p) => s => {
  c[0] ||= setTimeout(() => c={})
  return s in c ? (c[s][1] ||= p(s)).cloneNode(1) : (c[s] = [p(s)])[0]
})({}, (s,t=document.createElement('template')) => (t.innerHTML = s, t.content.firstChild))
var __sym_upd = Symbol.update ||= Symbol.for('update')
var __slot_s = (v,a,b) => {
  let p, n = a.nextSibling
  for (p of v) n === p ? n=n.nextSibling : n.before(p)
  while (p = n, n = p.nextSibling, p !== b) p.remove()
}
var __slot = (v,a,c,q=c?.nextSibling === a) => {
  if (typeof v !== 'object') {
    if (q && c.nodeType === 3) { c.data != v && (c.data = v); return c }
    v = new Text(v)
  }
  q ? c !== v && c.replaceWith(v) : a.parentNode?.insertBefore(v,a)
  return v
}
function static0() {
  const __ret = __template(`<div><!>`)
  {
    const _v0 = __ret.firstChild
    const d = document.createElement('div')
    let _v1
    ;(__ret[__sym_upd] = () => {
      _v1 = __slot(d, _v0, _v1)
    })();
  }
  return __ret
}
function static1() {
  const __ret = __template(`<div><!>`)
  {
    let _v0 = __ret.firstChild
    let c = 0
    let _v1
    ;(__ret[__sym_upd] = () => {
      _v1 = __slot(c++, _v0, _v1)
    })();
  }
  return __ret
}
function staticNoInit() {
  const __ret = __template(`<div><!>`)
  {
    let _v0 = __ret.firstChild
    let c, _v1
    ;(__ret[__sym_upd] = () => {
      c = 0;
      _v1 = __slot(c++, _v0, _v1)
    })();
  }
  return __ret
}
function staticCoalesce() {
  const __ret = __template(`<div><!><!>`)
  {
    let _v2 = 0x1
    let _v0 = __ret.firstChild
    let _v1 = _v0.nextSibling
    let c, c1, c2, _v3, _v4
    ;(__ret[__sym_upd] = () => {
      c = 0;
      if (_v2) {
        _v2 >>= 1;
        c1 = ++c;
        c2 = ++c;
      }
      _v3 = __slot(c1++, _v0, _v3)
      _v4 = __slot(c2++, _v1, _v4)
    })();
  }
  return __ret
}
function staticClass() {
  const __ret = __template(`<div> is_same_x_ctor: <!>`)
  {
    let _v0 = __ret.firstChild
    _v0 = _v0.nextSibling;
    class X {}
    let last_x, _v1
    ;(__ret[__sym_upd] = () => {
      const needs_update = !last_x
      const x = new X()
      const is_same_x_ctor = last_x?.constructor === x.constructor
      last_x = x;
      if (needs_update) d[Symbol.update]();
      _v1 = __slot(String(is_same_x_ctor), _v0, _v1)
    })();
  }
  return __ret
}
function staticFn() {
  const __ret = __template(`<div> is_same_fn: <!>`)
  {
    let _v0 = __ret.firstChild
    _v0 = _v0.nextSibling;
    function f() {}
    let last_f, _v1
    ;(__ret[__sym_upd] = () => {
      const needs_update = !last_f
      const is_same_fn = f === last_f
      last_f = f;
      if (needs_update) d[Symbol.update]();
      _v1 = __slot(String(is_same_fn), _v0, _v1)
    })();
  }
  return __ret
}
function staticLazyInit() {
  let q = 0
  const __ret = __template(`<div><div> c: <!></div> q: <!>`)
  {
    let _v2 = 0x1
    let _v0 = __ret.firstChild
    _v0 = _v0.nextSibling;
    _v0 = _v0.nextSibling;
    let _v1 = _v0.firstChild
    _v1 = _v1.nextSibling;
    let c, _v3, _v4
    ;(__ret[__sym_upd] = () => {
      const y = Math.floor(Math.random() * 0.5)
      if (_v2) {
        _v2 >>= 1;
        c = ++q + y;
      }
      _v3 = __slot(c++, _v1, _v3)
      if (c === 2) q += 1;
      _v4 = __slot(q, _v0, _v4)
    })();
  }
  return __ret
}
function staticFnHoist1() {
  const __ret = __template(`<div><!>`)
  {
    let _v0 = __ret.firstChild
    function f1(a, b) {
      return a + b
    }
    function transitive1() {
      return f1(1, 2)
    }
    let _v1
    ;(__ret[__sym_upd] = () => {
      _v1 = __slot(f1(1, 2) + transitive1(), _v0, _v1)
    })();
  }
  return __ret
}
function attrHoist() {
  const __ret = __template(`<div><div>click`)
  {
    let _v1 = __ret.firstChild
    let _v0
    _v1.addEventListener('click', () => console.log(_v0));
    ;(__ret[__sym_upd] = () => {
      const derived = Math.floor(Math.random() * 0.5)
      _v0 = derived;
    })();
  }
  return __ret
}
function attrHoist2() {
  const __ret = __template(`<div><div>click`)
  {
    let _v1 = __ret.firstChild
    let _v2
    ;(__ret[__sym_upd] = () => {
      let c = 0
      const derived = Math.floor(Math.random() * 0.5)
      if (_v2) _v1.removeEventListener('click', _v2)
      _v1.addEventListener('click', _v2 = () => console.log(derived, c));
    })();
  }
  return __ret
}
{
  let c2 = 0
  document.body.append((() => {
    const _v0 = __template(`<div><div><div>c: <!> ; c2: <!></div><button>+1, delayed</button></div><button>reset`)
    const root = _v0.firstChild
    let _v1 = root.nextSibling
    const view = root.firstChild
    _v1 = view.nextSibling;
    let _v2, _v3
    _v1 = view.firstChild;
    _v1 = _v1.nextSibling;
    let _v5 = _v1.nextSibling
    _v5 = _v5.nextSibling;
    let _v6, _v7
    view[__sym_upd] = () => {
      const _v4 = _v2
      _v6 = __slot(_v4(), _v1, _v6)
      _v7 = __slot(c2, _v5, _v7)
    };
    let _v8
    root[__sym_upd] = () => {
      let c = 0
      _v2 = () => c;
      _v3 = v => c = v;
      view[__sym_upd]();
      if (_v8) _v1.removeEventListener('click', _v8)
      _v1.addEventListener('click', _v8 = () => {
        setTimeout(() => {
          c += 1;
          c2 += 1;
          view[Symbol.update]();
        }, 1000);
      });
    };
    _v1.addEventListener('click', () => {
      root[Symbol.update]();
    });
    ;(_v0[__sym_upd] = root[__sym_upd].bind(root))();
    return _v0
  })());
}
function manyStatic() {
  const __ret = __template(`<div><!>`)
  {
    let _v2 = 0x7
    let _v0 = __ret.firstChild
    let _v1, _v3, _v4, _v5
    ;(__ret[__sym_upd] = () => {
      const m = Math.random()
      if (_v2) {
        _v2 >>= 1;
        _v1 = m + Math.random();
      }
      const m2 = _v1
      const m3 = Math.random()
      if (_v2) {
        _v2 >>= 1;
        _v3 = m3 + Math.random();
      }
      const m4 = _v3
      const m5 = Math.random()
      if (_v2) {
        _v2 >>= 1;
        _v4 = m5 + Math.random();
      }
      const m6 = _v4
      const m7 = m6 + Math.random()
      _v5 = __slot(m7, _v0, _v5)
    })();
  }
  return __ret
}
{
  function Foo(attrs = {}, children = []) {
    const __ret = __template(`<div><!><!>`)
    let _v0 = __ret.firstChild
    return {
      root: __ret,
      [__sym_upd]: () => {
        __slot_s(children, _v0, );
      }
    }
  }
  function Bar(attrs = {}, children = []) {
    let __ret, _v0
    const _v1 = []
    return __ret = {
      root: null,
      [__sym_upd]: () => {
        _v0 ??= Foo(void 0, _v1);
        _v0[Symbol.update]();
        __ret.root ??= _v0.root;
      }
    };
  }
}
