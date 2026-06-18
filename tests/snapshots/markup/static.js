// @filename: main.syn
var __template = ((c,p) => (s,b) => {
  c[0] ||= setTimeout(() => c={})
  return s in c ? (c[s][1] ||= p(s,b)).cloneNode(1) : (c[s] = [p(s,b)])[0]
})({}, (s,b,t=document.createElement('template')) => {
  t.innerHTML = s
  return b ? t.content : t.content.firstChild
})
var __sym_upd = Symbol.update ||= Symbol.for('update')
var __splice_at = (c,i,v,d,l=c.length) => (c.splice(i,d,...v), c.length-l+d)
var __slot_s = (a,b,v) => {
  let p, n = a.nextSibling
  for (p of v) n === p ? n=n.nextSibling : n.before(p)
  while (p = n, n = p.nextSibling, p !== b) p.remove()
}
var __slot = (a,c,v) => {
  const q = c?.nextSibling === a
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
    const _v0 = __ret.firstChild // <!> - {}
    const d = document.createElement('div')
    let _v1
    ;(__ret[__sym_upd] = () => {
      _v1 = __slot(_v0, _v1, d)
    })();
  }
  return __ret
}
function static1() {
  const __ret = __template(`<div><!>`)
  {
    let _v0 = __ret.firstChild // <!> - {}
    let c = 0
    let _v1
    ;(__ret[__sym_upd] = () => {
      _v1 = __slot(_v0, _v1, c++)
    })();
  }
  return __ret
}
function staticNoInit() {
  const __ret = __template(`<div><!>`)
  {
    let _v0 = __ret.firstChild // <!> - {}
    let c, _v1
    ;(__ret[__sym_upd] = () => {
      c = 0;
      _v1 = __slot(_v0, _v1, c++)
    })();
  }
  return __ret
}
function staticCoalesce() {
  const __ret = __template(`<div><!><!>`)
  {
    let _v2 = 0x1
    let _v0 = __ret.firstChild // <!> - {}
    let _v1 = _v0.nextSibling // <!> - {}
    let c, c1, c2, _v3, _v4
    ;(__ret[__sym_upd] = () => {
      c = 0;
      if (_v2) {
        _v2 >>= 1;
        c1 = ++c;
        c2 = ++c;
      }
      _v3 = __slot(_v0, _v3, c1++)
      _v4 = __slot(_v1, _v4, c2++)
    })();
  }
  return __ret
}
function staticClass() {
  const __ret = __template(`<div>is_same_x_ctor: <!>`)
  {
    let _v0 = __ret.firstChild // #text
    _v0 = _v0.nextSibling; // <!> - {}
    class X {}
    let last_x, _v1
    ;(__ret[__sym_upd] = () => {
      const needs_update = !last_x
      const x = new X()
      const is_same_x_ctor = last_x?.constructor === x.constructor
      last_x = x;
      if (needs_update) d[Symbol.update]();
      _v1 = __slot(_v0, _v1, String(is_same_x_ctor))
    })();
  }
  return __ret
}
function staticFn() {
  const __ret = __template(`<div>is_same_fn: <!>`)
  {
    let _v0 = __ret.firstChild // #text
    _v0 = _v0.nextSibling; // <!> - {}
    function f() {}
    let last_f, _v1
    ;(__ret[__sym_upd] = () => {
      const needs_update = !last_f
      const is_same_fn = f === last_f
      last_f = f;
      if (needs_update) d[Symbol.update]();
      _v1 = __slot(_v0, _v1, String(is_same_fn))
    })();
  }
  return __ret
}
function staticLazyInit() {
  let q = 0
  const __ret = __template(`<div><div>c: <!></div>
q: <!>`)
  {
    let _v3 = 0x1
    const _v0 = __ret.firstChild // div
    let _v1 = _v0.nextSibling // #text
    _v1 = _v1.nextSibling; // <!> - {}
    let _v2 = _v0.firstChild // #text
    _v2 = _v2.nextSibling; // <!> - {}
    let c, _v4, _v5
    ;(__ret[__sym_upd] = () => {
      const y = Math.floor(Math.random() * 0.5)
      if (_v3) {
        _v3 >>= 1;
        c = ++q + y;
      }
      _v4 = __slot(_v2, _v4, c++)
      if (c === 2) q += 1;
      _v5 = __slot(_v1, _v5, q)
    })();
  }
  return __ret
}
function staticFnHoist1() {
  const __ret = __template(`<div><!>`)
  {
    let _v0 = __ret.firstChild // <!> - {}
    function f1(a, b) {
      return a + b
    }
    function transitive1() {
      return f1(1, 2)
    }
    let _v1
    ;(__ret[__sym_upd] = () => {
      _v1 = __slot(_v0, _v1, f1(1, 2) + transitive1())
    })();
  }
  return __ret
}
function attrHoist() {
  const __ret = __template(`<div><div>click`)
  {
    let _v1 = __ret.firstChild // div
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
    let _v1 = __ret.firstChild // div
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
    let _v1 = root.nextSibling // button
    const view = root.firstChild
    _v1 = view.nextSibling; // button
    let _v2, _v3
    _v1 = view.firstChild; // #text
    _v1 = _v1.nextSibling; // <!> - {}
    let _v5 = _v1.nextSibling // #text
    _v5 = _v5.nextSibling; // <!> - {}
    let _v6, _v7
    view[__sym_upd] = () => {
      const _v4 = _v2
      _v6 = __slot(_v1, _v6, _v4())
      _v7 = __slot(_v5, _v7, c2)
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
    let _v0 = __ret.firstChild // <!> - {}
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
      _v5 = __slot(_v0, _v5, m7)
    })();
  }
  return __ret
}
{
  function Foo(attrs = {}, children = []) {
    const __ret = __template(`<div><!><!>`)
    let _v0 = __ret.firstChild // <!> - {...}
    let _v1 = _v0.nextSibling // {...} - <!>
    return {
      root: __ret,
      [__sym_upd]: () => {
        __slot_s(_v0, _v1, children);
      }
    }
  }
  function Bar(attrs = {}, children = []) {
    let __ret, _v0
    const _v1 = []
    const _v2 = __template(`<div><!>`)
    const _v3 = _v2 // div
    let _v4 = _v3.firstChild // <!> - {}
    let c, _v5
    return __ret = {
      root: null,
      [__sym_upd]: () => {
        _v0 ??= Foo(void 0, _v1);
        _v1[0] = children[0];
        c = 0;
        _v5 = __slot(_v4, _v5, c++)
        _v1[1] = _v3;
        _v0[Symbol.update]();
        __ret.root ??= _v0.root;
      }
    };
  }
}
{
  function Foo(attrs = {}, children = []) {
    let c = 0
    const __ret = __template(`<div><div>c is: <!></div><!><div><!><!>`)
    const d = __ret.firstChild
    let _v0 = d.nextSibling // Bar
    _v0 = _v0.nextSibling; // div
    let _v1 = d.firstChild // #text
    _v1 = _v1.nextSibling; // <!> - {}
    let _v2
    d[__sym_upd] = () => {
      _v2 = __slot(_v1, _v2, c)
    };
    let _v3
    let _v4 = _v0.firstChild // <!> - {...}
    let _v5 = _v4.nextSibling // {...} - <!>
    function Bar(attrs = {}) {
      const __ret = __template(`<div>click! <!>`)
      __ret.addEventListener('click', () => {
        c += attrs.inc;
        d[Symbol.update]();
      });
      let _v6 = __ret.firstChild // #text
      _v6 = _v6.nextSibling; // <!> - {}
      let _v7
      return {
        root: __ret,
        [__sym_upd]: () => {
          _v7 = __slot(_v6, _v7, attrs.inc)
        }
      }
    }
    return {
      root: __ret,
      [__sym_upd]: () => {
        d[__sym_upd]();
        _v3 ??= Bar({
          inc: 2
        });
        _v3[Symbol.update]();
        if (_v0) _v0 = void _v0.replaceWith(_v3.root);
        __slot_s(_v4, _v5, children);
      },
      Bar
    }
  }
  document.body.append((() => {
    let f
    const _v0 = []
    let _v1, _v3
    function _v2() {
      if (!f) {
        f = Foo(void 0, _v0);
        _v1 = f[Symbol.update];
        f[Symbol.update] = _v2;
      }
      _v0[0] = ' ';
      _v3 ??= f.Bar({
        inc: 3
      });
      _v3[Symbol.update]();
      _v0[1] = _v3.root;
      _v0[2] = ' ';
      _v1?.();
    }
    _v2();
    return f
  })().root);
}
{
  function Foo(attrs = {}, children = []) {
    let seconds = 0
    const v = __template(`<div><div>seconds: <!></div><div><!><!>`)
    const _v0 = v.firstChild // div
    const _v1 = _v0.nextSibling // div
    let t
    let _v2 = _v0.firstChild // #text
    _v2 = _v2.nextSibling; // <!> - {}
    let _v3
    let _v4 = _v1.firstChild // <!> - {...}
    let _v5 = _v4.nextSibling // {...} - <!>
    v[__sym_upd] = () => {
      if (!v.isConnected) {
        clearInterval(t);
        t = undefined;
      } else {
        t ??= setInterval(() => {
          seconds += 1;
          v[Symbol.update]();
        }, 1000);
      }
      _v3 = __slot(_v2, _v3, seconds)
      __slot_s(_v4, _v5, children);
    };
    return {
      root: v,
      [__sym_upd]: () => {
        v[__sym_upd]();
      }
    }
  }
  document.body.append((() => {
    const d = __template(`<div><button>toggle: <!></button><!>`)
    const _v0 = d.firstChild // button
    let _v1 = _v0.nextSibling // Foo
    let cond = false
    let count = 0
    _v0.addEventListener('click', () => {
      cond = !cond;
      d[Symbol.update]();
    });
    let _v2 = _v0.firstChild // #text
    _v2 = _v2.nextSibling; // <!> - {}
    let _v3, _v4
    const _v5 = []
    let _v6 = 0
    let _v7, _v32
    ;(d[__sym_upd] = () => {
      _v3 = __slot(_v2, _v3, String(cond))
      _v4 ??= Foo(void 0, _v5);
      const _v33 = !!cond
      if (_v33) {
        if (!_v7) {
          const _v8 = [_v7 = __template(`<div>test!`)]
          let _v9 = 0
          let _v10, _v11, timerFiredCount = 0, _v12, _v27, _v29
          _v7 = _v8;
          _v7[__sym_upd] = () => {
            count += 1;
            let cond2 = false
            _v10 = () => cond2;
            _v11 = v => cond2 = v;
            const _v30 = count % 2 === 0
            if (_v30) {
              if (!_v12) {
                const _v13 = [_v12 = __template(`<div><div>even</div><button>toggle "hello"</button><!><!>`)]
                const d2 = _v12
                let _v15 = d2.firstChild // div
                _v15 = _v15.nextSibling; // button
                const _v16 = _v15.nextSibling // <!> - #if
                let _v17 = _v16.nextSibling // #if - <!>
                _v15.addEventListener('click', () => {
                  const _v18 = _v10
                  const _v19 = _v11
                  _v19(!_v18());
                  d2[Symbol.update]();
                  setTimeout(() => {
                    _v19(false);
                    timerFiredCount += 1;
                    d2[Symbol.update]();
                  }, 10000);
                });
                let _v20, _v24
                d2[__sym_upd] = () => {
                  const _v14 = _v10
                  const _v25 = !!_v14()
                  if (_v25) {
                    if (!_v20) {
                      const _v21 = [_v20 = __template(`<div>hello! <!>`)]
                      let _v22 = _v20.firstChild // #text
                      _v22 = _v22.nextSibling; // <!> - {}
                      let _v23
                      _v20 = _v21;
                      _v20[__sym_upd] = () => {
                        _v23 = __slot(_v22, _v23, timerFiredCount)
                      };
                    }
                  }
                  const _v26 = _v25 ? _v20 : []
                  if (_v25 !== _v24) __slot_s(_v16, _v17, _v26);
                  _v24 = _v25;
                  _v26[Symbol.update]?.()
                };
                _v12 = _v13;
                _v12[__sym_upd] = () => {
                  d2[__sym_upd]();
                };
              }
            } else {
              if (!_v27) {
                _v27 = [__template(`<div>odd`)];
              }
            }
            const _v31 = _v30 ? _v12 : _v27
            _v31[Symbol.update]?.()
            if (_v30 !== _v29) _v9 = __splice_at(_v7, 1, _v31, _v9);
            _v29 = _v30;
          };
        }
      }
      const _v34 = _v33 ? _v7 : []
      _v34[Symbol.update]?.()
      if (_v33 !== _v32) _v6 = __splice_at(_v5, 0, _v34, _v6);
      _v32 = _v33;
      _v4[Symbol.update]();
      if (_v1) _v1 = void _v1.replaceWith(_v4.root);
    })();
    return d
  })());
}
