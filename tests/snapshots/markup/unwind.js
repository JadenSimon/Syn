// @filename: main.syn
var __template = ((p,c,u) => s => {
  u ||= setTimeout(() => (c = {}, u = 0))
  return s in c ? (c[s]._n ||= p(s)).cloneNode(true) : c[s] = p(s)
})((s,t = document.createElement('template')) => (t.innerHTML = s, t.content.firstChild), {})
var __sym_upd = Symbol.update ||= Symbol.for('update')
var __comp = root => ({ root, _p: {}, [__sym_upd]() {
  let t = this.root
  this._u?.()
  if (typeof t === 'function') {
    t = this.root = this.root(this._p)
    this._a && t.setAttribute(this._a,'')
    this._s?.() ?? this._b?.replaceWith(t)
  } else {
    t[__sym_upd]?.()
    this._s?.()
  }
}})
var __slot = (a,v,c) => {
  const q = c?.nextSibling === a
  if (typeof v !== 'object') {
    if (q && c.nodeType === 3) { c.nodeValue != v && (c.nodeValue = v); return c }
    v = new Text(v)
  }
  q ? c !== v && c.replaceWith(v) : a.parentNode?.insertBefore(v,a)
  return v
}
var __unw = (a,f) => { while (f = a.pop()) if (Array.isArray(f)) __unw(f); else f() }
function partial() {
  let t = 0
  let c1 = 0
  let c2 = 0
  const __ret = __template(`<div><div>t: <!></div><div>c1: <!></div><div> c2: <!></div><button>update d1</button><button>update d2`)
  {
    const _v0 = __ret.firstChild
    const _v1 = _v0.nextSibling
    const d2 = _v1.nextSibling
    const _v2 = d2.nextSibling
    const _v3 = _v2.nextSibling
    const d1 = __ret
    let _v5 = _v0.firstChild
    _v5 = _v5.nextSibling;
    let _v6
    let _v7 = _v1.firstChild
    _v7 = _v7.nextSibling;
    let _v8
    let _v9 = d2.firstChild
    _v9 = _v9.nextSibling;
    let _v11
    let _v10 = []
    d2[__sym_upd] = () => {
      __unw(_v10);
      _v10.push(() => {
        c2 += 1;
      });
      _v11 = __slot(_v9, c2, _v11);
    };
    _v2.addEventListener('click', () => {
      d1[Symbol.update]?.();
    });
    _v3.addEventListener('click', () => {
      d2[Symbol.update]?.();
    });
    let _v4 = []
    ;(__ret[__sym_upd] = () => {
      __unw(_v4);
      c1 += 1;
      _v4.push(() => {
        t += 1;
        c1 -= 1;
      });
      _v6 = __slot(_v5, t, _v6);
      _v8 = __slot(_v7, c1, _v8);
      d2[__sym_upd]();
      _v4.push(_v10);
    })();
  }
  return __ret
}
function unmount() {
  function Foo(props) {
    let c = 0
    let t = 0
    function unmount() {
      clearInterval(t);
      t = 0;
    }
    this.unmount = unmount;
    let __ret = __template(`<div><span>counter: <!>`)
    const s = __ret.firstChild
    let _v0 = s.firstChild
    _v0 = _v0.nextSibling;
    let _v1
    s[__sym_upd] = () => {
      _v1 = __slot(_v0, c, _v1);
    };
    ;(__ret[__sym_upd] = () => {
      s[__sym_upd]();
      t ||= setInterval(() => {
        {
          c += 1;
        }
        s[Symbol.update]?.();
      }, 1000);
    })();
    return __ret
  }
  let show = false
  const __ret = __template(`<div><!><button>toggle`)
  {
    const _v0 = __ret.firstChild
    const _v1 = _v0.nextSibling
    const d = __ret
    let _v2
    const f = __comp(Foo)
    f._b = _v2;
    _v0[__sym_upd] = () => {
      const _v4 = !!show
      if (_v4 != _v0._s) {
        if (_v4) {
          if (_v0._s !== 0) {
            _v2 = __template(`<!>`);
          }
          _v0.replaceWith(_v2);
          _v0._s = 1;
        } else {
          if (_v0._s === 1) {
            _v2.replaceWith(_v0);
            _v0._s = 0;
          }
          return 
        }
      }
    };
    _v1.addEventListener('click', () => {
      {
        show = !show;
      }
      d[Symbol.update]?.();
    });
    let _v3 = []
    ;(__ret[__sym_upd] = () => {
      __unw(_v3);
      _v0[__sym_upd]();
    })();
  }
  return __ret
}
