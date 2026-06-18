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
function basicIf(show) {
  const __ret = __template(`<div><!><!>`)
  {
    const _v0 = __ret.firstChild // <!> - #if
    let _v1 = _v0.nextSibling // #if - <!>
    let _v2, _v4
    ;(__ret[__sym_upd] = () => {
      const _v5 = !!show
      if (_v5) {
        if (!_v2) {
          _v2 = [__template(`<span>hello`)];
        }
      }
      const _v6 = _v5 ? _v2 : []
      if (_v5 !== _v4) __slot_s(_v0, _v1, _v6);
      _v4 = _v5;
      _v6[Symbol.update]?.()
    })();
  }
  return __ret
}
function ifElse(show) {
  const __ret = __template(`<div><!><!>`)
  {
    const _v0 = __ret.firstChild // <!> - #if
    let _v1 = _v0.nextSibling // #if - <!>
    let _v2, _v4, _v6
    ;(__ret[__sym_upd] = () => {
      const _v7 = !!show
      if (_v7) {
        if (!_v2) {
          _v2 = [__template(`<span>yes`)];
        }
      } else {
        if (!_v4) {
          _v4 = [__template(`<span>no`)];
        }
      }
      const _v8 = _v7 ? _v2 : _v4
      if (_v7 !== _v6) __slot_s(_v0, _v1, _v8);
      _v6 = _v7;
      _v8[Symbol.update]?.()
    })();
  }
  return __ret
}
function ifExpr(count, show) {
  const __ret = __template(`<div><!><!>`)
  {
    const _v0 = __ret.firstChild // <!> - #if
    let _v1 = _v0.nextSibling // #if - <!>
    let _v2, _v5
    ;(__ret[__sym_upd] = () => {
      const _v6 = !!show
      if (_v6) {
        if (!_v2) {
          const _v3 = [_v2 = new Comment()]
          let _v4
          _v2 = _v3;
          _v2[__sym_upd] = () => {
            _v4 = __slot(_v2, _v4, count)
          };
        }
      }
      const _v7 = _v6 ? _v2 : []
      if (_v6 !== _v5) __slot_s(_v0, _v1, _v7);
      _v5 = _v6;
      _v7[Symbol.update]?.()
    })();
  }
  return __ret
}
function ifElseExpr(val, show) {
  const __ret = __template(`<div><!><!>`)
  {
    const _v0 = __ret.firstChild // <!> - #if
    let _v1 = _v0.nextSibling // #if - <!>
    let _v2, _v5, _v8
    ;(__ret[__sym_upd] = () => {
      const _v9 = !!show
      if (_v9) {
        if (!_v2) {
          const _v3 = [_v2 = new Comment()]
          let _v4
          _v2 = _v3;
          _v2[__sym_upd] = () => {
            _v4 = __slot(_v2, _v4, val)
          };
        }
      } else {
        if (!_v5) {
          const _v6 = [_v5 = new Comment()]
          let _v7
          _v5 = _v6;
          _v5[__sym_upd] = () => {
            _v7 = __slot(_v5, _v7, "hidden")
          };
        }
      }
      const _v10 = _v9 ? _v2 : _v5
      if (_v9 !== _v8) __slot_s(_v0, _v1, _v10);
      _v8 = _v9;
      _v10[Symbol.update]?.()
    })();
  }
  return __ret
}
function siblingIfs(a, b) {
  const __ret = __template(`<div><!><!><!><!>`)
  {
    const _v0 = __ret.firstChild // <!> - #if
    let _v1 = _v0.nextSibling // #if - <!>
    const _v2 = _v1.nextSibling // <!> - #if
    let _v3 = _v2.nextSibling // #if - <!>
    let _v4, _v6, _v9, _v11
    ;(__ret[__sym_upd] = () => {
      const _v7 = !!a
      if (_v7) {
        if (!_v4) {
          _v4 = [__template(`<span>a`)];
        }
      }
      const _v8 = _v7 ? _v4 : []
      if (_v7 !== _v6) __slot_s(_v0, _v1, _v8);
      _v6 = _v7;
      _v8[Symbol.update]?.()
      const _v12 = !!b
      if (_v12) {
        if (!_v9) {
          _v9 = [__template(`<span>b`)];
        }
      }
      const _v13 = _v12 ? _v9 : []
      if (_v12 !== _v11) __slot_s(_v2, _v3, _v13);
      _v11 = _v12;
      _v13[Symbol.update]?.()
    })();
  }
  return __ret
}
function ifWithBinding(show) {
  const __ret = __template(`<div><!><!>`)
  {
    const _v0 = __ret.firstChild // <!> - #if
    let _v1 = _v0.nextSibling // #if - <!>
    let _v2, _v4
    ;(__ret[__sym_upd] = () => {
      const _v5 = !!show
      if (_v5) {
        if (!_v2) {
          const _v3 = [_v2 = __template(`<span>hello`)]
          const el = _v2
          _v2 = _v3;
        }
      }
      const _v6 = _v5 ? _v2 : []
      if (_v5 !== _v4) __slot_s(_v0, _v1, _v6);
      _v4 = _v5;
      _v6[Symbol.update]?.()
    })();
  }
  return __ret
}
function singleSlotNoEffects(show) {
  const __ret = __template(`<div><!><!>`)
  {
    const _v0 = __ret.firstChild // <!> - #if
    let _v1 = _v0.nextSibling // #if - <!>
    let _v2, _v4, _v6
    ;(__ret[__sym_upd] = () => {
      const _v7 = !!show
      if (_v7) {
        if (!_v2) {
          _v2 = [new Text(`1`)];
        }
      } else {
        if (!_v4) {
          _v4 = [new Text(`2`)];
        }
      }
      const _v8 = _v7 ? _v2 : _v4
      if (_v7 !== _v6) __slot_s(_v0, _v1, _v8);
      _v6 = _v7;
      _v8[Symbol.update]?.()
    })();
  }
  return __ret
}
function branchWithBinding(show) {
  const __ret = __template(`<div><!><!>`)
  {
    const _v0 = __ret.firstChild // <!> - #if
    let _v1 = _v0.nextSibling // #if - <!>
    let _v2, _v4, _v6
    ;(__ret[__sym_upd] = () => {
      const _v7 = !!show
      if (_v7) {
        if (!_v2) {
          const _v3 = [_v2 = __template(`<div>`)]
          const d = _v2
          d[__sym_upd] = () => {
            console.log(d);
          };
          _v2 = _v3;
          _v2[__sym_upd] = () => {
            d[__sym_upd]();
          };
        }
      } else {
        if (!_v4) {
          _v4 = [new Text(`0`)];
        }
      }
      const _v8 = _v7 ? _v2 : _v4
      if (_v7 !== _v6) __slot_s(_v0, _v1, _v8);
      _v6 = _v7;
      _v8[Symbol.update]?.()
    })();
  }
  return __ret
}
function conditionalComponentInstanceChildren(cond) {
  function Inner(_attrs, children = []) {
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
  const __ret = __template(`<div><!>`)
  {
    let _v0 = __ret.firstChild // Inner
    let _v1
    const _v2 = []
    let _v3 = 0
    let _v4, _v6
    ;(__ret[__sym_upd] = () => {
      _v1 ??= Inner(void 0, _v2);
      const _v7 = !!cond
      if (_v7) {
        if (!_v4) {
          _v4 = [`test`];
        }
      }
      const _v8 = _v7 ? _v4 : []
      _v8[Symbol.update]?.()
      if (_v7 !== _v6) _v3 = __splice_at(_v2, 0, _v8, _v3);
      _v6 = _v7;
      _v1[Symbol.update]();
      if (_v0) _v0 = void _v0.replaceWith(_v1.root);
    })();
  }
  return __ret
}
