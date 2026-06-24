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
var __swap_tree = (a,b,v,u=[]) => {
  let p, n = a.nextSibling
  while (p = n, n = p.nextSibling, p !== b) (u[u.length] = p).remove()
  a.after(...v)
  v.length = 0
}
function basicIf(show) {
  const __ret = __template(`<div><!><!>`)
  {
    const _v0 = __ret.firstChild // <!> - #if
    let _v1 = _v0.nextSibling // #if - <!>
    let _v2, _v5
    ;(__ret[__sym_upd] = () => {
      const _v4 = !!show
      if (_v4) {
        _v2 ??= [__template(`<span>hello`)];
      }
      if (_v4 !== _v5) __swap_tree(_v0, _v1, _v4 ? _v2 : [], _v4 ? [] : _v2);
      _v5 = _v4;
    })();
  }
  return __ret
}
function ifElse(show) {
  const __ret = __template(`<div><!><!>`)
  {
    const _v0 = __ret.firstChild // <!> - #if
    let _v1 = _v0.nextSibling // #if - <!>
    let _v2, _v4, _v7
    ;(__ret[__sym_upd] = () => {
      const _v6 = !!show
      if (_v6) {
        _v2 ??= [__template(`<span>yes`)];
      } else {
        _v4 ??= [__template(`<span>no`)];
      }
      if (_v6 !== _v7) __swap_tree(_v0, _v1, _v6 ? _v2 : _v4, _v6 ? _v4 : _v2);
      _v7 = _v6;
    })();
  }
  return __ret
}
function ifExpr(count, show) {
  const __ret = __template(`<div><!><!>`)
  {
    const _v0 = __ret.firstChild // <!> - #if
    let _v1 = _v0.nextSibling // #if - <!>
    let _v2, _v6
    ;(__ret[__sym_upd] = () => {
      const _v5 = !!show
      if (_v5) {
        if (!_v2) {
          const _v3 = [_v2 = new Comment()]
          let _v4
          _v2 = _v3;
          _v2[__sym_upd] = () => {
            _v4 = __slot(_v2, _v4, count)
          };
        }
      }
      if (_v5 !== _v6) __swap_tree(_v0, _v1, _v5 ? _v2 : [], _v5 ? [] : _v2);
      _v6 = _v5;
      if (_v5) _v2[Symbol.update]();
    })();
  }
  return __ret
}
function ifElseExpr(val, show) {
  const __ret = __template(`<div><!><!>`)
  {
    const _v0 = __ret.firstChild // <!> - #if
    let _v1 = _v0.nextSibling // #if - <!>
    let _v2, _v5, _v10
    ;(__ret[__sym_upd] = () => {
      const _v8 = !!show
      if (_v8) {
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
      const _v9 = _v8 ? _v2 : _v5
      if (_v8 !== _v10) __swap_tree(_v0, _v1, _v9, _v8 ? _v5 : _v2);
      _v10 = _v8;
      _v9[Symbol.update]();
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
    let _v4, _v7, _v8, _v11
    ;(__ret[__sym_upd] = () => {
      const _v6 = !!a
      if (_v6) {
        _v4 ??= [__template(`<span>a`)];
      }
      if (_v6 !== _v7) __swap_tree(_v0, _v1, _v6 ? _v4 : [], _v6 ? [] : _v4);
      _v7 = _v6;
      const _v10 = !!b
      if (_v10) {
        _v8 ??= [__template(`<span>b`)];
      }
      if (_v10 !== _v11) __swap_tree(_v2, _v3, _v10 ? _v8 : [], _v10 ? [] : _v8);
      _v11 = _v10;
    })();
  }
  return __ret
}
function ifWithBinding(show) {
  const __ret = __template(`<div><!><!>`)
  {
    const _v0 = __ret.firstChild // <!> - #if
    let _v1 = _v0.nextSibling // #if - <!>
    let _v2, _v5
    ;(__ret[__sym_upd] = () => {
      const _v4 = !!show
      if (_v4) {
        if (!_v2) {
          const _v3 = [_v2 = __template(`<span>hello`)]
          const el = _v2
          _v2 = _v3;
        }
      }
      if (_v4 !== _v5) __swap_tree(_v0, _v1, _v4 ? _v2 : [], _v4 ? [] : _v2);
      _v5 = _v4;
    })();
  }
  return __ret
}
function singleSlotNoEffects(show) {
  const __ret = __template(`<div><!><!>`)
  {
    const _v0 = __ret.firstChild // <!> - #if
    let _v1 = _v0.nextSibling // #if - <!>
    let _v2, _v4, _v7
    ;(__ret[__sym_upd] = () => {
      const _v6 = !!show
      if (_v6) {
        _v2 ??= [new Text(`1`)];
      } else {
        _v4 ??= [new Text(`2`)];
      }
      if (_v6 !== _v7) __swap_tree(_v0, _v1, _v6 ? _v2 : _v4, _v6 ? _v4 : _v2);
      _v7 = _v6;
    })();
  }
  return __ret
}
function branchWithBinding(show) {
  const __ret = __template(`<div><!><!>`)
  {
    const _v0 = __ret.firstChild // <!> - #if
    let _v1 = _v0.nextSibling // #if - <!>
    let _v2, _v4, _v7
    ;(__ret[__sym_upd] = () => {
      const _v6 = !!show
      if (_v6) {
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
        _v4 ??= [new Text(`0`)];
      }
      if (_v6 !== _v7) __swap_tree(_v0, _v1, _v6 ? _v2 : _v4, _v6 ? _v4 : _v2);
      _v7 = _v6;
      if (_v6) _v2[Symbol.update]();
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
    let _v4, _v7
    ;(__ret[__sym_upd] = () => {
      _v1 ??= Inner(void 0, _v2);
      const _v6 = !!cond
      if (_v6) {
        _v4 ??= [new Text(`test`)];
      }
      if (_v7 === false && _v6) _v2.splice(0, 0, _v4[0]);
      _v7 = _v6;
      _v1[Symbol.update]();
      if (_v0) _v0 = void _v0.replaceWith(_v1.root);
    })();
  }
  return __ret
}
function conditionalComponentInstanceChildren2(cond) {
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
      const _v8 = !!cond
      if (_v8) {
        _v4 ??= [new Text(`test`)];
      } else {
        _v6 ??= [new Text(`test2`)];
      }
      _v2[0] = (_v8 ? _v4 : _v6)[0];
      _v1[Symbol.update]();
      if (_v0) _v0 = void _v0.replaceWith(_v1.root);
    })();
  }
  return __ret
}
function conditionalComponentInstanceChildren3(cond) {
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
  let x = 'aaa'
  const __ret = __template(`<div><!>`)
  {
    let _v0 = __ret.firstChild // Inner
    let _v1
    const _v2 = []
    let _v3 = 0
    let _v4, _v6
    ;(__ret[__sym_upd] = () => {
      _v1 ??= Inner(void 0, _v2);
      const _v8 = !!cond
      if (_v8) {
        if (!_v4) {
          const _v5 = [_v4 = new Text(`test
`)]
          _v5[2] = new Text(' '); // #text
          _v4 = _v5;
          _v4[__sym_upd] = () => {
            _v5[1] = x;
            _v5[3] = x;
          };
        }
      } else {
        if (!_v6) {
          const _v7 = [_v6 = new Text(`test2
`)]
          _v7[2] = new Text(' '); // #text
          _v6 = _v7;
          _v6[__sym_upd] = () => {
            _v7[1] = x.repeat(2);
            _v7[3] = x.repeat(3);
          };
        }
      }
      const _v9 = _v8 ? _v4 : _v6
      _v9[Symbol.update]();
      let _v10 = -1
      while (++_v10 < 4) _v2[_v10] = _v9[_v10];
      _v1[Symbol.update]();
      if (_v0) _v0 = void _v0.replaceWith(_v1.root);
    })();
  }
  return __ret
}
function conditionalComponentInstanceChildren4(cond) {
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
  let x = 'aaa'
  const __ret = __template(`<div><!>`)
  {
    let _v0 = __ret.firstChild // Inner
    let _v1
    const _v2 = []
    let _v3 = 0
    let _v4, _v6, _v10
    ;(__ret[__sym_upd] = () => {
      _v1 ??= Inner(void 0, _v2);
      const _v8 = !!cond
      if (_v8) {
        _v4 ??= [new Text(`test`)];
      } else {
        if (!_v6) {
          const _v7 = [_v6 = new Text(`test2
`)]
          _v7[2] = new Text(' '); // #text
          _v6 = _v7;
          _v6[__sym_upd] = () => {
            _v7[1] = x.repeat(2);
            _v7[3] = x.repeat(3);
          };
        }
      }
      if (!_v8) _v6[Symbol.update]();
      const _v9 = _v8 ? _v4 : _v6
      _v2[0] = _v9[0];
      if (_v10 === true && !_v8) _v2.splice(1, 0, _v6[1], _v6[2], _v6[3]);
      if (_v10 === false && _v8) _v2.splice(1, 3);
      _v10 = _v8;
      _v1[Symbol.update]();
      if (_v0) _v0 = void _v0.replaceWith(_v1.root);
    })();
  }
  return __ret
}
function branchRetention() {
  let cond = true
  const o = __template(`<div><!><!>`)
  {
    const _v0 = o.firstChild // <!> - #if
    let _v1 = _v0.nextSibling // #if - <!>
    let _v2, _v6
    ;(o[__sym_upd] = () => {
      const _v5 = !!cond
      if (_v5) {
        if (!_v2) {
          let _v4 = 0x1
          const _v3 = [_v2 = __template(`<div>d`)]
          const d = _v2
          _v2 = _v3;
          _v2[__sym_upd] = () => {
            if (_v4) {
              _v4 >>= 1;
              d.after('test!');
              cond = !cond;
              o[Symbol.update]();
              setTimeout(() => {
                cond = !cond;
                o[Symbol.update]();
              }, 2000);
            }
          };
        }
      }
      if (_v5 !== _v6) __swap_tree(_v0, _v1, _v5 ? _v2 : [], _v5 ? [] : _v2);
      _v6 = _v5;
      if (_v5) _v2[Symbol.update]();
    })();
  }
  return o
}
