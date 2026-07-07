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
    let _v2
    if (show) {
      _v0.before(_v2 = __template(`<span>hello`));
    }
  }
  return __ret
}
function ifElse(show) {
  const __ret = __template(`<div><!><!>`)
  {
    const _v0 = __ret.firstChild // <!> - #if
    let _v1 = _v0.nextSibling // #if - <!>
    let _v2, _v4
    if (show) {
      _v0.before(_v2 = __template(`<span>yes`));
    } else {
      _v0.before(_v4 = __template(`<span>no`));
    }
  }
  return __ret
}
function ifExpr(count, show) {
  const __ret = __template(`<div><!><!>`)
  {
    const _v0 = __ret.firstChild // <!> - #if
    let _v1 = _v0.nextSibling // #if - <!>
    let _v2
    if (show) {
      _v0.before(_v2 = new Comment());
      _v2.before(count);
    }
  }
  return __ret
}
function ifElseExpr(val, show) {
  const __ret = __template(`<div><!><!>`)
  {
    const _v0 = __ret.firstChild // <!> - #if
    let _v1 = _v0.nextSibling // #if - <!>
    let _v2, _v4
    if (show) {
      _v0.before(_v2 = new Comment());
      _v2.before(val);
    } else {
      _v0.before(_v4 = new Comment());
      _v4.before("hidden");
    }
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
    let _v4, _v6
    if (a) {
      _v0.before(_v4 = __template(`<span>a`));
    }
    if (b) {
      _v2.before(_v6 = __template(`<span>b`));
    }
  }
  return __ret
}
function ifWithBinding(show) {
  const __ret = __template(`<div><!><!>`)
  {
    const _v0 = __ret.firstChild // <!> - #if
    let _v1 = _v0.nextSibling // #if - <!>
    let _v2
    if (show) {
      _v0.before(_v2 = __template(`<span>hello`));
      const el = _v2
    }
  }
  return __ret
}
function singleSlotNoEffects(show) {
  const __ret = __template(`<div><!><!>`)
  {
    const _v0 = __ret.firstChild // <!> - #if
    let _v1 = _v0.nextSibling // #if - <!>
    let _v2, _v4
    if (show) {
      _v0.before(_v2 = new Text(`1`));
    } else {
      _v0.before(_v4 = new Text(`2`));
    }
  }
  return __ret
}
function branchWithBinding(show) {
  const __ret = __template(`<div><!><!>`)
  {
    const _v0 = __ret.firstChild // <!> - #if
    let _v1 = _v0.nextSibling // #if - <!>
    let _v2, _v4
    if (show) {
      _v0.before(_v2 = __template(`<div>`));
      const d = _v2
      d[__sym_upd] = () => {
        console.log(d);
      };
      d[__sym_upd]();
    } else {
      _v0.before(_v4 = new Text(`0`));
    }
  }
  return __ret
}
function ifWithSlotAndAttr(val, show) {
  const __ret = __template(`<div><!><!>`)
  {
    const _v0 = __ret.firstChild // <!> - #if
    let _v1 = _v0.nextSibling // #if - <!>
    let _v2, _v8
    ;(__ret[__sym_upd] = () => {
      const _v7 = !!show
      if (_v7) {
        if (!_v2) {
          const _v4 = __template(`<a><!>`)
          _v2 = [_v4];
          let _v5 = _v4.firstChild // <!> - {}
          let _v6
          _v2[__sym_upd] = () => {
            _v4.href = val;
            _v6 = __slot(_v5, _v6, val)
          };
        }
      }
      if (_v7 !== _v8) __swap_tree(_v0, _v1, _v7 ? _v2 : [], _v7 ? [] : _v2);
      _v8 = _v7;
      if (_v7) _v2[Symbol.update]();
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
    let _v3
    _v1 = Inner(void 0, _v2);
    if (cond) {
      _v2.push(_v3 = new Text(`test`));
    }
    _v1[Symbol.update]();
    _v0.replaceWith(_v1.root);
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
    let _v3, _v5
    _v1 = Inner(void 0, _v2);
    if (cond) {
      _v2.push(_v3 = new Text(`test`));
    } else {
      _v2.push(_v5 = new Text(`test2`));
    }
    _v1[Symbol.update]();
    _v0.replaceWith(_v1.root);
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
    let _v3, _v5
    _v1 = Inner(void 0, _v2);
    if (cond) {
      _v2.push(_v3 = new Text(`test
`));
      _v2[2] = new Text(' '); // #text
      _v2[1] = x;
      _v2[3] = x;
    } else {
      _v2.push(_v5 = new Text(`test2
`));
      _v2[2] = new Text(' '); // #text
      _v2[1] = x.repeat(2);
      _v2[3] = x.repeat(3);
    }
    _v1[Symbol.update]();
    _v0.replaceWith(_v1.root);
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
    let _v3, _v5
    _v1 = Inner(void 0, _v2);
    if (cond) {
      _v2.push(_v3 = new Text(`test`));
    } else {
      _v2.push(_v5 = new Text(`test2
`));
      _v2[2] = new Text(' '); // #text
      _v2[1] = x.repeat(2);
      _v2[3] = x.repeat(3);
    }
    _v1[Symbol.update]();
    _v0.replaceWith(_v1.root);
  }
  return __ret
}
function branchRetention() {
  let cond = true
  const o = __template(`<div><!><!>`)
  {
    const _v0 = o.firstChild // <!> - #if
    let _v1 = _v0.nextSibling // #if - <!>
    let _v2, _v7
    ;(o[__sym_upd] = () => {
      const _v6 = !!cond
      if (_v6) {
        if (!_v2) {
          let _v5 = 0x1
          const _v4 = __template(`<div>d`)
          _v2 = [_v4];
          const d = _v4
          _v2[__sym_upd] = () => {
            if (_v5) {
              _v5 >>= 1;
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
      if (_v6 !== _v7) __swap_tree(_v0, _v1, _v6 ? _v2 : [], _v6 ? [] : _v2);
      _v7 = _v6;
      if (_v6) _v2[Symbol.update]();
    })();
  }
  return o
}
function branchRetention2() {
  function Foo(_attrs, children = []) {
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
  let cond = true
  document.body.append((() => {
    let o
    const _v0 = []
    let _v1, _v3 = 0, _v4, _v10
    function _v2() {
      if (!o) {
        o = Foo(void 0, _v0);
        _v1 = o[Symbol.update];
        o[Symbol.update] = _v2;
      }
      const _v9 = !!cond
      if (_v9) {
        if (!_v4) {
          let _v8 = 0x1
          const _v6 = __template(`<div>d`)
          _v4 = [_v6];
          const _v7 = __template(`<div>d`)
          const d = _v7
          _v4[__sym_upd] = () => {
            if (_v8) {
              _v8 >>= 1;
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
      if (_v9) _v4[Symbol.update]();
      if (_v10 === false && _v9) _v0.splice(0, 0, _v4[0]);
      _v10 = _v9;
      _v1?.();
    }
    _v2();
    return o
  })().root);
}
function derivedStateAcrossBranch() {
  const __ret = __template(`<div><!><!>`)
  {
    const _v0 = __ret.firstChild // <!> - #if
    let _v1 = _v0.nextSibling // #if - <!>
    let _v2, _v3, _v9
    ;(__ret[__sym_upd] = () => {
      const d = (() => 1)()
      _v2 = d;
      const _v8 = !!d
      if (_v8) {
        if (!_v3) {
          const _v5 = __template(`<div><!>`)
          _v3 = [_v5];
          let _v6 = _v5.firstChild // <!> - {}
          let _v7
          _v3[__sym_upd] = () => {
            _v7 = __slot(_v6, _v7, _v2)
          };
        }
      }
      if (_v8 !== _v9) __swap_tree(_v0, _v1, _v8 ? _v3 : [], _v8 ? [] : _v3);
      _v9 = _v8;
      if (_v8) _v3[Symbol.update]();
    })();
  }
  return __ret
}
