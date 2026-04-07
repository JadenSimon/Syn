// @filename: main.syn
var __template = ((p,c,u) => s => {
  u ||= setTimeout(() => (c = {}, u = 0))
  return s in c ? (c[s].n ||= p(s)).cloneNode(true) : c[s] = p(s)
})((s,t = document.createElement('template')) => (t.innerHTML = s, t.content.firstChild), {})
var __sym_upd = Symbol.update ||= Symbol.for('update')
var __comp = root => ({
  root, _p: {}, [__sym_upd]() {
    let t = this.root
    this._u?.()
    if (typeof t === 'function') {
      t = this.root = t(this._p)
      this._s?.() ?? this._b?.replaceWith(t)
    } else {
      t[__sym_upd]?.()
      this._s?.()
    }
  }
})
var __pushAt = (c,i,v,l = c.length) => (c.splice(i,0,...v), c.length-l)
var __setSlotSpread = (a,v,b) => {
  b || a.after(b = a.cloneNode())
  let p, n = a.nextSibling
  while (p = n, n = p.nextSibling, p !== b) p.remove()
  a.after(...v)
  return b
}
var __setSlot = (a,v,c) => {
  const q = c?.nextSibling === a
  if (typeof v !== 'object') {
    if (q && c.nodeType === 3) { c.nodeValue = v; return c }
    v = new Text(v)
  }
  q ? c !== v && c.replaceWith(v) : a.parentNode?.insertBefore(v,a)
  return v
}
function basicIf(show) {
  const __ret = __template(`<div><!></div>`)
  {
    const _v0 = __ret.firstChild
    let _v1
    _v0[__sym_upd] = () => {
      const _v2 = !!show
      if (_v2 != _v0._s) {
        if (_v2) {
          if (_v0._s !== 0) {
            _v1 = __template(`<span>hello</span>`);
          }
          _v0.replaceWith(_v1);
          _v0._s = 1;
        } else {
          if (_v0._s === 1) {
            _v1.replaceWith(_v0);
            _v0._s = 0;
          }
          return 
        }
      }
    };
    ;(__ret[__sym_upd] = _v0[__sym_upd].bind(_v0))();
  }
  return __ret
}
function ifElse(show) {
  const __ret = __template(`<div><!></div>`)
  {
    const _v0 = __ret.firstChild
    let _v4
    let _v5
    let _v6
    ;(__ret[__sym_upd] = () => {
      let _v1
      if (show) {
        _v1 = _v4 ??= __template(`<span>yes</span>`);
      } else {
        _v1 = _v5 ??= __template(`<span>no</span>`);
      }
      _v1 != null ? _v6 = __setSlot(_v0, _v1, _v6) : _v6?.remove();
    })();
  }
  return __ret
}
function ifExpr(count, show) {
  const __ret = __template(`<div><!></div>`)
  {
    const _v0 = __ret.firstChild
    let _v1
    ;(__ret[__sym_upd] = () => {
      show ? _v1 = __setSlot(_v0, count, _v1) : _v1?.remove();
    })();
  }
  return __ret
}
function ifElseExpr(val, show) {
  const __ret = __template(`<div><!></div>`)
  {
    const _v0 = __ret.firstChild
    let _v4
    ;(__ret[__sym_upd] = () => {
      let _v1
      if (show) {
        _v1 = val;
      } else {
        _v1 = "hidden";
      }
      _v4 = __setSlot(_v0, _v1, _v4);
    })();
  }
  return __ret
}
function siblingIfs(a, b) {
  const __ret = __template(`<div><!><!></div>`)
  {
    const _v0 = __ret.firstChild
    const _v1 = _v0.nextSibling
    let _v2
    _v0[__sym_upd] = () => {
      const _v3 = !!a
      if (_v3 != _v0._s) {
        if (_v3) {
          if (_v0._s !== 0) {
            _v2 = __template(`<span>a</span>`);
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
    let _v4
    _v1[__sym_upd] = () => {
      const _v5 = !!b
      if (_v5 != _v1._s) {
        if (_v5) {
          if (_v1._s !== 0) {
            _v4 = __template(`<span>b</span>`);
          }
          _v1.replaceWith(_v4);
          _v1._s = 1;
        } else {
          if (_v1._s === 1) {
            _v4.replaceWith(_v1);
            _v1._s = 0;
          }
          return 
        }
      }
    };
    ;(__ret[__sym_upd] = () => {
      _v0[__sym_upd]();
      _v1[__sym_upd]();
    })();
  }
  return __ret
}
function ifWithBinding(show) {
  const __ret = __template(`<div><!></div>`)
  {
    const _v0 = __ret.firstChild
    let el
    _v0[__sym_upd] = () => {
      const _v1 = !!show
      if (_v1 != _v0._s) {
        if (_v1) {
          if (_v0._s !== 0) {
            el = __template(`<span>hello</span>`);
          }
          _v0.replaceWith(el);
          _v0._s = 1;
        } else {
          if (_v0._s === 1) {
            el.replaceWith(_v0);
            _v0._s = 0;
          }
          return 
        }
      }
    };
    ;(__ret[__sym_upd] = _v0[__sym_upd].bind(_v0))();
  }
  return __ret
}
function ifWithUnwind(show) {
  const __ret = __template(`<div><!></div>`)
  {
    const _v0 = __ret.firstChild
    let _v3
    let _v4
    let _v7
    let _v1 = []
    ;(__ret[__sym_upd] = () => {
      if (_v1.length) {
        let f
        while (f = _v1.pop()) f();
      }
      _v1.push(() => {
        console.log('2');
      });
      const _v2 = !!show
      if (_v2) {
        if (_v3) _v3[__sym_upd]();
         else _v3 = (() => {
          let _v5
          _v5 = [];
          _v5[0] = 'test ';
          ;(_v5[__sym_upd] = () => {
            _v1.push(() => {
              console.log('1');
            });
          })();
          return _v5
        })();
      } else {
        _v4 ??= (() => {
          let _v6
          _v6 = [];
          _v6[0] = 't';
          return _v6
        })();
      }
      _v7 = __setSlotSpread(_v0, _v2 ? _v3 : _v4, _v7);
    })();
  }
  return __ret
}
function singleSlotNoEffects(show) {
  const __ret = __template(`<div><!></div>`)
  {
    const _v0 = __ret.firstChild
    let _v4
    ;(__ret[__sym_upd] = () => {
      let _v1
      if (show) {
        _v1 = '1';
      } else {
        _v1 = '2';
      }
      _v4 = __setSlot(_v0, _v1, _v4);
    })();
  }
  return __ret
}
function branchWithBinding(show) {
  const __ret = __template(`<div><!></div>`)
  {
    const _v0 = __ret.firstChild
    let _v2
    let _v3
    let _v6
    ;(__ret[__sym_upd] = () => {
      const _v1 = !!show
      if (_v1) {
        if (_v2) _v2[__sym_upd]();
         else _v2 = (() => {
          let _v4 = __template(`<div></div>`)
          const d = _v4
          _v4 = [];
          d[__sym_upd] = () => {
            console.log(d);
          };
          _v4[0] = d;
          ;(_v4[__sym_upd] = d[__sym_upd].bind(d))();
          return _v4
        })();
      } else {
        _v3 ??= (() => {
          let _v5
          _v5 = [];
          _v5[0] = '0';
          return _v5
        })();
      }
      _v6 = __setSlotSpread(_v0, _v1 ? _v2 : _v3, _v6);
    })();
  }
  return __ret
}
function conditionalComponentInstanceChildren(cond) {
  function Inner(props) {
    let __ret = document.createElement('div')
    ;(__ret[__sym_upd] = () => {
      __ret.replaceChildren(...props.children);
    })();
    return __ret
  }
  const __ret = __template(`<div><!></div>`)
  {
    let _v0 = __ret.firstChild
    const _v1 = __comp(Inner)
    _v1._b = _v0;
    const _v2 = _v1._p.children = []
    let _v3 = 0
    let _v4 = 0
    const _v5 = []
    _v1._u = () => {
      if (_v3) {
        _v2.splice(_v3 -= _v4, _v4);
      }
      const _v6 = !!cond
      if (_v6) {
        if (!_v5._s) {
          _v5[0] = 'test';
          _v5._s = 1;
        }
      }
      _v3 += (_v4 = __pushAt(_v2, _v3, _v6 ? _v5 : []));
    };
    ;(__ret[__sym_upd] = _v1[__sym_upd].bind(_v1))();
  }
  return __ret
}
