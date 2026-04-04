// @filename: main.syn
var __template = ((p,c,u) => s => {
  u ||= setTimeout(() => (c = {}, u = 0))
  return s in c ? (c[s].n ||= p(s)).cloneNode(true) : c[s] = p(s)
})((s,t = document.createElement('template')) => (t.innerHTML = s, t.content.firstChild), {})
var __sym_upd = Symbol.update ||= Symbol.for('update')
var __setSlotSpread = (a,v,b) => {
  b || a.after(b = a.cloneNode())
  let p, n = a.nextSibling
  while (p = n, n = p.nextSibling, p !== b) p.remove()
  a.after(...v)
  return b
}
function basicIf(show) {
  const __ret = __template(`<div><!></div>`)
  {
    const _v0 = __ret.firstChild
    let _v1
    _v0[__sym_upd] = () => {
      const _v2 = !!(show)
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
    ;(__ret[__sym_upd] = () => {
      _v0[__sym_upd]();
    })();
  }
  return __ret
}
function ifElse(show) {
  const __ret = __template(`<div><!></div>`)
  {
    const _v0 = __ret.firstChild
    let _v1
    let _v2
    let _v4
    _v0[__sym_upd] = () => {
      const _v3 = !!(show)
      if (_v3) {
        _v1 ??= (() => {
          let _v0 = __template(`<span>yes</span>`)
          const _v1 = _v0
          _v0 = [];
          _v0[0] = _v1;
          return _v0
        })();
      } else {
        _v2 ??= (() => {
          let _v0 = __template(`<span>no</span>`)
          const _v1 = _v0
          _v0 = [];
          _v0[0] = _v1;
          return _v0
        })();
      }
      _v4 = __setSlotSpread(_v0, _v3 ? _v1 : _v2, _v4);
    };
    ;(__ret[__sym_upd] = () => {
      _v0[__sym_upd]();
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
      const _v3 = !!(a)
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
      const _v5 = !!(b)
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
      const _v1 = !!(show)
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
    ;(__ret[__sym_upd] = () => {
      _v0[__sym_upd]();
    })();
  }
  return __ret
}
