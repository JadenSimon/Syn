// @filename: main.syn
var __template = ((c,p) => (s,b) => {
  c[0] ||= setTimeout(() => c={})
  return s in c ? (c[s][1] ||= p(s,b)).cloneNode(1) : (c[s] = [p(s,b)])[0]
})({}, (s,b,t=document.createElement('template')) => {
  t.innerHTML = s
  return b ? t.content : t.content.firstChild
})
var __sym_upd = Symbol.update ||= Symbol.for('update')
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
{
  function Foo(_attrs, children = []) {
    let c = 0
    function inc() {
      c += 1;
    }
    const __ret = __template(`<div><div>c: <!></div>
----
<!><!>`)
    const _v0 = __ret.firstChild // div
    let _v1 = _v0.nextSibling // #text
    _v1 = _v1.nextSibling; // <!> - {...}
    let _v2 = _v1.nextSibling // {...} - <!>
    let _v3 = _v0.firstChild // #text
    _v3 = _v3.nextSibling; // <!> - {}
    let _v4
    function Bar(_attrs, children = []) {
      const __ret = __template(`<div>Bar: <!>
-----
<!><!>`)
      let _v5 = __ret.firstChild // #text
      _v5 = _v5.nextSibling; // <!> - {}
      let _v6 = _v5.nextSibling // #text
      _v6 = _v6.nextSibling; // <!> - {...}
      let _v7 = _v6.nextSibling // {...} - <!>
      let _v8
      return {
        root: __ret,
        [__sym_upd]: () => {
          _v8 = __slot(_v5, _v8, c)
          __slot_s(_v6, _v7, children);
        }
      }
    }
    return {
      root: __ret,
      [__sym_upd]: () => {
        _v4 = __slot(_v3, _v4, c)
        __slot_s(_v1, _v2, children);
      },
      Bar,
      inc
    }
  }
  let _f = void 0
  {
    const _v0 = [null]
    let _v1, _v3
    const _v4 = [null]
    let _v5
    function _v2() {
      if (!_f) {
        _f = Foo(void 0, _v0);
        _v1 = _f[Symbol.update];
        _f[Symbol.update] = _v2;
      }
      _v3 ??= f.Bar(void 0, _v4);
      _v5 ??= f.Bar();
      _v5[Symbol.update]();
      _v4[0] = _v5.root;
      _v3[Symbol.update]();
      _v0[0] = _v3.root;
      _v1?.();
    }
    _v2();
  }
  document.body.append(_f.root);
  setTimeout(() => {
    _f.inc();
    _f[Symbol.update]();
  }, 1000);
}
function createMountHook(el) {
  let connected = false
  let dispose
  return (cb) => {
    const next = el.isConnected
    if (next && !connected) {
      dispose?.();
      dispose = cb();
      connected = true;
    } else if (!next && connected) {
      dispose?.();
      dispose = undefined;
      connected = false;
    }
  }
}
function Timer(attrs = {}) {
  if (!('start' in attrs)) attrs.start = 0;
  let seconds = attrs.start
  function reset() {
    seconds = 0;
    d[Symbol.update]();
  }
  const d = __template(`<div>Seconds: <!>`)
  let _v3 = 0x1
  let _v0 = d.firstChild // #text
  _v0 = _v0.nextSibling; // <!> - {}
  let _v1
  d[__sym_upd] = () => {
    _v1 = __slot(_v0, _v1, seconds)
  };
  let _v2
  return {
    root: d,
    [__sym_upd]: () => {
      d[__sym_upd]();
      const onMount = (_v2 ??= createMountHook(d))
      if (_v3) {
        _v3 >>= 1;
        _v2 = createMountHook(d);
      }
      onMount(() => {
        const t = setInterval(() => {
          seconds += 1;
          d[Symbol.update]();
        }, 1000)
        return () => clearInterval(t)
      });
    },
    reset
  }
}
{
  const qq = __template(`<div><!><!><button>toggle`)
  {
    const _v0 = qq.firstChild // <!> - #if
    let _v1 = _v0.nextSibling // #if - <!>
    let _v2 = _v1.nextSibling // button
    let visible = true
    let _t, _v3, _v8, _v11
    _v2.addEventListener('click', () => {
      visible = !visible;
      qq[Symbol.update]();
    });
    ;(qq[__sym_upd] = () => {
      const _v10 = !!visible
      if (_v10) {
        if (!_v3) {
          const _v4 = []
          _v3 = __template(`<!><button>reset`, 1);
          _v4.push(..._v3.childNodes);
          _v2 = _v3.firstChild; // Timer
          let _v5 = _v2.nextSibling // button
          let t, _v6
          function _v7() {
            if (!t) {
              t = Timer();
              _v6 = t[Symbol.update];
              t[Symbol.update] = _v7;
            }
            _v6?.();
            if (_v2) _v2 = void _v2.replaceWith(t.root);
          }
          _v5.addEventListener('click', () => t.reset());
          _v3 = _v4;
          _v3[__sym_upd] = () => {
            _v7();
            _t = t;
          };
        }
      } else {
        if (!_v8) {
          _v8 = [new Comment()];
          _v8[__sym_upd] = () => {
            if (_t) {
              _t[Symbol.update]();
            }
          };
        }
      }
      const _v5 = _v10 ? _v3 : _v8
      if (_v10 !== _v11) __swap_tree(_v0, _v1, _v5, _v10 ? _v8 : _v3);
      _v11 = _v10;
      _v5[Symbol.update]();
    })();
  }
  document.body.append(qq);
  qq[Symbol.update]();
}
const timer = Timer()
timer[Symbol.update]();
document.body.append(timer.root);
timer[Symbol.update]();
