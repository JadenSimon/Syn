// @filename: main.syn
var __template = ((p,c,u) => s => {
  u ||= setTimeout(() => (c = {}, u = 0))
  return s in c ? (c[s]._n ||= p(s)).cloneNode(true) : c[s] = p(s)
})((s,t = document.createElement('template')) => (t.innerHTML = s, t.content.firstChild), {})
var __style = (c => s => c[s] ||= (document.head.insertAdjacentHTML('beforeend', `<style>${s}</style>`),1))({})
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
var __comp_s = function(a=this._b) {
  return this.root._sc(a),a
}
var __pushAt = (c,i,v,d=0,l=c.length) => (c.splice(i,d,...v), c.length-l+d)
var __slot_s = (a,v,b) => {
  b || a.after(b = a.cloneNode())
  let p, n = a.nextSibling
  for (p of v) n === p ? n=n.nextSibling : __slot(n,p,null,0)
  while (p = n, n = p.nextSibling, p !== b) p.remove()
  return b
}
var __slot = (a,v,c,q=c?.nextSibling === a) => {
  if (typeof v !== 'object') {
    if (q && c.nodeType === 3) { c.data != v && (c.data = v); return c }
    v = new Text(v)
  }
  q ? c !== v && c.replaceWith(v) : a.parentNode?.insertBefore(v,a)
  return v
}
function Styled() {
  let __ret = __template(`<div _wzt87kf2>hello`)
  __style(`
    div[_wzt87kf2] { color: red }
  `);
  return __ret
}
function Button(props) {
  let __ret = __template(`<button><!>`)
  let _v0 = __ret.firstChild
  let _v1
  ;(__ret[__sym_upd] = () => {
    _v1 = __slot(_v0, props.label, _v1);
  })();
  return __ret
}
function useButton(label) {
  const __ret = __comp(Button)
  __ret._u = () => {
    __ret._p.label = label;
  };
  __ret[__sym_upd]();
  return __ret
}
function Card(props) {
  let __ret = __template(`<div><!>`)
  let _v0 = __ret.firstChild
  let _v1
  ;(__ret[__sym_upd] = () => {
    _v1 = __slot(_v0, props.title, _v1);
  })();
  return __ret
}
function withCard(title) {
  const __ret = __template(`<div><!>`)
  {
    let _v0 = __ret.firstChild
    const _v2 = {}
    let _v1
    ;(__ret[__sym_upd] = () => {
      _v2.title = title;
      if (!_v1) {
        _v1 = Card(_v2);
        _v0.replaceWith(_v1);
      } else _v1[__sym_upd]?.();
    })();
  }
  return __ret
}
function Row(props) {
  let __ret = __template(`<a><span><!></span><span><!>`)
  const _v0 = __ret.firstChild
  const _v1 = _v0.nextSibling
  __ret = [];
  let _v2 = _v0.firstChild
  let _v3
  __ret[0] = _v0;
  let _v4 = _v1.firstChild
  let _v5
  __ret[1] = _v1;
  __ret._sc = a => {
    if (!a._d) {
      a._d = 1;
      a.after(...__ret);
      a.remove();
    }
  };
  ;(__ret[__sym_upd] = () => {
    _v3 = __slot(_v2, props.a, _v3);
    _v5 = __slot(_v4, props.b, _v5);
  })();
  return __ret
}
function Inner(props) {
  let __ret
  __ret = [];
  let _v0 = 0
  let _v1 = 0
  __ret._sc = a => {
    let d = a._d
    if (!d) {
      d = a._d = {
        c0: new Comment()
      };
      a.after(d.c0);
      a.remove();
    }
    let _v2 = _v0
    d.c0.s = __slot_s(d.c0, __ret.slice(_v2 -= _v1, _v2 + _v1), d.c0.s);
  };
  ;(__ret[__sym_upd] = () => {
    _v0 = 0;
    _v0 += (_v1 = __pushAt(__ret, _v0, props.children, _v1));
  })();
  return __ret
}
function withSpread(items) {
  const __ret = __template(`<div><!><!>`)
  {
    let _v0 = __ret.firstChild
    let _v1 = _v0.nextSibling
    const _v2 = __comp(Row)
    _v2._b = _v0;
    _v2._s = __comp_s;
    _v2._p.a = '1';
    _v2._p.b = '2';
    const _v3 = __comp(Inner)
    _v3._b = _v1;
    _v3._s = __comp_s;
    _v3._u = () => {
      _v3._p.children = _v3.root._mc || !Array.isArray(items) ? [...items] : items;
    };
    ;(__ret[__sym_upd] = () => {
      _v2[__sym_upd]();
      _v3[__sym_upd]();
    })();
  }
  return __ret
}
function Inner2(props) {
  let __ret = __template(`<a>`)
  let _v0 = __ret.firstChild
  __ret = [];
  let _v1 = 0
  let _v2 = 0
  let _v3 = 0
  const _v4 = _v0._p.children = []
  let _v5 = 0
  let _v6 = 0
  const _v7 = __comp(Row)
  _v7._p.a = '3';
  _v7._p.b = '4';
  __ret._sc = a => {
    let d = a._d
    if (!d) {
      d = a._d = {
        c0: new Comment(),
        c1: new Comment()
      };
      a.after(d.c0, d.c1);
      a.remove();
    }
    let _v8 = _v1
    d.c1.s = __slot_s(d.c1, __ret.slice(_v8 -= _v2, _v8 + _v2), d.c1.s);
    _v0.root._sc(d.c0);
    _v8 -= _v3;
  };
  ;(__ret[__sym_upd] = () => {
    _v1 = 0;
    _v5 = 0;
    _v7[__sym_upd]();
    _v5 += (_v6 = __pushAt(_v4, _v5, _v7.root, _v6));
    _v1 += (_v3 = __pushAt(__ret, _v1, _v0.root, _v3));
    _v1 += (_v2 = __pushAt(__ret, _v1, props.children, _v2));
  })();
  return __ret
}
function withSpread2() {
  const __ret = __template(`<div><!>`)
  {
    let _v0 = __ret.firstChild
    const _v1 = __comp(Inner2)
    _v1._b = _v0;
    _v1._s = __comp_s;
    const _v2 = _v1._p.children = []
    let _v3 = 0
    let _v4 = 0
    const _v5 = __comp(Inner2)
    const _v6 = __template(`<a><span>5</span><span>6`)
    const _v7 = _v6.firstChild
    const _v8 = _v7.nextSibling
    const _v9 = _v5._p.children = [_v7, _v8]
    _v1._u = () => {
      _v3 = 0;
      _v5[__sym_upd]();
      _v3 += (_v4 = __pushAt(_v2, _v3, _v5.root, _v4));
    };
    ;(__ret[__sym_upd] = _v1[__sym_upd].bind(_v1))();
  }
  return __ret
}
function NestedOuter() {
  let c = 0
  let selected = -1
  let __ret = __template(`<div><!><!>`)
  let _v0 = __ret.firstChild
  let _v1 = _v0.nextSibling
  const root = __ret
  function NesterInner() {
    const id = c++
    let counter = 0
    let __ret = __template(`<div _cic8oe6s style="padding: 16px; width: fit-content;"><div _cic8oe6s>id: <!></div><div _cic8oe6s>counter: <!></div><button _cic8oe6s>increment`)
    __style(`
        .selected[_cic8oe6s] {
          border: 1px solid blue;
          font-weight: bold;
        }
      `);
    const _v2 = __ret.firstChild
    const view = _v2.nextSibling
    const _v3 = view.nextSibling
    let _v4 = _v2.firstChild
    _v4 = _v4.nextSibling;
    _v4.replaceWith(id);
    let _v5 = view.firstChild
    _v5 = _v5.nextSibling;
    let _v6
    view[__sym_upd] = () => {
      _v6 = __slot(_v5, counter, _v6);
    };
    _v3.addEventListener('click', () => {
      {
        counter += 1;
      }
      view[Symbol.update]?.();
      if (selected !== id) {
        {
          selected = id;
        }
        root[Symbol.update]?.();
      }
    });
    ;(__ret[__sym_upd] = () => {
      __ret.classList.toggle('selected', selected === id);
      view[__sym_upd]();
    })();
    return __ret
  }
  let _v7, _v8
  ;(__ret[__sym_upd] = () => {
    if (!_v7) {
      _v7 = NesterInner();
      _v0.replaceWith(_v7);
    } else _v7[__sym_upd]?.();
    if (!_v8) {
      _v8 = NesterInner();
      _v1.replaceWith(_v8);
    } else _v8[__sym_upd]?.();
  })();
  return __ret
}
{
  function Squared(props) {
    let updateCount = 0
    let __ret = __template(`<div><div>update count: <!></div> x * x = <!>`)
    const _v0 = __ret.firstChild
    let _v1 = _v0.nextSibling
    _v1 = _v1.nextSibling;
    let _v2 = _v0.firstChild
    _v2 = _v2.nextSibling;
    let _v3, _v4
    ;(__ret[__sym_upd] = () => {
      _v3 = __slot(_v2, ++updateCount, _v3);
      _v4 = __slot(_v1, props.x * props.x, _v4);
    })();
    return __ret
  }
  let x = 0
  document.body.append((() => {
    const _v0 = __template(`<div><!><button>inc x: <!>`)
    let _v1 = _v0.firstChild
    const _v2 = _v1.nextSibling
    const d = _v0
    const _v4 = {}
    let _v3
    _v2.addEventListener('click', () => {
      {
        x += 1;
      }
      d[Symbol.update]?.();
    });
    let _v5 = _v2.firstChild
    _v5 = _v5.nextSibling;
    let _v6
    ;(_v0[__sym_upd] = () => {
      _v4.x = x;
      if (!_v3) {
        _v3 = Squared(_v4);
        _v1.replaceWith(_v3);
      } else _v3[__sym_upd]?.();
      _v6 = __slot(_v5, x, _v6);
    })();
    return _v0
  })());
}
function SharedScope() {
  function register(el) {}
  setTimeout(() => register(d));
  let __ret = document.createElement('div')
  const d = __ret
  return __ret
}
{
  let c = 0
  let ids = 0
  function Comp(props) {
    const id = ids++
    let __ret = __template(`<div>id: <!>; a: <!>; c: <!><div><!>`)
    let _v0 = __ret.firstChild
    _v0 = _v0.nextSibling;
    let _v1 = _v0.nextSibling
    _v1 = _v1.nextSibling;
    let _v2 = _v1.nextSibling
    _v2 = _v2.nextSibling;
    const _v3 = _v2.nextSibling
    _v0.replaceWith(id);
    let _v4, _v5
    let _v6 = _v3.firstChild
    let _v7
    ;(__ret[__sym_upd] = () => {
      _v4 = __slot(_v1, props.a, _v4);
      _v5 = __slot(_v2, c++, _v5);
      _v7 = __slot_s(_v6, props.children, _v7);
    })();
    return __ret
  }
  document.body.append((() => {
    const _v0 = __template(`<div><!><button>update`)
    let _v1 = _v0.firstChild
    const _v2 = _v1.nextSibling
    const d = _v0
    const _v3 = __comp(Comp)
    _v3._b = _v1;
    const _v4 = _v3._p.children = []
    const _v5 = __comp(Comp)
    const _v6 = _v5._p.children = []
    _v5._u = () => {
      _v5._p.a = c;
    };
    const _v7 = __comp(Comp)
    const _v8 = _v7._p.children = []
    _v7._u = () => {
      _v7._p.a = c;
    };
    _v3._u = () => {
      _v3._p.a = c;
      _v5[__sym_upd]();
      _v4[0] = _v5.root;
      _v7[__sym_upd]();
      _v4[1] = _v7.root;
    };
    _v2.addEventListener('click', () => {
      d[Symbol.update]?.();
    });
    ;(_v0[__sym_upd] = _v3[__sym_upd].bind(_v3))();
    return _v0
  })());
}
{
  function Foo() {
    let c = 0
    function inc() {
      {
        c++;
      }
      d[Symbol.update]?.();
    }
    this.inc = inc;
    let __ret = __template(`<div><!>`)
    let _v0 = __ret.firstChild
    const d = __ret
    let _v1
    ;(__ret[__sym_upd] = () => {
      _v1 = __slot(_v0, c, _v1);
    })();
    return __ret
  }
  document.body.append((() => {
    const _v0 = __template(`<div><!><button>click!`)
    let _v1 = _v0.firstChild
    const _v2 = _v1.nextSibling
    const f = __comp(Foo)
    f._b = _v1;
    _v2.addEventListener('click', () => f.inc());
    ;(_v0[__sym_upd] = f[__sym_upd].bind(f))();
    return _v0
  })());
}
{
  function NestedSync(props) {
    function getFoo() {
      return Foo
    }
    this.getFoo = getFoo;
    let __ret = __template(`<div>d: <!>`)
    const _v1 = __ret
    let _v0
    let _v2 = _v1.firstChild
    _v2 = _v2.nextSibling;
    let _v3
    __ret = _v1;
    function Foo(props) {
      let __ret = __template(`<div>d: <!> y: <!>`)
      let _v4 = __ret.firstChild
      _v4 = _v4.nextSibling;
      let _v5 = _v4.nextSibling
      _v5 = _v5.nextSibling;
      let _v6, _v7
      ;(__ret[__sym_upd] = () => {
        _v6 = __slot(_v4, _v0, _v6);
        _v7 = __slot(_v5, props.y, _v7);
      })();
      return __ret
    }
    ;(__ret[__sym_upd] = __p => {
      if (__p) {
        const d = props.x.repeat(2)
        _v0 = d;
      }
      _v3 = __slot(_v2, d, _v3);
    })();
    return __ret
  }
  document.body.append((() => {
    const _v0 = __template(`<div><!><label>change val:
<input></label><div><!></div><button>add Foo`)
    let _v1 = _v0.firstChild
    const _v2 = _v1.nextSibling
    const list = _v2.nextSibling
    const _v3 = list.nextSibling
    const root = _v0
    const comps = []
    let val = ''
    const n = __comp(NestedSync)
    n._b = _v1;
    n._u = () => {
      n._p.x = val;
    };
    _v1 = _v2.firstChild;
    _v1 = _v1.nextSibling;
    _v1.addEventListener('input', ev => {
      {
        val = ev.currentTarget.value;
      }
      root[Symbol.update]?.();
    });
    let _v4 = list.firstChild
    let _v5
    list[__sym_upd] = () => {
      _v5 = __slot_s(_v4, comps.map(x => x.root), _v5);
      for (const c of comps) c[Symbol.update]?.();
    };
    let _v6
    ;(_v0[__sym_upd] = () => {
      function addFoo() {
        const Foo = n.getFoo()
        const id = comps.length
        const inst = __comp(Foo)
        inst._p.y = id;
        inst[__sym_upd]();
        {
          comps.push(inst);
        }
        list[Symbol.update]?.();
      }
      n[__sym_upd]();
      list[__sym_upd]();
      if (_v6) _v3.removeEventListener('click', _v6)
      _v3.addEventListener('click', _v6 = addFoo);
    })();
    return _v0
  })());
}
{
  ;(() => {
    const _v0 = __template(`<div><div><div>c: <!></div><!></div><button>back to zero`)
    const outer = _v0.firstChild
    const _v2 = outer.nextSibling
    const view = outer.firstChild
    let _v3 = view.nextSibling
    let _v1
    let _v4
    let _v5
    let _v6 = view.firstChild
    _v6 = _v6.nextSibling;
    let _v7
    view[__sym_upd] = () => {
      _v7 = __slot(_v6, _v4(), _v7);
    };
    function Foo() {
      let __ret = __template(`<button>delayed inc`)
      let _v10
      ;(__ret[__sym_upd] = () => {
        const _v8 = _v4
        const _v9 = _v5
        if (_v10) __ret.removeEventListener('click', _v10)
        __ret.addEventListener('click', _v10 = () => {
          setTimeout(() => {
            {
              _v9(_v8() + 1);
            }
            view[Symbol.update]?.();
          }, 1000);
        });
      })();
      return __ret
    }
    let _v11
    outer[__sym_upd] = () => {
      let c = 0
      _v4 = () => c;
      _v5 = v => c = v;
      _v1 = c;
      view[__sym_upd]();
      if (!_v11) {
        _v11 = Foo();
        _v3.replaceWith(_v11);
      } else _v11[__sym_upd]?.();
    };
    _v2.addEventListener('click', () => {
      outer[Symbol.update]?.();
    });
    ;(_v0[__sym_upd] = outer[__sym_upd].bind(outer))();
    return _v0
  })();
}
{
  function Imm(props) {
    let __ret = new Comment()
    let _v0 = __ret
    const _v1 = __comp(Inner)
    _v1._b = _v0;
    _v1._u = () => {
      _v1._p.y = props.x.repeat(2);
    };
    __ret = _v0;
    function Inner(props) {
      let __ret = __template(`<div><!> : <!>`)
      let _v2 = __ret.firstChild
      let _v3 = _v2.nextSibling
      _v3 = _v3.nextSibling;
      let _v4, _v5
      ;(__ret[__sym_upd] = () => {
        _v4 = __slot(_v2, props.x, _v4);
        _v5 = __slot(_v3, props.y, _v5);
      })();
      return __ret
    }
    ;(__ret[__sym_upd] = __p => {
      _v1[__sym_upd]();
      __ret = _v1.root;
    })();
    return __ret
  }
  function Run() {
    let __ret = __template(`<div>`)
    const d = __ret
    d[__sym_upd] = () => {
      console.log('2');
    };
    const _v0 = d[__sym_upd]
    __ret = d;
    ;(__ret[__sym_upd] = __p => {
      if (__p) console.log('1');
      _v0()
    })();
    return __ret
  }
}
