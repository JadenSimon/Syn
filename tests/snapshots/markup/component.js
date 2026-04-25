// @filename: main.syn
var __template = ((p,c,u) => s => {
  u ||= setTimeout(() => (c = {}, u = 0))
  return s in c ? (c[s].n ||= p(s)).cloneNode(true) : c[s] = p(s)
})((s,t = document.createElement('template')) => (t.innerHTML = s, t.content.firstChild), {})
var __style = (c => s => c[s] ||= (document.head.insertAdjacentHTML('beforeend', s),1))({})
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
var __spreadComp = function(a=this._b) {
  return this.root._sc(a), a
}
var __pushAt = (c,i,v,d=0,l = c.length) => (c.splice(i,d,...v), c.length-l+d)
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
function Styled(props) {
  let __ret = __template(`<div _wzt87kf2>hello</div>`)
  __style(`<style>
    div[_wzt87kf2] { color: red }
  </style>`);
  return __ret
}
function Button(props) {
  let __ret = __template(`<button><!></button>`)
  let _v0 = __ret.firstChild
  let _v1
  ;(__ret[__sym_upd] = () => {
    _v1 = __setSlot(_v0, props.label, _v1);
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
  let __ret = __template(`<div><!></div>`)
  let _v0 = __ret.firstChild
  let _v1
  ;(__ret[__sym_upd] = () => {
    _v1 = __setSlot(_v0, props.title, _v1);
  })();
  return __ret
}
function withCard(title) {
  const __ret = __template(`<div><!></div>`)
  {
    let _v0 = __ret.firstChild
    const _v1 = __comp(Card)
    _v1._b = _v0;
    _v1._u = () => {
      _v1._p.title = title;
    };
    ;(__ret[__sym_upd] = _v1[__sym_upd].bind(_v1))();
  }
  return __ret
}
function Row(props) {
  let __ret = __template(`<a><span><!></span><span><!></span>`)
  const _v0 = __ret.firstChild
  const _v1 = _v0.nextSibling
  __ret = [];
  let _v2 = _v0.firstChild
  let _v3
  __ret[0] = _v0;
  let _v4 = _v1.firstChild
  let _v5
  __ret[1] = _v1;
  __ret._sc = (a) => {
    if (!a._d) {
      a = 1;
      a.after(...__ret);
      a.remove();
    }
  };
  ;(__ret[__sym_upd] = () => {
    _v3 = __setSlot(_v2, props.a, _v3);
    _v5 = __setSlot(_v4, props.b, _v5);
  })();
  return __ret
}
function Inner(props) {
  let __ret
  __ret = [];
  let _v0 = 0
  let _v1 = 0
  __ret._sc = (a) => {
    let d = a._d
    if (!d) {
      d = a._d = {
        c0: new Comment()
      };
      a.after(d.c0);
      a.remove();
    }
    let _v2 = _v0
    d.c0.s = __setSlotSpread(d.c0, __ret.slice(_v2 -= _v1, _v2 + _v1), d.c0.s);
  };
  ;(__ret[__sym_upd] = () => {
    if (_v0) {
      __ret.splice(_v0 -= _v1, _v1);
    }
    _v0 += (_v1 = __pushAt(__ret, _v0, props.children));
  })();
  return __ret
}
function withSpread(items) {
  const __ret = __template(`<div><!><!></div>`)
  {
    let _v0 = __ret.firstChild
    let _v1 = _v0.nextSibling
    const _v2 = __comp(Row)
    _v2._b = _v0;
    _v2._s = __spreadComp;
    _v2._p.a = '1';
    _v2._p.b = '2';
    const _v3 = __comp(Inner)
    _v3._b = _v1;
    _v3._s = __spreadComp;
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
