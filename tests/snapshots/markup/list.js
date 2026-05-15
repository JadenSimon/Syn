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
    t = this.root = t(this._p)
    this._a && t.setAttribute(this._a,'')
    this._s?.() ?? this._b?.replaceWith(t)
  } else {
    t[__sym_upd]?.()
    this._s?.()
  }
}})
var __comp_s = function(a=this._b) {
  return this.root._sc(a), a
}
var __pushAt = (c,i,v,d=0,l = c.length) => (c.splice(i,d,...v), c.length-l+d)
var __slot_s = (a,v,b) => {
  b || a.after(b = a.cloneNode())
  let p, n = a.nextSibling
  while (p = n, n = p.nextSibling, p !== b) p.remove()
  a.after(...v)
  return b
}
var __slot = (a,v,c) => {
  const q = c?.nextSibling === a
  if (typeof v !== 'object') {
    if (q && c.nodeType === 3) { c.nodeValue != v && (c.nodeValue = v); return c }
    v = new Text(v)
  }
  q ? c !== v && c.replaceWith(v) : a.parentNode?.insertBefore(v,a)
  return v
}
function List(props) {
  const m = new Map()
  let __ret
  __ret = [];
  let _v0 = 0
  let _v1 = 0
  let elements
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
    d.c0.s = __slot_s(d.c0, __ret.slice(_v2 -= _v1, _v2 + _v1), d.c0.s);
  };
  ;(__ret[__sym_upd] = () => {
    _v0 = 0;
    elements = new Set();
    for (let i = 0; i < props.items.length; i++) {
      const x = props.items[i]
      const k = props.key?.(x) ?? (x && typeof x === 'object' ? x : i)
      let n = m.get(k)
      elements.add(n);
    }
    for (const [k, v] of m) {
      if (!elements.has(v)) m.delete(k);
    }
    _v0 += (_v1 = __pushAt(__ret, _v0, Array.from(elements), _v1));
  })();
  return __ret
}
const items = [{
  id: 1,
  first: '1',
  last: '2'
}, {
  id: 2,
  first: '3',
  last: '4'
}]
let nextId = 3
document.body.append((() => {
  const _v0 = __template(`<div><div style="margin-bottom: 8px;"><button>add</button><button>reverse</button><button>sort</button><button>remove last</button></div><!>`)
  const _v1 = _v0.firstChild
  let _v2 = _v1.nextSibling
  const root = _v0
  const _v3 = _v1.firstChild
  const _v4 = _v3.nextSibling
  const _v5 = _v4.nextSibling
  const _v6 = _v5.nextSibling
  _v3.addEventListener('click', () => {
    {
      items.push({
        id: nextId++,
        first: String(nextId),
        last: String(nextId + 1)
      });
    }
    root[Symbol.update]?.();
  });
  _v4.addEventListener('click', () => {
    {
      items.reverse();
    }
    root[Symbol.update]?.();
  });
  _v5.addEventListener('click', () => {
    {
      items.sort((a, b) => a.first.localeCompare(b.first));
    }
    root[Symbol.update]?.();
  });
  _v6.addEventListener('click', () => {
    {
      items.pop();
    }
    root[Symbol.update]?.();
  });
  const _v7 = __comp(List)
  _v7._b = _v2;
  _v7._s = __comp_s;
  _v7._p.items = items;
  _v7._p.key = x => x.id;
  const _v8 = _v7._p.children = []
  _v8[0] = x => (() => {
    const _v0 = __template(`<div><!> <!>`)
    let _v1 = _v0.firstChild
    let _v2 = _v1.nextSibling
    _v2 = _v2.nextSibling;
    let _v3
    let _v4
    ;(_v0[__sym_upd] = () => {
      _v3 = __slot(_v1, x.first, _v3);
      _v4 = __slot(_v2, x.last, _v4);
    })();
    return _v0
  })();
  ;(_v0[__sym_upd] = _v7[__sym_upd].bind(_v7))();
  return _v0
})());
