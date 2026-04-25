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
var __spreadComp = function(a=this._b) {
  return this.root._sc(a), a
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
function Inner(props) {
  const a = document.createComment('')
  const b = document.createComment('')
  let len = 0
  let __ret
  __ret = [];
  __ret[0] = a;
  __ret[1] = b;
  let cur
  let i
  __ret._sc = (a) => {
    let d = a._d
    if (!d) {
      d = a._d = {
        c0: new Comment(),
        c1: new Comment()
      };
      a.after(d.c0, d.c1);
      a.remove();
    }
    d.c1.s = __setSlot(d.c1, __ret[1], d.c1.s);
    d.c0.s = __setSlot(d.c0, __ret[0], d.c0.s);
  };
  ;(__ret[__sym_upd] = () => {
    cur = a.nextSibling;
    i = 0;
    while (cur && cur !== b) {
      const n = cur
      cur = cur.nextSibling;
      if (i >= props.children.length) {
        n.remove();
        continue
      }
      const c = props.children[i++]
      if (n === c || (n.nodeType === 3 && typeof c !== 'object' && n.nodeValue === String(c))) continue
      if (len > props.children.length) {
        i--;
        n.remove();
        len -= 1;
      } else if (len < props.children.length) {
        cur = n;
        cur.before(c);
        len += 1;
      } else n.replaceWith(c);
    }
    while (i < props.children.length) b.before(props.children[i++]);
    len = i;
  })();
  return __ret
}
function WithSpread(props) {
  let __ret = __template(`<div><!></div>`)
  let _v0 = __ret.firstChild
  const _v1 = __comp(Inner)
  _v1._b = _v0;
  _v1._s = __spreadComp;
  _v1._u = () => {
    _v1._p.children = _v1.root._mc || !Array.isArray(props.items) ? [...props.items] : props.items;
  };
  ;(__ret[__sym_upd] = _v1[__sym_upd].bind(_v1))();
  return __ret
}
const items = []
const c = __comp(WithSpread)
c._p.items = items;
c[__sym_upd]();
let flipped = false
document.body.append((() => {
  const _v0 = __template(`<div>update items</div>`)
  _v0.addEventListener('click', () => {
    const t = setInterval(() => {
      if (items.length === 500) {
        clearInterval(t);
        items.length = 250;
        flipped = !flipped;
      } else {
        const s = `item-${items.length}`
        {
          if (flipped) {
            items.unshift(s);
          } else {
            items.push(s);
          }
        }
        c[Symbol.update]?.();
      }
    }, 5)
  });
  return _v0
})(), c.root);
