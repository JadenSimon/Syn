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
function Inner(props) {
  const a = document.createComment('')
  const b = document.createComment('')
  let len = 0
  let __ret = __template(`<div><!><!></div>`)
  const _v0 = __ret.firstChild
  const _v1 = _v0.nextSibling
  _v0.replaceWith(a);
  _v1.replaceWith(b);
  let cur
  let i
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
      if (n === c) continue
      if (n.nodeType === 3 && typeof c !== 'object') {
        n.nodeValue = c;
        continue
      }
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
  let __ret = __comp(Inner)
  const _v0 = __ret._p.children = []
  let _v1 = 0
  let _v2 = 0
  __ret._u = () => {
    if (_v1) {
      _v0.splice(_v1 -= _v2, _v2);
    }
    _v1 += (_v2 = __pushAt(_v0, _v1, props.items));
  };
  __ret[__sym_upd]();
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
