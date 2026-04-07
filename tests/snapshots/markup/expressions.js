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
var __setSlot = (a,v,c) => {
  const q = c?.nextSibling === a
  if (typeof v !== 'object') {
    if (q && c.nodeType === 3) { c.nodeValue = v; return c }
    v = new Text(v)
  }
  q ? c !== v && c.replaceWith(v) : a.parentNode?.insertBefore(v,a)
  return v
}
function staticText() {
  return __template(`<div>hello world</div>`)
}
function singleExpr(v) {
  const __ret = __template(`<div><!></div>`)
  {
    let _v0 = __ret.firstChild
    let _v1
    ;(__ret[__sym_upd] = () => {
      _v1 = __setSlot(_v0, v, _v1);
    })();
  }
  return __ret
}
function multiExpr(a, b) {
  const __ret = __template(`<div><!> and <!></div>`)
  {
    let _v0 = __ret.firstChild
    let _v1 = _v0.nextSibling
    _v1 = _v1.nextSibling;
    let _v2
    let _v3
    ;(__ret[__sym_upd] = () => {
      _v2 = __setSlot(_v0, a, _v2);
      _v3 = __setSlot(_v1, b, _v3);
    })();
  }
  return __ret
}
function mixedStaticDynamic(name) {
  const __ret = __template(`<div>hello <!>!</div>`)
  {
    let _v0 = __ret.firstChild
    _v0 = _v0.nextSibling;
    let _v1
    ;(__ret[__sym_upd] = () => {
      _v1 = __setSlot(_v0, name, _v1);
    })();
  }
  return __ret
}
function pureExpr() {
  const x = 'hello'
  const __ret = __template(`<div><!></div>`)
  {
    const _v0 = __ret.firstChild
    _v0.replaceWith(x);
  }
  return __ret
}
function singularSpread(items) {
  const __ret = document.createElement('div')
  ;(__ret[__sym_upd] = () => {
    __ret.replaceChildren(...items);
  })();
  return __ret
}
function mixedSpread(items) {
  const __ret = __template(`<div>before: <!> :after</div>`)
  {
    let _v0 = __ret.firstChild
    _v0 = _v0.nextSibling;
    let _v1
    ;(__ret[__sym_upd] = () => {
      _v1 = __setSlotSpread(_v0, items, _v1);
    })();
  }
  return __ret
}
