// @filename: main.syn
var __template = ((p,c,u) => s => {
  u ||= setTimeout(() => (c = {}, u = 0))
  return s in c ? (c[s]._n ||= p(s)).cloneNode(true) : c[s] = p(s)
})((s,t = document.createElement('template')) => (t.innerHTML = s, t.content.firstChild), {})
var __sym_upd = Symbol.update ||= Symbol.for('update')
var __slot = (a,v,c) => {
  const q = c?.nextSibling === a
  if (typeof v !== 'object') {
    if (q && c.nodeType === 3) { c.nodeValue != v && (c.nodeValue = v); return c }
    v = new Text(v)
  }
  q ? c !== v && c.replaceWith(v) : a.parentNode?.insertBefore(v,a)
  return v
}
var __unw = (a,f) => { while (f = a.pop()) if (Array.isArray(f)) __unw(f); else f() }
function basicRun(x) {
  const __ret = __template(`<div><!>`)
  {
    let _v0 = __ret.firstChild
    let _v1
    ;(__ret[__sym_upd] = () => {
      console.log(x);
      _v1 = __slot(_v0, x, _v1);
    })();
  }
  return __ret
}
function runDeclaredVar(items) {
  const __ret = __template(`<div><!>`)
  {
    let _v0 = __ret.firstChild
    let count, _v1
    ;(__ret[__sym_upd] = () => {
      count = items.length;
      _v1 = __slot(_v0, count, _v1);
    })();
  }
  return __ret
}
function runWithUnwind() {
  const __ret = document.createElement('div')
  {
    let id
    let _v0 = []
    ;(__ret[__sym_upd] = () => {
      __unw(_v0);
      id = setInterval(() => {}, 500);
      _v0.push(() => {
        clearInterval(id);
      });
    })();
  }
  return __ret
}
function runWithBinding(active) {
  const __ret = __template(`<div><span>`)
  {
    const el = __ret.firstChild
    ;(__ret[__sym_upd] = () => {
      el.hidden = !active;
    })();
  }
  return __ret
}
function runOnComponent(rate) {
  const __ret = document.createElement('div')
  {
    const root = __ret
    let _v0 = []
    ;(__ret[__sym_upd] = () => {
      __unw(_v0);
      if (rate) {
        const id = setInterval(() => {}, rate)
        _v0.push(() => {
          clearInterval(id);
        });
      }
    })();
  }
  return __ret
}
