// @filename: main.syn
var __template = ((p,c,u) => s => {
  u ||= setTimeout(() => (c = {}, u = 0))
  return s in c ? (c[s].n ||= p(s)).cloneNode(true) : c[s] = p(s)
})((s,t = document.createElement('template')) => (t.innerHTML = s, t.content.firstChild), {})
var __sym_upd = Symbol.update ||= Symbol.for('update')
var __setSlot = (a,v,c) => {
  const q = c?.nextSibling === a
  if (typeof v !== 'object') {
    if (q && c.nodeType === 3) { c.nodeValue = v; return c }
    v = document.createTextNode(v)
  }
  q ? c !== v && c.replaceWith(v) : a.parentNode?.insertBefore(v,a)
  return v
}
function basicRun(x) {
  const __ret = __template(`<div><!></div>`)
  {
    let _v0 = __ret.firstChild
    let _v1
    ;(__ret[__sym_upd] = () => {
      console.log(x);
      _v1 = __setSlot(_v0, x, _v1);
    })();
  }
  return __ret
}
function runDeclaredVar(items) {
  const __ret = __template(`<div><!></div>`)
  {
    let _v0 = __ret.firstChild
    let count
    let _v1
    ;(__ret[__sym_upd] = () => {
      count = items.length;
      _v1 = __setSlot(_v0, count, _v1);
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
      if (_v0.length) {
        let f
        while (f = _v0.pop()) f();
      }
      id = setInterval(() => {}, 500);
      _v0.push(() => {
        clearInterval(id);
      });
    })();
  }
  return __ret
}
function runWithBinding(active) {
  const __ret = __template(`<div><span></span></div>`)
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
      if (_v0.length) {
        let f
        while (f = _v0.pop()) f();
      }
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
