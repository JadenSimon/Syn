// @filename: main.syn
var __template = ((p,c,u) => s => {
  u ||= setTimeout(() => (c = {}, u = 0))
  return s in c ? (c[s].n ||= p(s)).cloneNode(true) : c[s] = p(s)
})((s,t = document.createElement('template')) => (t.innerHTML = s, t.content.firstChild), {})
var __setAttr = (x,k,v) => v == null ? x.removeAttribute(k) : x.setAttribute(k,v)
var __spreadAttributes = (f => (x, v, c = {}, t, k) => {
  for (k in c) k in v || f(x, k, null, t)
  for (k in v) f(x, k, v[k], t)
  return v
})((x, k, v, t, u = t?.[k] ?? k) => !u ? __setAttr(x,k,v) : x[u] = v)
var __sym_upd = Symbol.update ||= Symbol.for('update')
var __setSlot = (a,v,c) => {
  const q = c?.nextSibling === a
  if (typeof v !== 'object') {
    if (q && c.nodeType === 3) { c.nodeValue = v; return c }
    v = new Text(v)
  }
  q ? c !== v && c.replaceWith(v) : a.parentNode?.insertBefore(v,a)
  return v
}
function staticAttrs() {
  return __template(`<input type="text" placeholder="enter value">`)
}
function staticBoolAttrs() {
  return __template(`<input checked>`)
}
function dynamicProp(checked) {
  const __ret = __template(`<input type="checkbox">`)
  ;(__ret[__sym_upd] = () => {
    __ret.checked = checked;
  })();
  return __ret
}
function camelCaseProp() {
  const __ret = __template(`<iframe></iframe>`)
  __ret.marginWidth = 100;
  return __ret
}
function attrAlias(readOnly) {
  const __ret = document.createElement('input')
  ;(__ret[__sym_upd] = () => {
    __ret.readOnly = readOnly;
  })();
  return __ret
}
function shorthandAttr(readonly) {
  const __ret = document.createElement('input')
  ;(__ret[__sym_upd] = () => {
    __ret.readOnly = readonly;
  })();
  return __ret
}
function attrSetAttributeFallback(itemid) {
  const __ret = document.createElement('div')
  ;(__ret[__sym_upd] = () => {
    __setAttr(__ret, 'itemid', itemid);
  })();
  return __ret
}
function dataAttr(val) {
  const __ret = document.createElement('div')
  ;(__ret[__sym_upd] = () => {
    __setAttr(__ret, 'data-value', val);
  })();
  return __ret
}
function ariaAttr(label) {
  const __ret = __template(`<button>x</button>`)
  ;(__ret[__sym_upd] = () => {
    __setAttr(__ret, 'aria-label', label);
  })();
  return __ret
}
function classAlias(cls) {
  const __ret = document.createElement('div')
  ;(__ret[__sym_upd] = () => {
    __ret.className = cls;
  })();
  return __ret
}
function spreadAttrs(o) {
  const __ret = document.createElement('div')
  {
    let _v0
    ;(__ret[__sym_upd] = () => {
      _v0 = __spreadAttributes(__ret, {
        ...o
      }, _v0);
    })();
  }
  return __ret
}
function spreadAttrs2(o, o2) {
  const __ret = document.createElement('div')
  {
    let _v0
    ;(__ret[__sym_upd] = () => {
      _v0 = __spreadAttributes(__ret, {
        ...o,
        ...o2
      }, _v0);
    })();
  }
  return __ret
}
function spreadAttrs3(o) {
  const __ret = document.createElement('div')
  {
    let _v0
    ;(__ret[__sym_upd] = () => {
      _v0 = __spreadAttributes(__ret, {
        class: 'a',
        ...o
      }, _v0);
    })();
  }
  return __ret
}
function spreadAttrs4(o) {
  const __ret = document.createElement('div')
  {
    let _v0
    ;(__ret[__sym_upd] = () => {
      _v0 = __spreadAttributes(__ret, {
        ...o,
        class: 'a'
      }, _v0);
    })();
  }
  return __ret
}
function spreadAttrs5(o, value) {
  const __ret = document.createElement('input')
  {
    let _v0
    ;(__ret[__sym_upd] = () => {
      _v0 = __spreadAttributes(__ret, {
        ...o,
        value: value
      }, _v0);
    })();
  }
  return __ret
}
function manyNodes(v) {
  const __ret = __template(`<div>1
<span><!></span> 2
<span><!></span></div>`)
  {
    let _v0 = __ret.firstChild
    _v0 = _v0.nextSibling;
    let _v1 = _v0.nextSibling
    _v1 = _v1.nextSibling;
    let _v2 = _v0.firstChild
    let _v3
    let _v4 = _v1.firstChild
    let _v5
    ;(__ret[__sym_upd] = () => {
      _v3 = __setSlot(_v2, v, _v3);
      _v5 = __setSlot(_v4, v, _v5);
    })();
  }
  return __ret
}
