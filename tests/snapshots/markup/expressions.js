// @filename: main.syn
var __template = ((c,p) => s => {
  c[0] ||= setTimeout(() => c={})
  return s in c ? (c[s][1] ||= p(s)).cloneNode(1) : (c[s] = [p(s)])[0]
})({}, (s,t=document.createElement('template')) => (t.innerHTML = s, t.content.firstChild))
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
function staticText() {
  return __template(`<div>hello world`)
}
function singleExpr(v) {
  const __ret = __template(`<div><!>`)
  {
    let _v0 = __ret.firstChild
    let _v1
    ;(__ret[__sym_upd] = () => {
      _v1 = __slot(_v0, _v1, v)
    })();
  }
  return __ret
}
function multiExpr(a, b) {
  const __ret = __template(`<div><!> and <!>`)
  {
    let _v0 = __ret.firstChild
    let _v1 = _v0.nextSibling
    _v1 = _v1.nextSibling;
    let _v2, _v3
    ;(__ret[__sym_upd] = () => {
      _v2 = __slot(_v0, _v2, a)
      _v3 = __slot(_v1, _v3, b)
    })();
  }
  return __ret
}
function multiExprSpaced(a, b) {
  const __ret = __template(`<div><!> <!>`)
  {
    let _v0 = __ret.firstChild
    let _v1 = _v0.nextSibling
    _v1 = _v1.nextSibling;
    let _v2, _v3
    ;(__ret[__sym_upd] = () => {
      _v2 = __slot(_v0, _v2, a)
      _v3 = __slot(_v1, _v3, b)
    })();
  }
  return __ret
}
function multiExprSpaced2(a, b) {
  const __ret = __template(`<div><!>   <!>`)
  {
    let _v0 = __ret.firstChild
    let _v1 = _v0.nextSibling
    _v1 = _v1.nextSibling;
    let _v2, _v3
    ;(__ret[__sym_upd] = () => {
      _v2 = __slot(_v0, _v2, a)
      _v3 = __slot(_v1, _v3, b)
    })();
  }
  return __ret
}
function multiExprMultiline(a, b) {
  const __ret = __template(`<div><!><!>`)
  {
    let _v0 = __ret.firstChild
    let _v1 = _v0.nextSibling
    let _v2, _v3
    ;(__ret[__sym_upd] = () => {
      _v2 = __slot(_v0, _v2, a)
      _v3 = __slot(_v1, _v3, b)
    })();
  }
  return __ret
}
function mixedStaticDynamic(name) {
  const __ret = __template(`<div>hello <!>!`)
  {
    let _v0 = __ret.firstChild
    _v0 = _v0.nextSibling;
    let _v1
    ;(__ret[__sym_upd] = () => {
      _v1 = __slot(_v0, _v1, name)
    })();
  }
  return __ret
}
function pureExpr() {
  const x = 'hello'
  const __ret = __template(`<div><!>`)
  {
    const _v0 = __ret.firstChild
    let _v1
    ;(__ret[__sym_upd] = () => {
      _v1 = __slot(_v0, _v1, x)
    })();
  }
  return __ret
}
function singularSpread(items) {
  const __ret = document.createElement('div')
  {
    let _v0 = __ret.firstChild
    let _v1 = _v0.nextSibling
    ;(__ret[__sym_upd] = () => {
      __slot_s(_v0, _v1, items);
    })();
  }
  return __ret
}
function mixedSpread(items) {
  const __ret = __template(`<div>before: <!><!> :after`)
  {
    let _v0 = __ret.firstChild
    _v0 = _v0.nextSibling;
    let _v1 = _v0.nextSibling
    ;(__ret[__sym_upd] = () => {
      __slot_s(_v0, _v1, items);
    })();
  }
  return __ret
}
function mutations() {
  let x = __template(`<span id="a">`)
  const d = __template(`<div><span></span><!>`)
  {
    let _v0 = d.firstChild
    _v0 = _v0.nextSibling;
    let _v1
    ;(d[__sym_upd] = () => {
      _v1 = __slot(_v0, _v1, x)
    })();
  }
  const s = d.firstElementChild
  s.after(__template(`<span id="b">`));
  x.before(__template(`<span id="c">`));
  x.after(__template(`<span id="d">`));
  x = __template(`<span id="e">`);
  d[Symbol.update]();
  x.remove();
  x = x;
  d[Symbol.update]();
}
function multilineElements(a) {
  const __ret = __template(`<div><span>1</span><span><!>`)
  {
    let _v0 = __ret.firstChild
    _v0 = _v0.nextSibling;
    let _v1 = _v0.firstChild
    let _v2
    ;(__ret[__sym_upd] = () => {
      _v2 = __slot(_v1, _v2, a)
    })();
  }
  return __ret
}
function multilineElements2() {
  return __template(`<div><span>1</span><span>2`)
}
function spacedElements() {
  return __template(`<div><span>1</span>   <span>2`)
}
function multilineText() {
  return __template(`<div>hello
world`)
}
function multilineText2() {
  return __template(`<div>   a

b   `)
}
function multilineText3() {
  const __ret = __template(`<div>   a
<!>
b   `)
  {
    let _v0 = __ret.firstChild
    _v0 = _v0.nextSibling;
    let _v1
    ;(__ret[__sym_upd] = () => {
      _v1 = __slot(_v0, _v1, 'c')
    })();
  }
  return __ret
}
function multilineTextFragment() {
  return __template(`<div>This is text across multiple lines`)
}
function samelineText() {
  return __template(`<div>   a   b   `)
}
function samelineTextEmpty() {
  return __template(`<div>   `)
}
