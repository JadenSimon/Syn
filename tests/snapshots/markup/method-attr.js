// @filename: main.syn
var __template = ((c,p) => (s,b) => {
  c[0] ||= setTimeout(() => c={})
  return s in c ? (c[s][1] ||= p(s,b)).cloneNode(1) : (c[s] = [p(s,b)])[0]
})({}, (s,b,t=document.createElement('template')) => {
  t.innerHTML = s
  return b ? t.content : t.content.firstChild
})
function spreadMapped() {
  let currentSubNav
  const subnav = []
  const __ret = __template(`<div><!><!>`)
  {
    let _v0 = __ret.firstChild // <!> - {...}
    let _v1 = _v0.nextSibling // {...} - <!>
    _v0.after(...subnav.map(s => (() => {
      const _v0 = __template(`<a><!>`)
      _v0.addEventListener('click', () => {
        if (currentSubNav) currentSubNav.classList.remove('cur', 'jumped-to');
        currentSubNav = _v0;
        _v0.classList.add('cur', 'jumped-to');
      });
      let _v1 = _v0.firstChild // <!> - {}
      _v0.href = s.href;
      _v1.before(s.label);
      return _v0
    })()));
  }
  return __ret
}
