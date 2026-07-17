let _c0 = {};
let _c1 = "function(a,b) { return () => a(b) }";
_c0.s0 = _c1;
let _c2 = (p) => console.log('hi', p);
let _c3 = {};
_c3.f = () => _c0.s0;
_c0.v0 = (function(a,b) { return () => a(b) })(_c2, _c3);
_c0.v1 = _c2;
_c0.v2 = _c3;
{
  let _d0 = 1
    _c0.v3 = () => _d0++;
  _c0.v4 = () => _d0;
}

const _ = _c0