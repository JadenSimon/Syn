let _c0 = {};
{
  let _d0 = 1
    _c0["Symbol.update"] = () => {
  _d0 += 1;
  _c0[Symbol.update]();
};
  _c0.foo = () => _c0.bar();
}

const _ = _c0