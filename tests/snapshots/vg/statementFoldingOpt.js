let _c0 = {};
{
  let _d0 = 1
    _c0["Symbol.update"] = () => {
  _d0 += 1;
  _c0[Symbol.update]();
};
}

const _ = _c0