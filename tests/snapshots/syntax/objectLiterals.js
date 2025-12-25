// @filename: main.ts
const o = {}
const x = {
  async async() {},
  async get() {},
  async *gen() {},
  *gen2() {},
  method() {},
  method2(y = 'a' in o) {},
  'string': 1,
  ['computed' + '!']: 'hi',
  get set() {
    return 1
  },
  set set(v) {},
  get 'set!'() {
    return '!'
  },
  get [`computed_${1}`]() {
    return 12
  },
  ...o,
  o,
  same_line: 'yes!',
  prop: 2,
  predicate: (x) => (x in o , 'jk'),
  delete() {},
  void() {}
}
