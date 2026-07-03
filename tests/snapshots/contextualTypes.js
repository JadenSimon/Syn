// @filename: main.syn

const o1 = {
  bar: a => a.length,
  tuple: [{
    f: a => a.length
  }],
  array: [{
    f: a => a.length
  }]
}
const o2 = {
  kind: 1,
  f: a => a.length
}
const o3 = {
  kind: 2,
  f: a => a.toFixed(1)
}
