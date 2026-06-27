// @filename: main.syn

export function f() {
  return x.a
}
export function f2() {
  return x.b
}
export function f3() {
  return x.b()
}
export function f4() {
  return x2.c
}
export function f5() {
  return x2.d()
}
export function f6() {
  return x2.zz
}
export function f7() {
  return x2.f({
    a: 1
  })
}
export function f9() {
  return x3.f()
}
export function f10() {
  return x3.f2(x3)
}
export function f11() {
  return x3.f3({
    b: 1
  })
}
