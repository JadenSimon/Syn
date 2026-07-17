// @filename: main.syn
function factory() {
  let c = 0
  function f1() {
    const id = c++
    return () => id
  }
  function f2() {
    console.log(c);
  }
  return {
    f1,
    f2
  }
}
const x = factory()
