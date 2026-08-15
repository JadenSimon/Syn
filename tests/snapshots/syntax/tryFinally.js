// @filename: main.syn
function bar() {}
function foo() {
  let c = 0
  const x = {
    dispose() {
      c += 1;
    }
  }
  try {
    bar();
  } finally {
    x.dispose();
  }
  if (!c) throw new Error("did not dispose?")
}
foo();
