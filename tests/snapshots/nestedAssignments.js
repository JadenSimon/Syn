// @filename: main.syn
Object.defineProperty(exports, "__esModule", { value: true });
const x = {
  n: 0
}
check(x.n);
if (cond) {
  x.n = 1;
  if (x.n !== 2) {}
}
 else {
  x.n = 2;
}
check(x.n);
const x2 = {
  o: {
    n: 0
  }
}
check(x2.o.n);
if (cond) {
  x2.o = {
    n: 3
  };
}
 else {
  x2.o.n = 4;
}
check(x2.o.n);
const x3 = {
  o: {
    n: ''
  }
}
if (f(x3.o.n)) {
  check(x3);
}
check(x3);
