// @filename: main.syn
Object.defineProperty(exports, "__esModule", { value: true });
try {} catch(e) {
  if (!(e instanceof Error)) e = new Error(String(e));
}
try {} catch(e) {}
try {} catch(e) {
  if (!(e instanceof Error)) throw e
}
try {} catch(e) {
  if (!((e instanceof Error) || (typeof e === 'string'))) throw e
}
switch (1) {
  case 1: {
    try {} catch(e) {
      if (!((e instanceof Error) || (typeof e === 'string'))) throw e
    }
    break
  }
}
