// @filename: main.syn
Object.defineProperty(exports, "__esModule", { value: true });
try {} catch(e) {
  if (!(e instanceof Error)) e = new Error(e);
}
try {} catch(e) {}
try {} catch(e) {
  if (!(e instanceof Error)) throw e
}
try {} catch(e) {
  if (!(e instanceof Error || typeof e === 'string')) throw e
}
switch (1) {
  case 1: {
    try {} catch(e) {
      if (!(e instanceof Error || typeof e === 'string')) throw e
    }
    break
  }
}
class CustomError extends Error {
  code = 1;
}
try {
  throw new CustomError()
} catch(e) {
  if (!(e instanceof CustomError)) throw e
  e.code;
}
