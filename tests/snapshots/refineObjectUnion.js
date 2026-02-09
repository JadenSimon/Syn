// @filename: main.syn
Object.defineProperty(exports, "__esModule", { value: true });
exports.refine = refine;
exports.refine2 = refine2;
function refine(input) {
  if (input.kind === 1) {
    return input
  }
  if (input.kind === 2) {
    return input
  }
  if (input.kind === 3) {
    return input
  }
}
function refine2(input) {
  if (input.kind === 1) {
    return input
  }
   else if (input.kind === 2) {
    return input
  }
   else if (input.kind === 3) {
    return input
  }
  if (input.extra === 0) return input.extra
}
