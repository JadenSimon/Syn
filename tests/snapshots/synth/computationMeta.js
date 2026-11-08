// @filename: main.syn
if (!Symbol.toComputation) {
  Object.defineProperty(Symbol, 'toComputation', {
    value: Symbol.for('toComputation')
  });
}
const __meta$0 = {
  ambient: ['Math']
}
const __meta$1 = {
  ambient: ['Math']
}
const __meta$2 = {
  ambient: ['Math']
}
export function ambientOnly() {
  return Math.max(1, 2)
}
ambientOnly[Symbol.toComputation] = () => ['function ambientOnly() {\n  return Math.max(1, 2)\n}', [], __meta$0];
export class PrivateOnly {
  #x = 1;
  get() {
    return this.#x
  }
  static {
    this.prototype.get[Symbol.toComputation] = () => ['function get() {\n  return this.#x\n}', [], undefined, PrivateOnly];
    this[Symbol.toComputation] = () => ['class PrivateOnly {\n  #x = 1;\n  get() {\n    return this.#x\n  }\n}', []];
  }
}
export class Both {
  #x = 1;
  get() {
    return Math.abs(this.#x)
  }
  static {
    this.prototype.get[Symbol.toComputation] = () => ['function get() {\n  return Math.abs(this.#x)\n}', [], __meta$1, Both];
    this[Symbol.toComputation] = () => ['class Both {\n  #x = 1;\n  get() {\n    return Math.abs(this.#x)\n  }\n}', [], __meta$2];
  }
}
