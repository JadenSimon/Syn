export class Mixer {
  static foo = 42
  get bar(): number { return Mixer.foo }
  set bar(v: number) { Mixer.foo = v }
  method?<T>(v?: T): void {}
}
export namespace Mixer {
  export const ext = true
}

// (function (Mixer) {
//     Mixer.ext = true;
// })(Mixer || (Mixer = {}));
