// @filename: main.syn
export declare function ambientOnly(): number;
export declare class PrivateOnly {
  #x = 1;
  get() {
    return this.#x
  }
}
export declare class Both {
  #x = 1;
  get() {
    return Math.abs(this.#x)
  }
}
