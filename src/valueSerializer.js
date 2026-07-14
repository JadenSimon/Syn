const Kind = {
  undefined: 1,
  null: 2,
  true: 3,
  false: 4,
  string: 5,
  number: 6,
  ref: 7,
  array: 8,
  object: 9,
  computed: 10,
};

const numberImmediates = new Map([[-1, 1], [0, 2], [1, 3], [2, 4], [3, 5], [4, 6]]);
const refImmediates = new Map([
  [-1, 1], [-2, 2], [-4, 3], [-8, 4], [-16, 5], [-32, 6],
  [-64, 7], [-128, 8], [-256, 9], [-512, 10], [-1024, 11],
]);

const textEncoder = new TextEncoder();
const toComputed = Symbol.for('toComputed')

class Encoder {
  constructor() {
    this.bytes = [];
    this.seen = new Map();
  }

  get pos() {
    return this.bytes.length;
  }

  u8(byte) {
    this.bytes.push(byte & 0xff);
  }

  tag(kind, upper) {
    this.u8((upper << 4) | kind);
  }

  uintLE(value, byteCount) {
    for (let i = 0; i < byteCount; i++) {
      this.bytes.push(value & 0xff);
      value = Math.floor(value / 256);
    }
  }

  intLE(value, byteCount) {
    this.uintLE(value < 0 ? value + 2 ** (byteCount * 8) : value, byteCount);
  }

  floatLE(value, byteCount) {
    const view = new DataView(new ArrayBuffer(byteCount));
    if (byteCount === 4) view.setFloat32(0, value, true);
    else view.setFloat64(0, value, true);
    for (let i = 0; i < byteCount; i++) this.bytes.push(view.getUint8(i));
  }

  encode(value) {
    if (value === undefined) return this.tag(Kind.undefined, 0);
    if (value === null) return this.tag(Kind.null, 0);
    if (value === true) return this.tag(Kind.true, 0);
    if (value === false) return this.tag(Kind.false, 0);
    if (typeof value === 'number') return this.encodeNumber(value);
    if (typeof value === 'string') return this.encodeString(value);
    const c = value[toComputed]
    if (c) {
        const target = typeof c === 'function' ? c : c.target
        const input = typeof c === 'function' ? [] : c.input
        return this.encodeCompound(Kind.computed, value, () => {
            this.encode(target);
            this.encode(input);
        })
    }
    if (Array.isArray(value)) {
      return this.encodeCompound(Kind.array, value, () => {
        for (const item of value) this.encode(item);
      });
    }
    if (typeof value === 'object') {
      return this.encodeCompound(Kind.object, value, () => {
        for (const [k, v] of Object.entries(value)) {
          this.encodeString(k);
          this.encode(v);
        }
      });
    }
    throw new TypeError(`Cannot serialize value of type ${typeof value}`);
  }

  encodeCompound(kind, value, writeContent) {
    const existing = this.seen.get(value);
    if (existing !== undefined) return this.encodeRef(existing);

    const tagPos = this.pos;
    this.seen.set(value, tagPos);
    this.u8(0);
    writeContent();
    const contentLength = this.pos - tagPos - 1;

    if (contentLength <= 10) {
      this.bytes[tagPos] = ((contentLength + 1) << 4) | kind;
    } else {
      this.bytes[tagPos] = kind;
      this.u8(0);
    }
  }

  encodeRef(targetPos) {
    const offset = targetPos - this.pos;

    const immediate = refImmediates.get(offset);
    if (immediate !== undefined) return this.tag(Kind.ref, immediate);

    if (offset >= -128 && offset <= 127) {
      this.tag(Kind.ref, 0b1100);
      this.intLE(offset, 1);
    } else if (offset >= -32768 && offset <= 32767) {
      this.tag(Kind.ref, 0b1101);
      this.intLE(offset, 2);
    } else if (offset >= -2147483648 && offset <= 2147483647) {
      this.tag(Kind.ref, 0b1110);
      this.intLE(offset, 4);
    } else {
      this.tag(Kind.ref, 0b1111);
      this.intLE(offset, 8);
    }
  }

  encodeNumber(n) {
    if (Number.isInteger(n) && Number.isSafeInteger(n)) {
      const immediate = numberImmediates.get(n);
      if (immediate !== undefined) return this.tag(Kind.number, immediate);

      if (n >= 0 && n <= 255) { this.tag(Kind.number, 0b0111); return this.uintLE(n, 1); }
      if (n >= -128 && n <= 127) { this.tag(Kind.number, 0b1010); return this.intLE(n, 1); }
      if (n >= 0 && n <= 65535) { this.tag(Kind.number, 0b1000); return this.uintLE(n, 2); }
      if (n >= -32768 && n <= 32767) { this.tag(Kind.number, 0b1011); return this.intLE(n, 2); }
      if (n >= 0 && n <= 4294967295) { this.tag(Kind.number, 0b1001); return this.uintLE(n, 4); }
      if (n >= -2147483648 && n <= 2147483647) { this.tag(Kind.number, 0b1100); return this.intLE(n, 4); }
    }

    if (Math.fround(n) === n) { this.tag(Kind.number, 0b1110); return this.floatLE(n, 4); }
    this.tag(Kind.number, 0b1111);
    this.floatLE(n, 8);
  }

  maybeEncodeStringRef(str) {
    const pos = this.seen.get(str);
    if (pos === undefined) return false;
    const delta = pos - this.pos;
    if (refImmediates.has(delta)) { 
        this.encodeRef(pos);
        return true;
    }
    let requiredBytes
    if (delta >= -128 && delta <= 127) 
        requiredBytes = 1
    else if (delta >= -32768 && delta <= 32767) 
        requiredBytes = 2
    else if (delta >= -2147483648 && delta <= 2147483647) 
        requiredBytes = 4
    else 
        requiredBytes = 8
    // we're going to assume that strings are mostly ASCII
    // so, the length of the string is approximately the # of bytes needed to store it
    if (requiredBytes > str.length) return false;
    this.encodeRef(pos);
    return true;
  }

  encodeString(str) {
    if (str === '') return this.tag(Kind.string, 1);
    if (this.maybeEncodeStringRef(str)) return;

    const start = this.pos;
    this.seen.set(str, start);
    const bytes = textEncoder.encode(str);

    if (bytes.length <= 10) {
      this.tag(Kind.string, bytes.length + 1);
      this.bytes.push(...bytes);
      return;
    }

    if (!bytes.includes(0)) {
      this.tag(Kind.string, 0);
      this.bytes.push(...bytes);
      this.u8(0);
      return;
    }

    let widthBytes, upper;
    if (bytes.length < 256) { widthBytes = 1; upper = 0b1100; }
    else if (bytes.length < 65536) { widthBytes = 2; upper = 0b1101; }
    else if (bytes.length < 4294967296) { widthBytes = 4; upper = 0b1110; }
    else { widthBytes = 8; upper = 0b1111; }

    this.tag(Kind.string, upper);
    this.uintLE(bytes.length, widthBytes);
    this.bytes.push(...bytes);
  }
}

export function serialize(value) {
  const encoder = new Encoder();
  encoder.encode(value);
  return Uint8Array.from(encoder.bytes);
}