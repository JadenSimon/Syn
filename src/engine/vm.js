"use strict";

// Opcodes
//
// Every instruction is `[Op.X, ...operands]`.
// Operands:
//   r, r1, r2, ...   register index
//   k                constant pool index
//   idx              abs index into bytecode array

// heap design
// - no tagged handles
// - zero prefix = relative handle
// - cells allocated in 8 byte increments, but addressed in 4 byte increments. 4 byte alignment OK
// - low 8 bits = offset into an SC
// - remaining high bits = decoded into start of the SC (raw ptr) via base addr. we reserve 16gb virtual mem ahead of time.
// - start of the SC is the SC itself
// - leaf SCs for stack frames don't have internal ref counters
// - 0x0 and 0xFFFF_FFXX cannot be used as the first word of a heap cell


const Op = {
    Mov: 0x00,                // [rsrc, rdst]  reg[rdst] = reg[rsrc]

    Star: 0x01,               // [r]     reg[r] = acc
    Star0: 0x02,              // []      reg[0] = acc
    Star1: 0x03,              // []      reg[1] = acc
    Star2: 0x04,              // []      reg[2] = acc
    Star3: 0x05,              // []      reg[3] = acc
    Star4: 0x06,              // []      reg[4] = acc
    Star5: 0x07,              // []      reg[5] = acc
    Star6: 0x08,              // []      reg[6] = acc
    Star7: 0x09,              // []      reg[7] = acc

    LdaImm: 0x22,           // [n]           acc = n (SIGNED)
    LdaZero: 0x23,     // []            acc = 0
    LdaConst: 0x24,       // [k]           acc = constants[k]
    LdaUndefined: 0x26, // []
    LdaNull: 0x27,         // []
    LdaTrue: 0x28,         // []
    LdaFalse: 0x29,       // []
    Ldar: 0x2A,     // [r]           acc = reg[r]
    Ldar0: 0x2B,     // []           acc = reg[0]
    Ldar1: 0x2C,     // []           acc = reg[1]
    Ldar2: 0x2D,     // []           acc = reg[2]
    Ldar3: 0x2E,     // []           acc = reg[3]

    Add: 0x30, Sub: 0x31, Mul: 0x32, Div: 0x33, Mod: 0x34, Exp: 0x35,
    BitOr: 0x36, BitAnd: 0x37, BitXor: 0x38,
    ShiftLeft: 0x39, ShiftRight: 0x3A, ShiftRightLogical: 0x3B,
    TestEqual: 0x3C, TestStrictEqual: 0x3D,
    TestLessThan: 0x3E, TestGreaterThan: 0x3F,
    TestLessThanOrEqual: 0x40, TestGreaterThanOrEqual: 0x41,
    TestIn: 0x42, TestInstanceOf: 0x43,

    // AddImm, ModImm, BitOrImm, BitAndImm, BitXorImm, ShiftLeftImm, ShiftRightImm, ShiftRightLogicalImm

    Inc: 0x4A, Dec: 0x4B, Negate: 0x4C, BitNot: 0x4D,
    LogicalNot: 0x4E, TypeOf: 0x4F,

    // TODO: CoerceToBoolean - !!x

    Jump: 0x60,                       // [idx]
    JumpIfToBooleanTrue: 0x61,   // [idx]  if (acc) pc = off
    JumpIfToBooleanFalse: 0x62, // [idx]  if (!acc) pc = off
    // JumpIfNull: 0x2D,           // [idx]
    // JumpIfUndefined: 0x2E, // [idx]

    // normal call -> first arg is recv?
    Call: 0x63,           // [rcallee, rargStart, argCount]   
    CallConst: 0x64,
    CallBuiltin: 0x66,
    Construct: 0x69,
    CatchEnter: 0x75, // [relIdx] registers an EH
    CatchExit: 0x76, // [relIdx] pops EH + jmp
    Throw: 0x77,        // [] uses acc
    Rethrow: 0x78,      // [] does not add anything to the trace
    Return: 0x79,       // []



    GetProperty: 0x80, // [rkey]   acc = acc[reg[rkey]]
    SetProperty: 0x81, // [robj, rkey]   reg[robj][reg[rkey]] = acc
    DeleteProperty: 0x82,   // [rkey] acc = delete acc[reg[rkey]]

    GetPropertyImm: 0x90, // [unsigned imm] acc = acc[imm]
    SetPropertyImm: 0x91, // [robj, unsigned imm] reg[robj][imm] = acc

    GetAccProperty: 0x94, // [robj, rdst]    reg[rdst] = reg[robj][acc]
    SetAccProperty: 0x95, // [robj, rval]    reg[robj][acc] = reg[rval]

    CreateEmptyObject: 0xA0, // []  acc = {}
    CreateEmptyArray: 0xA1,   // []  acc = []

    CreateClosure: 0xC5, // consumes a function template inside constant pool. frames are templated and resolved at this point

    // these change the interpretation of the next instruction similar to V8's ignition bytecode
    // we need to specialize the dispatch directly for this, we don't want to be checking which width we have in the fast path
    Width2: 0xF0, // 2 byte operands
    Width4: 0xF1, // 4 byte operands
    Width8: 0xF2, // 8 byte operands

    // variable length. this is NOT a deopt, it is a runtime assertion that aborts the program
    Assert: 0xFF, // [assertionKind, ...args] 
}
// const switchDispatchOrder = [
//   "Mov",
//   "Star",
//   "Star0",
//   "Star1",
//   "Star2",
//   "Star3",
//   "Star4",
//   "Star5",
//   "Star6",
//   "Star7",
//   "LdaConst",
//   "LdaImm",
//   "LdaUndefined",
//   "LdaNull",
//   "LdaTrue",
//   "LdaFalse",
//   "LdaZero",
//   "Ldar",
//   "Ldar0",
//   "Ldar1",
//   "Ldar2",
//   "Ldar3",
//   "Add",
//   "Sub",
//   "Mul",
//   "Div",
//   "Mod",
//   "Exp",
//   "BitOr",
//   "BitAnd",
//   "BitXor",
//   "ShiftLeft",
//   "ShiftRight",
//   "ShiftRightLogical",
//   "TestEqual",
//   "TestStrictEqual",
//   "TestLessThan",
//   "TestGreaterThan",
//   "TestLessThanOrEqual",
//   "TestGreaterThanOrEqual",
//   "TestIn",
//   "TestInstanceOf",
//   "Inc",
//   "Dec",
//   "Negate",
//   "BitNot",
//   "LogicalNot",
//   "TypeOf",
//   "Jump",
//   "JumpIfToBooleanTrue",
//   "JumpIfToBooleanFalse",
//   "Call",
//   "CallConst",
//   "Return",
//   "GetProperty",
//   "SetProperty",
//   "DeleteProperty",
//   "CreateEmptyObject",
//   "CreateEmptyArray",
//   "Width2",
//   "Assert",
// ]
// for (const [k, v] of Object.entries(Op)) {
//     Op[k] = switchDispatchOrder.indexOf(k)
// }
// grep -oP '^\s{12}case Op\.\K[A-Za-z0-9]+' src/vm.js | awk '{printf "  \"%s\",\n", $0}'

// u8
const AssertionKind = {
    AccumulatorIs: 0,       // [Type]
    AccumulatorIsNot: 1,    // [Type]
    RegisterIs: 10,         // [regIdx, Type]
    RegisterIsNot: 11,      // [regIdx, Type]
    // these assertions happen inside operations that consume registers w/ the acc
    NextRegisterIs: 20,     // [Type]
    // NextRegisterIsNot: 21,  // [Type]

    // sets up the interpreter for symbolic evaluation
    Instrument: 255,
}

let hasPendingRegisterAssertion = 0
let pendingRegisterAssertionType = 0

// T is expected
// U is current type, which may be wider than T or absent for reg loads
function checkType(val, T, U) {
    if (U === undefined) {
        if (val & 1) {
            val = val >> 1
            U = Type.Handle
        } else {
            U = Type.Smi
        }
    }
    if (T === U) return true
    if (U === Type.Handle) {
        // TODO: better checks
        // also, register assertions should ideally happen during the register load in the op
        // otherwise it's harder to propagate facts correctly due to `Intrinsics.load` being opaque
        return getNarrowedTypeFromHeapCell(val) === T
    }
    return false
}

function assert(ok) {
    if (!ok) throw "Assertion failed"
}

function assertIs(val, T, U) {
    assert(checkType(val, T, U))
    if (U === undefined) {
        if (typeNeedsHandleInRegister(T)) {
            assert((val & 1) === 1)
        } else {
            assert((val & 1) === 0)
        }
    }
}

function assertIsNot(val, T, U) {
    assert(!checkType(val, T, U))
    if (U === undefined) {
        if (!typeNeedsHandleInRegister(T)) {
            assert((val & 1) === 1)
        } else {
            assert((val & 1) === 0)
        }
    }
}

function assertCurrentRegValue(v) {
    if (hasPendingRegisterAssertion) {
        hasPendingRegisterAssertion = 0
        assertIs(v, pendingRegisterAssertionType)
    }
}

function getNarrowedTypeFromHeapCell(val) {
    switch (Intrinsics.load(val, Type.Handle)) {
        case True: return Type.True
        case False: return Type.False
        case Null: return Type.Null
        case Undefined: return Type.Undefined

        // heap numbers
        case HeapF64: return Type.f64
        case HeapU64: return Type.u64
        case HeapI64: return Type.i64
        case HeapU32: return Type.u32
        case HeapI32: return Type.i32
    }   
}


function typeNeedsHandleInRegister(T) {
    return T >= Type.u32
}

const Type = {
    Smi: 0,
    u8: 1,
    i8: 2,
    u16: 3,
    i16: 4, // i16/u16/u8/i8 end up as SMIs in acc/regs
    u32: 5,
    i32: 6,
    u64: 7,
    i64: 8,
    f64: 9,

    NativePtr: 19, // void ptr
    Handle: 20, // opaque heap handle

    // special types, these don't exist in the interpreter but are useful for assertions
    Float: 26,
    Integer: 27,
    Undefined: 28,
    Null: 29,
    Nullish: 30,
    True: 35,
    False: 36,
    String: 37,
    Array: 38,
    Object: 39,
    RegExp: 40,
    Function: 41, // any function
    Symbol: 42,
    Boolean: 43,
    Promise: 44, // exactly a Promise
    PromiseLike: 45,
    Error: 46, // exactly an Error
    ErrorLike: 47,
    AsyncFunction: 48,

    // some basic fn specializations
    Receiverless: 70,

    Computation: 80, // the primitive

    AnyArrayBuffer: 90,
    ArrayBuffer: 91,
    SharedArrayBuffer: 92,

    // Typed arrays
    U8Array: 100,
    U16Array: 101,
    U32Array: 102,
    U64Array: 103,

    I8Array: 104,
    I16Array: 105,
    I32Array: 106,
    I64Array: 107,

    F16Array: 108,
    F32Array: 109,
    F64Array: 110,    

    // Some internal types
    CFunction: 151, // uses a C call conv
    JSFunction: 152, // same as NativeFunction, but entry point is the `interpret` fn + bytecode
    NativeFunction: 153, // uses our call conv

    InterpreterFrame: 160,
}

function sizeof(T) {
    switch (T) {
        case Type.u64:
        case Type.i64:
        case Type.f64: return 8
        case Type.u32:
        case Type.i32:
        case Type.Handle: return 4
        case Type.u16:
        case Type.i16: return 2
        case Type.u8:
        case Type.i8: return 1
        default:
            return 4
    }
}

const maxIntf64Cmp = 2**53
const maxSmi = 2**31 // a smi is already bit shifted by 1

const TypeName = Object.fromEntries(Object.entries(Type).map(([k, v]) => [v, k]))

const cellBytes = 8
const slotBytes = 4
const heapBytes = 1 << 20
const stackBytes = 1 << 20
const memBuffer = new ArrayBuffer(heapBytes+stackBytes)
const memView = new DataView(memBuffer)
let heapTop = cellBytes
let stackPointer = heapBytes // hmmm this stack grows the wrong way. whatever

const externalObjects = [] // JS objects

const Intrinsics = {
    Type,
    accType: 0,
    OF: 0,
    as: function(v, T) {
        return v
    },
    addressof: function(buf) {
        return buf.byteOffset
    },
    stackalloc: function(num, T) {
        const start = stackPointer
        if (T === undefined) {
            stackPointer += num
            return start
        }
        const bytes = num*sizeof(T)
        stackPointer += bytes
        switch (T) {
            case Type.u32: return new Uint32Array(memBuffer, start, num)
            case Type.i32: return new Int32Array(memBuffer, start, num)
            throw `not handled: ${T}`
        }
    },
    stackfree: function(num, T) {
        const bytes = num*sizeof(T)
        stackPointer -= bytes
    },
    alloc: function(numSlots) {
        const bytes = Math.max(8, numSlots*4)
        if (heapTop + bytes > heapBytes) throw new Error('heap exhausted')
        const handle = heapTop
        heapTop += bytes
        return handle
    },
    store: function(ptr, val, T) {
        switch (T) {
            case Type.u8: memView.setUint8(ptr, val); return
            case Type.i8: memView.setInt8(ptr, val); return
            case Type.u16: memView.setUint16(ptr, val, true); return
            case Type.i16: memView.setInt16(ptr, val, true); return
            case Type.u32: memView.setUint32(ptr, val >>> 0, true); return
            case Type.i32: memView.setInt32(ptr, val, true); return
            case Type.f64: memView.setFloat64(ptr, val, true); return
            case Type.Handle: memView.setUint32(ptr, val >>> 0, true); return
            case Type.u64:
            case Type.i64: {
                const lo = val >>> 0
                const hi = Math.floor(val / 0x1_0000_0000) | 0
                memView.setUint32(ptr, lo, true)
                memView.setInt32(ptr + 4, hi, true)
                return
            }
            default:
                throw new Error(`store: unsupported type ${TypeName[T]}`)
        }
    },
    load: function(ptr, T) {
        switch (T) {
            case Type.u8: return memView.getUint8(ptr)
            case Type.i8: return memView.getInt8(ptr)
            case Type.u16: return memView.getUint16(ptr, true)
            case Type.i16: return memView.getInt16(ptr, true)
            case Type.u32: return memView.getUint32(ptr, true)
            case Type.i32: return memView.getInt32(ptr, true)
            case Type.f64: return memView.getFloat64(ptr, true)
            case Type.Handle: return memView.getUint32(ptr, true)
            case Type.u64: {
                const lo = memView.getUint32(ptr, true)
                const hi = memView.getUint32(ptr + 4, true)
                return hi * 0x1_0000_0000 + lo
            }
            case Type.i64: {
                const lo = memView.getUint32(ptr, true)
                const hi = memView.getInt32(ptr + 4, true)
                return hi * 0x1_0000_0000 + lo
            }
            default:
                throw new Error(`load: unsupported type ${TypeName[T]}`)
        }
    },
    // selects between two values using the cond
    // cond _must_ be a bool! we will not coerce
    select: function(cond, v1, v2) {
        return cond ? v1 : v2
    },
    // converts T -> U
    convert: function(v, T, U) {
        if (T === U) return v
        if (U === Type.f64) return Number(v)
        if (T === Type.f64) return Math.trunc(v)
        return v
    },
    cmpf64: function(a, b) {
        return a > a
    },
    // use with f64 and i64/u64 comparisons
    // assumes a is `f64`
    cmpmixed64: function(a, b, T) {
        if (b > -maxIntf64Cmp && b < maxIntf64Cmp) {
            return Intrinsics.cmpf64(a, Intrinsics.convert(b, T, Type.f64))
        }
        return false // todo
    },
    isInteger: function(v) {
        return Number.isInteger(v)
    }
}

const Null = Intrinsics.alloc(0)
const Undefined = Intrinsics.alloc(0)
const True = Intrinsics.alloc(0)
const False = Intrinsics.alloc(0)

function tagHandle(handle) {
    return (handle << 1) | 1
}

const TaggedNull = tagHandle(Null)
const TaggedUndefined = tagHandle(Undefined)
const TaggedTrue = tagHandle(True)
const TaggedFalse = tagHandle(False)

// some type descriptors
const HeapF64 = Intrinsics.alloc(0)
const HeapI64 = Intrinsics.alloc(0)
const HeapU64 = Intrinsics.alloc(0)
const HeapU32 = Intrinsics.alloc(0)
const HeapI32 = Intrinsics.alloc(0)

const JSFunction = Intrinsics.alloc(0)

// only used for symbolic evaluation
// unknowns may be specialized further
const Unknown = Intrinsics.alloc(0)
function allocOpaque() {
    const h = Intrinsics.alloc(2)
    Intrinsics.store(h, Unknown, Type.Handle)
    return h
}

function promoteType(t1, t2) {
    if (t1 === t2) return t1
    if (t1 === Type.Smi) return t2
    if (t2 === Type.Smi) return t1
    if (t1 === Type.f64) return t1
    if (t2 === Type.f64) return t2
    return Type.i64
}

const SMI_MAX = maxSmi / 2 - 1
const SMI_MIN = -(maxSmi / 2)

function narrowSmi(raw, wideType) {
    if (Intrinsics.isInteger(raw) && raw >= SMI_MIN && raw <= SMI_MAX) return Type.Smi
    return wideType
}

let resolvedType
function resolveNumeric(val, T) {
    if (T === Type.Smi) { resolvedType = Type.Smi; return val >> 1 }
    if (T !== Type.Handle) { resolvedType = T; return val }
    const m = maybeGetNumericHeapType(val)
    if (m === undefined) throw new Error(`not a numeric handle: ${val}`)
    resolvedType = m
    return taggedUnbox(val, m)
}

function smiTaggedOp(opName, a, b) {
    switch (opName) {
        case 'add': return Intrinsics.add32(a, b)
        case 'sub': return Intrinsics.sub32(a, b)
        case 'mod': return Intrinsics.mod32(a, b)
        case 'bitor': return Intrinsics.bitor32(a, b)
        case 'bitand': return Intrinsics.bitand32(a, b)
        case 'bitxor': return Intrinsics.bitxor32(a, b)
        default: return undefined
    }
}

function binOpDirect(opName, a, T, b, U) {
    if (T === Type.Smi && T === U) {
        const raw = smiTaggedOp(opName, a, b)
        if (raw !== undefined) {
            Intrinsics.accType = Type.Smi
            if (!Intrinsics.checkoverflow()) {    
                return raw
            }
            const raw2 = Intrinsics[opName + '64'](a / 2, b / 2)
            Intrinsics.accType = raw2 > 0 ? Type.u64 : Type.i64
            return raw2
        }
    }

    if (opName === 'add' && (T === Type.Handle || U === Type.Handle) && (isString(a) || isString(b))) {
        // TODO: doesn't handle mixed concats
        const r = strconcat(a, b)
        Intrinsics.accType = Type.handle
        return r
    }

    a = resolveNumeric(a, T); T = resolvedType
    b = resolveNumeric(b, U); U = resolvedType
    const C = promoteType(T, U)
    a = Intrinsics.convert(a, T, C)
    b = Intrinsics.convert(b, U, C)
    let raw
    if (C === Type.f64) raw = Intrinsics[opName + 'f64'](a, b)
    else if (C === Type.Smi) raw = Intrinsics[opName + '32'](a, b)
    else raw = Intrinsics[opName + '64'](a, b)
    Intrinsics.accType = narrowSmi(raw, C)
    return raw
}

function binOp(opName, a, T, b) {
    if (hasPendingRegisterAssertion) {
        hasPendingRegisterAssertion = 0
        assertIs(b, pendingRegisterAssertionType)
    }
    if (b & 1) return binOpDirect(opName, a, T, b >> 1, Type.Handle)
    return binOpDirect(opName, a, T, b, Type.Smi)
}

function divOp(a, T, rawB) {
    assertCurrentRegValue(rawB)
    let b, U
    if ((rawB & 1) === 0) { b = rawB; U = Type.Smi }
    else { b = rawB >> 1; U = Type.Handle }
    a = resolveNumeric(a, T); T = resolvedType
    b = resolveNumeric(b, U); U = resolvedType
    const raw = Intrinsics.divf64(Intrinsics.convert(a, T, Type.f64), Intrinsics.convert(b, U, Type.f64))
    Intrinsics.accType = narrowSmi(raw, Type.f64)
    return raw
}

function shiftRightOp(a, T, rawB) {
    assertCurrentRegValue(rawB)
    let b, U
    if ((rawB & 1) === 0) { b = rawB; U = Type.Smi }
    else { b = rawB >> 1; U = Type.Handle }
    a = resolveNumeric(a, T); T = resolvedType
    b = resolveNumeric(b, U); U = resolvedType
    const C = promoteType(T, U)
    const opName = C === Type.u64 ? 'shrl' : 'shr'
    a = Intrinsics.convert(a, T, C)
    b = Intrinsics.convert(b, U, C)
    const raw = C === Type.Smi ? Intrinsics[opName + '32'](a, b) : Intrinsics[opName + '64'](a, b)
    Intrinsics.accType = narrowSmi(raw, C)
    return raw
}

const compareOpSuffixes = {
    0: 'lt',
    1: 'gt',
    2: 'lte',
    3: 'gte',
}

function compareOp(op, a, T, b) {
    assertCurrentRegValue(b)
    if (T !== Type.Smi || (b & 1)) {
        let U
        if (b & 1) { b = b >> 1; U = Type.Handle } else { U = Type.Smi }
        a = resolveNumeric(a, T); T = resolvedType
        b = resolveNumeric(b, U); U = resolvedType
        const C = promoteType(T, U)
        a = Intrinsics.convert(a, T, C)
        b = Intrinsics.convert(b, U, C)
        const width = C === Type.f64 ? 'f64' : '64'
        return Intrinsics[`cmp${width}${compareOpSuffixes[op]}`](a, b)
    }
    switch (op) {
        case 0: return Intrinsics.cmp32lt(a, b)
        case 1: return Intrinsics.cmp32gt(a, b)
        case 2: return Intrinsics.cmp32lte(a, b)
        case 3: return Intrinsics.cmp32gte(a, b)
    }
    throw new Error(`missing op: ${op}`)
}
Intrinsics.checkoverflow = () => Intrinsics.OF
Intrinsics.add32 = (a, b) => {
    const raw = a + b
    Intrinsics.OF = (((a ^ raw) & (b ^ raw)) < 0) ? 1 : 0
    return raw
}
Intrinsics.sub32 = (a, b) => {
    const raw = a - b
    Intrinsics.OF = (((a ^ b) & (a ^ raw)) < 0) ? 1 : 0
    return raw
}
Intrinsics.mul32 = (a, b) => { Intrinsics.OF = 0; return Math.imul(a, b) }
Intrinsics.mod32 = (a, b) => { Intrinsics.OF = 0; return (a % b) }
Intrinsics.pow32 = (a, b) => { Intrinsics.OF = 0; return (a ** b) }
Intrinsics.bitor32 = (a, b) => { Intrinsics.OF = 0; return a | b }
Intrinsics.bitand32 = (a, b) => { Intrinsics.OF = 0; return a & b }
Intrinsics.bitxor32 = (a, b) => { Intrinsics.OF = 0; return a ^ b }
Intrinsics.shl32 = (a, b) => { 
    const raw = a << b
    Intrinsics.OF = 0 // ((a ^ raw) < 0) ? 1 : 0
    return raw
}
Intrinsics.shr32 = (a, b) => { Intrinsics.OF = 0; return a >> b }
Intrinsics.shrl32 = (a, b) => { Intrinsics.OF = 0; return a >>> b }

Intrinsics.add64 = (a, b) => a + b
Intrinsics.sub64 = (a, b) => a - b
Intrinsics.mul64 = (a, b) => a * b
Intrinsics.mod64 = (a, b) => a % b
Intrinsics.pow64 = (a, b) => a ** b
Intrinsics.bitor64 = Intrinsics.bitori64
Intrinsics.bitand64 = Intrinsics.bitandi64
Intrinsics.bitxor64 = Intrinsics.bitxori64
Intrinsics.shl64 = Intrinsics.shli64
Intrinsics.shr64 = Intrinsics.shri64   
Intrinsics.shrl64 = Intrinsics.shrli64

Intrinsics.addf64 = (a, b) => a + b
Intrinsics.subf64 = (a, b) => a - b
Intrinsics.mulf64 = (a, b) => a * b
Intrinsics.divf64 = (a, b) => a / b
Intrinsics.modf64 = (a, b) => a % b
Intrinsics.powf64 = (a, b) => a ** b

Intrinsics.cmp32gt = (a, b) => a > b
Intrinsics.cmp32lt = (a, b) => a < b
Intrinsics.cmp64lt = Intrinsics.cmpf64lt = Intrinsics.cmp32lt 
Intrinsics.cmp64gt = Intrinsics.cmpf64gt = Intrinsics.cmp32gt

Intrinsics.cmp32gte = (a, b) => a >= b
Intrinsics.cmp32lte = (a, b) => a <= b
Intrinsics.cmp64lte = Intrinsics.cmpf64lte = Intrinsics.cmp32lte 
Intrinsics.cmp64gte = Intrinsics.cmpf64gte = Intrinsics.cmp32gte

function splitI64(v) {
    return [v >>> 0, Math.floor(v / 0x1_0000_0000) | 0]
}
function joinI64(lo, hi) {
    return hi * 0x1_0000_0000 + (lo >>> 0)
}
function shiftLeft64(v, amount) {
    amount &= 63
    const [lo, hi] = splitI64(v)
    if (amount === 0) return [lo, hi]
    if (amount < 32) return [(lo << amount) >>> 0, ((hi << amount) | (lo >>> (32 - amount))) | 0]
    return [0, (lo << (amount - 32)) | 0]
}
function shiftRightArith64(v, amount) {
    amount &= 63
    const [lo, hi] = splitI64(v)
    if (amount === 0) return [lo, hi]
    if (amount < 32) return [((lo >>> amount) | (hi << (32 - amount))) >>> 0, hi >> amount]
    return [(hi >> (amount - 32)) >>> 0, hi >> 31]
}
function shiftRightLogical64(v, amount) {
    amount &= 63
    const [lo, hiSigned] = splitI64(v)
    const hi = hiSigned >>> 0
    if (amount === 0) return [lo, hi | 0]
    if (amount < 32) return [((lo >>> amount) | (hi << (32 - amount))) >>> 0, (hi >>> amount) | 0]
    return [(hi >>> (amount - 32)) >>> 0, 0]
}
Intrinsics.bitori64 = (a, b) => { const [al, ah] = splitI64(a), [bl, bh] = splitI64(b); return joinI64(al | bl, ah | bh) }
Intrinsics.bitandi64 = (a, b) => { const [al, ah] = splitI64(a), [bl, bh] = splitI64(b); return joinI64(al & bl, ah & bh) }
Intrinsics.bitxori64 = (a, b) => { const [al, ah] = splitI64(a), [bl, bh] = splitI64(b); return joinI64(al ^ bl, ah ^ bh) }
Intrinsics.shli64 = (a, b) => joinI64(...shiftLeft64(a, b))
Intrinsics.shri64 = (a, b) => joinI64(...shiftRightArith64(a, b))
Intrinsics.shrli64 = (a, b) => joinI64(...shiftRightLogical64(a, b))
Intrinsics.bitnoti64 = (a) => { const [lo, hi] = splitI64(a); return joinI64(~lo, ~hi) }

function taggedBox(val, T) {
    const handle = Intrinsics.alloc(3)
    let m
    switch (T) {
        case Type.f64: m = HeapF64; break
        case Type.u64: m = HeapU64; break
        case Type.i64: m = HeapI64; break
        case Type.u32: m = HeapU32; break
        case Type.i32: m = HeapI32; break
        default: throw `unhandled: ${T}`
    }
    Intrinsics.store(handle, m, Type.Handle)
    Intrinsics.store(handle+4, val, T)
    return handle
}

function maybeGetNumericHeapType(handle) {
    const m = Intrinsics.load(handle, Type.Handle)
    switch (m) {
        case HeapF64: return Type.f64
        case HeapU64: return Type.u64
        case HeapI64: return Type.i64
        case HeapU32: return Type.u32
        case HeapI32: return Type.i32
    }
}

function isString(handle) {
    const m = Intrinsics.load(handle, Type.Handle)
    return getTDBase(m) === BaseTypeKind.String
}

function taggedUnbox(handle, T) {
    return Intrinsics.load(handle+4, T)
}

function handleToBool(handle) {
    if (handle === True) return true
    if (handle === False || handle === Undefined || handle === Null) return false
    const m = maybeGetNumericHeapType(handle)
    if (!m) return true
    return !!taggedUnbox(handle, m)
}

function storeHandle(dest, offset, handle) {
    Intrinsics.store(dest + offset, handle, Type.Handle)
}

// an "Object" is the only TD with conventional field descriptors
// certain Object variants are wrappers around a different base. wrapping a function uniquely preserves its "typeof" value.
const BaseTypeKind = {
    Oddball: 1, // true/false/undefined/null etc.
    String: 2,
    Number: 3,
    Symbol: 4,
    Array: 5,
    Object: 6,
    Function: 7,

    StringTable: 128,
    NativeStruct: 132,
}

const TDFlags = {
    Callable: 1 << 0,
    // anything managing data offheap will need this set
    HasDestructor: 1 << 1,
    // means an instance _might_ be ok to stack allocate
    StackAllocatable: 1 << 7,
}

// all strings are assumed utf-8 unless named otherwise
const StringVariant = {
    Inline: 1, // slot1 contains packed code units, length is extraSlots + 1. The TD _is_ the string!
    Internal: 2, // instance has the index into a string table, slot1 of the TD is the string table
    External: 3, // slot1 says if the instance is heap allocated or is a raw ptr (including static addr)
}


// two-byte "category"
// * low byte is the general kind like object/function/array/string
// * high byte is a variant
// third byte is # of extra inline slots used by this TD
// high byte is a bitset used for various reflective purposes. 
//  - expresses information that usually can be found elsewhere but is useful for general purpose logic e.g. debugging 
// 
// interpretation of the second slot depends on the above. some TDs may use extra inline slots _and_ this slot for "extra info"
// JS object TDs use the 2nd slot for the "parent" TD
function createTypeDescriptor(base, variant = 0, bitset = 0, slot1 = 0, extraSlots = 0) {
    const handle = Intrinsics.alloc(2)
    Intrinsics.store(handle, base, Type.u8)
    Intrinsics.store(handle+1, variant, Type.u8)
    Intrinsics.store(handle+2, extraSlots, Type.u8)
    Intrinsics.store(handle+3, bitset, Type.u8)
    Intrinsics.store(handle+4, slot1, Type.u32)
    return handle
}

// not created at runtime, keys are always indices the static string table
function createNativeStructTD(desc) {

}

function getTDBase(ptr) {
    return Intrinsics.load(ptr, Type.u8)
}

// const StaticString = createTypeDescriptor(BaseTypeKind.String, StringVariant.External)
const HeapString = createTypeDescriptor(BaseTypeKind.String, StringVariant.External)

function memcpy(dst, src, amt) {
    for (let i = 0; i < amt; i++) {
        const v = Intrinsics.load(src+i, Type.u8)
        Intrinsics.store(dst+i, v, Type.u8)
    }
}

function allocString(buf, len) {
    const slots = (len >> 2)+2 // (TD + len)
    const dest = Intrinsics.alloc(slots)
    storeHandle(dest, 0, HeapString)
    Intrinsics.store(dest+4, len, Type.u32)
    memcpy(dest+8, buf, len)
    return dest
}

// assumes HeapString
function streql(a, b) {
    if (a === b) return true
    const l1 = Intrinsics.load(a+4, Type.u32)
    const l2 = Intrinsics.load(b+4, Type.u32)
    if (l1 !== l2) return false
    const s1 = a+8
    const s2 = b+8
    for (let i = 0; i < l1; i++) {
        const v1 = Intrinsics.load(s1+i, Type.u8)
        const v2 = Intrinsics.load(s2+i, Type.u8)
        if (v1 !== v2) return false
    }
    return true
}

function strconcat(a, b) {
    const l1 = Intrinsics.load(a+4, Type.u32)
    const l2 = Intrinsics.load(b+4, Type.u32)
    const t = l1 + l2
    const c = Intrinsics.stackalloc(t)
    memcpy(c, a+8, l1)
    memcpy(c+l1, b+8, l2)
    const r = allocString(c, t)
    Intrinsics.stackfree(c, Type.u8)
    return r
}

// fully dynamic
const DynamicObject = createTypeDescriptor(BaseTypeKind.Object, 0) 

const HeapMapOffsets = {
    count: 0,
    capacity: 4,
    entries: 8,
}

function mapEntryKey(entries, i) { return Intrinsics.load(entries + i * 8, Type.u32) }
function mapEntryVal(entries, i) { return Intrinsics.load(entries + i * 8 + 4, Type.u32) }

function mapFindIndex(header, key) {
    const entries = Intrinsics.load(header + HeapMapOffsets.entries, Type.Handle)
    const count = Intrinsics.load(header + HeapMapOffsets.count, Type.u32)
    for (let i = 0; i < count; i++) {
        if (mapEntryKey(entries, i) === key) return i
    }
    return -1
}

class HeapMap {
    constructor(capacity = 4) {
        const header = Intrinsics.alloc(3)
        const entries = Intrinsics.alloc(capacity * 2)
        Intrinsics.store(header + HeapMapOffsets.count, 0, Type.u32)
        Intrinsics.store(header + HeapMapOffsets.capacity, capacity, Type.u32)
        storeHandle(header, HeapMapOffsets.entries, entries)
        this.header = header
    }

    has(key) {
        return mapFindIndex(this.header, key) >= 0
    }

    get(key) {
        const i = mapFindIndex(this.header, key)
        if (i < 0) return undefined
        const entries = Intrinsics.load(this.header + HeapMapOffsets.entries, Type.Handle)
        return mapEntryVal(entries, i)
    }

    set(key, value) {
        const header = this.header
        let entries = Intrinsics.load(header + HeapMapOffsets.entries, Type.Handle)

        const existing = mapFindIndex(header, key)
        if (existing >= 0) {
            Intrinsics.store(entries + existing * 8 + 4, value, Type.u32)
            return this
        }

        let count = Intrinsics.load(header + HeapMapOffsets.count, Type.u32)
        const capacity = Intrinsics.load(header + HeapMapOffsets.capacity, Type.u32)
        if (count >= capacity) {
            const newCapacity = capacity * 2
            const newEntries = Intrinsics.alloc(newCapacity * 2)
            for (let i = 0; i < count; i++) {
                Intrinsics.store(newEntries + i * 8, mapEntryKey(entries, i), Type.u32)
                Intrinsics.store(newEntries + i * 8 + 4, mapEntryVal(entries, i), Type.u32)
            }
            Intrinsics.store(header + HeapMapOffsets.capacity, newCapacity, Type.u32)
            storeHandle(header, HeapMapOffsets.entries, newEntries)
            entries = newEntries
        }

        Intrinsics.store(entries + count * 8, key, Type.u32)
        Intrinsics.store(entries + count * 8 + 4, value, Type.u32)
        Intrinsics.store(header + HeapMapOffsets.count, count + 1, Type.u32)
        return this
    }
}

class BytecodeFunction {
    constructor({ name = '<anonymous>', paramCount = 0, registerCount, code, constants, maxWidth = 1 }) {
        this.name = name
        this.paramCount = paramCount
        this.registerCount = registerCount
        this.code = codeToArrayBuffer(code)
        this.constants = constants
        this.maxWidth = maxWidth
    }
}

function codeToArrayBuffer(code) {
    let len = 0
    for (const instr of code) len += instr.length
    const buf = new ArrayBuffer(len)
    const view = new Uint8Array(buf)
    let i = 0
    for (const instr of code) {
        for (const b of instr) view[i++] = b
    }
    return view
}

function storeReg(regs, idx, acc, accType) {
    if (accType === Type.Smi) { regs[idx] = acc; return }
    if (accType === Type.Handle) { regs[idx] = (acc << 1) | 1; return }
    regs[idx] = (taggedBox(acc, accType) << 1) | 1
}


function loadReg(regs, idx, regCount) {
    const raw = regs[idx]
    if (raw & 1) { regs[regCount] = Type.Handle; return raw >> 1 }
    regs[regCount] = Type.Smi
    return raw
}

function signExtendSmi1Byte(v) {
    return (Intrinsics.as(v, Type.i32) << 24) >> 23
}

function signExtendSmi2Byte(v) {
    return (Intrinsics.as(v, Type.i32) << 16) >> 15
}

let isNativeSmall
function getNativeEndianness() {
    if (isNativeSmall !== undefined) return isNativeSmall
    const arr = new Uint16Array(1)
    arr[0] = 1
    const v = (new Uint8Array(arr.buffer, arr.byteOffset, arr.byteLength))[0]
    return isNativeSmall = (v === 1)
}

Intrinsics.getNativeEndianness = getNativeEndianness

let currentEh = 0 // u32 stack handle
function registerEh(pc, frameIdx) {
    const saveEh = currentEh
    const handle = Intrinsics.stackalloc(12)
    Intrinsics.store(handle, saveEh, Type.u32)
    Intrinsics.store(handle+4, pc, Type.u32)
    Intrinsics.store(handle+8, frameIdx << 1, Type.u32)
    currentEh = handle
}

function interpret(fn, thisArg, args, instrumentCb) {
    const frames = []
    let fp = 0
    let code = fn.code
    let constants = fn.constants
    let regCount = fn.registerCount

    let regs = Intrinsics.stackalloc(regCount + 1, Type.i32)
    {
        const argc = fn.paramCount
        for (let i = 0; i < argc; i++) regs[i] = args[i]
    }

    let view = fn.maxWidth > 1 ? new DataView(code.buffer, code.byteOffset, code.byteLength) : undefined

    let acc = Intrinsics.as(0, Type.i64)
    let pc = Intrinsics.as(0, Type.u32)
    pc = 0 // XXX: `as` is treated as opaque

    for (;;) {
        const instr = code[pc++]
        switch (instr) {
            case Op.Mov: regs[code[pc+2]] = regs[code[pc+1]]; pc += 2; break
            case Op.Star: storeReg(regs, code[pc++], acc, regs[regCount]); break
            case Op.Star0: storeReg(regs, 0, acc, regs[regCount]); break
            case Op.Star1: storeReg(regs, 1, acc, regs[regCount]); break
            case Op.Star2: storeReg(regs, 2, acc, regs[regCount]); break
            case Op.Star3: storeReg(regs, 3, acc, regs[regCount]); break
            case Op.Star4: storeReg(regs, 4, acc, regs[regCount]); break
            case Op.Star5: storeReg(regs, 5, acc, regs[regCount]); break
            case Op.Star6: storeReg(regs, 6, acc, regs[regCount]); break
            case Op.Star7: storeReg(regs, 7, acc, regs[regCount]); break

            case Op.LdaConst: acc = constants[code[pc++]]; regs[regCount] = Type.Handle; break
            case Op.LdaImm: acc = signExtendSmi1Byte(code[pc++]); regs[regCount] = Type.Smi; break
            case Op.LdaUndefined: acc = Undefined; regs[regCount] = Type.Handle; break
            case Op.LdaNull: acc = Null; regs[regCount] = Type.Handle; break
            case Op.LdaTrue: acc = True; regs[regCount] = Type.Handle; break
            case Op.LdaFalse: acc = False; regs[regCount] = Type.Handle; break
            case Op.LdaZero: acc = 0; regs[regCount] = Type.Smi; break

            case Op.Ldar: acc = loadReg(regs, code[pc++], regCount); break
            case Op.Ldar0: acc = loadReg(regs, 0, regCount); break
            case Op.Ldar1: acc = loadReg(regs, 1, regCount); break
            case Op.Ldar2: acc = loadReg(regs, 2, regCount); break
            case Op.Ldar3: acc = loadReg(regs, 3, regCount); break

            case Op.Add: { acc = binOp('add', acc, regs[regCount], regs[code[pc++]]); regs[regCount] = Intrinsics.accType; break }
            case Op.Sub: { acc = binOp('sub', acc, regs[regCount], regs[code[pc++]]); regs[regCount] = Intrinsics.accType; break }
            case Op.Mul: { acc = binOp('mul', acc, regs[regCount], regs[code[pc++]]); regs[regCount] = Intrinsics.accType; break }
            case Op.Div: { acc = divOp(acc, regs[regCount], regs[code[pc++]]); regs[regCount] = Intrinsics.accType; break }
            case Op.Mod: { acc = binOp('mod', acc, regs[regCount], regs[code[pc++]]); regs[regCount] = Intrinsics.accType; break }
            case Op.Exp: { acc = binOp('pow', acc, regs[regCount], regs[code[pc++]]); regs[regCount] = Intrinsics.accType; break }
            case Op.BitOr: { acc = binOp('bitor', acc, regs[regCount], regs[code[pc++]]); regs[regCount] = Intrinsics.accType; break }
            case Op.BitAnd: { acc = binOp('bitand', acc, regs[regCount], regs[code[pc++]]); regs[regCount] = Intrinsics.accType; break }
            case Op.BitXor: { acc = binOp('bitxor', acc, regs[regCount], regs[code[pc++]]); regs[regCount] = Intrinsics.accType; break }
            case Op.ShiftLeft: { acc = binOp('shl', acc, regs[regCount], regs[code[pc++]]); regs[regCount] = Intrinsics.accType; break }
            case Op.ShiftRight: { acc = shiftRightOp(acc, regs[regCount], regs[code[pc++]]); regs[regCount] = Intrinsics.accType; break }
            case Op.ShiftRightLogical: { acc = binOp('shrl', acc, regs[regCount], regs[code[pc++]]); regs[regCount] = Intrinsics.accType; break }

            case Op.TestEqual: throw new Error('==: not implemented')
            case Op.TestStrictEqual: {
                const rawB = regs[code[pc++]]
                assertCurrentRegValue(rawB)
                let braw, bT
                if ((rawB & 1) === 0) { braw = rawB; bT = Type.Smi } else { braw = rawB >> 1; bT = Type.Handle }
                if (regs[regCount] === Type.Handle || bT === Type.Handle) throw new Error('TestStrictEqual: not implemented for Handle operands')
                const eq = (regs[regCount] === bT) && (acc === braw)
                acc = eq ? True : False; regs[regCount] = Type.Handle
                break
            }
            case Op.TestLessThan: { const r = compareOp(0, acc, regs[regCount], regs[code[pc++]]); acc = r ? True : False; regs[regCount] = Type.Handle; break }
            case Op.TestGreaterThan: { const r = compareOp(1, acc, regs[regCount], regs[code[pc++]]); acc = r ? True : False; regs[regCount] = Type.Handle; break }
            case Op.TestLessThanOrEqual: { const r = compareOp(2, acc, regs[regCount], regs[code[pc++]]); acc = r ? True : False; regs[regCount] = Type.Handle; break }
            case Op.TestGreaterThanOrEqual: { const r = compareOp(3, acc, regs[regCount], regs[code[pc++]]); acc = r ? True : False; regs[regCount] = Type.Handle; break }

            case Op.TestIn: throw new Error('TestIn: not implemented')
            case Op.TestInstanceOf: throw new Error('TestInstanceOf: not implemented')

            case Op.Inc: {
                const T = regs[regCount]
                if (T === Type.Smi) {
                    const z = acc
                    acc = Intrinsics.add32(z, 1 << 1)
                    if (Intrinsics.checkoverflow()) {
                        acc = Intrinsics.add64(z / 2, 1)
                        regs[regCount] = Type.u64
                    }
                    break
                } else {
                    acc = binOpDirect('add', acc, T, 1 << 1, Type.Smi) 
                }
                regs[regCount] = Intrinsics.accType; 
                break 
            }
            case Op.Dec: { acc = binOpDirect('sub', acc, regs[regCount], 1 << 1, Type.Smi); regs[regCount] = Intrinsics.accType; break }
            case Op.Negate: { acc = binOpDirect('sub', 0, Type.Smi, acc, regs[regCount]); regs[regCount] = Intrinsics.accType; break }
            case Op.BitNot: {
                const T = regs[regCount]
                const a = resolveNumeric(acc, T); const rT = resolvedType
                const raw = rT === Type.Smi
                    ? Intrinsics.bitxor32(a, -1)
                    : Intrinsics.bitnoti64(Intrinsics.convert(a, rT, Type.i64))
                acc = raw; regs[regCount] = narrowSmi(raw, rT === Type.Smi ? Type.Smi : Type.i64)
                break
            }
            case Op.LogicalNot: {
                const T = regs[regCount]
                const b = T === Type.Handle ? handleToBool(acc) : !!acc
                acc = b ? False : True; regs[regCount] = Type.Handle
                break
            }
            case Op.TypeOf: throw new Error('TypeOf: not implemented')

            case Op.Jump: pc = code[pc]; break
            case Op.JumpIfToBooleanTrue: {
                const cond = regs[regCount] === Type.Handle ? handleToBool(acc) : !!acc
                const target = code[pc++]
                pc = Intrinsics.select(cond, target, pc)
                break
            }
            case Op.JumpIfToBooleanFalse:  {
                const cond = regs[regCount] === Type.Handle ? handleToBool(acc) : !!acc
                const target = code[pc++]
                pc = Intrinsics.select(cond, pc, target)
                break
            }
            // case Op.JumpIfNull: pc = (acc === Null) ? instr[1] : pc + 1; break
            // case Op.JumpIfUndefined: pc = (acc === Undefined) ? instr[1] : pc + 1; break

            case Op.Call: throw "todo"

            case Op.CallConst: {
                const k = code[pc++]
                const rargStart = code[pc++]
                const argCount = code[pc++]
                const target = constants[k]
                const newRegCount = target.registerCount
                const newFp = stackPointer
                const newRegs = Intrinsics.stackalloc(newRegCount + 1, Type.i32)
                for (let i = 0; i < argCount; i++) newRegs[i] = regs[rargStart+i]
                frames.push([pc, fn, regs, fp])
                regCount = newRegCount
                constants = target.constants
                regs = newRegs
                code = target.code
                view = target.maxWidth > 1 ? new DataView(code.buffer, code.byteOffset, code.byteLength) : undefined
                pc = 0
                fp = newFp
                break
            }

            case Op.Return: {
                Intrinsics.accType = regs[regCount]
                stackPointer = fp
                if (fp) {
                    const f = frames.pop()
                    pc = f[0]
                    fn = f[1]
                    regs = f[2]
                    fp = f[3]
                    code = fn.code
                    constants = fn.constants
                    regCount = fn.registerCount
                    view = fn.maxWidth > 1 ? new DataView(code.buffer, code.byteOffset, code.byteLength) : undefined
                    regs[regCount] = Intrinsics.accType
                    continue
                }
                return acc
            }

            case Op.Throw: {
                if (!currentEh) {
                    throw "ohhhh nooooo"
                }
                const saveEh = Intrinsics.load(currentEh, Type.u32)
                const targetPc = Intrinsics.load(currentEh+4, Type.u32)
                const frameIdx = Intrinsics.load(currentEh+8, Type.u32)
                Intrinsics.store(currentEh+8, frameIdx | 1, Type.u32)
                const actualIdx = frameIdx >> 1
                let f
                while (frames.length > actualIdx) {
                    f = frames.pop() // TODO: record frames
                }
                pc = targetPc
                if (f) {
                    fn = f[1]
                    regs = f[2]
                    fp = f[3]
                    code = fn.code
                    constants = fn.constants
                    regCount = fn.registerCount
                    view = fn.maxWidth > 1 ? new DataView(code.buffer, code.byteOffset, code.byteLength) : undefined     
                }
                break
            }

            case Op.CatchEnter: {
                const offset = code[pc++]
                registerEh(pc+offset, frames.length)
                break
            }

            case Op.CatchExit: {
                const offset = code[pc++]
                const saveEh = Intrinsics.load(currentEh, Type.u32)
                const frameIdx = Intrinsics.load(currentEh+8, Type.u32)
                Intrinsics.stackfree(3, Type.u32)
                currentEh = saveEh
                const skipCatchBlock = !(frameIdx & 1)
                pc = Intrinsics.select(skipCatchBlock, pc+offset, pc)
                break
            }

            case Op.GetProperty: throw new Error('GetProperty: not implemented')
            case Op.SetProperty: throw new Error('SetProperty: not implemented')
            case Op.DeleteProperty: throw new Error('DeleteProperty: not implemented')
            case Op.CreateEmptyObject: throw new Error('CreateEmptyObject: not implemented')
            case Op.CreateEmptyArray: throw new Error('CreateEmptyArray: not implemented')

            case Op.Width2: {
                const next = code[pc++]
                switch (next) {
                    case Op.LdaImm:
                        acc = signExtendSmi2Byte(view.getInt16(pc, Intrinsics.getNativeEndianness())); regs[regCount] = Type.Smi; pc += 2;
                        break
                    case Op.LdaConst:
                        acc = constants[view.getUint16(pc, Intrinsics.getNativeEndianness())]; regs[regCount] = Type.Handle; pc += 2;
                        break
                    case Op.Jump:
                        pc = view.getUint16(pc, Intrinsics.getNativeEndianness());
                        break
                    default: throw `todo 2 byte op: ${next}`
                }
                break
            }

            case Op.Assert: {
                const kind = code[pc++]
                switch (kind) {
                    case AssertionKind.AccumulatorIs: {
                        assertIs(acc, code[pc++], regs[regCount])
                        break
                    }
                    case AssertionKind.AccumulatorIsNot: {
                        assertIsNot(acc, code[pc++], regs[regCount])
                        break
                    }
                    case AssertionKind.NextRegisterIs: {
                        hasPendingRegisterAssertion = 1
                        pendingRegisterAssertionType = code[pc++]
                        break
                    }
                    case AssertionKind.Instrument: {
                        function setPc(val) { pc = val }
                        function getPc() { return pc }
                        function getRegs() { return regs }
                        function getAcc() { return acc }
                        function getFn() { return fn }
                        instrumentCb({
                            setPc,
                            getPc,
                            getAcc,
                            getRegs,
                            getFn,
                            frames,
                        })
                        break
                    }
                    default: throw `unknown assertion kind: ${kind}`
                }
            }

            default:
                throw new Error(`unknown opcode: ${instr}`)
        }
    }
}

function makeConstants(values = []) {
    return values
}

const addFn = new BytecodeFunction({
    name: 'add',
    paramCount: 2,
    registerCount: 2,
    constants: makeConstants(),
    code: [
        [Op.Ldar, 0],   // acc = a
        [Op.Add, 1],    // acc = acc + b
        [Op.Return],
    ],
})

// while -> do/while transform?
// function sumTo(n) { let acc = 0; let i = 0; while (i < n) { acc += i; i += 1 } return acc }
const sumToFn2 = new BytecodeFunction({
    name: 'sumTo',
    paramCount: 1,   // r0 = n
    registerCount: 3, // r0 = n, r1 = acc, r2 = i
    constants: makeConstants(),
    code: [
        /*  0 */ [Op.LdaZero],
        /*  1 */ [Op.Star1],              // acc = 0
        /*  2 */ [Op.Star2],              // i = 0
        /*  3 */ [Op.Ldar2],              // loop: acc(reg) = i
        /*  4 */ [Op.TestLessThan, 0],    // acc = i < n
        /*  5 */ [Op.JumpIfToBooleanFalse, 17],
        /*  6 */ [Op.Ldar1],
        /*  7 */ [Op.Add, 2],             // acc = acc(local) + i
        /*  8 */ [Op.Star1],              // acc(local) = acc
        /*  9 */ [Op.Ldar2],
        /* 10 */ [Op.Inc],                // acc = i + 1
        /* 11 */ [Op.Star2],              // i = acc
        /* 12 */ [Op.Jump, 3],
        /* 13 */ [Op.Ldar1],
        /* 14 */ [Op.Return],
    ],
})

const sumToManyFn3 = new BytecodeFunction({
    name: 'sumToMany',
    paramCount: 2,   // r0 = n, r1 = n2
    registerCount: 4, // r2 = acc, r3 = i
    constants: makeConstants([sumToFn2]),
    code: [
        /*  0 */ [Op.LdaZero],
        /*  1 */ [Op.Star2],              // acc = 0
        /*  2 */ [Op.Star3],              // i = 0
        /*  3 */ [Op.Ldar2],              // loop: acc(reg) = i
        /*  4 */ [Op.CallConst, 0, 1, 1], // acc = acc(local) + i
        /* 5 */ [Op.Add, 2],            
        /* 6 */ [Op.Star2],             
        /* 7 */ [Op.Ldar3],
        /* 8 */ [Op.Inc],                 // acc = i + 1
        /* 9 */ [Op.Star3],               // i = acc
        /* 11 */ [Op.TestLessThan, 0],    // acc = i < n
        /* 12 */ [Op.JumpIfToBooleanTrue, 3],
        /* 13 */ [Op.Ldar2],
        /* 14 */ [Op.Return],
    ],
})

const tryThrowFn = new BytecodeFunction({
    name: 'tryThrow',
    paramCount: 0,
    registerCount: 2,   // r0 unused, r1 = caught error
    constants: makeConstants(),
    code: [
        /* byte  0 */ [Op.CatchEnter, 3],  // EH target -> CatchExit @ byte 5
        /* byte  2 */ [Op.LdaImm, 7],      // acc = 7 (error)
        /* byte  4 */ [Op.Throw],          // throw acc
        /* byte  5 */ [Op.CatchExit, 6],   // no error -> skip to byte 13
        /* byte  7 */ [Op.Star1],          // catch: r1 = e (= 7)
        /* byte  8 */ [Op.LdaImm, 100],    // acc = 100
        /* byte 10 */ [Op.Add, 1],         // acc = 100 + r1 = 107
        /* byte 12 */ [Op.Return],
    ],
})

const tryNoThrowFn = new BytecodeFunction({
    name: 'tryNoThrow',
    paramCount: 0,
    registerCount: 1,
    constants: makeConstants(),
    code: [
        /* byte  0 */ [Op.CatchEnter, 2],  // EH target -> CatchExit @ byte 4
        /* byte  2 */ [Op.LdaImm, 5],      // acc = 5 (no throw)
        /* byte  4 */ [Op.CatchExit, 3],   // no error -> skip to byte 9
        /* byte  6 */ [Op.LdaImm, 99],     // catch handler (skipped)
        /* byte  8 */ [Op.Return],
        /* byte  9 */ [Op.Return],         // normal path: return acc (= 5)
    ],
})

const throwerFn = new BytecodeFunction({
    name: 'thrower',
    paramCount: 0,
    registerCount: 1,
    constants: makeConstants(),
    code: [
        /* byte 0 */ [Op.LdaImm, 7],   // acc = 7 (error)
        /* byte 2 */ [Op.Throw],       // no local EH -> unwinds to caller
    ],
})

const tryCallFn = new BytecodeFunction({
    name: 'tryCall',
    paramCount: 0,
    registerCount: 2,   // r1 = caught error
    constants: makeConstants([throwerFn]),
    code: [
        /* byte  0 */ [Op.CatchEnter, 4],     // EH target -> CatchExit @ byte 6
        /* byte  2 */ [Op.CallConst, 0, 0, 0], // thrower()  (k=0, rargStart=0, argc=0)
        /* byte  6 */ [Op.CatchExit, 5],      // no error -> skip to byte 14
        /* byte  8 */ [Op.Star1],             // catch: r1 = e (= 7)
        /* byte  9 */ [Op.LdaImm, 100],       // acc = 100
        /* byte 11 */ [Op.Add, 1],            // acc = 100 + r1 = 107
        /* byte 13 */ [Op.Return],
    ],
})

function tryCatchDemo() {
    const caught = interpret(tryThrowFn, undefined, [])
    console.log('tryThrow() =', caught >> 1, '(expected 107)')
    const normal = interpret(tryNoThrowFn, undefined, [])
    console.log('tryNoThrow() =', normal >> 1, '(expected 5)')
    const crossFrame = interpret(tryCallFn, undefined, [])
    console.log('tryCall() =', crossFrame >> 1, '(expected 107)')
}
tryCatchDemo()

function bench(f, c = 100_000) {
    const p = performance.now()
    for (let i = 0; i < c; i++) {
        f()
    }
    return performance.now() - p
}


function sumTo4(n) {
    let acc = 0; let i = 0; while (i < n) { 
        acc += i; i += 1 
    } return acc 
}


let x = 0
function sumTo3(n) {
    let acc = 0; let i = 0; while (i < n) { 
        acc += i; i += 1 
        x += 1
        if (x === acc) x = 0
    } return acc 
}

function sumToMany4(m, n) {
    x = 0
    let total = 0
    for (let i = 0; i < m; i++) {
        total += sumTo3(n)
        x = 0
    }
    return total
}

function sumToMany5(m, n) {
    let total = 0
    for (let i = 0; i < m; i++) {
        total += sumTo4(n)
    }
    return total
}


function runExamples() {
    console.log(sumTo3(5))
    const r = interpret(sumToFn2, undefined, [5 << 1])
    console.log('x sumTo2(5) =', r, Intrinsics.accType)
    const args1 = new Int32Array([15 << 1])
    console.log(bench(() => interpret(sumToFn2, undefined, args1)))
    console.log(bench(() => interpret(sumToFn2, undefined, args1)))

    const args2 = new Int32Array([3 << 1, 5 << 1])
    console.log(bench(() => interpret(sumToManyFn3, undefined, args2)))
    console.log(bench(() => interpret(sumToManyFn3, undefined, args2)))

    // console.log(bench(() => interpret2(sumToFn2, undefined, [150 << 1])))
    // console.log(bench(() => interpret2(sumToFn2, undefined, [150 << 1])))

    console.log(bench(() => sumTo3(15)))
    console.log(bench(() => sumTo3(15)))
    console.log(bench(() => sumTo4(15)))
    console.log(bench(() => sumTo4(15)))

    args2[0] = 100_000 << 1
    args2[1] = 15 << 1
    console.log(bench(() => interpret(sumToManyFn3, undefined, args2), 1))


    console.log(bench(() => sumToMany4(100_000, 150), 1))
    console.log(bench(() => sumToMany4(100_000, 150), 1))

        console.log(bench(() => sumToMany5(100_000, 15), 1))
    console.log(bench(() => sumToMany5(100_000, 15), 1))

}

runExamples()

function heapDemo() {
    const MAP_POINT = 42
    const cell = Intrinsics.alloc(4)
    Intrinsics.store(cell, MAP_POINT, Type.u32)
    Intrinsics.store(cell + 1 * slotBytes, 7, Type.i32)
    Intrinsics.store(cell + 2 * slotBytes, 3.5, Type.f64)
    console.log('heap cell map =', Intrinsics.load(cell, Type.u32))
    console.log('heap cell x =', Intrinsics.load(cell + 1 * slotBytes, Type.i32))
    console.log('heap cell y =', Intrinsics.load(cell + 2 * slotBytes, Type.f64))
}
heapDemo()

function mapDemo() {
    const m = new HeapMap(2) // start small so we exercise a grow
    m.set(10, 100).set(20, 200).set(30, 300) // 3rd insert forces capacity 2 -> 4
    console.log('map has 20 =', m.has(20), '(expected true)')
    console.log('map has 99 =', m.has(99), '(expected false)')
    console.log('map get 10 =', m.get(10), '(expected 100)')
    console.log('map get 30 =', m.get(30), '(expected 300)')
    console.log('map get 99 =', m.get(99), '(expected undefined)')
    m.set(20, 999) // update in place
    console.log('map get 20 after update =', m.get(20), '(expected 999)')
}
mapDemo()

function boxingDemo2() {
    function tagf64(v) { return (taggedBox(v, Type.f64) << 1) | 1 }
    function tagSmi(v) { return v << 1 }

    Intrinsics.accType = 0
    console.log('interpret2 add(2.5, 2.5) =', interpret(addFn, undefined, [tagf64(2.5), tagf64(2.5)]), TypeName[Intrinsics.accType])
    console.log('interpret2 add(2.5, 1) =', interpret(addFn, undefined, [tagf64(2.5), tagSmi(1)]), TypeName[Intrinsics.accType])
    console.log('interpret2 add(1e300, 1e300) =', interpret(addFn, undefined, [tagf64(1e300), tagf64(1e300)]), TypeName[Intrinsics.accType])

    const big = SMI_MAX
    const res = interpret(addFn, undefined, [tagSmi(big), tagSmi(big)])
    console.log(`interpret2 add(${big}, ${big}) =`, res, TypeName[Intrinsics.accType], '(expected value', big + big, '-- widened to i64/u64)')

    const negateFn = new BytecodeFunction({
        name: 'negate', paramCount: 1, registerCount: 1, constants: makeConstants(),
        code: [[Op.Ldar, 0], [Op.Negate], [Op.Return]],
    })
    const negRaw = interpret(negateFn, undefined, [tagSmi(5)])
    console.log('interpret2 negate(5) =', negRaw >> 1, TypeName[Intrinsics.accType], '(expected -5 Smi)')
}
boxingDemo2()

function f(x, y) {
    for (let i = 0; i < 12; i++) {
        x = x * y
    }
    // v8 stores f64 on heap
    // --interpreted_frames_native_stack --print_all_exceptions
    throw new Error('')
    return x
}
