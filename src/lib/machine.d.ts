/**
 * A fixed-width machine integer type.
 * @param Width The bit width of the integer, e.g. `8`, `16`, `32`, `64`.
 * @param Signed Whether the integer is signed (`true`) or unsigned (`false`).
 */
type Int<Width extends number, Signed extends boolean> = intrinsic;

/**
 * A fixed-width machine floating-point type.
 * @param Width The bit width of the float, e.g. `16`, `32`, `64`.
 */
type Float<Width extends number> = intrinsic;

// unsigned
type u8 = Int<8, false>;
type u16 = Int<16, false>;
type u32 = Int<32, false>;
type u64 = Int<64, false>;

// signed
type i8 = Int<8, true>;
type i16 = Int<16, true>;
type i32 = Int<32, true>;
type i64 = Int<64, true>;

// floats
type f16 = Float<16>;
type f32 = Float<32>;
type f64 = Float<64>;

// environment dependent (TODO: use LookupType to make these configurable)
// LookupType<"SmiWidth", 31>
// LookupType<"NativeWidth", 64>
type smi = Int<31, true>;
type usize = Int<64, false>;
type isize = Int<64, true>;

declare namespace Type {
    function isFloat(t: Type): boolean
    function isSigned(t: Type): boolean | undefined
    function isInteger(t: Type): boolean
    function getBitWidth(t: Type): u32 | undefined
}
