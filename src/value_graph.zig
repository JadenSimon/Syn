const std = @import("std");
const getAllocator = @import("./string_immutable.zig").getAllocator;
const debugPrint = @import("./parser.zig").debugPrint;
const BumpAllocator = @import("./parser.zig").BumpAllocator;

// SPECIFIC TERMINOLOGY:
// * parse - decodes bytes into a _potentially_ usable structure. Used interchangeably with "decoding".
// * reconstitution - converting the parsed bytes into a form relevant for a program. This does not imply execution, but can be the potential for execution.
// * semantic error - an error caused by a technically valid value graph but cannot be reconstituted

// 1 byte type tag
// lower 4 bits are the kind, NUL (zero) is used as a terminator and so is not a type tag
//
//
// when no size bits are set, all non-zero width types are zero terminated
// refs are always relative byte offsets (signed) *from the current type tag position*, pointing to the type tag of the chosen value. identity semantics.
//   - example: a 2-byte sized ref (tag: 0xD7) encoding -0x0010 at offset 0x25 moves the decode cursor to 0x28 and points to 0x15.
// refs are NOT meant for deduplication/compression. while they can be used for this, they exist to represent object graphs.
// number/ref are parsed as an ASCII string when zero terminated
//  - malformed zero-terminated refs/numbers MUST fail during parse
//  - with the exception of hex numbers, **follow JSON number parsing rules**
//  - for hex numbers, the prefix must be EXACTLY `0x` or `-0x` (for negation). case insensitive thereafter. you can use the following regex for validity: `^-?0x[0-9a-zA-Z]+$`
// out-of-range refs are not parse errors, but they MUST result in an error during reconstitution
//  - this is because it's possible for a larger system to stitch together many parsed value graphs to produce something that can be reconstituted
//
// unexpected EOF -> abort
// parsers stop exactly at the terminator or declared width; subsequent bytes belong to the enclosing value.
// strings containing a zero byte MUST be encoded as sized; there is no escaping mechanism.

// arrays enumerate their elements
// objects enumerate their entries (key -> value -> key -> value -> ...)
//  - hitting a zero byte or the declared byte width before completing a pair is a parse error
// computations are two values: the "subject" and the "input". These may be any value.
//  - A computation is already a perfectly valid value on its own. Evaluation is something a consumer chooses to do, not something demanded by the format.
//
// for sized arrays and objects, decoders MUST check that the end of the trailing value is aligned with the expected width
//  - decoders do not need to track bounds during recursion, and it is often easier to check after parsing the last expected value.
//
// keys are normal values, so all possible types are valid to parse even if semantically errors.
// duplicate keys are not parse errors
// encountering an unexpected zero byte is an immediate parse error
// when any size bit is set, we say that the value is "sized"

// for all potential semantic errors, a decoder MAY choose to fail early
// if it knows the consumer would reject them as well i.e. via explicit config

// size bits for true/false/null
// xxxx - unused

// for strings/array/object/computed, size bit encoding:
// 0000 - zero terminated
// 0001 - 0 byte width imm
// 0010 - 1 byte width imm
// 0011 - 2 byte width imm
// 0100 - 3 byte width imm
// 0101 - 4 byte width imm
// 0110 - 5 byte width imm
// 0111 - 6 byte width imm
// 1000 - 7 byte width imm
// 1001 - 8 byte width imm
// 1010 - 9 byte width imm
// 1011 - 10 byte width imm
// 1100 - 1 byte length field
// 1101 - 2 byte length field
// 1110 - 4 byte length field
// 1111 - 8 byte length field

// for NUMBERS
// 0000 - zero terminated
// 0001 - -1 imm
// 0010 - 0 imm
// 0011 - 1 imm
// 0100 - 2 imm
// 0101 - 3 imm
// 0110 - 4 imm
// 0111 - u8
// 1000 - u16
// 1001 - u32
// 1010 - i8
// 1011 - i16
// 1100 - i32
// 1101 - f16
// 1110 - f32
// 1111 - f64

// for REFS
// 0000 - zero terminated
// 0001 - -1 imm
// 0010 - -2 imm
// 0011 - -4 imm
// 0100 - -8 imm
// 0101 - -16 imm
// 0110 - -32 imm
// 0111 - -64 imm
// 1000 - -128 imm
// 1001 - -256 imm
// 1010 - -512 imm
// 1011 - -1024 imm
// 1100 - 1 byte (i8)
// 1101 - 2 byte (i16)
// 1110 - 4 byte (i32)
// 1111 - 8 byte (i64)

// there is no u64/i64 encoding, use zero terminated or f64
// decoder starts at byte offset 0 unless told otherwise out-of-band. bytes unreachable from the root are never looked at.
// non-numeric sized types ALWAYS specify the width of the payload in **bytes** following the tag and any width bytes
//  - example: 0x25:: 0xC9 0x10 ... ---> object value ends at 0x37 = 0x25 (start) + 0x01 (tag) + 0x01 (width byte) + 0x10 (width)

// ALWAYS LITTLE ENDIAN

// String payloads MUST be valid UTF-8.

// unknown kinds MUST produce a parse error
// true/false/null MUST have their size nibble set to 0, otherwise it is a parse error

// for all types, including zero-terminated numbers/refs, a decoder that would otherwise overflow MUST report a parse error.
//  - a decoder is not required to handle all data widths i.e. a 32bit-only decoder may reject 8-byte refs.

const Kind = enum(u4) {
    NUL = 0,
    undefined = 1,
    null = 2,
    true = 3,
    false = 4,
    string = 5,
    number = 6,
    ref = 7,
    array = 8,
    object = 9,
    computed = 10,
};

const NumberType = enum(u4) {
    unsigned,
    signed,
    float,
};

pub const ValueNode = packed struct {
    kind: Kind,
    next: u28 = 0,
    slot0: u32 = 0,
    slot1: u32 = 0,
    slot2: u32 = 0,
};
pub const ValueRef = u32;

const ValueParser = struct {
    bytes: []const u8,
    cursor: u64 = 0,
    nodes: BumpAllocator(ValueNode),
    root: ValueRef = 0,
    prev: ValueRef = 0,
    pos_to_ref: std.AutoHashMapUnmanaged(u64, ValueRef) = .{},

    pub fn parse(bytes: []const u8) !@This() {
        var t = @This(){
            .bytes = bytes,
            .nodes = BumpAllocator(ValueNode).init(getAllocator(), std.heap.page_allocator),
        };
        try t.nodes.preAlloc();
        _ = try t.nodes.push(.{});
        t.root = try t._next();
        return t;
    }

    fn parseStringZ(this: *@This()) ValueNode {
        const start = this.cursor;
        while (true) {
            if (this.bytes[this.cursor] == 0) {
                this.cursor += 1;
                break;
            }
            this.cursor += 1;
        }
        const slice = this.bytes[start .. this.cursor - 1];
        return .{
            .kind = .string,
            .slot0 = @truncate(@intFromPtr(slice.ptr)),
            .slot1 = @truncate(@intFromPtr(slice.ptr) >> 32),
            .slot2 = @truncate(slice.len),
        };
    }

    fn parseString(this: *@This()) ValueNode {
        const tag = this.parseTag();
        if (tag >> 4 == 0) return this.parseStringZ();
        const u: u64 = blk: {
            if (getImmediate(tag)) |v| {
                break :blk @intCast(v);
            }
            const c = getLengthByteCount(tag) orelse unreachable;
            break :blk this.readBytesAs(c, u64);
        };
        const start = this.cursor;
        this.cursor += u;
        const slice = this.bytes[start..this.cursor];
        return .{
            .kind = .string,
            .slot0 = @truncate(@intFromPtr(slice.ptr) >> 32),
            .slot1 = @truncate(@intFromPtr(slice.ptr)),
            .slot2 = @truncate(slice.len),
        };
    }

    pub inline fn getSlice(node: *const ValueNode) []const u8 {
        if (node.slot2 == 0) return &.{};
        const ptr: [*]const u8 = @ptrFromInt((@as(u64, node.slot0) << 32) | node.slot1);
        return ptr[0..node.slot2];
    }

    inline fn readBytesAs(this: *@This(), count: u8, comptime T: type) T {
        const b = this.bytes[this.cursor .. this.cursor + count];
        const v: T = switch (count) {
            1 => @bitCast(b[0..1]),
            2 => @bitCast(b[0..2]),
            4 => @bitCast(b[0..4]),
            8 => @bitCast(b[0..8]),
            else => unreachable,
        };
        this.cursor += count;
        return v;
    }

    inline fn getImmediate(tag: u8) ?i16 {
        const upper: u4 = @truncate(tag >> 4);
        return switch (@as(Kind, @enumFromInt(tag & 0xF))) {
            .string, .object, .array, .computed => switch (upper) {
                0b0001...0b1011 => upper - 1,
                else => null,
            },
            .ref => switch (upper) {
                0b0001...0b1011 => @as(i16, -1) << (upper),
                else => null,
            },
            .number => switch (upper) {
                0b0001...0b0110 => @as(i8, upper) - 2,
                else => null,
            },
            else => unreachable,
        };
    }

    inline fn getLengthByteCount(tag: u8) ?u8 {
        const upper: u4 = @truncate(tag >> 4);
        return switch (@as(Kind, @enumFromInt(tag & 0xF))) {
            .string, .object, .array, .computed, .ref => switch (upper) {
                0b1100...0b1111 => 1 << (upper - 0b1100),
                else => null,
            },
            .number => switch (upper) {
                0b0111, 0b1010 => 1,
                0b1000, 0b1011, 0b1101 => 2,
                0b1001, 0b1100, 0b1110 => 4,
                0b1111 => 8,
                else => null,
            },
            else => unreachable,
        };
    }

    inline fn parsedSizedNumber(this: @This(), tag: u8) f64 {
        std.debug.assert(@as(Kind, @enumFromInt(tag & 0xF)) == .number);
        return switch (tag >> 4) {
            0b0111 => @floatFromInt(this.readBytesAs(1, u8)),
            0b1000 => @floatFromInt(this.readBytesAs(2, u16)),
            0b1001 => @floatFromInt(this.readBytesAs(4, u32)),
            0b1010 => @floatFromInt(this.readBytesAs(1, i8)),
            0b1011 => @floatFromInt(this.readBytesAs(2, i16)),
            0b1100 => @floatFromInt(this.readBytesAs(4, i32)),
            0b1101 => this.readBytesAs(2, f16),
            0b1110 => this.readBytesAs(4, f32),
            0b1111 => this.readBytesAs(8, f64),
            else => unreachable,
        };
    }

    fn isNumberFormattedLikeAnInteger(value: []const u8) bool {
        if (std.mem.eql(u8, value, "-0")) return false;
        return std.mem.indexOfAny(u8, value, ".eE") == null;
    }

    fn parseNumberZ(this: *@This()) !ValueNode {
        var n = this.parseStringZ();
        n.kind = .number;
        const slice = getSlice(&n);
        if (slice.len == 0) return error.InvalidNumber;
        if (slice.len > 2 and slice[0] == '0' and slice[1] == 'x') {
            const u = try std.fmt.parseInt(u64, slice, 16);
            n.slot0 = @truncate(u >> 32);
            n.slot1 = @truncate(u);
            n.slot2 = @intFromEnum(NumberType.unsigned);
            return n;
        }
        if (slice.len > 3 and slice[0] == '-' and slice[1] == '0' and slice[2] == 'x') {
            const u: u64 = @bitCast(try std.fmt.parseInt(i64, slice, 16));
            n.slot0 = @truncate(u >> 32);
            n.slot1 = @truncate(u);
            n.slot2 = @intFromEnum(NumberType.signed);
            return n;
        }
        if (isNumberFormattedLikeAnInteger(slice)) {
            if (slice[0] == '-') {
                const u: u64 = @bitCast(try std.fmt.parseInt(i64, slice, 10));
                n.slot0 = @truncate(u >> 32);
                n.slot1 = @truncate(u);
                n.slot2 = @intFromEnum(NumberType.signed);
                return n;
            }
            const u: u64 = try std.fmt.parseInt(u64, slice, 10);
            n.slot0 = @truncate(u >> 32);
            n.slot1 = @truncate(u);
            n.slot2 = @intFromEnum(NumberType.unsigned);
            return n;
        }
        const v = try std.fmt.parseFloat(f64, slice);
        const u: u64 = @bitCast(v);
        n.slot0 = @truncate(u >> 32);
        n.slot1 = @truncate(u);
        n.slot2 = @intFromEnum(NumberType.float);
        return n;
    }

    fn getNumberFromNode(n: *const ValueNode, comptime T: type) T {
        std.debug.assert(n.kind == .number);
        const u: u64 = (@as(u64, n.slot0) << 32) | n.slot1;
        const t = @as(NumberType, @enumFromInt(n.slot2));
        return switch (T) {
            f16, f32, f64 => switch (t) {
                .unsigned => @floatFromInt(u),
                .signed => @floatFromInt(@as(i64, @bitCast(u))),
                .float => @floatCast(u),
            },
            i8, i16, i32, i64 => switch (t) {
                .unsigned => @intCast(u),
                .signed => @intCast(@as(i64, @bitCast(u))),
                .float => @intFromFloat(@as(f64, @bitCast(u))),
            },
            u8, u16, u32, u64 => switch (t) {
                .unsigned => @intCast(u),
                .signed => @intCast(@as(i64, @bitCast(u))),
                .float => @intFromFloat(@as(f64, @bitCast(u))),
            },
            else => {
                @compileLog(T);
                @compileError("Unhandled type");
            },
        };
    }

    fn parseRefZ(this: *@This(), current: u64) !ValueNode {
        var n = try this.parseNumberZ();
        n.kind = .ref;
        const absolute: u32 = @intCast(@as(i64, current) + getNumberFromNode(&n, i64));
        n.slot0 = absolute;
        n.slot1 = 0;
        n.slot2 = 0;
        return n;
    }

    inline fn parseTag(this: *@This()) u8 {
        const tag = this.bytes[this.cursor];
        this.cursor += 1;
        return tag;
    }

    fn parseRef(this: *@This()) !ValueNode {
        const current = this.cursor;
        const tag = this.parseTag();
        const o: i64 = blk: {
            if (getImmediate(tag)) |v| {
                break :blk v;
            }
            if (getLengthByteCount(tag)) |c| {
                break :blk this.readBytesAs(c, i64);
            }
            return this.parseRefZ(current);
        };
        const absolute: u32 = @intCast(@as(i64, current) + o);
        return .{
            .kind = .ref,
            .slot0 = absolute,
            .slot1 = 0,
            .slot2 = 0, // unresolved
        };
    }

    fn parseNumber(this: *@This()) !ValueNode {
        const tag = this.parseTag();
        if (tag >> 4 == 0) return this.parseNumberZ();
        const u: f64 = blk: {
            if (getImmediate(tag)) |v| {
                break :blk @floatFromInt(v);
            }
            break :blk this.parsedSizedNumber(tag);
        };
        const v: u64 = @bitCast(u);
        return .{
            .kind = .number,
            .slot0 = @truncate(v >> 32),
            .slot1 = @truncate(v),
            .slot2 = @intFromEnum(NumberType.float),
        };
    }

    fn parseArrayOrObject(this: *@This(), comptime k: Kind) !ValueNode {
        std.debug.assert(k == .array or k == .object);
        const tag = this.parseTag();
        const u: i64 = blk: {
            if (tag >> 4 == 0) break :blk -1;
            if (getImmediate(tag)) |v| break :blk @intCast(v);
            const c = getLengthByteCount(tag) orelse unreachable;
            break :blk this.readBytesAs(c, u64);
        };

        const start = this.cursor;
        var head: u32 = 0;
        while (true) {
            if (u == -1 and this.bytes[this.cursor] == 0) break;
            if (u != -1 and this.cursor == start + @as(u64, @intCast(u))) break;
            if (comptime k == .object) {
                const key = try this.parseValue();
                if (head == 0) head = key;
                _ = try this.parseValue();
            } else {
                const el = try this.parseValue();
                if (head == 0) head = el;
            }
        }

        this.prev = 0;
        return .{
            .kind = k,
            .slot0 = head,
        };
    }

    fn parseObject(this: *@This()) !ValueNode {
        return this.parseArrayOrObject(.object);
    }

    fn parseArray(this: *@This()) !ValueNode {
        return this.parseArrayOrObject(.array);
    }

    fn parseComputed(this: *@This()) !ValueNode {
        const tag = this.parseTag();
        const u: i64 = blk: {
            if (tag >> 4 == 0) break :blk -1;
            if (getImmediate(tag)) |v| break :blk @intCast(v);
            const c = getLengthByteCount(tag) orelse unreachable;
            break :blk this.readBytesAs(c, u64);
        };
        _ = u;
        const subject = try this.parseValue();
        const input = try this.parseValue();
        this.prev = 0;
        return .{
            .kind = .computed,
            .slot0 = subject,
            .slot1 = input,
        };
    }

    inline fn peekTagKind(this: *const @This()) Kind {
        return @enumFromInt(this.bytes[this.cursor] & 0xF);
    }

    fn parseValue(this: *@This()) anyerror!ValueRef {
        if (this.cursor == this.bytes.len) return error.UnexpectedEOF;
        const start = this.cursor;
        const prev = this.prev;
        this.prev = 0;
        const n: ValueNode = switch (this.peekTagKind()) {
            .ref => try this.parseRef(),
            .number => try this.parseNumber(),
            .string => this.parseString(),
            .object => try this.parseObject(),
            .array => try this.parseObject(),
            .computed => try this.parseObject(),
            .null, .undefined, .true, .false => |k| .{ .kind = k },
            else => unreachable,
        };
        const r = try this.nodes.push(n);
        if (prev != 0) {
            this.nodes.at(prev).next = r;
        }
        this.prev = r;
        try this.pos_to_ref.put(this.nodes.pages.allocator, start, r);
        return r;
    }
};

const ValueGraph = struct {
    values: *ValueParser,
    replacements: *std.AutoHashMapUnmanaged(ValueRef, ValueRef) = .{},

    fn followRefNode(this: *@This(), n: *ValueNode) !ValueRef {
        std.debug.assert(n.kind == .ref);
        if (n.slot2 == 0) {
            n.slot2 = 1;
            const p = n.slot0;
            n.slot0 = this.values.pos_to_ref.get(p) orelse return error.RefNotFound;
        }
        return n.slot0;
    }

    fn followRef(this: *@This(), ref: ValueRef) !ValueRef {
        const n = this.values.nodes.at(ref);
        return this.followRefNode(n);
    }

    fn followReplacements(this: *@This(), ref: ValueRef) ValueRef {
        var r = ref;
        while (true) {
            const n = this.replacements.get(r) orelse break;
            r = n;
        }
        return r;
    }

    pub fn getValue(this: *@This(), ref: ValueRef) *ValueNode {
        return this.values.nodes.at(this.followReplacements(ref));
    }

    pub fn replaceValue(this: *@This(), a: ValueRef, b: ValueRef) !void {
        const f = this.followReplacements(a);
        try this.replacements.put(getAllocator(), f, b);
        if (f != a) {
            try this.replacements.put(getAllocator(), a, b);
        }
    }

    pub fn createRef(this: *@This(), ref: ValueRef) !ValueRef {
        return this.values.nodes.push(.{
            .kind = .ref,
            .slot0 = ref,
            .slot1 = 0,
            .slot2 = 1,
        });
    }

    pub fn createComputed(this: *@This(), subject: ValueRef, input: ValueRef) !ValueRef {
        return this.values.nodes.push(.{
            .kind = .computed,
            .slot0 = subject,
            .slot1 = input,
        });
    }

    pub fn createArray(this: *@This(), head: ValueRef) !ValueRef {
        return this.values.nodes.push(.{
            .kind = .array,
            .slot0 = head,
        });
    }

    pub fn createObject(this: *@This(), head: ValueRef) !ValueRef {
        return this.values.nodes.push(.{
            .kind = .object,
            .slot0 = head,
        });
    }

    pub fn createNumber(this: *@This(), v: f64) !ValueRef {
        const u: u64 = @bitCast(v);
        return this.values.nodes.push(.{
            .kind = .number,
            .slot0 = @truncate(u >> 32),
            .slot1 = @truncate(u),
            .slot2 = @intFromEnum(NumberType.float),
        });
    }

    pub fn createString(this: *@This(), v: []const u8) !ValueRef {
        return this.values.nodes.push(.{
            .kind = .string,
            .slot0 = @truncate(@intFromPtr(v.ptr)),
            .slot1 = @truncate(@intFromPtr(v.ptr) >> 32),
            .slot2 = @truncate(v.len),
        });
    }

    pub fn createBoolean(this: *@This(), v: bool) !ValueRef {
        return this.values.nodes.push(.{
            .kind = if (v == true) .true else .false,
        });
    }

    pub fn createUndefined(this: *@This()) !ValueRef {
        return this.values.nodes.push(.{
            .kind = .undefined,
        });
    }

    pub fn createNull(this: *@This()) !ValueRef {
        return this.values.nodes.push(.{
            .kind = .null,
        });
    }
};

// most optimizations require a reducer for .computed nodes
// this is for reconstitution, not encoding optimization
//
// the optimizations we try to do:
// 1. computation folding - this is similar to constant folding, though are goal is to reduce graph nodes by simplifying or eliminating computations
//    - given a computation node, we ask a reducer for a simplified version providing the subject/input
//    - note that the reducer may respond with another computation node with different subjects/inputs.
// 2. inlining / input propagation - replaces a "factory function call" with templated input bindings
// 3. merging - we can combine multiple computation nodes into 1, potentially simplifying the graph (e.g. via lexical scopes)
// 4. value inlining - this should be done after merging as much as we can. this is similar to input propgation except we remove the value from the graph entirely.
// 5. value destructuring - can only be done after inlining into a single computed node
//
const Optimizer = struct {
    values: *ValueParser,
};

// here's a real-world computation folding example (note: this should be done after inlining, but you may do it earlier if a factory fn is only used once!)
//
// const subject = `function(_c_c, _c__v3, _v4, __template, __slot, _v10, __swap_tree, _v0, _v1) {
//   return () => {
//     const d = (() => _c_c[0])()
//     _c__v3[0] = d;
//     const _v9 = !!d
//     if (_v9) {
//       if (!_v4) {
//         const _v6 = __template(\`<div _iqikm4s3><!>\`)
//         _v4 = [_v6];
//         let _v7 = _v6.firstChild
//         let _v8
//         _v4[Symbol.update] = () => {
//           _v8 = __slot(_v7, _v8, _c__v3[0]);
//         };
//       }
//     }
//     if (_v9 !== _v10) __swap_tree(_v0, _v1, _v9 ? _v4 : [], _v9 ? [] : _v4);
//     _v10 = _v9;
//     if (_v9) _v4[Symbol.update]();
//   }
// }`
// const _11 = [1]
// const _12 = []
// const _13 = []
// // use noops for these computed-typed captures just as an example
// const _14 = () => {} // __template
// const _20 = () => {} // __slot
// const _22 = () => {} // __swap_tree
// const _24 = _25(_5) // some DOM node
// const _26 = _27(_28) // some DOM node
// const input = [_11, _12, _13, _14, _20, true, _22, _24, _26]
//
// we analyze the code given the known inputs, and see if we can eliminate any code paths
// from analysis, we see that:
// 1. _v4 is never assigned in a truthy path but initializes as truthy, thus, that path can never be taken
// 2. because that branch is never taken, many of the parameters/captures are never used, producing:
// const subject = `function(_c_c, _c__v3, _v4, _v10, __swap_tree, _v0, _v1) {
//   return () => {
//     const d = (() => _c_c[0])()
//     _c__v3[0] = d;
//     const _v9 = !!d
//     if (_v9 !== _v10) __swap_tree(_v0, _v1, _v9 ? _v4 : [], _v9 ? [] : _v4);
//     _v10 = _v9;
//     if (_v9) _v4[Symbol.update]();
//   }
// }`
//
// and because `__template` and `__slot` are pure computations, we can fully remove them
// if they weren't pure, and weren't referenced elsewhere, you could still use the optimized subject but would have to express the `input` as a computation:
//
// const input = (x => {
//   const __14 = ...
//   const __20 = ...
//   return x
// })([_11, _12, _13, true, _22, _24, _26])
//
// ------------------- computation inlining -------------------
//
// computation inlining example:
//
// const _11 = [1]
// const _5 = ... // some DOM node
// const _35 = function(_c_c, el) {
//   return () => {
//     _c_c[0] += 1;
//     el[Symbol.update]();
//   }
// }
// const _34 = _35(_11, _5) // this is an emitted computation node
//
// here, the reducer (working over the value graph, not emit!) recognizes that `_35` (which is just a string under `subject`):
// * is a function returning another function
// * has no parameter bindings being reassigned
// * is only referenced once, via `_34`
//
// we can then directly substitute the inputs (symbolically, not literally) into the code fragment to give:
// const _34 = () => {
//   _11[0] += 1;
//   _5[Symbol.update]();
// }
//
// which could be encoded as a computation node with a templating subject, so that an emitter can assign bindings later:
//   subject: {
//      kind: 'expression-template',
//      template: '() => {\n  $0[0] += 1;\n  $1[Symbol.update]();\n}'
//   },
//   input: [_11, _5]
// note: the above "template" would need escaping of `$` followed by a numeric, the easiest way is to repeat the `$` token so that a literal "$1" becomes "$$1". we use `$` because it's already valid for JS identifiers, so we can parse the code w/o any extra steps.
// likewise, an emitter would have to turn all inputs into bindings and then feed that into the template. either via "symbol replacements" OR using regexps: one to replace `[^$\\]($\d+)` with bindings, then another to replace `$($\d+)` with `$1` (the match group) to unescape
//
// ------------------- advanced inlining -------------------
// we can inline the first example as well by recognizing that mutated parameter bindings can be expressed inside an IIFE, producing:
//   subject: {
//      kind: 'expression-template',
//      template: '(_p0 => () => {\n  const d = (() => $0[0])()\n  $1[0] = d\n  const _v9 = !!d\n  if (_v9 !== _p0) $3($4, $5, _v9 ? $2 : [], _v9 ? [] : $2)\n  _p0 = _v9\n  if (_v9) $2[Symbol.update]()})(true)'
//   },
//   input: [_11, _12, _13, _22, _24, _26]
//
//
//
// ------------------- computation folding into statements -------------------
// computed nodes with no refs and used directly apart of an object entry value can be combined together into a single computation node, splicing itself in front of the original value node
// consider the `_34` example used in "computation inlining". This is only used apart of `_5` (an object) as `[Symbol.update]: _34`, so we can create the following computation node:
//
//   subject: {
//      kind: 'statement-template',
//      template: '$0[Symbol.update] = () => {\n  $1[0] += 1;\n  $0[Symbol.update]();\n}'
//   },
//   input: [_11, _5]
//
// this transformation needs to preserve graph order. that is, you must transform object entries w/ computed values _in reverse_ so that the last assignment is the "result". if _5 had something like `foo: _50()` that came after Symbol.update, we'd also have this node, where `_42` is the statement node we just created:
//
//   subject: {
//      kind: 'statement-template',
//      template: '$0.foo = $1();',
//   },
//   input: [_42, _50]
//
// note: we are treating `input[0]` (if present and non-nullish) as being the "result" of the statement template as well as an input. this is just a convention.
//
//
// ------------------- merging -------------------
// if a computed node is only reachable through another computed node, then we can merge them together
// there are some rules to merging:
// 1. we won't merge statements with expressions
// 2. expressions can be merged together, however, they must still preserve relative order of other computed nodes. so, if you cannot inline a computed node in the input (in reverse order), you should bail
// 3. statement + block = statement is prepended or appended (depending on relative order)
// 4. a block + block merge can remove the inner block if it contains no named declarations OR we rewrite the declarations to avoid collisions
//
// so, the above statement examples could be merged as:
//   subject: {
//      kind: 'statement-template',
//      template: '{\n  $0[Symbol.update] = () => {\n  $1[0] += 1;\n  $0[Symbol.update]();\n}\n  $0.foo = $2()\n}',
//   },
//   input: [_11, _5, _50]
//
//
// ------------------- value inlining and destructuring -------------------
// once a value is only reachable by a single computed node, we may inline the value into the node.
// consider this templated block, created from merging:
//
//  {
//    $0[Symbol.update] = () => {
//      console.log($1[0])
//    }
//    $2.addEventListener('click', () => {
//      $1[0] += 1;
//      $0[Symbol.update]();
//    })
//  }
//
// if `$1` is not reachable elsewhere, we can do this (using `_d\d+` for declaration names as convention):
//  {
//    const _d0 = [1]
//    $0[Symbol.update] = () => {
//      console.log(_d0[0])
//    }
//    $1.addEventListener('click', () => {
//      _d0[0] += 1;
//      $0[Symbol.update]();
//    })
//  }
//
// the value is no longer present as an explicit input, as the computed node contains it fully.
//
// now that the value is no longer directly in the graph, we can recognize that, inside of the `computed` node, `_d0` is:
//  * NEVER directly referenced, it's always by `[0]`
//  * we directly know the result of the initializer, and if know that `this` would never be used (for example, we assume no Array prototype monkeypatching), then we can immediately destructuring it, while still ensuring that any possible effects aren't removed, e.g. `const _d0 = [1, foo()]` may need to create an expression statement for `foo()` to preserve behavior!
//  * `_d0[0]` is mutated, thus we should emit `let` instead of `const`
//
// so we simplify to:
//  {
//    let _d0 = 1
//    $0[Symbol.update] = () => {
//      console.log(_d0)
//    }
//    $1.addEventListener('click', () => {
//      _d0 += 1;
//      $0[Symbol.update]();
//    })
//  }
