const std = @import("std");
const getAllocator = @import("./string_immutable.zig").getAllocator;
const isIdentifier = @import("./lexer.zig").isIdentifier;
const isIdentPart = @import("./lexer.zig").isIdentifierContinue;

const parser = @import("./parser.zig");
const value_syntax = @import("./value_syntax.zig");
const debugPrint = parser.debugPrint;
const BumpAllocator = parser.BumpAllocator;

const NodeRef = parser.NodeRef;
const SyntaxKind = parser.SyntaxKind;
const AstNode = parser.AstNode;
const AstData = parser.AstData;
const Factory = parser.Factory;
const NodeIterator = parser.NodeIterator;
const getPackedData = parser.getPackedData;
const getLeft = parser.getLeft;
const getRight = parser.getRight;
const unwrapRef = parser.unwrapRef;
const maybeUnwrapRef = parser.maybeUnwrapRef;
const getAstSlice = parser.getSlice;

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
// References encode signed integer values. Unlike numbers, all fixed-width reference encodings are signed
//  - zero terminated references are parsed the same way as numbers, you then can apply the relative offset to get an absolute
//  - the resulting integer is interpreted as a relative byte offset from the start of the type tag

// unexpected EOF -> abort
// parsers stop exactly at the terminator or declared width; subsequent bytes belong to the enclosing value.
// strings containing a zero byte MUST be encoded as sized; there is no escaping mechanism.

// arrays enumerate their elements
// objects enumerate their entries (key -> value -> key -> value -> ...)
//  - hitting a zero byte or the declared byte width before completing a pair is a parse error
// computations are _parsed_ as zero or more values just like arrays. however the sematics are that:
//  - the first value is called the "subject"
//  - the second value, if present, is called the "input"
//  - anything after that is the "rest" of the computation
// computations can be made up of any other values: references, opaque, other computations, etc.
//  - A computation is already a perfectly valid value on its own. Evaluation is something a consumer chooses to do, not something demanded by the format.
// an empty computation isn't a syntax error but is unlikely to be useful for much
//
// for sized arrays/objects/computations, decoders MUST check that the end of the trailing value is aligned with the expected width
//  - in other words, the cursor must end exactly at the declared end. if the cursor is less than the expected end, keep parsing values
//
// keys are normal values, so all possible types are valid to parse even if semantically errors.
// duplicate keys are not parse errors
// encountering an unexpected zero byte is an immediate parse error
// when any size bit is set, we say that the value is "sized"

// for all potential semantic errors, a decoder MAY choose to fail early
// if it knows the consumer would reject them as well i.e. via explicit config

// size bits for true/false/null
// xxxx - unused

// for strings/array/object/computed/opaque, size bit encoding:
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

// THE OPAQUE KIND ISN'T SPECIAL, IT JUST HAS NO SPECIFIC INTERPRETATION OF ITS CONTENTS. SO, IT'S JUST A BUNCH OF BYTES.
// it is a sized kind like all the other non-primitives
// .vson cannot use opaque but the binary format .vg can.

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
    @"opaque" = 15,
};

pub const NumberType = enum(u4) {
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
    cursor: usize = 0,
    nodes: BumpAllocator(ValueNode),
    root: ValueRef = 0,
    prev: ValueRef = 0,
    pos_to_ref: std.AutoHashMapUnmanaged(usize, ValueRef) = .{},

    pub fn parse(bytes: []const u8) !@This() {
        var t = @This(){
            .bytes = bytes,
            .nodes = BumpAllocator(ValueNode).init(getAllocator(), std.heap.page_allocator),
        };
        try t.nodes.preAlloc();
        _ = try t.nodes.push(.{ .kind = .NUL });
        t.root = try t.parseValue();
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
            .slot0 = if (comptime @import("builtin").target.isWasm()) 0 else @truncate(@intFromPtr(slice.ptr) >> 32),
            .slot1 = @truncate(@intFromPtr(slice.ptr)),
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
        this.cursor += @intCast(u);
        const slice = this.bytes[start..this.cursor];
        return .{
            .kind = .string,
            .slot0 = if (comptime @import("builtin").target.isWasm()) 0 else @truncate(@intFromPtr(slice.ptr) >> 32),
            .slot1 = @truncate(@intFromPtr(slice.ptr)),
            .slot2 = @truncate(slice.len),
        };
    }

    fn parseOpaque(this: *@This()) ValueNode {
        // i guess strings are opaque in all but name. well except the whole utf8 thing.
        var n = this.parseString();
        n.kind = .@"opaque";
        return n;
    }

    pub inline fn getSlice(node: *const ValueNode) []const u8 {
        return triplesToSlice(u8, node.slot0, node.slot1, node.slot2);
    }

    inline fn ByteSizedType(comptime T: type, comptime size: comptime_int) type {
        return switch (size) {
            1 => switch (T) {
                u8, i8 => T,
                i64 => i8,
                u64 => u8,
                f64 => unreachable,
                else => @compileError("out of types :("),
            },
            2 => switch (T) {
                u16, i16, f16 => T,
                i64 => i16,
                u64 => u16,
                f64 => f16,
                else => @compileError("out of types :("),
            },
            4 => switch (T) {
                u32, i32, f32 => T,
                i64 => i32,
                u64 => u32,
                f64 => f32,
                else => @compileError("out of types :("),
            },
            8 => switch (T) {
                i64, u64, f64 => T,
                else => @compileError("out of types :("),
            },
            else => @compileError("unhandled size"),
        };
    }

    inline fn readBytesAs(this: *@This(), count: u8, comptime T: type) T {
        const b = this.bytes[this.cursor .. this.cursor + count];
        if (@typeInfo(T) == .Float) {
            const v: T = switch (count) {
                2 => @floatCast(@as(f16, @bitCast(b[0..2].*))),
                4 => @floatCast(@as(f32, @bitCast(b[0..4].*))),
                8 => @floatCast(@as(f64, @bitCast(b[0..8].*))),
                else => unreachable,
            };
            this.cursor += count;
            return v;     
        }
        const v: T = switch (count) {
            1 => @intCast(@as(ByteSizedType(T, 1), @bitCast(b[0..1].*))),
            2 => @intCast(@as(ByteSizedType(T, 2), @bitCast(b[0..2].*))),
            4 => @intCast(@as(ByteSizedType(T, 4), @bitCast(b[0..4].*))),
            8 => @intCast(@as(ByteSizedType(T, 8), @bitCast(b[0..8].*))),
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
                0b0001...0b1011 => @as(i16, -1) << (upper - 1),
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
                0b1100...0b1111 => @as(u8, 1) << @as(u3, @intCast(upper - 0b1100)),
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

    inline fn parsedSizedNumber(this: *@This(), tag: u8) f64 {
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

    pub fn getNumberFromNode(n: *const ValueNode, comptime T: type) T {
        if (comptime @import("builtin").cpu.arch.endian() == .big) {
            @compileError("TODO: big endian byte swap");
        }
        std.debug.assert(n.kind == .number);
        const u: u64 = (@as(u64, n.slot0) << 32) | n.slot1;
        const t = @as(NumberType, @enumFromInt(n.slot2));
        return switch (T) {
            f16, f32, f64 => switch (t) {
                .unsigned => @floatFromInt(u),
                .signed => @floatFromInt(@as(i64, @bitCast(u))),
                .float => @floatCast(@as(f64, @bitCast(u))),
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
        const absolute: u32 = @intCast(@as(i64, @intCast(current)) + getNumberFromNode(&n, i64));
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
        const absolute: u32 = @intCast(@as(i64, @intCast(current)) + o);
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

    fn parseStructure(this: *@This(), comptime k: Kind) !ValueNode {
        std.debug.assert(k == .array or k == .object or k == .computed);
        const tag = this.parseTag();
        const u: i64 = blk: {
            if (tag >> 4 == 0) break :blk -1;
            if (getImmediate(tag)) |v| break :blk @intCast(v);
            const c = getLengthByteCount(tag) orelse unreachable;
            break :blk @intCast(this.readBytesAs(c, u64));
        };

        const start = this.cursor;
        var head: u32 = 0;
        var expect_pair: bool = false;
        while (true) {
            if (u == -1 and this.bytes[this.cursor] == 0) {
                this.cursor += 1;
                break;
            }
            if (u != -1 and this.cursor >= start + @as(usize, @intCast(u))) {
                if (this.cursor > start + @as(usize, @intCast(u))) return error.InvalidValueWidth;
                break;
            }
            const el = try this.parseValue();
            std.debug.assert(el != 0);
            if (head == 0) head = el;
            if (comptime k == .object) expect_pair = !expect_pair;
        }
        if (expect_pair) return error.IncompleteKeyValuePair;

        this.prev = 0;
        return .{
            .kind = k,
            .slot0 = head,
        };
    }

    fn parseObject(this: *@This()) !ValueNode {
        return this.parseStructure(.object);
    }

    fn parseArray(this: *@This()) !ValueNode {
        return this.parseStructure(.array);
    }

    fn parseComputed(this: *@This()) !ValueNode {
        var n = try this.parseStructure(.computed);
        if (n.slot0 != 0) { // #input
            n.slot1 = this.nodes.at(n.slot0).next;
        }
        if (n.slot1 != 0) { // #rest
            n.slot2 = this.nodes.at(n.slot1).next;
        }
        return n;
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
            .array => try this.parseArray(),
            .computed => try this.parseComputed(),
            .@"opaque" => this.parseOpaque(),
            .null, .undefined, .true, .false => |k| blk: {
                this.cursor += 1;
                break :blk .{ .kind = k };
            },
            else => unreachable,
        };
        const r = try this.nodes.push(n);
        if (prev != 0) {
            this.nodes.at(prev).next = @truncate(r);
        }
        this.prev = r;
        try this.pos_to_ref.put(this.nodes.pages.allocator, start, r);
        return r;
    }
};

fn triplesToSlice(comptime T: type, slot0: u32, slot1: u32, slot2: u32) []const T {
    if (slot2 == 0) return &.{};
    if (comptime @import("builtin").target.isWasm()) {
        const ptr: [*]const T = @ptrFromInt(slot1);
        return ptr[0..slot2];
    }
    const ptr: [*]const T = @ptrFromInt((@as(u64, slot0) << 32) | slot1);
    return ptr[0..slot2];
}

inline fn valueHasEdges(node: *const ValueNode) bool {
    if (node.next != 0) return true;
    return switch (node.kind) {
        .NUL => false, // keep it simple instead of failing here
        .string, .number, .null, .undefined, .true, .false, .@"opaque" => false,
        .ref, .array, .object, .computed => true,
    };
}

const ValueGraph = struct {
    values: *ValueParser,
    replacements: *std.AutoHashMapUnmanaged(ValueRef, ValueRef),
    counts: std.AutoHashMapUnmanaged(ValueRef, u32) = .{},
    normalizing: bool = false,

    pub fn getRefCount(this: *@This(), ref: ValueRef) u32 {
        return this.counts.get(this.followReplacements(ref)) orelse 0;
    }

    pub inline fn assertFreshNode(this: *@This(), ref: ValueRef) void {
        std.debug.assert(!this.replacements.contains(ref));
        std.debug.assert(this.getRefCount(ref) == 0);
    }

    pub fn setNext(this: *@This(), from: ValueRef, to: ValueRef) void {
        this.assertFreshNode(from);
        this.values.nodes.at(from).next = @truncate(to);
    }

    fn adjustRefCount(this: *@This(), _ref: ValueRef, amt: i2, visited: *std.AutoHashMapUnmanaged(ValueRef, void)) anyerror!void {
        if (_ref == 0) return;
        std.debug.assert(amt != 0 and amt != -2);
        const ref= this.followReplacements(_ref);
        const entry = try this.counts.getOrPutValue(getAllocator(), ref, 0);
        const v = entry.value_ptr.*;
        if (amt < 0 and v == 0) return;
        entry.value_ptr.* = @intCast(@as(i32, @intCast(v)) + amt);
        if ((amt < 0 and v == -amt) or (amt > 0 and v == 0)) {
            try this.walkForRefCounts(ref, amt, visited);
        }
    }

    fn walkForRefCounts(
        this: *@This(),
        ref: ValueRef,
        amt: i2,
        visited: *std.AutoHashMapUnmanaged(ValueRef, void),
    ) anyerror!void {
        const n = this.getValue(ref);
        if (!valueHasEdges(n)) return;

        if (visited.contains(ref)) return;
        try visited.put(getAllocator(), ref, {});
        switch (n.kind) {
            .array, .object, .computed => {
                try this.adjustRefCount(n.slot0, amt, visited);
                // var s = n.slot0;
                // while (s != 0) {
                //     try this.adjustRefCount(s, amt, visited);
                //     s = this.getValue(s).next;
                // }
            },
            .ref => {
                const target = try this.followRefNode(n);
                try this.adjustRefCount(target, amt, visited);
            },
            else => {},
        }
        try this.adjustRefCount(n.next, amt, visited);
    }

    const EdgeDiff = struct {
        added: u2 = 0,
        removed: u2 = 0,
        added_buf: [2]ValueRef = undefined,
        removed_buf: [2]ValueRef = undefined,

        inline fn addedSlice(this: *const @This()) []const ValueRef {
            return this.added_buf[0..this.added];
        }

        inline fn removedSlice(this: *const @This()) []const ValueRef {
            return this.removed_buf[0..this.removed];
        }

        inline fn _insert(this: *@This(), ref: ValueRef, comptime is_remove: bool) void {
            if (ref == 0) return;
            const s = if (comptime is_remove) this.addedSlice() else this.removedSlice();
            const dest = if (comptime is_remove) this.removed_buf[0..] else this.added_buf[0..];
            const c1 = if (comptime is_remove) &this.removed else &this.added;
            const c2 = if (comptime is_remove) &this.added else &this.removed;
            if ((s.len == 2 and s[1] == ref) or (s.len == 1 and s[0] == ref)) {
                c2.* -= 1;
            } else if (s.len == 2 and s[0] == ref) {
                const u = if (comptime is_remove) this.added_buf[0..] else this.removed_buf[0..];
                u[0] = s[1];
                c2.* -= 1;
            } else {
                dest[c1.*] = ref;
                c1.* += 1;
            }
        }

        fn insertAdded(this: *@This(), ref: ValueRef) void {
            this._insert(ref, false);
        }

        fn insertRemoved(this: *@This(), ref: ValueRef) void {
            this._insert(ref, true);
        }
    };

    fn diffEdges(
        this: *@This(), 
        a: ValueRef, // the node that is REMOVED
        b: ValueRef, // the new node
    ) !EdgeDiff {
        var diff: EdgeDiff = .{};
        const na = this.getValue(a);
        const nb = this.getValue(b);
        switch (na.kind) {
            .array, .object, .computed => {
                if (na.slot0 != b) {
                    diff.insertRemoved(na.slot0);
                }
            },
            .ref => {
                const target = try this.followRefNode(na);
                if (target != b) {
                    diff.insertRemoved(target);
                }
            },
            else => {},
        }
        switch (nb.kind) {
            .array, .object, .computed => {
                if (nb.slot0 != a) {
                    diff.insertAdded(nb.slot0);
                }
            },
            .ref => {
                const target = try this.followRefNode(nb);
                if (target != a) {
                    diff.insertAdded(target);
                }
            },
            else => {},
        }
        if (na.next != b) diff.insertRemoved(na.next);
        if (nb.next != a) diff.insertAdded(nb.next);
        return diff;
    }

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

    pub fn followAllRefs(this: *@This(), ref: ValueRef) !ValueRef {
        var r = ref;
        var n = this.getValue(r);
        if (n.kind != .ref) return ref;

        // TODO: detect cycles during replacement? only debug mode?
        var visited = std.AutoHashMapUnmanaged(ValueRef, void){};
        defer visited.deinit(getAllocator());

        while (n.kind == .ref) {
            try visited.put(getAllocator(), r, {});
            r = try this.followRefNode(n);
            n = this.getValue(r);
            if (visited.contains(r)) return error.FoundRefCycle;
        }

        return r;
    }

    // "strictly same" means referential equality AND replacements accounted for.
    pub fn isStrictlySameValueRef(this: *@This(), a: ValueRef, b: ValueRef) !bool {
        return this.followReplacements(try this.followAllRefs(a)) == this.followReplacements(try this.followAllRefs(b));
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

    pub fn getFollowedValue(this: *@This(), ref: ValueRef) !*ValueNode {
        return this.values.nodes.at(try this.followAllRefs(this.followReplacements(ref)));
    }

    pub fn getSubject(_: *@This(), n: *const ValueNode) ValueRef {
        std.debug.assert(n.kind == .computed);
        return n.slot0;
    }

    pub fn getInput(_: *@This(), n: *const ValueNode) ValueRef {
        std.debug.assert(n.kind == .computed);
        return n.slot1;
    }

    pub fn getBoolean(_: *@This(), n: *const ValueNode) bool {
        std.debug.assert(n.kind == .true or n.kind == .false);
        return n.kind == .true;
    }

    pub fn getString(_: *@This(), n: *const ValueNode) []const u8 {
        std.debug.assert(n.kind == .string);
        return ValueParser.getSlice(n);
    }

    pub fn getDouble(_: *@This(), n: *const ValueNode) f64 {
        std.debug.assert(n.kind == .number);
        return ValueParser.getNumberFromNode(n, f64);
    }

    pub fn tryLiteralText(this: *@This(), ref: ValueRef) !?[]const u8 {
        const resolved = try this.followAllRefs(ref);
        const n = this.getValue(resolved);
        return switch (n.kind) {
            .true => try getAllocator().dupe(u8, "true"),
            .false => try getAllocator().dupe(u8, "false"),
            .null => try getAllocator().dupe(u8, "null"),
            .undefined => try getAllocator().dupe(u8, "undefined"),
            .number => blk: {
                const v = this.getDouble(n);
                if (v == @trunc(v) and @abs(v) < 1e15) {
                    break :blk try std.fmt.allocPrint(getAllocator(), "{d}", .{@as(i64, @intFromFloat(v))});
                }
                break :blk try std.fmt.allocPrint(getAllocator(), "{d}", .{v});
            },
            .string => blk: {
                const s = this.getString(n);
                var out = std.ArrayList(u8).init(getAllocator());
                try out.append('"');
                for (s) |c| {
                    switch (c) {
                        '"' => try out.appendSlice("\\\""),
                        '\\' => try out.appendSlice("\\\\"),
                        '\n' => try out.appendSlice("\\n"),
                        '\r' => try out.appendSlice("\\r"),
                        else => try out.append(c),
                    }
                }
                try out.append('"');
                break :blk out.items;
            },
            else => null,
        };
    }

    pub fn renderValueAsLiteral(this: *@This(), ref: ValueRef) anyerror!?[]const u8 {
        const resolved = try this.followAllRefs(ref);
        const n = this.getValue(resolved);
        switch (n.kind) {
            .ref => {
                // we can sometimes see orphaned values but these are fine to emit direct
                const inner = try this.followRefNode(n);
                if (this.getRefCount(inner) > 1) return null;
                return try this.renderValueAsLiteral(inner);
            },
            .true, .false, .null, .undefined, .number, .string => return try this.tryLiteralText(resolved),
            .array => {
                var out = std.ArrayList(u8).init(getAllocator());
                try out.append('[');
                var i: u32 = 0;
                var first = true;
                // oh my. this is rough.
                while (this.getArrayElement(n, i)) |el| : (i += 1) {
                    const el_text = try this.renderValueAsLiteral(el) orelse return null;
                    if (!first) try out.appendSlice(", ");
                    first = false;
                    try out.appendSlice(el_text);
                }
                try out.append(']');
                return out.items;
            },
            .object => {
                var out = std.ArrayList(u8).init(getAllocator());
                try out.append('{');
                var key_ref = n.slot0;
                var first = true;
                while (key_ref != 0) {
                    const key_node = this.getValue(key_ref);
                    const value_ref = key_node.next;
                    const key_resolved = try this.followAllRefs(key_ref);
                    const key_n = this.getValue(key_resolved);
                    if (key_n.kind != .string) return null; // only plain string keys
                    const key_text = this.getString(key_n);
                    const val_text = try this.renderValueAsLiteral(value_ref) orelse return null;
                    if (!first) try out.appendSlice(", ");
                    first = false;
                    if (isIdentifier(key_text)) {
                        try out.appendSlice(key_text);
                    } else {
                        try out.appendSlice(try this.tryLiteralText(key_resolved) orelse return null);
                    }
                    try out.appendSlice(": ");
                    try out.appendSlice(val_text);
                    key_ref = this.getValue(value_ref).next;
                }
                try out.append('}');
                return out.items;
            },
            else => return null, // computed (or unresolved ref cycle)
        }
    }

    pub fn valueToParseNode(this: *@This(), ref: ValueRef, factory: parser.Factory) anyerror!?NodeRef {
        const n = this.getValue(ref);
        return switch (n.kind) {
            .string => {
                const z = try factory.createStringLiteralAllocated(this.getString(n));
                factory.nodes.at(z).flags |= @intFromEnum(parser.StringFlags.synthetic);
                return z;
            },
            .number => {
                const v = this.getDouble(n);
                return try factory.createNumericLiteral(v);
            },
            .true => try factory.createTrue(),
            .false => try factory.createFalse(),
            .undefined => try factory.createUndefined(),
            .null => try factory.createNull(),
            .array => {
                var list = parser.NodeList.init(factory.nodes);
                var s = n.slot0;
                while (s != 0) {
                    const v = try this.valueToParseNode(s, factory) orelse return;
                    list.appendRef(v);
                    s = this.getValue(s).next;
                }
                return try factory.createArrayLiteralExpression(list.head);
            },
            .object => {
                var list = parser.NodeList.init(factory.nodes);
                var s = n.slot0;
                var k: NodeRef = 0;
                while (s != 0) {
                    if (k != 0) {
                        const v = try this.valueToParseNode(s, factory) orelse return;
                        list.appendRef(try factory.createPropertyAssignment(k, v));
                        k = 0;
                    } else {
                        const c = this.getValue(try this.followAllRefs(s));
                        if (c.kind == .string) {
                            k = try factory.createPropertyName(this.getString(c));
                        } else {
                            k = try this.valueToParseNode(s, factory) orelse return;
                        }
                    }
                    s = this.getValue(s).next;
                }
                return try factory.createObjectLiteralExpression(list.head);
            },
            .ref => {
                const inner = try this.followRefNode(n);
                if (this.getRefCount(inner) > 1) {
                    var buf: [32]u8 = undefined;
                    const s = try std.fmt.bufPrint(&buf, "$00{}", .{inner});
                    return try factory.createIdentifier(s);
                }
                return try this.valueToParseNode(inner, factory);
            },
            else => return null,
        };
    }

    pub fn getArrayElement(this: *@This(), n: *const ValueNode, index: u32) ?ValueRef {
        std.debug.assert(n.kind == .array);
        var s = n.slot0;
        var i: u32 = 0;
        while (s != 0) {
            if (i == index) return s;
            const k = this.getValue(s);
            s = k.next;
            i += 1;
        }
        return null;
    }

    pub fn getPropertyValue(this: *@This(), n: *const ValueNode, key: ValueRef) !?ValueRef {
        std.debug.assert(n.kind == .object);
        var s = n.slot0;
        while (s != 0) {
            const k = this.getValue(s);
            const next = k.next;
            if (try this.valuesEql(s, key)) {
                return next;
            }
            s = this.getValue(next).next;
        }
        return null;
    }

    pub fn getStringKeyPropertyValue(this: *@This(), n: *const ValueNode, key: []const u8) !?ValueRef {
        std.debug.assert(n.kind == .object);
        var s = n.slot0;
        while (s != 0) {
            var k = this.getValue(s);
            const next = k.next;
            k = this.getValue(try this.followAllRefs(s));
            if (k.kind == .string) {
                if (std.mem.eql(u8, this.getString(k), key)) {
                    return next;
                }
            }
            s = this.getValue(next).next;
        }
        return null;
    }

    pub fn valuesEql(this: *@This(), a: ValueRef, b: ValueRef) !bool {
        if (a == b) return true;
        const na = this.getValue(try this.followAllRefs(a));
        const nb = this.getValue(try this.followAllRefs(b));
        if (na == nb) return true;
        if (na.kind != nb.kind) return false;
        if (na.kind == .string) {
            return std.mem.eql(u8, this.getString(na), this.getString(nb));
        }
        if (na.kind == .number) {
            return this.getDouble(na) == this.getDouble(nb);
        }
        if (na.kind == .true or na.kind == .false or na.kind == .null or na.kind == .undefined) {
            return true;
        }
        return false;
    }

    fn updateRefCounts(this: *@This(), a: ValueRef, b: ValueRef) !void {
        std.debug.assert(!this.counts.contains(b));
        std.debug.assert(!this.replacements.contains(b));
        const diff = try this.diffEdges(a, b);
        var visited = std.AutoHashMapUnmanaged(ValueRef, void){};
        defer visited.deinit(getAllocator());
        for (diff.removedSlice()) |x| {
            std.debug.assert(x != b);
            try this.adjustRefCount(x, -1, &visited);
            visited.clearRetainingCapacity();
        }        
        for (diff.addedSlice()) |x| {
            try this.adjustRefCount(x, 1, &visited);
            visited.clearRetainingCapacity();
        }
        // the new edges may reference `b`, we add this count into the prior ref count 
        const nc = this.counts.get(b) orelse 0;
        try this.counts.put(getAllocator(), b, nc + this.getRefCount(a));
    }

    pub fn replaceValue(this: *@This(), a: ValueRef, b: ValueRef) !void {
        const f = this.followReplacements(a);
        const bn = this.values.nodes.at(b);
        std.debug.assert(bn.next == 0);
        bn.next = this.values.nodes.at(f).next;
        try this.updateRefCounts(f, b);
        try this.replacements.put(getAllocator(), f, b);
        // if (f != a) {
        //     try this.replacements.put(getAllocator(), a, b);
        // }
    }

    // should only be used during normalization passes, where node mutation is OK
    fn directReplaceValue(this: *@This(), dest: ValueRef, src: ValueRef) void {
        std.debug.assert(this.normalizing);
        const n = this.values.nodes.at(dest);
        const next = n.next;
        n.* = this.values.nodes.at(src).*;
        n.next = next;
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
        this.setNext(subject, input);
        return this.values.nodes.push(.{
            .kind = .computed,
            .slot0 = subject,
            .slot1 = input,
        });
    }

    pub fn createList(this: *@This(), values: []const ValueRef) !ValueRef {
        if (values.len == 0) return 0;
        for (values[0 .. values.len - 1], 1..) |it, idx| {
            this.values.nodes.at(it).next = @truncate(values[idx]);
        }
        this.values.nodes.at(values[values.len - 1]).next = 0;
        return values[0];
    }

    pub fn createArrayFromItems(this: *@This(), items: []const ValueRef) !ValueRef {
        return this.createArray(try this.createList(items));
    }

    pub fn createObjectFromPairs(this: *@This(), pairs: []const [2]ValueRef) !ValueRef {
        var items = std.ArrayListUnmanaged(ValueRef){};
        defer items.deinit(getAllocator());
        for (pairs) |p| {
            try items.append(getAllocator(), p[0]);
            try items.append(getAllocator(), p[1]);
        }
        return this.createObject(try this.createList(items.items));
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
            .slot0 = if (comptime @import("builtin").target.isWasm()) 0 else @truncate(@intFromPtr(v.ptr) >> 32),
            .slot1 = @truncate(@intFromPtr(v.ptr)),
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

    // Should only be used on synthetic values. Will add a new key/value pair if-needed.
    // key: ValueRef | []const u8 (string key)
    pub fn setProperty(this: *@This(), ref: ValueRef, key: anytype, value: ValueRef) !void {
        this.assertFreshNode(ref);
        const n = this.getValue(ref);
        std.debug.assert(n.kind == .object);

        const key_ref: ValueRef = if (@TypeOf(key) == ValueRef) key else try this.createString(key);

        var s = n.slot0;
        var last: ValueRef = 0;
        while (s != 0) {
            const key_node = this.getValue(s);
            const value_ref = key_node.next;
            if (try this.valuesEql(s, key_ref)) {
                // in-place swap
                const continuation = this.getValue(value_ref).next;
                this.setNext(value, continuation);
                this.setNext(s, value);
                return;
            }
            last = value_ref;
            s = this.getValue(value_ref).next;
        }

        // append
        this.setNext(key_ref, value);
        this.setNext(value, 0);
        if (last == 0) {
            n.slot0 = @truncate(key_ref);
        } else {
            this.setNext(last, key_ref);
        }
    }

    // strips `.next`
    pub fn cloneValue(this: *@This(), ref: ValueRef) !ValueRef {
        var clone = this.values.nodes.at(this.followReplacements(ref)).*;
        clone.next = 0;
        return this.values.nodes.push(clone);
    }

    // mutates in-place, creates Refs of entries preceding the key
    pub fn deleteKey(this: *@This(), obj_ref: ValueRef, key_ref: ValueRef) !bool {
        this.assertFreshNode(obj_ref);
        var s = this.getValue(obj_ref).slot0;
        var found = false;
        while (s != 0) {
            if (s == key_ref) {
                found = true;
                break;
            }
            s = this.getValue(s).next;
        }
        if (!found) return false;
        s = this.getValue(obj_ref).slot0;
        var c: ValueRef = 0;
        var l: ValueRef = 0;
        while (s != 0) {
            if (s == key_ref) {
                const v = this.getValue(s).next;
                l = this.getValue(v).next;
                break;
            }
            const n = this.getValue(s);
            const kn = try this.createRef(s);
            s = n.next;
            if (c == 0) {
                this.values.nodes.at(obj_ref).slot0 = @truncate(kn);
            } else {
                this.values.nodes.at(c).next = @truncate(kn);
            }
            c = kn;
        }
        if (c != 0) this.values.nodes.at(c).next = @truncate(l) else this.values.nodes.at(obj_ref).slot0 = @truncate(l);
        return true;
    }

    // does not include itself, does not follow ref nodes
    pub fn hasComputedNode(this: *@This(), ref: ValueRef) bool {
        if (ref == 0) return false;
        const v = this.getValue(ref);
        switch (v.kind) {
            .array, .object, .computed => {
                var s = v.slot0;
                while (s != 0) {
                    if (this.hasComputed(s)) return true;
                    s = this.getValue(s).next;
                }
            },
            else => {},
        }
        return false;
    }

    pub fn hasComputed(this: *@This(), ref: ValueRef) bool {
        if (ref == 0) return false;
        const v = this.getValue(ref);
        if (v.kind == .computed) return true;
        return this.hasComputedNode(ref);
    }

    pub fn dependsOnComputed(this: *@This(), ref: ValueRef) !bool {
        if (ref == 0) return false;
        var visited = std.AutoHashMap(ValueRef, void).init(getAllocator());
        defer visited.deinit();
        return this._dependsOnComputed(ref, &visited);
    }

    pub fn _dependsOnComputed(this: *@This(), ref: ValueRef, visited: *std.AutoHashMap(ValueRef, void)) !bool {
        if (ref == 0) return false;

        const v = this.getValue(ref);
        switch (v.kind) {
            .computed => return true,
            .ref => {
                if (visited.contains(ref)) return false;
                try visited.put(ref, {});
                return try this._dependsOnComputed(try this.followAllRefs(ref), visited);
            },
            .array, .object => {
                if (visited.contains(ref)) return false;
                try visited.put(ref, {});

                var s = v.slot0;
                while (s != 0) {
                    if (try this._dependsOnComputed(s, visited)) return true;
                    s = this.getValue(s).next;
                }
            },
            else => {},
        }
        return false;
    }

    pub fn referencesSelf(this: *@This(), ref: ValueRef) !bool {
        if (ref == 0) return false;
        var visited = std.AutoHashMap(ValueRef, void).init(getAllocator());
        defer visited.deinit();
        return this._referencesSelf(ref, ref, &visited);
    }

    pub fn _referencesSelf(this: *@This(), root: ValueRef, ref: ValueRef, visited: *std.AutoHashMap(ValueRef, void)) !bool {
        if (ref == 0) return false;
        if (root == ref and visited.count() > 0) return true;

        const v = this.getValue(ref);
        switch (v.kind) {
            .ref => {
                if (visited.contains(ref)) return false;
                try visited.put(ref, {});
                return try this._referencesSelf(root, try this.followAllRefs(ref), visited);
            },
            .array, .object, .computed => {
                if (visited.contains(ref)) return false;
                try visited.put(ref, {});

                var s = v.slot0;
                while (s != 0) {
                    if (try this._referencesSelf(root, s, visited)) return true;
                    s = this.getValue(s).next;
                }
            },
            else => {},
        }
        return false;
    }

    const PathInfo = struct {
        parent: ?ValueRef,
        path_segment: []const u8,
    };
    const PathInfoMap = std.AutoHashMapUnmanaged(ValueRef, PathInfo);

    pub fn buildPathInfo(this: *@This(), root: ValueRef) !PathInfoMap {
        var map = PathInfoMap{};
        try this.walkForPathInfo(root, null, "$", &map);
        return map;
    }

    fn walkForPathInfo(
        this: *@This(),
        ref: ValueRef,
        parent: ?ValueRef,
        path_segment: []const u8,
        map: *PathInfoMap,
    ) anyerror!void {
        if (ref == 0 or map.contains(ref)) return;
        try map.put(getAllocator(), ref, .{ .parent = parent, .path_segment = path_segment });
        const replaced = this.followReplacements(ref);
        if (replaced != ref) {
            try map.put(getAllocator(), replaced, .{ .parent = parent, .path_segment = path_segment });
        }

        const n = this.getValue(ref);
        switch (n.kind) {
            .array => {
                var s = n.slot0;
                var i: u32 = 0;
                while (s != 0) {
                    const seg = try std.fmt.allocPrint(getAllocator(), "[{d}]", .{i});
                    try this.walkForPathInfo(s, ref, seg, map);
                    s = this.getValue(s).next;
                    i += 1;
                }
            },
            .object => {
                var s = n.slot0;
                while (s != 0) {
                    const key_node = this.getValue(s);
                    const value_ref = key_node.next;
                    if (key_node.kind == .string) {
                        const key_text = this.getString(key_node);
                        const seg = if (isIdentifier(key_text))
                            try std.fmt.allocPrint(getAllocator(), ".{s}", .{key_text})
                        else
                            try std.fmt.allocPrint(getAllocator(), "[{s}]", .{try this.tryLiteralText(s) orelse "?"});
                        try this.walkForPathInfo(value_ref, ref, seg, map);
                    } else {
                        try this.walkForPathInfo(value_ref, ref, "[?]", map);
                    }
                    s = this.getValue(value_ref).next;
                }
            },
            .computed => {
                try this.walkForPathInfo(n.slot0, ref, ".#subject", map);
                try this.walkForPathInfo(n.slot1, ref, ".#input", map);
            },
            else => {},
        }
    }

    fn formatPath(this: *@This(), map: *const PathInfoMap, ref: ValueRef, out: *std.ArrayList(u8)) !void {
        const info = map.get(ref) orelse {
            try out.appendSlice("?");
            return;
        };
        if (info.parent) |p| try this.formatPath(map, p, out);
        try out.appendSlice(info.path_segment);
    }

    fn formatRelativePath(this: *@This(), map: *const PathInfoMap, from: ValueRef, ref: ValueRef, out: *std.ArrayList(u8)) !void {
        const info = map.get(from) orelse {
            try out.appendSlice("?");
            return;
        };
        
        var obj_parent: ValueRef = 0;
        var p: ValueRef = info.parent orelse return;
        while (true) {
            if (this.getValue(p).kind == .object) {
                obj_parent = p;
                break;
            }
            const p_info = map.get(p) orelse break;
            p = p_info.parent orelse break;
        }
        if (obj_parent == 0) return this.formatPath(map, ref, out);
        
        var tmp = std.ArrayList(u8).init(getAllocator());
        defer tmp.deinit();
        var tmp2 = std.ArrayList(u8).init(getAllocator());
        defer tmp2.deinit();

        try this.formatPath(map, obj_parent, &tmp);
        try this.formatPath(map, ref, &tmp2);

        if (!std.mem.startsWith(u8, tmp2.items, tmp.items)) {
            return out.appendSlice(tmp2.items);
        }

        try out.appendSlice("this");
        try out.appendSlice(tmp2.items[tmp.items.len..]);
    }

    // shows with replacements
    pub fn printGraph(this: *@This()) !void {
        debugPrint("{s}\n", .{try this.printGraphToString()});
    }

    pub fn printGraphToString(this: *@This()) ![]const u8 {
        var path_info = try this.buildPathInfo(this.values.root);
        defer path_info.deinit(getAllocator());

        var out = std.ArrayList(u8).init(getAllocator());
        var path = std.AutoHashMapUnmanaged(ValueRef, void){};
        defer path.deinit(getAllocator());
        try this.printGraphValue(this.values.root, &path_info, &out, &path, 0);
        return out.items;
    }

    fn printGraphValue(
        this: *@This(),
        ref: ValueRef,
        path_info: *const PathInfoMap,
        out: *std.ArrayList(u8),
        path: *std.AutoHashMapUnmanaged(ValueRef, void),
        indent_depth: u32,
    ) anyerror!void {
        if (ref == 0) {
            try out.appendSlice("NUL");
            return;
        }

        const n = this.getValue(ref);

        if (n.kind == .ref) {
            if (ref == this.values.root) {
                try out.writer().print("$", .{});
                return;
            }
            const inner = try this.followRefNode(n);

            if (path_info.contains(inner)) {
                try this.formatRelativePath(path_info, ref, inner, out);
                return;
            }
            const r = this.followReplacements(inner);
            if (path_info.contains(r)) {
                try this.formatRelativePath(path_info, ref, r, out);
                return;
            }
            // try out.writer().print("<{d}", .{n.slot0});
            // try out.appendSlice("> ");
            if (path.contains(ref)) {
                try out.appendSlice("...");
                return;
            }
            try path.put(getAllocator(), ref, {});
            defer _ = path.remove(ref);
            try this.printGraphValue(inner, path_info, out, path, indent_depth);
            return;
        }

        if (path.contains(ref)) {
            try this.formatPath(path_info, ref, out);
            return;
        }
        try path.put(getAllocator(), ref, {});
        defer _ = path.remove(ref);

        switch (n.kind) {
            .NUL => try out.appendSlice("NUL"),
            .undefined => try out.appendSlice("undefined"),
            .null => try out.appendSlice("null"),
            .true => try out.appendSlice("true"),
            .false => try out.appendSlice("false"),
            .number => try out.appendSlice(try this.tryLiteralText(ref) orelse unreachable),
            .string => try out.appendSlice(try this.tryLiteralText(ref) orelse unreachable),
            .array => {
                try out.append('[');
                var s = n.slot0;
                var first = true;
                while (s != 0) {
                    if (!first) try out.appendSlice(", ");
                    first = false;
                    try this.printGraphValue(s, path_info, out, path, indent_depth);
                    s = this.getValue(s).next;
                }
                try out.append(']');
            },
            .computed => {
                try out.append('(');
                var s = n.slot0;
                var first = true;
                while (s != 0) {
                    if (!first) try out.appendSlice(", ");
                    first = false;
                    try this.printGraphValue(s, path_info, out, path, indent_depth);
                    s = this.getValue(s).next;
                }
                try out.append(')');
            },
            .object => {
                try out.append('{');
                if (n.slot0 == 0) return try out.append('}');

                var s = n.slot0;
                var first = true;
                while (s != 0) {
                    var key_node = this.getValue(s);
                    const value_ref = key_node.next;
                    if (!first) try out.appendSlice(", ");
                    first = false;

                    try out.append('\n');
                    for (0..indent_depth + 1) |_| {
                        try out.appendSlice("  ");
                    }

                    if (key_node.kind == .ref) key_node = try this.getFollowedValue(s);

                    if (key_node.kind == .string) {
                        const key_text = this.getString(key_node);
                        if (isIdentifier(key_text)) {
                            try out.appendSlice(key_text);
                        } else {
                            try out.appendSlice(try this.tryLiteralText(s) orelse unreachable);
                        }
                    } else {
                        try this.printGraphValue(s, path_info, out, path, indent_depth);
                    }
                    try out.appendSlice(": ");
                    try this.printGraphValue(value_ref, path_info, out, path, indent_depth + 1);
                    s = this.getValue(value_ref).next;
                }
                try out.append('\n');
                for (0..indent_depth) |_| {
                    try out.appendSlice("  ");
                }
                try out.append('}');
            },
            .ref => unreachable, // handled above
            .@"opaque" => try out.appendSlice("<opaque>"),
        }
    }

    // Reduces references pointing to values that would otherwise not be directly reachable by the graph
    // The first reference that reaches an orphaned value adopts it, every other reference points to the new position. 
    pub fn normalizeRefs(this: *@This(), root: ValueRef) !void {
        this.normalizing = true;
        defer this.normalizing = false;

        this.counts.clearAndFree(getAllocator());

        var direct = std.AutoHashMapUnmanaged(ValueRef, void){};
        defer direct.deinit(getAllocator());
        try this.markDirectReachable(root, &direct);

        var visited = std.AutoHashMapUnmanaged(ValueRef, void){};
        defer visited.deinit(getAllocator());
        try this.normalizeWalk(root, &direct, &visited);

        // simple, albeit ugly
        const r = this.followReplacements(root);
        if (r != root) {
            this.values.nodes.at(root).* = this.values.nodes.at(r).*;
            try this.replacements.put(getAllocator(), r, root);
            _ = this.replacements.remove(root);
        }

        visited.clearRetainingCapacity();
        _ = try this.simplifyReplacements(root, &visited);
        this.replacements.clearAndFree(getAllocator());
    }

    fn markDirectReachable(this: *@This(), ref: ValueRef, direct: *std.AutoHashMapUnmanaged(ValueRef, void)) anyerror!void {
        if (ref == 0) return;
        const resolved = this.followReplacements(ref);
        if (direct.contains(resolved)) return;
        try direct.put(getAllocator(), resolved, {});

        const n = this.values.nodes.at(resolved);
        switch (n.kind) {
            .ref => {},
            .array, .object, .computed => {
                var s = n.slot0;
                while (s != 0) {
                    try this.markDirectReachable(s, direct);
                    s = this.getValue(s).next;
                }
            },
            else => {},
        }
    }

    fn normalizeWalk(
        this: *@This(),
        ref: ValueRef,
        direct: *std.AutoHashMapUnmanaged(ValueRef, void),
        visited: *std.AutoHashMapUnmanaged(ValueRef, void),
    ) anyerror!void {
        if (ref == 0) return;
        const resolved = this.followReplacements(ref);
        if (visited.contains(resolved)) return;
        try visited.put(getAllocator(), resolved, {});

        if (this.counts.get(resolved)) |v| {
            try this.counts.put(getAllocator(), resolved, v + 1);
        } else {
            try this.counts.put(getAllocator(), resolved, 1);
        }

        const n = this.values.nodes.at(resolved);
        switch (n.kind) {
            .ref => {
                const target = try this.followRef(resolved);
                const target_resolved = this.followReplacements(target);
                if (direct.contains(target_resolved)) {
                    try this.counts.put(getAllocator(), target_resolved, (this.counts.get(target_resolved) orelse 0) + 1);
                    return;
                }
                try direct.put(getAllocator(), target_resolved, {});

                this.directReplaceValue(resolved, target_resolved);
                try this.replacements.put(getAllocator(), target, resolved);
                if (target != target_resolved)
                    try this.replacements.put(getAllocator(), target_resolved, resolved);

                _ = direct.remove(resolved);
                _ = visited.remove(resolved);
                // otherwise we double count
                try this.counts.put(getAllocator(), resolved, (this.counts.get(resolved) orelse 0) - 1);

                try this.markDirectReachable(resolved, direct);
                try this.normalizeWalk(resolved, direct, visited);
            },
            .array, .object, .computed => {
                var s = n.slot0;
                while (s != 0) {
                    try this.normalizeWalk(s, direct, visited);
                    s = this.getValue(s).next;
                }
            },
            else => {},
        }
    }

    // mutates in-place
    fn simplifyReplacements(
        this: *@This(),
        ref: ValueRef,
        visited: *std.AutoHashMapUnmanaged(ValueRef, void),
    ) anyerror!ValueRef {
        if (ref == 0) return ref;
        const resolved = this.followReplacements(ref);
        const n = this.values.nodes.at(resolved);
        switch (n.kind) {
            .NUL => return resolved,
            .true, .false, .undefined, .null, .number, .string, .@"opaque" => return resolved,
            else => {},
        }

        if (visited.contains(resolved)) return resolved;
        try visited.put(getAllocator(), resolved, {});

        switch (n.kind) {
            .ref => {
                const inner = try this.followRefNode(n);
                n.slot0 = try this.simplifyReplacements(inner, visited);
            },
            .array, .object, .computed => {
                var s = n.slot0;
                var l: ValueRef = 0;
                while (s != 0) {
                    const next = this.getValue(s).next;
                    const v = try this.simplifyReplacements(s, visited);
                    if (l == 0) n.slot0 = v
                    else this.getValue(l).next = @truncate(v);
                    l = v;
                    s = next;
                }
            },
            else => {},
        }

        return resolved;
    }
};

const AssignmentChecker = struct {
    nodes: *const BumpAllocator(AstNode),
    binder: *const parser.Binder,
    targets: []const parser.SymbolRef,
    found: bool = false,

    fn isTarget(self: *@This(), sym: parser.SymbolRef) bool {
        for (self.targets) |t| {
            if (t == sym) return true;
        }
        return false;
    }

    fn markIfIdent(self: *@This(), ref: NodeRef) void {
        if (ref == 0 or self.found) return;
        const n = self.nodes.at(ref);
        if (n.kind != .identifier) return;
        const sym = self.binder.getSymbol(ref) orelse return;
        if (sym != 0 and self.isTarget(sym)) self.found = true;
    }

    pub fn visit(self: *@This(), node: *const AstNode, ref: NodeRef) anyerror!void {
        if (self.found or ref == 0) return;
        switch (node.kind) {
            .binary_expression => {
                if (parser.isAssignmentOp(@enumFromInt(node.len))) {
                    self.markIfIdent(getPackedData(node).left);
                }
                try parser.forEachChild(self.nodes, node, self);
            },
            .prefix_unary_expression => {
                const d = getPackedData(node);
                const op: SyntaxKind = @enumFromInt(d.left);
                if (op == .plus_plus_token or op == .minus_minus_token) self.markIfIdent(d.right);
                try parser.forEachChild(self.nodes, node, self);
            },
            .postfix_unary_expression => {
                const d = getPackedData(node);
                const op: SyntaxKind = @enumFromInt(d.right);
                if (op == .plus_plus_token or op == .minus_minus_token) self.markIfIdent(d.left);
                try parser.forEachChild(self.nodes, node, self);
            },
            else => try parser.forEachChild(self.nodes, node, self),
        }
    }
};

fn containsAssignmentToAny(
    nodes: *const BumpAllocator(AstNode),
    binder: *const parser.Binder,
    ref: NodeRef,
    targets: []const parser.SymbolRef,
) !bool {
    var checker = AssignmentChecker{ .nodes = nodes, .binder = binder, .targets = targets };
    try checker.visit(nodes.at(ref), ref);
    return checker.found;
}

const AssignedParamsCollector = struct {
    nodes: *const BumpAllocator(AstNode),
    binder: *const parser.Binder,
    targets: []const parser.SymbolRef,
    found: *std.AutoArrayHashMapUnmanaged(parser.SymbolRef, void),

    fn markIfIdent(self: *@This(), ref: NodeRef) !void {
        if (ref == 0) return;
        const n = self.nodes.at(ref);
        if (n.kind != .identifier) return;
        const sym = self.binder.getSymbol(ref) orelse return;
        if (sym == 0) return;
        for (self.targets) |t| {
            if (t == sym) {
                try self.found.put(getAllocator(), sym, {});
                return;
            }
        }
    }

    pub fn visit(self: *@This(), node: *const AstNode, ref: NodeRef) anyerror!void {
        if (ref == 0) return;
        switch (node.kind) {
            .binary_expression => {
                if (parser.isAssignmentOp(@enumFromInt(node.len))) {
                    try self.markIfIdent(getPackedData(node).left);
                }
                try parser.forEachChild(self.nodes, node, self);
            },
            .prefix_unary_expression => {
                const d = getPackedData(node);
                const op: SyntaxKind = @enumFromInt(d.left);
                if (op == .plus_plus_token or op == .minus_minus_token) try self.markIfIdent(d.right);
                try parser.forEachChild(self.nodes, node, self);
            },
            .postfix_unary_expression => {
                const d = getPackedData(node);
                const op: SyntaxKind = @enumFromInt(d.right);
                if (op == .plus_plus_token or op == .minus_minus_token) try self.markIfIdent(d.left);
                try parser.forEachChild(self.nodes, node, self);
            },
            else => try parser.forEachChild(self.nodes, node, self),
        }
    }
};

fn collectAssignedParams(
    nodes: *const BumpAllocator(AstNode),
    binder: *const parser.Binder,
    ref: NodeRef,
    targets: []const parser.SymbolRef,
    out: *std.AutoArrayHashMapUnmanaged(parser.SymbolRef, void),
) !void {
    var collector = AssignedParamsCollector{ .nodes = nodes, .binder = binder, .targets = targets, .found = out };
    try collector.visit(nodes.at(ref), ref);
}

// rewrites $N placeholders to some other $N placeholder
fn remapPlaceholders(template: []const u8, index_map: []const u32) ![]const u8 {
    var out = std.ArrayList(u8).init(getAllocator());
    var i: usize = 0;
    while (i < template.len) {
        if (template[i] == '$' and i + 1 < template.len and template[i + 1] >= '0' and template[i + 1] <= '9') {
            var j = i + 1;
            while (j < template.len and template[j] >= '0' and template[j] <= '9') j += 1;
            const n = try std.fmt.parseInt(u32, template[i + 1 .. j], 10);
            const mapped = if (n < index_map.len) index_map[n] else n;
            try out.writer().print("${d}", .{mapped});
            i = j;
        } else {
            try out.append(template[i]);
            i += 1;
        }
    }
    return out.items;
}

// rewrites $N placeholders to arbitrary text
fn substitutePlaceholders(template: []const u8, subst: []const []const u8) ![]const u8 {
    var out = std.ArrayList(u8).init(getAllocator());
    var i: usize = 0;
    while (i < template.len) {
        if (template[i] == '$' and i + 1 < template.len and template[i + 1] >= '0' and template[i + 1] <= '9') {
            var j = i + 1;
            while (j < template.len and template[j] >= '0' and template[j] <= '9') j += 1;
            const n = try std.fmt.parseInt(u32, template[i + 1 .. j], 10);
            if (n < subst.len) {
                try out.appendSlice(subst[n]);
            } else {
                try out.appendSlice(template[i..j]);
            }
            i = j;
        } else {
            try out.append(template[i]);
            i += 1;
        }
    }
    return out.items;
}

// :/
fn allOccurrencesIndexZero(text: []const u8, name: []const u8) bool {
    var i: usize = 0;
    var found_any = false;
    while (std.mem.indexOfPos(u8, text, i, name)) |pos| {
        const before_ok = pos == 0 or !isIdentPart(text[pos - 1]);
        const end = pos + name.len;
        const after_ok = end >= text.len or !isIdentPart(text[end]);
        if (before_ok and after_ok) {
            found_any = true;
            if (end + 3 > text.len or !std.mem.eql(u8, text[end .. end + 3], "[0]")) return false;
        }
        i = pos + 1;
    }
    return found_any;
}

const is_debug = @import("builtin").mode == .Debug;

// treat this as a union
// this is meant for coarse analysis and cannot be used for much beyond basic dce
// the facts are designed to always decrease useful information as you add more of them. adding another fact should produce broader facts downstream, not change the underlying.
const ValueFacts = enum(u32) {
    none = 0,
    null = 1 << 0,
    undefined = 1 << 1,
    true = 1 << 2,
    false = 1 << 3,

    object = 1 << 4, // we do not include null
    array = 1 << 5, 
    number = 1 << 6, 
    string = 1 << 7, 
    function = 1 << 8, 
    symbol = 1 << 9, 

    zero = 1 << 15, // means "zero sized", modifies all other facts to include zero-width variants e.g. numeric zero or empty string
    falsy = 1 << 20, // negative integers and zero are treated as falsy because we _might_ increment them later. the inclusion of `number` makes the total truthy and falsy.
    complex = 1 << 24,
    decorated = 1 << 25, // e.g. extra descriptors and/or accessors. also used for negative numbers.
    float = 1 << 26,
    throws = 1 << 27,
    infallible = 1 << 28,
    any = 1 << 29,
    incomplete = 1 << 30, // this is negated so the "exact fact" path is simpler
    literal = 1 << 31, // struct is external

    nullish = (1 << 0) | (1 << 1),
    boolean = (1 << 2) | (1 << 3),
    truthy = (1 << 2) | (1 << 4) | (1 << 5) | (1 << 8) | (1 << 9),
    number_or_string = (1 << 6) | (1 << 7),
    falsy_number_or_string = (1 << 6) | (1 << 7) | (1 << 20),

    numeric_zero = (1 << 6) | (1 << 15), // number | zero
    any_string = (1 << 7) | (1 << 15), // string | zero
    negative_number = (1 << 6) | (1 << 25), // number | decorated
    any_number = (1 << 6) | (1 << 15) | (1 << 25), // number | zero | decorated
    object_or_array_or_null = (1 << 4) | (1 << 5) | (1 << 0),
    any_structural_value = (1 << 4) | (1 << 5) | (1 << 8), // array | object | function
    any_non_structural_non_nullish_primitive = (1 << 2) | (1 << 3) | (1 << 6) | (1 << 7) | (1 << 9) | (1 << 15) | (1 << 25), // nullish | boolean | number | string | symbol | zero | decorated

    _falsy = (1 << 0) | (1 << 1) | (1 << 2) | (1 << 3) | (1 << 6) | (1 << 7) | (1 << 15),

    // excludes atoms
    primitive_types = (1 << 4) | (1 << 5) | (1 << 6) | (1 << 7) | (1 << 8) | (1 << 9),

    pub fn hasFact(facts: u32, fact: ValueFacts) bool {
        return (facts & @intFromEnum(fact)) != 0;
    }

    pub fn hasExactFact(facts: u32, fact: ValueFacts) bool {
        return facts == @intFromEnum(fact);
    }

    pub fn hasExactPrimitiveType(facts: u32, fact: ValueFacts) bool {
        return (facts & @intFromEnum(ValueFacts.primitive_types)) == @intFromEnum(fact);
    }

    pub fn excludeFact(facts: u32, comptime fact: ValueFacts) u32 {
        return facts & ~@intFromEnum(fact);
    }

    pub fn maskFact(facts: u32, fact: ValueFacts) u32 {
        return facts & @intFromEnum(fact);
    }

    pub fn isAny(facts: u32) bool {
        return hasFact(facts, .any) or hasFact(facts, .incomplete);
    }

    pub fn isComplete(facts: u32) bool {
        return !hasFact(facts, .incomplete);
    }

    pub fn isComplex(facts: u32) bool {
        return hasFact(facts, .decorated);
    }

    pub fn hasNullish(facts: u32) bool {
        return hasFact(facts, .nullish);
    }

    pub fn hasAnyFalsy(facts: u32) bool {
        return hasNullish(facts) or hasFact(facts, .false) or hasFact(facts, .falsy) or hasExactFact(facts, .any_string) or hasExactFact(facts, .numeric_zero);
    }

    pub fn isDefinitelyTruthy(facts: u32) bool {
        if (!isComplete(facts)) return false;
        if (hasAnyFalsy(facts)) return false;
        if (isAny(facts)) return false;
        return hasFact(facts, .truthy);
    }

    pub fn coerceBool(a: u32) u32 {
        if (isAny(a)) return a;
        const truthy = hasFact(a, .truthy) or hasFact(a, .number_or_string) or hasFact(a, .true);
        const falsy = hasAnyFalsy(a);
        if (truthy and !falsy) return @intFromEnum(ValueFacts.true);
        if (falsy and !truthy) return @intFromEnum(ValueFacts.false);
        return @intFromEnum(ValueFacts.boolean);
    }

    pub fn logicalAnd(a: u32, b: u32) u32 {
        return (a & b) | maskFact(a, ._falsy);
    }

    pub fn logicalOr(a: u32, b: u32) u32 {
        return excludeFact(a, ._falsy) | b;
    }

    pub fn nullishCoalesce(a: u32, b: u32) u32 {
        return excludeFact(a, .nullish) | b;
    }

    pub fn relates(a: u32, b: u32, op: parser.SyntaxKind) u32 {
        // todo: string
        if (isAny(a | b)) return @intFromEnum(ValueFacts.boolean);
        if (!hasExactPrimitiveType(a | b, .number) or hasFact(a | b, .float)) return @intFromEnum(ValueFacts.boolean);
        const delta: u8 = @popCount(a) - @popCount(b);
        if (delta == 0) return @intFromEnum(ValueFacts.boolean);
        _ = op;
        // TODO: we currently cannot prove much for comparisons because there is no "exclusively negative" fact
        // we could make .zero by itself be exclusively the numeric zero, we just need to adjust primitive type checks to account for this
        // we would do the same thing for ".decorated": on its own, it would mean _exclusively_ a negative number. 
        return @intFromEnum(ValueFacts.boolean);
    }

    pub fn negate(a: u32) u32 {
        if (hasExactFact(a, .true)) return @intFromEnum(ValueFacts.false);
        if (hasExactFact(a, .false)) return @intFromEnum(ValueFacts.true);
        if (isDefinitelyTruthy(a) and !hasAnyFalsy(a)) return @intFromEnum(ValueFacts.false);
        if (!isDefinitelyTruthy(a) and hasAnyFalsy(a)) return @intFromEnum(ValueFacts.true);
        // TODO: nullish
        return @intFromEnum(ValueFacts.boolean);
    }

    pub fn strictEquals(_a: u32, _b: u32) u32 {
        const a = _a & ~@intFromEnum(ValueFacts.complex);
        const b = _b & ~@intFromEnum(ValueFacts.complex);

        if (@popCount(a) != @popCount(b)) {
            return @intFromEnum(ValueFacts.boolean);
        }
        if (a != b) {
            if (a <= @intFromEnum(ValueFacts.false) or b <= @intFromEnum(ValueFacts.false)) {
                if (@popCount(a) != 1) return @intFromEnum(ValueFacts.boolean);
                return @intFromEnum(ValueFacts.false);
            }
            // also too conservative
            return @intFromEnum(ValueFacts.boolean);
        }
        if (a <= @intFromEnum(ValueFacts.false)) {
            return @intFromEnum(ValueFacts.true);
        }
        return @intFromEnum(ValueFacts.boolean);
    }

    pub fn arithmeticNegation(a: u32) u32 {
        if (a == @intFromEnum(ValueFacts.zero)) return a;
        if (a == @intFromEnum(ValueFacts.null) or a == @intFromEnum(ValueFacts.false)) return @intFromEnum(ValueFacts.numeric_zero);
        if (a == @intFromEnum(ValueFacts.true)) return @intFromEnum(ValueFacts.negative_number);
        // todo: should set whatever flag for NaN if we have other primitives beyond number
        if (!hasExactPrimitiveType(a, .number)) return @intFromEnum(ValueFacts.any);
        return a ^ @intFromEnum(ValueFacts.decorated);
    }

    pub fn arithmeticAdd(a: u32, b: u32) u32 {
        var r = a | b;
        if ((a & @intFromEnum(ValueFacts.primitive_types) & ~@intFromEnum(ValueFacts.number_or_string)) != 0) {
            return @intFromEnum(ValueFacts.any);
        }

        // positive + negative OR negative + positive can be zero
        const should_add_zero = ((a ^ b) & @intFromEnum(ValueFacts.decorated)) != 0;
        // 0 + 1 -> non-zero
        // 0 + 0 -> zero
        // '' + 'aa' -> non-zero
        // 1 + 1 -> non-zero
        // etc.
        const should_remove_zero = ((r & @intFromEnum(ValueFacts.decorated)) == 0) and (a & b & @intFromEnum(ValueFacts.zero)) == 0;

        if (should_add_zero) r |= @intFromEnum(ValueFacts.zero);
        if (should_remove_zero) r = excludeFact(r, .zero);

        // strings dominate
        if (hasFact(r, .string)) r = excludeFact(r, .number);
        
        return r;
    }

    pub fn printInfo(a: u32) void {
        debugPrint("any: {}\n", .{hasFact(a, .any)});
        debugPrint("incomplete: {}\n", .{hasFact(a, .incomplete)});
        debugPrint("true: {}\n", .{hasFact(a, .true)});
        debugPrint("false: {}\n", .{hasFact(a, .false)});
        debugPrint("truthy: {}\n", .{isDefinitelyTruthy(a)});
        debugPrint("falsy: {}\n", .{hasAnyFalsy(a)});
        debugPrint("null: {}\n", .{hasFact(a, .null)});
        debugPrint("undefined: {}\n", .{hasFact(a, .undefined)});
        debugPrint("array: {}\n", .{hasFact(a, .array)});
        debugPrint("object: {}\n", .{hasFact(a, .object)});
        debugPrint("string: {}\n", .{hasFact(a, .any_string)});
        debugPrint("number: {}\n", .{hasFact(a, .any_number)});
    }
};

// we may eventually use `program.zig` directly, using types to model facts instead
// but we need the idea of a "range" type first. the current architecture for value
// facts is kept a bit more local than types and so does not need machinery for modules
const ValueFactLiteral = packed struct {
    const LiteralKind = enum(u4) {
        number,
        string,
        array,
        object,
        symbol,
        function,
        combination, // union or intersection
        external, // points into another computation
    };
    const LiteralFlags = enum(u3) {
        possibly_undefined = 1 << 0,
    };
    const CombinationFlags = enum(u3) {
        intersection = 1 << 2,
    };
    const NumberFlags = enum(u3) {
        range = 1 << 2, // ranges are always i32 starts/ends inclusive, with the max val representing infinity
    };

    kind: LiteralKind,
    flags: u3 = 0,
    next: u25 = 0,
    slot0: u32 = 0,
    slot1: u32 = 0,
    slot2: u32 = 0,

    comptime {
        if (@sizeOf(@This()) != 16) {
            @compileError("Expected literal struct to be 16 bytes");
        }
    }

    pub fn createNumber(v: f64) @This() {
        const u: u64 = @bitCast(v);
        return .{
            .kind = .number,
            .slot0 = @truncate(u >> 32),
            .slot1 = @truncate(u),
        };
    }

    pub fn createRange(from: i32, to: i32) @This() {
        return .{
            .kind = .number,
            .slot0 = @bitCast(from),
            .slot1 = @bitCast(to),
            .slot2 = @intFromEnum(NumberFlags.range),
        };
    }

    pub fn createString(v: []const u8) @This() {
        return .{
            .kind = .string,
            .slot0 = if (comptime @import("builtin").target.isWasm()) 0 else @truncate(@intFromPtr(v.ptr) >> 32),
            .slot1 = @truncate(@intFromPtr(v.ptr)),
            .slot2 = @truncate(v.len),
        };
    }

    pub fn collapse(this: *const @This()) u32 {
        var r: u32 = 0;
        switch (this.kind) {
            .number => {
                r |= @intFromEnum(ValueFacts.number);
                if (this.isRange()) {
                    const start = this.getRangeStart();
                    const end = this.getRangeEnd();
                    if (start < 0) {
                        r |= @intFromEnum(ValueFacts.negative_number);
                    } 
                    if (start == 0 or end == 0 or (start < 0 and end > 0)) {
                        r |= @intFromEnum(ValueFacts.zero);
                    }
                } else {
                    const v = this.getDouble();
                    if (v == 0) r |= @intFromEnum(ValueFacts.zero);
                    if (v < 0) r |= @intFromEnum(ValueFacts.negative_number);
                }
            },
            .string => {
                r |= @intFromEnum(ValueFacts.string);
                if (this.getLengthOrCount() == 0) r |= @intFromEnum(ValueFacts.zero);
            },
            .array => {
                r |= @intFromEnum(ValueFacts.array);
                if (this.getLengthOrCount() == 0) r |= @intFromEnum(ValueFacts.zero);
            },
            .object => {
                r |= @intFromEnum(ValueFacts.object);
                if (this.getLengthOrCount() == 0) r |= @intFromEnum(ValueFacts.zero);
            },
            .symbol => {
                r |= @intFromEnum(ValueFacts.symbol);
            },
            .function => {
                r |= @intFromEnum(ValueFacts.function);
            },
            else => {
                r |= @intFromEnum(ValueFacts.any);
            }
        }
        if (this.flags & @intFromEnum(LiteralFlags.possibly_undefined) != 0) {
            r |= @intFromEnum(ValueFacts.undefined);
        }
        return r;
    }

    pub fn isRange(this: *const @This()) bool {
        return this.kind == .number and (this.flags & @intFromBool(NumberFlags.range)) != 0;
    }

    pub fn getDouble(this: *const @This()) f64 {
        std.debug.assert(this.kind == .number and !this.isRange());
        const u: u64 = (@as(u64, this.slot0) << 32) | this.slot1;
        return @bitCast(u);
    }

    pub fn getRangeStart(this: *const @This()) i32 {
        std.debug.assert(this.isRange());
        return @bitCast(this.slot0);
    }

    pub fn getRangeEnd(this: *const @This()) i32 {
        std.debug.assert(this.isRange());
        return @bitCast(this.slot1);
    }

    pub fn getString(this: *const @This()) []const u8 {
        std.debug.assert(this.kind == .string);
        return triplesToSlice(u8, this.slot0, this.slot1, this.slot2);
    }

    pub fn getLengthOrCount(this: *const @This()) u32 {
        return switch (this.kind) {
            .string, .array, .object => this.slot2,
            else => 0,
        };
    }

};

const ReferenceCollector = struct {
    file: *parser.ParsedFile,
    parents: std.AutoHashMapUnmanaged(parser.NodeRef, parser.NodeRef) = .{},
    parents_watermark: usize = 1,
    // excludes the decl reference
    references: std.AutoHashMapUnmanaged(parser.SymbolRef, std.ArrayListUnmanaged(parser.NodeRef)) = .{},
    stack: std.ArrayListUnmanaged(parser.NodeRef) = .{},
    
    decl_count: u32 = 0,
    external_facts: std.AutoHashMapUnmanaged(parser.SymbolRef, u32) = .{},
    value_facts: std.AutoHashMapUnmanaged(parser.SymbolRef, u32) = .{},
    value_literals: @import("./lexer.zig").SizedBumpAllocator(8, ValueFactLiteral) = undefined,

    pub fn init(file: *parser.ParsedFile) !@This() {
        var self = @This(){ .file = file };
        self.value_literals = @import("./lexer.zig").SizedBumpAllocator(8, ValueFactLiteral).init(getAllocator());
        try self.visit(file.ast.nodes.at(file.ast.start), file.ast.start);
        return self;
    }

    pub fn deinit(self: *@This()) void {
        self.parents.deinit(getAllocator());
        self.stack.deinit(getAllocator());
        var iter = self.references.valueIterator();
        while (iter.next()) |arr| {
            arr.deinit(getAllocator());
        }
        self.references.deinit(getAllocator());
        self.value_facts.deinit(getAllocator());
        self.external_facts.deinit(getAllocator());
        self.value_literals.deinit();
    }

    // mutates the nodes allocator and resets both maps
    pub fn commitInPlace(
        self: *@This(),
        replacements: *std.AutoArrayHashMap(NodeRef, NodeRef),
        symbol_replacements: *std.AutoArrayHashMapUnmanaged(parser.SymbolRef, NodeRef)
    ) !void {
        const V = struct {
            parent: NodeRef = 0,
            prev: NodeRef = 0,
            nodes: *BumpAllocator(parser.AstNode),
            replacements: *std.AutoArrayHashMap(NodeRef, NodeRef),
            symbol_replacements: *std.AutoArrayHashMapUnmanaged(parser.SymbolRef, NodeRef),

            fn findSelfPtr(this: *@This(), old: NodeRef) *NodeRef {
                if (this.prev != 0) {
                    const ptr = &this.nodes.at(this.prev).next;
                    if (ptr.* == old) return ptr;
                }
                if (this.parent == 0) unreachable;
                var ptr: *NodeRef = undefined;
                const p = this.nodes.at(this.parent);
                std.debug.assert(!parser.isLeafNode(p.kind));
                const slots: *[8]u32 = @ptrCast(p);
                switch (p.kind) {
                    .prefix_unary_expression => {
                        ptr = &slots[3];
                    },
                    .postfix_unary_expression => {
                        ptr = &slots[2];
                    },
                    .binary_expression => {
                        if (slots[2] == old) ptr = &slots[2]
                        else if (slots[3] == old) ptr = &slots[3]
                        else unreachable;
                    },
                    else => {
                        for (2..8) |i| {
                            if (slots[i] == old) {
                                ptr = &slots[i];
                                break;
                            }
                        }
                    },
                }
                std.debug.assert(ptr.* == old);
                return ptr;
            }

            fn replaceRef(this: *@This(), old: NodeRef, new: NodeRef) void {
                const ptr = this.findSelfPtr(old);
                ptr.* = new;
            }

            pub fn visit(this: *@This(), node: *const AstNode, ref: NodeRef) anyerror!void {
                if (this.replacements.get(ref)) |x| {
                    this.replaceRef(ref, x);
                    return this.visit(this.nodes.at(x), x);
                }
                switch (node.kind) {
                    .identifier => {
                        if (this.symbol_replacements.get(node.extra_data)) |x| {
                            this.replaceRef(ref, x);
                            return this.visit(this.nodes.at(x), x);
                        }
                    },
                    .not_emitted_statement => {
                        // have to simplify the tree for later passes
                        return this.replaceRef(ref, node.next);
                    },
                    .block => {
                        if (node.hasFlag(.reduce_block)) {
                            this.replaceRef(ref, unwrapRef(node));
                            return try parser.forEachChild(this.nodes, this.nodes.at(ref), this);
                        }
                    },
                    .variable_declaration => {
                        // we are stamping the relative decl order to these nodes as an optimization and must clear them here
                        this.nodes.at(ref).extra_data2 = 0;
                    },
                    else => {},
                }
                if (parser.isLeafNode(node.kind)) {
                    this.prev = ref;
                    return;
                }
                const save_parent = this.parent;
                this.parent = ref;
                this.prev = 0;
                try parser.forEachChild(this.nodes, this.nodes.at(ref), this);
                this.parent = save_parent;
                this.prev = ref;
            }
        };
        var v = V{
            .nodes = &self.file.ast.nodes,
            .replacements = replacements,
            .symbol_replacements = symbol_replacements,
        };
        const start = self.file.ast.start;
        std.debug.assert(!replacements.contains(start));
        try v.visit(self.file.ast.nodes.at(start), start);
        {
            const sym_count = self.file.binder.symbols.count();
            for (0..sym_count) |i| {
                const sym = self.file.binder.symbols.at(i);
                if (sym.hasFlag(.late_bound)) continue;
                if (sym.declaration == 0) continue;
                if (replacements.get(sym.declaration)) |x| {
                    sym.declaration = x;
                }
            }
        }
        replacements.clearAndFree();
        symbol_replacements.clearAndFree(getAllocator());
        self.decl_count = 0;
        self.parents_watermark = 1;
        self.parents.clearRetainingCapacity();
        self.references.clearRetainingCapacity();
        self.value_facts.clearRetainingCapacity();
        try self.visit(self.file.ast.nodes.at(start), start);
    }

    fn markParents(self: *@This()) !void {
        var i: usize = self.parents_watermark;
        std.debug.assert(i >= 1);
        while (i < self.stack.items.len) {
            try self.parents.put(getAllocator(), self.stack.items[i], self.stack.items[i - 1]);
            i += 1;
        }
        self.parents_watermark = self.stack.items.len;
    }

    fn addReference(self: *@This(), sym_ref: parser.SymbolRef, node_ref: parser.NodeRef) !void {
        const entry = try self.references.getOrPut(getAllocator(), sym_ref);
        if (!entry.found_existing) {
            entry.value_ptr.* = std.ArrayListUnmanaged(parser.NodeRef){};
        }
        try entry.value_ptr.append(getAllocator(), node_ref);
    }

    pub fn createLiteralFact(self: *@This(), literal: ValueFactLiteral) !u32 {
        const c = self.value_literals.count();
        if (c == 0) {
            try self.value_literals.warmup();
        }
        try self.value_literals.append(literal);
        return @intFromEnum(ValueFacts.literal) | c;
    }

    pub fn visit(self: *@This(), node: *const AstNode, ref: NodeRef) anyerror!void {
        if (node.kind == .identifier) {
            const sym_ref = self.file.binder.getSymbol(ref) orelse return;
            const sym = self.file.binder.symbols.at(sym_ref);
            if (sym.isImportedOrExported()) return;
            try self.markParents();
            try self.parents.put(getAllocator(), ref, self.stack.getLast());
            if (sym.binding == ref) return;
            if (sym.declaration != 0) {
                if (parser.getDeclarationNameRef(self.file.ast.nodes.at(sym.declaration)) == ref) return;
            }
            try self.addReference(sym_ref, ref);
            return;
        }
        if (parser.isLeafNode(node.kind)) return;
        if (node.kind == .variable_declaration) {
            self.decl_count += 1;
            self.file.ast.nodes.at(ref).extra_data2 = self.decl_count;
        }
        try self.stack.append(getAllocator(), ref);
        defer {
            _ = self.stack.pop();
            self.parents_watermark = @min(self.stack.items.len, self.parents_watermark);
        }
        try parser.forEachChild(&self.file.ast.nodes, node, self);
    }

    const ReferenceIterator = struct {
        collector: *const ReferenceCollector,
        references: *const std.ArrayListUnmanaged(parser.NodeRef),
        sym_ref: parser.SymbolRef,
        index: usize = 0,

        inline fn getNodes(this: *const @This()) *const parser.BumpAllocator(parser.AstNode) {
            return &this.collector.file.ast.nodes;
        }

        fn parent(this: *const @This(), ref: parser.NodeRef) ?*const parser.AstNode {
            return this.getNodes().at(this.parentRef(ref) orelse return null);
        }

        fn parentRef(this: *const @This(), ref: NodeRef) ?NodeRef {
            return this.collector.parents.get(ref);
        }

        // intentionally skips over postfix/prefix unary expressions
        pub fn getAssignedValue(this: *const @This(), ref: NodeRef) ?NodeRef {
            const res = this._getAssignmentNode(ref) orelse return null;
            return res[2];
        }

        pub fn getAssignedTarget(this: *const @This(), ref: NodeRef) ?NodeRef {
            const res = this._getAssignmentNode(ref) orelse return null;
            return res[1];
        }

        pub fn getAssignmentNode(this: *const @This(), ref: NodeRef) ?NodeRef {
            const res = this._getAssignmentNode(ref) orelse return null;
            return res[0];
        }

        // (parent, target, value?)
        pub fn _getAssignmentNode(this: *const @This(), ref: NodeRef) ?struct { NodeRef, NodeRef, ?NodeRef } {
            const nodes = this.getNodes();
            var current_ref = ref;
            var parent_ref = this.parentRef(ref) orelse return null;
            while (true) {
                const p = nodes.at(parent_ref);
                switch (p.kind) {
                    .parenthesized_expression => {
                        current_ref = parent_ref;
                        parent_ref = this.parentRef(current_ref) orelse break;
                    },
                    .binary_expression => {
                        if (parser.isAssignmentOp(@enumFromInt(p.len))) {
                            if (getLeft(p) != current_ref) break;
                            return .{parent_ref, current_ref, getRight(p)};
                        }
                        break;
                    },
                    .prefix_unary_expression => {
                        const op: SyntaxKind = @enumFromInt(getLeft(p));
                        if (op == .plus_plus_token or op == .minus_minus_token) {
                            std.debug.assert(getRight(p) == current_ref);
                            return .{parent_ref, current_ref, null};
                        }
                        break;
                    },
                    .postfix_unary_expression => {
                        const op: SyntaxKind = @enumFromInt(getRight(p));
                        if (op == .plus_plus_token or op == .minus_minus_token) {
                            std.debug.assert(getLeft(p) == current_ref);
                            return .{parent_ref, current_ref, null};
                        }   
                        break;
                    },
                    else => break,
                }
            }
            return null;
        }

        // a direct assignment counts as an alias e.g. `const y = x` means y aliases x
        pub fn getAlias(this: *const @This(), ref: NodeRef) ?NodeRef {
            const nodes = this.getNodes();
            var current_ref = ref;
            var parent_ref = this.parentRef(ref) orelse return null;
            while (true) {
                const p = nodes.at(parent_ref);
                switch (p.kind) {
                    .await_expression, .parenthesized_expression => {
                        current_ref = parent_ref;
                        parent_ref = this.parentRef(current_ref) orelse break;
                    },
                    .binary_expression => {
                        if (parser.isAssignmentOp(@enumFromInt(p.len))) {
                            if (getPackedData(p).right != current_ref) break;
                            return getPackedData(p).left;
                        }
                        break;
                    },
                    // TODO: binding element?
                    .parameter, .variable_declaration => {
                        if (getPackedData(p).right != current_ref) break;
                        const l = getPackedData(p).left;
                        // if (nodes.at(l).kind != .identifier) break;
                        return l;
                    },
                    else => break,
                }
            }
            return null;
        }

        pub fn getEscapeTarget(this: *const @This(), ref: NodeRef) ?NodeRef {
            const nodes = this.getNodes();
            var current_ref = ref;
            var parent_ref = this.parentRef(ref) orelse return null;
            while (true) {
                const p = nodes.at(parent_ref);
                switch (p.kind) {
                    .parenthesized_expression => {
                        current_ref = parent_ref;
                        parent_ref = this.parentRef(current_ref) orelse break;
                    },
                    .shorthand_property_assignment,
                    .await_expression, .array_literal_expression, .object_literal_expression => return parent_ref,
                    .property_assignment => {
                        if (getRight(p) != current_ref) break;
                        return parent_ref;
                    },
                    .call_expression, .new_expression => {
                        const target = getLeft(p);
                        if (target == current_ref) break;
                        // TODO: it'd be useful to know which argument position
                        return target;
                    },
                    // a few other expressions can cause escapes
                    // for example, coercion can trigger `toPrimitive`, `instanceof` can dispatch
                    else => break,
                }
            }
            return null;
        }

        // can be an identifier (from dotted access) OR some arbitrary exp from element access
        pub fn getAccessExpression(this: *const @This(), ref: NodeRef) ?NodeRef {
            const nodes = this.getNodes();
            var current_ref = ref;
            var parent_ref = this.parentRef(ref) orelse return null;
            while (true) {
                const p = nodes.at(parent_ref);
                switch (p.kind) {
                    .await_expression, .parenthesized_expression => {
                        current_ref = parent_ref;
                        parent_ref = this.parentRef(current_ref) orelse break;
                    },
                    .property_access_expression, .element_access_expression => {
                        const d = getPackedData(p);
                        if (d.left != current_ref) break;
                        return d.right;
                    },
                    else => break,
                }
            }
            return null;
        }

        // FIXME: duplicated with the above
        pub fn getAccessExpressionLeft(this: *const @This(), ref: NodeRef) ?NodeRef {
            const nodes = this.getNodes();
            var current_ref = ref;
            var parent_ref = this.parentRef(ref) orelse return null;
            while (true) {
                const p = nodes.at(parent_ref);
                switch (p.kind) {
                    .await_expression, .parenthesized_expression => {
                        current_ref = parent_ref;
                        parent_ref = this.parentRef(current_ref) orelse break;
                    },
                    .property_access_expression, .element_access_expression => {
                        const d = getPackedData(p);
                        if (d.left != current_ref) break;
                        return current_ref;
                    },
                    else => break,
                }
            }
            return null;
        }

        pub fn next(this: *@This()) ?NodeRef {
            if (this.index == this.references.items.len) return null;
            const r = this.references.items[this.index];
            this.index += 1;
            return r;
        }
    };

    // must be inline due to the stack references
    inline fn getReferenceIterator(self: *const @This(), sym_ref: parser.SymbolRef) ?ReferenceIterator {
        const references = self.references.get(sym_ref) orelse return null;
        return .{ .collector = self, .references = &references, .sym_ref = sym_ref };
    }

    fn equalAccessExps(self: *@This(), a: NodeRef, b: NodeRef) bool {
        const na = self.file.ast.nodes.at(a);
        const nb = self.file.ast.nodes.at(b);
        if (na.kind == .numeric_literal and nb.kind == .numeric_literal) {
            return na.data == nb.data;
        }
        return false;
    }

    pub fn getSingularAccessExp(self: *@This(), sym_ref: parser.SymbolRef) ?NodeRef {
        var iter = self.getReferenceIterator(sym_ref) orelse return null;
        var cur: ?NodeRef = null;
        while (iter.next()) |r| {
            const exp = iter.getAccessExpression(r) orelse return null;
            if (cur) |c| {
                if (!self.equalAccessExps(c, exp)) return null;
            } else cur = exp;
        }
        return cur;
    }

    pub fn isAssignedTo(self: *@This(), sym_ref: parser.SymbolRef) bool {
        var iter = self.getReferenceIterator(sym_ref) orelse return false;
        while (iter.next()) |r| {
            if (iter.getAssignmentNode(r) != null) return true;
        }
        return false;
    }

    // this includes assignments
    fn isEscapePosition(self: *@This(), ref: NodeRef) bool {
        var c = ref;
        while (c != 0) {
            const p_ref = self.parents.get(c) orelse return false;
            const p = self.file.ast.nodes.at(p_ref);
            switch (p.kind) {
                .parenthesized_expression => {
                    c = p_ref;
                },
                .shorthand_property_assignment,
                .await_expression, .array_literal_expression, .object_literal_expression => return true,
                .property_assignment => {
                    if (getRight(p) != c) break;
                    return true;
                },
                .call_expression, .new_expression => {
                    if (getLeft(p) == c) break;
                    return true;
                },
                .binary_expression => {
                    if (parser.isAssignmentOp(@enumFromInt(p.len))) {
                        return getRight(p) == c;
                    }
                    break;
                },
                else => break,
            }
        }
        return false;
    }

    // returns the node right below the scope node as well
    fn getScopeRef(self: *@This(), ref: NodeRef) ?struct {NodeRef, NodeRef} {
        var c = ref;
        while (c != 0) {
            const p_ref = self.parents.get(c) orelse return null;
            const p = self.file.ast.nodes.at(p_ref);
            switch (p.kind) {
                .source_file, .block => return .{c, p_ref},
                .arrow_function => return .{c, p_ref},
                .method_declaration, .get_accessor, .set_accessor,
                .class_declaration, .class_expression,
                .function_declaration, .function_expression => {
                    if (c != getLeft(p)) return .{c, p_ref};
                },
                else => {},
            }
            c = p_ref;
        }
        return null;
    }

    pub fn hasEscapedBeforeExp(self: *@This(), sym_ref: parser.SymbolRef, exp_ref: NodeRef) !bool {
        var iter = self.getReferenceIterator(sym_ref) orelse return false;
        while (iter.next()) |r| {
            if (r > exp_ref) break;
            if (iter._getAssignmentNode(r) != null) continue;
            // if (iter._getAssignmentNode(r) != null) return true;
            if (self.isEscapePosition(r)) return true;
        }
        return false;
    }

    fn isSimpleAccessExpressionOfSym(self: *@This(), sym_ref: parser.SymbolRef, exp_ref: NodeRef, max_depth: u8) bool {
        return self._isSimpleAccessExpressionOfSym(sym_ref, exp_ref, 0, max_depth);
    }

    fn _isSimpleAccessExpressionOfSym(self: *@This(), sym_ref: parser.SymbolRef, exp_ref: NodeRef, depth: u8, max_depth: u8) bool {
        const nodes = &self.file.ast.nodes;
        const exp = nodes.at(exp_ref);
        return switch (exp.kind) {
            .identifier => (self.file.binder.getSymbol(exp_ref) orelse return false) == sym_ref,
            .element_access_expression,
            .property_access_expression => depth < max_depth and self._isSimpleAccessExpressionOfSym(sym_ref, getLeft(exp), depth + 1, max_depth),
            .parenthesized_expression => self._isSimpleAccessExpressionOfSym(sym_ref, unwrapRef(exp), depth, max_depth),
            else => false,
        };
    }

    pub fn getReferenceUsageLocation(self: *@This(), exp_ref: parser.SymbolRef) NodeRef {
        const nodes = &self.file.ast.nodes;
        var c = exp_ref;
        while (c != 0) {
            const p_ref = self.parents.get(c) orelse return c;
            std.debug.assert(p_ref != c);
            const p = nodes.at(p_ref);
            switch (p.kind) {
                .source_file,
                .expression_statement => return c,
                .call_expression, .new_expression => return c,
                .parameter,
                .variable_declaration,
                .binding_element => return c,
                // not always accurate
                .case_clause,
                .switch_statement,
                .if_statement,
                .while_statement,
                .for_statement,
                .for_of_statement,
                .for_in_statement => return c,
                .object_literal_expression,
                .array_literal_expression => return c, // TODO: depends, only if parent of the literal isn't an exp statement
                .element_access_expression => {
                    if (getRight(p) == c) return c;
                },
                .property_assignment => {
                    return c;
                },
                .binary_expression => {
                    if (parser.isAssignmentOp(@enumFromInt(p.len)) and getLeft(p) == c) {
                        return c;
                    }
                },
                else => {},
            }
            c = p_ref;
        }
        return exp_ref;
    }

    pub fn isUsed(self: *@This(), sym_ref: parser.SymbolRef) !bool {
        var iter = self.getReferenceIterator(sym_ref) orelse return false;
        while (iter.next()) |r| {
            // we ignore any writes. this assumes that the decl initialzer is know. `max_depth` could be adjusted based on how much we know about a given value 
            if (iter._getAssignmentNode(r)) |x| {
                if (!self.isSimpleAccessExpressionOfSym(sym_ref, x[1], 0)) return true;

                if (x[2]) |v| {
                    if (try self.hasEffects(v)) return true;
                }
                // if (try self.hasEffects(getLeft(self.file.ast.nodes.at(x[1])))) return true;


                continue;
            }
            const loc = self.getReferenceUsageLocation(r);
            if (self.parents.get(loc)) |p_ref| {
                if (self.file.ast.nodes.at(p_ref).kind == .expression_statement) {
                    if (!try self.hasEffects(loc)) continue;
                }
            }
            return true;
        }
        return false;
    }

    fn isBefore(self: *@This(), a: NodeRef, b: NodeRef) bool {
        const nodes = &self.file.ast.nodes;
        const na = nodes.at(a);
        const nb = nodes.at(b);
        if (na.kind == .variable_declaration and nb.kind == .variable_declaration) {
            return na.extra_data2 < nb.extra_data2;
        }
        // FIXME: we can only use the fast path if both nodes aren't synthetic
        // return a < b;
        return false;
    }

    fn hasSimpleDataDepsAfterDest(self: *@This(), dest: NodeRef, exp_ref: NodeRef) bool {
        const exp = self.file.ast.nodes.at(exp_ref);
        return switch (exp.kind) {
            .identifier => {
                if (self.file.binder.getSymbol(exp_ref)) |sym_ref| {
                    const sym = self.file.binder.symbols.at(sym_ref);
                    if (sym.hasFlag(.late_bound)) return true;
                    if (sym.declaration == 0) return true;
                    if (self.references.get(sym_ref)) |references| {
                        for (references.items) |z| {
                            if (self.hasEffects(z) catch return false) return false;
                        }
                    }
                    return self.isBefore(sym.declaration, dest);
                }
                return false;
            },
            .postfix_unary_expression => self.hasSimpleDataDepsAfterDest(dest, getLeft(exp)),
            .property_assignment,
            .prefix_unary_expression => self.hasSimpleDataDepsAfterDest(dest, getRight(exp)),
            .binary_expression => self.hasSimpleDataDepsAfterDest(dest, getLeft(exp)) and self.hasSimpleDataDepsAfterDest(dest, getRight(exp)),
            .parenthesized_expression,
            .shorthand_property_assignment,
            .computed_property_name => self.hasSimpleDataDepsAfterDest(dest, unwrapRef(exp)),
            .object_literal_expression, .array_literal_expression => {
                var iter = NodeIterator.init(&self.file.ast.nodes, maybeUnwrapRef(exp) orelse return true);
                while (iter.nextRef()) |x| {
                    if (!self.hasSimpleDataDepsAfterDest(dest, x)) return false;
                }
                return true;
            },
            .numeric_literal, .string_literal, .no_substitution_template_literal => true,
            else => parser.isKeyword(exp.kind),
        };
    }

    pub fn findSingleInlineLocation(self: *@This(), sym_ref: parser.SymbolRef) !?NodeRef {
        const sym = self.file.binder.symbols.at(sym_ref);
        if (sym.hasFlag(.late_bound)) return null;
        if (sym.declaration == 0) return null;
        if (!self.isUsedExactlyOnce(sym_ref)) return null;
        const decl = self.file.ast.nodes.at(sym.declaration);
        if (decl.kind != .variable_declaration) return null;
        if (try self.hasEffects(sym.declaration)) return null;
        const p_ref = self.parents.get(sym.declaration) orelse return null;
        const stmt = self.file.ast.nodes.at(p_ref);
        if (stmt.kind != .variable_statement) return null;
        var iter = self.getReferenceIterator(sym_ref) orelse return null;
        while (iter.next()) |r| return r;
        return null;
    }

    pub fn findEffectiveInitializer(self: *@This(), sym_ref: parser.SymbolRef) !?struct {NodeRef,NodeRef} {
        const sym = self.file.binder.symbols.at(sym_ref);
        if (sym.hasFlag(.late_bound)) return null;
        if (sym.declaration == 0) return null;
        const decl = self.file.ast.nodes.at(sym.declaration);
        if (decl.kind != .variable_declaration) return null;
        const p_ref = self.parents.get(sym.declaration) orelse return null;
        const stmt = self.file.ast.nodes.at(p_ref);
        if (stmt.kind != .variable_statement) return null;
        const scope_ref = self.parents.get(p_ref) orelse return null;

        var res: NodeRef = 0;
        var top: NodeRef = 0;
        var iter = self.getReferenceIterator(sym_ref) orelse return null;
        while (iter.next()) |r| {
            if (iter._getAssignmentNode(r)) |x| {
                if (!self.isSimpleAccessExpressionOfSym(sym_ref, x[1], 0)) break;
                if (x[2]) |v| {
                    top = x[0];
                    res = v;
                    if (try self.hasEffects(v)) break;
                } else break;

                continue;
            }
            const loc = self.getReferenceUsageLocation(r);
            if (self.parents.get(loc)) |p_ref2| {
                if (self.file.ast.nodes.at(p_ref2).kind == .expression_statement) {
                    if (!try self.hasEffects(loc)) continue;
                }
            }
            break;
        }
        if (res == 0) return null;
        const s = self.getScopeRef(top) orelse return null;
        if (s[1] != scope_ref) return null;
        const ref_stmt = self.file.ast.nodes.at(s[0]);
        if (ref_stmt.kind != .expression_statement) return null; // TODO: handle other statements?
        if (unwrapRef(ref_stmt) != top) return null; // TODO: handle cases like `x = foo(y = z)` by reducing elsewhere
        const decl_init = getRight(decl);
        if (!self.hasSimpleDataDepsAfterDest(sym.declaration, res)) return null;
        if (try self.hasEffects(decl_init)) {
            // now we gotta check intervening effects
            var c = stmt.next;
            while (c != 0 and c != s[0]) {
                if (try self.hasEffects(c)) return null;
                c = self.file.ast.nodes.at(c).next;
            }
        }
        return .{top,res};
    }

    pub fn isUsedExactlyOnce(self: *@This(), sym_ref: parser.SymbolRef) bool {
        return (self.references.get(sym_ref) orelse return false).items.len == 1;
    }

    pub fn getParamInit(self: *@This(), sym_ref: parser.SymbolRef) ?NodeRef {
        const sym = self.file.binder.symbols.at(sym_ref);
        const param = self.file.ast.nodes.at(sym.declaration);
        std.debug.assert(param.kind == .parameter);
        return if (getRight(param) != 0) getRight(param) else null;
    }

    pub fn isExpDefinitelyNotUndefined(self: *@This(), exp_ref: NodeRef) bool {
        const exp = self.file.ast.nodes.at(exp_ref);
        return switch (exp.kind) {
            .undefined_keyword => false,
            .void_expression => false,
            .numeric_literal, .string_literal, .no_substitution_template_literal, .template_expression => true,
            .array_literal_expression, .object_literal_expression => true,
            .parenthesized_expression => self.isExpDefinitelyNotUndefined(unwrapRef(exp)),
            else => {
                if (parser.isKeyword(exp.kind)) return true;
                // most binary exp shouldn't be?
                return false;
            }
        };
    }

    pub fn typeofExp(self: *@This(), left: NodeRef, right: NodeRef) !u32 {
        const facts = (try self.getValueFacts(left)) & ~@intFromEnum(ValueFacts.complex);
        if (ValueFacts.isAny(facts)) return @intFromEnum(ValueFacts.boolean);
        const n = self.file.ast.nodes.at(right);
        switch (n.kind) {
            .string_literal => {
                var r: u32 = @intFromEnum(ValueFacts.boolean);
                const v = parser.getSlice(n, u8);
                var mask: ValueFacts = .none;
                if (std.mem.eql(u8, v, "string")) {
                    mask = .any_string;
                } else if (std.mem.eql(u8, v, "number")) {
                    mask = .any_number;
                } else if (std.mem.eql(u8, v, "object")) {
                    mask = .object_or_array_or_null;
                } else if (std.mem.eql(u8, v, "function")) {
                    mask = .function;
                } else if (std.mem.eql(u8, v, "symbol")) {
                    mask = .symbol;
                } else if (std.mem.eql(u8, v, "undefined")) {
                    mask = .undefined;
                }
                if (mask == .none or facts == 0) return r;
                if (ValueFacts.maskFact(facts, mask) == facts) {
                    r = ValueFacts.excludeFact(r, .false);
                }
                if (!ValueFacts.hasFact(facts, mask)) {
                    r = ValueFacts.excludeFact(r, .true);
                }
                // TODO: never fact?
                if (r == 0) return @intFromEnum(ValueFacts.boolean);
                return r;
            },
            else => {},
        }
        return @intFromEnum(ValueFacts.boolean);
    }

    pub fn getDeclarationFacts(self: *@This(), sym_ref: parser.SymbolRef) !u32 {
        const sym = self.file.binder.symbols.at(sym_ref);
        if (sym.declaration == 0 or sym.hasFlag(.late_bound)) {
            return self.external_facts.get(sym_ref) orelse @intFromEnum(ValueFacts.any);
        }
        return try self.getValueFacts(sym.declaration);
    }

    fn getSimpleValueFacts(self: *@This(), exp_ref: NodeRef) u32 {
        if (exp_ref == 0) return 0;
        const exp = self.file.ast.nodes.at(exp_ref);
        const fact: ValueFacts = switch (exp.kind) {
            .true_keyword => .true,
            .false_keyword => .false,
            .array_literal_expression => .array,
            .null_keyword => .null,
            .undefined_keyword,
            .void_expression => .undefined,
            else => .any,
        };
        return @intFromEnum(fact);
    }

    pub fn getValueFacts(self: *@This(), exp_ref: NodeRef) anyerror!u32 {
        if (exp_ref == 0) return 0;
        const exp = self.file.ast.nodes.at(exp_ref);
        return switch (exp.kind) {
            .identifier => {
                const sym_ref = self.file.binder.getSymbol(exp_ref) orelse return @intFromEnum(ValueFacts.any);
                const facts = try self.getSymValueFacts(sym_ref);
                if (!ValueFacts.isComplete(facts)) return @intFromEnum(ValueFacts.any);
                return facts;
            },
            .variable_declaration => {
                const init_exp = getRight(exp);
                if (init_exp != 0) {
                    return try self.getValueFacts(init_exp);
                }
                return @intFromEnum(ValueFacts.undefined);
            },
            .numeric_literal => getNumberFacts(parser.getNumber(exp)),
            .string_literal, .no_substitution_template_literal => getStringFacts(parser.getSlice(exp, u8)),
            .template_expression => @intFromEnum(ValueFacts.string) | @intFromEnum(ValueFacts.zero),
            .object_literal_expression => {
                var r = @intFromEnum(ValueFacts.object);
                var members_iter = NodeIterator.init(&self.file.ast.nodes, maybeUnwrapRef(exp) orelse 0);
                while (members_iter.next()) |n| {
                    switch (n.kind) {
                        .get_accessor, .set_accessor => {
                            r |= @intFromEnum(ValueFacts.complex);
                            return r;
                        },
                        else => {},
                    }
                }
                return r;
            },
            // .call_expression => {

            // },
            .postfix_unary_expression => {
                // this case is a bit odd because we are in an expression position, so this is the fact about the result (pass through)
                const operand = try self.getValueFacts(getLeft(exp));
                return operand;
            },
            .prefix_unary_expression => {
                const op: SyntaxKind = @enumFromInt(getLeft(exp));
                const operand = try self.getValueFacts(getRight(exp));
                return switch (op) {
                    .minus_token => ValueFacts.arithmeticNegation(operand),
                    .exclamation_token => ValueFacts.negate(operand),
                    .plus_plus_token => ValueFacts.arithmeticAdd(operand, @intFromEnum(ValueFacts.number)),
                    .minus_minus_token => ValueFacts.arithmeticAdd(operand, @intFromEnum(ValueFacts.negative_number)),
                    else => @intFromEnum(ValueFacts.any), // TODO: bitwise not?
                };
            },
            .typeof_expression => {
                return @intFromEnum(ValueFacts.string);
            },
            .binary_expression => {
                const op: SyntaxKind = @enumFromInt(exp.len);
                if (parser.isAssignmentOp(op)) {
                    var left = try self.getValueFacts(getLeft(exp));
                    switch (op) {
                        .equals_token => {
                            left = 0; // direct assignments override the facts known about the lhs
                        },
                        .question_question_equals_token => {
                            left = ValueFacts.excludeFact(left, .nullish);
                        },
                        .plus_equals_token => {
                            return ValueFacts.arithmeticAdd(left, try self.getValueFacts(getRight(exp)));
                        },
                        .minus_equals_token => {
                            return ValueFacts.arithmeticAdd(left, ValueFacts.arithmeticNegation(try self.getValueFacts(getRight(exp))));
                        },
                        else => return @intFromEnum(ValueFacts.any),
                    }
                    return left | try self.getValueFacts(getRight(exp));
                }
                switch (op) {
                    .equals_equals_token,
                    .exclamation_equals_token,
                    .exclamation_equals_equals_token,
                    .equals_equals_equals_token => {
                        const negated = op == .exclamation_equals_token or op == .exclamation_equals_equals_token;
                        const left = self.file.ast.nodes.at(getLeft(exp));
                        if (left.kind == .typeof_expression) {
                            const result = try self.typeofExp(unwrapRef(left), getRight(exp));
                            return if (negated) ValueFacts.negate(result) else result;
                        }
                        const l = try self.getValueFacts(getLeft(exp));
                        if (ValueFacts.isAny(l)) return @intFromEnum(ValueFacts.boolean);
                        const r = try self.getValueFacts(getRight(exp));
                        if (ValueFacts.isAny(r)) return @intFromEnum(ValueFacts.boolean);
                        const result = ValueFacts.strictEquals(l, r);
                        return if (negated) ValueFacts.negate(result) else result;
                    },
                    .ampersand_ampersand_token => {
                        return ValueFacts.logicalAnd(try self.getValueFacts(getLeft(exp)), try self.getValueFacts(getRight(exp)));
                    },
                    .bar_bar_token => {
                        return ValueFacts.logicalOr(try self.getValueFacts(getLeft(exp)), try self.getValueFacts(getRight(exp)));
                    },
                    .question_question_token => {
                        return ValueFacts.nullishCoalesce(try self.getValueFacts(getLeft(exp)), try self.getValueFacts(getRight(exp)));
                    },
                    .less_than_token, .greater_than_token, .less_than_equals_token, .greater_than_equals_token => {
                        return ValueFacts.relates(try self.getValueFacts(getLeft(exp)), try self.getValueFacts(getRight(exp)), op);
                    },
                    .plus_token => {
                        return ValueFacts.arithmeticAdd(try self.getValueFacts(getLeft(exp)), try self.getValueFacts(getRight(exp)));
                    },
                    .minus_token => {
                        return ValueFacts.arithmeticAdd(try self.getValueFacts(getLeft(exp)), ValueFacts.arithmeticNegation(try self.getValueFacts(getRight(exp))));
                    },
                    .asterisk_token, .slash_token, .percent_token, .asterisk_asterisk_token, .bar_token, .ampersand_token, .caret_token => {
                        return @intFromEnum(ValueFacts.any_number);
                    },
                    else => return @intFromEnum(ValueFacts.any),
                }
            },
            else => self.getSimpleValueFacts(exp_ref),
        };
    }

    pub fn getSymValueFacts(self: *@This(), sym_ref: parser.SymbolRef) anyerror!u32 {
        const entry = try self.value_facts.getOrPut(getAllocator(), sym_ref);
        if (entry.found_existing) {
            return entry.value_ptr.*;
        }
        
        entry.value_ptr.* = @intFromEnum(ValueFacts.incomplete);
        const v = try self.getDeclarationFacts(sym_ref);
        entry.value_ptr.* |= v;

        if (self.file.binder.symbols.at(sym_ref).hasFlag(.@"const")) {
            entry.value_ptr.* &= ~@intFromEnum(ValueFacts.incomplete);
            return entry.value_ptr.*;   
        }
        var iter = self.getReferenceIterator(sym_ref) orelse {
            entry.value_ptr.* &= ~@intFromEnum(ValueFacts.incomplete);
            return entry.value_ptr.*;
        };
        while (iter.next()) |x| {
            if (iter._getAssignmentNode(x)) |q| {
                const val = q[2] orelse continue; // TODO: if this is null, it is postfix/prefix unary
                entry.value_ptr.* |= try self.getValueFacts(val);
            }
        }
        entry.value_ptr.* &= ~@intFromEnum(ValueFacts.incomplete);
        return entry.value_ptr.*;
    }

    pub fn hasEffects(self: *@This(), exp: NodeRef) anyerror!bool {
        const nodes = &self.file.ast.nodes;
        const n = nodes.at(exp);
        switch (n.kind) {
            .identifier => {
                if (self.file.binder.getSymbol(exp)) |sym_ref| {
                    const sym = self.file.binder.symbols.at(sym_ref);
                    if (sym.hasFlag(.late_bound)) {
                        // TODO: check the facts, a computation as a ref transitively inherits effectful-ness
                        return !self.external_facts.contains(sym_ref);
                    }
                    return false;
                }
                return true;
            },
            .property_access_expression,
            .element_access_expression => {
                if (n.kind == .element_access_expression) {
                    if (try self.hasEffects(getRight(n))) return true;
                }
                const l = getLeft(n);
                if (try self.hasEffects(l)) return true;
                const facts = try self.getValueFacts(l);
                if (ValueFacts.isAny(facts)) return true;
                if (ValueFacts.maskFact(facts, .any_non_structural_non_nullish_primitive) == facts) return false;
                if (ValueFacts.hasFact(facts, .nullish)) return true;
                if (ValueFacts.hasFact(facts, .any_structural_value) and !ValueFacts.hasFact(facts, .complex)) {
                    const left = nodes.at(l);
                    switch (left.kind) {
                        .identifier => {},
                        .array_literal_expression, .object_literal_expression => return false,
                        else => return true,
                    }
                    const sym_ref = self.file.binder.getSymbol(l) orelse return true;
                    if (try self.hasEscapedBeforeExp(sym_ref, exp)) return true;
                    return false;
                }
                return true;
            },
            .binding_element => {
                const r = getRight(n);
                if (r != 0 and try self.hasEffects(r)) {
                    return true;
                }
                // TODO: 
                return true;
            },
            .new_expression, .call_expression => {
                return true;
            },
            .parenthesized_expression => {
                return self.hasEffects(unwrapRef(n));
            },
            // todo: `instanceof` should be treated as effectful if we cannot find the impl
            .binary_expression => {
                const d = getPackedData(n);
                if (parser.isAssignmentOp(@enumFromInt(n.len))) {
                    return true;
                }
                return try self.hasEffects(d.left) or try self.hasEffects(d.right);
            },
            .prefix_unary_expression => {
                const d = getPackedData(n);
                switch (@as(SyntaxKind, @enumFromInt(d.left))) {
                    .plus_plus_token, .minus_minus_token => {
                        return true;
                    },
                    else => {}, // not always correct to do this
                }
                return self.hasEffects(d.right);
            },
            .conditional_expression => {
                const d = getPackedData(n);
                return try self.hasEffects(d.left) or try self.hasEffects(d.right) or try self.hasEffects(n.len);
            },
            .typeof_expression, .void_expression => {
                return self.hasEffects(unwrapRef(n));
            },
            .numeric_literal, .string_literal, .no_substitution_template_literal, .true_keyword, .false_keyword, .null_keyword, .undefined_keyword, .arrow_function, .function_expression => {
                return false;
            },
            // template_expression
            .class_expression => {
                return true; // TODO: static blocks
            },
            // statements
            .throw_statement => return true,
            .expression_statement => {
                return self.hasEffects(unwrapRef(n));
            },
            .return_statement => {
                return true;
            },
            .block => {
                var statements_iter = NodeIterator.init(nodes, maybeUnwrapRef(n) orelse 0);
                while (statements_iter.nextRef()) |s_ref| {
                    if (try self.hasEffects(s_ref)) return true;
                }
                return false;
            },
            .array_literal_expression, .object_literal_expression => {
                var iter = NodeIterator.init(nodes, maybeUnwrapRef(n) orelse 0);
                while (iter.nextRef()) |r| {
                    if (try self.hasEffects(r)) return true;
                }
                return false;
            },
            .if_statement => {
                const d = getPackedData(n);
                if (try self.hasEffects(d.left)) return true;
                if (try self.hasEffects(d.right)) return true;
                if (n.len != 0 and try self.hasEffects(n.len)) return true;
                return false;
            },
            // should we even support `do...while` in Syn?
            .while_statement => {
                const d = getPackedData(n);
                return try self.hasEffects(d.left) or try self.hasEffects(d.right);
            },
            .for_statement => {
                const d = getPackedData(n);
                if (d.left != 0 and try self.hasEffects(d.left)) return true;
                if (d.right != 0 and try self.hasEffects(d.right)) return true;
                if (n.len != 0 and try self.hasEffects(n.len)) return true;
                return try self.hasEffects(n.extra_data);
            },
            .for_of_statement, .for_in_statement => {
                const d = getPackedData(n);
                return try self.hasEffects(d.left) or try self.hasEffects(d.right) or try self.hasEffects(n.len);
            },
            .case_clause, .default_clause => {
                if (maybeUnwrapRef(n)) |d| {
                    if (try self.hasEffects(d)) return true;
                }
                var iter = NodeIterator.init(nodes, n.len);
                while (iter.nextRef()) |s_ref| {
                    if (try self.hasEffects(s_ref)) return true;
                }
                return false;
            },
            .switch_statement => {
                const d = getPackedData(n);
                if (try self.hasEffects(d.left)) return true;
                var iter = NodeIterator.init(nodes, d.right);
                while (iter.nextRef()) |case_ref| {
                    if (try self.hasEffects(case_ref)) return true;
                }
                return false;
            },
            .break_statement, .continue_statement => return false,
            .variable_declaration => {
                const d = getPackedData(n);
                if (nodes.at(d.left).kind != .identifier) {
                    if (try self.hasEffects(d.left)) return true;
                }
                if (d.right != 0) {
                    return try self.hasEffects(d.right);
                }
                return false;
            },
            .variable_statement => {
                var decls = NodeIterator.init(nodes, unwrapRef(n));
                while (decls.nextRef()) |d_ref| {
                    if (try self.hasEffects(d_ref)) return true;
                }
                return false;
            },
            .start => return false, // nil node
            else => return true, // assume effects by default
        }
    }
};

fn getNumberFacts(v: f64) u32 {
    // TODO: infinity?
    var r: u32 = @intFromEnum(ValueFacts.number);
    if (v == 0) r |= @intFromEnum(ValueFacts.zero);
    if (r < 0) r |= @intFromEnum(ValueFacts.negative_number);
    if (@round(v) != v or std.math.isNan(v)) r |= @intFromEnum(ValueFacts.any); // TODO: handle floats/nan
    return r;
}

fn getStringFacts(v: []const u8) u32 {
    var r: u32 = @intFromEnum(ValueFacts.string);
    if (v.len == 0) r |= @intFromEnum(ValueFacts.zero);
    return r;
}

const JsCodeReducer = struct {
    refs: *ReferenceCollector,
    nodes: *BumpAllocator(parser.AstNode),
    factory: parser.Factory,
    replacements: *std.AutoArrayHashMap(NodeRef, NodeRef),
    symbol_replacements: *std.AutoArrayHashMapUnmanaged(parser.SymbolRef, NodeRef),
    simple_reduced_structures: std.AutoHashMapUnmanaged(parser.SymbolRef, NodeRef) = .{},
    parent: NodeRef = 0,
    did_change: bool = false,
    pass_count: u32 = 0,

    fn replace(self: *@This(), from: NodeRef, to: NodeRef) !void {
        const prior = self.replacements.get(from) orelse from;
        self.nodes.at(to).next = self.nodes.at(prior).next;
        try self.replacements.put(from, to);
        self.did_change = true;
    }

    fn replaceWithList(self: *@This(), from: NodeRef, to: NodeRef, tail: NodeRef) !void {
        std.debug.assert(self.nodes.at(tail).next == 0);
        self.nodes.at(tail).next = self.nodes.at(from).next;
        try self.replacements.put(from, to);
        self.did_change = true;
    }

    fn remove(self: *@This(), ref: NodeRef) !void {
        // const n = self.nodes.at(ref);
        const replacement = try self.factory.createNotEmittedStatement(); // TODO: add to factory
        const prior = self.replacements.get(ref) orelse ref;
        self.nodes.at(replacement).next = self.nodes.at(prior).next;
        try self.replacements.put(ref, replacement);
        self.did_change = true;
    }

    fn clone(self: *@This(), ref: NodeRef) !NodeRef {
        const replacement = self.replacements.get(ref) orelse ref;
        return self.factory.cloneNodeRef(replacement);
    }

    fn visitChildren(self: *@This(), ref: NodeRef) !void {
        const save_parent = self.parent;
        self.parent = ref;
        try parser.forEachChild(self.nodes, self.nodes.at(ref), self);
        self.parent = save_parent;
    }

    fn reduceIfStatement(self: *@This(), node: *const parser.AstNode, ref: NodeRef, is_true: bool) !void {
        // TODO: preserve effects and/or bail
        const then_ref = getRight(node);
        const else_ref = node.len;
        if (!is_true) {
            const r = if (else_ref != 0) try self.clone(else_ref) else try self.factory.createNotEmittedStatement();
            try self.replace(ref, r);
            if (else_ref != 0) try self.visitChildren(else_ref);
            return;
        }
        try self.replace(ref, try self.clone(then_ref));
        return try self.visitChildren(then_ref);
    }

    fn reduceStructures(self: *@This()) !void {
        var symIter = self.refs.references.keyIterator();
        while (symIter.next()) |_s| {
            const s = _s.*;
            const sym = self.refs.file.binder.symbols.at(s);
            const is_const = sym.hasFlag(.@"const");
            if (sym.declaration == 0) continue; // TODO
            if (sym.declaration != 0) {
                const decl = self.nodes.at(sym.declaration);
                switch (decl.kind) {
                    .variable_declaration => {
                        const stmt_ref = self.refs.parents.get(sym.declaration) orelse continue;
                        if (self.nodes.at(stmt_ref).kind != .variable_statement) continue;
                    },
                    else => continue,
                }
            }
            const exp = self.refs.getSingularAccessExp(s) orelse continue;
            const exp_node = self.refs.file.ast.nodes.at(exp);
            if (exp_node.kind != .numeric_literal) continue;
            var iter = self.refs.getReferenceIterator(s) orelse continue;
            while (iter.next()) |x| {
                const t = iter.getAccessExpressionLeft(x) orelse unreachable;
                const c = try self.clone(t);
                const p = iter.parentRef(t) orelse unreachable;
                try self.replace(p, c);
            }
            if (!sym.hasFlag(.late_bound)) {
                if (is_const) sym.removeFlag(.@"const");
                const v = parser.getNumber(exp_node);
                const decl = self.nodes.at(sym.declaration);
                const init = getRight(decl);
                const literal = self.nodes.at(init);
                if (literal.kind != .array_literal_expression) unreachable;
                if (@round(v) != v) unreachable;
                if (v < 0) unreachable;
                var i: u32 = 0;
                var element_iter = NodeIterator.init(self.nodes, maybeUnwrapRef(literal) orelse 0);
                while (element_iter.nextRef()) |x| {
                    if (i == @as(u32, @intFromFloat(v))) {
                        try self.replace(init, try self.clone(x));
                        break;
                    }
                    i += 1;
                }
                if (is_const) {
                    const stmt_ref = self.refs.parents.get(sym.declaration) orelse unreachable;
                    std.debug.assert(self.nodes.at(stmt_ref).kind == .variable_statement);
                    const c = try self.clone(stmt_ref);
                    self.nodes.at(c).flags &= ~@intFromEnum(parser.NodeFlags.@"const");
                    self.nodes.at(c).flags |= @intFromEnum(parser.NodeFlags.let);
                    try self.replace(stmt_ref, c);
                }
            } else {
                try self.simple_reduced_structures.put(getAllocator(), s, exp);
            }
        }
    }

    fn removeUnused(self: *@This()) !void {
        var symIter = self.refs.references.keyIterator();
        while (symIter.next()) |s_ptr| {
            const s = s_ptr.*;
            const sym = self.refs.file.binder.symbols.at(s);
            if (sym.hasFlag(.late_bound)) continue;

            const decl_ref = sym.declaration;
            const decl = self.nodes.at(decl_ref);

            if (try self.refs.isUsed(s)) {
                if (try self.refs.findEffectiveInitializer(s)) |res| {
                    const exp = res[1];
                    const initializer = getRight(decl);
                    const stmt_info = self.refs.getScopeRef(res[0]) orelse unreachable;
                    try self.remove(stmt_info[0]);

                    if (try self.refs.hasEffects(initializer)) {
                        // const p_ref = self.refs.parents.get(decl_ref) orelse unreachable;
                        // if (self.nodes.at(p_ref).kind != .variable_statement) unreachable;
                        // const has_one_child = self.nodes.at(unwrapRef(self.nodes.at(p_ref))).next == 0;
                        // if (!has_one_child) unreachable;
                        const c = try self.clone(decl_ref);
                        const comma_exp = try self.factory.createParenthesizedExpression(
                            try self.factory.createBinaryExpression(initializer, .comma_token, exp)
                        );
                        self.nodes.at(c).data = (@as(u64, comma_exp) << 32) | getLeft(decl);
                        try self.replace(decl_ref, c);
                        continue;
                    }

                    if (initializer == 0) {
                        const c = try self.clone(decl_ref);
                        self.nodes.at(c).data = (@as(u64, exp) << 32) | getLeft(decl);
                        try self.replace(decl_ref, c);
                    } else {
                        try self.replace(initializer, try self.clone(exp));
                    }
                    continue;
                }
                if (self.did_change) continue; // FIXME: figure out why this affects opt
                const p_ref = self.refs.parents.get(decl_ref) orelse continue;
                if (self.nodes.at(p_ref).kind != .variable_statement) continue;
                const has_one_child = self.nodes.at(unwrapRef(self.nodes.at(p_ref))).next == 0;
                if (!has_one_child) continue;
                if (try self.refs.findSingleInlineLocation(s)) |ref| {
                    const init_ref = getRight(decl);
                    const v = if (init_ref != 0) try self.factory.cloneNodeRef(init_ref) else try self.factory.createVoidZero();
                    try self.remove(p_ref);
                    // if (self.refs.getScopeRef(ref)) |x| {
                    //     if (parser.getAssignmentExpFromStmt(self.nodes, x[0])) |z| blk: {
                    //         const sy = self.refs.file.binder.getSymbol(getLeft(self.nodes.at(z))) orelse break :blk;
                    //         if (s == sy) {
                    //             try self.replace(z, v);
                    //             continue;
                    //         }
                    //     }
                    // }
                    try self.replace(ref, v);
                }
                continue;
            }

            switch (decl.kind) {
                .variable_declaration => {
                    const p_ref = self.refs.parents.get(decl_ref) orelse continue;
                    if (self.nodes.at(p_ref).kind != .variable_statement) continue;
                    const has_one_child = self.nodes.at(unwrapRef(self.nodes.at(p_ref))).next == 0;
                    if (!has_one_child) continue;
                    try self.remove(p_ref);
                },
                else => continue,
            }

            var iter = self.refs.getReferenceIterator(s) orelse continue;
            while (iter.next()) |r| {
                // var rem: ?NodeRef = null;
                // if (try self.hasEffects(getLeft(x[1]))) return true;
                // if (x[2]) |v| {
                //     if (try self.hasEffects(v)) return true;
                // }
                if (iter._getAssignmentNode(r)) |x| {
                    if (iter.parentRef(x[0])) |p| {
                        if (self.nodes.at(p).kind == .expression_statement) {
                            try self.remove(p);
                            continue;
                        }
                    }
                    try self.replace(x[0], if (x[2]) |v| try self.clone(v) else return error.UnexpectedUnaryUsage);
                    continue;
                }
                const loc = self.refs.getReferenceUsageLocation(r);
                if (self.refs.parents.get(loc)) |p_ref| {
                    if (self.nodes.at(p_ref).kind == .expression_statement) {
                        try self.remove(p_ref);
                    } else {
                        std.debug.print("{?}\n",. {self.nodes.at(p_ref).kind});
                    }
                } else {
                    std.debug.print("?? {s}\n",. {parser.getSlice(self.nodes.at(r), u8)});
                    // try self.replace();
                }
            }
        }
    }

    pub fn reduce(self: *@This()) !void {
        const start_time = if (comptime @import("builtin").target.isWasm()) 0 else std.time.microTimestamp();
        try self.reduceStructures();
        while (true) {
            self.pass_count += 1;
            try self.removeUnused();
            try parser.forEachChild(self.nodes, self.nodes.at(self.refs.file.ast.start), self);
            if (!self.did_change) break;
            self.did_change = false;
            try self.refs.commitInPlace(self.replacements, self.symbol_replacements);
        }
        const end_time = if (comptime @import("builtin").target.isWasm()) 0 else std.time.microTimestamp();
        debugPrint("TOTAL PASSES: {}\n", .{self.pass_count});
        debugPrint("TIME {d:.3}us\n", .{ end_time - start_time });
    }

    pub fn visit(self: *@This(), node: *const parser.AstNode, ref: NodeRef) anyerror!void {
        if (self.replacements.get(ref)) |x| {
            return self.visit(self.nodes.at(x), x);
        }
        switch (node.kind) {
            .if_statement => {
                const cond = getLeft(node);
                const cond_facts = ValueFacts.coerceBool(try self.refs.getValueFacts(cond));
                if (ValueFacts.hasExactFact(cond_facts, .true) or ValueFacts.hasExactFact(cond_facts, .false)) {
                    return self.reduceIfStatement(node, ref, ValueFacts.hasExactFact(cond_facts, .true));
                }
            },
            .call_expression => {
                if (try tryInlineIife(self.refs, getLeft(node), getRight(node), self.symbol_replacements)) |x| {
                    // XXX: currently won't detect certain statement parents e.g. `if (cond) (() => {})();
                    switch (self.nodes.at(self.parent).kind) {
                        .source_file, .block, .start => {
                            try self.replace(ref, try self.factory.createExpressionStatement(x));
                        },
                        else => try self.replace(ref, x),
                    }
                }
            },
            .variable_statement => {
                const has_one_child = self.nodes.at(unwrapRef(node)).next == 0;
                if (has_one_child) {
                    const decl = self.nodes.at(unwrapRef(node));
                    if (self.nodes.at(getLeft(decl)).kind == .identifier) {
                        if (self.refs.file.binder.getSymbol(getLeft(decl))) |sym_ref| {
                            if (!self.refs.references.contains(sym_ref) and (getRight(decl) == 0 or !try self.refs.hasEffects(getRight(decl)))) {
                                return try self.remove(ref);
                            }
                        }
                    }
                }
            },
            .expression_statement => {
                if (!try self.refs.hasEffects(ref)) {
                    return try self.remove(ref);
                }
            },
            .block => {
                const start = maybeUnwrapRef(node) orelse 0;
                if (start == 0 and self.parent != 0) {
                    const p = self.nodes.at(self.parent);
                    switch (p.kind) {
                        .source_file, .block => {
                            try self.remove(ref);
                        },
                        .if_statement, .while_statement => {
                            // if/while (x) {} 
                            if (getRight(p) == ref and (p.kind != .if_statement or p.len == 0)) {
                                if (!try self.refs.hasEffects(getLeft(p))) {
                                    try self.remove(self.parent);
                                }
                            }
                        },
                        else => {},
                    }
                }
                // block merging
                if (start != 0 and !self.did_change) blk: {
                    if (self.refs.parents.get(ref)) |p_ref| {
                        if (self.nodes.at(p_ref).kind == .labeled_statement) {
                            break :blk;
                        }
                    }
                    var iter = NodeIterator.init(self.nodes, start);
                    var has_decl = false;
                    var tail: NodeRef = 0;
                    while (iter.nextRef()) |x| {
                        switch (self.nodes.at(x).kind) {
                            .class_declaration,
                            .function_declaration,
                            .variable_statement => {
                                has_decl = true;
                                break;
                            },
                            else => {
                                tail = x;
                            },
                        }
                    }
                    if (!has_decl) {
                        try self.visitChildren(ref);
                        if (self.replacements.contains(tail) or self.replacements.contains(ref)) return;
                        self.nodes.at(ref).flags |= @intFromEnum(parser.NodeFlags.reduce_block);
                        const c = try self.clone(tail);
                        try self.replace(tail, c);
                        std.debug.assert(self.nodes.at(c).next == 0);
                        self.nodes.at(c).next = node.next;
                    }
                }
            },
            else => {},
        }
        try self.visitChildren(ref);
    }
};

// sigh
fn stripIndexZero(text: []const u8, name: []const u8) ![]const u8 {
    var out = std.ArrayList(u8).init(getAllocator());
    var i: usize = 0;
    while (std.mem.indexOfPos(u8, text, i, name)) |pos| {
        try out.appendSlice(text[i..pos]);
        try out.appendSlice(name);
        i = pos + name.len;
        if (i + 3 <= text.len and std.mem.eql(u8, text[i .. i + 3], "[0]")) {
            i += 3;
        }
    }
    try out.appendSlice(text[i..]);
    return out.items;
}

const ParamReplacer = struct {
    nodes: *const BumpAllocator(AstNode),
    binder: *const parser.Binder,
    targets: []const parser.SymbolRef,
    placeholders: []const NodeRef,
    out: *std.AutoArrayHashMap(NodeRef, NodeRef),

    pub fn visit(self: *@This(), node: *const AstNode, ref: NodeRef) anyerror!void {
        if (ref == 0) return;
        if (node.kind == .identifier) {
            const sym = self.binder.getSymbol(ref) orelse return;
            for (self.targets, 0..) |t, i| {
                if (t == sym) {
                    try self.out.put(ref, self.placeholders[i]);
                    return;
                }
            }
            return;
        }
        try parser.forEachChild(self.nodes, node, self);
    }
};

fn collectParamReplacements(
    nodes: *const BumpAllocator(AstNode),
    binder: *const parser.Binder,
    ref: NodeRef,
    targets: []const parser.SymbolRef,
    placeholders: []const NodeRef,
    out: *std.AutoArrayHashMap(NodeRef, NodeRef),
) !void {
    var replacer = ParamReplacer{ .nodes = nodes, .binder = binder, .targets = targets, .placeholders = placeholders, .out = out };
    try replacer.visit(nodes.at(ref), ref);
}

fn tryGetTrivialIifeBody(
    nodes: *const BumpAllocator(AstNode),
    binder: *const parser.Binder,
    call_ref: NodeRef,
    replacements: *std.AutoArrayHashMap(NodeRef, NodeRef),
) !?NodeRef {
    const call = nodes.at(call_ref);
    if (call.kind != .call_expression) return null;
    const d = getPackedData(call);
    const callee = nodes.at(d.left);
    if (callee.kind != .parenthesized_expression) return null;
    const arrow_ref = maybeUnwrapRef(callee) orelse return null;
    const arrow = nodes.at(arrow_ref);
    if (arrow.kind != .arrow_function) return null;

    const body_ref = getPackedData(arrow).right;
    const body = nodes.at(body_ref);
    if (body.kind == .block) return null; // concise body only

    const params_head = getPackedData(arrow).left;
    var param_syms = std.ArrayListUnmanaged(parser.SymbolRef){};
    defer param_syms.deinit(getAllocator());
    {
        var it = NodeIterator.init(nodes, params_head);
        while (it.nextRef()) |p_ref| {
            const p = nodes.at(p_ref);
            const pd = getPackedData(p);
            if (pd.right != 0) return null; // has a default value
            const name_node = nodes.at(pd.left);
            if (name_node.kind != .identifier) return null; // no destructuring
            const sym = binder.getSymbol(pd.left) orelse return null;
            if (sym == 0) return null;
            try param_syms.append(getAllocator(), sym);
        }
    }

    var arg_refs = std.ArrayListUnmanaged(NodeRef){};
    defer arg_refs.deinit(getAllocator());
    {
        var it = NodeIterator.init(nodes, d.right);
        while (it.nextRef()) |a_ref| try arg_refs.append(getAllocator(), a_ref);
    }
    if (param_syms.items.len != arg_refs.items.len) return null;

    for (param_syms.items) |sym| {
        var counter = SymbolUseCounter{ .nodes = nodes, .binder = binder, .sym = sym };
        try counter.visit(nodes.at(body_ref), body_ref);
        if (counter.count != 1) return null; // used zero or 2+ times
    }

    if (param_syms.items.len > 0) {
        try collectParamReplacements(nodes, binder, body_ref, param_syms.items, arg_refs.items, replacements);
    }
    return body_ref;
}

const TrivialIifeCollector = struct {
    nodes: *const BumpAllocator(AstNode),
    binder: *const parser.Binder,
    replacements: *std.AutoArrayHashMap(NodeRef, NodeRef),

    pub fn visit(self: *@This(), node: *const AstNode, ref: NodeRef) anyerror!void {
        if (ref == 0) return;
        if (node.kind == .call_expression) {
            if (try tryGetTrivialIifeBody(self.nodes, self.binder, ref, self.replacements)) |body_ref| {
                try self.replacements.put(ref, body_ref);
                try self.visit(self.nodes.at(body_ref), body_ref);
                return;
            }
        }
        if (parser.isLeafNode(node.kind)) return;
        try parser.forEachChild(self.nodes, node, self);
    }
};

fn collectTrivialIifeReplacements(
    nodes: *const BumpAllocator(AstNode),
    binder: *const parser.Binder,
    root_ref: NodeRef,
    replacements: *std.AutoArrayHashMap(NodeRef, NodeRef),
) !void {
    var collector = TrivialIifeCollector{ .nodes = nodes, .binder = binder, .replacements = replacements };
    try collector.visit(nodes.at(root_ref), root_ref);
}

const SymbolUseCounter = struct {
    nodes: *const BumpAllocator(AstNode),
    binder: *const parser.Binder,
    sym: parser.SymbolRef,
    count: u32 = 0,

    pub fn visit(self: *@This(), node: *const AstNode, ref: NodeRef) anyerror!void {
        if (ref == 0) return;
        if (node.kind == .identifier) {
            const sym = self.binder.getSymbol(ref) orelse return;
            if (sym == self.sym) self.count += 1;
            return;
        }
        try parser.forEachChild(self.nodes, node, self);
    }
};

fn isExprSideEffectFree(nodes: *const BumpAllocator(AstNode), ref: NodeRef) bool {
    if (ref == 0) return true;
    const n = nodes.at(ref);
    return switch (n.kind) {
        .identifier, .true_keyword, .false_keyword, .null_keyword, .undefined_keyword, .numeric_literal, .string_literal, .this_keyword, .no_substitution_template_literal => true,
        .prefix_unary_expression => blk: {
            const d = getPackedData(n);
            const op: SyntaxKind = @enumFromInt(d.left);
            break :blk op == .exclamation_token and isExprSideEffectFree(nodes, d.right);
        },
        .parenthesized_expression => isExprSideEffectFree(nodes, unwrapRef(n)),
        else => false,
    };
}

fn dceStripDeadWritesFor(
    nodes: *BumpAllocator(AstNode),
    binder: *const parser.Binder,
    stmts: *std.ArrayList(NodeRef),
    sym: parser.SymbolRef,
) !bool {
    var changed = false;
    var factory = Factory{ .nodes = nodes };
    var i: usize = 0;
    while (i < stmts.items.len) {
        const stmt_ref = stmts.items[i];
        const stmt = nodes.at(stmt_ref);

        var is_write = false;
        var rhs: NodeRef = 0;
        if (stmt.kind == .variable_statement) {
            if (maybeUnwrapRef(stmt)) |decls_head| {
                const decl = nodes.at(decls_head);
                if (decl.next == 0 and decl.kind == .variable_declaration) {
                    const d = getPackedData(decl);
                    const name_node = nodes.at(d.left);
                    if (name_node.kind == .identifier and (binder.getSymbol(d.left) orelse 0) == sym) {
                        is_write = true;
                        rhs = d.right;
                    }
                }
            }
        } else if (stmt.kind == .expression_statement) {
            const inner_ref = unwrapRef(stmt);
            const inner = nodes.at(inner_ref);
            if (inner.kind == .binary_expression and inner.len == @intFromEnum(SyntaxKind.equals_token)) {
                const d = getPackedData(inner);
                const lhs = nodes.at(d.left);
                if (lhs.kind == .identifier and (binder.getSymbol(d.left) orelse 0) == sym) {
                    is_write = true;
                    rhs = d.right;
                }
            }
        }

        if (!is_write) {
            i += 1;
            continue;
        }
        changed = true;
        if (isExprSideEffectFree(nodes, rhs)) {
            _ = stmts.orderedRemove(i);
            continue;
        }
        stmts.items[i] = try factory.createExpressionStatement(rhs);
        i += 1;
    }
    return changed;
}

fn containsDollarDigit(s: []const u8) bool {
    var i: usize = 0;
    while (i + 1 < s.len) : (i += 1) {
        if (s[i] == '$' and s[i + 1] >= '0' and s[i + 1] <= '9') return true;
    }
    return false;
}

// bleh
fn getInnerFunctionExpr(parsed: *parser.ParsedFile) ?NodeRef {
    const source = parsed.ast.nodes.at(parsed.ast.start);
    const stmts_head = maybeUnwrapRef(source) orelse return null;
    const stmt = parsed.ast.nodes.at(stmts_head);
    if (stmt.next != 0) return null; // must be the only top-level statement
    if (stmt.kind != .expression_statement) return null;
    var inner_ref = unwrapRef(stmt);
    var inner = parsed.ast.nodes.at(inner_ref);
    if (inner.kind == .parenthesized_expression) {
        inner_ref = unwrapRef(inner);
        inner = parsed.ast.nodes.at(inner_ref);
    }
    if (inner.kind != .function_expression and inner.kind != .function_declaration) return null;
    return inner_ref;
}

/// destructures an IIFE (arrow fn only)
fn getIifeCallExpr(parsed: *parser.ParsedFile) ?struct { arrow_ref: NodeRef, args_head: NodeRef } {
    const source = parsed.ast.nodes.at(parsed.ast.start);
    const stmts_head = maybeUnwrapRef(source) orelse return null;
    const stmt = parsed.ast.nodes.at(stmts_head);
    if (stmt.next != 0) return null;
    if (stmt.kind != .expression_statement) return null;
    const inner_ref = maybeUnwrapRef(stmt) orelse return null;
    const inner = parsed.ast.nodes.at(inner_ref);
    if (inner.kind != .parenthesized_expression) return null;
    const call_ref = maybeUnwrapRef(inner) orelse return null;
    const call = parsed.ast.nodes.at(call_ref);
    if (call.kind != .call_expression) return null;
    const d = getPackedData(call);
    const callee = parsed.ast.nodes.at(d.left);
    if (callee.kind != .parenthesized_expression) return null;
    const arrow_ref = maybeUnwrapRef(callee) orelse return null;
    const arrow = parsed.ast.nodes.at(arrow_ref);
    if (arrow.kind != .arrow_function) return null;
    return .{ .arrow_ref = arrow_ref, .args_head = d.right };
}

fn getWrappedTopExpr(parsed: *parser.ParsedFile) ?NodeRef {
    const source = parsed.ast.nodes.at(parsed.ast.start);
    const stmts_head = maybeUnwrapRef(source) orelse return null;
    const stmt = parsed.ast.nodes.at(stmts_head);
    if (stmt.next != 0) return null;
    if (stmt.kind != .expression_statement) return null;
    return maybeUnwrapRef(stmt);
}

const DceLayer = struct {
    arrow_ref: NodeRef,
    params_head: NodeRef,
    args_head: NodeRef, // 0 = bare arrow, not itself called
    tracked_start: usize,
    tracked_end: usize,
};

fn dceUnwrap(
    parsed: *parser.ParsedFile,
    expr_ref: NodeRef,
    layers: *std.ArrayListUnmanaged(DceLayer),
    tracked: *std.ArrayListUnmanaged(TrackedParam),
) anyerror!?NodeRef {
    const n = parsed.ast.nodes.at(expr_ref);
    switch (n.kind) {
        .block => return expr_ref,
        .parenthesized_expression => {
            const inner = maybeUnwrapRef(n) orelse return null;
            return try dceUnwrap(parsed, inner, layers, tracked);
        },
        .arrow_function => {
            const params_head = getPackedData(n).left;
            const body_ref = getPackedData(n).right;
            try layers.append(getAllocator(), .{
                .arrow_ref = expr_ref,
                .params_head = params_head,
                .args_head = 0,
                .tracked_start = tracked.items.len,
                .tracked_end = tracked.items.len,
            });
            return try dceUnwrap(parsed, body_ref, layers, tracked);
        },
        .call_expression => {
            const d = getPackedData(n);
            const callee = parsed.ast.nodes.at(d.left);
            if (callee.kind != .parenthesized_expression) return null;
            const arrow_ref = maybeUnwrapRef(callee) orelse return null;
            const arrow = parsed.ast.nodes.at(arrow_ref);
            if (arrow.kind != .arrow_function) return null;
            const params_head = getPackedData(arrow).left;
            const args_head = d.right;

            const tracked_start = tracked.items.len;
            {
                var params_it = NodeIterator.init(&parsed.ast.nodes, params_head);
                var args_it = NodeIterator.init(&parsed.ast.nodes, args_head);
                while (params_it.nextRef()) |p_ref| {
                    const arg_ref = args_it.nextRef() orelse break;
                    const p = parsed.ast.nodes.at(p_ref);
                    const name_ref = getPackedData(p).left;
                    const name_node = parsed.ast.nodes.at(name_ref);
                    if (name_node.kind != .identifier) continue;
                    const sym = parsed.binder.getSymbol(name_ref) orelse continue;
                    if (sym == 0) continue;
                    const lit = evalLiteral(&parsed.ast.nodes, arg_ref) orelse continue;
                    try tracked.append(getAllocator(), .{ .sym = sym, .value = lit });
                }
            }
            try layers.append(getAllocator(), .{
                .arrow_ref = arrow_ref,
                .params_head = params_head,
                .args_head = args_head,
                .tracked_start = tracked_start,
                .tracked_end = tracked.items.len,
            });

            return try dceUnwrap(parsed, getPackedData(arrow).right, layers, tracked);
        },
        else => return null,
    }
}

const KnownValue = union(enum) {
    string: []const u8,
    boolean: bool,
    number: f64,
    null_,
    undefined_,
    truthy_reference,

    fn toBool(self: KnownValue) bool {
        return switch (self) {
            .boolean => |b| b,
            .number => |n| n != 0,
            .string => |s| s.len > 0,
            .null_, .undefined_ => false,
            .truthy_reference => true,
        };
    }
};

fn evalLiteral(nodes: *const BumpAllocator(AstNode), ref: NodeRef) ?KnownValue {
    const n = nodes.at(ref);
    return switch (n.kind) {
        .true_keyword => .{ .boolean = true },
        .false_keyword => .{ .boolean = false },
        .null_keyword => .null_,
        .void_expression,
        .undefined_keyword => .undefined_,
        .numeric_literal => .{ .number = parser.getNumber(n) },
        .string_literal, .no_substitution_template_literal => .{ .string = parser.getSlice(n, u8), }, // NOT DECODED!
        .regular_expression_literal,
        .class_expression, .class_declaration,
        .arrow_function, .function_expression, .function_declaration,
        .array_literal_expression, .object_literal_expression => .truthy_reference,
        else => null,
    };
}

const TrackedParam = struct {
    sym: parser.SymbolRef,
    value: ?KnownValue, // null = no longer statically known
    graph_value: ?ValueRef = null,
};

fn evalExprKnownValue(
    nodes: *const BumpAllocator(AstNode),
    binder: *const parser.Binder,
    expr_ref: NodeRef,
    tracked: []const TrackedParam,
    invariant_facts: ?*const std.AutoHashMapUnmanaged(ValueRef, void),
) ?KnownValue {
    const n = nodes.at(expr_ref);
    switch (n.kind) {
        .identifier => {
            const sym = binder.getSymbol(expr_ref) orelse return null;
            if (sym == 0) return null;
            for (tracked) |t| {
                if (t.sym == sym) return t.value;
            }
            return null;
        },
        .prefix_unary_expression => {
            const d = getPackedData(n);
            const op: SyntaxKind = @enumFromInt(d.left);
            if (op != .exclamation_token) return null;
            const inner = evalExprKnownValue(nodes, binder, d.right, tracked, invariant_facts) orelse return null;
            return .{ .boolean = !inner.toBool() };
        },
        .parenthesized_expression => {
            const inner_ref = maybeUnwrapRef(n) orelse return null;
            return evalExprKnownValue(nodes, binder, inner_ref, tracked, invariant_facts);
        },
        .binary_expression => {
            const d = getPackedData(n);
            const op: SyntaxKind = @enumFromInt(n.len);
            if (op != .equals_equals_equals_token and op != .exclamation_equals_equals_token) return null;
            const l = evalExprKnownValue(nodes, binder, d.left, tracked, invariant_facts) orelse return null;
            const r = evalExprKnownValue(nodes, binder, d.right, tracked, invariant_facts) orelse return null;
            const eq = knownValueEql(l, r);
            return .{ .boolean = if (op == .equals_equals_equals_token) eq else !eq };
        },
        .element_access_expression => {
            const d = getPackedData(n);
            const base = nodes.at(d.left);
            if (base.kind != .identifier) return null;
            const idx = nodes.at(d.right);
            if (idx.kind != .numeric_literal or parser.getNumber(idx) != 0) return null;
            const sym = binder.getSymbol(d.left) orelse return null;
            if (sym == 0) return null;
            const facts = invariant_facts orelse return null;
            for (tracked) |t| {
                if (t.sym != sym) continue;
                const gv = t.graph_value orelse return null;
                if (facts.contains(gv)) return .truthy_reference;
                return null;
            }
            return null;
        },
        .call_expression => {
            // unwraps small iife
            const d = getPackedData(n);
            if (d.right != 0) return null; // has args
            const callee_ref = blk: {
                const callee = nodes.at(d.left);
                break :blk if (callee.kind == .parenthesized_expression) (maybeUnwrapRef(callee) orelse return null) else d.left;
            };
            const arrow = nodes.at(callee_ref);
            if (arrow.kind != .arrow_function) return null;
            if (getPackedData(arrow).left != 0) return null; // has params
            return evalExprKnownValue(nodes, binder, getPackedData(arrow).right, tracked, invariant_facts);
        },
        else => return evalLiteral(nodes, expr_ref),
    }
}

fn evalCondition(
    nodes: *const BumpAllocator(AstNode),
    binder: *const parser.Binder,
    expr_ref: NodeRef,
    tracked: []const TrackedParam,
    invariant_facts: ?*const std.AutoHashMapUnmanaged(ValueRef, void),
) ?bool {
    const v = evalExprKnownValue(nodes, binder, expr_ref, tracked, invariant_facts) orelse return null;
    return v.toBool();
}

fn tryUpdateTrackedFromAssignment(
    nodes: *const BumpAllocator(AstNode),
    binder: *const parser.Binder,
    stmt: *const AstNode,
    tracked: []TrackedParam,
    invariant_facts: ?*const std.AutoHashMapUnmanaged(ValueRef, void),
) void {
    if (stmt.kind != .expression_statement) return;
    const inner_ref = unwrapRef(stmt);
    const inner = nodes.at(inner_ref);
    if (inner.kind != .binary_expression or inner.len != @intFromEnum(SyntaxKind.equals_token)) return;
    const d = getPackedData(inner);
    const lhs = nodes.at(d.left);
    if (lhs.kind != .identifier) return;
    const sym = binder.getSymbol(d.left) orelse return;
    if (sym == 0) return;
    for (tracked) |*t| {
        if (t.sym != sym) continue;
        t.value = evalExprKnownValue(nodes, binder, d.right, tracked, invariant_facts);
        t.graph_value = null; // reassigned to a local expression, no longer traceable to a captured graph value
        return;
    }
}

fn tryTrackNewLocal(
    nodes: *const BumpAllocator(AstNode),
    binder: *const parser.Binder,
    stmt: *const AstNode,
    tracked: *std.ArrayListUnmanaged(TrackedParam),
    invariant_facts: ?*const std.AutoHashMapUnmanaged(ValueRef, void),
) !void {
    if (stmt.kind != .variable_statement) return;
    const decls_head = maybeUnwrapRef(stmt) orelse return;
    const decl_ref = decls_head;
    const decl = nodes.at(decl_ref);
    if (decl.next != 0) return; // only a single declarator
    if (decl.kind != .variable_declaration) return;
    const d = getPackedData(decl);
    if (d.right == 0) return; // no initializer
    const name_node = nodes.at(d.left);
    if (name_node.kind != .identifier) return; // no destructuring
    const sym = binder.getSymbol(d.left) orelse return;
    if (sym == 0) return;
    const value = evalExprKnownValue(nodes, binder, d.right, tracked.items, invariant_facts);
    try tracked.append(getAllocator(), .{ .sym = sym, .value = value });
}

const SymbolUseChecker = struct {
    nodes: *const BumpAllocator(AstNode),
    binder: *const parser.Binder,
    sym: parser.SymbolRef,
    found: bool = false,

    pub fn visit(self: *@This(), node: *const AstNode, ref: NodeRef) anyerror!void {
        if (self.found or ref == 0) return;
        if (node.kind == .identifier) {
            const sym = self.binder.getSymbol(ref) orelse return;
            if (sym == self.sym) self.found = true;
            return;
        }
        try parser.forEachChild(self.nodes, node, self);
    }
};

fn isSymbolUsedInExpr(nodes: *const BumpAllocator(AstNode), binder: *const parser.Binder, ref: NodeRef, sym: parser.SymbolRef) bool {
    if (ref == 0) return false;
    var checker = SymbolUseChecker{ .nodes = nodes, .binder = binder, .sym = sym };
    checker.visit(nodes.at(ref), ref) catch return true;
    return checker.found;
}

fn isSymbolUsedInStatement(
    nodes: *const BumpAllocator(AstNode),
    binder: *const parser.Binder,
    stmt_ref: NodeRef,
    sym: parser.SymbolRef,
) bool {
    const stmt = nodes.at(stmt_ref);
    if (stmt.kind == .variable_statement) {
        const decls_head = maybeUnwrapRef(stmt) orelse return false;
        var it = NodeIterator.init(nodes, decls_head);
        while (it.nextRef()) |decl_ref| {
            const decl = nodes.at(decl_ref);
            if (decl.kind != .variable_declaration) {
                if (isSymbolUsedInExpr(nodes, binder, decl_ref, sym)) return true;
                continue;
            }
            const d = getPackedData(decl);
            if (d.right != 0 and isSymbolUsedInExpr(nodes, binder, d.right, sym)) return true;
        }
        return false;
    }
    if (stmt.kind == .expression_statement) {
        const inner_ref = unwrapRef(stmt);
        const inner = nodes.at(inner_ref);
        if (inner.kind == .binary_expression and inner.len == @intFromEnum(SyntaxKind.equals_token)) {
            const d = getPackedData(inner);
            const lhs = nodes.at(d.left);
            if (lhs.kind == .identifier) {
                return isSymbolUsedInExpr(nodes, binder, d.right, sym);
            }
            return isSymbolUsedInExpr(nodes, binder, d.left, sym) or isSymbolUsedInExpr(nodes, binder, d.right, sym);
        }
        return isSymbolUsedInExpr(nodes, binder, inner_ref, sym);
    }
    return isSymbolUsedInExpr(nodes, binder, stmt_ref, sym);
}

fn isSymbolUsedInStatements(
    nodes: *const BumpAllocator(AstNode),
    binder: *const parser.Binder,
    stmts: []const NodeRef,
    sym: parser.SymbolRef,
) bool {
    for (stmts) |s| {
        if (isSymbolUsedInStatement(nodes, binder, s, sym)) return true;
    }
    return false;
}

fn knownValueEql(a: ?KnownValue, b: ?KnownValue) bool {
    if (a == null or b == null) return a == null and b == null;
    return std.meta.eql(a.?, b.?);
}

fn cloneStmtForList(nodes: *BumpAllocator(AstNode), ref: NodeRef) !NodeRef {
    if (ref == 0) return 0;
    var clone = nodes.at(ref).*;
    clone.next = 0;
    return nodes.push(clone);
}

fn dceProcessStatement(
    nodes: *BumpAllocator(AstNode),
    binder: *const parser.Binder,
    stmt_ref: NodeRef,
    tracked: *std.ArrayListUnmanaged(TrackedParam),
    out: *std.ArrayList(NodeRef),
    invariant_facts: ?*const std.AutoHashMapUnmanaged(ValueRef, void),
) anyerror!bool {
    const stmt = nodes.at(stmt_ref);
    if (stmt.kind == .if_statement) {
        const d = getPackedData(stmt);
        const cond_ref = d.left;
        const then_ref = d.right;
        const else_ref = stmt.len;
        if (evalCondition(nodes, binder, cond_ref, tracked.items, invariant_facts)) |taken| {
            const branch = if (taken) then_ref else else_ref;
            if (branch != 0) {
                const branch_node = nodes.at(branch);
                if (branch_node.kind == .block) {
                    const inner_head = maybeUnwrapRef(branch_node) orelse 0;
                    _ = try dceWalkStatements(nodes, binder, inner_head, tracked, out, invariant_facts);
                } else {
                    _ = try dceProcessStatement(nodes, binder, branch, tracked, out, invariant_facts);
                }
            }
            return true;
        }
 
        const orig_len = tracked.items.len;

        var then_tracked = try tracked.clone(getAllocator());
        defer then_tracked.deinit(getAllocator());
        var then_stmts = std.ArrayList(NodeRef).init(getAllocator());
        defer then_stmts.deinit();
        var then_changed = false;
        if (then_ref != 0) {
            const then_node = nodes.at(then_ref);
            if (then_node.kind == .block) {
                const head = maybeUnwrapRef(then_node) orelse 0;
                then_changed = try dceWalkStatements(nodes, binder, head, &then_tracked, &then_stmts, invariant_facts);
            } else {
                then_changed = try dceProcessStatement(nodes, binder, then_ref, &then_tracked, &then_stmts, invariant_facts);
            }
        }

        var else_tracked = try tracked.clone(getAllocator());
        defer else_tracked.deinit(getAllocator());
        var else_stmts = std.ArrayList(NodeRef).init(getAllocator());
        defer else_stmts.deinit();
        var else_changed = false;
        if (else_ref != 0) {
            const else_node = nodes.at(else_ref);
            if (else_node.kind == .block) {
                const head = maybeUnwrapRef(else_node) orelse 0;
                else_changed = try dceWalkStatements(nodes, binder, head, &else_tracked, &else_stmts, invariant_facts);
            } else {
                else_changed = try dceProcessStatement(nodes, binder, else_ref, &else_tracked, &else_stmts, invariant_facts);
            }
        }

        for (tracked.items[0..orig_len], 0..) |*t, i| {
            t.value = if (knownValueEql(then_tracked.items[i].value, else_tracked.items[i].value)) then_tracked.items[i].value else null;
            if (t.value == null) t.graph_value = null;
        }

        if (!then_changed and !else_changed) {
            try out.append(try cloneStmtForList(nodes, stmt_ref));
            return false;
        }

        var factory = Factory{ .nodes = nodes };
        const new_then: NodeRef = if (then_ref == 0) 0 else if (!then_changed) then_ref else try factory.createBlock(then_stmts.items);
        const new_else: NodeRef = if (else_ref == 0) 0 else if (!else_changed) else_ref else try factory.createBlock(else_stmts.items);
        const new_if = try factory.createIfStatement(cond_ref, new_then, new_else);
        try out.append(new_if);
        return true;
    }

    tryUpdateTrackedFromAssignment(nodes, binder, stmt, tracked.items, invariant_facts);
    try tryTrackNewLocal(nodes, binder, stmt, tracked, invariant_facts);
    try out.append(try cloneStmtForList(nodes, stmt_ref));
    return false;
}

fn dceWalkStatements(
    nodes: *BumpAllocator(AstNode),
    binder: *const parser.Binder,
    stmts_head: NodeRef,
    tracked: *std.ArrayListUnmanaged(TrackedParam),
    out: *std.ArrayList(NodeRef),
    invariant_facts: ?*const std.AutoHashMapUnmanaged(ValueRef, void),
) anyerror!bool {
    var changed = false;
    var it = NodeIterator.init(nodes, stmts_head);
    while (it.nextRef()) |stmt_ref| {
        if (try dceProcessStatement(nodes, binder, stmt_ref, tracked, out, invariant_facts)) changed = true;
    }
    return changed;
}

// used to normalize some stuff and manage analysis state
const JsComputation = struct {
    file: *parser.ParsedFile = undefined,
    reference_collector: ?*ReferenceCollector = null,
    is_exp: bool = true,
    is_implicit_iife: bool = false,
    root_node: NodeRef = undefined,
    symbol_replacements: std.AutoArrayHashMapUnmanaged(parser.SymbolRef, NodeRef) = .{},
    
    // you can represent the "current execution environment" as an explicit value (as a computation or opaque)
    // then, you thread this value into all "foreign" calls, including known subjects that depend on the execution env transitively
    inferred_order: u32 = 0,

    // assumes normalized code
    // notably, we do not expect parens as the top node
    pub fn init(source: []const u8) !*@This() {
        std.debug.assert(source.len != 0);
        const this = try getAllocator().create(@This());
        errdefer getAllocator().destroy(this);
        this.* = .{};
        const first = source[0];
        const last = source[source.len-1];
        if (last == ';') {
            this.is_exp = false;
            this.file = try parser.ParsedFile.createFromBuffer(source[0..source.len-1], null, false, null);
        } else if (first == '(' and last == ')') {
            this.is_implicit_iife = true;
            this.file = try parser.ParsedFile.createFromExpression(source, null);
        } else {
            this.file = try parser.ParsedFile.createFromExpression(source, null);
        }
        var root_node = unwrapRef(this.file.ast.nodes.at(this.file.ast.start));
        if (this.is_exp and this.file.ast.nodes.at(root_node).kind == .expression_statement) {
            root_node = unwrapRef(this.file.ast.nodes.at(root_node));
        }
        if (this.is_exp and this.file.ast.nodes.at(root_node).kind == .parenthesized_expression) {
            root_node = unwrapRef(this.file.ast.nodes.at(root_node));
        }
        this.root_node = root_node;
        return this;
    }

    pub fn deinit(this: *@This()) void {
        this.file.deinit();
        if (this.reference_collector) |c| {
            c.deinit();
            getAllocator().destroy(c);
        }
    }

    fn getReferenceCollector(this: *@This()) !*ReferenceCollector {
        if (this.reference_collector) |c| return c;
        const c = try ReferenceCollector.init(this.file);
        this.reference_collector = try getAllocator().create(ReferenceCollector);
        this.reference_collector.?.* = c;
        return this.reference_collector.?;        
    }

    fn fmtOrdinal(ordinal: u32, buf: []u8) []const u8 {
        var c: u32 = 0;
        var n = ordinal;
        while (n > 0) {
            const rem: u8 = @intCast(@rem(n, 10));
            n = n / 10;
            buf[c] = '0' + rem;
            c += 1;
        }
        if (c == 0) {
            buf[0] = '0';
            c += 1;
        }
        return buf[0..c];
    }

    pub fn findFreeVariable(this: *@This(), ordinal: u32) ?parser.SymbolRef {
        var buf: [16]u8 = undefined;
        const suffix = fmtOrdinal(ordinal, &buf);
        for (this.file.binder.unbound_symbols.values()) |x| {
            const sym = this.file.binder.symbols.at(x);
            const binding = this.file.ast.nodes.at(sym.binding);
            std.debug.assert(binding.kind == .identifier);
            const text = parser.getSlice(binding, u8);
            if (text.len != suffix.len+1) continue;
            if (text[0] != '$' or text.len == 1 or text[1] == '$') continue;
            if (std.mem.eql(u8, text[1..], suffix)) return x;
        }
        return null;
    }

    pub fn inlineValue(this: *@This(), symbol_ref: parser.SymbolRef, graph: *ValueGraph, value_ref: ValueRef) !bool {
        const factory = parser.Factory{ .nodes = &this.file.ast.nodes };
        const literal = try graph.valueToParseNode(value_ref, factory) orelse return false;
        try this.symbol_replacements.put(getAllocator(), symbol_ref, literal);
        return true;
    }

    // pub fn inlinePartialInput(this: *@This(), graph: *ValueGraph, value_ref: ValueRef) !?ValueRef {
    //     const val = try graph.getFollowedValue(value_ref);
    //     const factory = parser.Factory{ .nodes = &this.file.ast.nodes };
    //     const literal = try graph.valueToParseNode(value_ref, factory) orelse return false;
    //     try this.symbol_replacements.put(getAllocator(), symbol_ref, literal);
    //     return true;
    // }

    // pub fn eval(this: *@This(), graph: *ValueGraph, input: ValueRef) anyerror!?ValueRef {
        
    // }

    pub fn simplify(this: *@This(), graph: *ValueGraph, input: ValueRef) !?ValueRef {
        const refs = try this.getReferenceCollector();

        const followed_input = try graph.getFollowedValue(input);
        std.debug.assert(followed_input.kind == .array);
        var s: ValueRef = followed_input.slot0;
        var ordinal: u32 = 0;
        while (s != 0) {
            if (this.findFreeVariable(ordinal)) |sym_ref| {
                const el_node = try graph.getFollowedValue(s);
                const f: u32 = switch (el_node.kind) {
                    .true => @intFromEnum(ValueFacts.true),
                    .false => @intFromEnum(ValueFacts.false),
                    .null => @intFromEnum(ValueFacts.null),
                    .undefined => @intFromEnum(ValueFacts.undefined),
                    .object => @intFromEnum(ValueFacts.object),
                    .array => @intFromEnum(ValueFacts.object),
                    .number => getNumberFacts(graph.getDouble(el_node)),
                    .string => getStringFacts(graph.getString(el_node)),
                    .computed => @intFromEnum(ValueFacts.any),
                    else => @intFromEnum(ValueFacts.any),
                };
                try refs.external_facts.put(getAllocator(), sym_ref, f);
            }
            s = graph.getValue(s).next;
            ordinal += 1;
        }

        var replacements = std.AutoArrayHashMap(NodeRef, NodeRef).init(getAllocator());
        var reducer = JsCodeReducer{
            .refs = refs,
            .nodes = &this.file.ast.nodes,
            .factory = .{ .nodes = &this.file.ast.nodes },
            .replacements = &replacements,
            .symbol_replacements = &this.symbol_replacements,
        };
        try reducer.reduce();

        var d = this.file.ast;
        d.start = replacements.get(d.start) orelse d.start;

        const res = try parser.printWithOptions(d, .{
            .replacements = &replacements,
            .symbol_replacements = &this.symbol_replacements,
        });

        return try graph.createString(res.contents);
    }

    pub fn mergeWith(this: *@This()) !?*JsComputation {
        _ = this;
        unreachable;
    }
};

fn createCommaExpressionFromList(
    refs: *ReferenceCollector,
    arg_start: NodeRef,
    param_syms: *std.ArrayList(parser.SymbolRef),
) !NodeRef {
    const nodes = &refs.file.ast.nodes;
    var factory = parser.Factory{ .nodes = nodes };
    if (arg_start == 0) return try factory.createVoidZero();
    var l: NodeRef = 0;
    var iter = NodeIterator.init(nodes, arg_start);
    var needs_parens = false;
    var count: u32 = 0;
    while (iter.nextRef()) |x| {
        count += 1;
        if (l == 0) {
            l = x;
            continue;
        }
        needs_parens = true;
        l = try factory.createBinaryExpression(l, .comma_token, x);
    }
    if (count < param_syms.items.len) {
        for (param_syms.items[count..]) |s| {
            const init = refs.getParamInit(s) orelse continue;
            // FIXME: this would break
            // ((a = b(), c = a) => {})()
            if (l == 0) {
                l = init;
            } else {
                needs_parens = true;
                l = try factory.createBinaryExpression(l, .comma_token, init);
            }
        }
    }
    if (needs_parens) l = try factory.createParenthesizedExpression(l);
    return try factory.createVoidExpression(l);
}

// (function(a, b, [c] = [1]) { return (d = (c = a[0])) => b + d })([2],3)

fn tryInlineIife(
    refs: *ReferenceCollector,
    subject: NodeRef,
    arg_start: NodeRef,
    symbol_replacements: *std.AutoArrayHashMapUnmanaged(parser.SymbolRef, NodeRef),
) !?parser.NodeRef {
    const nodes = &refs.file.ast.nodes;
    var factory = parser.Factory{ .nodes = nodes };
    var r = subject;
    var n = nodes.at(subject);
    while (n.kind == .parenthesized_expression) {
        r = unwrapRef(n);
        n = nodes.at(r);
    }
    const params_start = switch (n.kind) {
        .arrow_function => getLeft(n),
        .function_expression => getRight(n),
        else => return null,
    };
    if (n.hasFlag(.@"async")) return null;
    var param_syms = std.ArrayList(parser.SymbolRef).init(getAllocator());
    defer param_syms.deinit();
    var params_iter = NodeIterator.init(nodes, params_start);
    while (params_iter.next()) |p| {
        const binding = getLeft(p);
        // TODO: destructuring...
        const b = nodes.at(binding);
        if (b.kind != .identifier) return null;
        // TODO: ...spread
        if (b.hasFlag(.generator)) return null;
        // TODO: we can still inline an IIFE if assigned, however, it requires returning a block
        const sym_ref = refs.file.binder.getSymbol(binding) orelse return null;
        if (refs.isAssignedTo(sym_ref)) return null;
        if (!refs.isUsedExactlyOnce(sym_ref)) return null;
        if (refs.getParamInit(sym_ref) != null) {
            if (!isArgDefinitelyNotUndefined(refs, arg_start, @intCast(param_syms.items.len))) {
                return null;
            }
        }
        try param_syms.append(sym_ref);
    }
    const body_ref = switch (n.kind) {
        .arrow_function => getRight(n),
        .function_expression => n.len,
        else => return null,
    };
    std.debug.assert(body_ref != 0);
    var body_exp_ref = body_ref;
    const body = nodes.at(body_ref);
    if (body.kind == .block) {
        const s = maybeUnwrapRef(body) 
            orelse return try createCommaExpressionFromList(refs, arg_start, &param_syms);
        var iter = NodeIterator.init(nodes, s);
        const t = iter.tail();
        if (s != t) return null; // TODO: produce a block
        const u = nodes.at(t);
        switch (u.kind) {
            .return_statement => {
                const inner = maybeUnwrapRef(u) 
                    orelse return try createCommaExpressionFromList(refs, arg_start, &param_syms);
                body_exp_ref = inner;
            },
            .expression_statement => {
                body_exp_ref = unwrapRef(u);
                body_exp_ref = try factory.createVoidExpression(try factory.parens(.void_expression, body_exp_ref));
            },
            else => return null,
        }
        if (nodes.at(body_exp_ref).kind == .object_literal_expression) {
            body_exp_ref = try factory.createParenthesizedExpression(body_exp_ref);
        }
    } // else: concise body
    
    var args_iter = NodeIterator.init(nodes, arg_start);
    // we'd have to show the remainder either has no effects or make a block. can't do comma exp w/o changing order.
    if (args_iter.computeLength() != param_syms.items.len) return null;
    // we cannot bail out past this point.
    var count: u32 = 0;
    while (args_iter.nextRef()) |x| {
        const s = param_syms.items[count];        
        count += 1;
        // TODO: parens rules, grab reference to determine ctx
        try symbol_replacements.put(getAllocator(), s, x);
    }
    return body_exp_ref;
}

fn isArgDefinitelyNotUndefined(refs: *ReferenceCollector, args_start: NodeRef, ordinal: u32) bool {
    var i: u32 = 0;
    var iter = NodeIterator.init(&refs.file.ast.nodes, args_start);
    while (iter.nextRef()) |x| {
        if (i == ordinal) {
            return refs.isExpDefinitelyNotUndefined(x);
        }
        i += 1;
    }
    return false;
}

// const SymbolReplacer = struct {
//     file: *parser.ParsedFile,
//     replacements: *std.AutoArrayHashMap(NodeRef, NodeRef),
//     symbol_replacements: *std.AutoHashMapUnmanaged(parser.SymbolRef, NodeRef) = .{},
// };

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
// 6. computation peeling - turns structural computations into 
//
const Optimizer = struct {
    values: *ValueParser,
    graph: *ValueGraph,
    invariant_facts: std.AutoHashMapUnmanaged(ValueRef, void) = .{},

    pub fn countReferences(this: *@This(), root: ValueRef) !std.AutoHashMapUnmanaged(ValueRef, u32) {
        var counts = std.AutoHashMapUnmanaged(ValueRef, u32){};
        var visited = std.AutoHashMapUnmanaged(ValueRef, void){};
        defer visited.deinit(getAllocator());
        try this.walkForRefCounts(root, &counts, &visited);
        return counts;
    }

    fn bumpRefCount(this: *@This(), counts: *std.AutoHashMapUnmanaged(ValueRef, u32), ref: ValueRef) !void {
        _ = this;
        if (ref == 0) return;
        const gp = try counts.getOrPutValue(getAllocator(), ref, 0);
        gp.value_ptr.* += 1;
    }

    fn walkForRefCounts(
        this: *@This(),
        ref: ValueRef,
        counts: *std.AutoHashMapUnmanaged(ValueRef, u32),
        visited: *std.AutoHashMapUnmanaged(ValueRef, void),
    ) anyerror!void {
        if (ref == 0) return;
        const n = this.graph.getValue(ref);
        switch (n.kind) {
            .string, .number, .null, .undefined, .true, .false => return,
            else => {},
        }
        if (visited.contains(ref)) return;
        try visited.put(getAllocator(), ref, {});
        switch (n.kind) {
            .computed => {
                try this.bumpRefCount(counts, n.slot0);
                try this.walkForRefCounts(n.slot0, counts, visited);
                try this.bumpRefCount(counts, n.slot1);
                try this.walkForRefCounts(n.slot1, counts, visited);
            },
            .array, .object => {
                var s = n.slot0;
                while (s != 0) {
                    try this.bumpRefCount(counts, s);
                    try this.walkForRefCounts(s, counts, visited);
                    s = this.graph.getValue(s).next;
                }
            },
            .ref => {
                const target = try this.graph.followRefNode(n);
                try this.bumpRefCount(counts, target);
                try this.walkForRefCounts(target, counts, visited);
            },
            else => {},
        }
    }

    fn refCountOf(this: *@This(), counts: *const std.AutoHashMapUnmanaged(ValueRef, u32), ref: ValueRef) !u32 {
        // const resolved = try this.graph.followAllRefs(ref);
        // return counts.get(resolved) orelse 0;
        _ = counts;
        if (try this.isReferenced(ref)) return 2;
        return 0;
    }

    fn isReferenced(this: *@This(), ref: ValueRef) !bool {
        const resolved = try this.graph.followAllRefs(ref);
        return this.graph.getRefCount(resolved) > 1;
    }

    fn createTemplatedSubject(this: *@This(), kind: []const u8, tmpl: []const u8) !ValueRef {
        const kind_key = try this.graph.createString("kind");
        const kind_val = try this.graph.createString(kind);
        const template_key = try this.graph.createString("template");
        const template_val = try this.graph.createString(tmpl);
        return try this.graph.createObjectFromPairs(&.{
            .{ kind_key, kind_val },
            .{ template_key, template_val },
        });
    }

    const TemplatedSubject = struct {
        kind: []const u8,
        template: []const u8,
        is_block: bool,
        has_decls: bool,
    };

    fn getTemplatedSubject(this: *@This(), ref: ValueRef) !?TemplatedSubject {
        const subj_node = try this.graph.getFollowedValue(this.graph.getSubject(this.graph.getValue(ref)));
        if (subj_node.kind != .object) return null;
        const kind_val_ref = try this.graph.getStringKeyPropertyValue(subj_node, "kind") orelse return null;
        const kind_node = try this.graph.getFollowedValue(kind_val_ref);
        if (kind_node.kind != .string) return null;
        const tmpl_val_ref = try this.graph.getStringKeyPropertyValue(subj_node, "template") orelse return null;
        const tmpl_node = try this.graph.getFollowedValue(tmpl_val_ref);
        if (tmpl_node.kind != .string) return null;

        var is_block = false;
        if (try this.graph.getStringKeyPropertyValue(subj_node, "isBlock")) |v| {
            const vn = try this.graph.getFollowedValue(v);
            if (vn.kind == .true or vn.kind == .false) is_block = this.graph.getBoolean(vn);
        }
        var has_decls = false;
        if (try this.graph.getStringKeyPropertyValue(subj_node, "hasDecls")) |v| {
            const vn = try this.graph.getFollowedValue(v);
            if (vn.kind == .true or vn.kind == .false) has_decls = this.graph.getBoolean(vn);
        }

        return .{
            .kind = this.graph.getString(kind_node),
            .template = this.graph.getString(tmpl_node),
            .is_block = is_block,
            .has_decls = has_decls,
        };
    }

    fn canInlineAsLiteral(this: *@This(), ref: ValueRef, counts: *const std.AutoHashMapUnmanaged(ValueRef, u32)) anyerror!bool {
        const resolved = try this.graph.followAllRefs(ref);
        if ((try this.refCountOf(counts, resolved)) > 1) return false;
        const n = this.graph.getValue(resolved);
        switch (n.kind) {
            .array => {
                var i: u32 = 0;
                while (this.graph.getArrayElement(n, i)) |el| : (i += 1) {
                    if (!(try this.canInlineAsLiteral(el, counts))) return false;
                }
            },
            .object => {
                var s = n.slot0;
                while (s != 0) {
                    const value_ref = this.graph.getValue(s).next;
                    if (!(try this.canInlineAsLiteral(value_ref, counts))) return false;
                    s = this.graph.getValue(value_ref).next;
                }
            },
            else => {},
        }
        return true;
    }

    // inlines a factory fn call if the factory is only referenced once
    pub fn tryInlineComputationCall(
        this: *@This(),
        computed_ref: ValueRef,
        counts: *const std.AutoHashMapUnmanaged(ValueRef, u32),
    ) !bool {
        const node = this.graph.getValue(computed_ref);
        if (node.kind != .computed) return false;

        const subject_ref = try this.graph.followAllRefs(this.graph.getSubject(node));
        const subject_node = this.graph.getValue(subject_ref);
        if (subject_node.kind != .string) return false;

        var comp = try JsComputation.init(this.graph.getString(subject_node));
        if (try comp.simplify(this.graph, this.graph.getInput(node))) |x| {
            const qq = this.graph.getString(this.graph.getValue(x));
            std.debug.print("{s}\n", .{qq});
        }

        if ((try this.refCountOf(counts, subject_ref)) > 1) return false;

        const input_ref = try this.graph.followAllRefs(this.graph.getInput(node));
        const input_node = this.graph.getValue(input_ref);
        if (input_node.kind != .array) return false;

        const subject_text = this.graph.getString(subject_node);

        // TODO: get rid of all this stringy code
        if (containsDollarDigit(subject_text)) return false;

        const parsed = try parser.ParsedFile.createFromExpression(subject_text, null);
        defer parsed.deinit();

        const fn_node_ref = getInnerFunctionExpr(parsed) orelse return false;
        const fn_node = parsed.ast.nodes.at(fn_node_ref);

        const params_head = getPackedData(fn_node).right;
        var param_syms = std.ArrayListUnmanaged(parser.SymbolRef){};
        defer param_syms.deinit(getAllocator());
        {
            var it = NodeIterator.init(&parsed.ast.nodes, params_head);
            while (it.nextRef()) |p_ref| {
                const p = parsed.ast.nodes.at(p_ref);
                const name_ref = getPackedData(p).left;
                const name_node = parsed.ast.nodes.at(name_ref);
                if (name_node.kind != .identifier) return false; // no destructuring params
                const sym = parsed.binder.getSymbol(name_ref) orelse return false;
                if (sym == 0) return false;
                try param_syms.append(getAllocator(), sym);
            }
        }

        // Input array must have exactly one element per param.
        var input_items = std.ArrayListUnmanaged(ValueRef){};
        defer input_items.deinit(getAllocator());
        {
            var i: u32 = 0;
            while (this.graph.getArrayElement(input_node, i)) |el| : (i += 1) {
                try input_items.append(getAllocator(), el);
            }
        }
        if (input_items.items.len != param_syms.items.len) return false;

        const body_block_ref = fn_node.len;
        if (body_block_ref == 0) return false;
        const body_block = parsed.ast.nodes.at(body_block_ref);
        const first_stmt_ref = maybeUnwrapRef(body_block) orelse return false;
        const first_stmt = parsed.ast.nodes.at(first_stmt_ref);
        if (first_stmt.next != 0) return false; // must be a single statement
        if (first_stmt.kind != .return_statement) return false;
        const expr_ref = maybeUnwrapRef(first_stmt) orelse return false;

        // mutated params cannot directly use $N idents, we use an IIFE instead to give it a binding
        var mutated = std.AutoArrayHashMapUnmanaged(parser.SymbolRef, void){};
        defer mutated.deinit(getAllocator());
        try collectAssignedParams(&parsed.ast.nodes, &parsed.binder, expr_ref, param_syms.items, &mutated);

        var literal_texts = try getAllocator().alloc(?[]const u8, param_syms.items.len);
        defer getAllocator().free(literal_texts);
        for (literal_texts) |*t| t.* = null;
        for (param_syms.items, 0..) |sym, i| {
            if (!mutated.contains(sym)) continue;
            if (try this.graph.tryLiteralText(input_items.items[i])) |t| {
                literal_texts[i] = t;
                continue;
            }
            if (!(try this.canInlineAsLiteral(input_items.items[i], counts))) return false;
            literal_texts[i] = try this.graph.renderValueAsLiteral(input_items.items[i]) orelse return false;
        }

        var placeholders = std.ArrayListUnmanaged(NodeRef){};
        defer placeholders.deinit(getAllocator());
        var new_input_items = std.ArrayListUnmanaged(ValueRef){};
        defer new_input_items.deinit(getAllocator());
        var bound_names = std.ArrayListUnmanaged([]const u8){};
        defer bound_names.deinit(getAllocator());
        var bound_literals = std.ArrayListUnmanaged([]const u8){};
        defer bound_literals.deinit(getAllocator());

        var factory = Factory{ .nodes = &parsed.ast.nodes };
        for (param_syms.items, 0..) |sym, i| {
            if (mutated.contains(sym)) {
                var buf: [16]u8 = undefined;
                const name = try std.fmt.bufPrint(&buf, "_p{d}", .{bound_names.items.len});
                const owned = try getAllocator().dupe(u8, name);
                try placeholders.append(getAllocator(), try factory.createIdentifierAllocated(owned));
                try bound_names.append(getAllocator(), owned);
                try bound_literals.append(getAllocator(), literal_texts[i].?);
            } else {
                var buf: [16]u8 = undefined;
                const name = try std.fmt.bufPrint(&buf, "${d}", .{new_input_items.items.len});
                const owned = try getAllocator().dupe(u8, name);
                try placeholders.append(getAllocator(), try factory.createIdentifierAllocated(owned));
                try new_input_items.append(getAllocator(), try this.graph.createRef(input_items.items[i]));
            }
        }

        var replacements = std.AutoArrayHashMap(NodeRef, NodeRef).init(getAllocator());
        defer replacements.deinit();
        try collectParamReplacements(&parsed.ast.nodes, &parsed.binder, expr_ref, param_syms.items, placeholders.items, &replacements);

        var writer = try parser.Writer.init(subject_text.len);
        var printer = parser.Printer(parser.Writer, .{ .use_replacements = true }).init(parsed.ast, &writer);
        printer.skip_types = true;
        printer.replacements = &replacements;
        try printer.visit(parsed.ast.nodes.at(expr_ref));

        const template_text = if (bound_names.items.len == 0)
            try getAllocator().dupe(u8, writer.buf.items)
        else blk: {
            var out = std.ArrayList(u8).init(getAllocator());
            try out.append('(');
            for (bound_names.items, 0..) |n, i| {
                if (i != 0) try out.appendSlice(", ");
                try out.appendSlice(n);
            }
            try out.appendSlice(" => ");
            try out.appendSlice(writer.buf.items);
            try out.appendSlice(")(");
            for (bound_literals.items, 0..) |l, i| {
                if (i != 0) try out.appendSlice(", ");
                try out.appendSlice(l);
            }
            try out.append(')');
            break :blk out.items;
        };

        const subject_obj = try this.createTemplatedSubject("expression-template", template_text);

        const new_input = try this.graph.createArrayFromItems(new_input_items.items);
        const new_computed = try this.graph.createComputed(subject_obj, new_input);
        try this.graph.replaceValue(computed_ref, new_computed);
        return true;
    }

    // we remove computed values in property assignment position so that they become assignment statements
    pub fn tryPeelComputedObjectEntryIntoStatement(
        this: *@This(),
        obj_ref: ValueRef,
        counts: *const std.AutoHashMapUnmanaged(ValueRef, u32),
    ) !bool {
        const obj_node = this.graph.getValue(obj_ref);
        if (obj_node.kind != .object) return false;
        return this.foldObjectSuffix(obj_ref, obj_ref, counts);
    }

    fn foldObjectSuffix(
        this: *@This(),
        current_ref: ValueRef,
        root_ref: ValueRef,
        counts: *const std.AutoHashMapUnmanaged(ValueRef, u32),
    ) anyerror!bool {
        const obj_node = this.graph.getValue(current_ref);
        if (obj_node.kind != .object or obj_node.slot0 == 0) return false;

        var key_ref = obj_node.slot0;
        var value_ref: ValueRef = 0;
        while (true) {
            const key_node = this.graph.getValue(key_ref);
            value_ref = key_node.next;
            const next_key_ref = this.graph.getValue(value_ref).next;
            if (next_key_ref == 0) break;
            key_ref = next_key_ref;
        }

        const clone_ref = try this.tryFoldEntry(current_ref, root_ref, key_ref, value_ref, counts) orelse return false;
        _ = try this.foldObjectSuffix(clone_ref, root_ref, counts);
        return true;
    }

    fn tryFoldEntry(
        this: *@This(),
        current_ref: ValueRef,
        root_ref: ValueRef,
        key_ref: ValueRef,
        value_ref: ValueRef,
        counts: *const std.AutoHashMapUnmanaged(ValueRef, u32),
    ) !?ValueRef {
        const value_resolved = try this.graph.followAllRefs(value_ref);
        const value_node = this.graph.getValue(value_resolved);
        if (value_node.kind != .computed) return null;
        if ((try this.refCountOf(counts, value_resolved)) > 1) return null;

        const subj = try this.getTemplatedSubject(value_resolved) orelse return null;
        const is_expr = std.mem.eql(u8, subj.kind, "expression-template");
        const is_stmt = std.mem.eql(u8, subj.kind, "statement-template");
        if (!is_expr and !is_stmt) return null;

        const expr_template = subj.template;

        const key_resolved = try this.graph.followAllRefs(key_ref);
        const key_node = this.graph.getValue(key_resolved);
        if (key_node.kind != .string) return null; // only plain string keys supported
        const key_text = this.graph.getString(key_node);

        const expr_input_ref = try this.graph.followAllRefs(this.graph.getInput(value_node));
        const expr_input_node = this.graph.getValue(expr_input_ref);
        if (expr_input_node.kind != .array) return null;
        var expr_inputs = std.ArrayListUnmanaged(ValueRef){};
        defer expr_inputs.deinit(getAllocator());
        {
            var i: u32 = 0;
            while (this.graph.getArrayElement(expr_input_node, i)) |el| : (i += 1) {
                try expr_inputs.append(getAllocator(), el);
            }
        }

        // The computation we're constructing represents the property assignment, so we delete the entry to produce a residual object
        const clone_ref = try this.graph.cloneValue(current_ref);
        _ = try this.graph.deleteKey(clone_ref, key_ref);

        const root_resolved = try this.graph.followAllRefs(root_ref);
        var index_map = try getAllocator().alloc(u32, expr_inputs.items.len);
        defer getAllocator().free(index_map);
        var stmt_input_items = std.ArrayListUnmanaged(ValueRef){};
        defer stmt_input_items.deinit(getAllocator());
        try stmt_input_items.append(getAllocator(), clone_ref);
        for (expr_inputs.items, 0..) |item, i| {
            const item_resolved = try this.graph.followAllRefs(item);
            if (item_resolved == root_resolved) {
                index_map[i] = 0;
            } else {
                index_map[i] = @intCast(stmt_input_items.items.len);
                try stmt_input_items.append(getAllocator(), try this.graph.createRef(item));
            }
        }

        const remapped = try remapPlaceholders(expr_template, index_map);

        const access = if (isIdentifier(key_text))
            try std.fmt.allocPrint(getAllocator(), ".{s}", .{key_text})
        else
            try std.fmt.allocPrint(getAllocator(), "[\"{s}\"]", .{key_text});
        defer getAllocator().free(access);

        const stmt_template = if (is_expr)
            try std.fmt.allocPrint(getAllocator(), "$0{s} = {s};", .{ access, remapped })
        else
            try std.fmt.allocPrint(getAllocator(), "$0{s} = $1;\n{s}", .{ access, remapped });

        const stmt_subject = try this.createTemplatedSubject("statement-template", stmt_template);

        const stmt_input = try this.graph.createArrayFromItems(stmt_input_items.items);

        const stmt_computed = try this.graph.createComputed(stmt_subject, stmt_input);
        try this.graph.replaceValue(current_ref, stmt_computed);
        return clone_ref;
    }

    pub fn tryMergeConsecutiveStatementEntries(
        this: *@This(),
        ref: ValueRef,
        counts: *const std.AutoHashMapUnmanaged(ValueRef, u32),
    ) !bool {
        const outer_resolved = try this.graph.followAllRefs(ref);
        const outer = this.graph.getValue(outer_resolved);
        if (outer.kind != .computed) return false;
        if ((try this.getStatementTemplate(outer_resolved)) == null) return false;

        const in_ref = try this.graph.followAllRefs(this.graph.getInput(outer));
        const in_node = this.graph.getValue(in_ref);
        if (in_node.kind != .array) return false;
        const zero_ref = this.graph.getArrayElement(in_node, 0) orelse return false;
        const inner_resolved = try this.graph.followAllRefs(zero_ref);
        if (this.graph.getValue(inner_resolved).kind != .computed) return false;
        if ((try this.refCountOf(counts, inner_resolved)) > 1) return false;
        if ((try this.getStatementTemplate(inner_resolved)) == null) return false;

        if (!(try this.tryMergeStatementValues(inner_resolved, outer_resolved, counts))) return false;

        _ = try this.tryMergeConsecutiveStatementEntries(ref, counts);
        return true;
    }

    fn getStatementTemplate(this: *@This(), value_resolved: ValueRef) !?[]const u8 {
        const subj = try this.getStatementTemplateSubject(value_resolved) orelse return null;
        return subj.template;
    }

    fn getStatementTemplateSubject(this: *@This(), value_resolved: ValueRef) !?TemplatedSubject {
        const subj = try this.getTemplatedSubject(value_resolved) orelse return null;
        if (!std.mem.eql(u8, subj.kind, "statement-template")) return null;
        return subj;
    }

    fn tryMergeStatementValues(
        this: *@This(),
        value1_ref: ValueRef,
        value2_ref: ValueRef,
        counts: *const std.AutoHashMapUnmanaged(ValueRef, u32),
    ) !bool {
        const v1 = this.graph.getValue(value1_ref);
        if (v1.kind != .computed) return false;
        if ((try this.refCountOf(counts, value1_ref)) > 1) return false;
        const v2 = this.graph.getValue(value2_ref);
        if (v2.kind != .computed) return false;
        if ((try this.refCountOf(counts, value2_ref)) > 1) return false;

        const subj1 = try this.getStatementTemplateSubject(value1_ref) orelse return false;
        const subj2 = try this.getStatementTemplateSubject(value2_ref) orelse return false;
        const t1 = subj1.template;
        const t2 = subj2.template;

        const in1_ref = try this.graph.followAllRefs(this.graph.getInput(v1));
        const in1_node = this.graph.getValue(in1_ref);
        if (in1_node.kind != .array) return false;
        const in2_ref = try this.graph.followAllRefs(this.graph.getInput(v2));
        const in2_node = this.graph.getValue(in2_ref);
        if (in2_node.kind != .array) return false;

        var merged_inputs = std.ArrayListUnmanaged(ValueRef){};
        defer merged_inputs.deinit(getAllocator());
        {
            var i: u32 = 0;
            while (this.graph.getArrayElement(in1_node, i)) |el| : (i += 1) {
                try merged_inputs.append(getAllocator(), try this.graph.createRef(el));
            }
        }

        var index_map2 = std.ArrayListUnmanaged(u32){};
        defer index_map2.deinit(getAllocator());
        {
            var i: u32 = 0;
            while (this.graph.getArrayElement(in2_node, i)) |el| : (i += 1) {
                // it's common for computations to share $0 references in some other position
                if (try this.graph.isStrictlySameValueRef(el, value1_ref)) {
                    try index_map2.append(getAllocator(), 0);
                    continue;
                }

                var found: ?u32 = null;
                for (merged_inputs.items, 0..) |existing, j| {
                    if (try this.graph.isStrictlySameValueRef(existing, el)) {
                        found = @intCast(j);
                        break;
                    }
                }
                if (found) |f| {
                    try index_map2.append(getAllocator(), f);
                } else {
                    try index_map2.append(getAllocator(), @intCast(merged_inputs.items.len));
                    try merged_inputs.append(getAllocator(), try this.graph.createRef(el));
                }
            }
        }

        const t2_remapped = try remapPlaceholders(t2, index_map2.items);

        const needs_block = subj1.is_block or subj1.has_decls or subj2.is_block or subj2.has_decls;
        const merged_template = if (needs_block)
            try std.fmt.allocPrint(getAllocator(), "{{\n  {s}\n  {s}\n}}", .{ t1, t2_remapped })
        else
            try std.fmt.allocPrint(getAllocator(), "{s}\n  {s}", .{ t1, t2_remapped });

        const subject_obj = try this.createTemplatedSubject("statement-template", merged_template);
        if (needs_block) {
            try this.graph.setProperty(subject_obj, "isBlock", try this.graph.createBoolean(true));
            try this.graph.setProperty(subject_obj, "hasDecls", try this.graph.createBoolean(subj1.has_decls or subj2.has_decls));
        }

        const merged_input = try this.graph.createArrayFromItems(merged_inputs.items);
        const merged_computed = try this.graph.createComputed(subject_obj, merged_input);
        try this.graph.replaceValue(value1_ref, merged_computed);
        try this.graph.replaceValue(value2_ref, try this.graph.createRef(merged_computed));
        return true;
    }

    // TODO: needs to use better machinery, this is bleh
    pub fn tryInlineValue(
        this: *@This(),
        computed_ref: ValueRef,
        counts: *const std.AutoHashMapUnmanaged(ValueRef, u32),
    ) !bool {
        const node = this.graph.getValue(computed_ref);
        if (node.kind != .computed) return false;

        const subj = try this.getTemplatedSubject(computed_ref) orelse return false;
        const template_kind = subj.kind;
        const is_stmt = std.mem.eql(u8, template_kind, "statement-template");
        const is_expr = std.mem.eql(u8, template_kind, "expression-template");
        if (!is_stmt and !is_expr) return false;

        const template_text = subj.template;

        const input_ref = try this.graph.followAllRefs(this.graph.getInput(node));
        const input_node = this.graph.getValue(input_ref);
        if (input_node.kind != .array) return false;
        var input_items = std.ArrayListUnmanaged(ValueRef){};
        defer input_items.deinit(getAllocator());
        {
            var i: u32 = 0;
            while (this.graph.getArrayElement(input_node, i)) |el| : (i += 1) {
                try input_items.append(getAllocator(), el);
            }
        }
        if (input_items.items.len == 0) return false;

        const DeclEntry = struct { name: []const u8, literal: []const u8, source: ValueRef };
        var decls = std.ArrayListUnmanaged(DeclEntry){};
        defer decls.deinit(getAllocator());

        var subst = try getAllocator().alloc([]const u8, input_items.items.len);
        defer getAllocator().free(subst);
        var new_input_items = std.ArrayListUnmanaged(ValueRef){};
        defer new_input_items.deinit(getAllocator());

        for (input_items.items, 0..) |item, i| {
            const resolved = try this.graph.followAllRefs(item);
            inline_check: {
                // $0 of a statement-template is the object being mutated
                if (is_stmt and i == 0) break :inline_check;
                if ((try this.refCountOf(counts, resolved)) > 1) break :inline_check;
                const lit = try this.graph.renderValueAsLiteral(resolved) orelse break :inline_check;
                var buf: [16]u8 = undefined;
                const name = try std.fmt.bufPrint(&buf, "_d{d}", .{decls.items.len});
                const owned = try getAllocator().dupe(u8, name);
                try decls.append(getAllocator(), .{ .name = owned, .literal = lit, .source = resolved });
                subst[i] = owned;
                continue;
            }
            var buf2: [16]u8 = undefined;
            const dollar_name = try std.fmt.bufPrint(&buf2, "${d}", .{new_input_items.items.len});
            subst[i] = try getAllocator().dupe(u8, dollar_name);
            try new_input_items.append(getAllocator(), try this.graph.createRef(item));
        }

        if (decls.items.len == 0) return false;

        var substituted = try substitutePlaceholders(template_text, subst);

        for (decls.items) |*d| {
            const src_node = this.graph.getValue(d.source);
            if (src_node.kind != .array) continue;
            const elem = this.graph.getArrayElement(src_node, 0) orelse continue;
            if (this.graph.getArrayElement(src_node, 1) != null) continue; 
            const elem_resolved = try this.graph.followAllRefs(elem);
            const elem_node = this.graph.getValue(elem_resolved);
            switch (elem_node.kind) {
                .true, .false, .null, .undefined, .number, .string => {},
                else => continue,
            }
            if (!allOccurrencesIndexZero(substituted, d.name)) continue;

            substituted = try stripIndexZero(substituted, d.name);
            d.literal = try this.graph.tryLiteralText(elem_resolved) orelse continue;
        }

        var decl_text = std.ArrayList(u8).init(getAllocator());
        for (decls.items) |d| {
            try decl_text.writer().print("let {s} = {s}\n  ", .{ d.name, d.literal });
        }

        const new_template = if (is_stmt) blk: {
            break :blk try std.fmt.allocPrint(getAllocator(), "{{\n  {s}  {s}\n}}", .{ decl_text.items, substituted });
        } else blk: {
            var params = std.ArrayList(u8).init(getAllocator());
            var args = std.ArrayList(u8).init(getAllocator());
            for (decls.items, 0..) |d, i| {
                if (i != 0) {
                    try params.appendSlice(", ");
                    try args.appendSlice(", ");
                }
                try params.appendSlice(d.name);
                try args.appendSlice(d.literal);
            }
            break :blk try std.fmt.allocPrint(getAllocator(), "({s} => {s})({s})", .{ params.items, substituted, args.items });
        };

        const new_subject = try this.createTemplatedSubject(template_kind, new_template);
        if (is_stmt) {
            try this.graph.setProperty(new_subject, "isBlock", try this.graph.createBoolean(true));
            try this.graph.setProperty(new_subject, "hasDecls", try this.graph.createBoolean(true));
        }

        const new_input = try this.graph.createArrayFromItems(new_input_items.items);
        const new_computed = try this.graph.createComputed(new_subject, new_input);
        try this.graph.replaceValue(computed_ref, new_computed);
        return true;
    }

    fn evalGraphKnownValue(this: *@This(), ref: ValueRef) !?KnownValue {
        const resolved = try this.resolveIdentity(ref);
        const base = try this.canonicalValueIdentity(resolved);
        const n = this.graph.getValue(base);
        return switch (n.kind) {
            .true => .{ .boolean = true },
            .false => .{ .boolean = false },
            .null => .null_,
            .undefined => .undefined_,
            .number => .{ .number = this.graph.getDouble(n) },
            .array, .object => .truthy_reference,
            else => null,
        };
    }

    fn parseSubjectForAnalysis(this: *@This(), computed_ref: ValueRef) !?struct {
        parsed: *parser.ParsedFile,
        param_syms: []parser.SymbolRef,
    } {
        const node = this.graph.getValue(computed_ref);
        if (node.kind != .computed) return null;

        const input_ref = try this.resolveIdentity(this.graph.getInput(node));
        const input_node = this.graph.getValue(input_ref);
        if (input_node.kind != .array) return null;
        var input_len: u32 = 0;
        while (this.graph.getArrayElement(input_node, input_len)) |_| : (input_len += 1) {}

        const maybe_subj = try this.getTemplatedSubject(computed_ref);
        var wrapped: []const u8 = undefined;
        if (maybe_subj) |s| {
            if (!std.mem.eql(u8, s.kind, "expression-template")) return null;
            var params_text = std.ArrayList(u8).init(getAllocator());
            for (0..input_len) |i| {
                if (i != 0) try params_text.appendSlice(", ");
                try params_text.writer().print("${d}", .{i});
            }
            wrapped = try std.fmt.allocPrint(getAllocator(), "(({s}) => ({s}))", .{ params_text.items, s.template });
        } else {
            const subject_resolved = try this.resolveIdentity(this.graph.getSubject(node));
            const subject_node = this.graph.getValue(subject_resolved);
            if (subject_node.kind != .string) return null;
            wrapped = try std.fmt.allocPrint(getAllocator(), "({s})", .{this.graph.getString(subject_node)});
        }

        const parsed = try parser.ParsedFile.createFromBuffer(wrapped, null, false, null);

        var param_syms = std.ArrayListUnmanaged(parser.SymbolRef){};
        if (maybe_subj != null) {
            const wrapped_top_ref = getWrappedTopExpr(parsed) orelse {
                parsed.deinit();
                return null;
            };
            const top_expr_ref = maybeUnwrapRef(parsed.ast.nodes.at(wrapped_top_ref)) orelse {
                parsed.deinit();
                return null;
            };
            const outer_arrow = parsed.ast.nodes.at(top_expr_ref);
            if (outer_arrow.kind != .arrow_function) {
                parsed.deinit();
                return null;
            }
            var it = NodeIterator.init(&parsed.ast.nodes, getPackedData(outer_arrow).left);
            while (it.nextRef()) |p_ref| {
                const p = parsed.ast.nodes.at(p_ref);
                const name_ref = getPackedData(p).left;
                const sym = parsed.binder.getSymbol(name_ref) orelse {
                    parsed.deinit();
                    return null;
                };
                try param_syms.append(getAllocator(), sym);
            }
        } else {
            const fn_node_ref = getInnerFunctionExpr(parsed) orelse {
                parsed.deinit();
                return null;
            };
            const fn_node = parsed.ast.nodes.at(fn_node_ref);
            var it = NodeIterator.init(&parsed.ast.nodes, getPackedData(fn_node).right);
            while (it.nextRef()) |p_ref| {
                const p = parsed.ast.nodes.at(p_ref);
                const name_ref = getPackedData(p).left;
                const name_node = parsed.ast.nodes.at(name_ref);
                if (name_node.kind != .identifier) {
                    parsed.deinit();
                    return null;
                }
                const sym = parsed.binder.getSymbol(name_ref) orelse {
                    parsed.deinit();
                    return null;
                };
                try param_syms.append(getAllocator(), sym);
            }
        }

        return .{ .parsed = parsed, .param_syms = try param_syms.toOwnedSlice(getAllocator()) };
    }

    const ElementUse = struct { computed_ref: ValueRef, position: u32 };

    fn collectElementUses(
        this: *@This(),
        ref: ValueRef,
        uses: *std.AutoHashMapUnmanaged(ValueRef, std.ArrayListUnmanaged(ElementUse)),
        visited: *std.AutoHashMapUnmanaged(ValueRef, void),
    ) anyerror!void {
        if (ref == 0) return;
        const resolved = try this.resolveIdentity(ref);
        if (visited.contains(resolved)) return;
        try visited.put(getAllocator(), resolved, {});

        const n = this.graph.getValue(resolved);
        switch (n.kind) {
            .array, .object => {
                var s = n.slot0;
                while (s != 0) {
                    try this.collectElementUses(s, uses, visited);
                    s = this.graph.getValue(s).next;
                }
            },
            .computed => {
                const input_ref = try this.resolveIdentity(n.slot1);
                const input_node = this.graph.getValue(input_ref);
                if (input_node.kind == .array) {
                    var i: u32 = 0;
                    while (this.graph.getArrayElement(input_node, i)) |el| : (i += 1) {
                        const el_resolved = try this.resolveIdentity(el);
                        const el_node = this.graph.getValue(el_resolved);
                        if (el_node.kind != .array) continue;
                        const elem0 = this.graph.getArrayElement(el_node, 0) orelse continue;
                        const elem0_resolved = try this.resolveIdentity(elem0);
                        const elem0_node = this.graph.getValue(elem0_resolved);
                        if (elem0_node.kind != .number or this.graph.getDouble(elem0_node) <= 0) continue;

                        const gp = try uses.getOrPutValue(getAllocator(), el_resolved, .{});
                        try gp.value_ptr.append(getAllocator(), .{ .computed_ref = resolved, .position = i });
                    }
                }
                try this.collectElementUses(n.slot0, uses, visited);
                try this.collectElementUses(n.slot1, uses, visited);
            },
            else => {},
        }
    }

    fn checkElementUseIsSafe(this: *@This(), computed_ref: ValueRef, position: u32) !bool {
        const analysis = try this.parseSubjectForAnalysis(computed_ref) orelse return false;
        defer analysis.parsed.deinit();
        if (position >= analysis.param_syms.len) return false;
        const sym = analysis.param_syms[position];

        var collector = try ReferenceCollector.init(analysis.parsed);
        defer collector.deinit();
        const nodes = &analysis.parsed.ast.nodes;

        var iter = collector.getReferenceIterator(sym) orelse return true; // never referenced
        while (iter.next()) |r| {
            const access_ref = collector.parents.get(r) orelse return false;
            const access = nodes.at(access_ref);
            if (access.kind != .element_access_expression) return false;
            const ad = getPackedData(access);
            if (ad.left != r) return false; 
            const idx = nodes.at(ad.right);
            if (idx.kind != .numeric_literal or parser.getNumber(idx) != 0) return false; // different index

            const parent2_ref = collector.parents.get(access_ref) orelse continue; // plain read
            const parent2 = nodes.at(parent2_ref);
            if (parent2.kind == .binary_expression) {
                const pd = getPackedData(parent2);
                if (pd.left == access_ref) {
                    const op: SyntaxKind = @enumFromInt(parent2.len);
                    if (op == .plus_equals_token) {
                        const rhs = nodes.at(pd.right);
                        if (rhs.kind == .numeric_literal and parser.getNumber(rhs) > 0) continue; // increment
                    }
                    return false; // some other assignment
                }
            }
        }
        return true;
    }

    pub fn computeInvariantCellFacts(this: *@This(), root: ValueRef) !std.AutoHashMapUnmanaged(ValueRef, void) {
        var uses = std.AutoHashMapUnmanaged(ValueRef, std.ArrayListUnmanaged(ElementUse)){};
        defer {
            var it = uses.valueIterator();
            while (it.next()) |list| list.deinit(getAllocator());
            uses.deinit(getAllocator());
        }
        {
            var visited = std.AutoHashMapUnmanaged(ValueRef, void){};
            defer visited.deinit(getAllocator());
            try this.collectElementUses(root, &uses, &visited);
        }

        var facts = std.AutoHashMapUnmanaged(ValueRef, void){};
        errdefer facts.deinit(getAllocator());

        var it = uses.iterator();
        outer: while (it.next()) |entry| {
            for (entry.value_ptr.items) |use| {
                if (!(try this.checkElementUseIsSafe(use.computed_ref, use.position))) continue :outer;
            }
            try facts.put(getAllocator(), entry.key_ptr.*, {});
        }

        return facts;
    }

    pub fn tryDeadCodeElimination(
        this: *@This(),
        computed_ref: ValueRef,
        counts: *const std.AutoHashMapUnmanaged(ValueRef, u32),
    ) !bool {
        _ = counts;

        const node = this.graph.getValue(computed_ref);
        if (node.kind != .computed) return false;

        const input_ref = try this.resolveIdentity(this.graph.getInput(node));
        const input_node = this.graph.getValue(input_ref);
        if (input_node.kind != .array) return false;
        var input_items = std.ArrayListUnmanaged(ValueRef){};
        defer input_items.deinit(getAllocator());
        {
            var i: u32 = 0;
            while (this.graph.getArrayElement(input_node, i)) |el| : (i += 1) {
                try input_items.append(getAllocator(), el);
            }
        }

        const maybe_subj = try this.getTemplatedSubject(computed_ref);
        if (maybe_subj) |s| {
            if (!std.mem.eql(u8, s.kind, "expression-template")) return false;
        }
        const is_raw = maybe_subj == null;

        var wrapped: []const u8 = undefined;
        if (maybe_subj) |s| {
            var params_text = std.ArrayList(u8).init(getAllocator());
            for (0..input_items.items.len) |i| {
                if (i != 0) try params_text.appendSlice(", ");
                try params_text.writer().print("${d}", .{i});
            }
            wrapped = try std.fmt.allocPrint(getAllocator(), "(({s}) => ({s}))", .{ params_text.items, s.template });
        } else {
            const subject_resolved = try this.resolveIdentity(this.graph.getSubject(node));
            const subject_node = this.graph.getValue(subject_resolved);
            if (subject_node.kind != .string) return false;
            wrapped = try std.fmt.allocPrint(getAllocator(), "({s})", .{this.graph.getString(subject_node)});
        }

        const parsed = try parser.ParsedFile.createFromBuffer(wrapped, null, false, null);
        defer parsed.deinit();

        var layers = std.ArrayListUnmanaged(DceLayer){};
        defer layers.deinit(getAllocator());
        var tracked = std.ArrayListUnmanaged(TrackedParam){};
        defer tracked.deinit(getAllocator());
        var top_param_name_refs = std.ArrayListUnmanaged(NodeRef){};
        defer top_param_name_refs.deinit(getAllocator());

        var start_expr_ref: NodeRef = 0;
        if (is_raw) {
            const fn_node_ref = getInnerFunctionExpr(parsed) orelse return false;
            const fn_node = parsed.ast.nodes.at(fn_node_ref);
            const params_head = getPackedData(fn_node).right;
            var count: usize = 0;
            {
                var it = NodeIterator.init(&parsed.ast.nodes, params_head);
                while (it.nextRef()) |p_ref| {
                    const p = parsed.ast.nodes.at(p_ref);
                    const name_ref = getPackedData(p).left;
                    const name_node = parsed.ast.nodes.at(name_ref);
                    if (name_node.kind != .identifier) return false; // no destructuring params
                    const sym = parsed.binder.getSymbol(name_ref) orelse return false;
                    if (sym == 0) return false;
                    if (count >= input_items.items.len) return false;
                    const val = try this.evalGraphKnownValue(input_items.items[count]);
                    const gv = try this.resolveIdentity(input_items.items[count]);
                    try tracked.append(getAllocator(), .{ .sym = sym, .value = val, .graph_value = gv });
                    try top_param_name_refs.append(getAllocator(), name_ref);
                    count += 1;
                }
            }
            if (count != input_items.items.len) return false; // must be exactly one param per input

            const body_block_ref = fn_node.len;
            if (body_block_ref == 0) return false;
            const body_block = parsed.ast.nodes.at(body_block_ref);
            const first_stmt_ref = maybeUnwrapRef(body_block) orelse return false;
            const first_stmt = parsed.ast.nodes.at(first_stmt_ref);
            if (first_stmt.next != 0) return false; // must be a single statement
            if (first_stmt.kind != .return_statement) return false;
            start_expr_ref = maybeUnwrapRef(first_stmt) orelse return false;
        } else {
            const wrapped_top_ref = getWrappedTopExpr(parsed) orelse return false;
            const top_expr_ref = maybeUnwrapRef(parsed.ast.nodes.at(wrapped_top_ref)) orelse return false;
            const outer_arrow = parsed.ast.nodes.at(top_expr_ref);
            if (outer_arrow.kind != .arrow_function) return false;
            const params_head = getPackedData(outer_arrow).left;
            {
                var params_it = NodeIterator.init(&parsed.ast.nodes, params_head);
                var i: usize = 0;
                while (params_it.nextRef()) |p_ref| : (i += 1) {
                    const p = parsed.ast.nodes.at(p_ref);
                    const name_ref = getPackedData(p).left;
                    const sym = parsed.binder.getSymbol(name_ref) orelse return false;
                    if (sym == 0) return false;
                    if (i >= input_items.items.len) return false;
                    const val = try this.evalGraphKnownValue(input_items.items[i]);
                    const gv = try this.resolveIdentity(input_items.items[i]);
                    try tracked.append(getAllocator(), .{ .sym = sym, .value = val, .graph_value = gv });
                    try top_param_name_refs.append(getAllocator(), name_ref);
                }
            }
            start_expr_ref = getPackedData(outer_arrow).right;
        }

        const top_param_count = tracked.items.len;

        const target_block_ref = try dceUnwrap(parsed, start_expr_ref, &layers, &tracked) orelse return false;
        if (tracked.items.len == 0) return false;

        const stmts_head = maybeUnwrapRef(parsed.ast.nodes.at(target_block_ref)) orelse 0;
        var new_stmts = std.ArrayList(NodeRef).init(getAllocator());
        defer new_stmts.deinit();
        const changed = try dceWalkStatements(&parsed.ast.nodes, &parsed.binder, stmts_head, &tracked, &new_stmts, &this.invariant_facts);

        var still_used = try getAllocator().alloc(bool, tracked.items.len);
        defer getAllocator().free(still_used);
        var any_unused = false;
        for (tracked.items, 0..) |t, i| {
            still_used[i] = isSymbolUsedInStatements(&parsed.ast.nodes, &parsed.binder, new_stmts.items, t.sym);
            if (!still_used[i]) any_unused = true;
        }

        for (tracked.items, 0..) |t, i| {
            if (still_used[i]) continue;
            if (try dceStripDeadWritesFor(&parsed.ast.nodes, &parsed.binder, &new_stmts, t.sym)) {
                still_used[i] = isSymbolUsedInStatements(&parsed.ast.nodes, &parsed.binder, new_stmts.items, t.sym);
            }
        }

        if (!changed and !any_unused) return false;

        var factory = Factory{ .nodes = &parsed.ast.nodes };
        var current_expr = try factory.createBlock(new_stmts.items);

        var li: usize = layers.items.len;
        while (li > 0) {
            li -= 1;
            const layer = layers.items[li];

            if (layer.args_head == 0) {
                const orig_arrow = parsed.ast.nodes.at(layer.arrow_ref);
                current_expr = try factory.createArrowFunction(layer.params_head, current_expr, orig_arrow.flags);
                continue;
            }

            var new_params = std.ArrayList(NodeRef).init(getAllocator());
            defer new_params.deinit();
            var new_args = std.ArrayList(NodeRef).init(getAllocator());
            defer new_args.deinit();
            {
                var params_it = NodeIterator.init(&parsed.ast.nodes, layer.params_head);
                var args_it = NodeIterator.init(&parsed.ast.nodes, layer.args_head);
                while (params_it.nextRef()) |p_ref| {
                    const arg_ref = args_it.nextRef() orelse break;
                    const p = parsed.ast.nodes.at(p_ref);
                    const name_ref = getPackedData(p).left;
                    var drop = false;
                    if (parsed.binder.getSymbol(name_ref)) |sym| {
                        for (tracked.items[layer.tracked_start..layer.tracked_end], layer.tracked_start..) |t, i| {
                            if (t.sym == sym and !still_used[i]) {
                                drop = true;
                                break;
                            }
                        }
                    }
                    if (!drop) {
                        try new_params.append(p_ref);
                        try new_args.append(arg_ref);
                    }
                }
            }

            if (new_params.items.len == 0) {
                if (parsed.ast.nodes.at(current_expr).kind == .block) {
                    current_expr = try factory.createArrowFunction(0, current_expr, 0);
                }
                continue;
            }
            const new_params_list = try factory.createList(new_params.items);
            const new_arrow = try factory.createArrowFunction(new_params_list, current_expr, 0);
            const paren_arrow = try factory.createParenthesizedExpression(new_arrow);
            current_expr = try factory.createCallExpression(paren_arrow, new_args.items);
        }
        const final_expr = current_expr;

        var iife_replacements = std.AutoArrayHashMap(NodeRef, NodeRef).init(getAllocator());
        defer iife_replacements.deinit();
        try collectTrivialIifeReplacements(&parsed.ast.nodes, &parsed.binder, final_expr, &iife_replacements);

        var writer = try parser.Writer.init(wrapped.len);
        var printer = parser.Printer(parser.Writer, .{ .use_replacements = true }).init(parsed.ast, &writer);
        printer.skip_types = true;
        printer.replacements = &iife_replacements;
        try printer.visit(parsed.ast.nodes.at(final_expr));
        const final_expr_text = try getAllocator().dupe(u8, writer.buf.items);

        var new_input_items = std.ArrayListUnmanaged(ValueRef){};
        defer new_input_items.deinit(getAllocator());
        var index_map = try getAllocator().alloc(u32, top_param_count);
        defer getAllocator().free(index_map);
        for (0..top_param_count) |i| {
            if (!still_used[i]) {
                index_map[i] = 0;
                continue;
            }
            index_map[i] = @intCast(new_input_items.items.len);
            try new_input_items.append(getAllocator(), input_items.items[i]);
        }

        if (is_raw) {
            var params_text = std.ArrayList(u8).init(getAllocator());
            var first = true;
            for (0..top_param_count) |i| {
                if (!still_used[i]) continue;
                if (!first) try params_text.appendSlice(", ");
                first = false;
                var pw = try parser.Writer.init(16);
                var pp = parser.Printer(parser.Writer, .{}).init(parsed.ast, &pw);
                pp.skip_types = true;
                try pp.visit(parsed.ast.nodes.at(top_param_name_refs.items[i]));
                try params_text.appendSlice(pw.buf.items);
            }
            const new_factory_text = try std.fmt.allocPrint(
                getAllocator(),
                "function({s}) {{\n  return {s};\n}}",
                .{ params_text.items, final_expr_text },
            );

            const new_subject_str = try this.graph.createString(new_factory_text);
            const new_input = try this.graph.createArrayFromItems(new_input_items.items);
            const new_computed_node = try this.graph.createComputed(new_subject_str, new_input);
            try this.graph.replaceValue(computed_ref, new_computed_node);
            return true;
        }

        const remapped_template = try remapPlaceholders(final_expr_text, index_map);
        const new_subject = try this.createTemplatedSubject("expression-template", remapped_template);
        const new_input = try this.graph.createArrayFromItems(new_input_items.items);
        const new_computed_node = try this.graph.createComputed(new_subject, new_input);
        try this.graph.replaceValue(computed_ref, new_computed_node);
        return true;
    }


    const OptimizationState = struct {
        targets: std.ArrayListUnmanaged(ValueRef) = .{},
        // child ref -> parent object/computed refs that directly reference it
        reverse_deps: std.AutoHashMapUnmanaged(ValueRef, std.ArrayListUnmanaged(ValueRef)) = .{},

        fn deinit(self: *OptimizationState) void {
            self.targets.deinit(getAllocator());
            var it = self.reverse_deps.valueIterator();
            while (it.next()) |list| list.deinit(getAllocator());
            self.reverse_deps.deinit(getAllocator());
        }

        fn addDep(self: *OptimizationState, child: ValueRef, parent: ValueRef) !void {
            const gp = try self.reverse_deps.getOrPutValue(getAllocator(), child, .{});
            try gp.value_ptr.append(getAllocator(), parent);
        }
    };

    fn buildOptimizationState(this: *@This(), root: ValueRef) !OptimizationState {
        var state = OptimizationState{};
        var visited = std.AutoHashMapUnmanaged(ValueRef, void){};
        defer visited.deinit(getAllocator());
        try this.walkForOptState(root, 0, &state, &visited);
        return state;
    }

    // `owner` is the nearest containing value that may be affected by a change in the dependency
    fn walkForOptState(
        this: *@This(),
        ref: ValueRef,
        owner: ValueRef,
        state: *OptimizationState,
        visited: *std.AutoHashMapUnmanaged(ValueRef, void),
    ) anyerror!void {
        if (ref == 0) return;
        if (visited.contains(ref)) {
            if (owner != 0) try state.addDep(ref, owner);
            return;
        }
        try visited.put(getAllocator(), ref, {});

        // `targets` is built leaf-first
        const n = this.graph.getValue(ref);
        switch (n.kind) {
            .computed => {
                if (owner != 0) try state.addDep(ref, owner);
                var s = n.slot0;
                while (s != 0) {
                    try this.walkForOptState(s, ref, state, visited);
                    s = this.graph.getValue(s).next;
                }
                try state.targets.append(getAllocator(), ref);
            },
            .object => {
                if (owner != 0) try state.addDep(ref, owner);
                var s = n.slot0;
                while (s != 0) {
                    try this.walkForOptState(s, ref, state, visited);
                    s = this.graph.getValue(s).next;
                }
                try state.targets.append(getAllocator(), ref);
            },
            .array => {
                var s = n.slot0;
                while (s != 0) {
                    try this.walkForOptState(s, owner, state, visited); // transparent: not a target itself
                    s = this.graph.getValue(s).next;
                }
            },
            .ref => {
                const target = try this.graph.followRefNode(n);
                try this.walkForOptState(target, owner, state, visited); // transparent
            },
            else => {},
        }
    }

    // stages run in order over the graph but can be staggered per-node.
    // a given stage only runs over a node once
    const Stage = enum { inline_call, fold_merge, inline_value, dce };

    fn tryOptimizeTargetStage(
        this: *@This(),
        ref: ValueRef,
        counts: *const std.AutoHashMapUnmanaged(ValueRef, u32),
        comptime stage: Stage,
    ) !bool {
        const n = this.graph.getValue(ref);
        return switch (stage) {
            .inline_call => if (n.kind == .computed) try this.tryInlineComputationCall(ref, counts) else false,
            .fold_merge => blk: {
                var changed = false;
                if (n.kind == .object) {
                    if (try this.tryPeelComputedObjectEntryIntoStatement(ref, counts)) changed = true;
                }
                // `n` might be a new node now
                if (this.graph.getValue(ref).kind == .computed) {
                    if (try this.tryMergeConsecutiveStatementEntries(ref, counts)) changed = true;
                }
                break :blk changed;
            },
            .inline_value => if (n.kind == .computed) try this.tryInlineValue(ref, counts) else false,
            .dce => if (n.kind == .computed) try this.tryDeadCodeElimination(ref, counts) else false,
        };
    }

    fn runStage(
        this: *@This(),
        root: ValueRef,
        state: *const OptimizationState,
        budget: *i64,
        comptime stage: Stage,
    ) !void {
        var worklist = std.ArrayListUnmanaged(ValueRef){};
        defer worklist.deinit(getAllocator());
        var pending = std.AutoHashMapUnmanaged(ValueRef, void){};
        defer pending.deinit(getAllocator());
        for (state.targets.items) |t| {
            try worklist.append(getAllocator(), t);
            try pending.put(getAllocator(), t, {});
        }

        var counts = try this.countReferences(root);
        defer counts.deinit(getAllocator());

        var head: usize = 0;
        while (head < worklist.items.len and budget.* > 0) {
            const ref = worklist.items[head];
            head += 1;
            _ = pending.remove(ref);

            if (!(try this.tryOptimizeTargetStage(ref, &counts, stage))) continue;
            budget.* -= 1;

            // FIXME: the graph should handle this
            counts.deinit(getAllocator());
            counts = try this.countReferences(root);

            if (!pending.contains(ref)) {
                try worklist.append(getAllocator(), ref);
                try pending.put(getAllocator(), ref, {});
            }
            if (state.reverse_deps.get(ref)) |deps| {
                for (deps.items) |dep| {
                    if (!pending.contains(dep)) {
                        try worklist.append(getAllocator(), dep);
                        try pending.put(getAllocator(), dep, {});
                    }
                }
            }
        }

        try this.graph.normalizeRefs(root);
    }

    pub fn optimizeAll(this: *@This(), root: ValueRef) !void {
        try this.graph.normalizeRefs(root);

        var state = try this.buildOptimizationState(root);
        defer state.deinit();
        if (state.targets.items.len == 0) return;

        // approximates an average # of passes per relevant value
        var budget: i64 = @as(i64, @intCast(state.targets.items.len)) * 4;

        try this.runStage(root, &state, &budget, .inline_call);
        if (budget <= 0) return;
        try this.runStage(root, &state, &budget, .fold_merge);
        if (budget <= 0) return;
        try this.runStage(root, &state, &budget, .inline_value);
        if (budget <= 0) return;

        this.invariant_facts.deinit(getAllocator());
        this.invariant_facts = try this.computeInvariantCellFacts(root);

        try this.runStage(root, &state, &budget, .dce);
    }

    // basic value graph -> JS emitter
    //
    //   1. A value referenced from more than one place (the target of any
    //      `.ref`, including a self-reference) gets its own `let` binding,
    //      materialized in graph order aka depth-first.
    //   2. A statement-template with no outside reference becomes an IIFE
    //   3. `$N` placeholders become the referenced binding's name if it
    //      has one, or the value rendered inline otherwise.
    //   4. Self-referencing objects/arrays can't be a single literal
    //      expression, they become a sequence of assignments instead
    //
    // ---------------------------------------------------------------

    const EmitCtx = enum { sequence, expression };

    const CodegenState = struct {
        needs_binding: *const std.AutoHashMapUnmanaged(ValueRef, void),
        bindings: std.AutoHashMapUnmanaged(ValueRef, []const u8) = .{},
        // Raw factory function text -> the binding name it was hoisted into
        factory_bindings: std.StringHashMapUnmanaged([]const u8) = .{},
        // why does this exist? need better normalization checks zzz
        repeated_factory_texts: *const std.StringHashMapUnmanaged(void),
        next_name: u32 = 0,

        fn allocName(self: *@This()) ![]const u8 {
            var buf: [16]u8 = undefined;
            const s = try std.fmt.bufPrint(&buf, "_c{d}", .{self.next_name});
            self.next_name += 1;
            return try getAllocator().dupe(u8, s);
        }
    };

    pub const CollapsedCode = struct {
        decls: []const u8,
        final: []const u8,
    };

    pub fn collapseToCode(this: *@This(), root: ValueRef) !CollapsedCode {
        var needs_binding = std.AutoHashMapUnmanaged(ValueRef, void){};
        defer needs_binding.deinit(getAllocator());
        {
            var visited = std.AutoHashMapUnmanaged(ValueRef, void){};
            defer visited.deinit(getAllocator());
            try this.markNeedsBinding(root, &needs_binding, &visited);
        }

        var repeated_factory_texts = std.StringHashMapUnmanaged(void){};
        defer repeated_factory_texts.deinit(getAllocator());
        {
            var text_counts = std.StringHashMapUnmanaged(u32){};
            defer text_counts.deinit(getAllocator());
            var visited = std.AutoHashMapUnmanaged(ValueRef, void){};
            defer visited.deinit(getAllocator());
            try this.collectFactoryTextCounts(root, &text_counts, &visited);
            var it = text_counts.iterator();
            while (it.next()) |entry| {
                if (entry.value_ptr.* > 1) try repeated_factory_texts.put(getAllocator(), entry.key_ptr.*, {});
            }
        }

        var state = CodegenState{ .needs_binding = &needs_binding, .repeated_factory_texts = &repeated_factory_texts };
        defer state.bindings.deinit(getAllocator());
        defer state.factory_bindings.deinit(getAllocator());

        var out = std.ArrayList(u8).init(getAllocator());
        const final = try this.emitValue(root, &state, &out, .sequence);
        return .{ .decls = out.items, .final = final };
    }

    fn collectFactoryTextCounts(
        this: *@This(),
        ref: ValueRef,
        counts: *std.StringHashMapUnmanaged(u32),
        visited: *std.AutoHashMapUnmanaged(ValueRef, void),
    ) anyerror!void {
        if (ref == 0) return;
        const resolved = try this.resolveIdentity(ref);
        if (visited.contains(resolved)) return;
        try visited.put(getAllocator(), resolved, {});

        const n = this.graph.getValue(resolved);
        switch (n.kind) {
            .array, .object => {
                var s = n.slot0;
                while (s != 0) {
                    try this.collectFactoryTextCounts(s, counts, visited);
                    s = this.graph.getValue(s).next;
                }
            },
            .computed => {
                if (try this.getTemplatedSubject(resolved) == null) {
                    const subject_resolved = try this.resolveIdentity(this.graph.getSubject(n));
                    const subject_node = this.graph.getValue(subject_resolved);
                    if (subject_node.kind == .string) {
                        const text = this.graph.getString(subject_node);
                        const gp = try counts.getOrPutValue(getAllocator(), text, 0);
                        gp.value_ptr.* += 1;
                    }
                }
                try this.collectFactoryTextCounts(n.slot0, counts, visited);
                try this.collectFactoryTextCounts(n.slot1, counts, visited);
            },
            else => {},
        }
    }

    fn resolveIdentity(this: *@This(), ref: ValueRef) !ValueRef {
        return this.graph.followReplacements(try this.graph.followAllRefs(ref));
    }

    // use this to know the "result" of a value
    fn canonicalValueIdentity(this: *@This(), resolved: ValueRef) !ValueRef {
        var cur = resolved;
        while (true) {
            const n = this.graph.getValue(cur);
            if (n.kind != .computed) return cur;
            const subj = try this.getTemplatedSubject(cur) orelse return cur;
            if (!std.mem.eql(u8, subj.kind, "statement-template")) return cur;
            const input_ref = try this.resolveIdentity(this.graph.getInput(n));
            const input_node = this.graph.getValue(input_ref);
            if (input_node.kind != .array) return cur;
            const zero = this.graph.getArrayElement(input_node, 0) orelse return cur;
            cur = try this.resolveIdentity(zero);
        }
    }

    fn markNeedsBinding(
        this: *@This(),
        ref: ValueRef,
        needs_binding: *std.AutoHashMapUnmanaged(ValueRef, void),
        visited: *std.AutoHashMapUnmanaged(ValueRef, void),
    ) anyerror!void {
        if (ref == 0) return;

        const raw_node = this.graph.getValue(ref);
        if (raw_node.kind == .ref) {
            const target = try this.canonicalValueIdentity(try this.resolveIdentity(ref));
            switch (this.graph.getValue(target).kind) {
                .true, .false, .null, .undefined, .number => {},
                .string => {
                    // most useful if the string has been deduped
                    if (this.graph.getString(this.graph.getValue(target)).len >= 10) {
                        try needs_binding.put(getAllocator(), target, {});
                    }
                },
                else => try needs_binding.put(getAllocator(), target, {}),
            }
        }

        const resolved = try this.resolveIdentity(ref);
        if (visited.contains(resolved)) return;
        try visited.put(getAllocator(), resolved, {});

        const n = this.graph.getValue(resolved);
        switch (n.kind) {
            .array, .object, .computed => {
                var s = n.slot0;
                while (s != 0) {
                    try this.markNeedsBinding(s, needs_binding, visited);
                    s = this.graph.getValue(s).next;
                }
            },
            else => {},
        }
    }

    fn emitObjectKeyLiteral(this: *@This(), key_ref: ValueRef) ![]const u8 {
        const resolved = try this.resolveIdentity(key_ref);
        const key_node = this.graph.getValue(resolved);
        if (key_node.kind == .string) {
            const key_text = this.graph.getString(key_node);
            if (isIdentifier(key_text)) return key_text;
            return (try this.graph.tryLiteralText(resolved)) orelse "/* key */ null";
        }
        return (try this.graph.tryLiteralText(resolved)) orelse "/* key */ null";
    }

    fn emitObjectKeyAccess(
        this: *@This(),
        key_ref: ValueRef,
        state: *CodegenState,
        out: *std.ArrayList(u8),
    ) anyerror![]const u8 {
        const resolved = try this.resolveIdentity(key_ref);
        const key_node = this.graph.getValue(resolved);
        if (key_node.kind == .string) {
            const key_text = this.graph.getString(key_node);
            if (isIdentifier(key_text)) return try std.fmt.allocPrint(getAllocator(), ".{s}", .{key_text});
            return try std.fmt.allocPrint(getAllocator(), "[{s}]", .{(try this.graph.tryLiteralText(resolved)).?});
        }
        const key_expr = try this.emitValue(key_ref, state, out, .expression);
        return try std.fmt.allocPrint(getAllocator(), "[{s}]", .{key_expr});
    }

    fn tryInlineTrivialArrowCall(
        subject_text: []const u8,
        args_text: []const []const u8,
    ) !?[]const u8 {
        const parsed = try parser.ParsedFile.createFromExpression(subject_text, null);
        defer parsed.deinit();

        const wrapped_top_ref = getWrappedTopExpr(parsed) orelse return null;
        const arrow_ref = wrapped_top_ref;
        const arrow = parsed.ast.nodes.at(arrow_ref);
        if (arrow.kind != .arrow_function) return null;

        const body_ref = getPackedData(arrow).right;
        const body = parsed.ast.nodes.at(body_ref);
        if (body.kind == .block) return null; // not a concise body

        const params_head = getPackedData(arrow).left;
        var param_syms = std.ArrayListUnmanaged(parser.SymbolRef){};
        defer param_syms.deinit(getAllocator());
        {
            var it = NodeIterator.init(&parsed.ast.nodes, params_head);
            while (it.nextRef()) |p_ref| {
                const p = parsed.ast.nodes.at(p_ref);
                const pd = getPackedData(p);
                if (pd.right != 0) return null; // has a default value
                const name_node = parsed.ast.nodes.at(pd.left);
                if (name_node.kind != .identifier) return null; // no destructuring
                const sym = parsed.binder.getSymbol(pd.left) orelse return null;
                if (sym == 0) return null;
                try param_syms.append(getAllocator(), sym);
            }
        }
        if (param_syms.items.len != args_text.len) return null;

        for (param_syms.items) |sym| {
            var counter = SymbolUseCounter{ .nodes = &parsed.ast.nodes, .binder = &parsed.binder, .sym = sym };
            try counter.visit(parsed.ast.nodes.at(body_ref), body_ref);
            if (counter.count != 1) return null; // used zero or 2+ times
        }

        var factory = Factory{ .nodes = &parsed.ast.nodes };
        var placeholders = std.ArrayListUnmanaged(NodeRef){};
        defer placeholders.deinit(getAllocator());
        for (args_text) |arg_text| {
            const replacement_text = if (isIdentifier(arg_text))
                arg_text
            else
                try std.fmt.allocPrint(getAllocator(), "({s})", .{arg_text});
            try placeholders.append(getAllocator(), try factory.createIdentifierAllocated(replacement_text));
        }

        var replacements = std.AutoArrayHashMap(NodeRef, NodeRef).init(getAllocator());
        defer replacements.deinit();
        try collectParamReplacements(&parsed.ast.nodes, &parsed.binder, body_ref, param_syms.items, placeholders.items, &replacements);

        var writer = try parser.Writer.init(subject_text.len);
        var printer = parser.Printer(parser.Writer, .{ .use_replacements = true }).init(parsed.ast, &writer);
        printer.skip_types = true;
        printer.replacements = &replacements;
        try printer.visit(parsed.ast.nodes.at(body_ref));

        return try getAllocator().dupe(u8, writer.buf.items);
    }

    fn emitPlainValue(this: *@This(), ref: ValueRef, state: *CodegenState, out: *std.ArrayList(u8)) anyerror![]const u8 {
        if (state.bindings.get(ref)) |name| return name;

        const n = this.graph.getValue(ref);

        // reserve early
        const reserved: ?[]const u8 = if (state.needs_binding.contains(ref)) blk: {
            const name = try state.allocName();
            try state.bindings.put(getAllocator(), ref, name);
            break :blk name;
        } else null;

        const text: []const u8 = switch (n.kind) {
            .true, .false, .null, .undefined, .number, .string => (try this.graph.tryLiteralText(ref)).?,
            .array => blk: {
                if (try this.graph.referencesSelf(ref)) {
                    const name = reserved.?;
                    try out.writer().print("let {s} = [];\n", .{name});
                    var i: u32 = 0;
                    while (this.graph.getArrayElement(n, i)) |el| : (i += 1) {
                        const el_text = try this.emitValue(el, state, out, .expression);
                        try out.writer().print("{s}[{d}] = {s};\n", .{ name, i, el_text });
                    }
                    return name;
                }
                var parts = std.ArrayList(u8).init(getAllocator());
                try parts.append('[');
                var i: u32 = 0;
                var first = true;
                while (this.graph.getArrayElement(n, i)) |el| : (i += 1) {
                    if (!first) try parts.appendSlice(", ");
                    first = false;
                    try parts.appendSlice(try this.emitValue(el, state, out, .expression));
                }
                try parts.append(']');
                break :blk parts.items;
            },
            .object => blk: {
                if (try this.graph.referencesSelf(ref)) {
                    // const name = reserved.?;
                    const name = reserved orelse "$oh_no";
                    try out.writer().print("let {s} = {{}};\n", .{name});
                    var s = n.slot0;
                    while (s != 0) {
                        const key_node = this.graph.getValue(s);
                        const value_ref = key_node.next;
                        const key_text = try this.emitObjectKeyAccess(s, state, out);
                        const val_text = try this.emitValue(value_ref, state, out, .expression);
                        try out.writer().print("{s}{s} = {s};\n", .{ name, key_text, val_text });
                        s = this.graph.getValue(value_ref).next;
                    }
                    return name;
                }
                var parts = std.ArrayList(u8).init(getAllocator());
                try parts.append('{');
                var s = n.slot0;
                var first = true;
                while (s != 0) {
                    const key_node = this.graph.getValue(s);
                    const value_ref = key_node.next;
                    if (!first) try parts.appendSlice(", ");
                    first = false;
                    try parts.appendSlice(try this.emitObjectKeyLiteral(s));
                    try parts.appendSlice(": ");
                    try parts.appendSlice(try this.emitValue(value_ref, state, out, .expression));
                    s = this.graph.getValue(value_ref).next;
                }
                try parts.append('}');
                break :blk parts.items;
            },
            .computed => blk: {
                if (try this.getTemplatedSubject(ref)) |subj| {
                    if (!std.mem.eql(u8, subj.kind, "expression-template")) return error.UnknownComputedSubject;
                    const input_ref = try this.resolveIdentity(this.graph.getInput(n));
                    const input_node = this.graph.getValue(input_ref);
                    var subst = std.ArrayList([]const u8).init(getAllocator());
                    if (input_node.kind == .array) {
                        var i: u32 = 0;
                        while (this.graph.getArrayElement(input_node, i)) |el| : (i += 1) {
                            try subst.append(try this.emitValue(el, state, out, .expression));
                        }
                    }
                    break :blk try substitutePlaceholders(subj.template, subst.items);
                }

                const subject_resolved = try this.resolveIdentity(this.graph.getSubject(n));
                const subject_node = this.graph.getValue(subject_resolved);

                const input_ref = try this.resolveIdentity(this.graph.getInput(n));
                const input_node = this.graph.getValue(input_ref);
                var args_text = std.ArrayList([]const u8).init(getAllocator());
                defer args_text.deinit();
                if (input_node.kind == .array) {
                    var i: u32 = 0;
                    while (this.graph.getArrayElement(input_node, i)) |el| : (i += 1) {
                        try args_text.append(try this.emitValue(el, state, out, .expression));
                    }
                }

                if (subject_node.kind == .string) {
                    const raw_text = this.graph.getString(subject_node);
                    if (try tryInlineTrivialArrowCall(raw_text, args_text.items)) |reduced| {
                        break :blk reduced;
                    }
                }

                const subject_text: []const u8 = switch (subject_node.kind) {
                    .string => hoist: {
                        const text = this.graph.getString(subject_node);
                        if (state.factory_bindings.get(text)) |name| break :hoist name;
                        if (!state.repeated_factory_texts.contains(text)) break :hoist text;
                        const name = try state.allocName();
                        try out.writer().print("let {s} = {s};\n", .{ name, text });
                        try state.factory_bindings.put(getAllocator(), text, name);
                        break :hoist name;
                    },
                    .computed => try this.emitValue(this.graph.getSubject(n), state, out, .expression),
                    else => return error.UnknownComputedSubject,
                };

                var call = std.ArrayList(u8).init(getAllocator());
                if (isIdentifier(subject_text)) {
                    try call.appendSlice(subject_text);
                } else {
                    try call.append('(');
                    try call.appendSlice(subject_text);
                    try call.append(')');
                }
                try call.append('(');
                for (args_text.items, 0..) |a, i| {
                    if (i != 0) try call.appendSlice(", ");
                    try call.appendSlice(a);
                }
                try call.append(')');
                break :blk call.items;
            },
            else => return error.UnrenderableValue,
        };

        if (reserved) |name| {
            try out.writer().print("let {s} = {s};\n", .{ name, text });
            return name;
        }
        return text;
    }

    fn emitBaseForMutation(this: *@This(), base: ValueRef, state: *CodegenState, out: *std.ArrayList(u8)) anyerror![]const u8 {
        const text = try this.emitPlainValue(base, state, out);
        if (state.bindings.get(base)) |name| return name; // emitPlainValue already bound it
        const name = try state.allocName();
        try out.writer().print("let {s} = {s};\n", .{ name, text });
        try state.bindings.put(getAllocator(), base, name);
        return name;
    }

    fn emitStatementChain(
        this: *@This(),
        chain: []const ValueRef,
        base_name: []const u8,
        state: *CodegenState,
        out: *std.ArrayList(u8),
    ) anyerror!void {
        var idx: usize = chain.len;
        while (idx > 0) {
            idx -= 1;
            const c = chain[idx];
            const subj = try this.getTemplatedSubject(c) orelse return error.UnknownComputedSubject;
            const input_ref = try this.resolveIdentity(this.graph.getInput(this.graph.getValue(c)));
            const input_node = this.graph.getValue(input_ref);

            var subst = std.ArrayList([]const u8).init(getAllocator());
            try subst.append(base_name);
            if (input_node.kind == .array) {
                var i: u32 = 1;
                while (this.graph.getArrayElement(input_node, i)) |el| : (i += 1) {
                    try subst.append(try this.emitValue(el, state, out, .expression));
                }
            }
            const stmt_text = try substitutePlaceholders(subj.template, subst.items);
            try out.writer().print("{s}\n", .{stmt_text});
        }
    }

    fn emitValue(
        this: *@This(),
        ref: ValueRef,
        state: *CodegenState,
        out: *std.ArrayList(u8),
        ctx: EmitCtx,
    ) anyerror![]const u8 {
        if (ref == 0) return "undefined";

        // Unwrap the statement chain
        var chain = std.ArrayListUnmanaged(ValueRef){};
        defer chain.deinit(getAllocator());
        var cur = try this.resolveIdentity(ref);
        while (true) {
            const n = this.graph.getValue(cur);
            if (n.kind != .computed) break;
            const subj = try this.getTemplatedSubject(cur) orelse break;
            if (!std.mem.eql(u8, subj.kind, "statement-template")) break;
            try chain.append(getAllocator(), cur);
            const input_ref = try this.resolveIdentity(this.graph.getInput(n));
            const input_node = this.graph.getValue(input_ref);
            if (input_node.kind != .array) break;
            const zero = this.graph.getArrayElement(input_node, 0) orelse break;
            cur = try this.resolveIdentity(zero);
        }
        const base = cur;

        if (state.bindings.get(base)) |name| return name;

        if (chain.items.len == 0) {
            return this.emitPlainValue(base, state, out);
        }

        const needs_outer = state.needs_binding.contains(base);

        if (ctx == .sequence or needs_outer) {
            const name = try this.emitBaseForMutation(base, state, out);
            try this.emitStatementChain(chain.items, name, state, out);
            return name;
        }

        var local_out = std.ArrayList(u8).init(getAllocator());
        const name = try this.emitBaseForMutation(base, state, &local_out);
        try this.emitStatementChain(chain.items, name, state, &local_out);
        return try std.fmt.allocPrint(getAllocator(), "(() => {{\n  {s}return {s};\n}})()", .{ local_out.items, name });
    }
};

// does not free
pub fn optimizeValueGraph(bytes: []const u8) ![]const u8 {
    // var values = try ValueParser.parse(bytes);
    const values = try getAllocator().create(ValueParser);
    defer getAllocator().destroy(values);
    values.* = try ValueParser.parse(bytes);

    var replacements = std.AutoHashMapUnmanaged(ValueRef, ValueRef){};
    var graph = ValueGraph{ .values = values, .replacements = &replacements };
    var opt = Optimizer{ .values = values, .graph = &graph };

    // try graph.printGraph();

    try opt.optimizeAll(values.root);

    const code = try opt.collapseToCode(graph.followReplacements(values.root));
    var out = std.ArrayList(u8).init(getAllocator());
    try out.appendSlice(code.decls);
    try out.append('\n');
    try out.appendSlice("const _ = ");
    try out.appendSlice(code.final);
    return out.items;
}



pub fn optimizeVson(source: []const u8, emit_vson: bool) ![]const u8 {
    const alloc = getAllocator();

    var nodes = BumpAllocator(ValueNode).init(alloc, std.heap.page_allocator);
    try nodes.preAlloc();
    _ = try nodes.push(.{ .kind = .NUL });

    var emitter = value_syntax.ValueEmitter.init(alloc, &nodes);
    var p = try value_syntax.Parser(*value_syntax.ValueEmitter).init(&emitter, .{ .contents = source }, alloc);
    try p.parse();
    if (emitter.had_error) return error.VsonParseError;
    const root = try emitter.finish();

    const values = try alloc.create(ValueParser);
    defer alloc.destroy(values);
    values.* = ValueParser{ .bytes = &.{}, .nodes = nodes };
    values.root = root;

    var replacements = std.AutoHashMapUnmanaged(ValueRef, ValueRef){};
    var graph = ValueGraph{ .values = values, .replacements = &replacements };
    var opt = Optimizer{ .values = values, .graph = &graph };

    try opt.optimizeAll(values.root);

    if (emit_vson) {
        return graph.printGraphToString();
    }

    const code = try opt.collapseToCode(graph.followReplacements(values.root));
    var out = std.ArrayList(u8).init(alloc);
    try out.appendSlice(code.decls);
    try out.append('\n');
    try out.appendSlice("const _ = ");
    try out.appendSlice(code.final);
    return out.items;
}

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
//   input: [_5, _11]
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
//   input: [_5, _11, _50]
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
