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
            .slot0 = @truncate(@intFromPtr(slice.ptr) >> 32),
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

    pub fn getNumberFromNode(n: *const ValueNode, comptime T: type) T {
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
    replacements: *std.AutoHashMapUnmanaged(ValueRef, ValueRef),
    counts: std.AutoHashMapUnmanaged(ValueRef, u32) = .{},

    fn adjustRefCount(this: *@This(), ref: ValueRef, amt: i32) !bool {
        if (ref == 0) return false;
        std.debug.assert(amt != 0);
        const entry = try this.counts.getOrPut(getAllocator(), ref);
        if (!entry.found_existing) {
            entry.value_ptr.* = 0;
        }
        const v = entry.value_ptr.*;
        if (amt < 0 and -amt > v) {
            return error.RefCountUnderflow;
        }
        entry.value_ptr.* = v + amt;
        return if (amt < 0) (v + amt) == 0 else v == 0;
    }

    fn walkForRefCounts(
        this: *@This(),
        ref: ValueRef,
        amt: i32,
        visited: *std.AutoHashMapUnmanaged(ValueRef, void),
    ) anyerror!void {
        if (ref == 0) return;
        const n = this.getValue(ref);
        switch (n.kind) {
            .string, .number, .null, .undefined, .true, .false => return,
            else => {},
        }

        if (visited.contains(ref)) return;
        try visited.put(getAllocator(), ref, {});
        switch (n.kind) {
            .computed => {
                if (this.adjustRefCount(n.slot0, amt))
                    try this.walkForRefCounts(n.slot0, amt, visited);
                if (this.adjustRefCount(n.slot1, amt))
                    try this.walkForRefCounts(n.slot1, amt, visited);
            },
            .array, .object => {
                var s = n.slot0;
                while (s != 0) {
                    if (this.adjustRefCount(s, amt))
                        try this.walkForRefCounts(s, amt, visited);
                    s = this.getValue(s).next;
                }
            },
            .ref => {
                const target = try this.followRefNode(n);
                if (this.adjustRefCount(target, amt))
                    try this.walkForRefCounts(target, amt, visited);
            },
            else => {},
        }
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
            .true, .false, .null, .undefined, .number, .string => return try this.tryLiteralText(resolved),
            .array => {
                var out = std.ArrayList(u8).init(getAllocator());
                try out.append('[');
                var i: u32 = 0;
                var first = true;
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

    pub fn replaceValue(this: *@This(), a: ValueRef, b: ValueRef) !void {
        const f = this.followReplacements(a);
        try this.replacements.put(getAllocator(), f, b);
        if (f != a) {
            try this.replacements.put(getAllocator(), a, b);
        }
        const bn = this.values.nodes.at(b);
        if (bn.next == 0) {
            bn.next = this.values.nodes.at(f).next;
        }
    }

    pub fn replaceValueNoNext(this: *@This(), a: ValueRef, b: ValueRef) !void {
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
            .slot0 = @truncate(@intFromPtr(v.ptr) >> 32),
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

    // Should only be used on synthetic values, for now. Will add a new key/value pair if-needed.
    // key: ValueRef | []const u8 (string key)
    pub fn setProperty(this: *@This(), ref: ValueRef, key: anytype, value: ValueRef) !void {
        const n = this.getValue(ref);
        std.debug.assert(n.kind == .object);

        const key_ref: ValueRef = if (@TypeOf(key) == ValueRef) key else try this.createString(key);

        var s = n.slot0;
        var last: ValueRef = 0;
        while (s != 0) {
            const key_node = this.getValue(s);
            const value_ref = key_node.next;
            if (try this.valuesEql(s, key_ref)) {
                // Replace in place, preserving the chain's continuation.
                const continuation = this.getValue(value_ref).next;
                this.values.nodes.at(value).next = @truncate(continuation);
                this.values.nodes.at(s).next = @truncate(value);
                return;
            }
            last = value_ref;
            s = this.getValue(value_ref).next;
        }

        // Not found: append a new key/value pair at the tail.
        this.values.nodes.at(key_ref).next = @truncate(value);
        this.values.nodes.at(value).next = 0;
        if (last == 0) {
            this.values.nodes.at(this.followReplacements(ref)).slot0 = @truncate(key_ref);
        } else {
            this.values.nodes.at(last).next = @truncate(key_ref);
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
                c = kn;
                this.values.nodes.at(obj_ref).slot0 = @truncate(kn);
            } else {
                this.values.nodes.at(c).next = @truncate(kn);
                c = kn;
            }
        }
        if (c != 0) this.values.nodes.at(c).next = @truncate(l) else this.values.nodes.at(obj_ref).slot0 = @truncate(l);
        return true;
    }

    // does not include itself, does not follow ref nodes
    pub fn hasComputedNode(this: *@This(), ref: ValueRef) bool {
        if (ref == 0) return false;
        const v = this.getValue(ref);
        switch (v.kind) {
            .computed => {
                return this.hasComputed(v.slot0) or this.hasComputed(v.slot1);
            },
            .array, .object => {
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
            .computed => {
                if (visited.contains(ref)) return false;
                try visited.put(ref, {});
                return try this._referencesSelf(root, v.slot0, visited) or try this._referencesSelf(root, v.slot1, visited);
            },
            .ref => {
                if (visited.contains(ref)) return false;
                try visited.put(ref, {});
                return try this._referencesSelf(root, try this.followAllRefs(ref), visited);
            },
            .array, .object => {
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
        var path_info = try this.buildPathInfo(this.values.root);
        defer path_info.deinit(getAllocator());

        var out = std.ArrayList(u8).init(getAllocator());
        defer out.deinit();
        var path = std.AutoHashMapUnmanaged(ValueRef, void){};
        defer path.deinit(getAllocator());
        try this.printGraphValue(this.values.root, &path_info, &out, &path, 0);
        debugPrint("{s}\n", .{out.items});
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

            if (path_info.contains(n.slot0)) {
                try this.formatRelativePath(path_info, ref, n.slot0, out);
                return;
            }
            const r = this.followReplacements(n.slot0);
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
            try this.printGraphValue(n.slot0, path_info, out, path, indent_depth);
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
            .computed => {
                try out.append('(');
                try this.printGraphValue(n.slot0, path_info, out, path, indent_depth);
                if (n.slot1 != 0) {
                    try out.appendSlice(", ");
                    try this.printGraphValue(n.slot1, path_info, out, path, indent_depth);
                }
                try out.append(')');
            },
            .ref => unreachable, // handled above
        }
    }

    // Reduces references pointing to values that would otherwise not be directly reachable by the graph
    // The first reference that reaches an orphaned value adopts it, every other reference points to the new position. 
    pub fn normalizeRefs(this: *@This(), root: ValueRef) !void {
        var direct = std.AutoHashMapUnmanaged(ValueRef, void){};
        defer direct.deinit(getAllocator());
        try this.markDirectReachable(root, &direct);

        var visited = std.AutoHashMapUnmanaged(ValueRef, void){};
        defer visited.deinit(getAllocator());
        try this.normalizeWalk(root, &direct, &visited);

        // TODO: we can use this if we had a better place for the current root to be

        visited.clearRetainingCapacity();
        const r = try this.simplifyReplacements(root, &visited);
        this.replacements.clearAndFree(getAllocator());

        // simpler, albeit ugly, to just assume you can use the same root ref for the "current root" 
        if (r != root) try this.replacements.put(getAllocator(), root, r);
    }

    fn markDirectReachable(this: *@This(), ref: ValueRef, direct: *std.AutoHashMapUnmanaged(ValueRef, void)) anyerror!void {
        if (ref == 0) return;
        const resolved = this.followReplacements(ref);
        if (direct.contains(resolved)) return;
        try direct.put(getAllocator(), resolved, {});

        const n = this.values.nodes.at(resolved);
        switch (n.kind) {
            .ref => {},
            .array, .object => {
                var s = n.slot0;
                while (s != 0) {
                    try this.markDirectReachable(s, direct);
                    s = this.getValue(s).next;
                }
            },
            .computed => {
                try this.markDirectReachable(n.slot0, direct);
                try this.markDirectReachable(n.slot1, direct);
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

        const n = this.values.nodes.at(resolved);
        switch (n.kind) {
            .ref => {
                const target = n.slot0;
                const target_resolved = this.followReplacements(target);
                if (direct.contains(target_resolved)) return;
                try direct.put(getAllocator(), target_resolved, {});

                const clone_ref = try this.cloneValue(target_resolved);
                try this.replaceValue(ref, clone_ref);
                try this.replacements.put(getAllocator(), target, clone_ref);
                if (target != target_resolved)
                    try this.replacements.put(getAllocator(), target_resolved, clone_ref);

                try this.markDirectReachable(clone_ref, direct);
                try this.normalizeWalk(clone_ref, direct, visited);
            },
            .array, .object => {
                var s = n.slot0;
                while (s != 0) {
                    try this.normalizeWalk(s, direct, visited);
                    s = this.getValue(s).next;
                }
            },
            .computed => {
                try this.normalizeWalk(n.slot0, direct, visited);
                try this.normalizeWalk(n.slot1, direct, visited);
            },
            else => {},
        }
    }

    fn simplifyReplacements(
        this: *@This(),
        ref: ValueRef,
        visited: *std.AutoHashMapUnmanaged(ValueRef, void),
    ) anyerror!ValueRef {
        if (ref == 0) return ref;
        const resolved = this.followReplacements(ref);
        const n = this.values.nodes.at(resolved);
        switch (n.kind) {
            .true, .false, .undefined, .null, .number, .string => return resolved,
            else => {},
        }

        if (visited.contains(resolved)) return resolved;
        try visited.put(getAllocator(), resolved, {});

        switch (n.kind) {
            .ref => {
                n.slot0 = try this.simplifyReplacements(n.slot0, visited);
            },
            .array, .object => {
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
            .computed => {
                n.slot0 = try this.simplifyReplacements(n.slot0, visited);
                n.slot1 = try this.simplifyReplacements(n.slot1, visited);
            },
            else => {},
        }

        return resolved;
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

const ReferenceCollector = struct {
    file: *parser.ParsedFile,
    parents: std.AutoHashMapUnmanaged(parser.NodeRef, parser.NodeRef) = .{},
    parents_watermark: usize = 1,
    // excludes the decl reference
    references: std.AutoHashMapUnmanaged(parser.SymbolRef, std.ArrayListUnmanaged(parser.NodeRef)) = .{},
    stack: std.ArrayListUnmanaged(parser.NodeRef) = .{},

    pub fn init(file: *parser.ParsedFile) !@This() {
        var self = @This(){ .file = file };
        try self.visit(file.ast.nodes.at(file.ast.start), file.ast.start);
        return self;
    }

    pub fn deinit(self: @This()) void {
        self.parents.deinit(getAllocator());
        self.stack.deinit(getAllocator());
        var iter = self.references.valueIterator();
        while (iter.next()) |arr| {
            arr.deinit(getAllocator());
        }
        self.references.deinit(getAllocator());
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

    pub fn visit(self: *@This(), node: *const AstNode, ref: NodeRef) anyerror!void {
        if (node.kind == .identifier) {
            const sym_ref = self.file.binder.getSymbol(ref) orelse return;
            const sym = self.file.binder.symbols.at(sym_ref);
            if (!sym.isStrictlyLocal()) return;
            if (sym.binding == ref) return;
            if (sym.declaration != 0) {
                if (parser.getDeclarationNameRef(self.file.ast.nodes.at(sym.declaration)) == ref) return;
            }
            try self.markParents();
            try self.parents.put(getAllocator(), ref, self.stack.getLast());
            try self.addReference(sym_ref, ref);
            return;
        }
        if (parser.isLeafNode(node.kind)) return;
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

        pub fn getAssignedValue(this: *const @This(), ref: NodeRef) ?NodeRef {
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
                            if (getPackedData(p).left != current_ref) break;
                            return getPackedData(p).right;
                        }
                        break;
                    },
                    // TODO: destructuring can rebind params!!!
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
                    .await_expression, .shorthand_property_assignment, .parenthesized_expression => {
                        current_ref = parent_ref;
                        parent_ref = this.parentRef(current_ref) orelse break;
                    },
                    .array_literal_expression, .object_literal_expression => return parent_ref,
                    .property_assignment => {
                        if (getPackedData(p).right != current_ref) break;
                        current_ref = parent_ref;
                        parent_ref = this.parentRef(current_ref) orelse break;
                    },
                    .call_expression, .new_expression => {
                        const target = getPackedData(p).left;
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
            if (iter.getAssignedValue(r) != null) return true;
        }
        return false;
    }
};

const DeadCodeEliminator = struct {
    file: *parser.ParsedFile,
    collector: ReferenceCollector,
    graph: *ValueGraph,
    known_values: std.AutoHashMapUnmanaged(parser.SymbolRef, std.ArrayListUnmanaged(ValueRef)) = .{},

    pub fn init(file: *parser.ParsedFile) !@This() {
        const collector = try ReferenceCollector.init(file);
        const self = @This(){ .file = file, .collector = collector };
        return self;
    }

    pub fn deinit(self: @This()) void {
        self.collector.deinit();
        var iter = self.known_values.valueIterator();
        while (iter.next()) |x| {
            x.deinit(getAllocator());
        }
        self.known_values.deinit(getAllocator());
    }

    pub fn addKnownValue(self: *@This(), sym_ref: parser.SymbolRef, val: ValueRef) !void {
        const entry = try self.known_values.getOrPut(getAllocator(), sym_ref);
        if (!entry.found_existing) {
            entry.found_existing.* = .{};
        }
        for (entry.value_ptr.items) |x| {
            if (try self.graph.valuesEql(x, val)) return;
        }
        try entry.value_ptr.append(getAllocator(), val);
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
    const inner_ref = maybeUnwrapRef(stmt) orelse return null;
    const inner = parsed.ast.nodes.at(inner_ref);
    if (inner.kind != .parenthesized_expression) return null;
    const fn_ref = maybeUnwrapRef(inner) orelse return null;
    const fn_node = parsed.ast.nodes.at(fn_ref);
    if (fn_node.kind != .function_expression and fn_node.kind != .function_declaration) return null;
    return fn_ref;
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

const KnownValue = union(enum) {
    boolean: bool,
    number: f64,
    null_,
    undefined_,

    fn toBool(self: KnownValue) bool {
        return switch (self) {
            .boolean => |b| b,
            .number => |n| n != 0,
            .null_, .undefined_ => false,
        };
    }
};

fn evalLiteral(nodes: *const BumpAllocator(AstNode), ref: NodeRef) ?KnownValue {
    const n = nodes.at(ref);
    return switch (n.kind) {
        .true_keyword => .{ .boolean = true },
        .false_keyword => .{ .boolean = false },
        .null_keyword => .null_,
        .undefined_keyword => .undefined_,
        .numeric_literal => .{ .number = parser.getNumber(n) },
        else => null,
    };
}

const TrackedParam = struct {
    sym: parser.SymbolRef,
    value: ?KnownValue, // null = no longer statically known
};

fn evalCondition(
    nodes: *const BumpAllocator(AstNode),
    binder: *const parser.Binder,
    expr_ref: NodeRef,
    tracked: []const TrackedParam,
) ?bool {
    const n = nodes.at(expr_ref);
    switch (n.kind) {
        .true_keyword => return true,
        .false_keyword => return false,
        .identifier => {
            const sym = binder.getSymbol(expr_ref) orelse return null;
            if (sym == 0) return null;
            for (tracked) |t| {
                if (t.sym != sym) continue;
                const v = t.value orelse return null;
                return v.toBool();
            }
            return null;
        },
        .prefix_unary_expression => {
            const d = getPackedData(n);
            const op: SyntaxKind = @enumFromInt(d.left);
            if (op != .exclamation_token) return null;
            const inner = evalCondition(nodes, binder, d.right, tracked) orelse return null;
            return !inner;
        },
        .parenthesized_expression => {
            const inner_ref = maybeUnwrapRef(n) orelse return null;
            return evalCondition(nodes, binder, inner_ref, tracked);
        },
        else => return null,
    }
}

fn tryUpdateTrackedFromAssignment(
    nodes: *const BumpAllocator(AstNode),
    binder: *const parser.Binder,
    stmt: *const AstNode,
    tracked: []TrackedParam,
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
        t.value = evalLiteral(nodes, d.right);
        return;
    }
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

fn isSymbolUsedInStatements(
    nodes: *const BumpAllocator(AstNode),
    binder: *const parser.Binder,
    stmts: []const NodeRef,
    sym: parser.SymbolRef,
) bool {
    for (stmts) |s| {
        var checker = SymbolUseChecker{ .nodes = nodes, .binder = binder, .sym = sym };
        checker.visit(nodes.at(s), s) catch return true;
        if (checker.found) return true;
    }
    return false;
}

fn dceProcessStatement(
    nodes: *const BumpAllocator(AstNode),
    binder: *const parser.Binder,
    stmt_ref: NodeRef,
    tracked: []TrackedParam,
    out: *std.ArrayList(NodeRef),
) anyerror!bool {
    const stmt = nodes.at(stmt_ref);
    if (stmt.kind == .if_statement) {
        const d = getPackedData(stmt);
        const cond_ref = d.left;
        const then_ref = d.right;
        const else_ref = stmt.len;
        if (evalCondition(nodes, binder, cond_ref, tracked)) |taken| {
            const branch = if (taken) then_ref else else_ref;
            if (branch != 0) {
                const branch_node = nodes.at(branch);
                if (branch_node.kind == .block) {
                    const inner_head = maybeUnwrapRef(branch_node) orelse 0;
                    _ = try dceWalkStatements(nodes, binder, inner_head, tracked, out);
                } else {
                    _ = try dceProcessStatement(nodes, binder, branch, tracked, out);
                }
            }
            return true;
        }
        for (tracked) |*t| t.value = null;
        try out.append(stmt_ref);
        return false;
    }

    tryUpdateTrackedFromAssignment(nodes, binder, stmt, tracked);
    try out.append(stmt_ref);
    return false;
}

fn dceWalkStatements(
    nodes: *const BumpAllocator(AstNode),
    binder: *const parser.Binder,
    stmts_head: NodeRef,
    tracked: []TrackedParam,
    out: *std.ArrayList(NodeRef),
) anyerror!bool {
    var changed = false;
    var it = NodeIterator.init(nodes, stmts_head);
    while (it.nextRef()) |stmt_ref| {
        if (try dceProcessStatement(nodes, binder, stmt_ref, tracked, out)) changed = true;
    }
    return changed;
}

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
        const resolved = try this.graph.followAllRefs(ref);
        return counts.get(resolved) orelse 0;
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

        if ((try this.refCountOf(counts, subject_ref)) > 1) return false;

        const input_ref = try this.graph.followAllRefs(this.graph.getInput(node));
        const input_node = this.graph.getValue(input_ref);
        if (input_node.kind != .array) return false;

        const subject_text = this.graph.getString(subject_node);

        // TODO: get rid of all this stringy code
        if (containsDollarDigit(subject_text)) return false;

        const wrapped = try std.fmt.allocPrint(getAllocator(), "({s})", .{subject_text});
        const parsed = try parser.ParsedFile.createFromBuffer(wrapped, null, false, null);
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
            literal_texts[i] = try this.graph.tryLiteralText(input_items.items[i]) orelse return false;
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
        try this.graph.replaceValue(value2_ref, merged_computed);
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

    pub fn tryDeadCodeElimination(
        this: *@This(),
        computed_ref: ValueRef,
        counts: *const std.AutoHashMapUnmanaged(ValueRef, u32),
    ) !bool {
        _ = counts;

        const node = this.graph.getValue(computed_ref);
        if (node.kind != .computed) return false;
        const subj = try this.getTemplatedSubject(computed_ref) orelse return false;

        if (!std.mem.eql(u8, subj.kind, "expression-template")) return false;

        const template_text = subj.template;

        const wrapped = try std.fmt.allocPrint(getAllocator(), "({s})", .{template_text});
        const parsed = try parser.ParsedFile.createFromBuffer(wrapped, null, false, null);
        defer parsed.deinit();

        const iife = getIifeCallExpr(parsed) orelse return false;
        const arrow = parsed.ast.nodes.at(iife.arrow_ref);
        const params_head = getPackedData(arrow).left;
        const body_ref = getPackedData(arrow).right;

        var tracked = std.ArrayListUnmanaged(TrackedParam){};
        defer tracked.deinit(getAllocator());
        {
            var params_it = NodeIterator.init(&parsed.ast.nodes, params_head);
            var args_it = NodeIterator.init(&parsed.ast.nodes, iife.args_head);
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
        if (tracked.items.len == 0) return false;

        const body_node = parsed.ast.nodes.at(body_ref);
        const Shape = enum { direct, nested_arrow };
        var target_block_ref: NodeRef = 0;
        var shape: Shape = .direct;
        if (body_node.kind == .block) {
            target_block_ref = body_ref;
            shape = .direct;
        } else if (body_node.kind == .arrow_function) {
            const inner_body_ref = getPackedData(body_node).right;
            if (parsed.ast.nodes.at(inner_body_ref).kind == .block) {
                target_block_ref = inner_body_ref;
                shape = .nested_arrow;
            }
        }
        if (target_block_ref == 0) return false;

        const stmts_head = maybeUnwrapRef(parsed.ast.nodes.at(target_block_ref)) orelse 0;
        var new_stmts = std.ArrayList(NodeRef).init(getAllocator());
        defer new_stmts.deinit();
        const changed = try dceWalkStatements(&parsed.ast.nodes, &parsed.binder, stmts_head, tracked.items, &new_stmts);

        var still_used = try getAllocator().alloc(bool, tracked.items.len);
        defer getAllocator().free(still_used);
        var any_unused = false;
        for (tracked.items, 0..) |t, i| {
            still_used[i] = isSymbolUsedInStatements(&parsed.ast.nodes, &parsed.binder, new_stmts.items, t.sym);
            if (!still_used[i]) any_unused = true;
        }
        if (!changed and !any_unused) return false;

        var factory = Factory{ .nodes = &parsed.ast.nodes };
        const new_block = try factory.createBlock(new_stmts.items);

        var new_params = std.ArrayList(NodeRef).init(getAllocator());
        defer new_params.deinit();
        var new_args = std.ArrayList(NodeRef).init(getAllocator());
        defer new_args.deinit();
        {
            var params_it = NodeIterator.init(&parsed.ast.nodes, params_head);
            var args_it = NodeIterator.init(&parsed.ast.nodes, iife.args_head);
            while (params_it.nextRef()) |p_ref| {
                const arg_ref = args_it.nextRef() orelse break;
                const p = parsed.ast.nodes.at(p_ref);
                const name_ref = getPackedData(p).left;
                var drop = false;
                if (parsed.binder.getSymbol(name_ref)) |sym| {
                    for (tracked.items, 0..) |t, i| {
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

        const final_expr: NodeRef = switch (shape) {
            .direct => blk: {
                if (new_params.items.len == 0) {
                    break :blk try factory.createArrowFunction(0, new_block, 0);
                }
                const new_params_list = try factory.createList(new_params.items);
                const new_arrow = try factory.createArrowFunction(new_params_list, new_block, 0);
                const paren_arrow = try factory.createParenthesizedExpression(new_arrow);
                break :blk try factory.createCallExpression(paren_arrow, new_args.items);
            },
            .nested_arrow => blk: {
                const orig_inner_arrow = parsed.ast.nodes.at(body_ref);
                const inner_params_head = getPackedData(orig_inner_arrow).left;
                const new_inner_arrow = try factory.createArrowFunction(inner_params_head, new_block, orig_inner_arrow.flags);

                if (new_params.items.len == 0) {
                    break :blk new_inner_arrow;
                }
                const new_params_list = try factory.createList(new_params.items);
                const new_outer_arrow = try factory.createArrowFunction(new_params_list, new_inner_arrow, 0);
                const paren_outer = try factory.createParenthesizedExpression(new_outer_arrow);
                break :blk try factory.createCallExpression(paren_outer, new_args.items);
            },
        };

        var writer = try parser.Writer.init(template_text.len);
        var printer = parser.Printer(parser.Writer, .{}).init(parsed.ast, &writer);
        printer.skip_types = true;
        try printer.visit(parsed.ast.nodes.at(final_expr));
        const new_template_text = try getAllocator().dupe(u8, writer.buf.items);

        const new_subject = try this.createTemplatedSubject("expression-template", new_template_text);

        const new_computed_node = try this.graph.createComputed(new_subject, try this.graph.createRef(node.slot1));
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
                try this.walkForOptState(n.slot0, ref, state, visited);
                try this.walkForOptState(n.slot1, ref, state, visited);
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
        try this.graph.normalizeRefs(root);
        try this.runStage(root, &state, &budget, .fold_merge);
        if (budget <= 0) return;
        try this.graph.normalizeRefs(root);
        try this.runStage(root, &state, &budget, .inline_value);
        if (budget <= 0) return;
        try this.graph.normalizeRefs(root);
        try this.runStage(root, &state, &budget, .dce);

        // finalize
        try this.graph.normalizeRefs(root);
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

        var state = CodegenState{ .needs_binding = &needs_binding };
        defer state.bindings.deinit(getAllocator());

        var out = std.ArrayList(u8).init(getAllocator());
        const final = try this.emitValue(root, &state, &out, .sequence);
        return .{ .decls = out.items, .final = final };
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
            .array, .object => {
                var s = n.slot0;
                while (s != 0) {
                    try this.markNeedsBinding(s, needs_binding, visited);
                    s = this.graph.getValue(s).next;
                }
            },
            .computed => {
                try this.markNeedsBinding(n.slot0, needs_binding, visited);
                try this.markNeedsBinding(n.slot1, needs_binding, visited);
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
                    const name = reserved.?;
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
                if (subject_node.kind != .string) return error.UnknownComputedSubject;
                const subject_text = this.graph.getString(subject_node);

                const input_ref = try this.resolveIdentity(this.graph.getInput(n));
                const input_node = this.graph.getValue(input_ref);
                var args = std.ArrayList(u8).init(getAllocator());
                try args.append('(');
                var first = true;
                if (input_node.kind == .array) {
                    var i: u32 = 0;
                    while (this.graph.getArrayElement(input_node, i)) |el| : (i += 1) {
                        if (!first) try args.appendSlice(", ");
                        first = false;
                        try args.appendSlice(try this.emitValue(el, state, out, .expression));
                    }
                }
                try args.append(')');
                break :blk try std.fmt.allocPrint(getAllocator(), "({s}){s}", .{ subject_text, args.items });
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

pub fn debugTestComputationInlining() !void {
    var nodes = BumpAllocator(ValueNode).init(getAllocator(), std.heap.page_allocator);
    try nodes.preAlloc();
    _ = try nodes.push(.{ .kind = .NUL }); // reserve index 0 as "null"

    var values = ValueParser{ .bytes = &.{}, .nodes = nodes };
    var replacements = std.AutoHashMapUnmanaged(ValueRef, ValueRef){};
    var graph = ValueGraph{ .values = &values, .replacements = &replacements };
    var opt = Optimizer{ .values = &values, .graph = &graph };

    const subject_text =
        \\function(_c_c, el) {
        \\  return () => {
        \\    _c_c[0] += 1;
        \\    el[Symbol.update]();
        \\  }
        \\}
    ;
    const subject_str = try graph.createString(subject_text);
    const num1 = try graph.createNumber(1);
    const captured_array = try graph.createArrayFromItems(&.{num1});
    const dom_placeholder = try graph.createString("<dom node placeholder>");

    const input_arr = try graph.createArrayFromItems(&.{ captured_array, dom_placeholder });
    const computed = try graph.createComputed(subject_str, input_arr);

    values.root = computed;

    var counts = try opt.countReferences(values.root);
    defer counts.deinit(getAllocator());

    const applied = try opt.tryInlineComputationCall(computed, &counts);
    debugPrint("computation inlining applied: {}\n", .{applied});

    const result = graph.getValue(computed);
    if (result.kind != .computed) return;
    const subj = graph.getValue(try graph.followAllRefs(graph.getSubject(result)));
    if (subj.kind != .object) return;
    if (try graph.getStringKeyPropertyValue(subj, "kind")) |k| {
        debugPrint("kind: {s}\n", .{graph.getString(graph.getValue(try graph.followAllRefs(k)))});
    }
    if (try graph.getStringKeyPropertyValue(subj, "template")) |t| {
        debugPrint("template: {s}\n", .{graph.getString(graph.getValue(try graph.followAllRefs(t)))});
    }
}

pub fn debugTestComputationInliningMutable() !void {
    var nodes = BumpAllocator(ValueNode).init(getAllocator(), std.heap.page_allocator);
    try nodes.preAlloc();
    _ = try nodes.push(.{ .kind = .NUL });

    var values = ValueParser{ .bytes = &.{}, .nodes = nodes };
    var replacements = std.AutoHashMapUnmanaged(ValueRef, ValueRef){};
    var graph = ValueGraph{ .values = &values, .replacements = &replacements };
    var opt = Optimizer{ .values = &values, .graph = &graph };

    const subject_text =
        \\function(flag, el) {
        \\  return () => {
        \\    if (!flag) {
        \\      flag = true;
        \\    }
        \\    el[Symbol.update]();
        \\  }
        \\}
    ;
    const subject_str = try graph.createString(subject_text);
    const flag_true = try graph.createBoolean(true);
    const dom_placeholder = try graph.createString("<dom node placeholder>");

    const input_arr = try graph.createArrayFromItems(&.{ flag_true, dom_placeholder });
    const computed = try graph.createComputed(subject_str, input_arr);

    values.root = computed;

    var counts = try opt.countReferences(values.root);
    defer counts.deinit(getAllocator());

    const applied = try opt.tryInlineComputationCall(computed, &counts);
    debugPrint("computation inlining (mutable) applied: {}\n", .{applied});

    const result = graph.getValue(computed);
    if (result.kind != .computed) return;
    const subj = graph.getValue(try graph.followAllRefs(graph.getSubject(result)));
    if (subj.kind != .object) return;
    if (try graph.getStringKeyPropertyValue(subj, "template")) |t| {
        debugPrint("template: {s}\n", .{graph.getString(graph.getValue(try graph.followAllRefs(t)))});
    }
    const new_input_ref = try graph.followAllRefs(graph.getInput(result));
    const new_input_node = graph.getValue(new_input_ref);
    var i: u32 = 0;
    while (graph.getArrayElement(new_input_node, i)) |_| : (i += 1) {}
    debugPrint("new input length: {}\n", .{i});
}

pub fn debugTestDeadBranchElimination() !void {
    var nodes = BumpAllocator(ValueNode).init(getAllocator(), std.heap.page_allocator);
    try nodes.preAlloc();
    _ = try nodes.push(.{ .kind = .NUL });

    var values = ValueParser{ .bytes = &.{}, .nodes = nodes };
    var replacements = std.AutoHashMapUnmanaged(ValueRef, ValueRef){};
    var graph = ValueGraph{ .values = &values, .replacements = &replacements };
    var opt = Optimizer{ .values = &values, .graph = &graph };

    const subject_text =
        \\function(flag, el) {
        \\  return () => {
        \\    if (!flag) {
        \\      flag = true;
        \\    }
        \\    el[Symbol.update]();
        \\  }
        \\}
    ;
    const subject_str = try graph.createString(subject_text);
    const flag_true = try graph.createBoolean(true);
    const dom_placeholder = try graph.createString("<dom node placeholder>");

    const input_arr = try graph.createArrayFromItems(&.{ flag_true, dom_placeholder });
    const computed = try graph.createComputed(subject_str, input_arr);

    values.root = computed;

    var counts = try opt.countReferences(values.root);
    defer counts.deinit(getAllocator());

    const inlined = try opt.tryInlineComputationCall(computed, &counts);
    debugPrint("dbe: inline call applied: {}\n", .{inlined});

    counts.deinit(getAllocator());
    counts = try opt.countReferences(values.root);

    const dbe_applied = try opt.tryDeadCodeElimination(computed, &counts);
    debugPrint("dbe: dead branch elimination applied: {}\n", .{dbe_applied});

    const result = graph.getValue(computed);
    if (result.kind != .computed) return;
    const subj = graph.getValue(try graph.followAllRefs(graph.getSubject(result)));
    if (subj.kind != .object) return;
    if (try graph.getStringKeyPropertyValue(subj, "template")) |t| {
        debugPrint("dbe: template: {s}\n", .{graph.getString(graph.getValue(try graph.followAllRefs(t)))});
    }

    debugPrint("dbe: graph:\n", .{});
    try graph.printGraph();
}

pub fn debugTestStatementFolding() !void {
    var nodes = BumpAllocator(ValueNode).init(getAllocator(), std.heap.page_allocator);
    try nodes.preAlloc();
    _ = try nodes.push(.{ .kind = .NUL });

    var values = ValueParser{ .bytes = &.{}, .nodes = nodes };
    var replacements = std.AutoHashMapUnmanaged(ValueRef, ValueRef){};
    var graph = ValueGraph{ .values = &values, .replacements = &replacements };
    var opt = Optimizer{ .values = &values, .graph = &graph };

    const subject_text =
        \\function(_c_c, el) {
        \\  return () => {
        \\    _c_c[0] += 1;
        \\    el[Symbol.update]();
        \\  }
        \\}
    ;
    const subject_str = try graph.createString(subject_text);
    const num1 = try graph.createNumber(1);
    const captured_array = try graph.createArrayFromItems(&.{num1});

    const obj_ref = try graph.createObject(0);
    const obj_self_ref = try graph.createRef(obj_ref);

    const entry_input = try graph.createArrayFromItems(&.{ captured_array, obj_self_ref });
    const entry_value = try graph.createComputed(subject_str, entry_input);

    const key_str = try graph.createString("Symbol.update");
    values.nodes.at(key_str).next = @truncate(entry_value);
    values.nodes.at(entry_value).next = 0;
    values.nodes.at(obj_ref).slot0 = key_str;

    values.root = obj_ref;

    {
        var pre_counts = try opt.countReferences(values.root);
        defer pre_counts.deinit(getAllocator());
        _ = try opt.tryInlineComputationCall(entry_value, &pre_counts);
    }

    var counts = try opt.countReferences(values.root);
    defer counts.deinit(getAllocator());

    const applied = try opt.tryPeelComputedObjectEntryIntoStatement(obj_ref, &counts);
    debugPrint("statement folding applied: {}\n", .{applied});

    const value_node = graph.getValue(try graph.followAllRefs(obj_ref));
    if (value_node.kind != .computed) return;
    const subj = graph.getValue(try graph.followAllRefs(graph.getSubject(value_node)));
    if (try graph.getStringKeyPropertyValue(subj, "template")) |t| {
        debugPrint("statement template: {s}\n", .{graph.getString(graph.getValue(try graph.followAllRefs(t)))});
    }

    const input_arr_ref = try graph.followAllRefs(graph.getInput(value_node));
    const input_arr_node = graph.getValue(input_arr_ref);
    const residual_ref = try graph.followAllRefs(graph.getArrayElement(input_arr_node, 0).?);
    const residual_node = graph.getValue(residual_ref);
    debugPrint("residual $0 kind: {}\n", .{residual_node.kind});
}

pub fn debugTestMerging() !void {
    var nodes = BumpAllocator(ValueNode).init(getAllocator(), std.heap.page_allocator);
    try nodes.preAlloc();
    _ = try nodes.push(.{ .kind = .NUL });

    var values = ValueParser{ .bytes = &.{}, .nodes = nodes };
    var replacements = std.AutoHashMapUnmanaged(ValueRef, ValueRef){};
    var graph = ValueGraph{ .values = &values, .replacements = &replacements };
    var opt = Optimizer{ .values = &values, .graph = &graph };

    const obj_ref = try graph.createObject(0);

    const subject1_text =
        \\function(_c_c, el) {
        \\  return () => {
        \\    _c_c[0] += 1;
        \\    el[Symbol.update]();
        \\  }
        \\}
    ;
    const subject1_str = try graph.createString(subject1_text);
    const num1 = try graph.createNumber(1);
    const captured_array = try graph.createArrayFromItems(&.{num1});
    const obj_self_ref1 = try graph.createRef(obj_ref);
    const entry1_input = try graph.createArrayFromItems(&.{ captured_array, obj_self_ref1 });
    const entry1_value = try graph.createComputed(subject1_str, entry1_input);

    const subject2_text =
        \\function(el) {
        \\  return () => el.bar()
        \\}
    ;
    const subject2_str = try graph.createString(subject2_text);
    const obj_self_ref2 = try graph.createRef(obj_ref);
    const entry2_input = try graph.createArrayFromItems(&.{obj_self_ref2});
    const entry2_value = try graph.createComputed(subject2_str, entry2_input);

    const key1_str = try graph.createString("Symbol.update");
    const key2_str = try graph.createString("foo");
    values.nodes.at(key1_str).next = @truncate(entry1_value);
    values.nodes.at(entry1_value).next = @truncate(key2_str);
    values.nodes.at(key2_str).next = @truncate(entry2_value);
    values.nodes.at(entry2_value).next = 0;
    values.nodes.at(obj_ref).slot0 = key1_str;

    values.root = obj_ref;

    {
        var pre_counts = try opt.countReferences(values.root);
        defer pre_counts.deinit(getAllocator());
        _ = try opt.tryInlineComputationCall(entry1_value, &pre_counts);
        _ = try opt.tryInlineComputationCall(entry2_value, &pre_counts);
    }

    {
        var counts = try opt.countReferences(values.root);
        defer counts.deinit(getAllocator());
        const folded = try opt.tryPeelComputedObjectEntryIntoStatement(obj_ref, &counts);
        debugPrint("statement folding applied: {}\n", .{folded});
    }

    {
        var counts = try opt.countReferences(values.root);
        defer counts.deinit(getAllocator());
        const merged = try opt.tryMergeConsecutiveStatementEntries(obj_ref, &counts);
        debugPrint("merging applied: {}\n", .{merged});
    }

    {
        const key1_ref = graph.getValue(obj_ref).slot0;
        const merged_value_ref = graph.getValue(key1_ref).next;
        var counts = try opt.countReferences(values.root);
        defer counts.deinit(getAllocator());
        const inlined = try opt.tryInlineValue(merged_value_ref, &counts);
        debugPrint("value inlining applied: {}\n", .{inlined});
    }

    const obj_node = graph.getValue(obj_ref);
    var key_ref = obj_node.slot0;
    var entry_count: u32 = 0;
    while (key_ref != 0) {
        entry_count += 1;
        const key_node = graph.getValue(key_ref);
        const value_ref = key_node.next;
        const value_node = graph.getValue(try graph.followAllRefs(value_ref));
        if (value_node.kind == .computed) {
            const subj = graph.getValue(try graph.followAllRefs(graph.getSubject(value_node)));
            if (try graph.getStringKeyPropertyValue(subj, "template")) |t| {
                debugPrint("final template: {s}\n", .{graph.getString(graph.getValue(try graph.followAllRefs(t)))});
            }
        }
        key_ref = graph.getValue(value_ref).next;
    }
    debugPrint("remaining entry count: {}\n", .{entry_count});
}

pub fn debugTestEndToEnd() !void {
    const alloc = getAllocator();
    const source =
        \\{
        \\  "Symbol.update": (
        \\    "function(_c_c, el) {\n  return () => {\n    _c_c[0] += 1;\n    el[Symbol.update]();\n  }\n}",
        \\    [[1], this]
        \\  ),
        \\  foo: (
        \\    "function(el) {\n  return () => el.bar()\n}",
        \\    [this]
        \\  ),
        \\}
    ;

    var nodes = BumpAllocator(ValueNode).init(alloc, std.heap.page_allocator);
    try nodes.preAlloc();
    _ = try nodes.push(.{ .kind = .NUL }); // reserve 0 as "null"

    var emitter = value_syntax.ValueEmitter.init(alloc, &nodes);
    var p = try value_syntax.Parser(*value_syntax.ValueEmitter).init(&emitter, .{ .contents = source }, alloc);
    try p.parse();
    const root = try emitter.finish();
    debugPrint("e2e: had_error={}\n", .{emitter.had_error});

    var values = ValueParser{ .bytes = &.{}, .nodes = nodes };
    var replacements = std.AutoHashMapUnmanaged(ValueRef, ValueRef){};
    var graph = ValueGraph{ .values = &values, .replacements = &replacements };
    var opt = Optimizer{ .values = &values, .graph = &graph };
    values.root = root;

    try opt.optimizeAll(root);

    const obj_node = graph.getValue(root);
    var key_ref = obj_node.slot0;
    var entry_count: u32 = 0;
    while (key_ref != 0) {
        entry_count += 1;
        const key_node = graph.getValue(key_ref);
        const value_ref = key_node.next;
        const value_node = graph.getValue(try graph.followAllRefs(value_ref));
        if (value_node.kind == .computed) {
            const subj = graph.getValue(try graph.followAllRefs(graph.getSubject(value_node)));
            if (subj.kind == .object) {
                if (try graph.getStringKeyPropertyValue(subj, "template")) |t| {
                    debugPrint("e2e final template: {s}\n", .{graph.getString(graph.getValue(try graph.followAllRefs(t)))});
                }
            } else if (subj.kind == .string) {
                debugPrint("e2e raw subject (not inlined): {s}\n", .{graph.getString(subj)});
            }
        }
        key_ref = graph.getValue(value_ref).next;
    }
    debugPrint("e2e remaining entry count: {}\n", .{entry_count});
}

pub fn debugTestEndToEnd2() !void {
    const alloc = getAllocator();
    const source =
        \\{
        \\  s0: "function(a,b) { return () => a(b) }",
        \\  v0: (this.s0, [this.v1, this.v2]),
        \\  v1: ("function() { return (p) => console.log('hi', p) }", []),
        \\  v2: { f: ("function(a) { return () => a.s0 }", [$]) },
        \\  v3: ("function(x) { return () => x[0]++ }", [[1]]),
        \\  v4: ("function(x) { return () => x[0] }", [this.v3.#input[0]]),
        \\}
    ;

    var nodes = BumpAllocator(ValueNode).init(alloc, std.heap.page_allocator);
    try nodes.preAlloc();
    _ = try nodes.push(.{ .kind = .NUL }); // reserve 0 as "null"

    var emitter = value_syntax.ValueEmitter.init(alloc, &nodes);
    var p = try value_syntax.Parser(*value_syntax.ValueEmitter).init(&emitter, .{ .contents = source }, alloc);
    try p.parse();
    const root = try emitter.finish();
    debugPrint("e2e: had_error={}\n", .{emitter.had_error});

    var values = ValueParser{ .bytes = &.{}, .nodes = nodes };
    var replacements = std.AutoHashMapUnmanaged(ValueRef, ValueRef){};
    var graph = ValueGraph{ .values = &values, .replacements = &replacements };
    var opt = Optimizer{ .values = &values, .graph = &graph };
    values.root = root;

    try opt.optimizeAll(root);
    try graph.printGraph();

    const obj_node = graph.getValue(root);
    var key_ref = obj_node.slot0;
    var entry_count: u32 = 0;
    while (key_ref != 0) {
        entry_count += 1;
        const key_node = graph.getValue(key_ref);
        const value_ref = key_node.next;
        const value_node = graph.getValue(try graph.followAllRefs(value_ref));
        if (value_node.kind == .computed) {
            const subj = graph.getValue(try graph.followAllRefs(graph.getSubject(value_node)));
            if (subj.kind == .object) {
                if (try graph.getStringKeyPropertyValue(subj, "template")) |t| {
                    debugPrint("e2e final template: {s}\n", .{graph.getString(graph.getValue(try graph.followAllRefs(t)))});
                }
            } else if (subj.kind == .string) {
                debugPrint("e2e raw subject (not inlined): {s}\n", .{graph.getString(subj)});
            }
        }
        key_ref = graph.getValue(value_ref).next;
    }
    debugPrint("e2e remaining entry count: {}\n", .{entry_count});
}

pub fn debugTestCollapseToCode() !void {
    const alloc = getAllocator();
    const source =
        \\{
        \\  s0: "function(a,b) { return () => a(b) }",
        \\  v0: (this.s0, [this.v1, this.v2]),
        \\  v1: ("function() { return (p) => console.log('hi', p) }", []),
        \\  v2: { f: ("function(a) { return () => a.s0 }", [$]) },
        \\  v3: ("function(x) { return () => x[0]++ }", [[1]]),
        \\  v4: ("function(x) { return () => x[0] }", [this.v3.#input[0]]),
        \\}
    ;

    var nodes = BumpAllocator(ValueNode).init(alloc, std.heap.page_allocator);
    try nodes.preAlloc();
    _ = try nodes.push(.{ .kind = .NUL });

    var emitter = value_syntax.ValueEmitter.init(alloc, &nodes);
    var p = try value_syntax.Parser(*value_syntax.ValueEmitter).init(&emitter, .{ .contents = source }, alloc);
    try p.parse();
    const root = try emitter.finish();

    var values = ValueParser{ .bytes = &.{}, .nodes = nodes };
    var replacements = std.AutoHashMapUnmanaged(ValueRef, ValueRef){};
    var graph = ValueGraph{ .values = &values, .replacements = &replacements };
    var opt = Optimizer{ .values = &values, .graph = &graph };
    values.root = root;

    try opt.optimizeAll(root);

    const code = try opt.collapseToCode(graph.followReplacements(root));
    debugPrint("collapse decls:\n{s}\n", .{code.decls});
    debugPrint("collapse final: {s}\n", .{code.final});
}

pub fn debugTestCollapseToCodeSelfRef() !void {
    const alloc = getAllocator();
    const source =
        \\{
        \\  name: "root",
        \\  self: $,
        \\}
    ;

    var nodes = BumpAllocator(ValueNode).init(alloc, std.heap.page_allocator);
    try nodes.preAlloc();
    _ = try nodes.push(.{ .kind = .NUL });

    var emitter = value_syntax.ValueEmitter.init(alloc, &nodes);
    var p = try value_syntax.Parser(*value_syntax.ValueEmitter).init(&emitter, .{ .contents = source }, alloc);
    try p.parse();
    const root = try emitter.finish();

    var values = ValueParser{ .bytes = &.{}, .nodes = nodes };
    var replacements = std.AutoHashMapUnmanaged(ValueRef, ValueRef){};
    var graph = ValueGraph{ .values = &values, .replacements = &replacements };
    var opt = Optimizer{ .values = &values, .graph = &graph };
    values.root = root;

    const code = try opt.collapseToCode(graph.followReplacements(root));
    debugPrint("collapse (self-ref) decls:\n{s}\n", .{code.decls});
    debugPrint("collapse (self-ref) final: {s}\n", .{code.final});
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
