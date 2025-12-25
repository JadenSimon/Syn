const std = @import("std");
const expect = std.testing.expect;
const string = []const u8;
const stringZ = [:0]const u8;
const CodePoint = i32;
const js_lexer = @import("./lexer.zig");

const assert = std.debug.assert;

pub const Environment = struct {
    pub const enableSIMD = true;
    pub const isLinux = false;
    pub const isWindows = false;
    pub const isNative = true;
    pub const allow_assert = false;
    pub const isWasm = false;
};

pub const Encoding = enum {
    ascii,
    utf8,
    latin1,
    utf16,
};

/// Returned by classification functions that do not discriminate between utf8 and ascii.
pub const EncodingNonAscii = enum {
    utf8,
    utf16,
    latin1,
};

pub inline fn containsChar(self: string, char: u8) bool {
    return indexOfChar(self, char) != null;
}

pub inline fn removeLeadingDotSlash(slice: []const u8) []const u8 {
    if (slice.len >= 2) {
        if ((@as(u16, @bitCast(slice[0..2].*)) == comptime std.mem.readInt(u16, "./", .little)) or
            (Environment.isWindows and @as(u16, @bitCast(slice[0..2].*)) == comptime std.mem.readInt(u16, ".\\", .little)))
        {
            return slice[2..];
        }
    }
    return slice;
}

// TODO: remove this
pub const w = toUTF16Literal;

pub fn toUTF16Literal(comptime str: []const u8) [:0]const u16 {
    return literal(u16, str);
}

pub fn literal(comptime T: type, comptime str: []const u8) *const [literalLength(T, str):0]T {
    const Holder = struct {
        pub const value = switch (T) {
            u8 => (str[0..str.len].* ++ .{0})[0..str.len :0],
            u16 => std.unicode.utf8ToUtf16LeStringLiteral(str),
            else => @compileError("unsupported type " ++ @typeName(T) ++ " in strings.literal() call."),
        };
    };

    return Holder.value;
}

fn literalLength(comptime T: type, comptime str: string) usize {
    return comptime switch (T) {
        u8 => str.len,
        u16 => std.unicode.calcUtf16LeLen(str) catch unreachable,
        else => 0, // let other errors report first
    };
}

// TODO: remove this
pub const toUTF16LiteralZ = toUTF16Literal;

pub const OptionalUsize = std.meta.Int(.unsigned, @bitSizeOf(usize) - 1);
pub fn indexOfAny(slice: string, comptime str: []const u8) ?OptionalUsize {
    switch (comptime str.len) {
        0 => @compileError("str cannot be empty"),
        1 => return indexOfChar(slice, str[0]),
        else => {},
    }

    var remaining = slice;
    if (remaining.len == 0) return null;

    if (comptime Environment.enableSIMD) {
        while (remaining.len >= ascii_vector_size) {
            const vec: AsciiVector = remaining[0..ascii_vector_size].*;
            var cmp: AsciiVectorU1 = @bitCast(vec == @as(AsciiVector, @splat(@as(u8, str[0]))));
            inline for (str[1..]) |c| {
                cmp |= @bitCast(vec == @as(AsciiVector, @splat(@as(u8, c))));
            }

            if (@reduce(.Max, cmp) > 0) {
                const bitmask = @as(AsciiVectorInt, @bitCast(cmp));
                const first = @ctz(bitmask);

                return @as(OptionalUsize, @intCast(first + slice.len - remaining.len));
            }

            remaining = remaining[ascii_vector_size..];
        }
    }

    for (remaining, 0..) |c, i| {
        if (strings.indexOfChar(str, c) != null) {
            return @as(OptionalUsize, @intCast(i + slice.len - remaining.len));
        }
    }

    return null;
}

pub fn indexOfAny16(self: []const u16, comptime str: anytype) ?OptionalUsize {
    return indexOfAnyT(u16, self, str);
}

pub fn indexOfAnyT(comptime T: type, str: []const T, comptime chars: anytype) ?OptionalUsize {
    if (T == u8) return indexOfAny(str, chars);
    for (str, 0..) |c, i| {
        inline for (chars) |a| {
            if (c == a) {
                return @as(OptionalUsize, @intCast(i));
            }
        }
    }

    return null;
}

pub fn indexAnyComptime(target: string, comptime chars: string) ?usize {
    for (target, 0..) |parent, i| {
        inline for (chars) |char| {
            if (char == parent) return i;
        }
    }
    return null;
}

pub fn indexAnyComptimeT(comptime T: type, target: []const T, comptime chars: []const T) ?usize {
    for (target, 0..) |parent, i| {
        inline for (chars) |char| {
            if (char == parent) return i;
        }
    }
    return null;
}

pub fn indexEqualAny(in: anytype, target: string) ?usize {
    for (in, 0..) |str, i| if (eqlLong(str, target, true)) return i;
    return null;
}

pub fn repeatingAlloc(allocator: std.mem.Allocator, count: usize, char: u8) ![]u8 {
    const buf = try allocator.alloc(u8, count);
    repeatingBuf(buf, char);
    return buf;
}

pub fn repeatingBuf(self: []u8, char: u8) void {
    @memset(self, char);
}

pub fn indexOfCharNeg(self: string, char: u8) i32 {
    for (self, 0..) |c, i| {
        if (c == char) return @as(i32, @intCast(i));
    }
    return -1;
}

pub fn indexOfSigned(self: string, str: string) i32 {
    const i = std.mem.indexOf(u8, self, str) orelse return -1;
    return @as(i32, @intCast(i));
}

pub inline fn lastIndexOf(self: string, str: string) ?usize {
    return std.mem.lastIndexOf(u8, self, str);
}

// --
// This is faster when the string is found, by about 2x for a 8 MB file.
// It is slower when the string is NOT found
// fn indexOfPosN(comptime T: type, buf: []const u8, start_index: usize, delimiter: []const u8, comptime n: comptime_int) ?usize {
//     const k = delimiter.len;
//     const V8x32 = @Vector(n, T);
//     const V1x32 = @Vector(n, u1);
//     const Vbx32 = @Vector(n, bool);
//     const first = @splat(n, delimiter[0]);
//     const last = @splat(n, delimiter[k - 1]);

//     var end: usize = start_index + n;
//     var start: usize = end - n;
//     while (end < buf.len) {
//         start = end - n;
//         const last_end = @min(end + k - 1, buf.len);
//         const last_start = last_end - n;

//         // Look for the first character in the delimter
//         const first_chunk: V8x32 = buf[start..end][0..n].*;
//         const last_chunk: V8x32 = buf[last_start..last_end][0..n].*;
//         const mask = @bitCast(V1x32, first == first_chunk) & @bitCast(V1x32, last == last_chunk);

//         if (@reduce(.Or, mask) != 0) {
//             // TODO: Use __builtin_clz???
//             for (@as([n]bool, @bitCast(Vbx32, mask))) |match, i| {
//                 if (match and eqlLong(buf[start + i .. start + i + k], delimiter, false)) {
//                     return start + i;
//                 }
//             }
//         }
//         end = @min(end + n, buf.len);
//     }
//     if (start < buf.len) return std.mem.indexOfPos(T, buf, start_index, delimiter);
//     return null; // Not found
// }

pub fn cat(allocator: std.mem.Allocator, first: string, second: string) !string {
    var out = try allocator.alloc(u8, first.len + second.len);
    copy2(u8, out, first);
    copy2(u8, out[first.len..], second);
    return out;
}

pub fn copy2(comptime Type: type, dest: []Type, src: []const Type) void {
    const input: []const u8 = std.mem.sliceAsBytes(src);
    const output: []u8 = std.mem.sliceAsBytes(dest);

    return memmove(output, input);
}

fn memmove(output: []u8, input: []const u8) void {
    if (@intFromPtr(output.ptr) == @intFromPtr(input.ptr) or output.len == 0) return;

    const does_input_or_output_overlap = (@intFromPtr(input.ptr) < @intFromPtr(output.ptr) and
        @intFromPtr(input.ptr) + input.len > @intFromPtr(output.ptr)) or
        (@intFromPtr(output.ptr) < @intFromPtr(input.ptr) and
        @intFromPtr(output.ptr) + output.len > @intFromPtr(input.ptr));

    if (!does_input_or_output_overlap) {
        @memcpy(output[0..input.len], input);
    } else if (true) {
        C.memmove(output.ptr, input.ptr, input.len);
    } else {
        for (input, output) |input_byte, *out| {
            out.* = input_byte;
        }
    }
}

const C = struct {
    pub extern fn memmove(dest: [*]u8, src: [*]const u8, n: usize) void;
    pub extern fn memchr(ptr: [*]const u8, ch: u8, n: usize) ?[*]u8;
};

/// Copy a string into a buffer
/// Return the copied version
pub fn copy(buf: []u8, src: []const u8) []const u8 {
    const len = @min(buf.len, src.len);
    if (len > 0)
        @memcpy(buf[0..len], src[0..len]);
    return buf[0..len];
}

/// startsWith except it checks for non-empty strings
pub fn hasPrefix(self: string, str: string) bool {
    return str.len > 0 and startsWith(self, str);
}

pub fn startsWith(self: string, str: string) bool {
    if (str.len > self.len) {
        return false;
    }

    return eqlLong(self[0..str.len], str, false);
}

pub inline fn endsWithComptime(self: string, comptime str: anytype) bool {
    return self.len >= str.len and eqlComptimeIgnoreLen(self[self.len - str.len .. self.len], comptime str);
}

pub inline fn startsWithChar(self: string, char: u8) bool {
    return self.len > 0 and self[0] == char;
}

pub inline fn endsWithChar(self: string, char: u8) bool {
    return self.len > 0 and self[self.len - 1] == char;
}

pub inline fn endsWithCharOrIsZeroLength(self: string, char: u8) bool {
    return self.len == 0 or self[self.len - 1] == char;
}

pub fn withoutTrailingSlash(this: string) []const u8 {
    var href = this;
    while (href.len > 1 and (switch (href[href.len - 1]) {
        '/', '\\' => true,
        else => false,
    })) {
        href.len -= 1;
    }

    return href;
}

/// Does not strip the C:\
pub fn withoutTrailingSlashWindowsPath(this: string) []const u8 {
    if (this.len < 3 or
        this[1] != ':') return withoutTrailingSlash(this);

    var href = this;
    while (href.len > 3 and (switch (href[href.len - 1]) {
        '/', '\\' => true,
        else => false,
    })) {
        href.len -= 1;
    }

    return href;
}

/// This will remove ONE trailing slash at the end of a string,
/// but on Windows it will not remove the \ in "C:\"
pub fn pathWithoutTrailingSlashOne(str: []const u8) []const u8 {
    return if (str.len > 0 and charIsAnySlash(str[str.len - 1]))
        if (Environment.isWindows and str.len == 3 and str[1] == ':')
            // Preserve "C:\"
            str
        else
            // Remove one slash
            str[0 .. str.len - 1]
    else
        str;
}

pub fn withoutLeadingSlash(this: string) []const u8 {
    return std.mem.trimLeft(u8, this, "/");
}

pub fn withoutLeadingPathSeparator(this: string) []const u8 {
    return std.mem.trimLeft(u8, this, &.{std.fs.path.sep});
}

pub fn endsWithAny(self: string, str: string) bool {
    const end = self[self.len - 1];
    for (str) |char| {
        if (char == end) {
            return true;
        }
    }

    return false;
}

pub fn quotedAlloc(allocator: std.mem.Allocator, self: string) !string {
    var count: usize = 0;
    for (self) |char| {
        count += @intFromBool(char == '"');
    }

    if (count == 0) {
        return allocator.dupe(u8, self);
    }

    var i: usize = 0;
    var out = try allocator.alloc(u8, self.len + count);
    for (self) |char| {
        if (char == '"') {
            out[i] = '\\';
            i += 1;
        }
        out[i] = char;
        i += 1;
    }

    return out;
}

pub fn eqlAnyComptime(self: string, comptime list: []const string) bool {
    inline for (list) |item| {
        if (eqlComptimeCheckLenWithType(u8, self, item, true)) return true;
    }

    return false;
}

/// Count the occurrences of a character in an ASCII byte array
/// uses SIMD
pub fn countChar(self: string, char: u8) usize {
    var total: usize = 0;
    var remaining = self;

    const splatted: AsciiVector = @splat(char);

    while (remaining.len >= 16) {
        const vec: AsciiVector = remaining[0..ascii_vector_size].*;
        const cmp = @popCount(@as(@Vector(ascii_vector_size, u1), @bitCast(vec == splatted)));
        total += @as(usize, @reduce(.Add, cmp));
        remaining = remaining[ascii_vector_size..];
    }

    while (remaining.len > 0) {
        total += @as(usize, @intFromBool(remaining[0] == char));
        remaining = remaining[1..];
    }

    return total;
}

pub fn endsWithAnyComptime(self: string, comptime str: string) bool {
    if (comptime str.len < 10) {
        const last = self[self.len - 1];
        inline for (str) |char| {
            if (char == last) {
                return true;
            }
        }

        return false;
    } else {
        return endsWithAny(self, str);
    }
}

pub fn eql(self: string, other: anytype) bool {
    if (self.len != other.len) return false;
    if (comptime @TypeOf(other) == *string) {
        return eql(self, other.*);
    }

    for (self, 0..) |c, i| {
        if (other[i] != c) return false;
    }
    return true;
}

pub fn eqlComptimeT(comptime T: type, self: []const T, comptime alt: anytype) bool {
    if (T == u16) {
        return eqlComptimeUTF16(self, alt);
    }

    return eqlComptime(self, alt);
}

pub fn eqlComptime(self: string, comptime alt: anytype) bool {
    return eqlComptimeCheckLenWithType(u8, self, alt, true);
}

pub fn eqlComptimeUTF16(self: []const u16, comptime alt: []const u8) bool {
    return eqlComptimeCheckLenWithType(u16, self, comptime toUTF16Literal(alt), true);
}

pub fn eqlComptimeIgnoreLen(self: string, comptime alt: anytype) bool {
    return eqlComptimeCheckLenWithType(u8, self, alt, false);
}

pub fn hasPrefixComptime(self: string, comptime alt: anytype) bool {
    return self.len >= alt.len and eqlComptimeCheckLenWithType(u8, self[0..alt.len], alt, false);
}

pub fn hasPrefixComptimeUTF16(self: []const u16, comptime alt: []const u8) bool {
    return self.len >= alt.len and eqlComptimeCheckLenWithType(u16, self[0..alt.len], comptime toUTF16Literal(alt), false);
}

pub fn hasSuffixComptime(self: string, comptime alt: anytype) bool {
    return self.len >= alt.len and eqlComptimeCheckLenWithType(u8, self[self.len - alt.len ..], alt, false);
}

fn eqlComptimeCheckLenU8(a: []const u8, comptime b: []const u8, comptime check_len: bool) bool {
    @setEvalBranchQuota(9999);

    if (comptime check_len) {
        if (a.len != b.len) return false;
    }

    comptime var b_ptr: usize = 0;

    inline while (b.len - b_ptr >= @sizeOf(usize)) {
        if (@as(usize, @bitCast(a[b_ptr..][0..@sizeOf(usize)].*)) != comptime @as(usize, @bitCast(b[b_ptr..][0..@sizeOf(usize)].*)))
            return false;
        comptime b_ptr += @sizeOf(usize);
        if (comptime b_ptr == b.len) return true;
    }

    if (comptime @sizeOf(usize) == 8) {
        if (comptime (b.len & 4) != 0) {
            if (@as(u32, @bitCast(a[b_ptr..][0..@sizeOf(u32)].*)) != comptime @as(u32, @bitCast(b[b_ptr..][0..@sizeOf(u32)].*)))
                return false;
            comptime b_ptr += @sizeOf(u32);
            if (comptime b_ptr == b.len) return true;
        }
    }

    if (comptime (b.len & 2) != 0) {
        if (@as(u16, @bitCast(a[b_ptr..][0..@sizeOf(u16)].*)) != comptime @as(u16, @bitCast(b[b_ptr..][0..@sizeOf(u16)].*)))
            return false;

        comptime b_ptr += @sizeOf(u16);

        if (comptime b_ptr == b.len) return true;
    }

    if ((comptime (b.len & 1) != 0) and a[b_ptr] != comptime b[b_ptr]) return false;

    return true;
}

inline fn compare(a: anytype, b: anytype) i2 {
    if (a < b) {
        return -1;
    } else if (a > b) {
        return 1;
    } else {
        return 0;
    }
}

pub fn orderComptime(a: []const u8, comptime b: []const u8) i2 {
    @setEvalBranchQuota(9999);

    comptime var b_ptr: usize = 0;

    inline while (b.len - b_ptr >= @sizeOf(usize)) {
        const c = compare(@as(usize, @bitCast(a[b_ptr..][0..@sizeOf(usize)].*)), comptime @as(usize, @bitCast(b[b_ptr..][0..@sizeOf(usize)].*)));
        if (c != 0) return c;
        comptime b_ptr += @sizeOf(usize);
        if (comptime b_ptr == b.len) return 0;
    }

    if (comptime @sizeOf(usize) == 8) {
        if (comptime (b.len & 4) != 0) {
            const c = compare(@as(u32, @bitCast(a[b_ptr..][0..@sizeOf(u32)].*)), comptime @as(u32, @bitCast(b[b_ptr..][0..@sizeOf(u32)].*)));
            if (c != 0) return c;
            comptime b_ptr += @sizeOf(u32);
            if (comptime b_ptr == b.len) return 0;
        }
    }

    if (comptime (b.len & 2) != 0) {
        const c = compare(@as(u16, @bitCast(a[b_ptr..][0..@sizeOf(u16)].*)), comptime @as(u16, @bitCast(b[b_ptr..][0..@sizeOf(u16)].*)));
        if (c != 0) return c;

        comptime b_ptr += @sizeOf(u16);
        if (comptime b_ptr == b.len) return 0;
    }

    if ((comptime (b.len & 1) != 0)) {
        return compare(a[b_ptr], comptime b[b_ptr]);
    }

    return 0;
}

fn eqlComptimeCheckLenWithKnownType(comptime Type: type, a: []const Type, comptime b: []const Type, comptime check_len: bool) bool {
    if (comptime Type != u8) {
        return eqlComptimeCheckLenU8(std.mem.sliceAsBytes(a), comptime std.mem.sliceAsBytes(b), comptime check_len);
    }
    return eqlComptimeCheckLenU8(a, comptime b, comptime check_len);
}

/// Check if two strings are equal with one of the strings being a comptime-known value
///
///   strings.eqlComptime(input, "hello world");
///   strings.eqlComptime(input, "hai");
pub fn eqlComptimeCheckLenWithType(comptime Type: type, a: []const Type, comptime b: anytype, comptime check_len: bool) bool {
    return eqlComptimeCheckLenWithKnownType(comptime Type, a, if (@typeInfo(@TypeOf(b)) != .Pointer) &b else b, comptime check_len);
}

pub fn eqlLong(a_str: string, b_str: string, comptime check_len: bool) bool {
    const len = b_str.len;

    if (comptime check_len) {
        if (len == 0) {
            return a_str.len == 0;
        }

        if (a_str.len != len) {
            return false;
        }
    } else {
        if (comptime Environment.allow_assert) assert(b_str.len <= a_str.len);
    }

    const end = b_str.ptr + len;
    var a = a_str.ptr;
    var b = b_str.ptr;

    if (a == b)
        return true;

    {
        var dword_length = len >> 3;
        while (dword_length > 0) : (dword_length -= 1) {
            if (@as(usize, @bitCast(a[0..@sizeOf(usize)].*)) != @as(usize, @bitCast(b[0..@sizeOf(usize)].*)))
                return false;
            b += @sizeOf(usize);
            if (b == end) return true;
            a += @sizeOf(usize);
        }
    }

    if (comptime @sizeOf(usize) == 8) {
        if ((len & 4) != 0) {
            if (@as(u32, @bitCast(a[0..@sizeOf(u32)].*)) != @as(u32, @bitCast(b[0..@sizeOf(u32)].*)))
                return false;

            b += @sizeOf(u32);
            if (b == end) return true;
            a += @sizeOf(u32);
        }
    }

    if ((len & 2) != 0) {
        if (@as(u16, @bitCast(a[0..@sizeOf(u16)].*)) != @as(u16, @bitCast(b[0..@sizeOf(u16)].*)))
            return false;

        b += @sizeOf(u16);

        if (b == end) return true;

        a += @sizeOf(u16);
    }

    if (((len & 1) != 0) and a[0] != b[0]) return false;

    return true;
}

pub inline fn append(allocator: std.mem.Allocator, self: string, other: string) ![]u8 {
    var buf = try allocator.alloc(u8, self.len + other.len);
    if (self.len > 0)
        @memcpy(buf[0..self.len], self);
    if (other.len > 0)
        @memcpy(buf[self.len..][0..other.len], other);
    return buf;
}

pub inline fn concatAllocT(comptime T: type, allocator: std.mem.Allocator, strs: anytype) ![]T {
    const buf = try allocator.alloc(T, len: {
        var len: usize = 0;
        inline for (strs) |s| {
            len += s.len;
        }
        break :len len;
    });

    return concatBufT(T, buf, strs) catch |e| switch (e) {
        error.NoSpaceLeft => unreachable, // exact size calculated
    };
}

pub inline fn concatBufT(comptime T: type, out: []T, strs: anytype) ![]T {
    var remain = out;
    var n: usize = 0;
    inline for (strs) |s| {
        if (s.len > remain.len) {
            return error.NoSpaceLeft;
        }
        @memcpy(remain.ptr, s);
        remain = remain[s.len..];
        n += s.len;
    }

    return out[0..n];
}

pub fn index(self: string, str: string) i32 {
    if (strings.indexOf(self, str)) |i| {
        return @as(i32, @intCast(i));
    } else {
        return -1;
    }
}

pub fn toUTF8Alloc(allocator: std.mem.Allocator, js: []const u16) ![]u8 {
    return try toUTF8AllocWithType(allocator, []const u16, js);
}

pub inline fn appendUTF8MachineWordToUTF16MachineWord(output: *[@sizeOf(usize) / 2]u16, input: *const [@sizeOf(usize) / 2]u8) void {
    output[0 .. @sizeOf(usize) / 2].* = @as(
        [4]u16,
        @bitCast(@as(
            @Vector(4, u16),
            @as(@Vector(4, u8), @bitCast(input[0 .. @sizeOf(usize) / 2].*)),
        )),
    );
}

pub inline fn copyU8IntoU16(output_: []u16, input_: []const u8) void {
    const output = output_;
    const input = input_;
    if (comptime Environment.allow_assert) assert(input.len <= output.len);

    // https://zig.godbolt.org/z/9rTn1orcY

    var input_ptr = input.ptr;
    var output_ptr = output.ptr;

    const last_input_ptr = input_ptr + @min(input.len, output.len);

    while (last_input_ptr != input_ptr) {
        output_ptr[0] = input_ptr[0];
        output_ptr += 1;
        input_ptr += 1;
    }
}

pub fn copyU8IntoU16WithAlignment(comptime alignment: u21, output_: []align(alignment) u16, input_: []const u8) void {
    var output = output_;
    var input = input_;
    const word = @sizeOf(usize) / 2;
    if (comptime Environment.allow_assert) assert(input.len <= output.len);

    // un-aligned data access is slow
    // so we attempt to align the data
    while (!std.mem.isAligned(@intFromPtr(output.ptr), @alignOf(u16)) and input.len >= word) {
        output[0] = input[0];
        output = output[1..];
        input = input[1..];
    }

    if (std.mem.isAligned(@intFromPtr(output.ptr), @alignOf(u16)) and input.len > 0) {
        copyU8IntoU16(@as([*]u16, @alignCast(output.ptr))[0..output.len], input);
        return;
    }

    for (input, 0..) |c, i| {
        output[i] = c;
    }
}

// pub inline fn copy(output_: []u8, input_: []const u8) void {
//     var output = output_;
//     var input = input_;
//     if (comptime Environment.allow_assert) assert(input.len <= output.len);

//     if (input.len > @sizeOf(usize) * 4) {
//         comptime var i: usize = 0;
//         inline while (i < 4) : (i += 1) {
//             appendUTF8MachineWord(output[i * @sizeOf(usize) ..][0..@sizeOf(usize)], input[i * @sizeOf(usize) ..][0..@sizeOf(usize)]);
//         }
//         output = output[4 * @sizeOf(usize) ..];
//         input = input[4 * @sizeOf(usize) ..];
//     }

//     while (input.len >= @sizeOf(usize)) {
//         appendUTF8MachineWord(output[0..@sizeOf(usize)], input[0..@sizeOf(usize)]);
//         output = output[@sizeOf(usize)..];
//         input = input[@sizeOf(usize)..];
//     }

//     for (input) |c, i| {
//         output[i] = c;
//     }
// }

pub inline fn copyU16IntoU8(output_: []u8, comptime InputType: type, input_: InputType) void {
    if (comptime Environment.allow_assert) assert(input_.len <= output_.len);
    var output = output_;
    var input = input_;
    if (comptime Environment.allow_assert) assert(input.len <= output.len);

    // https://zig.godbolt.org/z/9rTn1orcY

    const group = @as(usize, 16);
    // end at the last group of 16 bytes
    var input_ptr = input.ptr;
    var output_ptr = output.ptr;

    if (comptime Environment.enableSIMD) {
        const end_len = (@min(input.len, output.len) & ~(group - 1));
        const last_vector_ptr = input.ptr + end_len;
        while (last_vector_ptr != input_ptr) {
            const input_vec1: @Vector(group, u16) = input_ptr[0..group].*;
            inline for (0..group) |i| {
                output_ptr[i] = @as(u8, @truncate(input_vec1[i]));
            }

            output_ptr += group;
            input_ptr += group;
        }

        input.len -= end_len;
        output.len -= end_len;
    }

    const last_input_ptr = input_ptr + @min(input.len, output.len);

    while (last_input_ptr != input_ptr) {
        output_ptr[0] = @as(u8, @truncate(input_ptr[0]));
        output_ptr += 1;
        input_ptr += 1;
    }
}

const strings = @This();

/// It is common on Windows to find files that are not encoded in UTF8. Most of these include
/// a 'byte-order mark' codepoint at the start of the file. The layout of this codepoint can
/// determine the encoding.
///
/// https://en.wikipedia.org/wiki/Byte_order_mark
pub const BOM = enum {
    utf8,
    utf16_le,
    utf16_be,
    utf32_le,
    utf32_be,

    pub const utf8_bytes = [_]u8{ 0xef, 0xbb, 0xbf };
    pub const utf16_le_bytes = [_]u8{ 0xff, 0xfe };
    pub const utf16_be_bytes = [_]u8{ 0xfe, 0xff };
    pub const utf32_le_bytes = [_]u8{ 0xff, 0xfe, 0x00, 0x00 };
    pub const utf32_be_bytes = [_]u8{ 0x00, 0x00, 0xfe, 0xff };

    pub fn detect(bytes: []const u8) ?BOM {
        if (bytes.len < 3) return null;
        if (eqlComptimeIgnoreLen(bytes, utf8_bytes)) return .utf8;
        if (eqlComptimeIgnoreLen(bytes, utf16_le_bytes)) {
            // if (bytes.len > 4 and eqlComptimeIgnoreLen(bytes[2..], utf32_le_bytes[2..]))
            //   return .utf32_le;
            return .utf16_le;
        }
        // if (eqlComptimeIgnoreLen(bytes, utf16_be_bytes)) return .utf16_be;
        // if (bytes.len > 4 and eqlComptimeIgnoreLen(bytes, utf32_le_bytes)) return .utf32_le;
        return null;
    }

    pub fn detectAndSplit(bytes: []const u8) struct { ?BOM, []const u8 } {
        const bom = detect(bytes);
        if (bom == null) return .{ null, bytes };
        return .{ bom, bytes[bom.?.length()..] };
    }

    pub fn getHeader(bom: BOM) []const u8 {
        return switch (bom) {
            inline else => |t| comptime &@field(BOM, @tagName(t) ++ "_bytes"),
        };
    }

    pub fn length(bom: BOM) usize {
        return switch (bom) {
            inline else => |t| comptime (&@field(BOM, @tagName(t) ++ "_bytes")).len,
        };
    }

    /// If an allocation is needed, free the input and the caller will
    /// replace it with the new return
    pub fn removeAndConvertToUTF8AndFree(bom: BOM, allocator: std.mem.Allocator, bytes: []u8) ![]u8 {
        switch (bom) {
            .utf8 => {
                C.memmove(bytes.ptr, bytes.ptr + utf8_bytes.len, bytes.len - utf8_bytes.len);
                return bytes[0 .. bytes.len - utf8_bytes.len];
            },
            .utf16_le => {
                const trimmed_bytes = bytes[utf16_le_bytes.len..];
                const trimmed_bytes_u16: []const u16 = @alignCast(std.mem.bytesAsSlice(u16, trimmed_bytes));
                const out = try toUTF8Alloc(allocator, trimmed_bytes_u16);
                allocator.free(bytes);
                return out;
            },
            else => {
                // TODO: this needs to re-encode, for now we just remove the BOM
                const bom_bytes = bom.getHeader();
                C.memmove(bytes.ptr, bytes.ptr + bom_bytes.len, bytes.len - bom_bytes.len);
                return bytes[0 .. bytes.len - bom_bytes.len];
            },
        }
    }

    pub fn convertToUTF8(bom: BOM, allocator: std.mem.Allocator, bytes: []const u8) ![]const u8 {
        switch (bom) {
            .utf8 => {
                return bytes[utf8_bytes.len..];
            },
            .utf16_le => {
                const trimmed_bytes = bytes[utf16_le_bytes.len..];
                const trimmed_bytes_u16: []const u16 = @alignCast(std.mem.bytesAsSlice(u16, trimmed_bytes));
                return toUTF8Alloc(allocator, trimmed_bytes_u16);
            },
            else => {
                std.debug.print("unhandled BOM encoding: {any}\n", .{bom});
                return error.TODO_BOM;
            },
        }
    }

    /// This is required for fs.zig's `use_shared_buffer` flag. we cannot free that pointer.
    /// The returned slice will always point to the base of the input.
    ///
    /// Requires an arraylist in case it must be grown.
    pub fn removeAndConvertToUTF8WithoutDealloc(bom: BOM, allocator: std.mem.Allocator, list: *std.ArrayListUnmanaged(u8)) ![]u8 {
        const bytes = list.items;
        switch (bom) {
            .utf8 => {
                C.memmove(bytes.ptr, bytes.ptr + utf8_bytes.len, bytes.len - utf8_bytes.len);
                return bytes[0 .. bytes.len - utf8_bytes.len];
            },
            .utf16_le => {
                const trimmed_bytes = bytes[utf16_le_bytes.len..];
                const trimmed_bytes_u16: []const u16 = @alignCast(std.mem.bytesAsSlice(u16, trimmed_bytes));
                const out = try toUTF8Alloc(allocator, trimmed_bytes_u16);
                if (list.capacity < out.len) {
                    try list.ensureTotalCapacity(allocator, out.len);
                }
                list.items.len = out.len;
                @memcpy(list.items, out);
                return out;
            },
            else => {
                // TODO: this needs to re-encode, for now we just remove the BOM
                const bom_bytes = bom.getHeader();
                C.memmove(bytes.ptr, bytes.ptr + bom_bytes.len, bytes.len - bom_bytes.len);
                return bytes[0 .. bytes.len - bom_bytes.len];
            },
        }
    }
};

// https://github.com/WebKit/WebKit/blob/443e796d1538654c34f2690e39600c70c8052b63/Source/WebCore/PAL/pal/text/TextCodecUTF8.cpp#L69
pub fn nonASCIISequenceLength(first_byte: u8) u3 {
    return switch (first_byte) {
        0...193 => 0,
        194...223 => 2,
        224...239 => 3,
        240...244 => 4,
        245...255 => 0,
    };
}

pub fn utf16CodepointWithFFFD(comptime Type: type, input: Type) UTF16Replacement {
    return utf16CodepointWithFFFDAndFirstInputChar(Type, input[0], input);
}

fn utf16CodepointWithFFFDAndFirstInputChar(comptime Type: type, char: std.meta.Elem(Type), input: Type) UTF16Replacement {
    const c0 = @as(u21, char);

    if (c0 & ~@as(u21, 0x03ff) == 0xd800) {
        // surrogate pair
        if (input.len == 1)
            return .{
                .len = 1,
                .is_lead = true,
            };
        //error.DanglingSurrogateHalf;
        const c1 = @as(u21, input[1]);
        if (c1 & ~@as(u21, 0x03ff) != 0xdc00)
            if (input.len == 1) {
                return .{
                    .len = 1,
                };
            } else {
                return .{
                    .fail = true,
                    .len = 1,
                    .code_point = unicode_replacement,
                    .is_lead = true,
                };
            };
        // return error.ExpectedSecondSurrogateHalf;

        return .{ .len = 2, .code_point = 0x10000 + (((c0 & 0x03ff) << 10) | (c1 & 0x03ff)) };
    } else if (c0 & ~@as(u21, 0x03ff) == 0xdc00) {
        // return error.UnexpectedSecondSurrogateHalf;
        return .{ .fail = true, .len = 1, .code_point = unicode_replacement };
    } else {
        return .{ .code_point = c0, .len = 1 };
    }
}

pub fn utf16Codepoint(comptime Type: type, input: Type) UTF16Replacement {
    const c0 = @as(u21, input[0]);

    if (c0 & ~@as(u21, 0x03ff) == 0xd800) {
        // surrogate pair
        if (input.len == 1)
            return .{
                .len = 1,
            };
        //error.DanglingSurrogateHalf;
        const c1 = @as(u21, input[1]);
        if (c1 & ~@as(u21, 0x03ff) != 0xdc00)
            if (input.len == 1)
                return .{
                    .len = 1,
                };
        // return error.ExpectedSecondSurrogateHalf;

        return .{ .len = 2, .code_point = 0x10000 + (((c0 & 0x03ff) << 10) | (c1 & 0x03ff)) };
    } else if (c0 & ~@as(u21, 0x03ff) == 0xdc00) {
        // return error.UnexpectedSecondSurrogateHalf;
        return .{ .len = 1 };
    } else {
        return .{ .code_point = c0, .len = 1 };
    }
}

pub fn toUTF8AllocWithType(allocator: std.mem.Allocator, comptime Type: type, utf16: Type) ![]u8 {
    var list = try std.ArrayList(u8).initCapacity(allocator, utf16.len);
    list = try toUTF8ListWithType(list, Type, utf16);
    return list.items;
}

pub fn toUTF8ListWithType(list_: std.ArrayList(u8), comptime Type: type, utf16: Type) !std.ArrayList(u8) {
    _ = list_;
    _ = utf16;
    @panic("not implemented");
}

pub fn toUTF8FromLatin1(allocator: std.mem.Allocator, latin1: []const u8) !?std.ArrayList(u8) {
    if (isAllASCII(latin1))
        return null;

    const list = try std.ArrayList(u8).initCapacity(allocator, latin1.len);
    return try allocateLatin1IntoUTF8WithList(list, 0, []const u8, latin1);
}

pub fn toUTF8FromLatin1Z(allocator: std.mem.Allocator, latin1: []const u8) !?std.ArrayList(u8) {
    if (isAllASCII(latin1))
        return null;

    const list = try std.ArrayList(u8).initCapacity(allocator, latin1.len + 1);
    var list1 = try allocateLatin1IntoUTF8WithList(list, 0, []const u8, latin1);
    try list1.append(0);
    return list1;
}


pub const EncodeIntoResult = struct {
    read: u32 = 0,
    written: u32 = 0,
};

pub fn allocateLatin1IntoUTF8WithList(list_: std.ArrayList(u8), offset_into_list: usize, comptime Type: type, latin1_: Type) !std.ArrayList(u8) {
    var latin1 = latin1_;
    var i: usize = offset_into_list;
    var list = list_;
    try list.ensureUnusedCapacity(latin1.len);

    while (latin1.len > 0) {
        if (comptime Environment.allow_assert) assert(i < list.capacity);
        var buf = list.items.ptr[i..list.capacity];

        inner: {
            var count = latin1.len / ascii_vector_size;
            while (count > 0) : (count -= 1) {
                const vec: AsciiVector = latin1[0..ascii_vector_size].*;

                if (@reduce(.Max, vec) > 127) {
                    const Int = u64;
                    const size = @sizeOf(Int);

                    // zig or LLVM doesn't do @ctz nicely with SIMD
                    if (comptime ascii_vector_size >= 8) {
                        {
                            const bytes = @as(Int, @bitCast(latin1[0..size].*));
                            // https://dotat.at/@/2022-06-27-tolower-swar.html
                            const mask = bytes & 0x8080808080808080;

                            if (mask > 0) {
                                const first_set_byte = @ctz(mask) / 8;
                                if (comptime Environment.allow_assert) assert(latin1[first_set_byte] >= 127);

                                buf[0..size].* = @as([size]u8, @bitCast(bytes));
                                buf = buf[first_set_byte..];
                                latin1 = latin1[first_set_byte..];
                                break :inner;
                            }

                            buf[0..size].* = @as([size]u8, @bitCast(bytes));
                            latin1 = latin1[size..];
                            buf = buf[size..];
                        }

                        if (comptime ascii_vector_size >= 16) {
                            const bytes = @as(Int, @bitCast(latin1[0..size].*));
                            // https://dotat.at/@/2022-06-27-tolower-swar.html
                            const mask = bytes & 0x8080808080808080;

                            if (mask > 0) {
                                const first_set_byte = @ctz(mask) / 8;
                                if (comptime Environment.allow_assert) assert(latin1[first_set_byte] >= 127);

                                buf[0..size].* = @as([size]u8, @bitCast(bytes));
                                buf = buf[first_set_byte..];
                                latin1 = latin1[first_set_byte..];
                                break :inner;
                            }
                        }
                    }
                    unreachable;
                }

                buf[0..ascii_vector_size].* = @as([ascii_vector_size]u8, @bitCast(vec))[0..ascii_vector_size].*;
                latin1 = latin1[ascii_vector_size..];
                buf = buf[ascii_vector_size..];
            }

            while (latin1.len >= 8) {
                const Int = u64;
                const size = @sizeOf(Int);

                const bytes = @as(Int, @bitCast(latin1[0..size].*));
                // https://dotat.at/@/2022-06-27-tolower-swar.html
                const mask = bytes & 0x8080808080808080;

                if (mask > 0) {
                    const first_set_byte = @ctz(mask) / 8;
                    if (comptime Environment.allow_assert) assert(latin1[first_set_byte] >= 127);

                    buf[0..size].* = @as([size]u8, @bitCast(bytes));
                    latin1 = latin1[first_set_byte..];
                    buf = buf[first_set_byte..];
                    break :inner;
                }

                buf[0..size].* = @as([size]u8, @bitCast(bytes));
                latin1 = latin1[size..];
                buf = buf[size..];
            }

            {
                if (comptime Environment.allow_assert) assert(latin1.len < 8);
                const end = latin1.ptr + latin1.len;
                while (latin1.ptr != end and latin1[0] < 128) {
                    buf[0] = latin1[0];
                    buf = buf[1..];
                    latin1 = latin1[1..];
                }
            }
        }

        while (latin1.len > 0 and latin1[0] > 127) {
            i = @intFromPtr(buf.ptr) - @intFromPtr(list.items.ptr);
            list.items.len = i;
            try list.ensureUnusedCapacity(2 + latin1.len);
            buf = list.items.ptr[i..list.capacity];
            buf[0..2].* = latin1ToCodepointBytesAssumeNotASCII(latin1[0]);
            latin1 = latin1[1..];
            buf = buf[2..];
        }

        i = @intFromPtr(buf.ptr) - @intFromPtr(list.items.ptr);
        list.items.len = i;
    }

    return list;
}

pub const UTF16Replacement = struct {
    code_point: u32 = unicode_replacement,
    len: u3 = 0,

    /// Explicit fail boolean to distinguish between a Unicode Replacement Codepoint
    /// that was already in there
    /// and a genuine error.
    fail: bool = false,

    can_buffer: bool = true,
    is_lead: bool = false,

    pub inline fn utf8Width(replacement: UTF16Replacement) u3 {
        return switch (replacement.code_point) {
            0...0x7F => 1,
            (0x7F + 1)...0x7FF => 2,
            (0x7FF + 1)...0xFFFF => 3,
            else => 4,
        };
    }
};

fn convertUTF8BytesIntoUTF16WithLength(sequence: *const [4]u8, len: u3, remaining_len: usize) UTF16Replacement {
    if (comptime Environment.allow_assert) assert(sequence[0] > 127);
    switch (len) {
        2 => {
            if (sequence[1] < 0x80 or sequence[1] > 0xBF) {
                return .{ .len = 1, .fail = true, .can_buffer = remaining_len < 2 };
            }
            return .{ .len = len, .code_point = ((@as(u32, sequence[0]) << 6) + @as(u32, sequence[1])) - 0x00003080 };
        },
        3 => {
            switch (sequence[0]) {
                0xE0 => {
                    if (sequence[1] < 0xA0 or sequence[1] > 0xBF) {
                        return .{ .len = 1, .fail = true, .can_buffer = remaining_len < 2 };
                    }
                },
                0xED => {
                    if (sequence[1] < 0x80 or sequence[1] > 0x9F) {
                        return .{ .len = 1, .fail = true, .can_buffer = remaining_len < 2 };
                    }
                },
                else => {
                    if (sequence[1] < 0x80 or sequence[1] > 0xBF) {
                        return .{ .len = 1, .fail = true, .can_buffer = remaining_len < 2 };
                    }
                },
            }
            if (sequence[2] < 0x80 or sequence[2] > 0xBF) {
                return .{ .len = 2, .fail = true, .can_buffer = remaining_len < 3 };
            }
            return .{
                .len = len,
                .code_point = ((@as(u32, sequence[0]) << 12) + (@as(u32, sequence[1]) << 6) + @as(u32, sequence[2])) - 0x000E2080,
            };
        },
        4 => {
            switch (sequence[0]) {
                0xF0 => {
                    if (sequence[1] < 0x90 or sequence[1] > 0xBF) {
                        return .{ .len = 1, .fail = true, .can_buffer = remaining_len < 2 };
                    }
                },
                0xF4 => {
                    if (sequence[1] < 0x80 or sequence[1] > 0x8F) {
                        return .{ .len = 1, .fail = true, .can_buffer = remaining_len < 2 };
                    }
                },

                // invalid code point
                // this used to be an assertion
                0...(0xF0 - 1), 0xF4 + 1...std.math.maxInt(@TypeOf(sequence[0])) => {
                    return .{ .len = 1, .fail = true, .can_buffer = false };
                },

                else => {
                    if (sequence[1] < 0x80 or sequence[1] > 0xBF) {
                        return .{ .len = 1, .fail = true, .can_buffer = remaining_len < 2 };
                    }
                },
            }

            if (sequence[2] < 0x80 or sequence[2] > 0xBF) {
                return .{ .len = 2, .fail = true, .can_buffer = remaining_len < 3 };
            }
            if (sequence[3] < 0x80 or sequence[3] > 0xBF) {
                return .{ .len = 3, .fail = true, .can_buffer = remaining_len < 4 };
            }
            return .{
                .len = len,
                .code_point = ((@as(u32, sequence[0]) << 18) +
                    (@as(u32, sequence[1]) << 12) +
                    (@as(u32, sequence[2]) << 6) + @as(u32, sequence[3])) - 0x03C82080,
            };
        },
        // invalid unicode sequence
        // 1 or 0 are both invalid here
        else => return UTF16Replacement{ .len = 1, .fail = true },
    }
}

// This variation matches WebKit behavior.
// fn convertUTF8BytesIntoUTF16(sequence: *const [4]u8, remaining_len: usize) UTF16Replacement {
fn convertUTF8BytesIntoUTF16(bytes: []const u8) UTF16Replacement {
    const sequence: [4]u8 = switch (bytes.len) {
        0 => unreachable,
        1 => [_]u8{ bytes[0], 0, 0, 0 },
        2 => [_]u8{ bytes[0], bytes[1], 0, 0 },
        3 => [_]u8{ bytes[0], bytes[1], bytes[2], 0 },
        else => bytes[0..4].*,
    };
    if (comptime Environment.allow_assert) assert(sequence[0] > 127);
    const sequence_length = nonASCIISequenceLength(sequence[0]);
    return convertUTF8BytesIntoUTF16WithLength(&sequence, sequence_length, bytes.len);
}

pub fn replaceLatin1WithUTF8(buf_: []u8) void {
    var latin1 = buf_;
    while (strings.firstNonASCII(latin1)) |i| {
        latin1[i..][0..2].* = latin1ToCodepointBytesAssumeNotASCII(latin1[i]);

        latin1 = latin1[i + 2 ..];
    }
}

pub fn elementLengthLatin1IntoUTF8(comptime Type: type, latin1_: Type) usize {
    // https://zig.godbolt.org/z/zzYexPPs9

    var latin1 = latin1_;
    const input_len = latin1.len;
    var total_non_ascii_count: usize = 0;

    // This is about 30% faster on large input compared to auto-vectorization
    if (comptime Environment.enableSIMD) {
        const end = latin1.ptr + (latin1.len - (latin1.len % ascii_vector_size));
        while (latin1.ptr != end) {
            const vec: AsciiVector = latin1[0..ascii_vector_size].*;

            // Shifting a unsigned 8 bit integer to the right by 7 bits always produces a value of 0 or 1.
            const cmp = vec >> @as(AsciiVector, @splat(
                @as(u8, 7),
            ));

            // Anding that value rather than converting it into a @Vector(16, u1) produces better code from LLVM.
            const mask: AsciiVector = cmp & @as(AsciiVector, @splat(
                @as(u8, 1),
            ));

            total_non_ascii_count += @as(usize, @reduce(.Add, mask));
            latin1 = latin1[ascii_vector_size..];
        }

        // an important hint to the compiler to not auto-vectorize the loop below
        if (latin1.len >= ascii_vector_size) unreachable;
    }

    for (latin1) |c| {
        total_non_ascii_count += @as(usize, @intFromBool(c > 127));
    }

    // each non-ascii latin1 character becomes 2 UTF8 characters
    return input_len + total_non_ascii_count;
}

pub fn copyLatin1IntoUTF16(comptime Buffer: type, buf_: Buffer, comptime Type: type, latin1_: Type) EncodeIntoResult {
    var buf = buf_;
    var latin1 = latin1_;
    while (buf.len > 0 and latin1.len > 0) {
        const to_write = strings.firstNonASCII(latin1) orelse @as(u32, @truncate(@min(latin1.len, buf.len)));
        if (comptime std.meta.alignment(Buffer) != @alignOf(u16)) {
            strings.copyU8IntoU16WithAlignment(std.meta.alignment(Buffer), buf, latin1[0..to_write]);
        } else {
            strings.copyU8IntoU16(buf, latin1[0..to_write]);
        }

        latin1 = latin1[to_write..];
        buf = buf[to_write..];
        if (latin1.len > 0 and buf.len >= 1) {
            buf[0] = latin1ToCodepointBytesAssumeNotASCII16(latin1[0]);
            latin1 = latin1[1..];
            buf = buf[1..];
        }
    }

    return .{
        .read = @as(u32, @truncate(buf_.len - buf.len)),
        .written = @as(u32, @truncate(latin1_.len - latin1.len)),
    };
}

pub fn elementLengthLatin1IntoUTF16(comptime Type: type, latin1_: Type) usize {
    // latin1 is always at most 1 UTF-16 code unit long
    if (comptime std.meta.Child([]const u16) == Type) {
        return latin1_.len;
    }

    var count: usize = 0;
    var latin1 = latin1_;
    while (latin1.len > 0) {
        const function = comptime if (std.meta.Child(Type) == u8) strings.firstNonASCIIWithType else strings.firstNonASCII16;
        const to_write = function(Type, latin1) orelse @as(u32, @truncate(latin1.len));
        count += to_write;
        latin1 = latin1[to_write..];
        if (latin1.len > 0) {
            count += comptime if (std.meta.Child(Type) == u8) 2 else 1;
            latin1 = latin1[1..];
        }
    }

    return count;
}

fn Escaped(comptime T: type) type {
    return union(enum) {
        static: []const u8,
        original: void,
        allocated: []T,
    };
}

pub fn latin1ToCodepointAssumeNotASCII(char: u8, comptime CodePointType: type) CodePointType {
    return @as(
        CodePointType,
        @intCast(latin1ToCodepointBytesAssumeNotASCII16(char)),
    );
}

const latin1_to_utf16_conversion_table = [256]u16{
    0x0000, 0x0001, 0x0002, 0x0003, 0x0004, 0x0005, 0x0006, 0x0007, // 00-07
    0x0008, 0x0009, 0x000A, 0x000B, 0x000C, 0x000D, 0x000E, 0x000F, // 08-0F
    0x0010, 0x0011, 0x0012, 0x0013, 0x0014, 0x0015, 0x0016, 0x0017, // 10-17
    0x0018, 0x0019, 0x001A, 0x001B, 0x001C, 0x001D, 0x001E, 0x001F, // 18-1F
    0x0020, 0x0021, 0x0022, 0x0023, 0x0024, 0x0025, 0x0026, 0x0027, // 20-27
    0x0028, 0x0029, 0x002A, 0x002B, 0x002C, 0x002D, 0x002E, 0x002F, // 28-2F
    0x0030, 0x0031, 0x0032, 0x0033, 0x0034, 0x0035, 0x0036, 0x0037, // 30-37
    0x0038, 0x0039, 0x003A, 0x003B, 0x003C, 0x003D, 0x003E, 0x003F, // 38-3F
    0x0040, 0x0041, 0x0042, 0x0043, 0x0044, 0x0045, 0x0046, 0x0047, // 40-47
    0x0048, 0x0049, 0x004A, 0x004B, 0x004C, 0x004D, 0x004E, 0x004F, // 48-4F
    0x0050, 0x0051, 0x0052, 0x0053, 0x0054, 0x0055, 0x0056, 0x0057, // 50-57
    0x0058, 0x0059, 0x005A, 0x005B, 0x005C, 0x005D, 0x005E, 0x005F, // 58-5F
    0x0060, 0x0061, 0x0062, 0x0063, 0x0064, 0x0065, 0x0066, 0x0067, // 60-67
    0x0068, 0x0069, 0x006A, 0x006B, 0x006C, 0x006D, 0x006E, 0x006F, // 68-6F
    0x0070, 0x0071, 0x0072, 0x0073, 0x0074, 0x0075, 0x0076, 0x0077, // 70-77
    0x0078, 0x0079, 0x007A, 0x007B, 0x007C, 0x007D, 0x007E, 0x007F, // 78-7F
    0x20AC, 0x0081, 0x201A, 0x0192, 0x201E, 0x2026, 0x2020, 0x2021, // 80-87
    0x02C6, 0x2030, 0x0160, 0x2039, 0x0152, 0x008D, 0x017D, 0x008F, // 88-8F
    0x0090, 0x2018, 0x2019, 0x201C, 0x201D, 0x2022, 0x2013, 0x2014, // 90-97
    0x02DC, 0x2122, 0x0161, 0x203A, 0x0153, 0x009D, 0x017E, 0x0178, // 98-9F
    0x00A0, 0x00A1, 0x00A2, 0x00A3, 0x00A4, 0x00A5, 0x00A6, 0x00A7, // A0-A7
    0x00A8, 0x00A9, 0x00AA, 0x00AB, 0x00AC, 0x00AD, 0x00AE, 0x00AF, // A8-AF
    0x00B0, 0x00B1, 0x00B2, 0x00B3, 0x00B4, 0x00B5, 0x00B6, 0x00B7, // B0-B7
    0x00B8, 0x00B9, 0x00BA, 0x00BB, 0x00BC, 0x00BD, 0x00BE, 0x00BF, // B8-BF
    0x00C0, 0x00C1, 0x00C2, 0x00C3, 0x00C4, 0x00C5, 0x00C6, 0x00C7, // C0-C7
    0x00C8, 0x00C9, 0x00CA, 0x00CB, 0x00CC, 0x00CD, 0x00CE, 0x00CF, // C8-CF
    0x00D0, 0x00D1, 0x00D2, 0x00D3, 0x00D4, 0x00D5, 0x00D6, 0x00D7, // D0-D7
    0x00D8, 0x00D9, 0x00DA, 0x00DB, 0x00DC, 0x00DD, 0x00DE, 0x00DF, // D8-DF
    0x00E0, 0x00E1, 0x00E2, 0x00E3, 0x00E4, 0x00E5, 0x00E6, 0x00E7, // E0-E7
    0x00E8, 0x00E9, 0x00EA, 0x00EB, 0x00EC, 0x00ED, 0x00EE, 0x00EF, // E8-EF
    0x00F0, 0x00F1, 0x00F2, 0x00F3, 0x00F4, 0x00F5, 0x00F6, 0x00F7, // F0-F7
    0x00F8, 0x00F9, 0x00FA, 0x00FB, 0x00FC, 0x00FD, 0x00FE, 0x00FF, // F8-FF
};

pub fn latin1ToCodepointBytesAssumeNotASCII(char: u32) [2]u8 {
    var bytes = [4]u8{ 0, 0, 0, 0 };
    _ = encodeWTF8Rune(&bytes, @as(i32, @intCast(char)));
    return bytes[0..2].*;
}

pub fn latin1ToCodepointBytesAssumeNotASCII16(char: u32) u16 {
    return latin1_to_utf16_conversion_table[@as(u8, @truncate(char))];
}

pub fn elementLengthUTF16IntoUTF8(comptime Type: type, utf16: Type) usize {
    var utf16_remaining = utf16;
    var count: usize = 0;

    while (firstNonASCII16(Type, utf16_remaining)) |i| {
        count += i;

        utf16_remaining = utf16_remaining[i..];

        const replacement = utf16Codepoint(Type, utf16_remaining);

        count += replacement.utf8Width();
        utf16_remaining = utf16_remaining[replacement.len..];
    }

    return count + utf16_remaining.len;
}

pub fn elementLengthUTF8IntoUTF16(comptime Type: type, utf8: Type) usize {
    var utf8_remaining = utf8;
    var count: usize = 0;

    while (firstNonASCII(utf8_remaining)) |i| {
        count += i;

        utf8_remaining = utf8_remaining[i..];

        const replacement = utf16Codepoint(Type, utf8_remaining);

        count += replacement.len;
        utf8_remaining = utf8_remaining[@min(replacement.utf8Width(), utf8_remaining.len)..];
    }

    return count + utf8_remaining.len;
}

// Check utf16 string equals utf8 string without allocating extra memory
pub fn utf16EqlString(text: []const u16, str: string) bool {
    if (text.len > str.len) {
        // Strings can't be equal if UTF-16 encoding is longer than UTF-8 encoding
        return false;
    }

    var temp = [4]u8{ 0, 0, 0, 0 };
    const n = text.len;
    var j: usize = 0;
    var i: usize = 0;
    // TODO: is it safe to just make this u32 or u21?
    var r1: i32 = undefined;
    while (i < n) : (i += 1) {
        r1 = text[i];
        if (r1 >= 0xD800 and r1 <= 0xDBFF and i + 1 < n) {
            const r2: i32 = text[i + 1];
            if (r2 >= 0xDC00 and r2 <= 0xDFFF) {
                r1 = (r1 - 0xD800) << 10 | (r2 - 0xDC00) + 0x10000;
                i += 1;
            }
        }

        const width = encodeWTF8Rune(&temp, r1);
        if (j + width > str.len) {
            return false;
        }
        for (0..width) |k| {
            if (temp[k] != str[j]) {
                return false;
            }
            j += 1;
        }
    }

    return j == str.len;
}

// This is a clone of golang's "utf8.EncodeRune" that has been modified to encode using
// WTF-8 instead. See https://simonsapin.github.io/wtf-8/ for more info.
pub fn encodeWTF8Rune(p: *[4]u8, r: i32) u3 {
    return @call(
        .always_inline,
        encodeWTF8RuneT,
        .{
            p,
            u32,
            @as(u32, @intCast(r)),
        },
    );
}

pub fn encodeWTF8RuneT(p: *[4]u8, comptime R: type, r: R) u3 {
    switch (r) {
        0...0x7F => {
            p[0] = @as(u8, @intCast(r));
            return 1;
        },
        (0x7F + 1)...0x7FF => {
            p[0] = @as(u8, @truncate(0xC0 | ((r >> 6))));
            p[1] = @as(u8, @truncate(0x80 | (r & 0x3F)));
            return 2;
        },
        (0x7FF + 1)...0xFFFF => {
            p[0] = @as(u8, @truncate(0xE0 | ((r >> 12))));
            p[1] = @as(u8, @truncate(0x80 | ((r >> 6) & 0x3F)));
            p[2] = @as(u8, @truncate(0x80 | (r & 0x3F)));
            return 3;
        },
        else => {
            p[0] = @as(u8, @truncate(0xF0 | ((r >> 18))));
            p[1] = @as(u8, @truncate(0x80 | ((r >> 12) & 0x3F)));
            p[2] = @as(u8, @truncate(0x80 | ((r >> 6) & 0x3F)));
            p[3] = @as(u8, @truncate(0x80 | (r & 0x3F)));
            return 4;
        },
    }
}

pub fn wtf8Sequence(code_point: u32) [4]u8 {
    return switch (code_point) {
        0...0x7f => .{
            @intCast(code_point),
            0,
            0,
            0,
        },
        (0x7f + 1)...0x7ff => .{
            @truncate(0xc0 | (code_point >> 6)),
            @truncate(0x80 | (code_point & 0x3f)),
            0,
            0,
        },
        (0x7ff + 1)...0xffff => .{
            @truncate(0xe0 | (code_point >> 12)),
            @truncate(0x80 | ((code_point >> 6) & 0x3f)),
            @truncate(0x80 | (code_point & 0x3f)),
            0,
        },
        else => .{
            @truncate(0xf0 | (code_point >> 18)),
            @truncate(0x80 | ((code_point >> 12) & 0x3f)),
            @truncate(0x80 | ((code_point >> 6) & 0x3f)),
            @truncate(0x80 | (code_point & 0x3f)),
        },
    };
}

pub inline fn wtf8ByteSequenceLength(first_byte: u8) u3 {
    return switch (first_byte) {
        0 => 0,
        1...0x80 - 1 => 1,
        else => if ((first_byte & 0xE0) == 0xC0)
            @as(u3, 2)
        else if ((first_byte & 0xF0) == 0xE0)
            @as(u3, 3)
        else if ((first_byte & 0xF8) == 0xF0)
            @as(u3, 4)
        else
            @as(u3, 1),
    };
}

/// 0 == invalid
pub inline fn wtf8ByteSequenceLengthWithInvalid(first_byte: u8) u3 {
    return switch (first_byte) {
        0...0x80 - 1 => 1,
        else => if ((first_byte & 0xE0) == 0xC0)
            @as(u3, 2)
        else if ((first_byte & 0xF0) == 0xE0)
            @as(u3, 3)
        else if ((first_byte & 0xF8) == 0xF0)
            @as(u3, 4)
        else
            @as(u3, 1),
    };
}

/// Convert potentially ill-formed UTF-8 or UTF-16 bytes to a Unicode Codepoint.
/// Invalid codepoints are replaced with `zero` parameter
/// This is a clone of esbuild's decodeWTF8Rune
/// which was a clone of golang's "utf8.DecodeRune" that was modified to decode using WTF-8 instead.
/// Asserts a multi-byte codepoint
pub inline fn decodeWTF8RuneTMultibyte(p: *const [4]u8, len: u3, comptime T: type, comptime zero: T) T {
    if (comptime Environment.allow_assert) assert(len > 1);

    const s1 = p[1];
    if ((s1 & 0xC0) != 0x80) return zero;

    if (len == 2) {
        const cp = @as(T, p[0] & 0x1F) << 6 | @as(T, s1 & 0x3F);
        if (cp < 0x80) return zero;
        return cp;
    }

    const s2 = p[2];

    if ((s2 & 0xC0) != 0x80) return zero;

    if (len == 3) {
        const cp = (@as(T, p[0] & 0x0F) << 12) | (@as(T, s1 & 0x3F) << 6) | (@as(T, s2 & 0x3F));
        if (cp < 0x800) return zero;
        return cp;
    }

    const s3 = p[3];
    {
        const cp = (@as(T, p[0] & 0x07) << 18) | (@as(T, s1 & 0x3F) << 12) | (@as(T, s2 & 0x3F) << 6) | (@as(T, s3 & 0x3F));
        if (cp < 0x10000 or cp > 0x10FFFF) return zero;
        return cp;
    }

    unreachable;
}

pub const ascii_vector_size = if (Environment.isWasm) 8 else 16;
pub const ascii_u16_vector_size = if (Environment.isWasm) 4 else 8;
pub const AsciiVectorInt = std.meta.Int(.unsigned, ascii_vector_size);
pub const AsciiVectorIntU16 = std.meta.Int(.unsigned, ascii_u16_vector_size);
pub const max_16_ascii: @Vector(ascii_vector_size, u8) = @splat(@as(u8, 127));
pub const min_16_ascii: @Vector(ascii_vector_size, u8) = @splat(@as(u8, 0x20));
pub const max_u16_ascii: @Vector(ascii_u16_vector_size, u16) = @splat(@as(u16, 127));
pub const min_u16_ascii: @Vector(ascii_u16_vector_size, u16) = @splat(@as(u16, 0x20));
pub const AsciiVector = @Vector(ascii_vector_size, u8);
pub const AsciiVectorSmall = @Vector(8, u8);
pub const AsciiVectorU1 = @Vector(ascii_vector_size, u1);
pub const AsciiVectorU1Small = @Vector(8, u1);
pub const AsciiVectorU16U1 = @Vector(ascii_u16_vector_size, u1);
pub const AsciiU16Vector = @Vector(ascii_u16_vector_size, u16);
pub const max_4_ascii: @Vector(4, u8) = @splat(@as(u8, 127));

const UTF8_ACCEPT: u8 = 0;
const UTF8_REJECT: u8 = 12;

const utf8d: [364]u8 = .{
    // The first part of the table maps bytes to character classes that
    // to reduce the size of the transition table and create bitmasks.
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,
    7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,
    8,  8,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,
    10, 3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  4,  3,  3,  11, 6,  6,  6,  5,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8,

    // The second part is a transition table that maps a combination
    // of a state of the automaton and a character class to a state.
    0,  12, 24, 36, 60, 96, 84, 12, 12, 12, 48, 72, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 0,  12, 12, 12, 12, 12, 0,
    12, 0,  12, 12, 12, 24, 12, 12, 12, 12, 12, 24, 12, 24, 12, 12, 12, 12, 12, 12, 12, 12, 12, 24, 12, 12, 12, 12, 12, 24, 12, 12,
    12, 12, 12, 12, 12, 24, 12, 12, 12, 12, 12, 12, 12, 12, 12, 36, 12, 36, 12, 12, 12, 36, 12, 12, 12, 12, 12, 36, 12, 36, 12, 12,
    12, 36, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12,
};

pub fn decodeCheck(state: u8, byte: u8) u8 {
    const char_type: u32 = utf8d[byte];
    // we dont care about the codep
    // codep = if (*state != UTF8_ACCEPT) (byte & 0x3f) | (*codep << 6) else (0xff >> char_type) & (byte);

    const value = @as(u32, 256) + state + char_type;
    if (value >= utf8d.len) return UTF8_REJECT;
    return utf8d[value];
}

// Copyright (c) 2008-2009 Bjoern Hoehrmann <bjoern@hoehrmann.de>
// See http://bjoern.hoehrmann.de/utf-8/decoder/dfa/ for details.
pub fn isValidUTF8WithoutSIMD(slice: []const u8) bool {
    var state: u8 = 0;

    for (slice) |byte| {
        state = decodeCheck(state, byte);
    }
    return state == UTF8_ACCEPT;
}

pub fn isValidUTF8(slice: []const u8) bool {
    return isValidUTF8WithoutSIMD(slice);
}

pub fn isAllASCII(slice: []const u8) bool {
    if (@inComptime()) {
        for (slice) |char| {
            if (char > 127) {
                return false;
            }
        }
        return true;
    }

    var remaining = slice;

    // The NEON SIMD unit is 128-bit wide and includes 16 128-bit registers that can be used as 32 64-bit registers
    if (comptime Environment.enableSIMD) {
        const remaining_end_ptr = remaining.ptr + remaining.len - (remaining.len % ascii_vector_size);
        while (remaining.ptr != remaining_end_ptr) : (remaining.ptr += ascii_vector_size) {
            const vec: AsciiVector = remaining[0..ascii_vector_size].*;

            if (@reduce(.Max, vec) > 127) {
                return false;
            }
        }
    }

    const Int = u64;
    const size = @sizeOf(Int);
    const remaining_last8 = slice.ptr + slice.len - (slice.len % size);
    while (remaining.ptr != remaining_last8) : (remaining.ptr += size) {
        const bytes = @as(Int, @bitCast(remaining[0..size].*));
        // https://dotat.at/@/2022-06-27-tolower-swar.html
        const mask = bytes & 0x8080808080808080;

        if (mask > 0) {
            return false;
        }
    }

    const final = slice.ptr + slice.len;
    while (remaining.ptr != final) : (remaining.ptr += 1) {
        if (remaining[0] > 127) {
            return false;
        }
    }

    return true;
}

// #define U16_LEAD(supplementary) (UChar)(((supplementary)>>10)+0xd7c0)
pub inline fn u16Lead(supplementary: anytype) u16 {
    return @intCast((supplementary >> 10) + 0xd7c0);
}

// #define U16_TRAIL(supplementary) (UChar)(((supplementary)&0x3ff)|0xdc00)
pub inline fn u16Trail(supplementary: anytype) u16 {
    return @intCast((supplementary & 0x3ff) | 0xdc00);
}

// #define U16_IS_TRAIL(c) (((c)&0xfffffc00)==0xdc00)
pub inline fn u16IsTrail(supplementary: u16) bool {
    return (@as(u32, @intCast(supplementary)) & 0xfffffc00) == 0xdc00;
}

// #define U16_IS_LEAD(c) (((c)&0xfffffc00)==0xd800)
pub inline fn u16IsLead(supplementary: u16) bool {
    return (@as(u32, @intCast(supplementary)) & 0xfffffc00) == 0xd800;
}

// #define U16_GET_SUPPLEMENTARY(lead, trail) \
//     (((UChar32)(lead)<<10UL)+(UChar32)(trail)-U16_SURROGATE_OFFSET)
pub inline fn u16GetSupplementary(lead: u32, trail: u32) u32 {
    const shifted = lead << 10;
    return (shifted + trail) - u16_surrogate_offset;
}

// #define U16_SURROGATE_OFFSET ((0xd800<<10UL)+0xdc00-0x10000)
pub const u16_surrogate_offset = 56613888;

pub fn firstNonASCII(slice: []const u8) ?u32 {
    return firstNonASCIIWithType([]const u8, slice);
}

pub fn firstNonASCIIWithType(comptime Type: type, slice: Type) ?u32 {
    var remaining = slice;

    if (comptime Environment.enableSIMD) {
        if (remaining.len >= ascii_vector_size) {
            const remaining_start = remaining.ptr;
            const remaining_end = remaining.ptr + remaining.len - (remaining.len % ascii_vector_size);

            while (remaining.ptr != remaining_end) {
                const vec: AsciiVector = remaining[0..ascii_vector_size].*;

                if (@reduce(.Max, vec) > 127) {
                    const Int = u64;
                    const size = @sizeOf(Int);
                    remaining.len -= @intFromPtr(remaining.ptr) - @intFromPtr(remaining_start);

                    {
                        const bytes = @as(Int, @bitCast(remaining[0..size].*));
                        // https://dotat.at/@/2022-06-27-tolower-swar.html
                        const mask = bytes & 0x8080808080808080;

                        if (mask > 0) {
                            const first_set_byte = @ctz(mask) / 8;

                            return @as(u32, first_set_byte) + @as(u32, @intCast(slice.len - remaining.len));
                        }
                        remaining = remaining[size..];
                    }
                    {
                        const bytes = @as(Int, @bitCast(remaining[0..size].*));
                        const mask = bytes & 0x8080808080808080;

                        if (mask > 0) {
                            const first_set_byte = @ctz(mask) / 8;

                            return @as(u32, first_set_byte) + @as(u32, @intCast(slice.len - remaining.len));
                        }
                    }
                    unreachable;
                }

                // the more intuitive way, using slices, produces worse codegen
                // specifically: it subtracts the length at the end of the loop
                // we don't need to do that
                // we only need to subtract the length once at the very end
                remaining.ptr += ascii_vector_size;
            }
            remaining.len -= @intFromPtr(remaining.ptr) - @intFromPtr(remaining_start);
        }
    }

    {
        const Int = u64;
        const size = @sizeOf(Int);
        const remaining_start = remaining.ptr;
        const remaining_end = remaining.ptr + remaining.len - (remaining.len % size);

        if (remaining.len >= size) {
            while (remaining.ptr != remaining_end) {
                const bytes = @as(Int, @bitCast(remaining[0..size].*));
                // https://dotat.at/@/2022-06-27-tolower-swar.html
                const mask = bytes & 0x8080808080808080;

                if (mask > 0) {
                    remaining.len -= @intFromPtr(remaining.ptr) - @intFromPtr(remaining_start);
                    const first_set_byte = @ctz(mask) / 8;

                    return @as(u32, first_set_byte) + @as(u32, @intCast(slice.len - remaining.len));
                }

                remaining.ptr += size;
            }
            remaining.len -= @intFromPtr(remaining.ptr) - @intFromPtr(remaining_start);
        }
    }

    if (comptime Environment.allow_assert) assert(remaining.len < 8);

    for (remaining) |*char| {
        if (char.* > 127) {
            // try to prevent it from reading the length of the slice
            return @as(u32, @truncate(@intFromPtr(char) - @intFromPtr(slice.ptr)));
        }
    }

    return null;
}

pub fn indexOfNewlineOrNonASCIIOrANSI(slice_: []const u8, offset: u32) ?u32 {
    const slice = slice_[offset..];
    var remaining = slice;

    if (remaining.len == 0)
        return null;

    if (comptime Environment.enableSIMD) {
        while (remaining.len >= ascii_vector_size) {
            const vec: AsciiVector = remaining[0..ascii_vector_size].*;
            const cmp = @as(AsciiVectorU1, @bitCast((vec > max_16_ascii))) | @as(AsciiVectorU1, @bitCast((vec < min_16_ascii))) |
                @as(AsciiVectorU1, @bitCast(vec == @as(AsciiVector, @splat(@as(u8, '\r'))))) |
                @as(AsciiVectorU1, @bitCast(vec == @as(AsciiVector, @splat(@as(u8, '\n'))))) |
                @as(AsciiVectorU1, @bitCast(vec == @as(AsciiVector, @splat(@as(u8, '\x1b')))));

            if (@reduce(.Max, cmp) > 0) {
                const bitmask = @as(AsciiVectorInt, @bitCast(cmp));
                const first = @ctz(bitmask);

                return @as(u32, first) + @as(u32, @intCast(slice.len - remaining.len)) + offset;
            }

            remaining = remaining[ascii_vector_size..];
        }

        if (comptime Environment.allow_assert) assert(remaining.len < ascii_vector_size);
    }

    for (remaining) |*char_| {
        const char = char_.*;
        if (char > 127 or char < 0x20 or char == '\n' or char == '\r' or char == '\x1b') {
            return @as(u32, @truncate((@intFromPtr(char_) - @intFromPtr(slice.ptr)))) + offset;
        }
    }

    return null;
}

pub fn indexOfNewlineOrNonASCII(slice_: []const u8, offset: u32) ?u32 {
    return indexOfNewlineOrNonASCIICheckStart(slice_, offset, true);
}

pub fn indexOfNewlineOrNonASCIICheckStart(slice_: []const u8, offset: u32, comptime check_start: bool) ?u32 {
    const slice = slice_[offset..];
    var remaining = slice;

    if (remaining.len == 0)
        return null;

    if (comptime check_start) {
        // this shows up in profiling
        if (remaining[0] > 127 or remaining[0] < 0x20 or remaining[0] == '\r' or remaining[0] == '\n') {
            return offset;
        }
    }

    if (comptime Environment.enableSIMD) {
        while (remaining.len >= ascii_vector_size) {
            const vec: AsciiVector = remaining[0..ascii_vector_size].*;
            const cmp = @as(AsciiVectorU1, @bitCast((vec > max_16_ascii))) | @as(AsciiVectorU1, @bitCast((vec < min_16_ascii))) |
                @as(AsciiVectorU1, @bitCast(vec == @as(AsciiVector, @splat(@as(u8, '\r'))))) |
                @as(AsciiVectorU1, @bitCast(vec == @as(AsciiVector, @splat(@as(u8, '\n')))));

            if (@reduce(.Max, cmp) > 0) {
                const bitmask = @as(AsciiVectorInt, @bitCast(cmp));
                const first = @ctz(bitmask);

                return @as(u32, first) + @as(u32, @intCast(slice.len - remaining.len)) + offset;
            }

            remaining = remaining[ascii_vector_size..];
        }

        if (comptime Environment.allow_assert) assert(remaining.len < ascii_vector_size);
    }

    for (remaining) |*char_| {
        const char = char_.*;
        if (char > 127 or char < 0x20 or char == '\n' or char == '\r') {
            return @as(u32, @truncate((@intFromPtr(char_) - @intFromPtr(slice.ptr)))) + offset;
        }
    }

    return null;
}

pub fn containsNewlineOrNonASCIIOrQuote(slice_: []const u8) bool {
    const slice = slice_;
    var remaining = slice;

    if (remaining.len == 0)
        return false;

    if (comptime Environment.enableSIMD) {
        while (remaining.len >= ascii_vector_size) {
            const vec: AsciiVector = remaining[0..ascii_vector_size].*;
            const cmp = @as(AsciiVectorU1, @bitCast((vec > max_16_ascii))) | @as(AsciiVectorU1, @bitCast((vec < min_16_ascii))) |
                @as(AsciiVectorU1, @bitCast(vec == @as(AsciiVector, @splat(@as(u8, '\r'))))) |
                @as(AsciiVectorU1, @bitCast(vec == @as(AsciiVector, @splat(@as(u8, '\n'))))) |
                @as(AsciiVectorU1, @bitCast(vec == @as(AsciiVector, @splat(@as(u8, '"')))));

            if (@reduce(.Max, cmp) > 0) {
                return true;
            }

            remaining = remaining[ascii_vector_size..];
        }

        if (comptime Environment.allow_assert) assert(remaining.len < ascii_vector_size);
    }

    for (remaining) |*char_| {
        const char = char_.*;
        if (char > 127 or char < 0x20 or char == '\n' or char == '\r' or char == '"') {
            return true;
        }
    }

    return false;
}

pub fn indexOfNeedsEscape(slice: []const u8) ?u32 {
    var remaining = slice;
    if (remaining.len == 0)
        return null;

    if (remaining[0] >= 127 or remaining[0] < 0x20 or remaining[0] == '\\' or remaining[0] == '"') {
        return 0;
    }

    if (comptime Environment.enableSIMD) {
        while (remaining.len >= ascii_vector_size) {
            const vec: AsciiVector = remaining[0..ascii_vector_size].*;
            const cmp = @as(AsciiVectorU1, @bitCast((vec > max_16_ascii))) | @as(AsciiVectorU1, @bitCast((vec < min_16_ascii))) |
                @as(AsciiVectorU1, @bitCast(vec == @as(AsciiVector, @splat(@as(u8, '\\'))))) |
                @as(AsciiVectorU1, @bitCast(vec == @as(AsciiVector, @splat(@as(u8, '"')))));

            if (@reduce(.Max, cmp) > 0) {
                const bitmask = @as(AsciiVectorInt, @bitCast(cmp));
                const first = @ctz(bitmask);

                return @as(u32, first) + @as(u32, @truncate(@intFromPtr(remaining.ptr) - @intFromPtr(slice.ptr)));
            }

            remaining = remaining[ascii_vector_size..];
        }
    }

    for (remaining) |*char_| {
        const char = char_.*;
        if (char > 127 or char < 0x20 or char == '\\' or char == '"') {
            return @as(u32, @truncate(@intFromPtr(char_) - @intFromPtr(slice.ptr)));
        }
    }

    return null;
}

pub fn indexOfChar(slice: []const u8, char: u8) ?u32 {
    return @as(u32, @truncate(indexOfCharUsize(slice, char) orelse return null));
}

pub fn indexOfCharUsize(slice: []const u8, char: u8) ?usize {
    if (slice.len == 0)
        return null;

    if (comptime Environment.isNative) {
        const ptr = C.memchr(slice.ptr, char, slice.len) orelse return null;

        return @intFromPtr(ptr) - @intFromPtr(slice.ptr);
    }

    return std.mem.indexOfScalar(u8, slice, char);
}

pub fn indexOfChar16Usize(slice: []const u16, char: u16) ?usize {
    return std.mem.indexOfScalar(u16, slice, char);
}

pub fn indexOfNotChar(slice: []const u8, char: u8) ?u32 {
    var remaining = slice;
    if (remaining.len == 0)
        return null;

    if (remaining[0] != char)
        return 0;

    if (comptime Environment.enableSIMD) {
        while (remaining.len >= ascii_vector_size) {
            const vec: AsciiVector = remaining[0..ascii_vector_size].*;
            const cmp = @as(AsciiVector, @splat(char)) != vec;
            if (@reduce(.Max, @as(AsciiVectorU1, @bitCast(cmp))) > 0) {
                const bitmask = @as(AsciiVectorInt, @bitCast(cmp));
                const first = @ctz(bitmask);
                return @as(u32, first) + @as(u32, @intCast(slice.len - remaining.len));
            }

            remaining = remaining[ascii_vector_size..];
        }
    }

    for (remaining) |*current| {
        if (current.* != char) {
            return @as(u32, @truncate(@intFromPtr(current) - @intFromPtr(slice.ptr)));
        }
    }

    return null;
}

const invalid_char: u8 = 0xff;
const hex_table: [255]u8 = brk: {
    var values: [255]u8 = [_]u8{invalid_char} ** 255;
    values['0'] = 0;
    values['1'] = 1;
    values['2'] = 2;
    values['3'] = 3;
    values['4'] = 4;
    values['5'] = 5;
    values['6'] = 6;
    values['7'] = 7;
    values['8'] = 8;
    values['9'] = 9;
    values['A'] = 10;
    values['B'] = 11;
    values['C'] = 12;
    values['D'] = 13;
    values['E'] = 14;
    values['F'] = 15;
    values['a'] = 10;
    values['b'] = 11;
    values['c'] = 12;
    values['d'] = 13;
    values['e'] = 14;
    values['f'] = 15;

    break :brk values;
};

pub fn decodeHexToBytes(destination: []u8, comptime Char: type, source: []const Char) !usize {
    return _decodeHexToBytes(destination, Char, source, false);
}

pub fn decodeHexToBytesTruncate(destination: []u8, comptime Char: type, source: []const Char) usize {
    return _decodeHexToBytes(destination, Char, source, true) catch 0;
}

inline fn _decodeHexToBytes(destination: []u8, comptime Char: type, source: []const Char, comptime truncate: bool) !usize {
    var remain = destination;
    var input = source;

    while (remain.len > 0 and input.len > 1) {
        const int = input[0..2].*;
        if (comptime @sizeOf(Char) > 1) {
            if (int[0] > std.math.maxInt(u8) or int[1] > std.math.maxInt(u8)) {
                if (comptime truncate) break;
                return error.InvalidByteSequence;
            }
        }
        const a = hex_table[@as(u8, @truncate(int[0]))];
        const b = hex_table[@as(u8, @truncate(int[1]))];
        if (a == invalid_char or b == invalid_char) {
            if (comptime truncate) break;
            return error.InvalidByteSequence;
        }
        remain[0] = a << 4 | b;
        remain = remain[1..];
        input = input[2..];
    }

    if (comptime !truncate) {
        if (remain.len > 0 and input.len > 0) return error.InvalidByteSequence;
    }

    return destination.len - remain.len;
}

fn byte2hex(char: u8) u8 {
    return switch (char) {
        0...9 => char + '0',
        10...15 => char - 10 + 'a',
        else => unreachable,
    };
}

pub fn encodeBytesToHex(destination: []u8, source: []const u8) usize {
    const to_write = if (destination.len < source.len * 2)
        destination.len - destination.len % 2
    else
        source.len * 2;

    const to_read = to_write / 2;

    var remaining = source[0..to_read];
    var remaining_dest = destination;
    if (comptime Environment.enableSIMD) {
        const remaining_end = remaining.ptr + remaining.len - (remaining.len % 16);
        while (remaining.ptr != remaining_end) {
            const input_chunk: @Vector(16, u8) = remaining[0..16].*;
            const input_chunk_4: @Vector(16, u8) = input_chunk >> @as(@Vector(16, u8), @splat(@as(u8, 4)));
            const input_chunk_15: @Vector(16, u8) = input_chunk & @as(@Vector(16, u8), @splat(@as(u8, 15)));

            var lower_16: @Vector(16, u8) = undefined;
            var upper_16: @Vector(16, u8) = undefined;

            comptime var i = 0;
            inline while (i < 16) : (i += 1) {
                lower_16[i] = byte2hex(input_chunk_4[i]);
                upper_16[i] = byte2hex(input_chunk_15[i]);
            }

            const output_chunk = std.simd.interlace(.{
                lower_16,
                upper_16,
            });

            remaining_dest[0..32].* = @bitCast(output_chunk);
            remaining_dest = remaining_dest[32..];
            remaining = remaining[16..];
        }
    }

    for (remaining) |c| {
        const charset = "0123456789abcdef";

        const buf: [2]u8 = .{ charset[c >> 4], charset[c & 15] };
        remaining_dest[0..2].* = buf;
        remaining_dest = remaining_dest[2..];
    }

    return to_read * 2;
}

/// Leave a single leading char
/// ```zig
/// trimSubsequentLeadingChars("foo\n\n\n\n", '\n') -> "foo\n"
/// ```
pub fn trimSubsequentLeadingChars(slice: []const u8, char: u8) []const u8 {
    if (slice.len == 0) return slice;
    var end = slice.len - 1;
    var endend = slice.len;
    while (end > 0 and slice[end] == char) : (end -= 1) {
        endend = end + 1;
    }
    return slice[0..endend];
}

pub fn trimLeadingChar(slice: []const u8, char: u8) []const u8 {
    if (indexOfNotChar(slice, char)) |i| {
        return slice[i..];
    }
    return "";
}

/// Get the line number and the byte offsets of `line_range_count` above the desired line number
/// The final element is the end index of the desired line
const LineRange = struct {
    start: u32,
    end: u32,
};
pub fn indexOfLineRanges(text: []const u8, target_line: u32, comptime line_range_count: usize) std.BoundedArray(LineRange, line_range_count) {
    const remaining = text;
    if (remaining.len == 0) return .{};

    var ranges = std.BoundedArray(LineRange, line_range_count){};

    var current_line: u32 = 0;
    const first_newline_or_nonascii_i = strings.indexOfNewlineOrNonASCIICheckStart(text, 0, true) orelse {
        if (target_line == 0) {
            ranges.appendAssumeCapacity(.{
                .start = 0,
                .end = @truncate(text.len),
            });
        }

        return ranges;
    };

    var iter = CodepointIterator.initOffset(text, 0);
    var cursor = CodepointIterator.Cursor{
        .i = first_newline_or_nonascii_i,
    };
    const first_newline_range: LineRange = brk: {
        while (iter.next(&cursor)) {
            const codepoint = cursor.c;
            switch (codepoint) {
                '\n' => {
                    current_line += 1;
                    break :brk .{
                        .start = 0,
                        .end = cursor.i,
                    };
                },
                '\r' => {
                    if (iter.next(&cursor)) {
                        const codepoint2 = cursor.c;
                        if (codepoint2 == '\n') {
                            current_line += 1;
                            break :brk .{
                                .start = 0,
                                .end = cursor.i,
                            };
                        }
                    }
                },
                else => {},
            }
        }

        ranges.appendAssumeCapacity(.{
            .start = 0,
            .end = @truncate(text.len),
        });
        return ranges;
    };

    ranges.appendAssumeCapacity(first_newline_range);

    if (target_line == 0) {
        return ranges;
    }

    var prev_end = first_newline_range.end;
    while (strings.indexOfNewlineOrNonASCIICheckStart(text, cursor.i + @as(u32, cursor.width), true)) |current_i| {
        cursor.i = current_i;
        cursor.width = 0;
        const current_line_range: LineRange = brk: {
            if (iter.next(&cursor)) {
                const codepoint = cursor.c;
                switch (codepoint) {
                    '\n' => {
                        const start = prev_end;
                        prev_end = cursor.i;
                        break :brk .{
                            .start = start,
                            .end = cursor.i + 1,
                        };
                    },
                    '\r' => {
                        const current_end = cursor.i;
                        if (iter.next(&cursor)) {
                            const codepoint2 = cursor.c;
                            if (codepoint2 == '\n') {
                                defer prev_end = cursor.i;
                                break :brk .{
                                    .start = prev_end,
                                    .end = current_end,
                                };
                            }
                        }
                    },
                    else => continue,
                }
            }
        };

        if (ranges.len == line_range_count and current_line <= target_line) {
            var new_ranges = std.BoundedArray(LineRange, line_range_count){};
            new_ranges.appendSliceAssumeCapacity(ranges.slice()[1..]);
            ranges = new_ranges;
        }
        ranges.appendAssumeCapacity(current_line_range);

        if (current_line >= target_line) {
            return ranges;
        }

        current_line += 1;
    }

    if (ranges.len == line_range_count and current_line <= target_line) {
        var new_ranges = std.BoundedArray(LineRange, line_range_count){};
        new_ranges.appendSliceAssumeCapacity(ranges.slice()[1..]);
        ranges = new_ranges;
    }

    return ranges;
}

/// Get N lines from the start of the text
pub fn getLinesInText(text: []const u8, line: u32, comptime line_range_count: usize) ?std.BoundedArray([]const u8, line_range_count) {
    const ranges = indexOfLineRanges(text, line, line_range_count);
    if (ranges.len == 0) return null;
    var results = std.BoundedArray([]const u8, line_range_count){};
    results.len = ranges.len;

    for (results.slice()[0..ranges.len], ranges.slice()) |*chunk, range| {
        chunk.* = text[range.start..range.end];
    }

    std.mem.reverse([]const u8, results.slice());

    return results;
}

pub fn firstNonASCII16(comptime Slice: type, slice: Slice) ?u32 {
    var remaining = slice;
    const remaining_start = remaining.ptr;

    if (Environment.enableSIMD and Environment.isNative) {
        const end_ptr = remaining.ptr + remaining.len - (remaining.len % ascii_u16_vector_size);
        if (remaining.len >= ascii_u16_vector_size) {
            while (remaining.ptr != end_ptr) {
                const vec: AsciiU16Vector = remaining[0..ascii_u16_vector_size].*;
                const max_value = @reduce(.Max, vec);

                if (max_value > 127) {
                    const cmp = vec > max_u16_ascii;
                    const bitmask: u8 = @as(u8, @bitCast(cmp));
                    const index_of_first_nonascii_in_vector = @ctz(bitmask);

                    const offset_of_vector_in_input = (@intFromPtr(remaining.ptr) - @intFromPtr(remaining_start)) / 2;
                    const out: u32 = @intCast(offset_of_vector_in_input + index_of_first_nonascii_in_vector);

                    return out;
                }

                remaining.ptr += ascii_u16_vector_size;
            }
            remaining.len -= (@intFromPtr(remaining.ptr) - @intFromPtr(remaining_start)) / 2;
        }
    }

    var i: usize = (@intFromPtr(remaining.ptr) - @intFromPtr(remaining_start)) / 2;

    for (remaining) |char| {
        if (char > 127) {
            return @truncate(i);
        }
        i += 1;
    }

    return null;
}

/// Fast path for printing template literal strings
pub fn @"nextUTF16NonASCIIOr$`\\"(
    comptime Slice: type,
    slice: Slice,
) ?u32 {
    var remaining = slice;

    if (comptime Environment.enableSIMD and Environment.isNative) {
        while (remaining.len >= ascii_u16_vector_size) {
            const vec: AsciiU16Vector = remaining[0..ascii_u16_vector_size].*;

            const cmp = @as(AsciiVectorU16U1, @bitCast((vec > max_u16_ascii))) |
                @as(AsciiVectorU16U1, @bitCast((vec < min_u16_ascii))) |
                @as(AsciiVectorU16U1, @bitCast((vec == @as(AsciiU16Vector, @splat(@as(u16, '$')))))) |
                @as(AsciiVectorU16U1, @bitCast((vec == @as(AsciiU16Vector, @splat(@as(u16, '`')))))) |
                @as(AsciiVectorU16U1, @bitCast((vec == @as(AsciiU16Vector, @splat(@as(u16, '\\'))))));

            const bitmask = @as(u8, @bitCast(cmp));
            const first = @ctz(bitmask);
            if (first < ascii_u16_vector_size) {
                return @as(u32, @intCast(@as(u32, first) +
                    @as(u32, @intCast(slice.len - remaining.len))));
            }

            remaining = remaining[ascii_u16_vector_size..];
        }
    }

    for (remaining, 0..) |char, i| {
        switch (char) {
            '$', '`', '\\', 0...0x20 - 1, 128...std.math.maxInt(u16) => {
                return @as(u32, @truncate(i + (slice.len - remaining.len)));
            },

            else => {},
        }
    }

    return null;
}

/// Convert potentially ill-formed UTF-8 or UTF-16 bytes to a Unicode Codepoint.
/// - Invalid codepoints are replaced with `zero` parameter
/// - Null bytes return 0
pub fn decodeWTF8RuneT(p: *const [4]u8, len: u3, comptime T: type, comptime zero: T) T {
    if (len == 0) return zero;
    if (len == 1) return p[0];

    return decodeWTF8RuneTMultibyte(p, len, T, zero);
}

pub fn codepointSize(comptime R: type, r: R) u3 {
    return switch (r) {
        0b0000_0000...0b0111_1111 => 1,
        0b1100_0000...0b1101_1111 => 2,
        0b1110_0000...0b1110_1111 => 3,
        0b1111_0000...0b1111_0111 => 4,
        else => 0,
    };
}

// /// Encode Type into UTF-8 bytes.
// /// - Invalid unicode data becomes U+FFFD REPLACEMENT CHARACTER.
// /// -
// pub fn encodeUTF8RuneT(out: *[4]u8, comptime R: type, c: R) u3 {
//     switch (c) {
//         0b0000_0000...0b0111_1111 => {
//             out[0] = @intCast(u8, c);
//             return 1;
//         },
//         0b1100_0000...0b1101_1111 => {
//             out[0] = @truncate(u8, 0b11000000 | (c >> 6));
//             out[1] = @truncate(u8, 0b10000000 | c & 0b111111);
//             return 2;
//         },

//         0b1110_0000...0b1110_1111 => {
//             if (0xd800 <= c and c <= 0xdfff) {
//                 // Replacement character
//                 out[0..3].* = [_]u8{ 0xEF, 0xBF, 0xBD };

//                 return 3;
//             }

//             out[0] = @truncate(u8, 0b11100000 | (c >> 12));
//             out[1] = @truncate(u8, 0b10000000 | (c >> 6) & 0b111111);
//             out[2] = @truncate(u8, 0b10000000 | c & 0b111111);
//             return 3;
//         },
//         0b1111_0000...0b1111_0111 => {
//             out[0] = @truncate(u8, 0b11110000 | (c >> 18));
//             out[1] = @truncate(u8, 0b10000000 | (c >> 12) & 0b111111);
//             out[2] = @truncate(u8, 0b10000000 | (c >> 6) & 0b111111);
//             out[3] = @truncate(u8, 0b10000000 | c & 0b111111);
//             return 4;
//         },
//         else => {
//             // Replacement character
//             out[0..3].* = [_]u8{ 0xEF, 0xBF, 0xBD };

//             return 3;
//         },
//     }
// }

pub fn containsNonBmpCodePoint(text: string) bool {
    var iter = CodepointIterator.init(text);
    var curs = CodepointIterator.Cursor{};

    while (iter.next(&curs)) {
        if (curs.c > 0xFFFF) {
            return true;
        }
    }

    return false;
}

// this is std.mem.trim except it doesn't forcibly change the slice to be const
pub fn trim(slice: anytype, comptime values_to_strip: []const u8) @TypeOf(slice) {
    var begin: usize = 0;
    var end: usize = slice.len;

    while (begin < end and std.mem.indexOfScalar(u8, values_to_strip, slice[begin]) != null) : (begin += 1) {}
    while (end > begin and std.mem.indexOfScalar(u8, values_to_strip, slice[end - 1]) != null) : (end -= 1) {}
    return slice[begin..end];
}

pub const whitespace_chars = [_]u8{ ' ', '\t', '\n', '\r', std.ascii.control_code.vt, std.ascii.control_code.ff };

pub fn lengthOfLeadingWhitespaceASCII(slice: string) usize {
    brk: for (slice) |*c| {
        inline for (whitespace_chars) |wc| if (c.* == wc) continue :brk;
        return @intFromPtr(c) - @intFromPtr(slice.ptr);
    }

    return slice.len;
}

pub fn containsNonBmpCodePointUTF16(_text: []const u16) bool {
    const n = _text.len;
    if (n > 0) {
        var i: usize = 0;
        const text = _text[0 .. n - 1];
        while (i < n - 1) : (i += 1) {
            switch (text[i]) {
                // Check for a high surrogate
                0xD800...0xDBFF => {
                    // Check for a low surrogate
                    switch (text[i + 1]) {
                        0xDC00...0xDFFF => {
                            return true;
                        },
                        else => {},
                    }
                },
                else => {},
            }
        }
    }

    return false;
}

pub fn join(slices: []const string, delimiter: string, allocator: std.mem.Allocator) !string {
    return try std.mem.join(allocator, delimiter, slices);
}

pub fn order(a: []const u8, b: []const u8) std.math.Order {
    // const len = @min(a.len, b.len);

    // const cmp = if (comptime Environment.isNative) C.memcmp(a.ptr, b.ptr, len) else return std.mem.order(u8, a, b);
    // return switch (std.math.sign(cmp)) {
    //     0 => std.math.order(a.len, b.len),
    //     1 => .gt,
    //     -1 => .lt,
    //     else => unreachable,
    // };
    return std.mem.order(u8, a, b);
}

pub fn cmpStringsAsc(_: void, a: string, b: string) bool {
    return order(a, b) == .lt;
}

pub fn cmpStringsDesc(_: void, a: string, b: string) bool {
    return order(a, b) == .gt;
}

const sort_asc = std.sort.asc(u8);
const sort_desc = std.sort.desc(u8);

pub fn sortAsc(in: []string) void {
    // TODO: experiment with simd to see if it's faster
    std.sort.pdq([]const u8, in, {}, cmpStringsAsc);
}

pub fn sortDesc(in: []string) void {
    // TODO: experiment with simd to see if it's faster
    std.sort.pdq([]const u8, in, {}, cmpStringsDesc);
}

pub const StringArrayByIndexSorter = struct {
    keys: []const []const u8,
    pub fn lessThan(sorter: *const @This(), a: usize, b: usize) bool {
        return strings.order(sorter.keys[a], sorter.keys[b]) == .lt;
    }

    pub fn init(keys: []const []const u8) @This() {
        return .{
            .keys = keys,
        };
    }
};

pub fn isASCIIHexDigit(c: u8) bool {
    return std.ascii.isHex(c);
}

pub fn toASCIIHexValue(character: u8) u8 {
    if (comptime Environment.allow_assert) assert(isASCIIHexDigit(character));
    return switch (character) {
        0...('A' - 1) => character - '0',
        else => (character - 'A' + 10) & 0xF,
    };
}

pub inline fn utf8ByteSequenceLength(first_byte: u8) u3 {
    return switch (first_byte) {
        0b0000_0000...0b0111_1111 => 1,
        0b1100_0000...0b1101_1111 => 2,
        0b1110_0000...0b1110_1111 => 3,
        0b1111_0000...0b1111_0111 => 4,
        else => 0,
    };
}

pub const PackedCodepointIterator = struct {
    const Iterator = @This();
    const CodePointType = u32;
    const zeroValue = 0;

    bytes: []const u8,
    i: usize,
    next_width: usize = 0,
    width: u3 = 0,
    c: CodePointType = zeroValue,

    pub const ZeroValue = zeroValue;

    pub const Cursor = packed struct {
        i: u32 = 0,
        c: u29 = zeroValue,
        width: u3 = 0,
        pub const CodePointType = u29;
    };

    pub fn init(str: string) Iterator {
        return Iterator{ .bytes = str, .i = 0, .c = zeroValue };
    }

    pub fn initOffset(str: string, i: usize) Iterator {
        return Iterator{ .bytes = str, .i = i, .c = zeroValue };
    }

    pub inline fn next(it: *const Iterator, cursor: *Cursor) bool {
        const pos: u32 = @as(u32, cursor.width) + cursor.i;
        if (pos >= it.bytes.len) {
            return false;
        }

        const cp_len = wtf8ByteSequenceLength(it.bytes[pos]);
        const error_char = comptime std.math.minInt(CodePointType);

        const codepoint = @as(
            CodePointType,
            switch (cp_len) {
                0 => return false,
                1 => it.bytes[pos],
                else => decodeWTF8RuneTMultibyte(it.bytes[pos..].ptr[0..4], cp_len, CodePointType, error_char),
            },
        );

        {
            @setRuntimeSafety(false);
            cursor.* = Cursor{
                .i = pos,
                .c = if (error_char != codepoint)
                    @truncate(codepoint)
                else
                    unicode_replacement,
                .width = if (codepoint != error_char) cp_len else 1,
            };
        }

        return true;
    }

    inline fn nextCodepointSlice(it: *Iterator) []const u8 {
        const bytes = it.bytes;
        const prev = it.i;
        const next_ = prev + it.next_width;
        if (bytes.len <= next_) return "";

        const cp_len = utf8ByteSequenceLength(bytes[next_]);
        it.next_width = cp_len;
        it.i = @min(next_, bytes.len);

        const slice = bytes[prev..][0..cp_len];
        it.width = @as(u3, @intCast(slice.len));
        return slice;
    }

    pub fn needsUTF8Decoding(slice: string) bool {
        var it = Iterator{ .bytes = slice, .i = 0 };

        while (true) {
            const part = it.nextCodepointSlice();
            @setRuntimeSafety(false);
            switch (part.len) {
                0 => return false,
                1 => continue,
                else => return true,
            }
        }
    }

    pub fn scanUntilQuotedValueOrEOF(iter: *Iterator, comptime quote: CodePointType) usize {
        while (iter.c > -1) {
            if (!switch (iter.nextCodepoint()) {
                quote => false,
                '\\' => brk: {
                    if (iter.nextCodepoint() == quote) {
                        continue;
                    }
                    break :brk true;
                },
                else => true,
            }) {
                return iter.i + 1;
            }
        }

        return iter.i;
    }

    pub fn nextCodepoint(it: *Iterator) CodePointType {
        const slice = it.nextCodepointSlice();

        it.c = switch (slice.len) {
            0 => zeroValue,
            1 => @as(CodePointType, @intCast(slice[0])),
            2 => @as(CodePointType, @intCast(std.unicode.utf8Decode2(slice) catch unreachable)),
            3 => @as(CodePointType, @intCast(std.unicode.utf8Decode3(slice) catch unreachable)),
            4 => @as(CodePointType, @intCast(std.unicode.utf8Decode4(slice) catch unreachable)),
            else => unreachable,
        };

        return it.c;
    }

    /// Look ahead at the next n codepoints without advancing the iterator.
    /// If fewer than n codepoints are available, then return the remainder of the string.
    pub fn peek(it: *Iterator, n: usize) []const u8 {
        const original_i = it.i;
        defer it.i = original_i;

        var end_ix = original_i;
        var found: usize = 0;
        while (found < n) : (found += 1) {
            const next_codepoint = it.nextCodepointSlice() orelse return it.bytes[original_i..];
            end_ix += next_codepoint.len;
        }

        return it.bytes[original_i..end_ix];
    }
};

pub fn NewCodePointIterator(comptime CodePointType: type, comptime zeroValue: comptime_int) type {
    return struct {
        const Iterator = @This();
        bytes: []const u8,
        i: usize,
        next_width: usize = 0,
        width: u3 = 0,
        c: CodePointType = zeroValue,

        pub const ZeroValue = zeroValue;

        pub const Cursor = struct {
            i: u32 = 0,
            c: CodePointType = zeroValue,
            width: u3 = 0,
        };

        pub fn init(str: string) Iterator {
            return Iterator{ .bytes = str, .i = 0, .c = zeroValue };
        }

        pub fn initOffset(str: string, i: usize) Iterator {
            return Iterator{ .bytes = str, .i = i, .c = zeroValue };
        }

        pub inline fn next(it: *const Iterator, cursor: *Cursor) bool {
            const pos: u32 = @as(u32, cursor.width) + cursor.i;
            if (pos >= it.bytes.len) {
                return false;
            }

            const cp_len = wtf8ByteSequenceLength(it.bytes[pos]);
            const error_char = comptime std.math.minInt(CodePointType);

            const codepoint = @as(
                CodePointType,
                switch (cp_len) {
                    0 => return false,
                    1 => it.bytes[pos],
                    else => decodeWTF8RuneTMultibyte(it.bytes[pos..].ptr[0..4], cp_len, CodePointType, error_char),
                },
            );

            cursor.* = Cursor{
                .i = pos,
                .c = if (error_char != codepoint)
                    codepoint
                else
                    unicode_replacement,
                .width = if (codepoint != error_char) cp_len else 1,
            };

            return true;
        }

        inline fn nextCodepointSlice(it: *Iterator) []const u8 {
            const bytes = it.bytes;
            const prev = it.i;
            const next_ = prev + it.next_width;
            if (bytes.len <= next_) return "";

            const cp_len = utf8ByteSequenceLength(bytes[next_]);
            it.next_width = cp_len;
            it.i = @min(next_, bytes.len);

            const slice = bytes[prev..][0..cp_len];
            it.width = @as(u3, @intCast(slice.len));
            return slice;
        }

        pub fn needsUTF8Decoding(slice: string) bool {
            var it = Iterator{ .bytes = slice, .i = 0 };

            while (true) {
                const part = it.nextCodepointSlice();
                @setRuntimeSafety(false);
                switch (part.len) {
                    0 => return false,
                    1 => continue,
                    else => return true,
                }
            }
        }

        pub fn scanUntilQuotedValueOrEOF(iter: *Iterator, comptime quote: CodePointType) usize {
            while (iter.c > -1) {
                if (!switch (iter.nextCodepoint()) {
                    quote => false,
                    '\\' => brk: {
                        if (iter.nextCodepoint() == quote) {
                            continue;
                        }
                        break :brk true;
                    },
                    else => true,
                }) {
                    return iter.i + 1;
                }
            }

            return iter.i;
        }

        pub fn nextCodepoint(it: *Iterator) CodePointType {
            const slice = it.nextCodepointSlice();

            it.c = switch (slice.len) {
                0 => zeroValue,
                1 => @as(CodePointType, @intCast(slice[0])),
                2 => @as(CodePointType, @intCast(std.unicode.utf8Decode2(slice) catch unreachable)),
                3 => @as(CodePointType, @intCast(std.unicode.utf8Decode3(slice) catch unreachable)),
                4 => @as(CodePointType, @intCast(std.unicode.utf8Decode4(slice) catch unreachable)),
                else => unreachable,
            };

            return it.c;
        }

        /// Look ahead at the next n codepoints without advancing the iterator.
        /// If fewer than n codepoints are available, then return the remainder of the string.
        pub fn peek(it: *Iterator, n: usize) []const u8 {
            const original_i = it.i;
            defer it.i = original_i;

            var end_ix = original_i;
            for (0..n) |_| {
                const next_codepoint = it.nextCodepointSlice() orelse return it.bytes[original_i..];
                end_ix += next_codepoint.len;
            }

            return it.bytes[original_i..end_ix];
        }
    };
}

pub const CodepointIterator = NewCodePointIterator(CodePoint, -1);
pub const UnsignedCodepointIterator = NewCodePointIterator(u32, 0);

pub fn NewLengthSorter(comptime Type: type, comptime field: string) type {
    return struct {
        const LengthSorter = @This();
        pub fn lessThan(_: LengthSorter, lhs: Type, rhs: Type) bool {
            return @field(lhs, field).len < @field(rhs, field).len;
        }
    };
}

pub fn NewGlobLengthSorter(comptime Type: type, comptime field: string) type {
    return struct {
        const GlobLengthSorter = @This();
        pub fn lessThan(_: GlobLengthSorter, lhs: Type, rhs: Type) bool {
            // Assert: keyA ends with "/" or contains only a single "*".
            // Assert: keyB ends with "/" or contains only a single "*".
            const key_a = @field(lhs, field);
            const key_b = @field(rhs, field);

            // Let baseLengthA be the index of "*" in keyA plus one, if keyA contains "*", or the length of keyA otherwise.
            // Let baseLengthB be the index of "*" in keyB plus one, if keyB contains "*", or the length of keyB otherwise.
            const star_a = indexOfChar(key_a, '*');
            const star_b = indexOfChar(key_b, '*');
            const base_length_a = star_a orelse key_a.len;
            const base_length_b = star_b orelse key_b.len;

            // If baseLengthA is greater than baseLengthB, return -1.
            // If baseLengthB is greater than baseLengthA, return 1.
            if (base_length_a > base_length_b)
                return true;
            if (base_length_b > base_length_a)
                return false;

            // If keyA does not contain "*", return 1.
            // If keyB does not contain "*", return -1.
            if (star_a == null)
                return false;
            if (star_b == null)
                return true;

            // If the length of keyA is greater than the length of keyB, return -1.
            // If the length of keyB is greater than the length of keyA, return 1.
            if (key_a.len > key_b.len)
                return true;
            if (key_b.len > key_a.len)
                return false;

            return false;
        }
    };
}

pub const unicode_replacement = 0xFFFD;
pub const unicode_replacement_str = brk: {
    var out: [std.unicode.utf8CodepointSequenceLength(unicode_replacement) catch unreachable]u8 = undefined;
    _ = std.unicode.utf8Encode(unicode_replacement, &out) catch unreachable;
    break :brk out;
};

pub fn leftHasAnyInRight(to_check: []const string, against: []const string) bool {
    for (to_check) |check| {
        for (against) |item| {
            if (eqlLong(check, item, true)) return true;
        }
    }
    return false;
}

pub fn hasPrefixWithWordBoundary(input: []const u8, comptime prefix: []const u8) bool {
    if (hasPrefixComptime(input, prefix)) {
        if (input.len == prefix.len) return true;

        const next = input[prefix.len..];
        var bytes: [4]u8 = .{
            next[0],
            if (next.len > 1) next[1] else 0,
            if (next.len > 2) next[2] else 0,
            if (next.len > 3) next[3] else 0,
        };

        if (!js_lexer.isIdentifierContinue(decodeWTF8RuneT(&bytes, wtf8ByteSequenceLength(next[0]), i32, -1))) {
            return true;
        }
    }

    return false;
}

pub inline fn charIsAnySlash(char: u8) bool {
    return char == '/' or char == '\\';
}

pub fn isZeroWidthCodepointType(comptime T: type, cp: T) bool {
    if (cp <= 0x1f) {
        return true;
    }

    if (cp >= 0x7f and cp <= 0x9f) {
        // C1 control characters
        return true;
    }

    if (comptime @sizeOf(T) == 1) {
        return false;
    }

    if (cp >= 0x300 and cp <= 0x36f) {
        // Combining Diacritical Marks
        return true;
    }
    if (cp >= 0x300 and cp <= 0x36f)
        // Combining Diacritical Marks
        return true;

    if (cp >= 0x200b and cp <= 0x200f) {
        // Modifying Invisible Characters
        return true;
    }

    if (cp >= 0x20d0 and cp <= 0x20ff)
        // Combining Diacritical Marks for Symbols
        return true;

    if (cp >= 0xfe00 and cp <= 0xfe0f)
        // Variation Selectors
        return true;
    if (cp >= 0xfe20 and cp <= 0xfe2f)
        // Combining Half Marks
        return true;

    if (cp == 0xfeff)
        // Zero Width No-Break Space (BOM, ZWNBSP)
        return true;

    if (cp >= 0xe0100 and cp <= 0xe01ef)
        // Variation Selectors
        return true;

    return false;
}

/// Generic. Works on []const u8, []const u16, etc
pub inline fn indexOfScalar(input: anytype, scalar: std.meta.Child(@TypeOf(input))) ?usize {
    if (comptime std.meta.Child(@TypeOf(input)) == u8) {
        return strings.indexOfCharUsize(input, scalar);
    } else {
        return std.mem.indexOfScalar(std.meta.Child(@TypeOf(input)), input, scalar);
    }
}

/// Generic. Works on []const u8, []const u16, etc
pub fn containsScalar(input: anytype, item: std.meta.Child(@TypeOf(input))) bool {
    return indexOfScalar(input, item) != null;
}

pub fn withoutSuffixComptime(input: []const u8, comptime suffix: []const u8) []const u8 {
    if (hasSuffixComptime(input, suffix)) {
        return input[0 .. input.len - suffix.len];
    }
    return input;
}

pub fn withoutPrefixComptime(input: []const u8, comptime prefix: []const u8) []const u8 {
    if (hasPrefixComptime(input, prefix)) {
        return input[prefix.len..];
    }
    return input;
}

pub fn withoutPrefixIfPossibleComptime(input: string, comptime prefix: string) ?string {
    if (hasPrefixComptime(input, prefix)) {
        return input[prefix.len..];
    }
    return null;
}

const Allocator = std.heap.GeneralPurposeAllocator(.{});
var gpa = Allocator{};

pub inline fn getAllocator() std.mem.Allocator {
    if (comptime @import("builtin").os.tag == .linux) {
        return gpa.allocator();
    }
    return std.heap.c_allocator;
}
