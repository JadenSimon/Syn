const std = @import("std");
pub const SerializedBitset = extern struct {};

pub const Bitset = struct {
    const Cache = @import("identifier_cache.zig");
    const id_start_range: [2]i32 = Cache.id_start_meta.range;
    const id_end_range: [2]i32 = Cache.id_continue_meta.range;
    const id_start = &Cache.id_start;
    const id_continue = &Cache.id_continue;

    pub fn init() void {}

    pub fn isIdentifierStart(codepoint: i32) bool {
        return codepoint >= (comptime id_start_range[0]) and
            codepoint <= (comptime id_start_range[1]) and
            id_start.isSet((comptime @as(usize, @intCast(id_start_range[1]))) - @as(
            usize,
            @intCast(codepoint),
        ));
    }

    pub fn isIdentifierPart(codepoint: i32) bool {
        return codepoint >= (comptime id_end_range[0]) and
            codepoint <= (comptime id_end_range[1]) and
            id_continue.isSet(
            (comptime @as(usize, @intCast(id_end_range[1]))) - @as(
                usize,
                @intCast(codepoint),
            ),
        );
    }
};
