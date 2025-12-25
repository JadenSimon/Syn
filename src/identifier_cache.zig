const std = @import("std");

fn asByteSlice(buffer: anytype) []const u8 {
    return switch (@TypeOf(buffer)) {
        []const u8, []u8, [:0]const u8, [:0]u8 => buffer.ptr[0..buffer.len],
        [*:0]u8, [*:0]const u8 => buffer[0..std.mem.len(buffer)],

        else => buffer,
    };
}

pub const CachedBitset = extern struct {
    range: [2]i32,
    len: u32,

    // TODO: need to symbolically evaluate any `@embedFile` callsites
    pub fn fromFile(comptime filename: anytype) CachedBitset {
        return comptime @as(CachedBitset, @bitCast(asByteSlice(@embedFile(filename)).ptr[0..@sizeOf(CachedBitset)].*));
    }

    pub fn fromData(comptime data: anytype) CachedBitset {
        return comptime @as(CachedBitset, @bitCast(asByteSlice(data).ptr[0..@sizeOf(CachedBitset)].*));
    }
};

pub fn setMasks(masks: [*:0]const u8, comptime MaskType: type, masky: MaskType) void {
    const FieldInfo: std.builtin.Type.StructField = std.meta.fieldInfo(MaskType, "masks");
    masky.masks = @as(masks, @bitCast(FieldInfo.type));
}

const ident_data = @import("./identifier_data.zig");

pub const id_start_meta = ident_data.id_start_cached; // CachedBitset.fromData(@embedFile("id_start_bitset.meta.blob"));
pub const id_continue_meta = ident_data.id_continue_cached; //CachedBitset.fromData(@embedFile("id_continue_bitset.meta.blob"));
pub const id_start_masks = ident_data.id_start_data; // @embedFile("id_start_bitset.blob");
pub const id_continue_masks = ident_data.id_continue_data; // @embedFile("id_continue_bitset.blob");

pub const IDStartType = std.bit_set.ArrayBitSet(usize, id_start_meta.len);
pub const IDContinueType = std.bit_set.ArrayBitSet(usize, id_continue_meta.len);
pub const id_start = IDStartType{
    .masks = @as(std.meta.fieldInfo(IDStartType, .masks).type, @bitCast(@as(*const [id_start_masks.len]u8, @ptrCast(id_start_masks)).*)),
};
pub const id_continue = IDContinueType{
    .masks = @as(std.meta.fieldInfo(IDContinueType, .masks).type, @bitCast(@as(*const [id_continue_masks.len]u8, @ptrCast(id_continue_masks)).*)),
};
