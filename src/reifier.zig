const std = @import("std");
const parser = @import("./parser.zig");
const program = @import("./program.zig");

const Kind = program.Analyzer.Kind;
const TypeRef =  program.Analyzer.TypeRef;

const getSlice2 = program.Analyzer.getSlice2;

pub const Reifier = struct {
    analyzer: *program.Analyzer,
    allocator: std.mem.Allocator,
    out: std.ArrayListUnmanaged(u8) = .{},
    cached: std.AutoHashMapUnmanaged(TypeRef, void) = .{},

    pub fn init(allocator: std.mem.Allocator, analyzer: *program.Analyzer) @This() {
        return .{
            .allocator = allocator,
            .analyzer = analyzer,
        };
    }

    fn raw(this: *@This(), s: []const u8) !void {
        try this.out.appendSlice(this.allocator, s);
    }

    fn print(this: *@This(), comptime fmt: []const u8, args: anytype) !void {
        try this.out.writer(this.allocator).print(fmt, args);
    }

    fn string(this: *@This(), s: []const u8) !void {
        try std.json.encodeJsonString(s, .{}, this.out.writer(this.allocator));
    }

    fn number(this: *@This(), v: f64) !void {
        if (std.math.isNan(v)) return this.raw("[\"d\",\"NaN\"]");
        if (std.math.isInf(v)) return this.raw(if (v > 0) "[\"d\",\"Infinity\"]" else "[\"d\",\"-Infinity\"]");
        try this.print("{d}", .{v});
    }

    fn intrinsic(this: *@This(), comptime name: []const u8) !void {
        try this.raw("[\"i\",\"" ++ name ++ "\"]");
    }

    fn writeTuple(this: *@This(), types: []const TypeRef) !void {
        try this.raw("[\"T\",[");
        for (types, 0..) |u, i| {
            if (i > 0) try this.raw(",");
            if (u < @intFromEnum(Kind.false)) {
                const t2 = this.analyzer.types.at(u);
                if (t2.getKind() == .tuple_element) {
                    try this.write(t2.slot1);
                    continue;
                }
            }
            try this.write(u);
        }
        try this.raw("]]");
    }

    fn writePrimitive(this: *@This(), ty: TypeRef) !void {
        if (ty == @intFromEnum(Kind.false)) return this.raw("false");
        if (ty == @intFromEnum(Kind.true)) return this.raw("true");
        if (ty == @intFromEnum(Kind.undefined)) return this.raw("[\"u\"]");
        if (ty == @intFromEnum(Kind.null)) return this.raw("null");
        if (ty == @intFromEnum(Kind.void)) return this.intrinsic("Void");
        if (ty == @intFromEnum(Kind.any)) return this.intrinsic("any");
        if (ty == @intFromEnum(Kind.never)) return this.intrinsic("never");
        if (ty == @intFromEnum(Kind.unknown)) return this.intrinsic("unknown");
        if (ty == @intFromEnum(Kind.string)) return this.intrinsic("string");
        if (ty == @intFromEnum(Kind.number)) return this.intrinsic("number");
        if (ty == @intFromEnum(Kind.boolean)) return this.intrinsic("boolean");
        if (ty == @intFromEnum(Kind.object)) return this.intrinsic("object");
        if (ty == @intFromEnum(Kind.symbol)) return this.intrinsic("symbol");
        if (ty == @intFromEnum(Kind.empty_string)) return this.raw("\"\"");
        if (ty == @intFromEnum(Kind.empty_object)) return this.raw("[\"O\",null,[],[]]");
        if (ty == @intFromEnum(Kind.empty_tuple)) return this.raw("[\"t\"]");
        if (ty >= @intFromEnum(Kind.zero)) return this.number(this.analyzer.getDoubleFromType(ty));

        this.analyzer.printTypeInfo(ty);
        return error.TODO_unhandled_primitve_type;
    }

    fn write(this: *@This(), ty: TypeRef) anyerror!void {
        if (this.analyzer.isParameterizedRef(ty)) {
            this.analyzer.printTypeInfo(ty);
            return error.TODO_parameterized;
        }

        if (ty >= @intFromEnum(Kind.false)) return this.writePrimitive(ty);

        const t = this.analyzer.types.at(ty);
        switch (t.getKind()) {
            .alias => {
                if (this.cached.contains(ty)) return this.print("[\"c\",{d}]", .{ty});

                const followed = try this.analyzer.evaluateType(ty, 1 << 0);
                if (ty == followed) {
                    this.analyzer.printTypeInfo(ty);
                    return error.RecursiveAlias;
                }

                try this.print("[\"a\",{d},", .{ty});
                try this.write(followed);
                try this.raw("]");
                try this.cached.put(this.allocator, ty, {});
            },
            .conditional, .indexed, .keyof, .query, .mapped, .intersection => {
                const followed = try this.analyzer.evaluateType(ty, 1 << 0 | 1 << 30);
                if (ty == followed) {
                    this.analyzer.printTypeInfo(ty);
                    return error.Recursive;
                }

                try this.write(followed);
            },
            .array => {
                try this.raw("[\"A\",");
                try this.write(t.slot0);
                try this.raw("]");
            },
            .tuple => try this.writeTuple(getSlice2(t, TypeRef)),
            .@"union" => {
                try this.raw("[\"U\",[");
                for (getSlice2(t, TypeRef), 0..) |u, i| {
                    if (i > 0) try this.raw(",");
                    try this.write(u);
                }
                try this.raw("]]");
            },
            .string_literal => try this.string(this.analyzer.getSliceFromLiteral(ty)),
            .number_literal => try this.number(this.analyzer.getDoubleFromType(ty)),
            .object_literal => {
                if (this.cached.contains(ty)) return this.print("[\"c\",{d}]", .{ty});
                try this.cached.put(this.allocator, ty, {});

                try this.print("[\"O\",{d},[", .{ty});
                if (t.slot3 != 0) try this.write(t.slot3);
                try this.raw("],[");

                var first = true;
                for (getSlice2(t, program.Analyzer.ObjectLiteralMember)) |*u| {
                    if (u.kind != .property) continue;
                    if (!first) try this.raw(",");
                    first = false;
                    try this.raw("[");
                    try this.write(u.name);
                    try this.raw(",");
                    try this.write(try u.getType(this.analyzer));
                    try this.raw("]");
                }
                try this.raw("]]");
            },
            .function_literal => {
                if (this.cached.contains(ty)) return this.print("[\"c\",{d}]", .{ty});
                try this.cached.put(this.allocator, ty, {});

                try this.print("[\"F\",{d},", .{ty});
                try this.writeTuple(getSlice2(t, TypeRef));
                if (t.slot3 != @intFromEnum(Kind.void)) {
                    try this.raw(",");
                    try this.write(t.slot3);
                }
                try this.raw("]");
            },
            .machine_data_type => try this.print("[\"m\",{d},{d},{d}]", .{ ty, t.slot0, t.slot1 }),
            else => {
                this.analyzer.printTypeInfo(ty);
                return error.TODO_unhandled_allocated_type;
            },
        }
    }

    fn finish(this: *@This()) ![:0]u8 {
        defer {
            this.out.clearRetainingCapacity();
            this.cached.clearRetainingCapacity();
        }
        return try this.allocator.dupeZ(u8, this.out.items);
    }

    pub fn reifyExpression(this: *@This(), f: *program.ParsedFileData, node_ref: parser.NodeRef, type_params: parser.NodeRef) ![:0]u8 {
        const ty = try this.analyzer.getType(f, node_ref);
        if (type_params != 0) {
            const ty2 = try this.analyzer.createParameterizedTypeFromParams(f, type_params, ty);
            try this.print("[\"f\",{d}]", .{ty2});
        } else {
            try this.write(ty);
        }
        return this.finish();
    }

    pub fn evaluateTypeFunction(this: *@This(), inner: TypeRef, args: []TypeRef) ![:0]u8 {
        const ty = try this.analyzer.resolveWithTypeArgsSlice(this.analyzer.types.at(inner), args);
        try this.write(ty);
        return this.finish();
    }
};
