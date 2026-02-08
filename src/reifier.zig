const js = @import("js");
const std = @import("std");
const parser = @import("./parser.zig");
const program = @import("./program.zig");

const Kind = program.Analyzer.Kind;
const TypeRef =  program.Analyzer.TypeRef;

const getSlice2 = program.Analyzer.getSlice2;

pub const Reifier = struct {    
    env: *js.Env,
    analyzer: *program.Analyzer,
    type_map: std.AutoArrayHashMapUnmanaged(TypeRef, *js.Value) = std.AutoArrayHashMapUnmanaged(TypeRef, *js.Value){},
    allocator: std.mem.Allocator,
    type_module: *js.Ref,


    pub fn init(env: *js.Env, allocator: std.mem.Allocator, checker: *program.Analyzer, type_module: *js.Object) @This() {
        const ref = js.Ref.init(env, @ptrCast(type_module), 1) catch unreachable;
        return .{
            .env = env,
            .allocator = allocator,
            .checker = checker,
            .type_module = ref,
        };
    }

    pub fn initCurrentEnv(allocator: std.mem.Allocator, checker: *program.Analyzer, type_module: *js.Object) @This() {
        return @This().init(js.getCurrentEnv(), allocator, checker, type_module);
    }

    fn createShape(this: *@This(), comptime name: [:0]const u8) !*js.Object {
        const type_module = @as(*js.Object, @ptrCast(try this.type_module.getValue(this.env)));
        const prop = try type_module.getNamedProperty(this.env, name);

        const f: *js.Function = @ptrCast(prop);
        const val = try f.call(.{});

        return @ptrCast(val);
    }

    fn getIntrinsic(this: *@This(), comptime name: [:0]const u8) !*js.Value {
        const type_module = @as(*js.Object, @ptrCast(try this.type_module.getValue(this.env)));
        const prop = try type_module.getNamedProperty(this.env, name);

        return prop;
    }

    fn createSavedShape(this: *@This(), ty: TypeRef, comptime name: [:0]const u8) !*js.Object {
        const val = try this.createShape(name);
       // try this.type_map.put(this.allocator, ty, @ptrCast(val));
       _ = ty;

        return val;
    }

    fn callMethod(this: *@This(), o: *js.Object, comptime name: [:0]const u8, args: anytype) !void {        
        const prop = try o.getNamedProperty(this.env, name);

        const f: *js.Function = @ptrCast(prop);
        _ = try f.callWithThisArg(this.env, o, args);
    }

    fn setField(this: *@This(), o: *js.Object, comptime name: [:0]const u8, v: *js.Value) !void {
     //   const nt_name = try js.String.fromUtf8(this.env, name);
        try o.setNamedProperty(this.env, name, v);
    }

    fn hasCached(this: *@This(), n: TypeRef) !bool {
        const type_module = @as(*js.Object, @ptrCast(try this.type_module.getValue(this.env)));
        const prop = try type_module.getNamedProperty(this.env, "__hasCachedType");

        const f: *js.Function = @ptrCast(prop);
        const val = try f.call(.{n});
        const b: *Boolean = @ptrCast(val);

        return try b.getValue(this.env);
    }

    fn getCached(this: *@This(), n: TypeRef) !*js.Value {
        const type_module = @as(*js.Object, @ptrCast(try this.type_module.getValue(this.env)));
        const prop = try type_module.getNamedProperty(this.env, "__getCachedType");

        const f: *js.Function = @ptrCast(prop);
        const val = try f.call(.{n});

        return val;
    }

    fn setCached(this: *@This(), n: TypeRef, v: *js.Value) !void {
        const type_module = @as(*js.Object, @ptrCast(try this.type_module.getValue(this.env)));
        const prop = try type_module.getNamedProperty(this.env, "__setCachedType");

        const f: *js.Function = @ptrCast(prop);
        _ = try f.call(.{n, v});
    }

    fn reifyType(this: *@This(), ty: program.Analyzer.TypeRef) anyerror!*anyopaque {
        if (this.analyzer.isParameterizedRef(ty)) {
            this.analyzer.printTypeInfo(ty);
            return error.TODO_parameterized;
        }

        if (ty >= @intFromEnum(Kind.false)) {
            if (ty == @intFromEnum(Kind.false)) {
                return try this.env.getBool(false);
            }
            if (ty == @intFromEnum(Kind.true)) {
                return try this.env.getBool(true);
            }
            if (ty == @intFromEnum(Kind.undefined)) {
                return try this.env.getUndefined();
            }
            if (ty == @intFromEnum(Kind.null)) {
                return try this.env.getNull();
            }

            if (ty == @intFromEnum(Kind.void)) {
                return try this.getIntrinsic("Void");
            }

            if (ty == @intFromEnum(Kind.any)) {
                return try this.getIntrinsic("any");
            }
            if (ty == @intFromEnum(Kind.never)) {
                return try this.getIntrinsic("never");
            }
            if (ty == @intFromEnum(Kind.unknown)) {
                return try this.getIntrinsic("unknown");
            }

            if (ty == @intFromEnum(Kind.string)) {
                return try this.getIntrinsic("string");
            }
            if (ty == @intFromEnum(Kind.number)) {
                return try this.getIntrinsic("number");
            }
            if (ty == @intFromEnum(Kind.boolean)) {
                return try this.getIntrinsic("boolean");
            }
            if (ty == @intFromEnum(Kind.object)) {
                return try this.getIntrinsic("object");
            }
            if (ty == @intFromEnum(Kind.symbol)) {
                return try this.getIntrinsic("symbol");
            }

            if (ty == @intFromEnum(Kind.empty_string)) {
                return try js.String.fromUtf8(this.env, "");
            } else if (ty == @intFromEnum(Kind.empty_object)) {
                return try this.createShape("__Object");
            } else if (ty == @intFromEnum(Kind.empty_tuple)) {
                return try this.createShape("__Tuple");
            }

            // empty_element -> undefined ?

            if (ty >= @intFromEnum(Kind.zero)) {
                return try js.Number.createDouble(this.env, this.analyzer.getDoubleFromType(ty));
            }

            this.analyzer.printTypeInfo(ty);
            return error.TODO_unhandled_primitve_type;
        }

        if (this.type_map.get(ty)) |p| {
            return p;
        }

        const t = this.analyzer.types.at(ty);
        switch (t.getKind()) {
            .alias => {
                if (try this.hasCached(ty)) {
                    return try this.getCached(ty);
                }

                const followed = try this.analyzer.evaluateType(ty, 1 << 0);
                if (ty == followed) {
                    this.analyzer.printTypeInfo(ty);
                    return error.RecursiveAlias;
                }

                const result = try this.reifyType(followed);
                try this.setCached(ty, @ptrCast(result));

                return result;
            },
            .conditional, .indexed, .keyof, .query, .mapped, .intersection => {
                const followed = try this.analyzer.evaluateType(ty, 1 << 0 | 1 << 30);
                if (ty == followed) {
                    this.analyzer.printTypeInfo(ty);
                    return error.Recursive;
                }

                return try this.reifyType(followed);
            },
            .array => {
                const o = try this.createSavedShape(ty, "__ArrayType");
                try this.setField(o, "element", @alignCast(@ptrCast(try this.reifyType(t.slot0))));
                return o;
            },
            .tuple => {
                const o = try this.createSavedShape(ty, "__Tuple");
                const types = getSlice2(t, TypeRef);
                for (types) |u| {
                    if (u < @intFromEnum(Kind.false)) {
                        const t2 = this.analyzer.types.at(u);
                        const el_type = t2.slot1;
                        try this.callMethod(o, "add", .{
                            @as(*js.Value, @alignCast(@ptrCast(try this.reifyType(el_type))))
                        });
                        continue;
                    }

                    try this.callMethod(o, "add", .{
                        @as(*js.Value, @alignCast(@ptrCast(try this.reifyType(u))))
                    });
                }
                return o;
            },
            // .tuple_element => {
            //     const o = try this.createSavedShape(ty, "__TupleElement");
            //     const types = getSlice2(t, TypeRef);
            //     for (types) |u| {
            //         try this.callMethod(o, "add", .{
            //             @as(*js.Value, @alignCast(@ptrCast(try this.reifyType(u))))
            //         });
            //     }
            //     return o;
            // },
            .@"union" => {
                const o = try this.createSavedShape(ty, "__Union");
                const types = getSlice2(t, TypeRef);
                for (types) |u| {
                    try this.callMethod(o, "add", .{
                        @as(*js.Value, @alignCast(@ptrCast(try this.reifyType(u))))
                    });
                }
                return o;
            },
            .string_literal => {
                const s = this.analyzer.getSliceFromLiteral(ty);
                return try js.String.fromUtf8(this.env, s);
            },
            .number_literal => {
                return try js.Number.createDouble(this.env, this.analyzer.getDoubleFromType(ty));
            },
            .object_literal => {
                if (try this.hasCached(ty)) {
                    return try this.getCached(ty);
                }

                const o = try this.createSavedShape(ty, "__Object");
                try this.setCached(ty, @ptrCast(o));

                const members = getSlice2(t, program.Analyzer.ObjectLiteralMember);
                for (members) |*u| {
                    if (u.kind != .property) continue; // TODO

                    const name: *js.Value = @alignCast(@ptrCast(try this.reifyType(u.name)));

                    const inner = try u.getType(this.analyzer);
                    const v = @as(*js.Value, @alignCast(@ptrCast(try this.reifyType(inner))));
                    try o.setProperty(this.env, name, v);
                }

                return o;
            },
            else => {},
            // class
            // template_literal
            // function_literal
            // module_namespace
            // symbol_literal (only well-known symbols)
        }

        this.analyzer.printTypeInfo(ty);
        return error.TODO_unhandled_allocated_type;
    }

    pub fn processReifyExpression(this: *@This(), f: *program.ParsedFileData, node_ref: parser.NodeRef, type_params: parser.NodeRef) !*anyopaque {
        // const scope = try HandleScope.init(this.env);
        // defer scope.deinit(this.env) catch {};
        const scope = try EscapableHandleScope.open(this.env);
        defer scope.close(this.env) catch {};

        const ty = try this.analyzer.getType(f, node_ref);

        if (type_params != 0) {
            const ty2 = try this.analyzer.createParameterizedTypeFromParams(f, type_params, ty);

            const type_module = @as(*js.Object, @ptrCast(try this.type_module.getValue(this.env)));
            const prop = try type_module.getNamedProperty(this.env, "__TypeFunction");

            const createTypeFn: *js.Function = @ptrCast(prop);
            const val = try createTypeFn.call(.{ ty2 }); // TODO: compute arity

            try this.setCached(ty2, val);

            return try scope.escape(this.env, val);
        }

        return try scope.escape(this.env, @alignCast(@ptrCast(try this.reifyType(ty))));
    }

    pub fn evaluateTypeFunction(this: *@This(), inner: TypeRef, args: []TypeRef) !*anyopaque {
        const scope = try EscapableHandleScope.open(this.env);
        defer scope.close(this.env) catch {};

        const ty = try this.analyzer.resolveWithTypeArgsSlice(this.analyzer.types.at(inner), args);

        return try scope.escape(this.env, @alignCast(@ptrCast(try this.reifyType(ty))));
    }

    pub fn valueToTypeRef(this: *@This(), val: *js.Value) !TypeRef {
        _ = this;
        _ = val;
        return 0;
    }

    // try js.ArrayPointer.initCapacity(this.env, 0),
};

// const Tag = enum {
//     object,
//     array,
//     tuple,
//     @"union",
//     function,
//     template,
//     type_function,
//     intrinsic,
//     literal,
// };

const EscapableHandleScope = opaque {
    extern fn napi_open_escapable_handle_scope(env: *js.Env, result: **EscapableHandleScope) Status;
    extern fn napi_close_escapable_handle_scope(env: *js.Env, scope: *EscapableHandleScope) Status;
    extern fn napi_escape_handle(env: *js.Env, scope: *EscapableHandleScope, escapee: *js.Value, result: **js.Value) Status;

    pub fn open(env: *js.Env) !*EscapableHandleScope {
        var result: *EscapableHandleScope = undefined;
        const status = napi_open_escapable_handle_scope(env, &result);
        try checkStatus(status);

        return result;
    }

    pub fn close(this: *EscapableHandleScope, env: *js.Env) !void {
        const status = napi_close_escapable_handle_scope(env, this);
        try checkStatus(status);
    }

    pub fn escape(this: *EscapableHandleScope, env: *js.Env, escapee: *js.Value) !*js.Value {
        var result: *js.Value = undefined;
        const status = napi_escape_handle(env, this, escapee, &result);
        try checkStatus(status);

        return result;
    }
};


const Status = enum(u16) {
    napi_ok,
    napi_invalid_arg,
    napi_object_expected,
    napi_string_expected,
    napi_name_expected,
    napi_function_expected,
    napi_number_expected,
    napi_boolean_expected,
    napi_array_expected,
    napi_generic_failure,
    napi_pending_exception,
    napi_cancelled,
    napi_escape_called_twice,
    napi_handle_scope_mismatch,
    napi_callback_scope_mismatch,
    napi_queue_full,
    napi_closing,
    napi_bigint_expected,
    napi_date_expected,
    napi_arraybuffer_expected,
    napi_detachable_arraybuffer_expected,
    napi_would_deadlock,
    napi_no_external_buffers_allowed,
    napi_cannot_run_js,
};

const JSError = error{
    InvalidArg,
    ObjectExpected,
    StringExpected,
    NameExpected,
    FunctionExpected,
    NumberExpected,
    BooleanExpected,
    ArrayExpected,
    GenericFailure,
    PendingException,
    Cancelled,
    EscapeCalledTwice,
    HandleScopeMismatch,
    CallbackScopeMismatch,
    QueueFull,
    Closing,
    BigintExpected,
    DateExpected,
    ArrayBufferExpected,
    DetatchableArrayBufferExpected,
    WouldDeadlock,
    NoExternalArrayBuffersAllowed,
    CannotRunJs,
};

fn checkStatus(status: Status) JSError!void {
    return switch (status) {
        .napi_ok => {},
        .napi_invalid_arg => JSError.InvalidArg,
        .napi_object_expected => JSError.ObjectExpected,
        .napi_string_expected => JSError.StringExpected,
        .napi_name_expected => JSError.NameExpected,
        .napi_function_expected => JSError.FunctionExpected,
        .napi_number_expected => JSError.NumberExpected,
        .napi_boolean_expected => JSError.NameExpected,
        .napi_array_expected => JSError.ArrayExpected,
        .napi_generic_failure => JSError.GenericFailure,
        .napi_pending_exception => JSError.PendingException,
        .napi_cancelled => JSError.Cancelled,
        .napi_escape_called_twice => JSError.EscapeCalledTwice,
        .napi_handle_scope_mismatch => JSError.HandleScopeMismatch,
        .napi_callback_scope_mismatch => JSError.CallbackScopeMismatch,
        .napi_queue_full => JSError.QueueFull,
        .napi_closing => JSError.Closing,
        .napi_bigint_expected => JSError.BigintExpected,
        .napi_date_expected => JSError.DateExpected,
        .napi_arraybuffer_expected => JSError.ArrayBufferExpected,
        .napi_detachable_arraybuffer_expected => JSError.DetatchableArrayBufferExpected,
        .napi_would_deadlock => JSError.WouldDeadlock,
        .napi_no_external_buffers_allowed => JSError.NoExternalArrayBuffersAllowed,
        .napi_cannot_run_js => JSError.CannotRunJs, 
    };
}

const Boolean = opaque {
    extern fn napi_get_boolean(env: *js.Env, val: bool, result: **Boolean) Status;
    extern fn napi_get_value_bool(env: *js.Env, val: *const Boolean, result: *bool) Status;

    pub fn getBoolean(env: *js.Env, val: bool) !*Boolean {
        var result: *Boolean = undefined;
        const status = napi_get_boolean(env, val, &result);
        try checkStatus(status);

        return result;
    }

    pub fn getValue(this: *const Boolean, env: *js.Env) !bool {
        var result: bool = undefined;
        const status = napi_get_value_bool(env, this, &result);
        try checkStatus(status);

        return result;
    }
};