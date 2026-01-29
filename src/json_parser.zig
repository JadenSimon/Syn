const std = @import("std");
const lexer = @import("./lexer.zig");
const parser = @import("./parser.zig");
const ComptimeStringMap = @import("comptime_string_map.zig").ComptimeStringMap;
const strings = @import("./string_immutable.zig");

const Json5Lexer = lexer.NewLexer(true, .{
    .is_json = true,
    .allow_comments = true,
    .allow_trailing_commas = true,
    .always_decode_escape_sequences = true,
});

const NodeRef = u32;

const AstNode = packed struct {
    kind: parser.SyntaxKind,
    flags: u20 = 0,
    next: NodeRef = 0,
};

const ParseEventKind = enum {
    null_value,
    string_value,
    number_value,
    boolean_value,

    object_start,
    object_end,

    property_name,

    array_start,
    array_end,

    syntax_error,
};

fn ParseEventBase(comptime T: type) type {
    return struct {
        location: u32,
        data: T,
    };
}

const StartOrEnd = enum(u1) { start, end };

pub const ParseEvent = union(enum) {
    Error: ParseEventBase([]const u8),
    Array: ParseEventBase(StartOrEnd),
    Object: ParseEventBase(StartOrEnd),
    Boolean: ParseEventBase(bool),
    String: ParseEventBase([]const u8),
    Number: ParseEventBase(f64),
    Null: ParseEventBase(void),
    PropertyName: ParseEventBase([]const u8),
};

fn Parser_(comptime Listener: type, comptime is_json5: bool) type {
    _ = is_json5;

    return struct {
        lexer: Json5Lexer,
        listener: Listener,

        pub fn init(listener: Listener, source: lexer.Source, allocator: std.mem.Allocator) !@This() {
            return .{
                .listener = listener,
                .lexer = Json5Lexer.initWithoutReading(source, allocator),
            };
        }

        pub fn parse(this: *@This()) !void {
            this.lexer.pause_on_comments = false;
            try this.lexer.scanFirst();
            try this.parseValue();
            try this.lexer.expect(.t_end_of_file);
        }

        inline fn getLocation(this: *const @This()) u32 {
            return parser.encodeLocation(
                this.lexer.line_count,
                @as(u32, @intCast(this.lexer.start - this.lexer.last_line)),
            );
        }

        inline fn _emitEvent(this: *@This(), ev: ParseEvent) !void {
            return this.listener.onEvent(ev);
        }

        inline fn emitEvent(this: *@This(), kind: ParseEventKind) !void {
            switch (kind) {
                .object_start, .array_start => {
                    const ev = ParseEventBase(StartOrEnd){
                        .data = .start,
                        .location = this.getLocation(),
                    };

                    try this.lexer.next(); // { and [

                    if (kind == .object_start) {
                        return this._emitEvent(.{ .Object = ev });
                    } else {
                        return this._emitEvent(.{ .Array = ev });
                    }
                },
                .object_end, .array_end => {
                    const ev = ParseEventBase(StartOrEnd){
                        .data = .end,
                        .location = this.getLocation(),
                    };

                    try this.lexer.next(); // } and ]

                    if (kind == .object_end) {
                        return this._emitEvent(.{ .Object = ev });
                    } else {
                        return this._emitEvent(.{ .Array = ev });
                    }
                },
                .null_value => {
                    try this.lexer.next();
                    return this._emitEvent(.{ .Null = .{ .data = {}, .location = 0 } });
                },
                else => {},
            }
        }

        inline fn emitPropertyEvent(this: *@This()) !void {
            const ev = ParseEventBase([]const u8){
                .data = this.lexer.identifier,
                .location = this.getLocation(),
            };

            try this._emitEvent(.{ .PropertyName = ev });

            try this.lexer.next();
        }

        inline fn emitStringOrPropertyEvent(this: *@This(), kind: ParseEventKind) !void {
            const ev = ParseEventBase([]const u8){
                .data = this.lexer.string_literal_slice,
                .location = this.getLocation(),
            };

            try switch (kind) {
                .property_name => this._emitEvent(.{ .PropertyName = ev }),
                .string_value => this._emitEvent(.{ .String = ev }),
                else => {},
            };

            try this.lexer.next();
        }

        inline fn emitBooleanEvent(this: *@This(), comptime value: bool) !void {
            try this.lexer.next();
            return this._emitEvent(.{ .Boolean = .{
                .data = value,
                .location = 0,
            } });
        }

        inline fn emitNumberEvent(this: *@This(), comptime is_negative: bool) !void {
            if (comptime is_negative) {
                try this.lexer.next();
                if (this.lexer.token != .t_numeric_literal) {
                    return this.emitErrorEvent("No number after minus sign");
                }
            }

            const value = this.lexer.number;
            try this.lexer.next();
            return this._emitEvent(.{ .Number = .{
                .data = if (comptime is_negative) -value else value,
                .location = 0,
            } });
        }

        inline fn emitErrorEvent(this: *@This(), comptime msg: []const u8) !void {
            return this._emitEvent(.{ .Error = .{
                .data = msg,
                .location = this.getLocation(),
            } });
        }

        fn parseValue(this: *@This()) !void {
            return switch (this.lexer.token) {
                .t_open_brace => this.parseObject(),
                .t_open_bracket => this.parseArray(),
                .t_numeric_literal => this.emitNumberEvent(false),
                .t_minus => this.emitNumberEvent(true),
                .t_string_literal => this.emitStringOrPropertyEvent(.string_value),
                .t_true => this.emitBooleanEvent(true),
                .t_false => this.emitBooleanEvent(false),
                .t_null => this.emitEvent(.null_value),
                else => {
                    try this.emitErrorEvent("Invalid JSON value");
                    try this.lexer.next();
                },
            };
        }

        fn parseArray(this: *@This()) !void {
            try this.emitEvent(.array_start);

            while (this.lexer.token != .t_close_bracket) {
                try this.parseValue();

                if (this.lexer.token == .t_comma) {
                    try this.lexer.next();
                }
            }

            try this.emitEvent(.array_end);
        }

        fn parseObject(this: *@This()) !void {
            try this.emitEvent(.object_start);

            while (this.lexer.token != .t_close_brace) {
                if (this.lexer.token == .t_identifier) {
                    try this.emitPropertyEvent();
                    try this.lexer.expect(.t_colon);
                    try this.parseValue();
                    if (this.lexer.token == .t_comma) {
                        try this.lexer.next();
                    }
                    continue;
                }

                if (this.lexer.token != .t_string_literal) {
                    try this.emitErrorEvent("Expected property name");
                    try this.lexer.next();
                    continue;
                }

                try this.emitStringOrPropertyEvent(.property_name);
                try this.lexer.expect(.t_colon);
                try this.parseValue();

                if (this.lexer.token == .t_comma) {
                    try this.lexer.next();
                }
            }

            try this.emitEvent(.object_end);
        }
    };
}

const JsValueEmitter = struct {
    const js = @import("js");

    const PartialObject = struct {
        current_prop: ?*js.Value = null,
        handle: *js.Object,
    };

    const PartialArray = struct {
        len: u32 = 0,
        handle: *js.ArrayPointer,
    };

    const StackValue = union(enum) {
        Object: PartialObject,
        Array: PartialArray,
    };

    env: *js.Env,
    stack: std.ArrayList(StackValue),
    result: *js.Value = undefined,

    pub fn init(env: *js.Env, allocator: std.mem.Allocator) @This() {
        return .{
            .env = env,
            .stack = std.ArrayList(StackValue).init(allocator),
        };
    }

    pub fn initCurrentEnv(allocator: std.mem.Allocator) @This() {
        return @This().init(js.getCurrentEnv(), allocator);
    }

    pub fn getResult(this: @This()) *js.Value {
        defer this.stack.deinit();

        return this.result;
    }

    fn pushValue(this: *@This(), val: *anyopaque) !void {
        const v: *js.Value = @ptrCast(val);
        if (this.stack.items.len == 0) {
            this.result = v;
            return;
        }

        switch (this.stack.items[this.stack.items.len - 1]) {
            .Object => |*o| {
                if (o.current_prop) |p| {
                    try o.handle.setProperty(this.env, p, v);
                    o.current_prop = null;
                } else {
                    o.current_prop = v;
                }
            },
            .Array => |*arr| {
                try arr.handle.set(this.env, arr.len, v);
                arr.len += 1;
            },
        }
    }

    pub fn onEvent(this: *@This(), event: ParseEvent) !void {
        switch (event) {
            .Object => |ev| {
                if (ev.data == .start) {
                    try this.stack.append(.{
                        .Object = .{
                            .handle = try js.Object.init(this.env),
                        },
                    });
                } else {
                    switch (this.stack.pop()) {
                        .Object => |obj| {
                            try this.pushValue(obj.handle);
                        },
                        else => return error.Unexpected,
                    }
                }
            },
            .Array => |ev| {
                if (ev.data == .start) {
                    try this.stack.append(.{
                        .Array = .{
                            .handle = try js.ArrayPointer.initCapacity(this.env, 0),
                        },
                    });
                } else {
                    switch (this.stack.pop()) {
                        .Array => |arr| {
                            try this.pushValue(arr.handle);
                        },
                        else => return error.Unexpected,
                    }
                }
            },
            .Number => |ev| {
                try this.pushValue(try js.Number.createDouble(this.env, ev.data));
            },
            .Boolean => |ev| {
                try this.pushValue(try this.env.getBool(ev.data));
            },
            .Null => |_| {
                try this.pushValue(try this.env.getNull());
            },
            .String => |ev| {
                try this.pushValue(try js.String.fromUtf8(this.env, ev.data));
                // try this.pushValue(try js.String.fromLatin1External(this.env, ev.data));
            },
            .PropertyName => |ev| {
                try this.pushValue(try js.String.fromUtf8(this.env, ev.data));
                // try this.pushValue(try js.String.fromLatin1External(this.env, ev.data));
            },
            .Error => |ev| {
                _ = ev; // TODO
                return error.SyntaxError;
            },
        }
    }
};

pub fn parseJson5ToJs(source: []const u8, allocator: std.mem.Allocator) !*@import("js").Value {
    var emitter = JsValueEmitter.initCurrentEnv(allocator);
    const Parser = Parser_(*JsValueEmitter, true);
    var p = try Parser.init(&emitter, .{ .contents = source }, allocator);
    try p.parse();

    return emitter.getResult();
}

pub const Json5Parser = Parser_(void, true);
