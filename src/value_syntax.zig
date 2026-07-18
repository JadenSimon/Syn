const std = @import("std");
const lexer = @import("./lexer.zig");
const parser = @import("./parser.zig");
const value_graph = @import("./value_graph.zig");
const getAllocator = @import("./string_immutable.zig").getAllocator;

const ValueNode = value_graph.ValueNode;
const ValueRef = value_graph.ValueRef;
const NumberType = value_graph.NumberType;
const BumpAllocator = parser.BumpAllocator;

const Json5Lexer = lexer.NewLexer(true, .{
    .is_json = true,
    .allow_comments = true,
    .allow_trailing_commas = true,
    .always_decode_escape_sequences = true,
});

fn ParseEventBase(comptime T: type) type {
    return struct {
        location: u32,
        data: T,
    };
}

const StartOrEnd = enum(u1) {
    start,
    end,
};

pub const ReferenceBase = enum {
    document_root,
    this,
    super,
};

pub const ParseEvent = union(enum) {
    Error: ParseEventBase([]const u8),

    Array: ParseEventBase(StartOrEnd),
    Object: ParseEventBase(StartOrEnd),
    Computation: ParseEventBase(StartOrEnd),
    Reference: ParseEventBase(StartOrEnd),
    ReferenceElement: ParseEventBase(StartOrEnd),

    ReferenceBase: ParseEventBase(ReferenceBase),
    ReferenceProperty: ParseEventBase([]const u8),

    Boolean: ParseEventBase(bool),
    String: ParseEventBase([]const u8),
    Number: ParseEventBase(f64),
    Null: ParseEventBase(void),
    PropertyName: ParseEventBase([]const u8),
    PrivateName: ParseEventBase([]const u8),
};

pub fn Parser(comptime Listener: type) type {
    return struct {
        lexer: Json5Lexer,
        listener: Listener,

        const Self = @This();

        pub fn init(
            listener: Listener,
            source: lexer.Source,
            allocator: std.mem.Allocator,
        ) !Self {
            return .{
                .listener = listener,
                .lexer = Json5Lexer.initWithoutReading(source, allocator),
            };
        }

        pub fn parse(self: *Self) !void {
            self.lexer.pause_on_comments = false;

            try self.lexer.scanFirst();
            try self.parseValue();

            if (self.lexer.token != .t_end_of_file) {
                try self.emitError("Expected end of file");
            }
        }

        inline fn location(self: *const Self) u32 {
            return parser.encodeLocation(
                self.lexer.line_map.count,
                @as(u32, @intCast(
                    self.lexer.start - self.lexer.last_line,
                )),
            );
        }

        inline fn stringLocation(self: *const Self) u32 {
            return parser.encodeLocation(
                self.lexer.line_map.count,
                @as(u32, @intCast(
                    self.lexer.start - self.lexer.full_last_line,
                )),
            );
        }

        inline fn emit(self: *Self, event: ParseEvent) anyerror!void {
            try self.listener.onEvent(event);
        }

        inline fn emitError(self: *Self, message: []const u8) anyerror!void {
            try self.emit(.{
                .Error = .{
                    .location = self.location(),
                    .data = message,
                },
            });
        }

        fn emitContainer(
            self: *Self,
            comptime tag: enum {
                array,
                object,
                computation,
                reference,
                reference_element,
            },
            state: StartOrEnd,
        ) anyerror!void {
            const event = ParseEventBase(StartOrEnd){
                .location = self.location(),
                .data = state,
            };

            switch (tag) {
                .array => try self.emit(.{ .Array = event }),
                .object => try self.emit(.{ .Object = event }),
                .computation => try self.emit(.{ .Computation = event }),
                .reference => try self.emit(.{ .Reference = event }),
                .reference_element => {
                    try self.emit(.{ .ReferenceElement = event });
                },
            }
        }

        fn parseValue(self: *Self) anyerror!void {
            switch (self.lexer.token) {
                .t_open_brace => try self.parseObject(),
                .t_open_bracket => try self.parseArray(),
                .t_open_paren => try self.parseComputation(),

                .t_identifier, .t_this, .t_super => try self.parseReference(),

                .t_numeric_literal => try self.parseNumber(false),
                .t_minus => try self.parseNumber(true),

                .t_string_literal => try self.parseString(),
                .t_true => try self.parseBoolean(true),
                .t_false => try self.parseBoolean(false),
                .t_null => try self.parseNull(),

                else => {
                    try self.emitError("Invalid value");
                    try self.lexer.next();
                },
            }
        }

        fn parseNull(self: *Self) anyerror!void {
            const event = ParseEventBase(void){
                .location = self.location(),
                .data = {},
            };

            try self.lexer.next();
            try self.emit(.{ .Null = event });
        }

        fn parseBoolean(self: *Self, value: bool) anyerror!void {
            const event = ParseEventBase(bool){
                .location = self.location(),
                .data = value,
            };

            try self.lexer.next();
            try self.emit(.{ .Boolean = event });
        }

        fn parseString(self: *Self) anyerror!void {
            const event = ParseEventBase([]const u8){
                .location = self.stringLocation(),
                .data = self.lexer.string_literal_slice,
            };

            try self.emit(.{ .String = event });
            try self.lexer.next();
        }

        fn parseNumber(self: *Self, negative: bool) anyerror!void {
            const start_location = self.location();

            if (negative) {
                try self.lexer.next();

                if (self.lexer.token != .t_numeric_literal) {
                    try self.emitError("Expected number after minus sign");
                    return;
                }
            }

            const value = self.lexer.number;
            try self.lexer.next();

            try self.emit(.{
                .Number = .{
                    .location = start_location,
                    .data = if (negative) -value else value,
                },
            });
        }

        fn parseArray(self: *Self) anyerror!void {
            try self.emitContainer(.array, .start);
            try self.lexer.next();

            while (self.lexer.token != .t_close_bracket and
                self.lexer.token != .t_end_of_file)
            {
                try self.parseValue();

                if (self.lexer.token == .t_comma) {
                    try self.lexer.next();

                    if (self.lexer.token == .t_close_bracket) {
                        break;
                    }
                } else if (self.lexer.token != .t_close_bracket) {
                    try self.emitError("Expected ',' or ']'");
                    try self.lexer.next();
                }
            }

            if (self.lexer.token != .t_close_bracket) {
                try self.emitError("Unterminated array");
                return;
            }

            try self.emitContainer(.array, .end);
            try self.lexer.next();
        }

        fn parseObject(self: *Self) anyerror!void {
            try self.emitContainer(.object, .start);
            try self.lexer.next();

            while (self.lexer.token != .t_close_brace and
                self.lexer.token != .t_end_of_file)
            {
                try self.parsePropertyName();

                if (self.lexer.token != .t_colon) {
                    try self.emitError("Expected ':' after property name");
                    try self.recoverObject();
                    continue;
                }

                try self.lexer.next();
                try self.parseValue();

                if (self.lexer.token == .t_comma) {
                    try self.lexer.next();

                    if (self.lexer.token == .t_close_brace) {
                        break;
                    }
                } else if (self.lexer.token != .t_close_brace) {
                    try self.emitError("Expected ',' or '}'");
                    try self.recoverObject();
                }
            }

            if (self.lexer.token != .t_close_brace) {
                try self.emitError("Unterminated object");
                return;
            }

            try self.emitContainer(.object, .end);
            try self.lexer.next();
        }

        fn parsePropertyName(self: *Self) anyerror!void {
            const event = switch (self.lexer.token) {
                .t_identifier, .t_private_identifier => ParseEventBase([]const u8){
                    .location = self.location(),
                    .data = self.lexer.identifier,
                },
                .t_string_literal => ParseEventBase([]const u8){
                    .location = self.location(),
                    .data = self.lexer.string_literal_slice,
                },
                else => {
                    try self.emitError("Expected property name");
                    try self.lexer.next();
                    return;
                },
            };

            if (self.lexer.token == .t_private_identifier) {
                try self.emit(.{ .PrivateName = event });
            } else {
                try self.emit(.{ .PropertyName = event });
            }
            try self.lexer.next();
        }

        fn parseComputation(self: *Self) anyerror!void {
            try self.emitContainer(.computation, .start);
            try self.lexer.next();

            if (self.lexer.token == .t_close_paren) {
                try self.emitError("Computation requires a subject");
                try self.emitContainer(.computation, .end);
                try self.lexer.next();
                return;
            }

            try self.parseValue();

            if (self.lexer.token == .t_comma) {
                try self.lexer.next();

                if (self.lexer.token == .t_close_paren) {
                    try self.emitError(
                        "Expected computation input after comma",
                    );
                } else {
                    try self.parseValue();
                }
            }

            if (self.lexer.token != .t_close_paren) {
                try self.emitError("Expected ')' after computation");
                try self.recoverUntil(.t_close_paren);
            }

            if (self.lexer.token == .t_close_paren) {
                try self.emitContainer(.computation, .end);
                try self.lexer.next();
            }
        }

        fn parseReference(self: *Self) anyerror!void {
            try self.emitContainer(.reference, .start);
            if (self.lexer.token == .t_identifier) {
                if (!std.mem.eql(u8, self.lexer.identifier, "$")) unreachable;
            }

            const base = switch (self.lexer.token) {
                .t_identifier => ReferenceBase.document_root,
                .t_this => ReferenceBase.this,
                .t_super => ReferenceBase.super,
                else => unreachable,
            };

            try self.emit(.{
                .ReferenceBase = .{
                    .location = self.location(),
                    .data = base,
                },
            });

            try self.lexer.next();

            while (true) {
                switch (self.lexer.token) {
                    .t_dot => {
                        try self.parseReferenceProperty();
                    },
                    .t_open_bracket => {
                        try self.parseReferenceElement();
                    },
                    else => break,
                }
            }

            try self.emitContainer(.reference, .end);
        }

        fn parseReferenceProperty(self: *Self) anyerror!void {
            try self.lexer.next();

            if (self.lexer.token != .t_identifier and self.lexer.token != .t_private_identifier) {
                try self.emitError(
                    "Expected identifier after '.'",
                );
                return;
            }

            try self.emit(.{
                .ReferenceProperty = .{
                    .location = self.location(),
                    .data = self.lexer.identifier,
                },
            });

            try self.lexer.next();
        }

        fn parseReferenceElement(self: *Self) anyerror!void {
            try self.emitContainer(.reference_element, .start);
            try self.lexer.next(); // '['

            switch (self.lexer.token) {
                .t_string_literal => try self.parseString(),
                .t_numeric_literal => try self.parseNumber(false),
                .t_minus => try self.parseNumber(true),
                .t_identifier, .t_this, .t_super => try self.parseReference(),

                .t_open_brace,
                .t_open_bracket,
                .t_open_paren,
                => {
                    try self.emitError(
                        "Reference index cannot be an object, array, or computation",
                    );
                    try self.parseValue();
                },

                else => {
                    try self.emitError(
                        "Reference index must be a string, number, or reference",
                    );

                    if (self.lexer.token != .t_close_bracket) {
                        try self.lexer.next();
                    }
                },
            }

            if (self.lexer.token != .t_close_bracket) {
                try self.emitError("Expected ']' after reference index");
                try self.recoverUntil(.t_close_bracket);
            }

            if (self.lexer.token == .t_close_bracket) {
                try self.emitContainer(.reference_element, .end);
                try self.lexer.next();
            }
        }

        fn recoverObject(self: *Self) anyerror!void {
            while (self.lexer.token != .t_comma and
                self.lexer.token != .t_close_brace and
                self.lexer.token != .t_end_of_file)
            {
                try self.lexer.next();
            }

            if (self.lexer.token == .t_comma) {
                try self.lexer.next();
            }
        }

        fn recoverUntil(
            self: *Self,
            expected: @TypeOf(self.lexer.token),
        ) anyerror!void {
            while (self.lexer.token != expected and
                self.lexer.token != .t_end_of_file)
            {
                try self.lexer.next();
            }
        }
    };
}

pub const ValueEmitter = struct {
    alloc: std.mem.Allocator,
    nodes: *BumpAllocator(ValueNode),

    stack: std.ArrayListUnmanaged(Frame) = .{},
    object_stack: std.ArrayListUnmanaged(ValueRef) = .{},
    private_values: std.AutoHashMapUnmanaged(ValueRef, std.StringHashMapUnmanaged(ValueRef)) = .{},

    deferred_refs: std.ArrayListUnmanaged(DeferredRef) = .{},
    placeholder_to_idx: std.AutoHashMapUnmanaged(ValueRef, usize) = .{},

    result: ValueRef = 0,
    had_error: bool = false,

    const ChainBuilder = struct {
        head: ValueRef = 0,
        tail: ValueRef = 0,

        fn append(self: *ChainBuilder, nodes: *BumpAllocator(ValueNode), item: ValueRef) void {
            if (self.head == 0) {
                self.head = item;
            } else {
                nodes.at(self.tail).next = @truncate(item);
            }
            self.tail = item;
        }
    };

    const ComputationBuilder = struct {
        subject: ValueRef = 0,
        input: ValueRef = 0,
        count: u2 = 0,
    };

    const Frame = union(enum) {
        object: struct { 
            ref: ValueRef, 
            chain: ChainBuilder = .{}, 
            private_values: std.StringHashMapUnmanaged(ValueRef) = .{},
        },
        array: ChainBuilder,
        computation: ComputationBuilder,
        private: *ValueRef,
        // index into `deferred_refs`
        reference: usize,
        reference_element: struct { idx: usize, value: ValueRef = 0 },
    };

    const Segment = union(enum) {
        property: []const u8,
        element: ValueRef,
    };

    const DeferredRef = struct {
        placeholder: ValueRef,
        base: ReferenceBase,
        // resolved immediately for `this`/`super` (their target object is
        // already known on `object_stack`); null for `$`, resolved against
        // the document root once parsing finishes
        enclosing: ?ValueRef,
        segments: std.ArrayListUnmanaged(Segment) = .{},
    };

    pub fn init(alloc: std.mem.Allocator, nodes: *BumpAllocator(ValueNode)) ValueEmitter {
        return .{ .alloc = alloc, .nodes = nodes };
    }

    fn createString(self: *ValueEmitter, s: []const u8) !ValueRef {
        var decoded_buf = try std.ArrayList(u8).initCapacity(self.alloc, s.len);
        defer decoded_buf.deinit();
        _ = try lexer.decodeJSEscapeSequences(s, &decoded_buf, '"');
        const owned = try decoded_buf.toOwnedSlice();
        return self.nodes.push(.{
            .kind = .string,
            .slot0 = if (comptime @import("builtin").target.isWasm()) 0 else @truncate(@intFromPtr(owned.ptr) >> 32),
            .slot1 = @truncate(@intFromPtr(owned.ptr)),
            .slot2 = @truncate(owned.len),
        });
    }

    fn createNumber(self: *ValueEmitter, v: f64) !ValueRef {
        const u: u64 = @bitCast(v);
        return self.nodes.push(.{
            .kind = .number,
            .slot0 = @truncate(u >> 32),
            .slot1 = @truncate(u),
            .slot2 = @intFromEnum(NumberType.float),
        });
    }

    fn pushValue(self: *ValueEmitter, ref: ValueRef) !void {
        if (self.stack.items.len == 0) {
            self.result = ref;
            return;
        }
        switch (self.stack.items[self.stack.items.len - 1]) {
            .object => |*o| o.chain.append(self.nodes, ref),
            .array => |*a| a.append(self.nodes, ref),
            .computation => |*c| {
                switch (c.count) {
                    0 => c.subject = ref,
                    1 => c.input = ref,
                    else => return error.TooManyComputationValues,
                }
                c.count += 1;
            },
            .reference_element => |*re| re.value = ref,
            .private => |f| {
                f.* = ref;
                _ = self.stack.pop();
            },
            .reference => return error.UnexpectedValueInsideReference,
        }
    }

    pub fn onEvent(self: *ValueEmitter, event: ParseEvent) !void {
        switch (event) {
            .Error => |_| {
                self.had_error = true;
            },

            .Object => |ev| {
                if (ev.data == .start) {
                    const ref = try self.nodes.push(.{ .kind = .object });
                    try self.stack.append(self.alloc, .{ .object = .{ .ref = ref } });
                    try self.object_stack.append(self.alloc, ref);
                } else {
                    const frame = self.stack.pop();
                    const o = frame.object;
                    self.nodes.at(o.ref).slot0 = o.chain.head;
                    _ = self.object_stack.pop();
                    try self.pushValue(o.ref);
                    if (o.private_values.count() > 0) {
                        try self.private_values.put(self.alloc, o.ref, o.private_values);
                    }
                }
            },

            .Array => |ev| {
                if (ev.data == .start) {
                    try self.stack.append(self.alloc, .{ .array = .{} });
                } else {
                    const frame = self.stack.pop();
                    const ref = try self.nodes.push(.{ .kind = .array, .slot0 = frame.array.head });
                    try self.pushValue(ref);
                }
            },

            .Computation => |ev| {
                if (ev.data == .start) {
                    try self.stack.append(self.alloc, .{ .computation = .{} });
                } else {
                    const frame = self.stack.pop();
                    const c = frame.computation;
                    if (c.subject != 0) {
                        self.nodes.at(c.subject).next = @truncate(c.input);
                    }
                    const ref = try self.nodes.push(.{ .kind = .computed, .slot0 = c.subject, .slot1 = c.input });
                    try self.pushValue(ref);
                }
            },

            .Reference => |ev| {
                if (ev.data == .start) {
                    // nothing yet, we will build the chain
                } else {
                    const frame = self.stack.pop();
                    try self.pushValue(self.deferred_refs.items[frame.reference].placeholder);
                }
            },

            .ReferenceElement => |ev| {
                if (ev.data == .start) {
                    const top = self.stack.items[self.stack.items.len - 1];
                    const idx = switch (top) {
                        .reference => |i| i,
                        else => return error.ReferenceElementOutsideReference,
                    };
                    try self.stack.append(self.alloc, .{ .reference_element = .{ .idx = idx } });
                } else {
                    const frame = self.stack.pop();
                    const re = frame.reference_element;
                    try self.deferred_refs.items[re.idx].segments.append(self.alloc, .{ .element = re.value });
                }
            },

            .ReferenceBase => |ev| {
                const placeholder = try self.nodes.push(.{ .kind = .ref, .slot2 = 0 });
                var entry = DeferredRef{
                    .placeholder = placeholder,
                    .base = ev.data,
                    .enclosing = null,
                };
                switch (ev.data) {
                    .this => {
                        if (self.object_stack.items.len == 0) return error.NoEnclosingObject;
                        entry.enclosing = self.object_stack.items[self.object_stack.items.len - 1];
                    },
                    .super => {
                        if (self.object_stack.items.len < 2) return error.NoParentObject;
                        entry.enclosing = self.object_stack.items[self.object_stack.items.len - 2];
                    },
                    .document_root => {}, // resolved in `finish`
                }
                const idx = self.deferred_refs.items.len;
                try self.deferred_refs.append(self.alloc, entry);
                try self.placeholder_to_idx.put(self.alloc, placeholder, idx);
                try self.stack.append(self.alloc, .{ .reference = idx });
            },

            .ReferenceProperty => |ev| {
                const idx = switch (self.stack.items[self.stack.items.len - 1]) {
                    .reference => |i| i,
                    else => return error.ReferencePropertyOutsideReference,
                };
                const owned = try self.alloc.dupe(u8, ev.data);
                try self.deferred_refs.items[idx].segments.append(self.alloc, .{ .property = owned });
            },

            .Boolean => |ev| try self.pushValue(try self.nodes.push(.{ .kind = if (ev.data) .true else .false })),
            .Null => |_| try self.pushValue(try self.nodes.push(.{ .kind = .null })),
            .Number => |ev| try self.pushValue(try self.createNumber(ev.data)),
            .String, .PropertyName => |ev| try self.pushValue(try self.createString(ev.data)),

            .PrivateName => |ev| {
                const entry = try self.stack.items[self.stack.items.len - 1].object.private_values.getOrPut(self.alloc, ev.data);
                try self.stack.append(self.alloc, .{ .private = entry.value_ptr });
            },
        }
    }

    fn getStringSlice(node: *const ValueNode) []const u8 {
        if (node.slot2 == 0) return &.{};
        const ptr: [*]const u8 = @ptrFromInt((@as(u64, node.slot0) << 32) | node.slot1);
        return ptr[0..node.slot2];
    }

    fn getNumberValue(node: *const ValueNode) f64 {
        const u: u64 = (@as(u64, node.slot0) << 32) | node.slot1;
        return @bitCast(u);
    }

    fn lookupProperty(self: *ValueEmitter, container: ValueRef, name: []const u8) !ValueRef {
        const n = self.nodes.at(container);
        if (n.kind != .object) return error.NotAnObject;
        var key_ref = n.slot0;
        while (key_ref != 0) {
            const key_node = self.nodes.at(key_ref);
            const value_ref = key_node.next;
            if (key_node.kind == .string and std.mem.eql(u8, getStringSlice(key_node), name)) {
                return value_ref;
            }
            key_ref = self.nodes.at(value_ref).next;
        }
        return error.PropertyNotFound;
    }

    fn lookupIndex(self: *ValueEmitter, container: ValueRef, index_ref: ValueRef) !ValueRef {
        const idx_n = self.nodes.at(index_ref);
        switch (idx_n.kind) {
            .string => return self.lookupProperty(container, getStringSlice(idx_n)),
            .number => {
                const i = getNumberValue(idx_n);
                const n = self.nodes.at(container);
                if (n.kind == .computed) {
                    if (i == 0) return n.slot0;
                    if (i == 1 and n.slot1 != 0) return n.slot1;
                    return error.IndexOutOfRange;
                }
                if (n.kind != .array) return error.NotAnArray;
                var s = n.slot0;
                var pos: f64 = 0;
                while (s != 0) : (pos += 1) {
                    if (pos == i) return s;
                    s = self.nodes.at(s).next;
                }
                return error.IndexOutOfRange;
            },
            else => return error.InvalidIndexType,
        }
    }

    fn resolveIfReference(self: *ValueEmitter, ref: ValueRef) !ValueRef {
        const idx = self.placeholder_to_idx.get(ref) orelse return ref;
        return self.resolveDeferred(idx);
    }

    pub fn followRef(self: *ValueEmitter, ref: ValueRef) ValueRef {
        var r = ref;
        while (self.nodes.at(r).kind == .ref) {
            r = self.nodes.at(r).slot0;
        }
        return r;
    }

    fn resolveDeferred(self: *ValueEmitter, idx: usize) anyerror!ValueRef {
        const entry = &self.deferred_refs.items[idx];
        const node = self.nodes.at(entry.placeholder);
        if (node.slot2 == 1) return node.slot0;
        if (node.slot2 == 2) return error.CircularReference;
        node.slot2 = 2;

        var current: ValueRef = entry.enclosing orelse self.result; // `$` -> root
        for (entry.segments.items) |seg| {
            current = switch (seg) {
                .property => |name| blk: {
                    if (name[0] == '#') {
                        const n =  self.nodes.at(current);
                        if (n.kind == .computed) {
                            if (std.mem.eql(u8, name, "#subject")) break :blk n.slot0;
                            if (std.mem.eql(u8, name, "#input") and n.slot1 != 0) break :blk n.slot1;
                            return error.PropertyNotFound;
                        }
                        const m = self.private_values.get(current) orelse return error.MissingPrivateValueMap;
                        if (m.get(name)) |v| break :blk v;
                        return error.MissingPrivateValue;
                    }
                    break :blk try self.lookupProperty(current, name);
                },
                .element => |val_ref| try self.lookupIndex(current, try self.resolveIfReference(val_ref)),
            };
        }

        node.slot0 = current;
        node.slot1 = 0;
        node.slot2 = 1;
        return current;
    }

    pub fn finish(self: *ValueEmitter) !ValueRef {
        var i: usize = 0;
        while (i < self.deferred_refs.items.len) : (i += 1) {
            _ = try self.resolveDeferred(i);
        }
        return self.result;
    }
};
