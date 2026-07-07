//! Capture analysis + "moveable" instrumentation.
//!
//! This is a focused Zig port of the (commented out) TypeScript reference below.
//! It does *two* things:
//!   1. For every function-like node, compute the set of lexically captured
//!      symbols (anything referenced that was declared outside the function,
//!      including imports/globals).
//!   2. From that, build a context-free "micro program" + a `__moveable__`
//!      annotation so the function can be serialized/relocated.
//!
//! Only ESM is supported. `this`/`super`, enum inlining, class members, mutable
//! capture-by-reference, and module reduction from the reference impl are all
//! intentionally skipped for this first pass.

const std = @import("std");
const parser = @import("./parser.zig");
const getAllocator = @import("./string_immutable.zig").getAllocator;

const NodeRef = parser.NodeRef;
const SymbolRef = parser.SymbolRef;
const AstNode = parser.AstNode;
const Binder = parser.Binder;
const AstData = parser.AstData;
const Factory = parser.Factory;
const NodeIterator = parser.NodeIterator;
const BumpAllocator = parser.BumpAllocator;
const SyntaxKind = parser.SyntaxKind;
const forEachChild = parser.forEachChild;

const getPackedData = parser.getPackedData;
const maybeUnwrapRef = parser.maybeUnwrapRef;
const getSlice = parser.getSlice;
const toBinaryDataPtrRefs = parser.toBinaryDataPtrRefs;

pub const CapturedSymbol = struct {
    symbol: SymbolRef,
    name: []const u8,
};

pub const FunctionCapture = struct {
    decl_ref: NodeRef,
    kind: SyntaxKind,
    name: ?[]const u8, // null when anonymous / computed
    // The symbol this function is bound to, when knowable (named function
    // declarations/expressions). Used for circular-capture detection.
    self_symbol: SymbolRef = 0,
    captured: []const CapturedSymbol,
};

pub const MethodCapture = struct {
    // Display / module name. `__computed` for computed names.
    name: []const u8,
    // The method's name node — an `identifier` or a `computed_property_name`.
    name_ref: NodeRef,
    is_computed: bool,
    is_static: bool,
    captured: []const CapturedSymbol,
};

pub const ClassCapture = struct {
    decl_ref: NodeRef,
    name: ?[]const u8,
    self_symbol: SymbolRef = 0,
    // Construction-time captures (field initializers, static blocks, constructor).
    captured: []const CapturedSymbol,
    methods: []const MethodCapture,
};

pub const Analysis = struct {
    results: []FunctionCapture,
    classes: []ClassCapture,
    // Symbols reassigned via a binary `=`-family operator somewhere.
    assigned: std.AutoArrayHashMapUnmanaged(SymbolRef, void),
    // Symbols declared as a `for (...)` loop binding.
    for_bindings: std.AutoArrayHashMapUnmanaged(SymbolRef, void),

    pub fn deinit(self: *Analysis, alloc: std.mem.Allocator) void {
        alloc.free(self.results);
        for (self.classes) |c| alloc.free(c.methods);
        alloc.free(self.classes);
        self.assigned.deinit(alloc);
        self.for_bindings.deinit(alloc);
    }
};

fn isFunctionLike(kind: SyntaxKind) bool {
    return switch (kind) {
        .function_declaration,
        .function_expression,
        .arrow_function,
        .method_declaration,
        .constructor,
        .get_accessor,
        .set_accessor,
        => true,
        else => false,
    };
}

const Frame = struct {
    decl_ref: NodeRef,
    kind: SyntaxKind,
    name: ?[]const u8 = null,
    self_symbol: SymbolRef = 0,
    // Scope depth of the function's own params/top-level locals. Any referenced
    // symbol whose declaration depth is `< threshold` is captured by this frame.
    threshold: u16,
    captures: std.AutoArrayHashMapUnmanaged(SymbolRef, void) = .{},
};

// this expects a fresh parsed file (for simplicity)
pub const SynthInstrumenter = struct {
    alloc: std.mem.Allocator,
    binder: *const Binder,
    nodes: *BumpAllocator(AstNode),
    factory: *Factory,
    ast: *AstData,

    replacements: *std.AutoArrayHashMap(NodeRef, NodeRef),
    rebindings: std.ArrayListUnmanaged(NodeRef) = .{},

    emitting: bool = false,
    transforming: bool = false,

    depth: u16 = 1,

    frames: std.ArrayListUnmanaged(Frame) = .{},

    assigned: std.AutoArrayHashMapUnmanaged(SymbolRef, void) = .{},
    captured: std.AutoArrayHashMapUnmanaged(SymbolRef, NodeRef) = .{},
    captured_twice: std.AutoArrayHashMapUnmanaged(SymbolRef, void) = .{},

    fns: std.AutoArrayHashMapUnmanaged(NodeRef, std.AutoArrayHashMapUnmanaged(SymbolRef, void)) = .{},

    ignored: ?*const std.AutoArrayHashMapUnmanaged(SymbolRef, void) = null,

    pub fn transform(ast: *AstData, binder: *const Binder) !parser.PrintResult {
        const nodes = try ast.nodes.clone();
        ast.nodes = nodes;
        var r = std.AutoArrayHashMap(NodeRef, NodeRef).init(getAllocator());
        defer r.deinit();

        var factory = parser.Factory{ .nodes = &ast.nodes };
        var v = SynthInstrumenter{
            .alloc = getAllocator(),
            .ast = ast,
            .nodes = &ast.nodes,
            .factory = &factory,
            .replacements = &r,
            .binder = binder,
        };
        defer v.deinit();

        try v.visit(ast.nodes.at(ast.start), ast.start);
        v.transforming = true;
        try v.visit(ast.nodes.at(ast.start), ast.start);

        return try parser.printWithOptions(ast.*, .{
            .replacements = &r,
            .emit_source_map = true,
        });
    }

    fn deinit(self: *@This()) void {
        self.frames.deinit(self.alloc);
    }

    fn markAssignedIfIdent(self: *@This(), ref: NodeRef) anyerror!void {
        if (ref == 0) return;
        const n = self.nodes.at(ref);
        if (n.kind != .identifier) return;
        const sym = self.binder.getSymbol(ref) orelse return;
        if (sym == 0) return;
        if (self.ignored) |ig| if (ig.contains(sym)) return;
        try self.assigned.put(self.alloc, sym, {});
    }

    fn collectForBindings(self: *@This(), var_stmt_ref: NodeRef, local: *std.AutoArrayHashMapUnmanaged(SymbolRef, void)) anyerror!void {
        const decls_head = maybeUnwrapRef(self.nodes.at(var_stmt_ref)) orelse return;
        var it = NodeIterator.init(self.nodes, decls_head);
        while (it.nextRef()) |decl_ref| {
            const sym = self.binder.getSymbol(decl_ref) orelse continue;
            if (sym == 0) continue;
            try local.put(self.alloc, sym, {});
        }
    }

    fn funcParamsHead(node: *const AstNode) NodeRef {
        return switch (node.kind) {
            .arrow_function, .constructor => getPackedData(node).left,
            else => getPackedData(node).right,
        };
    }

    fn funcBodyRef(node: *const AstNode) NodeRef {
        return switch (node.kind) {
            .arrow_function, .constructor => getPackedData(node).right,
            else => node.len,
        };
    }

    fn needsCell(self: *@This(), sym_ref: SymbolRef) bool {
        return self.assigned.contains(sym_ref) and self.captured_twice.contains(sym_ref);
    }

    fn getCellOrSymbol(self: *@This(), sym_ref: SymbolRef) !NodeRef {
        const ident = parser.getIdentFromSymbol(self.binder, sym_ref) orelse {
            std.debug.print("{}\n",.{self.nodes.at(self.binder.symbols.at(sym_ref).declaration).kind});
            unreachable;
        };
        if (!self.needsCell(sym_ref)) {
            const ref = try self.factory.cloneNode(ident);
            self.nodes.at(ref).extra_data = 0;
            self.nodes.at(ref).next = 0;
            return ref;
        }
        var buf: [256]u8 = undefined;
        const name = try std.fmt.bufPrint(&buf, "_c_{s}", .{getSlice(ident, u8)});
        return try self.factory.createIdentifier(name);
    }

    fn buildSynthData(
        self: *@This(),
        decl_ref: NodeRef,
        captured: *const std.AutoArrayHashMapUnmanaged(SymbolRef, void),
    ) !NodeRef {
        var cap = std.ArrayListUnmanaged(NodeRef){};
        defer cap.deinit(self.alloc);
        for (captured.keys()) |k| {
            try cap.append(self.alloc, try self.getCellOrSymbol(k));
        }
        const cap_arr = try self.factory.createArrayLiteralExpression(cap.items);
        const props: []const NodeRef = &.{
            // try self.factory.createPropertyAssignment(
            //     try self.factory.createIdentifier("valueType"),
            //     try self.factory.createStringLiteral("function"),
            // ),
            try self.factory.createPropertyAssignment(
                try self.factory.createIdentifier("program"),
                try self.getMicroProgramStr(decl_ref, captured)
            ),
            try self.factory.createPropertyAssignment(try self.factory.createIdentifier("captured"), cap_arr),
        };
        const obj = try self.factory.createObjectLiteralExpression(props);
        return obj;
    }

    fn buildSynthDataFn(
        self: *@This(),
        decl_ref: NodeRef,
        captured: *const std.AutoArrayHashMapUnmanaged(SymbolRef, void),
    ) !NodeRef {
        const synth_data = try self.buildSynthData(decl_ref, captured);
        return try self.factory.createArrowFunction(0, try self.factory.createParenthesizedExpression(synth_data), 0);
    }

    fn buildMicroProgram(self: *@This(), decl_ref: NodeRef, captured: *const std.AutoArrayHashMapUnmanaged(SymbolRef, void)) !NodeRef {
        var params = std.ArrayListUnmanaged(NodeRef){};
        defer params.deinit(self.alloc);
        for (captured.keys()) |k| {
            try params.append(self.alloc, try self.factory.createParameter(try self.getCellOrSymbol(k), 0));
        }

        const exp = try self.factory.cloneNodeRef(decl_ref);
        if (self.nodes.at(exp).kind == .function_declaration) {
            self.nodes.at(exp).kind = .function_expression;
            self.nodes.at(exp).flags &= ~@intFromEnum(parser.NodeFlags.@"export");
        }

        const ret = try self.factory.createReturnStatement(exp);
        const body: []const NodeRef = &.{ret};

        return try self.factory.createFunctionDeclaration(
            0,
            params.items,
            body,
        );
    }

    fn getMicroProgramStr(self: *@This(), decl_ref: NodeRef, captured: *const std.AutoArrayHashMapUnmanaged(SymbolRef, void)) !NodeRef {
        const target = try self.buildMicroProgram(decl_ref, captured);
        self.emitting = true;
        defer self.emitting = false;

        const save_replacements = self.replacements;
        defer self.replacements = save_replacements;
        var replacements = std.AutoArrayHashMap(NodeRef, NodeRef).init(self.alloc);
        defer replacements.deinit();
        self.replacements = &replacements;

        var d = self.ast.*;
        d.start = target;

        try self.visit(self.nodes.at(self.nodes.at(target).len), self.nodes.at(target).len);
        d.nodes = self.nodes.*;

        const res = try parser.printWithOptions(d, .{
            .replacements = self.replacements,
        });
        var escaped = try std.ArrayList(u8).initCapacity(self.alloc, res.contents.len+1024);
        for (res.contents) |c| {
            switch (c) {
                '\\', '\'' => try escaped.append('\\'),
                '\n' => {
                    try escaped.appendSlice("\\n");
                    continue;
                },
                else => {},
            }
            try escaped.append(c);
        }
        return try self.factory.createStringLiteralAllocated(escaped.items);
    }

    fn visitFunction(self: *@This(), node: *const AstNode, ref: NodeRef) anyerror!void {
        if (funcBodyRef(node) == 0) return;
        if (self.transforming) {
            const captures = self.fns.get(ref) orelse unreachable;
            const move_key = try self.factory.createCallExpression(
                try self.factory.createPropertyAccessExpression(try self.factory.createIdentifier("Symbol"), "for"),
                &.{try self.factory.createStringLiteral("__moveable__")},
            );
            const save_rebindings = self.rebindings;
            defer self.rebindings = save_rebindings;
            self.rebindings = .{};
            try forEachChild(self.nodes, node, self);
            try self.drainRebindings(funcBodyRef(node));

            const next = node.next;
            const clone = try self.factory.cloneNodeRef(ref);
            self.nodes.at(clone).next = 0;
            const synth_data = try self.buildSynthDataFn(ref, &captures);
            switch (node.kind) {
                .arrow_function, .function_expression => {
                    const o = try self.factory.createObjectLiteralExpression(&.{
                        try self.factory.createPropertyAssignment(
                            try self.factory.createComputedName(move_key),
                            synth_data,
                        )
                    });
                    const call = try self.factory.createCallExpression(
                        try self.factory.createPropertyAccessExpression(try self.factory.createIdentifier("Object"), "assign"),
                        &.{clone, o},
                    );
                    self.nodes.at(call).next = next;
                    try self.replacements.put(ref, call);
                },
                .function_declaration => {
                    const assign = try self.factory.createAssignmentStatement(
                        try self.factory.createElementAccessExpression(try self.factory.cloneNodeRef(getPackedData(node).left), move_key),
                        synth_data,
                    );
                    self.nodes.at(clone).next = assign;
                    self.nodes.at(assign).next = next;
                    try self.replacements.put(ref, clone);
                },
                else => unreachable,
            }
            return;
        }

        self.depth += 1;
        defer self.depth -= 1;

        try self.frames.append(self.alloc, .{
            .decl_ref = ref,
            .kind = node.kind,
            .threshold = self.depth,
        });

        var pit = NodeIterator.init(self.nodes, funcParamsHead(node));
        while (pit.nextPair()) |p| try self.visit(p[0], p[1]);

        const body = funcBodyRef(node);
        if (body != 0) {
            const bn = self.nodes.at(body);
            if (bn.kind == .block) {
                var bit = NodeIterator.init(self.nodes, maybeUnwrapRef(bn) orelse 0);
                while (bit.nextPair()) |p| try self.visit(p[0], p[1]);
            } else {
                try self.visit(bn, body);
            }
        }
        const frame = self.frames.pop();
        try self.fns.put(self.alloc, ref, frame.captures);
    }

    // takes into account replacements
    fn resolveRef(self: *@This(), ref: NodeRef) NodeRef {
        return self.replacements.get(ref) orelse ref;
    }

    fn drainRebindings(self: *@This(), _ref: NodeRef) !void {
        std.debug.assert(_ref != 0);
        if (self.rebindings.items.len == 0) return; 
        const ref = self.resolveRef(_ref);
        switch (self.nodes.at(ref).kind) {
            .variable_statement => {
                const clone = try self.factory.cloneNodeRef(ref);
                try self.replacements.put(ref, clone);
                var t = clone;
                for (self.rebindings.items) |x| {
                    self.nodes.at(t).next = x;
                    t = x;
                }
                self.nodes.at(t).next = self.nodes.at(ref).next;
            },
            .block => {
                const first = maybeUnwrapRef(self.nodes.at(ref)) orelse 0;
                var head = if (first != 0) try self.factory.cloneNodeRef(first) else 0;
                var tail = head;
                if (first != 0) try self.replacements.put(first, head);
                for (self.rebindings.items) |x| {
                    if (tail != 0) self.nodes.at(tail).next = x;
                    if (head == 0) head = x;
                    tail = x;
                }
                if (first == 0) {
                    const clone = try self.factory.cloneNodeRef(ref);
                    self.nodes.at(clone).data = head;
                    try self.replacements.put(_ref, clone);
                } else {
                    self.nodes.at(tail).next = self.nodes.at(first).next;
                }
            },
            else => {
                // arrow fn concise body
                const ret = try self.factory.createReturnStatement(try self.factory.cloneNodeRef(ref));
                var head: NodeRef = 0;
                var tail = head;
                for (self.rebindings.items) |x| {
                    if (tail != 0) self.nodes.at(tail).next = x;
                    if (head == 0) head = x;
                    tail = x;
                }
                self.nodes.at(tail).next = ret;
                const blk = try self.factory.createBlock(head);
                try self.replacements.put(_ref, blk);
            },
        }

        self.rebindings.clearRetainingCapacity();
    }

    fn classifyReference(self: *@This(), ref: NodeRef) !void {
        const sym = self.binder.getSymbol(ref) orelse return;
        if (sym == 0) return;
        const s = self.binder.symbols.at(sym);
        if (s.hasFlag(.type)) return;
        if (s.hasFlag(.late_bound) or s.hasFlag(.imported) or s.hasFlag(.exported)) return;
        if (self.nodes.at(s.declaration).hasFlag(.declare)) return;

        if (self.transforming) {
            if (!self.needsCell(sym)) return;
            if (parser.getIdentFromSymbol(self.binder, sym) == self.nodes.at(ref)) {
                var buf: [256]u8 = undefined;
                const name = try std.fmt.bufPrint(&buf, "_c_{s}", .{getSlice(self.nodes.at(ref), u8)});
                const new_ident = try self.factory.createIdentifier(name);
                try self.rebindings.append(self.alloc, try self.factory.createConstVariable(new_ident, try self.factory.createArrayLiteralExpression(&.{try self.factory.createIdentifier(getSlice(self.nodes.at(ref), u8))})));
            } else {
                const z = try self.factory.createElementAccessExpression(try self.getCellOrSymbol(sym), @as(i64, 0));
                try self.replacements.put(ref, z);
                self.nodes.at(z).next = self.nodes.at(ref).next;
            }
            return;
        }

        const depth = s.getScopeDepth();
        for (self.frames.items) |*frame| {
            if (depth < frame.threshold) {
                if (s.declaration == frame.decl_ref) continue;
                try frame.captures.put(self.alloc, sym, {});
                if ((self.captured.get(sym) orelse frame.decl_ref) != frame.decl_ref) {
                    try self.captured_twice.put(self.alloc, sym, {});
                } else {
                    try self.captured.put(self.alloc, sym, frame.decl_ref);
                }
            }
        }
    }

    pub fn visit(self: *@This(), node: *const AstNode, ref: NodeRef) anyerror!void {
        if (ref == 0) return;
        if (self.emitting) {
            if (node.kind == .variable_statement) {
                try forEachChild(self.nodes, node, self);
                return try self.drainRebindings(ref);
            }
            if (node.kind == .arrow_function or node.kind == .function_expression or node.kind == .function_declaration) {
                const save_rebindings = self.rebindings;
                defer self.rebindings = save_rebindings;
                self.rebindings = .{};
                try forEachChild(self.nodes, node, self);
                return try self.drainRebindings(funcBodyRef(node));
            }
            if (node.kind == .shorthand_property_assignment) {
                const inner_ref = maybeUnwrapRef(node) orelse unreachable;
                try self.visit(self.nodes.at(inner_ref), inner_ref);
                if (self.replacements.get(inner_ref)) |v| {
                    _ = self.replacements.swapRemove(inner_ref);
                    const assign = try self.factory.createPropertyAssignment(ref, v);
                    self.nodes.at(assign).next = node.next;
                    try self.replacements.put(ref, assign);
                }
                return;
            }
            if (node.kind != .identifier) return try forEachChild(self.nodes, node, self);
            if (self.binder.getSymbol(ref)) |sym_ref| {
                if (sym_ref == 0) return;
                const sym = self.binder.symbols.at(sym_ref);
                if (sym.hasFlag(.late_bound) or sym.hasFlag(.imported) or sym.hasFlag(.exported)) return;
                if (self.needsCell(sym_ref)) {
                    if (parser.getIdentFromSymbol(self.binder, sym_ref) == node) {
                        var buf: [256]u8 = undefined;
                        const name = try std.fmt.bufPrint(&buf, "_c_{s}", .{getSlice(self.nodes.at(ref), u8)});
                        const new_ident = try self.factory.createIdentifier(name);
                        try self.rebindings.append(self.alloc, try self.factory.createConstVariable(new_ident, try self.factory.createArrayLiteralExpression(&.{try self.factory.createIdentifier(getSlice(self.nodes.at(ref), u8))})));
                        return;
                    }
                    const access = try self.factory.createElementAccessExpression(try self.getCellOrSymbol(sym_ref), @as(i64, 0));
                    self.nodes.at(access).next = node.next;
                    try self.replacements.put(ref, access);
                }
            }
            return;
        }

        switch (node.kind) {
            .function_declaration,
            .function_expression,
            .arrow_function,
            .method_declaration,
            .constructor,
            .get_accessor,
            .set_accessor,
            => try self.visitFunction(node, ref),

            // .class_declaration, .class_expression => try self.enterClass(node, ref),

            .block => {
                self.depth += 1;
                defer self.depth -= 1;
                try forEachChild(self.nodes, node, self);
            },

            .for_statement => {
                const d = getPackedData(node);
                const has_var = d.left != 0 and self.nodes.at(d.left).kind == .variable_statement;
                if (!has_var) {
                    return try forEachChild(self.nodes, node, self);
                }

                self.depth += 1;
                defer self.depth -= 1;
                {
                    var loop_binds = std.AutoArrayHashMapUnmanaged(SymbolRef, void){};
                    defer loop_binds.deinit(self.alloc);
                    try self.collectForBindings(d.left, &loop_binds);
                    const prev_ignore = self.ignored;
                    self.ignored = if (has_var) &loop_binds else prev_ignore;
                    defer self.ignored = prev_ignore;
                    if (d.left != 0) try self.visit(self.nodes.at(d.left), d.left);
                    if (d.right != 0) try self.visit(self.nodes.at(d.right), d.right);
                    if (node.len != 0) try self.visit(self.nodes.at(node.len), node.len);
                }
                if (node.extra_data != 0) try self.visit(self.nodes.at(node.extra_data), node.extra_data);
            },

            .for_of_statement, .for_in_statement => {
                const d = getPackedData(node);
                const has_var = d.left != 0 and self.nodes.at(d.left).kind == .variable_statement;
                if (!has_var) {
                    return try forEachChild(self.nodes, node, self);
                }
                self.depth += 1;
                defer self.depth -= 1;
                try forEachChild(self.nodes, node, self);
            },

            .variable_statement => {
                try forEachChild(self.nodes, node, self);
                try self.drainRebindings(ref);
            },

            .shorthand_property_assignment => {
                const inner_ref = maybeUnwrapRef(node) orelse unreachable;
                try self.classifyReference(inner_ref);
                if (self.replacements.get(inner_ref)) |v| {
                    _ = self.replacements.swapRemove(inner_ref);
                    const assign = try self.factory.createPropertyAssignment(ref, v);
                    self.nodes.at(assign).next = node.next;
                    try self.replacements.put(ref, assign);
                }
            },

            .binary_expression => {
                if (parser.isAssignmentOp(@enumFromInt(node.len))) {
                    try self.markAssignedIfIdent(getPackedData(node).left);
                }
                try forEachChild(self.nodes, node, self);
            },

            .prefix_unary_expression => {
                const d = getPackedData(node);
                const op: SyntaxKind = @enumFromInt(d.left);
                if (op == .plus_plus_token or op == .minus_minus_token) {
                    try self.markAssignedIfIdent(d.right);
                }
                try forEachChild(self.nodes, node, self);
            },

            .postfix_unary_expression => {
                const d = getPackedData(node);
                const op: SyntaxKind = @enumFromInt(d.right);
                if (op == .plus_plus_token or op == .minus_minus_token) {
                    try self.markAssignedIfIdent(d.left);
                }
                try forEachChild(self.nodes, node, self);
            },

            .identifier => try self.classifyReference(ref),

            .this_keyword, .super_keyword => {},

            else => try forEachChild(self.nodes, node, self),
        }
    }
};

const Analyzer = struct {
    binder: *const Binder,
    nodes: *const BumpAllocator(AstNode),
    alloc: std.mem.Allocator,

    // Mirrors the binder's value-scope depth. The binder pushes a scope for the
    // source file (depth 1), so module-level symbols live at depth 1.
    value_depth: u16 = 1,

    frames: std.ArrayListUnmanaged(Frame) = .{},
    results: std.ArrayListUnmanaged(FunctionCapture) = .{},
    class_results: std.ArrayListUnmanaged(ClassCapture) = .{},
    // Captured-symbol display names, taken from the first reference we observe.
    // Robust for globals/imports whose declaration node we can't resolve.
    names: std.AutoHashMapUnmanaged(SymbolRef, []const u8) = .{},

    // Symbols reassigned via `=`-family or `++`/`--`, *excluding* mutations of a
    // loop's own iteration symbol in the loop head (which just advances the
    // per-iteration binding). Transferred out via `Analysis`.
    assigned: std.AutoArrayHashMapUnmanaged(SymbolRef, void) = .{},
    // Symbols declared as a `for (...)` loop binding.
    for_bindings: std.AutoArrayHashMapUnmanaged(SymbolRef, void) = .{},
    // While visiting a for-loop head, the loop's own bindings — mutations of
    // these are ignored (they advance the per-iteration binding, not the cell).
    head_ignore: ?*const std.AutoArrayHashMapUnmanaged(SymbolRef, void) = null,

    fn deinit(self: *Analyzer) void {
        self.frames.deinit(self.alloc);
        self.names.deinit(self.alloc);
        // `results` / `assigned` / `for_bindings` ownership is transferred out.
    }

    fn markAssignedIfIdent(self: *Analyzer, ref: NodeRef) anyerror!void {
        if (ref == 0) return;
        const n = self.nodes.at(ref);
        if (n.kind != .identifier) return;
        const sym = self.binder.getSymbol(ref) orelse return;
        if (sym == 0) return;
        if (self.head_ignore) |ig| if (ig.contains(sym)) return;
        try self.assigned.put(self.alloc, sym, {});
    }

    /// Records a for-loop's bindings into the global `for_bindings` set and,
    /// optionally, into a per-loop `local` set used for head suppression.
    fn collectForBindings(self: *Analyzer, var_stmt_ref: NodeRef, local: ?*std.AutoArrayHashMapUnmanaged(SymbolRef, void)) anyerror!void {
        const decls_head = maybeUnwrapRef(self.nodes.at(var_stmt_ref)) orelse return;
        var it = NodeIterator.init(self.nodes, decls_head);
        while (it.nextRef()) |decl_ref| {
            const sym = self.binder.getSymbol(decl_ref) orelse continue;
            if (sym == 0) continue;
            try self.for_bindings.put(self.alloc, sym, {});
            if (local) |l| try l.put(self.alloc, sym, {});
        }
    }

    fn funcParamsHead(node: *const AstNode) NodeRef {
        return switch (node.kind) {
            .arrow_function, .constructor => getPackedData(node).left,
            else => getPackedData(node).right,
        };
    }

    fn funcBodyRef(node: *const AstNode) NodeRef {
        return switch (node.kind) {
            .arrow_function, .constructor => getPackedData(node).right,
            else => node.len,
        };
    }

    fn funcName(self: *Analyzer, node: *const AstNode) ?[]const u8 {
        switch (node.kind) {
            .function_declaration, .function_expression, .method_declaration => {
                const name_ref = getPackedData(node).left;
                if (name_ref == 0) return null;
                const nn = self.nodes.at(name_ref);
                if (nn.kind != .identifier) return null;
                return getSlice(nn, u8);
            },
            else => return null,
        }
    }

    /// Pushes a frame, walks the function's params + body, then pops and returns
    /// the resulting capture. Does not append to `results` (caller decides).
    fn analyzeFunction(self: *Analyzer, node: *const AstNode, ref: NodeRef) anyerror!FunctionCapture {
        self.value_depth += 1;
        defer self.value_depth -= 1;
        // Prefer an authoritative threshold from the first simple parameter
        // (params are bound at the body scope). Falls back to our mirrored
        // counter for param-less functions.
        // var threshold = self.value_depth;
        // const params_head = funcParamsHead(node);
        // if (params_head != 0) {
        //     if (self.binder.getSymbol(params_head)) |psym| {
        //         if (psym != 0 and !self.binder.symbols.at(psym).hasFlag(.type)) {
        //             threshold = self.binder.symbols.at(psym).getScopeDepth();
        //         }
        //     }
        // }

        // A named function decl/expr is bound to its own name symbol.
        const self_symbol: SymbolRef = switch (node.kind) {
            .function_declaration, .function_expression => self.binder.getSymbol(ref) orelse 0,
            else => 0,
        };

        try self.frames.append(self.alloc, .{
            .decl_ref = ref,
            .kind = node.kind,
            .name = self.funcName(node),
            .self_symbol = self_symbol,
            .threshold = self.value_depth + 1,
        });

        var pit = NodeIterator.init(self.nodes, funcParamsHead(node));
        while (pit.nextPair()) |p| try self.visit(p[0], p[1]);

        // Body. We visit the body block's statements directly so the function's
        // own body block is not counted as an extra nested scope.
        const body = funcBodyRef(node);
        if (body != 0) {
            const bn = self.nodes.at(body);
            if (bn.kind == .block) {
                var bit = NodeIterator.init(self.nodes, maybeUnwrapRef(bn) orelse 0);
                while (bit.nextPair()) |p| try self.visit(p[0], p[1]);
            } else {
                try self.visit(bn, body);
            }
        }

        var frame = self.frames.pop();

        return .{
            .decl_ref = frame.decl_ref,
            .kind = frame.kind,
            .name = frame.name,
            .self_symbol = frame.self_symbol,
            .captured = try self.finishFrame(&frame),
        };
    }

    /// Materializes a frame's captures into an owned slice and frees the frame's
    /// hash map.
    fn finishFrame(self: *Analyzer, frame: *Frame) anyerror![]const CapturedSymbol {
        const list = try self.alloc.alloc(CapturedSymbol, frame.captures.count());
        for (frame.captures.keys(), 0..) |sym, i| {
            list[i] = .{ .symbol = sym, .name = self.names.get(sym) orelse "<unknown>" };
        }
        frame.captures.deinit(self.alloc);
        return list;
    }

    fn enterFunction(self: *Analyzer, node: *const AstNode, ref: NodeRef) anyerror!void {
        const cap = try self.analyzeFunction(node, ref);
        try self.results.append(self.alloc, cap);
    }

    fn classMemberName(self: *Analyzer, name_ref: NodeRef) ?[]const u8 {
        if (name_ref == 0) return null;
        const nn = self.nodes.at(name_ref);
        if (nn.kind != .identifier) return null; // skip computed/private
        return getSlice(nn, u8);
    }

    fn isPrivateMember(self: *Analyzer, name_ref: NodeRef) bool {
        if (name_ref == 0) return false;
        return self.nodes.at(name_ref).kind == .private_identifier;
    }

    /// A `[expr]` member name is evaluated when the class is defined, so its
    /// expression is a construction-time reference. No-op for plain names.
    fn visitComputedName(self: *Analyzer, name_ref: NodeRef) anyerror!void {
        if (name_ref == 0) return;
        const n = self.nodes.at(name_ref);
        if (n.kind != .computed_property_name) return;
        const expr = maybeUnwrapRef(n) orelse return;
        try self.visit(self.nodes.at(expr), expr);
    }

    /// Analyzes a class: construction-time captures (field initializers, static
    /// blocks, constructor) roll into the class itself; each non-private,
    /// non-constructor method is captured separately.
    fn enterClass(self: *Analyzer, node: *const AstNode, ref: NodeRef) anyerror!void {
        const d = getPackedData(node);
        const members_head = d.right;

        const class_sym = self.binder.getSymbol(ref) orelse 0;

        // Construction runs effectively one scope in (like the constructor), so
        // anything from the enclosing scope outward is a capture.
        const class_frame = Frame{
            .decl_ref = ref,
            .kind = node.kind,
            .name = self.classMemberName(d.left),
            .self_symbol = class_sym,
            .threshold = self.value_depth + 1,
        };
        try self.frames.append(self.alloc, class_frame);

        // Heritage / extends clause (node.len) is construction-time.
        if (node.len != 0) try self.visit(self.nodes.at(node.len), node.len);

        // Pass 1: construction members → class frame. This includes computed
        // member-name expressions (evaluated when the class is defined).
        var it1 = NodeIterator.init(self.nodes, members_head);
        while (it1.nextPair()) |p| {
            const el = p[0];
            switch (el.kind) {
                .property_declaration => {
                    try self.visitComputedName(getPackedData(el).left);
                    if (self.isPrivateMember(getPackedData(el).left)) continue;
                    const init_ref = getPackedData(el).right;
                    if (init_ref != 0) try self.visit(self.nodes.at(init_ref), init_ref);
                },
                .method_declaration, .get_accessor, .set_accessor => {
                    try self.visitComputedName(getPackedData(el).left);
                },
                .class_static_block_declaration => {
                    var bit = NodeIterator.init(self.nodes, maybeUnwrapRef(el) orelse 0);
                    while (bit.nextPair()) |s| try self.visit(s[0], s[1]);
                },
                .constructor => {
                    // Captures roll into the class frame; the standalone result
                    // is discarded.
                    _ = try self.analyzeFunction(el, p[1]);
                },
                else => {},
            }
        }

        // Pop the class frame and record the class's construction captures.
        var popped = self.frames.pop();
        const class_captured = try self.finishFrame(&popped);

        // Pass 2: methods/accessors (without the class frame on the stack) so
        // their captures stay method-local.
        var methods = std.ArrayListUnmanaged(MethodCapture){};
        var it2 = NodeIterator.init(self.nodes, members_head);
        while (it2.nextPair()) |p| {
            const el = p[0];
            switch (el.kind) {
                .method_declaration, .get_accessor, .set_accessor => {
                    const name_ref = getPackedData(el).left;
                    if (self.isPrivateMember(name_ref)) continue;
                    const is_computed = name_ref != 0 and self.nodes.at(name_ref).kind == .computed_property_name;
                    const mname = if (is_computed) "__computed" else (self.classMemberName(name_ref) orelse continue);
                    const cap = try self.analyzeFunction(el, p[1]);
                    try methods.append(self.alloc, .{
                        .name = mname,
                        .name_ref = name_ref,
                        .is_computed = is_computed,
                        .is_static = el.hasFlag(.static),
                        .captured = cap.captured,
                    });
                },
                else => {},
            }
        }

        try self.class_results.append(self.alloc, .{
            .decl_ref = ref,
            .name = class_frame.name,
            .self_symbol = class_sym,
            .captured = class_captured,
            .methods = try methods.toOwnedSlice(self.alloc),
        });
    }

    fn classifyReference(self: *Analyzer, node: *const AstNode, ref: NodeRef) anyerror!void {
        const sym = self.binder.getSymbol(ref) orelse return;
        if (sym == 0) return;
        const s = self.binder.symbols.at(sym);
        if (s.hasFlag(.type)) return; // type-only reference, ignore

        if (!self.names.contains(sym)) {
            try self.names.put(self.alloc, sym, getSlice(node, u8));
        }

        const depth = s.getScopeDepth();
        for (self.frames.items) |*frame| {
            if (depth < frame.threshold) {
                try frame.captures.put(self.alloc, sym, {});
            }
        }
    }

    pub fn visit(self: *Analyzer, node: *const AstNode, ref: NodeRef) anyerror!void {
        if (ref == 0) return;

        switch (node.kind) {
            .function_declaration,
            .function_expression,
            .arrow_function,
            .method_declaration,
            .constructor,
            .get_accessor,
            .set_accessor,
            => try self.enterFunction(node, ref),

            .class_declaration, .class_expression => try self.enterClass(node, ref),

            .block => {
                self.value_depth += 1;
                defer self.value_depth -= 1;
                var it = NodeIterator.init(self.nodes, maybeUnwrapRef(node) orelse 0);
                while (it.nextPair()) |p| try self.visit(p[0], p[1]);
            },

            .for_statement => {
                const d = getPackedData(node);
                const has_var = d.left != 0 and self.nodes.at(d.left).kind == .variable_statement;
                var loop_binds = std.AutoArrayHashMapUnmanaged(SymbolRef, void){};
                defer loop_binds.deinit(self.alloc);
                if (has_var) {
                    self.value_depth += 1;
                    try self.collectForBindings(d.left, &loop_binds);
                }
                defer if (has_var) {
                    self.value_depth -= 1;
                };

                // Head = init (d.left), condition (d.right), incrementor (node.len).
                // Suppress mutation marking of the loop's own bindings here.
                const prev_ignore = self.head_ignore;
                self.head_ignore = if (has_var) &loop_binds else prev_ignore;
                if (d.left != 0) try self.visit(self.nodes.at(d.left), d.left);
                if (d.right != 0) try self.visit(self.nodes.at(d.right), d.right);
                if (node.len != 0) try self.visit(self.nodes.at(node.len), node.len);
                self.head_ignore = prev_ignore;

                // Body. Mutations here (e.g. `() => i++`) *do* make the binding a cell.
                if (node.extra_data != 0) try self.visit(self.nodes.at(node.extra_data), node.extra_data);
            },

            .for_of_statement, .for_in_statement => {
                const d = getPackedData(node);
                const has_var = d.left != 0 and self.nodes.at(d.left).kind == .variable_statement;
                if (has_var) {
                    self.value_depth += 1;
                    try self.collectForBindings(d.left, null);
                }
                defer if (has_var) {
                    self.value_depth -= 1;
                };
                try forEachChild(self.nodes, node, self);
            },

            .catch_clause => {
                self.value_depth += 1;
                defer self.value_depth -= 1;
                try forEachChild(self.nodes, node, self);
            },

            // Skip the type annotation (`node.len`) to avoid type-position noise.
            .parameter, .variable_declaration => {
                const d = getPackedData(node);
                if (d.left != 0) try self.visit(self.nodes.at(d.left), d.left);
                if (d.right != 0) try self.visit(self.nodes.at(d.right), d.right);
            },

            .binary_expression => {
                if (parser.isAssignmentOp(@enumFromInt(node.len))) {
                    try self.markAssignedIfIdent(getPackedData(node).left);
                }
                try forEachChild(self.nodes, node, self);
            },

            .prefix_unary_expression => {
                const d = getPackedData(node);
                const op: SyntaxKind = @enumFromInt(d.left);
                if (op == .plus_plus_token or op == .minus_minus_token) {
                    try self.markAssignedIfIdent(d.right);
                }
                try forEachChild(self.nodes, node, self);
            },

            .postfix_unary_expression => {
                const d = getPackedData(node);
                const op: SyntaxKind = @enumFromInt(d.right);
                if (op == .plus_plus_token or op == .minus_minus_token) {
                    try self.markAssignedIfIdent(d.left);
                }
                try forEachChild(self.nodes, node, self);
            },

            .identifier => try self.classifyReference(node, ref),

            // `this`/`super` are intentionally unsupported in this pass.
            .this_keyword, .super_keyword => {},

            else => try forEachChild(self.nodes, node, self),
        }
    }
};

/// Analyze every function-like node in `ast`, returning captures in the order
/// functions *finish* being visited (innermost first). Caller owns the result.
pub fn analyzeFile(binder: *const Binder, ast: *AstData) !Analysis {
    var a = Analyzer{
        .binder = binder,
        .nodes = &ast.nodes,
        .alloc = getAllocator(),
    };
    defer a.deinit();

    const root = ast.start;
    try a.visit(ast.nodes.at(root), root);

    return .{
        .results = try a.results.toOwnedSlice(a.alloc),
        .classes = try a.class_results.toOwnedSlice(a.alloc),
        .assigned = a.assigned,
        .for_bindings = a.for_bindings,
    };
}

/// Holds derived facts (mutability, circularity) and a registry of "cells" —
/// the `let __symbolN; const __getSymbolN = () => (__symbolN ??= { x })` boxes
/// used to share mutable / circular captures by reference.
pub const Instrumenter = struct {
    binder: *const Binder,
    analysis: *const Analysis,
    alloc: std.mem.Allocator,

    // self_symbol -> its captured symbols, for circular-capture detection.
    fn_deps: std.AutoArrayHashMapUnmanaged(SymbolRef, []const CapturedSymbol) = .{},
    // captured symbol -> assigned cell index.
    cells: std.AutoArrayHashMapUnmanaged(SymbolRef, u32) = .{},

    fn init(alloc: std.mem.Allocator, binder: *const Binder, analysis: *const Analysis) !Instrumenter {
        var self = Instrumenter{ .binder = binder, .analysis = analysis, .alloc = alloc };
        for (analysis.results) |r| {
            if (r.self_symbol != 0) {
                try self.fn_deps.put(alloc, r.self_symbol, r.captured);
            }
        }
        return self;
    }

    fn deinit(self: *Instrumenter) void {
        self.fn_deps.deinit(self.alloc);
        self.cells.deinit(self.alloc);
    }

    fn isMutable(self: *Instrumenter, sym: SymbolRef) bool {
        if (!self.binder.symbols.at(sym).hasFlag(.let_binding)) return false;
        if (self.analysis.for_bindings.contains(sym)) {
            return self.analysis.assigned.contains(sym);
        }
        return true;
    }

    fn transitiveContains(self: *Instrumenter, start: SymbolRef, target: SymbolRef, depth: u32) bool {
        if (depth > 64) return false; // guard pathological graphs
        const deps = self.fn_deps.get(start) orelse return false;
        for (deps) |c| {
            if (c.symbol == target) return true;
            if (self.transitiveContains(c.symbol, target, depth + 1)) return true;
        }
        return false;
    }

    /// `captured` (referenced inside the function owning `self_symbol`) is
    /// circular if its own dependency closure loops back to that function.
    fn isCircular(self: *Instrumenter, captured: SymbolRef, owner: SymbolRef) bool {
        if (owner == 0) return false;
        if (captured == owner) return true;
        return self.transitiveContains(captured, owner, 0);
    }

    fn needsCell(self: *Instrumenter, captured: SymbolRef, owner: SymbolRef) bool {
        return self.isMutable(captured) or self.isCircular(captured, owner);
    }

    /// Returns the cell index for `sym`, allocating a fresh one on first use.
    /// `is_new` is set when this call created the cell.
    fn cellIndex(self: *Instrumenter, sym: SymbolRef, is_new: *bool) !u32 {
        if (self.cells.get(sym)) |idx| {
            is_new.* = false;
            return idx;
        }
        const idx: u32 = @intCast(self.cells.count());
        try self.cells.put(self.alloc, sym, idx);
        is_new.* = true;
        return idx;
    }
};

/// Builds: `function _(c0, c1, ...) { return <fn as expression> }`
fn buildMicroProgram(factory: *Factory, capture: FunctionCapture) !NodeRef {
    var params = std.ArrayList(NodeRef).init(getAllocator());
    defer params.deinit();
    for (capture.captured) |c| {
        try params.append(try factory.createParameter(try factory.createIdentifier(c.name), 0));
    }

    // Re-emit the original function in expression position.
    const fn_expr = try factory.cloneNodeRef(capture.decl_ref);
    factory.nodes.at(fn_expr).kind = .function_expression;

    const ret = try factory.createReturnStatement(fn_expr);
    const body: []const NodeRef = &.{ret};
    const params_list: NodeRef = if (params.items.len == 0) 0 else try factory.createList(params.items);

    return try factory.createFunctionDeclaration(
        try factory.createIdentifier("_"),
        params_list,
        body,
    );
}

fn buildCell(factory: *Factory, idx: u32, name: []const u8) !NodeRef {
    var buf: [64]u8 = undefined;
    const cell_var = try std.fmt.bufPrint(&buf, "__symbol{d}", .{idx});
    const let_stmt = try factory.createLetVariable(try factory.createIdentifier(cell_var), 0);

    const shorthand = try factory.createShorthandPropertyAssignment(try factory.createIdentifier(name));
    const props: []const NodeRef = &.{shorthand};
    const obj = try factory.createObjectLiteralExpression(props);
    const coalesce = try factory.createBinaryExpression(
        try factory.createIdentifier(cell_var),
        .question_question_equals_token,
        obj,
    );
    const arrow = try factory.createArrowFunction(0, try factory.createParenthesizedExpression(coalesce), 0);

    var getter_buf: [64]u8 = undefined;
    const getter = try std.fmt.bufPrint(&getter_buf, "__getSymbol{d}", .{idx});
    const const_stmt = try factory.createConstVariable(try factory.createIdentifier(getter), arrow);

    factory.nodes.at(let_stmt).next = const_stmt;
    return let_stmt;
}

fn buildCapturedEntry(factory: *Factory, inst: *Instrumenter, c: CapturedSymbol, owner: SymbolRef) !NodeRef {
    if (!inst.needsCell(c.symbol, owner)) {
        return factory.createIdentifier(c.name);
    }
    var is_new: bool = undefined;
    const idx = try inst.cellIndex(c.symbol, &is_new);
    var buf: [64]u8 = undefined;
    const getter = try std.fmt.bufPrint(&buf, "__getSymbol{d}", .{idx});
    const empty: []const NodeRef = &.{};
    return factory.createCallExpression(try factory.createIdentifier(getter), empty);
}

fn buildMoveableAssignment(
    factory: *Factory,
    inst: *Instrumenter,
    subject: NodeRef,
    module_name: []const u8,
    captured: []const CapturedSymbol,
    owner: SymbolRef,
) !NodeRef {
    const move_key_args: []const NodeRef = &.{try factory.createStringLiteral("__moveable__")};
    const move_key = try factory.createCallExpression(
        try factory.createPropertyAccessExpression(try factory.createIdentifier("Symbol"), "for"),
        move_key_args,
    );
    const lhs = try factory.createElementAccessExpression(subject, move_key);

    // import.meta.filename
    const meta = try factory.createPropertyAccessExpression(try factory.createIdentifier("import"), "meta");
    const filename = try factory.createPropertyAccessExpression(meta, "filename");
    const ptr_args: []const NodeRef = &.{ filename, try factory.createStringLiteral(module_name) };
    const module_ptr = try factory.createCallExpression(try factory.createIdentifier("__getPointer"), ptr_args);

    var cap_items = std.ArrayList(NodeRef).init(getAllocator());
    defer cap_items.deinit();
    for (captured) |c| {
        try cap_items.append(try buildCapturedEntry(factory, inst, c, owner));
    }
    const cap_arr = try factory.createArrayLiteralExpression(cap_items.items);

    const props: []const NodeRef = &.{
        try factory.createPropertyAssignment(
            try factory.createIdentifier("valueType"),
            try factory.createStringLiteral("function"),
        ),
        try factory.createPropertyAssignment(try factory.createIdentifier("module"), module_ptr),
        try factory.createPropertyAssignment(try factory.createIdentifier("captured"), cap_arr),
    };
    const obj = try factory.createObjectLiteralExpression(props);

    const arrow = try factory.createArrowFunction(0, try factory.createParenthesizedExpression(obj), 0);
    const assign = try factory.createBinaryExpression(lhs, .equals_token, arrow);
    return try factory.createExpressionStatement(assign);
}

/// Emits the cell declarations needed by `captured` (mutable/circular shares)
/// that haven't been emitted yet, printing each via the debug stream.
fn emitNeededCells(factory: *Factory, inst: *Instrumenter, ast: *AstData, captured: []const CapturedSymbol, owner: SymbolRef) !void {
    for (captured) |c| {
        if (!inst.needsCell(c.symbol, owner)) continue;
        if (inst.cells.contains(c.symbol)) continue;
        var is_new: bool = undefined;
        const idx = try inst.cellIndex(c.symbol, &is_new);
        const cell = try buildCell(factory, idx, c.name);
        // `buildCell` chains the `const` getter as the `let`'s `.next`; print
        // each statement on its own (the printer won't follow `.next`).
        var let_node = ast.nodes.at(cell).*;
        const const_ref = let_node.next;
        let_node.next = 0;
        const let_src = try parser.printInMemory(ast.*, let_node);
        const const_src = try parser.printInMemory(ast.*, ast.nodes.at(const_ref).*);
        std.debug.print("{s}\n{s}\n", .{ let_src, const_src });
    }
}


// 1. normalize all import declarations to the namespace form e.g. `import * as foo from 'foo'`
// 2. change the namespace identifier e.g. `__foo_namespace` instead of `foo`
// 3. emit a wrapped assignment `const foo = __wrapExports('foo', import.meta.__importer, __foo_namespace)
// 4. use wrapped namespace to create original bindings, e.g. for default use `__wrapExports('foo', import.meta.__importer, __foo_namespace).default`
pub const ImportTransformer = struct {
    allocator: std.mem.Allocator,
    factory: *Factory,
    spec_map: std.AutoHashMapUnmanaged(u32, NodeRef) = .{},

    // from main visitor
    replacements: *std.AutoArrayHashMap(NodeRef, NodeRef),

    fn getSpecIdent(this: *@This(), spec_ref: NodeRef) !NodeRef {
        const key = try parser.getHashFromModuleNode(this.factory.nodes.at(spec_ref));
        const entry = try this.spec_map.getOrPut(this.allocator, key);
        if (entry.found_existing) {
            return this.factory.cloneNodeRef(entry.value_ptr.*);
        }
        var buf: [256]u8 = undefined;
        const name = try std.fmt.bufPrint(&buf, "__namespace_{d}", .{this.spec_map.size});
        const ident = try this.factory.createIdentifier(name);
        entry.value_ptr.* = ident;
        return this.factory.cloneNodeRef(ident);
    }

    /// `import.meta.__virtualId` — the "importer" passed to `__wrapExports`.
    fn createImporter(this: *@This()) !NodeRef {
        const meta = try this.factory.createPropertyAccessExpression(try this.factory.createIdentifier("import"), "meta");
        return this.factory.createPropertyAccessExpression(meta, "__virtualId");
    }

    /// `__wrapExports(<spec>, import.meta.__virtualId, <namespace>)`
    fn createWrapExportsCall(this: *@This(), spec: NodeRef, namespace: NodeRef) !NodeRef {
        const args: []const NodeRef = &.{ spec, try this.createImporter(), namespace };
        return this.factory.createCallExpression(try this.factory.createIdentifier("__wrapExports"), args);
    }

    /// `import * as <namespace> from '<spec>'` (+ optional attributes).
    fn createNamespaceImportDeclaration(this: *@This(), namespace: NodeRef, spec: []const u8, attributes: NodeRef) !NodeRef {
        const ns_import = try this.factory.createNamespaceImport(namespace);
        const clause = try this.factory.createImportClause(0, ns_import);
        return this.factory.createImportDeclaration(clause, try this.factory.createStringLiteral(spec), attributes);
    }

    /// `<importCall>.then(namespace => __wrapExports(<spec>, import.meta.__virtualId, namespace))`
    /// `spec` is the dynamic import's specifier expression (its first argument).
    fn wrapDynamicImport(this: *@This(), import_call: NodeRef, spec: NodeRef) !NodeRef {
        const param = try this.factory.createParameter(try this.factory.createIdentifier("namespace"), 0);
        const body = try this.createWrapExportsCall(spec, try this.factory.createIdentifier("namespace"));
        const arrow = try this.factory.createSingleParamArrowFunction(param, body, 0);
        const then = try this.factory.createPropertyAccessExpression(import_call, "then");
        const args: []const NodeRef = &.{arrow};
        return this.factory.createCallExpression(then, args);
    }

    /// `const <local> = __wrapExports('<spec>', import.meta.__virtualId, <namespace>).<prop>`
    fn createWrappedAlias(this: *@This(), spec: []const u8, namespace_ident: NodeRef, local: NodeRef, prop: []const u8) !NodeRef {
        const wrapped = try this.createWrapExportsCall(
            try this.factory.createStringLiteral(spec),
            namespace_ident,
        );
        if (prop.len == 0) {
            return this.factory.createConstVariable(local, wrapped);
        }
        const access = try this.factory.createPropertyAccessExpression(wrapped, prop);
        return this.factory.createConstVariable(local, access);
    }

    pub fn visitImportDecl(this: *@This(), ref: NodeRef) !void {
        const n = this.factory.nodes.at(ref);
        std.debug.assert(n.kind == .import_declaration);

        if (n.hasFlag(.declare)) return;

        const d = getPackedData(n);
        const clause_ref = d.left;
        const specifier_ref = d.right;
        const attributes = n.len;

        if (clause_ref == 0) return;

        const spec = getSlice(this.factory.nodes.at(specifier_ref), u8);
        const first_time = !this.spec_map.contains(try parser.getHashFromModuleNode(this.factory.nodes.at(specifier_ref)));
        const namespace_ident = try this.getSpecIdent(specifier_ref);

        var tail = ref;
        if (first_time) {
            const ns_import = try this.createNamespaceImportDeclaration(namespace_ident, spec, attributes);
            this.factory.nodes.at(ns_import).next = n.next;
            try this.replacements.put(ref, ns_import);
            tail = ns_import;
        } else {
            tail = try this.factory.createNotEmittedStatement();
            this.factory.nodes.at(tail).next = n.next;
            try this.replacements.put(ref, tail);
        }

        const clause = this.factory.nodes.at(clause_ref);
        const cd = getPackedData(clause);
        const default_name_ref = cd.left; // `import foo from ...`
        const bindings_ref = cd.right; // namespace_import | named_imports

        if (default_name_ref != 0) {
            const binding_statement = try this.createWrappedAlias(spec, namespace_ident, default_name_ref, "default");
            this.factory.insertAfter(tail, binding_statement);
            tail = binding_statement;
        }

        if (bindings_ref != 0) {
            const bindings = this.factory.nodes.at(bindings_ref);
            switch (bindings.kind) {
                .namespace_import => {
                    const ns_local_ref = maybeUnwrapRef(bindings) orelse return;
                    const binding_statement = try this.createWrappedAlias(spec, namespace_ident, ns_local_ref, "");
                    this.factory.insertAfter(tail, binding_statement);
                    tail = binding_statement;
                },
                .named_imports => {
                    var it = NodeIterator.init(this.factory.nodes, maybeUnwrapRef(bindings) orelse 0);
                    while (it.next()) |spec_node| {
                        if (spec_node.hasFlag(.declare)) continue; // `type` specifier
                        const sd = getPackedData(spec_node);
                        const prop = getSlice(this.factory.nodes.at(sd.left), u8);
                        const binding_statement = try this.createWrappedAlias(spec, namespace_ident, if (sd.right != 0) sd.right else sd.left, prop);
                        this.factory.insertAfter(tail, binding_statement);
                        tail = binding_statement;
                    }
                },
                else => {},
            }
        }
    }

    // pub fn visitDynamicImport(this: *@This(), ref: NodeRef) !void {
        
    // }

};

/// Debug helper: prints captures for every function in `file`, plus the
/// generated micro program + annotation (with mutable/circular cells) for
/// named function declarations.
pub fn debugPrintCaptures(binder: *const Binder, ast: *AstData) !void {
    var analysis = try analyzeFile(binder, ast);
    defer analysis.deinit(getAllocator());

    var inst = try Instrumenter.init(getAllocator(), binder, &analysis);
    defer inst.deinit();

    var factory = Factory{ .nodes = &ast.nodes };

    std.debug.print("=== captures: {s} ===\n", .{ast.source_name orelse "<anonymous>"});
    for (analysis.results) |r| {
        const nm = r.name orelse "<anonymous>";
        std.debug.print("\n{s} [{s}] captures ({d}):", .{ nm, @tagName(r.kind), r.captured.len });
        for (r.captured) |c| {
            const tag: []const u8 = if (inst.isMutable(c.symbol))
                "*"
            else if (inst.isCircular(c.symbol, r.self_symbol))
                "(*)"
            else
                "";
            std.debug.print(" {s}{s}", .{ tag, parser.getSliceFromSymbol(binder, c.symbol) orelse "<unknown>" });
        }
        std.debug.print("\n", .{});

        if (r.name != null and r.kind == .function_declaration) {
            // Emit any cells this annotation needs first (so the printed
            // `__getSymbolN()` references resolve). They'd be emitted in the
            // declaring scope of each captured symbol.
            try emitNeededCells(&factory, &inst, ast, r.captured, r.self_symbol);

            const micro = try buildMicroProgram(&factory, r);
            const subject = try factory.createIdentifier(r.name.?);
            const anno = try buildMoveableAssignment(&factory, &inst, subject, r.name.?, r.captured, r.self_symbol);
            const micro_src = try parser.printInMemory(ast.*, ast.nodes.at(micro).*);
            const anno_src = try parser.printInMemory(ast.*, ast.nodes.at(anno).*);
            std.debug.print("{s}\n{s}\n", .{ micro_src, anno_src });
        }
    }

    for (analysis.classes) |cls| {
        const nm = cls.name orelse "<anonymous>";
        std.debug.print("\nclass {s} captures ({d}):", .{ nm, cls.captured.len });
        for (cls.captured) |c| std.debug.print(" {s}", .{parser.getSliceFromSymbol(binder, c.symbol) orelse "<unknown>"});
        std.debug.print("\n", .{});
        for (cls.methods) |m| {
            std.debug.print("  .{s}{s} captures ({d}):", .{ if (m.is_static) "static " else "", m.name, m.captured.len });
            for (m.captured) |c| std.debug.print(" {s}", .{parser.getSliceFromSymbol(binder, c.symbol) orelse "<unknown>"});
            std.debug.print("\n", .{});
        }

        if (cls.name == null) continue;
        const class_name = cls.name.?;

        // Class-level moveable: `ClassName[Symbol.for("__moveable__")] = ...`
        try emitNeededCells(&factory, &inst, ast, cls.captured, cls.self_symbol);
        const class_subject = try factory.createIdentifier(class_name);
        const class_anno = try buildMoveableAssignment(&factory, &inst, class_subject, class_name, cls.captured, cls.self_symbol);
        std.debug.print("{s}\n", .{try parser.printInMemory(ast.*, ast.nodes.at(class_anno).*)});

        // Per-method moveable: `ClassName.prototype.m[Symbol.for("__moveable__")] = ...`
        for (cls.methods) |m| {
            try emitNeededCells(&factory, &inst, ast, m.captured, 0);
            var mod_buf: [256]u8 = undefined;
            const module_name = try std.fmt.bufPrint(&mod_buf, "{s}::{s}", .{ class_name, m.name });
            // Static methods live on the class itself; instance methods on the
            // prototype. Computed names use element access (`[expr]`).
            const base = try factory.createIdentifier(class_name);
            const owner_expr = if (m.is_static)
                base
            else
                try factory.createPropertyAccessExpression(base, "prototype");
            const subject = if (m.is_computed)
                try factory.createElementAccessExpression(owner_expr, try factory.cloneNodeRef(maybeUnwrapRef(ast.nodes.at(m.name_ref)) orelse 0))
            else
                try factory.createPropertyAccessExpression(owner_expr, m.name);
            const m_anno = try buildMoveableAssignment(&factory, &inst, subject, module_name, m.captured, 0);
            std.debug.print("{s}\n", .{try parser.printInMemory(ast.*, ast.nodes.at(m_anno).*)});
        }
    }
}

// =====================================================================
// Original TypeScript reference implementation (kept for porting notes):
// =====================================================================

// import ts from 'typescript'
// import * as fs from 'node:fs/promises'
// import * as path from 'node:path'
// import { failOnNode, getNodeLocation, isNonNullable } from './utils'
// import { getLogger } from '../logging'
// import { Mutable, isDeclared } from '../utils'


// export interface Symbol {
//     readonly id: number
//     readonly name: string
//     readonly references: ts.Expression[]
//     readonly declaration?: ts.Node
//     readonly members: Map<string | Symbol, Symbol>
//     readonly parent?: Symbol
//     readonly parentScope?: Scope
//     readonly computed?: boolean
//     readonly transient?: boolean // Used for "fake" symbols
//     readonly argSymbol?: Symbol
//     readonly isDeclared?: boolean // Checks for the `declare` keyword

//     // These fields are used to simplify lookups of associated nodes
//     readonly importClause?: ts.ImportClause
//     readonly variableDeclaration?: ts.VariableDeclaration
// }

// export interface Scope {
//     readonly id: number
//     readonly node: ts.Node
//     readonly symbol?: Symbol // Not all scopes have an associated symbol
//     readonly thisSymbol?: Symbol
//     readonly superSymbol?: Symbol
//     readonly staticThisSymbol?: Symbol
//     readonly dependencies: Set<Symbol>
//     readonly parent?: Scope
//     readonly declarations: Map<string, Symbol>
//     readonly subscopes: Set<Scope>

//     // TODO: implement this to conditionally exclude code during synthesis
//     // readonly condition?: ts.Expression

//     // Any call-like expression on external symbols are side-effects
//     // Any assignment/mutation to external symbols are side-effects
//     // readonly sideEffects: ts.Node[]
// }

// export interface RootScope extends Scope {
//     readonly symbols: Map<ts.Node, Symbol>
//     readonly scopeCache: Map<ts.Node, Scope>
// }

// function isSymbol(obj: unknown): obj is Symbol {
//     return (!!obj && typeof obj === 'object' && 'id' in obj && 'name' in obj)
// }

// function isStaticDeclaration(node: ts.Node) {
//     return ts.canHaveModifiers(node) && ts.getModifiers(node)?.find(x => x.kind === ts.SyntaxKind.StaticKeyword)
// }

// export function getRootSymbol(sym: Symbol): Symbol {
//     while (sym.parent !== undefined) sym = sym.parent

//     return sym
// }

// export function getRootAndSuccessorSymbol(sym: Symbol): [Symbol, Symbol?] {
//     let successor: Symbol | undefined
//     while (sym.parent !== undefined) {
//         successor = sym
//         sym = sym.parent
//     }

//     return [sym, successor]
// }

// // Object literals have a class-like scope with a fixed receiver

// export function isScopeNode(node: ts.Node) {
//     switch (node.kind) {
//         case ts.SyntaxKind.Block:
//         case ts.SyntaxKind.CaseBlock:
//         // case ts.SyntaxKind.IfStatement:
//         case ts.SyntaxKind.ForStatement:
//         case ts.SyntaxKind.ForOfStatement:
//         case ts.SyntaxKind.SourceFile:
//         case ts.SyntaxKind.ArrowFunction:
//         case ts.SyntaxKind.Constructor:
//         case ts.SyntaxKind.MethodDeclaration:
//         case ts.SyntaxKind.GetAccessor:
//         case ts.SyntaxKind.SetAccessor:
//         case ts.SyntaxKind.EnumDeclaration:
//         case ts.SyntaxKind.ClassExpression:
//         case ts.SyntaxKind.ClassDeclaration:
//         case ts.SyntaxKind.FunctionExpression:
//         case ts.SyntaxKind.FunctionDeclaration:
//             return true
//     }
// }

// // These are the only scopes that _might_ have symbol associated with them
// function isNameableScopeNode(node: ts.Node) {
//     switch (node.kind) {
//         case ts.SyntaxKind.GetAccessor:
//         case ts.SyntaxKind.SetAccessor:
//         case ts.SyntaxKind.MethodDeclaration:
//         case ts.SyntaxKind.EnumDeclaration:
//         case ts.SyntaxKind.ClassExpression:
//         case ts.SyntaxKind.ClassDeclaration:
//         case ts.SyntaxKind.FunctionExpression:
//         case ts.SyntaxKind.FunctionDeclaration:
//             return true
//     }
// }

// type FunctionLike =
//     | ts.ArrowFunction
//     | ts.AccessorDeclaration 
//     | ts.FunctionExpression 
//     | ts.FunctionDeclaration 
//     | ts.MethodDeclaration 
//     | ts.ConstructorDeclaration

// function isFunctionLike(node: ts.Node): node is FunctionLike {
//     switch (node.kind) {
//         case ts.SyntaxKind.GetAccessor:
//         case ts.SyntaxKind.SetAccessor:
//         case ts.SyntaxKind.Constructor:
//         case ts.SyntaxKind.ArrowFunction:
//         case ts.SyntaxKind.MethodDeclaration:
//         case ts.SyntaxKind.FunctionExpression:
//         case ts.SyntaxKind.FunctionDeclaration:
//             return true
//     }

//     return false
// }

// export function isDeclaration(node: ts.Node) {
//     switch (node.kind) {
//         // Ignore overloads
//         case ts.SyntaxKind.FunctionDeclaration:
//             return (node as any).body !== undefined

//         case ts.SyntaxKind.Parameter:
//         case ts.SyntaxKind.Constructor:
//         case ts.SyntaxKind.PropertyDeclaration:
//         case ts.SyntaxKind.MethodDeclaration:
//         case ts.SyntaxKind.VariableDeclaration:
//         case ts.SyntaxKind.ClassDeclaration:
//         case ts.SyntaxKind.ImportDeclaration:
//         case ts.SyntaxKind.EnumDeclaration:
//             return true
//     }
// }

// let scopeCount = 0

// function createDependencyGraph() {
//     // We track symbols per-file to ensure that ids stay consistent when the file doesn't change
//     let symbolCount = 0

//     const stack: Scope[] = []

//     function createSymbol(name: string, declaration: ts.Node | undefined, computed?: boolean): Symbol {
//         return {
//             id: symbolCount++,
//             name,
//             computed,
//             references: [],
//             declaration,
//             members: new Map(),
//         }
//     }

//     function getScopeName(node: ts.Node): string {
//         if (ts.isIdentifier(node)) {
//             return node.text
//         }

//         if (ts.isStringLiteral(node)) {
//             return node.text
//         }

//         if (ts.isPropertyName(node)) {
//             if (ts.isComputedPropertyName(node)) {
//                 return '__computed'
//             }

//             return node.text // bug? handle numeric literals differently?
//         }

//         if (ts.isVariableDeclaration(node) ||  ts.isParameter(node)) {
//             if (!ts.isIdentifier(node.name)) {
//                 return `__bindingPattern_${symbolCount}` // XXX
//                 //failOnNode('Binding pattern not implemented', node.name)
//             }

//             return getScopeName(node.name)
//         }

//         if (ts.isFunctionDeclaration(node) || ts.isFunctionExpression(node) || ts.isArrowFunction(node)) {
//             if (node.name) {
//                 return getScopeName(node.name)
//             }

//             return `__anonymousFunction_${symbolCount}`
//         }

//         if (ts.isClassDeclaration(node) || ts.isClassExpression(node)) {
//             if (node.name) {
//                 return getScopeName(node.name)
//             }

//             return `__anonymousClass_${symbolCount}`
//         }

//         if (ts.isEnumDeclaration(node)) {
//             return getScopeName(node.name)
//         }

//         if (ts.isConstructorDeclaration(node)) {
//             return '__constructor'
//         }

//         if (ts.isMethodDeclaration(node) || ts.isPropertyDeclaration(node)) {
//             return getScopeName(node.name)
//         }

//         if (ts.isForStatement(node)) {
//             return `__forStatement_${symbolCount}`
//         }

//         if (ts.isForOfStatement(node)) {
//             return `__forOfStatement_${symbolCount}`
//         }

//         if (ts.isBlock(node) || ts.isCaseBlock(node)) {
//             return `__block_${symbolCount}`
//         }

//         if (ts.isSourceFile(node)) {
//             return node.fileName
//         }

//         if (ts.isNamespaceImport(node)) {
//             return node.name.text
//         }

//         if (ts.isObjectLiteralExpression(node)) {
//             return `__object_${symbolCount}`
//         }

//         if (ts.isHeritageClause(node)) {
//             return `super`
//         }

//         if (ts.isGetAccessor(node)) {
//             return `__get_${getScopeName(node.name)}`
//         }

//         if (ts.isSetAccessor(node)) {
//             return `__set_${getScopeName(node.name)}`
//         }

//         if (ts.isBindingElement(node)) {
//             return getScopeName(node.name)
//         }

//         if (node.kind === ts.SyntaxKind.ThisKeyword) {
//             return 'this'
//         }

//         failOnNode('Not supported', node)
//     }

//     const symbols = new Map<ts.Node, Symbol>()

//     function addReference(node: ts.Expression, symbol: Symbol) {
//         if (symbol.declaration === node || (symbol.declaration as any)?.name === node) {
//             symbols.set(node, symbol)
//             return
//         }

//         symbol.references.push(node)
//         symbols.set(node, symbol)
//     }

//     function isPrimaryExpressionLike(node: ts.Node) {
//         switch (node.kind) {
//             case ts.SyntaxKind.PropertyAccessExpression:
//             case ts.SyntaxKind.ElementAccessExpression:
//             case ts.SyntaxKind.ParenthesizedExpression:
//                 return true
//         }
//     }

//     function getStatements(node: ts.Node): readonly ts.Statement[] | undefined {
//         switch (node.kind) {
//             case ts.SyntaxKind.Block:
//                 return (node as ts.Block).statements
//             // case ts.SyntaxKind.IfStatement:
//             //     return (node as ts.IfStatement).elseStatement 
//             //         ? [(node as ts.IfStatement).thenStatement, (node as ts.IfStatement).elseStatement!] 
//             //         : [(node as ts.IfStatement).thenStatement] 
//             case ts.SyntaxKind.ForStatement:
//             case ts.SyntaxKind.ForOfStatement:
//                 return getStatements((node as ts.ForStatement | ts.ForOfStatement).statement) // BUG: I think this misses expression statements
//             case ts.SyntaxKind.SourceFile:
//                 return (node as ts.SourceFile).statements
//             case ts.SyntaxKind.Constructor:
//             case ts.SyntaxKind.MethodDeclaration:
//             case ts.SyntaxKind.FunctionExpression:
//             case ts.SyntaxKind.FunctionDeclaration:
//                 return (node as ts.FunctionDeclaration).body?.statements
//             case ts.SyntaxKind.CaseBlock:
//                 return (node as ts.CaseBlock).clauses.flatMap(c => c.statements)
//             case ts.SyntaxKind.ArrowFunction:
//                 return ts.isBlock((node as ts.ArrowFunction).body) 
//                     ? ((node as any).body as ts.Block).statements 
//                     : undefined
//         }
//     }

//     function getDeclarations(node: ts.Node): ts.Node[]  | undefined {
//         const declarations: ts.Node[] = []
//         const statements = getStatements(node)
//         if (!statements) {
//             if (ts.isClassDeclaration(node) || ts.isClassExpression(node)) {
//                 for (const e of node.members) {
//                     if (isDeclaration(e)) {
//                         declarations.push(e)
//                     }
//                 }

//                 return declarations
//             } else if (ts.isArrowFunction(node)) {
//                 return [...node.parameters]
//             }

//             return
//         }

//         if (ts.isFunctionLike(node)) {
//             declarations.push(...node.parameters)
//         }

//         if (ts.isForStatement(node) || ts.isForOfStatement(node)) {
//             if (node.initializer && ts.isVariableDeclarationList(node.initializer)) {
//                 declarations.push(...node.initializer.declarations)
//             }
//         }

//         for (const s of statements) {
//             if (isDeclaration(s)) {
//                 declarations.push(s)
//             } else if (ts.isVariableStatement(s)) {
//                 declarations.push(...s.declarationList.declarations)
//             }
//         }

//         return declarations
//     }

//     function addMember(target: Symbol, key: string | Symbol, value: Symbol) {
//         target.members.set(key, value)
//         ;(value as Mutable<Symbol>).parent = target
//     }

//     function createThisSymbol(scope: Scope, val: ts.Node, isStatic?: boolean) {
//         const sym = createSymbol('this', val)
//         ;(sym as Mutable<Symbol>).parentScope = scope

//         const target = isStatic ? scope.symbol! : getPrototypeSymbol(scope.symbol!)
//         for (const [k, v] of target.members) {
//             const member = createSymbol(v.name, v.declaration)
//             addMember(sym, k, member)
//         }

//         return sym
//     }

//     function getThisSymbol() {
//         let isStatic = false
//         for (let i = stack.length - 1; i >= 0; i--) {
//             const scope = stack[i]
//             const val = scope.symbol?.declaration
//             if (val && isStaticDeclaration(val)) {
//                 isStatic = true
//                 continue
//             }

//             if (val === undefined || (!ts.isClassDeclaration(val) && !ts.isFunctionDeclaration(val) && !ts.isClassExpression(val) && !ts.isFunctionExpression(val))) {
//                 continue
//             }

//             if (isStatic) {
//                 if (scope.staticThisSymbol !== undefined) {
//                     return scope.staticThisSymbol
//                 }

//                 const sym = createThisSymbol(scope, val, true)
//                 ;(scope as Mutable<Scope>).staticThisSymbol = sym

//                 return sym
//             }

//             if (scope.thisSymbol !== undefined) {
//                 return scope.thisSymbol
//             }

//             const sym = createThisSymbol(scope, val, false)
//             ;(scope as Mutable<Scope>).thisSymbol = sym

//             return sym
//         }

//         return createGlobalSymbol('this')
//     }

//     // FIXME: this impl. is incomplete and doesn't track static vs. non-static `super`
//     function createSuperSymbol(scope: Scope, val: ts.Node, isStatic?: boolean) {
//         const sym = createSymbol('super', val)
//         ;(sym as Mutable<Symbol>).parentScope = scope

//         return sym
//     }

//     function getSuperSymbol() {
//         for (let i = stack.length - 1; i >= 0; i--) {
//             const scope = stack[i]
//             const val = scope.symbol?.declaration
//             if (val === undefined || !ts.isClassLike(val)) {
//                 continue
//             }
            
//             if (scope.superSymbol !== undefined) {
//                 return scope.superSymbol
//             }

//             const superExp = val.heritageClauses?.find(x => x.token === ts.SyntaxKind.ExtendsKeyword)?.types[0]
//             if (!superExp) break

//             const sym = createSuperSymbol(scope, superExp, false)
//             ;(scope as Mutable<Scope>).superSymbol = sym

//             return sym
//         }

//         return
//     }

//     function findSymbol(name: string): Symbol | undefined {
//         for (let i = stack.length - 1; i >= 0; i--) {
//             const scope = stack[i].symbol?.declaration
//             if (scope && (ts.isClassDeclaration(scope) || ts.isClassExpression(scope))) continue

//             const symbol = stack[i].declarations.get(name)
//             if (symbol !== undefined) {
//                 return symbol
//             }
//         }
//     }

//     function getPrototypeSymbol(sym: Symbol) {
//         if (sym.members.has('prototype')) {
//             return sym.members.get('prototype')!
//         }

//         const proto = createSymbol('prototype', undefined)
//         sym.members.set('prototype', proto)
//         ;(proto as Mutable<Symbol>).parent = sym

//         return proto
//     }

//     const scopeCache = new Map<ts.Node, Scope>()

//     function getScope(node: ts.Node): Scope {
//         if (scopeCache.has(node)) {
//             return scopeCache.get(node)!
//         }

//         const dependencies = new Set<Symbol>()
//         const declarations = new Map<string, Symbol>()
//         const parentScope = stack[stack.length - 1]

//         const scope: Scope = {
//             id: scopeCount++,
//             node,
//             declarations,
//             dependencies,
//             subscopes: new Set(),
//             parent: parentScope,
//         }

//         parentScope.subscopes.add(scope)
//         scopeCache.set(node, scope)

//         if (!isNameableScopeNode(node)) {
//             // if (node.kind === ts.SyntaxKind.IfStatement) {
//             //     ;(scope as Mutable<Scope>).condition = (node as ts.IfStatement).expression
//             // }

//             return scope
//         }

//         const symbol = bindSymbol(node, scope, ts.isClassElement(node))
//         ;(scope as Mutable<Scope>).symbol = symbol

//         if (isDeclared(node)) {
//             ;(symbol as Mutable<Symbol>).isDeclared = true
//         }

//         if (parentScope) {
//             const parentSym = parentScope.symbol
//             const parentVal = parentSym?.declaration

//             if (parentVal && ts.isClassLike(parentVal)) {
//                 const targetSym = isStaticDeclaration(node) ? parentSym : getPrototypeSymbol(parentSym)
//                 addMember(targetSym, symbol.name, symbol)
//             }
//         }

//         return scope
//     }

//     function bindVariableDeclaration(decl: ts.VariableDeclaration) {
//         const symbols = ts.isObjectBindingPattern(decl.name) || ts.isArrayBindingPattern(decl.name)
//             ? visitBindingPattern(decl.name)
//             : [bindSymbol(decl)]

//         for (const sym of symbols) {
//             (sym as Mutable<Symbol>).variableDeclaration = decl
//         }
//     }

//     function visitParameterDeclaration(decl: ts.ParameterDeclaration, parent: ts.Node) {
//         if (ts.isParameterPropertyDeclaration(decl, parent)) {
//             const classScope = stack[stack.length - 2]
//             const symbol = bindSymbol(decl, classScope, true)            
//             addMember(getPrototypeSymbol(classScope.symbol!), symbol.name, symbol)
//         }

//         if (ts.isIdentifier(decl.name)) {
//             bindSymbol(decl)
//         } else {
//             if ((decl.name as any).kind === ts.SyntaxKind.ThisKeyword) {
//                 return bindSymbol(decl)
//             }
//             visitBindingPattern(decl.name, true)
//         }
//     }

//     function bindPropertyDeclaration(decl: ts.PropertyDeclaration, scope: Scope) {
//         const parentSym = scope.symbol!
//         const targetSym = isStaticDeclaration(decl) ? parentSym : getPrototypeSymbol(parentSym)

//         if (!ts.isComputedPropertyName(decl.name)) {
//             const symbol = bindSymbol(decl, scope, true)
//             addMember(targetSym, symbol.name, symbol)

//             return
//         }

//         const symbol = visitExpression(decl.name.expression)
//         if (!symbol) {
//             failOnNode(`No symbol found for computed property name`, decl.name)
//         }

//         // XXX
//         // parentSym.members.set(symbol, symbol)
//     }

//     function visitScopeNode(node: ts.Node) {
//         const scope = getScope(node)
//         stack.push(scope)

//         // init declarations first
//         const declarations = getDeclarations(node)
//         if (declarations) {
//             for (const decl of declarations) {
//                 if (ts.isImportDeclaration(decl)) {
//                     visitImportDeclaration(decl)
//                 } else if (ts.isVariableDeclaration(decl)) {
//                     bindVariableDeclaration(decl)
//                 } else if (ts.isParameter(decl)) {
//                     visitParameterDeclaration(decl, node)
//                 } else if (ts.isPropertyDeclaration(decl)) {
//                     bindPropertyDeclaration(decl, scope)
//                 } else {
//                     if (ts.isMethodDeclaration(decl)) {
//                         if (ts.isComputedPropertyName(decl.name)) {
//                             visitExpression(decl.name.expression)
//                         }
//                     }
//                     const child = getScope(decl) // XXX
//                 }
//             }
//         }

//         if (isFunctionLike(node)) {
//             for (const param of node.parameters) {
//                 if (param.initializer) {
//                     visit(param.initializer)
//                 }
//             }

//             // Example case for why this is needed:
//             // `(a, b) => (id, ref) => b(id, ref, a)`
//             if (ts.isArrowFunction(node) && !ts.isBlock(node.body)) {
//                 visit(node.body)
//             } else {
//                 node.body?.forEachChild(visit)
//             }
//         } else {
//             // XXX: we add a fake scope for the heritage clause so it can be extracted more easily
//             const superClass = ts.isClassLike(node) 
//                 ? node.heritageClauses?.find(x => x.token === ts.SyntaxKind.ExtendsKeyword)
//                 : undefined

//             if (superClass) {
//                 const s = getScope(superClass)
//                 stack.push(s)
//                 superClass.types.forEach(visit)
//                 stack.pop()
//                 if ((node as ts.ClassDeclaration | ts.ClassExpression).name) {
//                     visit((node as ts.ClassDeclaration | ts.ClassExpression).name!)
//                 }
//                 ;(node as ts.ClassDeclaration | ts.ClassExpression).members.forEach(visit)
//                 stack.pop()

//                 return
//             }

//             node.forEachChild(visit)
//         }

//         stack.pop()!
//     }

//     function isTypeNode(node: ts.Node) {
//         switch (node.kind) {
//             case ts.SyntaxKind.HeritageClause:
//                 return (node as ts.HeritageClause).token === ts.SyntaxKind.ImplementsKeyword
            
//             case ts.SyntaxKind.Parameter:
//                 return (node as ts.ParameterDeclaration).name.kind === ts.SyntaxKind.Identifier &&
//                     ((node as ts.ParameterDeclaration).name as ts.Identifier).text === 'this'

//             case ts.SyntaxKind.VariableStatement:
//             case ts.SyntaxKind.ClassDeclaration:
//             case ts.SyntaxKind.ModuleDeclaration:
//                 return !!ts.getModifiers(node as ts.ClassDeclaration | ts.VariableStatement | ts.ModuleDeclaration)
//                     ?.find(m => m.kind === ts.SyntaxKind.DeclareKeyword)

//             case ts.SyntaxKind.Constructor:
//             case ts.SyntaxKind.MethodDeclaration:
//             case ts.SyntaxKind.FunctionDeclaration:
//                 return (node as ts.FunctionDeclaration | ts.MethodDeclaration | ts.ConstructorDeclaration).body === undefined

//             case ts.SyntaxKind.ImportDeclaration:
//                 return !!(node as ts.ImportDeclaration).importClause?.isTypeOnly
            
//             case ts.SyntaxKind.ImportEqualsDeclaration:
//                 return (node as ts.ImportEqualsDeclaration).isTypeOnly

//             case ts.SyntaxKind.ExportDeclaration:
//                 return (node as ts.ExportDeclaration).isTypeOnly
            
//             case ts.SyntaxKind.ImportSpecifier:
//             case ts.SyntaxKind.ExportSpecifier:
//                 return (node as ts.ImportSpecifier | ts.ExportSpecifier).isTypeOnly
            
//             case ts.SyntaxKind.PropertySignature:
//             case ts.SyntaxKind.ConstructorType:
//             case ts.SyntaxKind.MappedType:
//             case ts.SyntaxKind.ConditionalType:
//             case ts.SyntaxKind.TypeLiteral:
//             case ts.SyntaxKind.FunctionType:
//             case ts.SyntaxKind.TypeAliasDeclaration:
//             case ts.SyntaxKind.InterfaceDeclaration:
//             case ts.SyntaxKind.TypeQuery:
//             case ts.SyntaxKind.TypeOperator:
//             case ts.SyntaxKind.TypeReference:
//             case ts.SyntaxKind.TypePredicate:
//             case ts.SyntaxKind.TypeParameter:
//                 return true
//         }

//         return false
//     }

//     function visitIdentifier(node: ts.Identifier) {
//         // if (ts.isJsxAttribute(node.parent) && node.parent.name === node) {
//         //     return
//         // }

//         switch (node.parent.kind) {
//             // BIG HACK
//             // We're exploiting the fact that lowercase tags are intrinsic instead of fixing the real problem
//             case ts.SyntaxKind.JsxOpeningElement:
//             case ts.SyntaxKind.JsxClosingElement:
//             case ts.SyntaxKind.JsxSelfClosingElement:
//                 if ((node.parent as ts.JsxOpeningElement).tagName === node && node.text.toLowerCase() === node.text) {
//                     return
//                 }
//                 break

//             case ts.SyntaxKind.JsxAttribute:
//                 if ((node.parent as ts.JsxAttribute).name === node) {
//                     return
//                 }
//                 break
//         }

//         // if ((ts.isJsxOpeningElement(node.parent) || ts.isJsxClosingElement(node.parent) || ts.isJsxSelfClosingElement(node.parent)) && node.parent.tagName === node && node.text.toLowerCase() === node.text) {
//         //     return
//         // }

//         const name = node.text

//         for (let i = stack.length - 1; i >= 0; i--) {
//             const sym = findSymbol(name)

//             if (sym) {
//                 addReference(node, sym)

//                 return sym
//             }
//         }

//         const globalSym = createGlobalSymbol(name)
//         addReference(node, globalSym)

//         return globalSym
//     }

//     function visitThisExpression(node: ts.ThisExpression) {
//         const thisSymbol = getThisSymbol()

//         addReference(node, thisSymbol)

//         return thisSymbol
//     }

//     function visitSuperExpression(node: ts.SuperExpression) {
//         const sym = getSuperSymbol()
//         if (!sym) return

//         addReference(node, sym)

//         return sym
//     }

//     function getMemberSymbol(target: Symbol, member: string | Symbol): Symbol {
//         if (target.members.has(member)) {
//             return target.members.get(member)!
//         }

//         const name = typeof member === 'string' ? member : printSymbol(member)
//         const memberSym = createSymbol(name, undefined, typeof member !== 'string')
//         target.members.set(member, memberSym)
//         ;(memberSym as Mutable<Symbol>).parent = target
//         ;(memberSym as Mutable<Symbol>).argSymbol = typeof member !== 'string' ? member : undefined

//         return memberSym
//     }

//     function visitPropertyAccessExpression(node: ts.PropertyAccessExpression): Symbol | undefined {
//         const sym = visitExpression(node.expression)
//         if (!sym) {
//             return
//         }

//         const name = node.name.text
//         const memberSymbol = getMemberSymbol(sym, name)
//         addReference(node, memberSymbol)

//         return memberSymbol
//     }

//     function visitElementAccessExpression(node: ts.ElementAccessExpression): Symbol | undefined {
//         const sym = visitExpression(node.expression)
//         const nameSym = visitExpression(node.argumentExpression)
//         if (!sym || !nameSym) {
//             return sym ?? nameSym
//         }

//         const memberSymbol = getMemberSymbol(sym, nameSym)
//         addReference(node, memberSymbol)

//         return memberSymbol
//     }

//     function visitCallExpression(node: ts.CallExpression) {
//         const target = visitExpression(node.expression)
//         const args = node.arguments.map(visitExpression)

//         // if (target && isSymbol(target)) {
//         //     const graph = isSymbol(target)
//         //         ? getGraphFromSymbol(target)
//         //         : target

//         //     if (graph) {
//         //         graph.sideEffects.push(node)
//         //     }

//         //     symbols.set(node, target)
//         // }

//         return undefined
//     }

//     function visitNewExpression(node: ts.NewExpression) {
//         const target = visitExpression(node.expression)
//         const args = node.arguments?.map(visitExpression) ?? []

//         // if (target && !isSymbolWithinCurrentGraph(target)) {
//         //     const graph = isSymbol(target)
//         //         ? getGraphFromSymbol(target)
//         //         : target

//         //     if (graph) {
//         //         graph.sideEffects.push(node)
//         //     }
//         // }
    
//         return undefined
//     }

//     function visitEnumMember(node: ts.EnumMember) {
//         if (!ts.isIdentifier(node.name)) {
//             failOnNode('Not implemented', node)
//         }

//         const currentScope = stack[stack.length - 1]
//         const memberSymbol = createSymbol(node.name.text, node)
//         ;(memberSymbol as Mutable<Symbol>).parent = currentScope.symbol
//         currentScope.symbol!.members.set(node.name.text, memberSymbol)

//         return memberSymbol
//     }

//     function visitBinaryExpression(node: ts.BinaryExpression) {
//         const left = visitExpression(node.left)
//         const right = visitExpression(node.right)

//         // switch (node.operatorToken.kind) {
//         //     case ts.SyntaxKind.EqualsToken:
//         //     case ts.SyntaxKind.PlusEqualsToken: 
//         //     case ts.SyntaxKind.MinusEqualsToken:
//         //     case ts.SyntaxKind.AsteriskAsteriskEqualsToken:
//         //     case ts.SyntaxKind.AsteriskEqualsToken:
//         //     case ts.SyntaxKind.SlashEqualsToken:
//         //     case ts.SyntaxKind.PercentEqualsToken:
//         //     case ts.SyntaxKind.AmpersandEqualsToken:
//         //     case ts.SyntaxKind.BarEqualsToken:
//         //     case ts.SyntaxKind.CaretEqualsToken:
//         //     case ts.SyntaxKind.LessThanLessThanEqualsToken:
//         //     case ts.SyntaxKind.GreaterThanGreaterThanGreaterThanEqualsToken:
//         //     case ts.SyntaxKind.GreaterThanGreaterThanEqualsToken:
//         //     case ts.SyntaxKind.BarBarEqualsToken:
//         //     case ts.SyntaxKind.AmpersandAmpersandEqualsToken:
//         //     case ts.SyntaxKind.QuestionQuestionEqualsToken: {
//         //         const target = isSymbol(left) ? getGraphFromSymbol(left) : left
//         //         if (target && target !== stack[stack.length - 1]) {
//         //             target.sideEffects.push(node)
//         //         }
//         //     }        
//         // }
    
//         return undefined
//     }

//     function maybeAddDependency(node: ts.Node, sym: Symbol) {
//         const parent = node.parent
//         if (isPrimaryExpressionLike(parent)) {
//             return
//         }

//         const currentScope = stack[stack.length - 1]
//         // This is a terminal node, add it to the dependency graph
//         if (currentScope.symbol !== sym) {
//             currentScope.dependencies.add(sym)
//         }

//         // Check for any computed symbols in intermediate expressions
//         let currentSym: Symbol | undefined = sym
//         while (currentSym !== undefined) {
//             const argSymbol = currentSym.computed ? currentSym.argSymbol : undefined
//             if (argSymbol) {
//                 if (currentScope.symbol !== argSymbol) {
//                     currentScope.dependencies.add(argSymbol)
//                 }
//             }
//             currentSym = currentSym.parent
//         }
//     }

//     // Expressions should result in a graph or symbol
//     function visitExpression(node: ts.Expression): Symbol | undefined {
//         if (isTypeNode(node)) {
//             return
//         }

//         if (isScopeNode(node)) {
//             return void visitScopeNode(node)
//         }

//         function fn(): Symbol | undefined {
//             switch (node.kind) {
//                 case ts.SyntaxKind.Identifier:
//                     return visitIdentifier(node as ts.Identifier)
//                 case ts.SyntaxKind.ThisKeyword:
//                     return visitThisExpression(node as ts.ThisExpression)
//                 case ts.SyntaxKind.SuperKeyword:
//                     return visitSuperExpression(node as ts.SuperExpression)
//                 case ts.SyntaxKind.PropertyAccessExpression:
//                     return visitPropertyAccessExpression(node as ts.PropertyAccessExpression)
//                 case ts.SyntaxKind.ElementAccessExpression:
//                     return visitElementAccessExpression(node as ts.ElementAccessExpression)
//                 case ts.SyntaxKind.CallExpression:
//                     return visitCallExpression(node as ts.CallExpression)
//                 case ts.SyntaxKind.NewExpression:
//                     return visitNewExpression(node as ts.NewExpression)
//                 case ts.SyntaxKind.BinaryExpression:
//                     return visitBinaryExpression(node as ts.BinaryExpression)
//                 case ts.SyntaxKind.AwaitExpression:
//                 case ts.SyntaxKind.ParenthesizedExpression:
//                     return visitExpression((node as ts.ParenthesizedExpression).expression)
//                 default:
//                     node.forEachChild(visit)
//             }    
//         }

//         const symbol = fn()
//         if (symbol !== undefined) {
//             maybeAddDependency(node, symbol)

//             return symbol
//         }
//     }

//     function createGlobalSymbol(name: string) {        
//         const currentScope = stack[stack.length - 1]
//         const symbol = createSymbol(name, undefined)
//         stack[0].declarations.set(name, symbol)
//         currentScope.dependencies.add(symbol)
//         ;(symbol as Mutable<Symbol>).parentScope = stack[0]

//         return symbol
//     }

//     function visitImportDeclaration(node: ts.ImportDeclaration) {
//         const clause = node.importClause
//         if (!clause) {
//             // side-effect import
//             return
//         }

//         function bindWithClause(node: ts.Node) {
//             const sym = bindSymbol(node)
//             ;(sym as Mutable<Symbol>).importClause = clause
//         }

//         const bindings = clause.namedBindings
//         if (bindings) {
//             if (ts.isNamespaceImport(bindings)) {
//                 bindWithClause(bindings.name)
//             } else {
//                 const len = bindings.elements.length
//                 for (let i = 0; i < len; i++) {
//                     bindWithClause(bindings.elements[i].name)
//                 }
//             }
//         }

//         if (clause.name) {
//             bindWithClause(clause.name)
//         }
//     }

//     function bindSymbol(node: ts.Node, parentScope = stack[stack.length - 1], isClassElement = false) {
//         const name = getScopeName(node)
//         const symbol = createSymbol(name, node)
//         symbols.set(node, symbol)
//         ;(symbol as Mutable<Symbol>).parentScope = parentScope
//         if (!isClassElement) {
//             stack[stack.length - 1].declarations.set(name, symbol)
//         }

//         return symbol
//     }

//     function visitBindingPattern(node: ts.BindingPattern, visitInitializer = false) {
//         const symbols: Symbol[] = []
//         for (const element of node.elements) {
//             if (!ts.isBindingElement(element)) {
//                 continue
//             }

//             if (ts.isIdentifier(element.name)) {
//                 symbols.push(bindSymbol(element))

//                 if (visitInitializer && element.initializer) {
//                     visitExpression(element.initializer)
//                 }
//             } else {
//                 visitBindingPattern(element.name, visitInitializer)
//             }
//         }

//         return symbols
//     }

//     function visitCatchClause(node: ts.CatchClause) {
//         if (!node.variableDeclaration) {
//             return visitScopeNode(node.block)
//         }

//         const scope = getScope(node.block)
//         stack.push(scope)
//         bindSymbol(node.variableDeclaration)
//         node.block.forEachChild(visit)
//         stack.pop()
//     }

//     function visitExportDeclaration(node: ts.ExportDeclaration) {
//         if (!node.exportClause || !ts.isNamedExports(node.exportClause)) {
//             return
//         }

//         for (const spec of node.exportClause.elements) {
//             // We only want to add a symbol for the local identifier
//             const localIdent = spec.propertyName ?? spec.name
//             visitExpression(localIdent)
//         }
//     }

//     function visit(node: ts.Node) {
//         switch (node.kind) {
//             case ts.SyntaxKind.PropertyAssignment: {
//                 if (ts.isComputedPropertyName((node as ts.PropertyAssignment).name)) {
//                     const name = (node as ts.PropertyAssignment).name as ts.ComputedPropertyName
//                     visitExpression(name.expression)
//                 }
    
//                 return void visit((node as ts.PropertyAssignment).initializer)
//             }
    
//             case ts.SyntaxKind.PropertyDeclaration:
//                 return (node as ts.PropertyDeclaration).initializer 
//                     ? void visit((node as ts.PropertyDeclaration).initializer!) 
//                     : undefined

//             case ts.SyntaxKind.ExpressionStatement:
//                 return void visitExpression((node as ts.ExpressionStatement).expression)
    
//             case ts.SyntaxKind.EnumMember:
//                 return void visitEnumMember(node as ts.EnumMember)
    
//             case ts.SyntaxKind.CatchClause:
//                 return void visitCatchClause(node as ts.CatchClause)

//             case ts.SyntaxKind.ExportDeclaration:
//                 return void visitExportDeclaration(node as ts.ExportDeclaration)

//             case ts.SyntaxKind.LabeledStatement:
//                 return void visit((node as ts.LabeledStatement).statement)

//             // This will be visited earlier
//             case ts.SyntaxKind.ImportDeclaration:
//             // Skip labels
//             case ts.SyntaxKind.BreakStatement:
//             case ts.SyntaxKind.ContinueStatement:
//                 return

//         }

//         if (ts.isExpression(node)) {
//             return void visitExpression(node)
//         }

//         if (isTypeNode(node)) {
//             return
//         }

//         if (isScopeNode(node)) {
//             // XXX: this leaks the symbols into the outer scope. We're assuming that the method decl is apart of an object literal exp.
//             if (ts.isMethodDeclaration(node) && ts.isComputedPropertyName(node.name) && ts.isObjectLiteralExpression(node.parent)) {
//                 visitExpression(node.name.expression)
//             }

//             return void visitScopeNode(node)
//         }

//         node.forEachChild(visit)
//     }

//     return (s: ts.Node) => {
//         stack.push({
//             symbols,
//             scopeCache,
//             symbol: createSymbol('__global', undefined),
//             declarations: new Map(),
//             dependencies: new Set(),
//             subscopes: new Set(),
//         } as RootScope)

//         visitScopeNode(s)

//         return stack.pop()! as RootScope
//     }
// }

// // What do I want to know?
// // Given a function/module/class, determine:
// // 1. The permissions required to execute the code
// // 2. The resources required to execute the code
// // 3. The symbolic dependencies

// // var a, b;
// // var e = {foo: 5, bar: 6, baz: ['Baz', 'Content']};
// // var arr = [];
// // ({baz: [arr[0], arr[3]], foo: a, bar: b} = e);
// // getTerminalLogger().log(a + ',' + b + ',' + arr);	// displays: 5,6,Baz,,,Content
// // [a, b] = [b, a];		// swap contents of a and b


// // isSourceFileDefaultLibrary(file: SourceFile): boolean;

// const refInScopeCache = new Map<string, ts.Node[]>()
// export function getReferencesInScope(symbol: Symbol, scope: Scope) {
//     const key = `${scope.id}:${symbol.id}`
//     if (refInScopeCache.has(key)) {
//         return refInScopeCache.get(key)!
//     }

//     const result: ts.Node[] = []

//     function isInScope(node: ts.Node) {
//         if (ts.findAncestor(node, n => n === scope.node)) {
//             return true
//         }

//         return false
//     }

//     for (const ref of symbol.references) {
//         if (isInScope(ref)) {
//             result.push(ref)
//         }
//     }

//     refInScopeCache.set(key, result)

//     return result
// }

// /** Checks if `b` is contained by `a` */
// function isSubscope(a: Scope, b: Scope) {
//     let c: Scope | undefined = b

//     do {
//         if (a === c) return true
//         c = c.parent
//     } while (c !== undefined)

//     return false
// }

// function getChildrenDeps(scope: Scope, excluded: Scope[] = []): Symbol[] {
//     const deps: Symbol[] = []
//     for (const v of scope.subscopes) {
//         if (excluded.includes(v)) continue

//         deps.push(
//             ...v.dependencies,
//             ...getChildrenDeps(v, excluded)
//         )
//     }

//     return deps
// }

// export function getContainingScope(symbol: Symbol): Scope {
//     const scope = getRootSymbol(symbol).parentScope
//     if (!scope) {
//         if (symbol.declaration) {
//             failOnNode('Symbol is not apart of a graph', symbol.declaration)
//         }

//         throw new Error(`Symbol is not apart of a graph: ${symbol.name}`)
//     }

//     return scope
// }


// export function getImmediatelyCapturedSymbols(scope: Scope, excluded: Scope[] = []) {
//     if (!scope.node) { // Global scope
//         return []
//     }

//     const symbols: Symbol[] = []
//     const deps = [...getChildrenDeps(scope, excluded), ...scope.dependencies]
//     for (const d of deps) {
//         if (!isSubscope(scope, getContainingScope(d))) {
//             symbols.push(d)
//         }
//     }

//     return symbols
// }

// export function isAssignmentExpression(node: ts.BinaryExpression) {
//     switch (node.operatorToken.kind) {
//         case ts.SyntaxKind.EqualsToken:
//         case ts.SyntaxKind.PlusEqualsToken: 
//         case ts.SyntaxKind.MinusEqualsToken:
//         case ts.SyntaxKind.AsteriskAsteriskEqualsToken:
//         case ts.SyntaxKind.AsteriskEqualsToken:
//         case ts.SyntaxKind.SlashEqualsToken:
//         case ts.SyntaxKind.PercentEqualsToken:
//         case ts.SyntaxKind.AmpersandEqualsToken:
//         case ts.SyntaxKind.BarEqualsToken:
//         case ts.SyntaxKind.CaretEqualsToken:
//         case ts.SyntaxKind.LessThanLessThanEqualsToken:
//         case ts.SyntaxKind.GreaterThanGreaterThanGreaterThanEqualsToken:
//         case ts.SyntaxKind.GreaterThanGreaterThanEqualsToken:
//         case ts.SyntaxKind.BarBarEqualsToken:
//         case ts.SyntaxKind.AmpersandAmpersandEqualsToken:
//         case ts.SyntaxKind.QuestionQuestionEqualsToken:
//             return true
//     }

//     return false
// }


// // Rules:
// // 1. Any symbol on the LHS of an assignment operation must be decomposed and passed by reference
// //     * Mutation of variables shared between multiple functions cannot be captured without transforming it into a binding
// // 2. Computed symbols can only be captured in their entirety if they are constant
// // 3. References to private members result in an indivisible function. Additional bindings need to be added to make the function divisible.

// // Transforms a graph into a function declaration that passes in captured symbols by argument
// // Function/class declarations are placed inside, and stateful declarations are made into arguments

// export function getScopeEnvironment(scope: Scope, excluded: Scope[] = []) {
//     const capturedSymbols = new Set<Symbol>()
//     // const globals = new Set<Symbol>()

//     const captured = getImmediatelyCapturedSymbols(scope, excluded)
//     for (const c of captured) {
//         const rootSym = getRootSymbol(c)
//         const val = rootSym.declaration
//         if (val === undefined) {
//             // globals.add(rootSym)
//             continue
//         }

//         capturedSymbols.add(c)
//     }

//     return { 
//         // globals: Array.from(globals),
//         captured: Array.from(capturedSymbols), 
//     }
// }

// export function unwrapScope(scope: Scope): ts.Node | undefined {
//     const decl = scope.symbol?.declaration
//     if (decl && ts.isHeritageClause(decl)) {
//         return decl.types[0].expression
//     }

//     return decl
// }

// export function isParameter(scope: Scope): boolean {
//     const val = unwrapScope(scope)
//     if (val === undefined) {
//         return false
//     }

//     return ts.isParameter(val)
// }

// export function getSubscopeDfs(scope: Scope, node: ts.Node): Scope | undefined {
//     for (const g of scope.subscopes.values()) {
//         const n = getSubscopeDfs(g, node)
//         if (n) return n
//     }

//     if (ts.findAncestor(node, n => n === scope.node)) {
//         return scope
//     }

//     return
// }

// export function getSubscopeContaining(scope: Scope, node: ts.Node) {
//     for (const g of scope.subscopes.values()) {
//         if (ts.findAncestor(node, n => n === g.node)) {
//             return g
//         }
//     }

//     failOnNode('No subscope found', node)
// }

// export function createGraph(node: ts.Node): RootScope {
//     return createDependencyGraph()(node)
// }

// export function createGraphOmitGlobal(node: ts.Node): Scope {
//     return getSubscopeContaining(createDependencyGraph()(node), node)
// }

// export function printSymbol(symbol: Symbol): string {
//     if (!symbol.parent) {
//         return symbol.name
//     }

//     if (symbol.computed) {
//         return `${printSymbol(symbol.parent)}[${symbol.name}]`
//     }

//     return `${printSymbol(symbol.parent)}.${symbol.name}`
// }

// export function printDependencies(scope: Scope) {
//     return [...scope.dependencies].map(printSymbol).join(', ')
// }

// function getScopeSymbol(scope: Scope) {
//     while (!scope.symbol && scope.parent) {
//         scope = scope.parent
//     }

//     if (!scope.symbol) {
//         throw new Error(`No scope found with symbol starting from scope: ${scope}`)
//     }

//     return scope.symbol
// }

// function printGraph(scope: Scope, maxDepth = Infinity, hideAmbient = false, depth = 0) {
//     if (depth >= maxDepth) return

//     const print = (s: string) => getLogger().log(`${'  '.repeat(depth)}${s}`)
//     // const immediateDeps = [...graph.dependencies, ...getChildrenDeps(graph)]
//     const deps = printDependencies(scope)
//     const sym = getScopeSymbol(scope)
//     const val = sym.declaration
//     const isAmbient = val=== undefined

//     if (isAmbient && hideAmbient && depth > 0) return

//     if (!isAmbient && ts.isParameter(val)) {
//         print('<Parameter> ' + sym.name + (deps ? ` [${deps}]` : ''))
//     } else {
//         print(sym.name + (isAmbient ? '*' : '') + (deps ? ` [${deps}]` : ''))
//     }

//     for (const [k, v] of scope.subscopes.entries()) {
//         printGraph(v, maxDepth, hideAmbient, depth + 1)
//     }
// }

// export function createGraphFromText(fileName: string, text: string) {
//     const sourceFile = ts.createSourceFile(fileName, text, { languageVersion: ts.ScriptTarget.ES2020 }, true)
    
//     return createDependencyGraph()(sourceFile)
// }

// export function createGraphFromFile(sourceFile: ts.SourceFile) {    
//     return createDependencyGraph()(sourceFile)
// }


// ------

// import ts, { factory, isCallExpression } from 'typescript'
// import { PrinterOptions, RawPrintResult, SourceMapHost, createVariableStatement, emitChunk, extract, failOnNode, getNodeLocation, getNullTransformationContext, isNonNullable, printNodes } from './utils'
// import { isAssignmentExpression, Symbol, Scope, createGraphOmitGlobal, getContainingScope, unwrapScope, getSubscopeDfs, getReferencesInScope, getRootSymbol, RootScope, createGraph, getSubscopeContaining, getImmediatelyCapturedSymbols, getRootAndSuccessorSymbol, printSymbol } from './scopes'
// import { createLiteral, createObjectLiteral, createPropertyAssignment, createSymbolPropertyName, createSyntheticComment, hashNode, memoize, removeModifiers } from '../utils'
// import { SourceMapV3 } from '../runtime/sourceMaps'
// import { getScopeEnvironment } from './scopes'
// import { ResourceTypeChecker } from '../compiler/resourceGraph'
// import { throwIfCancelled, CancelError } from '../execution'


// function createMovablePropertyName(factory = ts.factory) {
//     return createSymbolPropertyName('__moveable__', factory)
// }

// // seeing if this speeds up emit (brittle)
// const cachedFactoryExpressions = new Map<string, ts.Expression>()
// function getFileNameNode(factory: ts.NodeFactory, moduleType: 'esm' | 'cjs') {
//     const key = `${moduleType}:filename`
//     const cached = cachedFactoryExpressions.get(key)
//     if (cached) {
//         return cached
//     }

//     const filename = moduleType === 'cjs'
//         ? factory.createIdentifier('__filename')
//         : factory.createPropertyAccessExpression(
//             factory.createPropertyAccessExpression(factory.createIdentifier('import'), 'meta'),
//             'filename'
//         )

//     cachedFactoryExpressions.set(key, filename)

//     return filename
// }

// function getCachedIdent(factory: ts.NodeFactory, name: string) {
//     const cached = cachedFactoryExpressions.get(name)
//     if (cached) {
//         return cached as ts.Identifier
//     }

//     const ident = factory.createIdentifier(name)
//     cachedFactoryExpressions.set(name, ident)

//     return ident
// }

// function createSerializationData(
//     targetModule: string,
//     captured: ts.Expression[],
//     factory = ts.factory,
//     moduleType: 'esm' | 'cjs'
// ) {
//     const filename = getFileNameNode(factory, moduleType)

//     const moduleExpression = factory.createCallExpression(
//         getCachedIdent(factory, '__getPointer'),
//         undefined,
//         [
//             filename,
//             factory.createStringLiteral(targetModule)
//         ]
//     )

//     return createObjectLiteral({
//         valueType: 'function',
//         module: moduleExpression,
//         captured,
//     }, factory)
// }

// function createClassSerializationData(
//     targetModule: string,
//     captured: ts.Expression[],
//     factory = ts.factory,
//     moduleType: 'esm' | 'cjs',
//     properties: string[]
// ) {
//     const filename = getFileNameNode(factory, moduleType)

//     const moduleExpression = factory.createCallExpression(
//         getCachedIdent(factory, '__getPointer'),
//         undefined,
//         [
//             filename,
//             factory.createStringLiteral(targetModule)
//         ]
//     )

//     return createObjectLiteral({
//         valueType: 'function',
//         module: moduleExpression,
//         captured,
//         properties: Object.fromEntries(properties.map(k => [k, null])),
//     }, factory)
// }

// function createModuleFunction(
//     serializationData: ts.Expression,
//     factory = ts.factory,
// ) {
//     return factory.createArrowFunction(
//         undefined, 
//         undefined, 
//         [], 
//         undefined, 
//         undefined,
//         factory.createBlock([
//             factory.createReturnStatement(serializationData)
//         ], true)
//     )
// }

// function addModuleSymbolToFunction(
//     node: ts.FunctionDeclaration,
//     serializationData: ts.Expression,
//     factory = ts.factory,
// ) {
//     if (!node.name) {
//         failOnNode('Expected name', node)
//     }

//     const access = factory.createElementAccessExpression(
//         factory.createIdentifier(node.name.text),
//         createMovablePropertyName(factory)
//     )

//     return factory.createExpressionStatement(
//         factory.createAssignment(
//             access,
//             createModuleFunction(serializationData, factory)
//         )
//     )
// }

// function addModuleSymbolToMethod(
//     node: ts.MethodDeclaration, 
//     serializationData: ts.Expression,
//     factory = ts.factory,
// ) {
//     // const className = ts.isClassLike(node.parent) ? node.parent.name : undefined
//     // if (!className) {
//     //     // This should only happen for `export default class {}`
//     //     // Until we allow methods over `const C = class {}`
//     //     failOnNode('Expected class name', node.parent)
//     //     return
//     // }

//     const isStatic = node.modifiers?.find(x => x.kind === ts.SyntaxKind.StaticKeyword)
//     const className = factory.createThis()

//     const subject = isStatic ? className : factory.createPropertyAccessExpression(className, 'prototype')
//     const accessExp = ts.isIdentifier(node.name) 
//         ? factory.createPropertyAccessExpression(subject, node.name)
//         : factory.createElementAccessExpression(
//             subject,
//             ts.isComputedPropertyName(node.name) ? node.name.expression : node.name
//         )

//     const access = factory.createElementAccessExpression(
//         accessExp,
//         createMovablePropertyName(factory)
//     )

//     return factory.createExpressionStatement(
//         factory.createAssignment(
//             access,
//             createModuleFunction(serializationData, factory)
//         )
//     )
// }

// function createAssignExpression(target: ts.Expression, value: ts.Expression, factory = ts.factory) {
//     return factory.createCallExpression(
//         factory.createPropertyAccessExpression(
//             factory.createIdentifier('Object'),
//             'assign'
//         ),
//         undefined,
//         [target, value]
//     )
// }

// function addModuleSymbolToFunctionExpression(
//     node: ts.FunctionExpression | ts.ArrowFunction, 
//     serializationData: ts.Expression,
//     factory = ts.factory,
// ) {
//     const fn = factory.createArrowFunction(
//         undefined, 
//         undefined, 
//         [], 
//         undefined, 
//         undefined,
//         factory.createParenthesizedExpression(serializationData)
//     )

//     return createAssignExpression(node, factory.createObjectLiteralExpression([
//         factory.createPropertyAssignment(
//             factory.createComputedPropertyName(
//                 createSymbolPropertyName('__moveable__', factory)
//             ),
//             fn
//         )
//     ]), factory)
// }

// export function createImporterExpression(moduleType: 'cjs' | 'esm', factory = ts.factory) {
//     if (moduleType === 'cjs') {
//         return factory.createIdentifier('__filename')
//     }

//     return factory.createPropertyAccessExpression(
//         factory.createPropertyAccessExpression(
//             factory.createIdentifier('import'),
//             'meta'
//         ),
//         'filename'
//     )
// }

// type ClauseReplacement = [clause: ts.HeritageClause, ident: ts.Identifier] | undefined

// function addDeserializeConstructor(
//     node: ts.ClassExpression | ts.ClassDeclaration,
//     clauseReplacement: ClauseReplacement,
//     factory: ts.NodeFactory
// ) {
//     const descIdent = factory.createIdentifier('desc')
//     const tag = createSymbolPropertyName('deserialize', factory)
//     const fields = node.members.filter(isPrivateField)
//     const privateFieldsIdent = factory.createIdentifier('privateFields')
//     const privateFields = factory.createVariableStatement(
//         undefined,
//         factory.createVariableDeclarationList(
//           [factory.createVariableDeclaration(
//             privateFieldsIdent,
//             undefined,
//             undefined,
//             factory.createCallExpression(
//                 factory.createPropertyAccessExpression(
//                     factory.createPropertyAccessExpression(descIdent, 'privateFields'),
//                     'pop'
//                 ),
//                 undefined,
//                 []
//             )
//             )],
//           ts.NodeFlags.Const
//         )
//     )

//     const callSuper = factory.createExpressionStatement(
//         factory.createCallChain(
//             factory.createElementAccessExpression(factory.createSuper(), tag),
//             factory.createToken(ts.SyntaxKind.QuestionDotToken),
//             undefined,
//             [descIdent]
//         )
//     )

//     const hasSuper = !!node.heritageClauses?.find(c => c.token === ts.SyntaxKind.ExtendsKeyword)?.types?.[0]?.expression
//     const preamble = hasSuper ? [callSuper, privateFields] : [privateFields]

//     const method = factory.createMethodDeclaration(
//         undefined,
//         undefined,
//         factory.createComputedPropertyName(tag),
//         undefined,
//         undefined,
//         [factory.createParameterDeclaration(undefined, undefined, descIdent)],
//         undefined,
//         factory.createBlock([
//             ...preamble,
//             ...fields.map(decl => factory.createExpressionStatement(factory.createAssignment(
//                 factory.createPropertyAccessExpression(
//                     factory.createThis(), 
//                     factory.createIdentifier(getMappedPrivateName(node, decl.name)),
//                 ),
//                 factory.createElementAccessExpression(
//                     privateFieldsIdent,
//                     factory.createStringLiteral(decl.name.text, true)
//                 )
//             )))
//         ], true)
//     )

//     const transformed = transformPrivateMembers(node, getNullTransformationContext())
//     const heritageClauses = !clauseReplacement ? undefined : node.heritageClauses?.map(c => {
//         if (ts.getOriginalNode(c) === ts.getOriginalNode(clauseReplacement[0])) {
//             const clauseExp = factory.createExpressionWithTypeArguments(clauseReplacement[1], [])

//             return factory.updateHeritageClause(c, [clauseExp])
//         }

//         return c
//     })

//     const props: ClassProps = {
//         members: [...transformed.members, method],
//         heritageClauses: heritageClauses,
//     }

//     return updateClass(transformed, props, factory)
// }

// function isAssignedTo(node: ts.Node) {
//     if (ts.isBinaryExpression(node.parent)) {
//         return isAssignmentExpression(node.parent) && node.parent.left === node
//     }
// }

// function isConstantVariableDecl(sym: Symbol & { variableDeclaration: ts.VariableDeclaration }) {
//     // Treat other bindings as constant (`var` is largely ignored)
//     if (!(sym.variableDeclaration.parent.flags & ts.NodeFlags.Let)) {
//         return true
//     }

//     // No initializer implies not constant
//     if (!sym.variableDeclaration.initializer) {
//         return false
//     }

//     // `for (let i = ...)`
//     if (sym.variableDeclaration.parent.parent.kind === ts.SyntaxKind.ForStatement) {
//         // `for` loops bind symbols uniquely per-iteration
//         for (const exp of sym.references) {
//             if (isAssignedTo(exp)) {
//                 return false
//             }
//         }

//         return true
//     }

//     // TODO: check if `let` can be converted to `const`

//     return false
// }

// function getPrivateAccessExpressionSymbol(sym: Symbol): Symbol | undefined {
//     if (!sym.parent) {
//         return
//     }

//     if (!sym.name.startsWith('#')) {
//         return getPrivateAccessExpressionSymbol(sym.parent)
//     }

//     const ref = sym.references[0]
//     if (!ref || !ts.isPropertyAccessExpression(ref) || !ts.isPrivateIdentifier(ref.name)) {
//         return
//     }

//     return sym
// }

// function isConstantEnumDeclaration(sym: Symbol) {
//     if (!sym.declaration || !ts.isEnumDeclaration(sym.declaration)) {
//         return false
//     }

//     if (!sym.declaration.modifiers?.find(m => m.kind === ts.SyntaxKind.ConstKeyword)) {
//         return false
//     }

//     return true
// }

// function coerceNumber(value: any) {
//     if (typeof value === 'number') {
//         return value
//     }
//     if (typeof value === 'string') {
//         const parsed = Number(value)
//         if (!isNaN(parsed)) {
//             return parsed
//         }
//     }
// }

// function evaluateBinaryExpression(expression: ts.BinaryExpression,lookup?: (ident: string, node: ts.Node) => any) {
//     function evaluateNumber(exp: ts.Expression) {
//         const result = coerceNumber(evaluateExpression(exp, lookup))
//         if (result !== undefined) {
//             return result
//         }
//         failOnNode('Not a number', exp)
//     }

//     switch (expression.operatorToken.kind) {
//         case ts.SyntaxKind.PercentToken:
//             return evaluateNumber(expression.left) % evaluateNumber(expression.right)
//         case ts.SyntaxKind.AsteriskAsteriskToken:
//             return evaluateNumber(expression.left) ** evaluateNumber(expression.right)
//         case ts.SyntaxKind.AmpersandToken:
//             return evaluateNumber(expression.left) & evaluateNumber(expression.right)
//         case ts.SyntaxKind.BarToken:
//             return evaluateNumber(expression.left) | evaluateNumber(expression.right)
//         case ts.SyntaxKind.CaretToken:
//             return evaluateNumber(expression.left) ^ evaluateNumber(expression.right)
//         case ts.SyntaxKind.LessThanLessThanToken:
//             return evaluateNumber(expression.left) << evaluateNumber(expression.right)
//         case ts.SyntaxKind.GreaterThanGreaterThanToken:
//             return evaluateNumber(expression.left) >> evaluateNumber(expression.right)
//         case ts.SyntaxKind.GreaterThanGreaterThanGreaterThanToken:
//             return evaluateNumber(expression.left) >>> evaluateNumber(expression.right)
//         case ts.SyntaxKind.MinusToken:
//             return evaluateNumber(expression.left) - evaluateNumber(expression.right)
//         case ts.SyntaxKind.SlashToken:
//             return evaluateNumber(expression.left) / evaluateNumber(expression.right)
//         case ts.SyntaxKind.AsteriskToken:
//             return evaluateNumber(expression.left) * evaluateNumber(expression.right)
//         case ts.SyntaxKind.PlusToken:
//             // (string | number) + (string | number) is valid
//             return evaluateExpression(expression.left, lookup) + (evaluateExpression(expression.right, lookup) as any)
//     }

//     // TODO
//     failOnNode('Unhandled binary expression', expression)
// }

// function evaluatePrefixUnaryExpression(expression: ts.PrefixUnaryExpression, lookup?: (ident: string, node: ts.Node) => any) {
//     function evaluateNumber() {
//         const result = coerceNumber(evaluateExpression(expression.operand, lookup))
//         if (result !== undefined) {
//             return result
//         }
//         failOnNode('Not a number', expression.operand)
//     }

//     switch (expression.operator) {
//         case ts.SyntaxKind.TildeToken:
//             return ~evaluateNumber()
//         case ts.SyntaxKind.PlusToken:
//             return +evaluateNumber()
//         case ts.SyntaxKind.MinusToken:
//             return -evaluateNumber()
//     }

//     // TODO
//     failOnNode('Unhandled unary expression', expression)
// }

// // Basic impl. that doesn't handle all cases
// function evaluateExpression(expression: ts.Expression, lookup?: (ident: string, node: ts.Node) => any): any {
//     switch (expression.kind) {
//         case ts.SyntaxKind.ParenthesizedExpression:
//             return evaluateExpression((expression as ts.ParenthesizedExpression).expression)
//         case ts.SyntaxKind.NumericLiteral:
//             return Number((expression as ts.NumericLiteral).text)
//         case ts.SyntaxKind.StringLiteral:
//             return (expression as ts.StringLiteral).text
//         case ts.SyntaxKind.PrefixUnaryExpression:
//             return evaluatePrefixUnaryExpression(expression as ts.PrefixUnaryExpression, lookup)
//         case ts.SyntaxKind.BinaryExpression:
//             return evaluateBinaryExpression(expression as ts.BinaryExpression, lookup)
//         case ts.SyntaxKind.Identifier:
//             if (lookup) {
//                 return lookup((expression as ts.Identifier).text, expression)
//             }
//             failOnNode('Failed to evaluate identfiier', expression)
//         case ts.SyntaxKind.PropertyAccessExpression: {
//             const target = evaluateExpression((expression as ts.PropertyAccessExpression), lookup)
            
//             try {
//                 return target[(expression as ts.PropertyAccessExpression).name.text]
//             } catch (e) {
//                 failOnNode(`Bad property access: ${(e as any).message}`, expression)
//             }
//         }
//     }

//     // TODO
//     failOnNode('Unhandled expression', expression)
// }

// function getEnumMemberName(member: ts.EnumMember) {
//     switch (member.name.kind) {
//         case ts.SyntaxKind.Identifier:
//         case ts.SyntaxKind.StringLiteral:
//             return (member.name as ts.Identifier | ts.StringLiteral).text

//         case ts.SyntaxKind.ComputedPropertyName: {
//             const exp = ((member.name) as ts.ComputedPropertyName).expression
//             if (ts.isStringLiteralLike(exp)) {
//                 return exp.text
//             }
//         }

//         default:
//             failOnNode('Invalid member name', member.name)
//     }
// }

// function getEnumInlineValue(member: ts.EnumMember): number | string {
//     const decl = member.parent
//     const initializer = member.initializer
//     if (initializer) {
//         return evaluateExpression(initializer, (ident, node) => {
//             // Identifiers can reference sibling members
//             const sibling = decl.members.find(x => getEnumMemberName(x) === ident)
//             if (sibling === member) {
//                 failOnNode('Cannot use before assignment', node)
//             }

//             if (sibling) {
//                 return getEnumInlineValue(sibling)
//             }

//             // TODO: need to lookup symbols in outer scopes
//             failOnNode('Not implemented', node)
//         })
//     }


//     // Find the closest member with an initializer
//     const index = decl.members.indexOf(member)
//     const closest = decl.members.slice(0, index).filter(x => x.initializer).at(-1)
//     if (!closest) {
//         return index
//     }

//     const indexOfClosest = decl.members.indexOf(closest)
//     const val = getEnumInlineValue(closest)
//     if (typeof val !== 'number') {
//         failOnNode('Enum must have an initializer', member)
//     }

//     return val + (index - indexOfClosest)
// }

// interface SymbolMapping {
//     identifier: ts.Identifier
//     bound?: boolean
//     isDefault?: boolean
//     lateBound?: boolean
//     moduleSpec?: string
//     replacementStack?: ts.Expression[]
// }

// const replacementStacks = new Map<Symbol, ts.Expression[]>()

// function rewriteCapturedSymbols(
//     scope: Scope, 
//     captured: Symbol[], 
//     circularRefs: Set<Symbol>,
//     runtimeTransformer?: (node: ts.Node) => ts.Node,
//     infraTransformer?: (node: ts.Node, depth: number) => ts.Node,
//     depth = 0,
//     factory = ts.factory
// ) {
//     const inner = scope.node
//     const isMethod = ts.isMethodDeclaration(inner)

//     const refs = new Map<Symbol, ts.Node[]>(
//         captured.map(c => [c, getReferencesInScope(c, scope)]),
//     )

//     // Any symbol that is reassigned cannot be directly replaced
//     // TODO: call expressions with a `this` target cannot be captured directly
//     // The binding should probably be applied to all symbols unless we know the original
//     // declaration was `const`
//     const reduced = new Map<Symbol, ts.Node[]>()
//     const boundSymbols = new Set<Symbol>()
//     const defaultImports = new Set<Symbol>()
//     const replacements = new Map<ts.Node, ts.Node>()
//     for (const [sym, nodes] of refs.entries()) {
//         if (circularRefs.has(sym) && !sym.parent) {
//             boundSymbols.add(sym)
//             reduced.set(sym, nodes)
//         } else if (!sym.parent && sym.references.some(isAssignedTo)) {
//             boundSymbols.add(sym)
//             reduced.set(sym, nodes)
//         } else if (sym.parent && sym.parent.references.some(isAssignedTo)) {
//             // The first check is to handle this case:
//             // ```ts
//             // const c = []
//             // function f(u, v) {
//             //     const a = c[u.value.id] ??= []
//             //     a[v.value.id] = r
//             // }
//             // ```
//             // 
//             // Right now this logic will always choose to bind `c`
//             // 
//             // The symbol `a[v.value.id]` should be ignored because it's local
//             // to the scope. The only symbol we care about is `c[u.value.id]`
//             //
//             // Because the argument is computed _and_ local to the scope, we
//             // cannot bind `c[u.value.id]`
            
//             if (sym.parent.computed && sym.parent.parent) {
//                 boundSymbols.add(sym.parent.parent)
//                 reduced.set(sym.parent.parent, getReferencesInScope(sym.parent.parent, scope))
//             } else {
//                 boundSymbols.add(sym.parent)
//                 reduced.set(sym.parent, getReferencesInScope(sym.parent, scope))
//             }
//         } else if (sym.variableDeclaration && !isConstantVariableDecl(sym as Symbol & { variableDeclaration: ts.VariableDeclaration })) {
//             boundSymbols.add(sym)
//             reduced.set(sym, nodes)
//         } else {
//             const privateExp = getPrivateAccessExpressionSymbol(sym)
//             if (privateExp) {
//                 reduced.set(privateExp, getReferencesInScope(privateExp, scope))

//                 continue
//             }

//             const root = getRootSymbol(sym)

//             const importClause = root.importClause
//             if (!importClause) {
//                 if (root !== sym && isConstantEnumDeclaration(root)) {
//                     // We can omit this capture only if there are no direct refs
//                     const refs = getReferencesInScope(root, scope)
//                     const hasDirect = refs.some(r =>  r.parent?.kind !== ts.SyntaxKind.PropertyAccessExpression)

//                     if (hasDirect) {
//                         reduced.set(root, refs)
//                     } else {
//                         const inlineValue = createLiteral(getEnumInlineValue(sym.declaration as ts.EnumMember))
//                         for (const n of nodes) {
//                             replacements.set(n, inlineValue)
//                         }
//                     }
//                 } else {
//                     // Don't rewrite `this` immediately inside a method declaration.
//                     // We can't capture the reference to `this` directly.
//                     if (isMethod && root.name === 'this') continue

//                     reduced.set(root, getReferencesInScope(root, scope))
//                 }

//                 continue
//             }

//             // Don't split up the module if we reference it directly
//             if (refs.has(root)) {
//                 reduced.set(root, getReferencesInScope(root, scope))
//                 const name = importClause.name
//                 if (name?.text === sym.name) {
//                     defaultImports.add(sym)
//                 }

//                 continue
//             }

//             const bindings = importClause.namedBindings
//             if (!bindings || !ts.isNamespaceImport(bindings)) {
//                 reduced.set(root, getReferencesInScope(root, scope))

//                 continue
//             }

//             // XXX: do not reduce built-in modules
//             const spec = importClause.parent.moduleSpecifier as ts.StringLiteral
//             if (spec.text.startsWith('node:')) {
//                 reduced.set(root, getReferencesInScope(root, scope))

//                 continue
//             }

//             // Symbol is from a module, let's reduce it
//             let didReduce = false
//             for (const [name, member] of root.members.entries()) {
//                 const nodes = getReferencesInScope(member, scope)
//                 if (nodes.length > 0) {
//                     reduced.set(member, nodes)
//                     didReduce = true
//                 }
//             }
//             if (!didReduce) {
//                 reduced.set(root, getReferencesInScope(root, scope))
//             }
//         }
//     }

//     // Map all symbols to a valid identifier. Root symbols can be used as-is
//     const idents = new Map<Symbol, SymbolMapping>()
//     for (const sym of reduced.keys()) {
//         if (!replacementStacks.has(sym)) {
//             replacementStacks.set(sym, [])
//         }

//         const isBound = boundSymbols.has(sym)
//         const isThis = sym.name === 'this'
//         const text = (sym.parent || isBound) ? `__${idents.size}` : isThis ? '__thisArg' : sym.name
//         idents.set(sym, {
//             identifier: factory.createIdentifier(text),
//             bound: isBound,
//             isDefault: defaultImports.has(sym),
//             lateBound: circularRefs.has(sym),
//             // moduleSpec,
//             replacementStack: replacementStacks.get(sym),
//         })
//     }

//     for (const [sym, nodes] of reduced) {
//         const { identifier, bound } = idents.get(sym)!
//         const newNode = bound 
//             ? factory.createPropertyAccessExpression(identifier, sym.name)
//             : identifier

//         replacementStacks.get(sym)![depth] = newNode

//         for (const n of nodes) {
//             // We don't need to replace equivalent nodes
//             const isShorthand = ts.isShorthandPropertyAssignment(n.parent)
//             if (!isShorthand && ts.isIdentifier(n) && newNode === identifier && newNode.text === n.text) {
//                 continue
//             }
 
//             const exp = isShorthand 
//                 ? factory.createPropertyAssignment(n.parent.name, cloneNode(newNode) as ts.Expression)
//                 : cloneNode(newNode)

//             ts.setTextRange(exp, n)
//             ts.setSourceMapRange(exp, n)

//             replacements.set(n, exp)
//         }
//     }

//     try {
//         const node = runtimeTransformer?.(inner) ?? inner
//         const infraNode = infraTransformer?.(inner, depth + 1) ?? inner
//         if (replacements.size === 0) {
//             return { node, infraNode, parameters: idents }
//         }

//         // Replace all non-root references with the identifier
//         const context = getNullTransformationContext()
//         function visit(node: ts.Node): ts.Node {
//             return replacements.get(node) ?? ts.visitEachChild(node, visit, context)
//         }

//         return {
//             node: visit(node),
//             // TODO: this doesn't need to be recursive
//             infraNode: visit(infraNode), // TODO: can be made more efficient. We are creating orphaned files atm
//             parameters: idents,
//         }
//     } catch (e) {
//         if (e instanceof CancelError) {
//             throw e
//         }
//         throw new Error(`Failed to rewrite symbols at: ${getNodeLocation(scope.node)}`, { cause: e })
//     }
// }

// const getCloneNode = memoize(function () {
//     if (!('cloneNode' in ts.factory)) {
//         throw new Error('"cloneNode" is missing from the typescript module')
//     }

//     return ts.factory.cloneNode as (node: ts.Node) => ts.Node
// })

// function cloneNode(node: ts.Node): ts.Node {
//     return getCloneNode()(node)
// }


// function getBindingName(target: ts.Expression) {
//     if (!ts.isPropertyAccessExpression(target)) {
//         failOnNode('Expected a property access expression', target)
//     }

//     // if (!ts.isIdentifier(target.name)) {
//     //     failOnNode('Not an identifier', target.name)
//     // }

//     return target.name
// }

// // This impl. doesn't work correctly in "scoped" cases. It's only kept around because
// // it handles capturing symbols that don't have a corresponding declaration/statement
// function renderLegacyBoundSymbol(symbol: Symbol, target: ts.Expression, lateBound = false, factory = ts.factory) {
//     const assignment = ts.isIdentifier(target) 
//         ? factory.createShorthandPropertyAssignment(target)
//         : factory.createPropertyAssignment(getBindingName(target), target)

//     return factory.createObjectLiteralExpression([
//         assignment,
//         factory.createPropertyAssignment(
//             factory.createComputedPropertyName(createSymbolPropertyName('symbolId', factory)),
//             createObjectLiteral({
//                 id: symbol.id,
//                 origin: factory.createIdentifier('__filename'),
//                 lateBound,
//             }, factory)
//         )
//     ])
// }

// function renderBoundSymbol(symbol: Symbol, target: ts.Expression, lateBound = false, factory = ts.factory) {
//     if (symbol.name.includes('.')) {
//         const message = `Unexpected symbol "${symbol.name}" [computed: ${symbol.computed}]`
//         const decl = symbol.declaration ?? symbol.parent?.declaration
//         if (decl) {
//             failOnNode(message, decl)
//         }
//         throw new Error(message)
//     }

//     const assignment = ts.isIdentifier(target) 
//         ? factory.createShorthandPropertyAssignment(target)
//         : factory.createPropertyAssignment(getBindingName(target), target)

//     const elements: ts.ObjectLiteralElementLike[] = [assignment]
//     if (lateBound) {
//         elements.push(factory.createPropertyAssignment(
//             factory.createComputedPropertyName(createSymbolPropertyName('symbolId', factory)),
//             createObjectLiteral({
//                 id: symbol.id, // This id is local to the _module_ !!!
//                 origin: factory.createIdentifier('__filename'),
//                 lateBound,
//             }, factory)
//         ))
//     }

//     const objExp = factory.createObjectLiteralExpression(elements)

//     const symName = `__symbol${symbol.id}`
//     const tmpDecl = factory.createVariableStatement(
//         undefined,
//         factory.createVariableDeclarationList([
//             factory.createVariableDeclaration(symName)
//         ], ts.NodeFlags.Let)
//     )

//     const lazyFn = factory.createArrowFunction(undefined, undefined, [], undefined, undefined, factory.createBinaryExpression(
//         factory.createIdentifier(symName),
//         ts.SyntaxKind.QuestionQuestionEqualsToken,
//         objExp
//     ))

//     const fnName = `__getSymbol${symbol.id}`
//     const lazyFnDecl = createVariableStatement(fnName, lazyFn)

//     return {
//         statements: [tmpDecl, lazyFnDecl],
//         expression: factory.createCallExpression(factory.createIdentifier(fnName), undefined, []),
//     }
// }

// function getMappedSymbolExpression(
//     symbol: Symbol, 
//     replacementStack: ts.Expression[] = [],
//     depth = 0
// ) {
//     if (replacementStack && depth > 0) {
//         // It's probably ok to just check for `mapping.replacementStack[depth - 1]`
//         for (let i = depth - 1; i >= 0; i--) {
//             if (replacementStack[i]) {
//                 if (i !== depth - 1) {
//                     throw new Error(`Nope: ${i} !== ${depth - 1}`)
//                 }
//                 return replacementStack[i]
//             }
//         }

//         // return replacementStack[depth - 1]
//     }

//     if (!symbol.parent) {
//         return factory.createIdentifier(symbol.name)
//     }

//     return cloneNode(symbol.references[0]!) as ts.Expression
// }

// function renderConstSymbol(symbol: Symbol, mapping: Omit<SymbolMapping, 'identifier'>, depth = 0, printDebug = false) {
//     const exp = getMappedSymbolExpression(symbol, mapping.replacementStack, depth)
//     if (printDebug) {
//         const rootSym = getRootSymbol(symbol)
//         const location = rootSym.declaration ? getNodeLocation(rootSym.declaration) : undefined
//         if (location) {
//             ts.setSyntheticLeadingComments(exp, [createSyntheticComment(` ${location}`)])
//         }    
//     }

//     return exp
// }

// let _moduleExports: ts.PropertyAccessExpression | undefined

// function createExportedDefaultFunction(
//     parameters: ts.ParameterDeclaration[],
//     block: ts.Block, 
//     moduleType: 'cjs' | 'esm' = 'cjs'
// ) {
//     if (moduleType === 'esm') {
//         return factory.createFunctionDeclaration(
//             [
//                 factory.createModifier(ts.SyntaxKind.ExportKeyword),
//                 factory.createModifier(ts.SyntaxKind.DefaultKeyword),
//             ],
//             undefined,
//             undefined, // factory.createIdentifier(name),
//             undefined,
//             parameters,
//             undefined,
//             block
//         )
//     }

//     if (!_moduleExports) {
//         _moduleExports = factory.createPropertyAccessExpression(
//             factory.createIdentifier('module'),
//             factory.createIdentifier('exports')
//         )
//     }

//     const exp = factory.createAssignment(_moduleExports, factory.createFunctionExpression(
//         [],
//         undefined,
//         undefined,
//         undefined,
//         parameters,
//         undefined,
//         block
//     ))

//     return factory.createExpressionStatement(exp)
// }

// export interface CompiledFile {
//     readonly sourceNode: ts.Node
//     readonly name: string
//     readonly source: string
//     readonly path: string
//     readonly data: string
//     readonly infraData: string
//     readonly infraDeps?: string[]
//     readonly parameters: [Symbol, SymbolMapping][]
//     readonly sourcesmaps?: {
//         readonly runtime: SourceMapV3
//         readonly infra: SourceMapV3
//     }
//     readonly rawData?: RawPrintResult
//     readonly rawInfraData?: RawPrintResult
//     // These are the names of all artifacts referenced by the source
//     // readonly artifactDependencies: string[]
// }

// // const cache = new Map<string, ts.Node>()
// // const nodeIds = new Map<ts.Node, number>()
// // const getNodeId = (node: ts.Node) => {
// //     if (nodeIds.has(node)) {
// //         return nodeIds.get(node)!
// //     }

// //     const id = nodeIds.size
// //     nodeIds.set(node, id)

// //     return id
// // }

// // const getNodeCacheKey = (node: ts.Node, depth: number) => {
// //     const id = getNodeId(ts.getOriginalNode(node))

// //     return `${id}-${depth}`
// // }

// function isClassElementModifier(modifier: ts.ModifierLike) {
//     switch (modifier.kind) {
//         case ts.SyntaxKind.StaticKeyword:
//         case ts.SyntaxKind.PublicKeyword:
//         case ts.SyntaxKind.PrivateKeyword: 
//         case ts.SyntaxKind.ProtectedKeyword:
//             return true
//     }

//     return false
// }

// function convertMethodToFunction(node: ts.MethodDeclaration) {
//     const name = ts.factory.createIdentifier('__fn') // TODO: use the method name and sanitize keywords
//     const modifiers = node.modifiers?.filter(x => !isClassElementModifier(x))

//     return ts.factory.createFunctionDeclaration(
//         modifiers,
//         node.asteriskToken,
//         name,
//         undefined,
//         node.parameters,
//         undefined,
//         node.body
//     )
// }

// export function getModuleType(opt: ts.ModuleKind | undefined): 'cjs' | 'esm' {
//     switch (opt) {
//         case ts.ModuleKind.ES2015:
//         case ts.ModuleKind.ES2020:
//         case ts.ModuleKind.ES2022:
//         case ts.ModuleKind.ESNext:
//             return 'esm'

//         case undefined: // TODO: default to `esm`
//         case ts.ModuleKind.Node16:
//         case ts.ModuleKind.NodeNext:
//         case ts.ModuleKind.CommonJS:
//             return 'cjs'

//         default:
//             throw new Error(`Module kind not supported: ${opt}`) // FIXME: make this user friendly
//     }
// }

// export function createGraphCompiler(
//     sourceMapHost: SourceMapHost,
//     compilerOptions: ts.CompilerOptions, 
//     moduleType = getModuleType(compilerOptions.module)
// ) {
//     const isSynBackend = (ts as any).isSyn 
//     const rootGraphs = new Map<ts.SourceFile, RootScope>()
//     const compiled = new Map<string, Map<string, CompiledFile>>()
//     const emitSourceMap = !!compilerOptions.sourceMap
//     const printerOptions: PrinterOptions = isSynBackend
//         ? { emitSourceMap, _stripTypes: true, inlineSourceMap: false }
//         : { emitSourceMap }

//     const dependencyStack: string[][] = []

//     function getGraph(node: ts.SourceFile) {
//         if (rootGraphs.has(node)) {
//             return rootGraphs.get(node)!
//         }

//         const graph = createGraph(node)
//         rootGraphs.set(node, graph)

//         return graph
//     }

//     function getSymbol(node: ts.Node) {
//         const graph = getGraph(node.getSourceFile())

//         return graph.symbols.get(node)
//     }

//     function isDeclared(node: ts.Node) {
//         return !!getSymbol(ts.getOriginalNode(node))?.isDeclared
//     }

//     const capturedSymbols = new Map<Symbol, Symbol[]>()
//     function getCaptured(sym: Symbol): Symbol[] {
//         if (capturedSymbols.has(sym)) {
//             return capturedSymbols.get(sym)!
//         }

//         const scope = getContainingScope(sym)
//         if (!scope.node) {
//             const r: Symbol[] = []
//             capturedSymbols.set(sym, r)

//             return r
//         }

//         const captured = getImmediatelyCapturedSymbols(scope)
//         capturedSymbols.set(sym, captured)

//         return captured
//     }

//     const dependencies = new Map<Symbol, Set<Symbol>>()
//     function getAllDependencies(sym: Symbol): Set<Symbol> {
//         if (dependencies.has(sym)) {
//             return dependencies.get(sym)!
//         }

//         const deps = new Set<Symbol>()
//         dependencies.set(sym, deps)
//         for (const s of getCaptured(sym)) {
//             deps.add(s)
//             getAllDependencies(s).forEach(c => deps.add(c))
//         }

//         return deps
//     }

//     function getCaptured2(node: ts.Node) {
//         const sourceFile = ts.getOriginalNode(node).getSourceFile()
//         const graph = getSubscopeContaining(getGraph(sourceFile), sourceFile)
//         const targetGraph = getSubscopeDfs(graph, node)
//         if (!targetGraph) {
//             failOnNode('No graph found', node)
//         }
//         if (targetGraph === graph) {
//             failOnNode('Got source file graph', node)
//         }

//         const { captured } = getScopeEnvironment(targetGraph)

//         return captured
//     }

//     function isCircularReference(currentSymbol: Symbol, nextSymbol: Symbol) {
//         return getAllDependencies(nextSymbol).has(currentSymbol)
//     }

//     const assets = new Map<string, { literal: ts.Expression }>() // unused


//     function lowerNode(
//         node: ts.Node, 
//         factory: ts.NodeFactory, 
//         runtimeTransformer?: (node: ts.Node) => ts.Node,
//         infraTransformer?: (node: ts.Node, depth: number) => ts.Node,
//         clauseReplacement: ClauseReplacement = undefined,
//         excluded: ts.Node[] = [],
//         depth = 0
//     ) {
//         const sourceFile = ts.getOriginalNode(node).getSourceFile()
//         const g = getGraph(sourceFile)
//         const graph = getSubscopeContaining(g, sourceFile)
//         //const immediateEnclosingScope = ts.findAncestor(node, n => (ts.isSourceFile(n) || ts.isFunctionDeclaration(n)) && n !== node)
//         //const parentGraph = immediateEnclosingScope === sourceFile ? graph : getSubgraphContaining(graph, immediateEnclosingScope!)
//         const targetGraph = g.scopeCache.get(node) ?? getSubscopeDfs(graph, node)
//         if (!targetGraph) {
//             failOnNode('No graph found', node)
//         }
//         if (targetGraph === graph) {
//             failOnNode('Got source file graph', node)
//         }

//         const res = getScopeEnvironment(
//             targetGraph, 
//             excluded.map(n => ts.getOriginalNode(n)).map(n => getSubscopeDfs(graph, n)!)
//         )

//         const extracted: ts.Node[] = []

//         const circularRefs = !targetGraph.symbol 
//             ? new Set<Symbol>() 
//             : new Set(res.captured.filter(s => isCircularReference(targetGraph.symbol!, s)))

//         const rewritten = rewriteCapturedSymbols(
//             targetGraph,
//             res.captured,
//             circularRefs,
//             runtimeTransformer,
//             infraTransformer,
//             depth,
//             factory
//         )

//         if (clauseReplacement) {
//             rewritten.parameters.set({ name: clauseReplacement[1].text } as any, { identifier: clauseReplacement[1] })
//         }


//         // Symbols are sorted by the # of parents first followed by their
//         // symbol id as a proxy for their position
//         //
//         // TODO: rename all identifiers to use their symbol id
//         function compareSymbols(a: Symbol, b: Symbol): number {
//             if (!a.parent && !b.parent) {
//                 return a.id - b.id
//             } else if (a.parent && !b.parent) {
//                 return 1
//             } else if (!a.parent && b.parent) {
//                 return -1
//             }

//             return compareSymbols(a.parent!, b.parent!) || (a.id - b.id)
//         }

//         // The order of the parameters matters (obviously...) so we convert the map to 
//         // an array to ensure that the parameters are always read in the same order
//         const parameters = [...rewritten.parameters.entries()].sort((a, b) => compareSymbols(a[0], b[0]))

//         function finalize(node: ts.Node) {
//             if (ts.isClassDeclaration(node) || ts.isClassExpression(node)) {
//                 return addDeserializeConstructor(node, clauseReplacement, factory)
//             }

//             if (ts.isMethodDeclaration(node)) {
//                 return convertMethodToFunction(node)
//             }
    
//             return ts.isVariableDeclaration(node) ? node.initializer! : node
//         }

//         function createClosure(body: ts.Node) {
//             const finalized = finalize(body)
//             const withoutModifiers = (ts.isFunctionDeclaration(finalized) || ts.isClassDeclaration(finalized))
//                 ? removeModifiers(finalized, [ts.SyntaxKind.ExportKeyword, ts.SyntaxKind.DefaultKeyword], factory)
//                 : factory.createReturnStatement(finalized as ts.Expression)

//             ts.setSourceMapRange(withoutModifiers, targetGraph!.node)    

//             const statements = ts.isFunctionDeclaration(finalized) || ts.isClassDeclaration(finalized)
//                 ? [
//                     withoutModifiers,
//                     factory.createReturnStatement(finalized.name) // TODO: handle `export default class/function`
//                 ]
//                 : [withoutModifiers]

//             const block = factory.createBlock(statements, true)
//             // note: values that are exclusively used during instantiation do not need to be captured for 
//             // serialization/deserialization of class instances
//             const params = parameters.map(c => factory.createParameterDeclaration(undefined, undefined, c[1].identifier))

//             return [
//                 ...extracted,
//                 createExportedDefaultFunction(params, block, moduleType)
//             ]
//         }

//         if (rewritten.node === rewritten.infraNode) {
//             return { 
//                 extracted: createClosure(rewritten.node), 
//                 parameters,
//                 assets: assets.size > 0 ? assets : undefined,
//             }
//         }

//         return { 
//             extracted: createClosure(rewritten.node), 
//             extractedInfra: createClosure(rewritten.infraNode),
//             parameters,
//             assets: assets.size > 0 ? assets : undefined,
//         }
//     }

//     const consumers: ((file: CompiledFile) => Promise<void> | void)[] = []
//     function onEmitFile(consumer: (file: CompiledFile) => Promise<void> | void) {
//         consumers.push(consumer)
//     }

//     function emitFile(file: CompiledFile) {
//         for (const consumer of consumers) {
//             consumer(file)
//         }
//     }

//     function compileNode(
//         name: string, 
//         node: ts.Node, 
//         factory: ts.NodeFactory,
//         runtimeTransformer?: (node: ts.Node) => ts.Node,
//         infraTransformer?: (node: ts.Node, depth: number) => ts.Node,
//         clauseReplacement: ClauseReplacement = undefined,
//         excluded: ts.Node[] = [],
//         depth = 0
//     ) {
//         const sourceFile = node.getSourceFile()
//         if (!sourceFile) {
//             failOnNode('Missing source file', node)
//         }

//         let chunks = compiled.get(sourceFile.fileName)
//         if (!chunks) {
//             chunks = new Map()
//             compiled.set(sourceFile.fileName, chunks)
//         }

//         let res = chunks.get(name)
//         if (!res) {
//             if (depth > 0) {
//                 dependencyStack[dependencyStack.length - 1].push(name)
//             }
    
//             // `doCompile` pops the stack
//             dependencyStack.push([])

//             const chunk = doCompile()
//             chunks.set(name, chunk)
//             emitFile(chunk)
            
//             res = chunk
//         }

//         return {
//             captured: res.parameters,
//             assets: res.assets,
//         }

//         function doCompile() {
//             const { extracted, extractedInfra, parameters, assets } = lowerNode(node, factory, runtimeTransformer, infraTransformer, clauseReplacement, excluded, depth)
//             const outfile = sourceFile.fileName.endsWith('.syn')
//                 ? sourceFile.fileName.replace(/\.syn$/, `-${name}.syn`)
//                 : sourceFile.fileName.replace(/\.(t|j)(sx?)$/, `-${name}.$1$2`)

//             const result = emitChunk(sourceMapHost, sourceFile, extracted as ts.Statement[], printerOptions) 
//             const resultInfra = extractedInfra === undefined 
//                 ? result
//                 : emitChunk(sourceMapHost, sourceFile, extractedInfra as ts.Statement[], printerOptions)

//             if (isSynBackend) {
//                 return {
//                     sourceNode: ts.getOriginalNode(node),
//                     name,
//                     source: sourceFile.fileName,
//                     path: outfile,
//                     data: '',
//                     infraData: '',
//                     parameters,
//                     assets,
//                     infraDeps: dependencyStack.pop(),
//                     rawData: result.result!,
//                     rawInfraData: resultInfra.result!,
//                 } satisfies CompiledFile
//             }

//             return {
//                 sourceNode: ts.getOriginalNode(node),
//                 name,
//                 source: sourceFile.fileName,
//                 path: outfile,
//                 data: result.text,
//                 infraData: resultInfra.text,
//                 parameters,
//                 assets,
//                 infraDeps: dependencyStack.pop(),
//                 sourcesmaps: emitSourceMap ? {
//                     runtime: result.sourcemap!,
//                     infra: resultInfra.sourcemap!,
//                 } : undefined,
//             }
//         }
//     }

//     return { getSymbol, compileNode, compiled, onEmitFile, isDeclared, getAllDependencies, getCaptured2, moduleType }
// }

// interface StatementUpdate {
//     readonly before?: ts.Statement[]
//     readonly after?: ts.Statement[]
// }

// function* updateStatements(
//     statements: ts.Statement[] |  ts.NodeArray<ts.Statement>, 
//     updates: Map<ts.Statement, StatementUpdate[]>
// ) {
//     for (const node of statements) {
//         const updateToApply = updates.get(ts.getOriginalNode(node) as ts.Statement)
//         const before = updateToApply?.reduce((a, b) => a.concat(b.before ?? []), [] as ts.Statement[])
//         const after = updateToApply?.reduce((a, b) => a.concat(b.after ?? []), [] as ts.Statement[])

//         if (before) {
//             yield* before
//         }

//         yield node

//         if (after) {
//             yield* after
//         }
//     }
// }

// function getAnonymousFunctionName(node: ts.ArrowFunction | ts.FunctionExpression) {
//     if (ts.isVariableDeclaration(node.parent) && node.parent.initializer === node && ts.isIdentifier(node.parent.name)) {
//         const name = node.parent.name.text
//         if (node.parent.parent.flags & ts.NodeFlags.Const) { // Is this even correct????
//             return name
//         }

//         return `${name}_${hashNode(node).slice(0, 16)}`
//     }

//     if (ts.isPropertyAssignment(node.parent) && node.parent.initializer === node && ts.isIdentifier(node.parent.name)) {
//         const name = node.parent.name.text

//         return `${name}_${hashNode(node).slice(0, 16)}`
//     }

//     if (ts.isJsxExpression(node.parent) && ts.isJsxAttribute(node.parent.parent) && ts.isIdentifier(node.parent.parent.name)) {
//         const name = node.parent.parent.name.text

//         return `${name}_${hashNode(node).slice(0, 16)}`
//     }
    
//     return `function_${hashNode(node).slice(0, 16)}`
// }

// export function createRuntimeTransformer(
//     compiler: ReturnType<typeof createGraphCompiler>,
//     resourceTypeChecker?: ResourceTypeChecker
// ): (node: ts.Node) => ts.Node {
//     const context = getNullTransformationContext()

//     function visitCallExpression(node: ts.CallExpression) {
//         const sym = node.expression.getSourceFile() ? compiler.getSymbol(node.expression) : undefined
//         if (!sym) {
//             return ts.visitEachChild(node, visit, context)
//         }

//         const callableMember = resourceTypeChecker?.getCallableMemberName(sym)
//         if (!callableMember) {
//             return ts.visitEachChild(node, visit, context)
//         }

//         return factory.updateCallExpression(
//             node,
//             factory.createPropertyAccessExpression(node.expression, callableMember),
//             node.typeArguments,
//             node.arguments.map(visit) as ts.Expression[],
//         )
//     }

//     function visit(node: ts.Node): ts.Node {
//         if (ts.isCallExpression(node)) {
//             return visitCallExpression(node)
//         }

//         if (ts.isImportDeclaration(node)) {
//             const spec = (node.moduleSpecifier as ts.StringLiteral).text
//             if (spec.endsWith('.zig')) {
//                 return ts.factory.updateImportDeclaration(
//                     node,
//                     node.modifiers,
//                     node.importClause,
//                     ts.factory.createStringLiteral(spec.replace(/\.zig$/, '.zig.js')),
//                     undefined,
//                 )
//             }
//         }

//         return ts.visitEachChild(node, visit, context)
//     }

//     return visit
// }

// export function createSerializer(
//     compiler: ReturnType<typeof createGraphCompiler>,
//     resourceTypeChecker?: ResourceTypeChecker
// ) {
//     const moduleType = compiler.moduleType
//     const patchedDefaultExports = new Set<Symbol>()

//     const names = new Set<string>()
//     const nameMap = new Map<ts.Node, string>()
//     function getUniqueName(node: ts.Node, name: string) {
//         if (nameMap.has(node)) {
//             return nameMap.get(node)!
//         }

//         let count = 0
//         const getName = () => count === 0 ? name : `${name}_${count}`
//         while (names.has(getName())) count++

//         const result = getName()
//         names.add(result)
//         nameMap.set(node, result)
    
//         return result
//     }

//     // This function will re-compile any emitted files to include serialization data
//     function createInfraTransformer(name: string, innerTransformer?: (node: ts.Node) => ts.Node): (node: ts.Node, depth: number) => ts.Node {
//         const context = getNullTransformationContext()

//         return (node, depth) => {
//             throwIfCancelled()

//             const transformer = createTransformer(context, innerTransformer, name, depth)

//             // First transform adds the '__moveable__' symbol
//             // Second transform deals with `__scope__`

//             const withMoveable = ts.visitEachChild(node, transformer.visit, context)

//             return innerTransformer?.(withMoveable) ?? withMoveable
//         }
//     }

//     const runtimeTransformer = createRuntimeTransformer(compiler, resourceTypeChecker)

//     function createTransformer(
//         context = getNullTransformationContext(), 
//         innerTransformer?: (node: ts.Node) => ts.Node,
//         namePrefix?: string,
//         depth = 0
//     ) {
//         const factory = context.factory

//         // statements to add _after_ the target node
//         const updates = new Map<ts.Statement, StatementUpdate[]>()
//         function addStatementUpdate(node: ts.Statement, update: StatementUpdate) {
//             if (!updates.has(node)) {
//                 updates.set(node, [])
//             }
//             updates.get(node)!.push(update)
//         }

//         // Why is this prefixed `hoist` when it doesn't hoist anything?
//         function hoistSerializationData(node: ts.FunctionDeclaration, name: string, captured: ts.Expression[]) {
//             const serializationData = createSerializationData(name, captured, factory, moduleType)
//             addStatementUpdate(ts.getOriginalNode(node) as ts.Statement, {
//                 after: [addModuleSymbolToFunction(node, serializationData, factory)]
//             })
//         }

//         function addClassMethodSerialization(node: ts.MethodDeclaration, name: string, captured: ts.Expression[]) {
//             const serializationData = createSerializationData(name, captured, factory, moduleType)
//             const instrumentation = addModuleSymbolToMethod(node, serializationData, factory)
//             staticStack[staticStack.length-1].push(instrumentation)
//         }

//         const boundSymbolExpressions = new Map<Symbol, ts.Expression>()
//         function renderSymbol(symbol: Symbol, mapping: SymbolMapping, depth: number) {
//             if (!mapping.bound) {
//                 if (depth === 0 && moduleType === 'cjs' && mapping.isDefault && !patchedDefaultExports.has(symbol)) {
//                     patchedDefaultExports.add(symbol)
//                     const node = symbol.importClause!.parent
//                     const moveSymbolExp = factory.createElementAccessExpression(factory.createIdentifier(symbol.name), createSymbolPropertyName('__moveable__2', factory))
//                     const b = factory.createExpressionStatement(
//                         factory.createAssignment(
//                             factory.createPropertyAccessExpression(
//                                 factory.createElementAccessExpression(
//                                     factory.createPropertyAccessExpression(
//                                         factory.createCallExpression(moveSymbolExp, undefined, undefined),
//                                         'operations'
//                                     ),
//                                     0
//                                 ),
//                                 '_d'
//                             ),
//                             factory.createTrue()
//                         )
//                     )
//                     addStatementUpdate(node as ts.Statement, {
//                         after: [b],
//                     })
//                 }

//                 return renderConstSymbol(symbol, mapping, depth)
//             }

//             if (boundSymbolExpressions.has(symbol)) {
//                 return boundSymbolExpressions.get(symbol)!
//             }

//             const exp = getMappedSymbolExpression(symbol, mapping.replacementStack, depth)
//             const transforms = renderBoundSymbol(symbol, exp, mapping.lateBound)
//             if (!symbol.declaration) {
//                 const fallback = renderLegacyBoundSymbol(symbol, exp, mapping.lateBound)
//                 boundSymbolExpressions.set(symbol, fallback)
//                 //throw new Error(`Missing symbol declaration: ${symbol.name}`)
//                 return fallback
//             }

//             const statement = ts.isVariableDeclaration(symbol.declaration) 
//                 ? symbol.declaration.parent.parent
//                 : symbol.declaration

//             if (!ts.isStatement(statement)) {
//                 const fallback = renderLegacyBoundSymbol(symbol, exp, mapping.lateBound)
//                 boundSymbolExpressions.set(symbol, fallback)
//                 // failOnNode('Not a statement', statement)
//                 return fallback
//             }

//             boundSymbolExpressions.set(symbol, transforms.expression)

//             addStatementUpdate(ts.getOriginalNode(statement) as ts.Statement, {
//                 after: transforms.statements,
//             })

//             return transforms.expression
//         }

//         function renderCapturedSymbols(mappings: [Symbol, SymbolMapping][]) {
//             const base = mappings.map(([x, v]) => renderSymbol(x, v, depth))

//             return base
//         }    

//         // FIXME: if an external class is used for deserialization and is also embedded into a module export
//         // then `instanceof` won't work between "moved" instances and instantiations within the export
//         //
//         // Isolating every declaration is one way to solve this
//         function extractClassDeclaration(node: ts.ClassDeclaration | ts.ClassExpression) {
//             node = ts.getOriginalNode(node) as ts.ClassDeclaration | ts.ClassExpression
//             const name = getName(node)

//             // XXX: visit heritage clauses first
//             nameStack.push(name)
//             node.heritageClauses?.forEach(visit)
//             nameStack.pop()

//             const clauseReplacement = Array.from(mappedClauses.entries())
//                 .map(([k, v]) => [k, v.ident] as NonNullable<ClauseReplacement>)
//                 .find(c => node.heritageClauses?.includes(ts.getOriginalNode(c[0]) as any))
        
//             const excluded = clauseReplacement ? [clauseReplacement[0]] : undefined

//             return {
//                 ...compiler.compileNode(name, node, factory, runtimeTransformer, createInfraTransformer(name, innerTransformer), clauseReplacement, excluded, depth),
//                 clauseReplacement,
//             }
//         }

//         const nameStack: string[] = namePrefix ? [namePrefix] : []
//         function getName(node: ts.Node) {
//             if (node.kind === ts.SyntaxKind.SuperKeyword) {
//                 return nameStack.length === 0 ? 'super' : `${nameStack[nameStack.length - 1]}::super`
//             }

//             const original = ts.getOriginalNode(node) as ts.ClassDeclaration | ts.FunctionDeclaration | ts.FunctionExpression | ts.ArrowFunction
//             const name = original.name?.text ?? getAnonymousFunctionName(node as any)

//             if (nameStack.length === 0) {
//                 return getUniqueName(original, name)
//             }

//             return getUniqueName(original, `${nameStack[nameStack.length - 1]}::${name}`)
//         }

//         function getRelativeName(name: string) {
//             if (!namePrefix) {
//                 return name
//             }

//             return name.slice(namePrefix.length + 2) // 2 is from `::`
//         }

//         const visited = new Map<ts.Node, ts.Node>()
//         function visit(node: ts.Node): ts.Node {
//             const key = ts.getOriginalNode(node)
//             if (visited.has(key)) {
//                 return visited.get(key)!
//             }

//             const result = transform()
//             visited.set(key, result)

//             return result

//             function transform() {
//                 if (ts.isClassLike(node)) {
//                     return visitClassDeclaration(node)
//                 }
    
//                 if (ts.isHeritageClause(node)) {
//                     return visitHeritageClause(node)
//                 }
    
//                 if (ts.isFunctionDeclaration(node)) {
//                     return visitFunctionDeclaration(node)
//                 }

//                 if (ts.isMethodDeclaration(node)) {
//                     return visitMethodDeclaration(node)
//                 }
    
//                 if (ts.isArrowFunction(node) || ts.isFunctionExpression(node)) {
//                     return visitArrowFunctionOrExpression(node)
//                 }
    
//                 if (ts.isBlock(node)) {
//                     return visitBlock(node)
//                 }
    
//                 if (ts.isSourceFile(node)) {
//                     return visitSourceFile(node)
//                 }
        
//                 return ts.visitEachChild(node, visit, context)
//             }
//         }

//         // Tracks statements meant to be added to a static class block
//         const staticStack: ts.Statement[][] = []
//         function visitClassDeclaration(node: ts.ClassDeclaration | ts.ClassExpression) {
//             if (compiler.isDeclared(node)) {
//                 return node
//             }

//             const r = extractClassDeclaration(node)

//             staticStack.push([])

//             const name = getName(node)
//             nameStack.push(name)
//             const visitedClass = ts.visitEachChild(node, visit, context)
//             nameStack.pop()

//             const staticStatements = staticStack.pop()!

//             return addSerializerSymbolToClass(
//                 visitedClass,
//                 (staticFields) => staticFields.length 
//                     ? createClassSerializationData(
//                         getRelativeName(name),
//                         renderCapturedSymbols(r.captured, r.assets),
//                         factory,
//                         moduleType,
//                         staticFields,
//                     )
//                     : createSerializationData(
//                         getRelativeName(name),
//                         renderCapturedSymbols(r.captured, r.assets),
//                         factory,
//                         moduleType,
//                     ),
//                 r.clauseReplacement,
//                 staticStatements,
//                 context,
//             )
//         }

//         function visitMethodDeclaration(node: ts.MethodDeclaration) {
//             // TODO: methods in object literals
//             if (staticStack.length === 0 || !ts.isClassLike(ts.getOriginalNode(node).parent) || ts.isPrivateIdentifier(node.name)) {
//                 return ts.visitEachChild(node, visit, context)
//             }

//             const sym = compiler.getSymbol(node)
//             if (!sym?.parentScope) {
//                 return ts.visitEachChild(node, visit, context)
//             }

//             // We can't serialize methods with private fields directly without rewriting them
//             // So we need to bail on serializing methods that reference private fields
//             for (const d of sym.parentScope.dependencies) {
//                 const [r, s] = getRootAndSuccessorSymbol(d)
//                 if (r.parentScope?.symbol !== sym) {
//                     if (r.name === 'super') {
//                         return ts.visitEachChild(node, visit, context)
//                     }
//                     continue
//                 }

//                 if (s?.name[0] === '#') {
//                     return ts.visitEachChild(node, visit, context)
//                 }
//             }

//             const name = getName(node)
//             const r = compiler.compileNode(name, node, factory, runtimeTransformer, createInfraTransformer(name, innerTransformer), undefined, undefined, depth)
//             addClassMethodSerialization(node, getRelativeName(name), renderCapturedSymbols(r.captured, r.assets))

//             nameStack.push(name)
//             const res = ts.visitEachChild(node, visit, context)
//             nameStack.pop()

//             return res
//         }

//         function visitArrowFunctionOrExpression(node: ts.ArrowFunction | ts.FunctionExpression) {
//             if (node.parent.kind === ts.SyntaxKind.ParenthesizedExpression && node.parent.parent.kind === ts.SyntaxKind.CallExpression && (node.parent.parent as ts.CallExpression).expression === node.parent) {
//                 return ts.visitEachChild(node, visit, context)
//             }
//             // if (!compiler.canSerialize(node)) {
//             //     return node
//             // }

//             const name = getName(node)
//             const r = compiler.compileNode(name, node, factory, runtimeTransformer, createInfraTransformer(name, innerTransformer), undefined, undefined, depth)

//             nameStack.push(name)
//             const visitedFn = ts.visitEachChild(node, visit, context)
//             nameStack.pop()

//             return addModuleSymbolToFunctionExpression(
//                 visitedFn,
//                 createSerializationData(
//                     getRelativeName(name),
//                     renderCapturedSymbols(r.captured, r.assets),
//                     factory,
//                     moduleType,
//                 ),
//                 factory,
//             )
//         }

//         function visitFunctionDeclaration(node: ts.FunctionDeclaration) {
//             // Overload
//             if (!node.body) {
//                 return node
//             }

//             const name = getName(node)
//             const r = compiler.compileNode(name, node, factory, runtimeTransformer, createInfraTransformer(name, innerTransformer), undefined, undefined, depth)
//             hoistSerializationData(node, getRelativeName(name), renderCapturedSymbols(r.captured, r.assets))

//             nameStack.push(name)
//             const res = ts.visitEachChild(node, visit, context)
//             nameStack.pop()

//             return res
//         }

//         function visitBlock(node: ts.Block) {
//             node = ts.visitEachChild(node, visit, context)

//             return factory.updateBlock(
//                 node,
//                 Array.from(updateStatements(node.statements, updates)),
//             )
//         }

//         const mappedClauses = new Map<ts.HeritageClause, { ident: ts.Identifier, res: ts.HeritageClause }>()
//         function visitHeritageClause(node: ts.HeritageClause) {
//             if (mappedClauses.has(node)) {
//                 return mappedClauses.get(node)!.res
//             }

//             if (node.token !== ts.SyntaxKind.ExtendsKeyword || !isCallExpression(getInnerExp(node.types[0]))) {
//                 return ts.visitEachChild(node, visit, context)
//             }

//             const name = getName(ts.factory.createSuper()).replaceAll('::', '_')
//             const ident = ts.factory.createIdentifier(name)
//             const updatedExp = visit(node.types[0].expression)
//             if (!ts.isExpression(updatedExp)) {
//                 failOnNode('Not an expression', updatedExp)
//             }

//             const statement = ts.findAncestor(node, ts.isStatement)
//             if (!statement) {
//                 failOnNode('Node is not apart of a statement', node)
//             }

//             const decl = createVariableStatement(ident, updatedExp)
//             addStatementUpdate(statement, { before: [decl] })

//             const res = factory.updateHeritageClause(
//                 node,
//                 [factory.updateExpressionWithTypeArguments(node.types[0], ident, undefined)]
//             )

//             mappedClauses.set(node, { ident, res })

//             return res
//         }

//         function visitSourceFile(node: ts.SourceFile) {
//             node = ts.visitEachChild(node, visit, context)
//             const statements = Array.from(updateStatements(node.statements, updates))

//             return factory.updateSourceFile(
//                 node,
//                 statements,
//                 node.isDeclarationFile,
//                 node.referencedFiles,
//                 node.typeReferenceDirectives,
//                 node.hasNoDefaultLib,
//                 node.libReferenceDirectives
//             )
//         }

//         return { visit }
//     }

//     function transform(node: ts.Node) {
//         const result = ts.transform(node, [c => createTransformer(c).visit])

//         return printNodes(result.transformed)
//     }

//     return { createTransformer, transform }
// }

// function getInnerExp(node: ts.Expression): ts.Expression {
//     if (ts.isParenthesizedExpression(node) || ts.isAsExpression(node) || ts.isExpressionWithTypeArguments(node)) {
//         return getInnerExp(node.expression)
//     }

//     return node
// }

// function getMappedPrivateName(
//     node: ts.ClassDeclaration | ts.ClassExpression,
//     memberName: ts.PrivateIdentifier,
//     baseName = node.name ? `__${node.name.text}` : `__` // TODO: check super classes for private members
// ) {
//     return `${baseName}${memberName.text.replace(/^#/, '_')}`
// }

// // Private methods will not work with `Reflect.construct`
// function transformPrivateMembers(
//     node: ts.ClassDeclaration | ts.ClassExpression,
//     context: ts.TransformationContext
// ) {
//     const methods = node.members.filter(isPrivateMethod)
//     const fields = node.members.filter(isPrivateField)

//     const mapped = new Map<string, string>()
//     for (const m of [...methods, ...fields]) {
//         mapped.set(m.name.text, getMappedPrivateName(node, m.name))
//     }

//     function visit(node: ts.Node): ts.Node {
//         if (ts.isPrivateIdentifier(node)) {
//             const newName = mapped.get(node.text)
//             if (newName) {
//                 return context.factory.createIdentifier(newName)
//             }
//         }

//         return ts.visitEachChild(node, visit, context)
//     }


//     return ts.visitEachChild(node, visit, context)
// }

// interface ClassProps {
//     members?: ts.ClassElement[]
//     heritageClauses?: ts.HeritageClause[]
// }

// function updateClass(node: ts.ClassDeclaration | ts.ClassExpression, props: ClassProps, factory = ts.factory) {
//     if (ts.isClassDeclaration(node)) {
//         return factory.updateClassDeclaration(
//             node,
//             node.modifiers,
//             node.name,
//             node.typeParameters,
//             props.heritageClauses ?? node.heritageClauses,
//             props.members ?? node.members,
//         )
//     } else {
//         return factory.updateClassExpression(
//             node,
//             node.modifiers,
//             node.name,
//             node.typeParameters,
//             props.heritageClauses ?? node.heritageClauses,
//             props.members ?? node.members,
//         )
//     }
// }

// function addSerializerSymbolToClass(
//     node: ts.ClassDeclaration | ts.ClassExpression,
//     // TODO: don't pass in a cb for this...
//     getSerializationData: (staticFields: string[]) => ts.Expression,
//     clauseReplacement: [clause: ts.HeritageClause, ident: ts.Identifier] | undefined,
//     staticStatements: ts.Statement[],
//     context: ts.TransformationContext,
// ) {
//     const factory = context.factory
//     const serializeSymbol = createSymbolPropertyName('serialize')
//     const moveableSymbol = createSymbolPropertyName('__moveable__')

//     const _privateFields: (ts.PropertyDeclaration & { name: ts.PrivateIdentifier })[] = []
//     const staticFields: string[] = []
//     for (let i = 0; i < node.members.length; i++) {
//         const m = node.members[i]
//         if (isPrivateField(m)) {
//             _privateFields.push(m)
//         } else if (ts.isPropertyDeclaration(m) && m.modifiers?.some(x => x.kind === ts.SyntaxKind.StaticKeyword) && m.name && ts.isIdentifier(m.name)) {
//             // we'll assume anything marked readonly is... readonly
//             if (m.modifiers?.some(x => x.kind === ts.SyntaxKind.ReadonlyKeyword)) {
//                 continue
//             }
//             staticFields.push(m.name.text)
//         }
//     }

//     const serializationData = getSerializationData(staticFields)

//     const privateFields = Object.fromEntries(_privateFields.map(n => [
//         n.name.text,
//         factory.createPropertyAccessExpression(
//             factory.createThis(),
//             n.name.text
//         )
//     ]))

//     const ident = factory.createIdentifier("privateFields")
//     const init = factory.createBinaryExpression(
//         factory.createPropertyAccessExpression(
//           factory.createIdentifier("desc"),
//           ident
//         ),
//         factory.createToken(ts.SyntaxKind.QuestionQuestionEqualsToken),
//         factory.createArrayLiteralExpression(
//           [],
//           false
//         )
//     )
//     const assignment = factory.createVariableStatement(
//         undefined,
//         factory.createVariableDeclarationList(
//           [factory.createVariableDeclaration(
//             ident,
//             undefined,
//             undefined,
//             init
//             )],
//           ts.NodeFlags.Const
//         )
//     )

//     const push = factory.createCallExpression(
//         factory.createPropertyAccessExpression(
//             ident,
//             'push'
//         ),
//         undefined,
//         [createObjectLiteral(privateFields, factory)]
//     )

//     const description = {
//         privateFields: ident,
//     }

//     // Private members will live on a stack
//     // Top of the stack will have private fields for the base class
//     // Each constructor pops the stack before returning

//     const superClassExp = node.heritageClauses?.find(c => c.token === ts.SyntaxKind.ExtendsKeyword)?.types?.[0]?.expression

//     const resultIdent = factory.createIdentifier('result')
//     const result = factory.createVariableStatement(
//         undefined,
//         factory.createVariableDeclarationList(
//           [factory.createVariableDeclaration(
//             resultIdent,
//             undefined,
//             undefined,
//             factory.createObjectLiteralExpression(
//                 [
//                     ...createObjectLiteral(description, factory).properties,
//                     factory.createSpreadAssignment(
//                         factory.createIdentifier('desc')
//                     )
//                 ],
//                 true
//             )
//             )],
//           ts.NodeFlags.Const
//         )
//     )


//     const serialize = factory.createMethodDeclaration(
//         // [factory.createToken(ts.SyntaxKind.StaticKeyword)],
//         undefined,
//         undefined,
//         factory.createComputedPropertyName(serializeSymbol),
//         undefined,
//         undefined,
//         [factory.createParameterDeclaration(undefined, undefined, 'desc', undefined, undefined, createObjectLiteral({}, factory))],
//         undefined,
//         factory.createBlock([
//             assignment,
//             factory.createExpressionStatement(push),
//             result,
//             superClassExp !== undefined
//                 ? factory.createReturnStatement(
//                     factory.createBinaryExpression(
//                         factory.createCallChain(
//                             factory.createElementAccessExpression(
//                                 factory.createSuper(),
//                                 serializeSymbol
//                             ),
//                             factory.createToken(ts.SyntaxKind.QuestionDotToken),
//                             undefined,
//                             [resultIdent]
//                         ),
//                         factory.createToken(ts.SyntaxKind.QuestionQuestionToken),
//                         resultIdent
//                     )
//                 )
//                 : factory.createReturnStatement(resultIdent)
//         ], true)
//     )

    
//     const move = factory.createMethodDeclaration(
//         [factory.createToken(ts.SyntaxKind.StaticKeyword)],
//         undefined,
//         factory.createComputedPropertyName(moveableSymbol),
//         undefined,
//         undefined,
//         [],
//         undefined,
//         factory.createBlock([
//             factory.createReturnStatement(serializationData)
//         ], true)
//     )

//     const heritageClauses = !clauseReplacement ? undefined : node.heritageClauses?.map(c => {
//         if (ts.getOriginalNode(c) === ts.getOriginalNode(clauseReplacement[0])) {
//             const clauseExp = factory.createExpressionWithTypeArguments(clauseReplacement[1], [])

//             return factory.updateHeritageClause(c, [clauseExp])
//         }

//         return c
//     })

//     const members = [...node.members, serialize, move]
//     if (staticStatements.length > 0) {
//         members.push(
//             factory.createClassStaticBlockDeclaration(factory.createBlock(staticStatements))
//         )
//     }

//     return updateClass(node, { members, heritageClauses }, factory)
// }

// function isPrivateField(node: ts.ClassElement): node is ts.PropertyDeclaration & { name: ts.PrivateIdentifier }  {
//     return !!node.name && ts.isPrivateIdentifier(node.name) && ts.isPropertyDeclaration(node)
// }

// function isPrivateMethod(node: ts.ClassElement): node is ts.MethodDeclaration & { name: ts.PrivateIdentifier } {
//     return !!node.name && ts.isPrivateIdentifier(node.name) && ts.isMethodDeclaration(node)
// }


