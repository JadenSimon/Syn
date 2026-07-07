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
                    self.nodes.at(clone).data = @ptrFromInt(head);
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

    fn createImporter(this: *@This()) !NodeRef {
        const meta = try this.factory.createPropertyAccessExpression(try this.factory.createIdentifier("import"), "meta");
        return this.factory.createPropertyAccessExpression(meta, "__virtualId");
    }

    fn createWrapExportsCall(this: *@This(), spec: NodeRef, namespace: NodeRef) !NodeRef {
        const args: []const NodeRef = &.{ spec, try this.createImporter(), namespace };
        return this.factory.createCallExpression(try this.factory.createIdentifier("__wrapExports"), args);
    }

    fn createNamespaceImportDeclaration(this: *@This(), namespace: NodeRef, spec: []const u8, attributes: NodeRef) !NodeRef {
        const ns_import = try this.factory.createNamespaceImport(namespace);
        const clause = try this.factory.createImportClause(0, ns_import);
        return this.factory.createImportDeclaration(clause, try this.factory.createStringLiteral(spec), attributes);
    }

    fn wrapDynamicImport(this: *@This(), import_call: NodeRef, spec: NodeRef) !NodeRef {
        const param = try this.factory.createParameter(try this.factory.createIdentifier("namespace"), 0);
        const body = try this.createWrapExportsCall(spec, try this.factory.createIdentifier("namespace"));
        const arrow = try this.factory.createSingleParamArrowFunction(param, body, 0);
        const then = try this.factory.createPropertyAccessExpression(import_call, "then");
        const args: []const NodeRef = &.{arrow};
        return this.factory.createCallExpression(then, args);
    }

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
