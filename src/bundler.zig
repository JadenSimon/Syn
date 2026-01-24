const std = @import("std");
const js_parser = @import("./parser.zig");
const js_program = @import("./program.zig");
const AstNode = js_parser.AstNode;
const NodeRef = js_parser.NodeRef;
const NodeIterator = js_parser.NodeIterator;
const NodeList = js_parser.NodeList;
const NodeFlags = js_parser.NodeFlags;
const SyntaxKind = js_parser.SyntaxKind;

const getSlice = js_parser.getSlice;
const unwrapRef = js_parser.unwrapRef;
const maybeUnwrapRef = js_parser.maybeUnwrapRef;
const toBinaryDataPtrRefs = js_parser.toBinaryDataPtrRefs;
const getPackedData = js_parser.getPackedData;
const getHashFromNode = js_parser.getHashFromNode;


const BundleOptions = struct {
    external: []const []const u8 = &.{},

};

const ExternalImportMap = std.AutoArrayHashMapUnmanaged(u64, u32);
const ModuleStack = std.ArrayListUnmanaged(u32);
const Visited = std.AutoArrayHashMapUnmanaged(u32, void);
const ExternalSet = std.AutoArrayHashMapUnmanaged(u64, void);


pub const Bundler = struct {
    options: BundleOptions = .{},
    program: *js_program.Program,
    external_imports: ExternalImportMap = ExternalImportMap{},
    module_stack: ModuleStack = ModuleStack{},
    visited: Visited = Visited{},
    writer: js_parser.Writer,
    external_set: ExternalSet = ExternalSet{},

    pub fn init(program: *js_program.Program) !@This() {
        var estimated_size: usize = 0;
        for (program.files.items) |x| {
            estimated_size += x.ast.source.len;
        }

        const writer = try js_parser.Writer.init(estimated_size);

        return .{
            .program = program,
            .writer = writer,
        };
    }

    pub fn bundle(this: *@This(), file_name: []const u8) !void {
        const id = try this.program.getFileIdByPath(file_name);
        try this.visit(id);
        std.debug.print("{s}\n", .{this.writer.buf.items});
    }

    fn visit(this: *@This(), file_id: u32) anyerror!void {
        const d = this.program.getFileData(file_id);
        if (std.mem.endsWith(u8, d.file_name.?, ".d.ts")) {
            return;
        }

        const entry = try this.visited.getOrPut(std.heap.c_allocator, file_id);
        if (entry.found_existing) {
            return;
        }

        const x2 = try std.fmt.allocPrint(std.heap.c_allocator, "__exports_{d}", .{file_id});

        this.writer.write("const ");
        this.writer.write(x2);
        this.writer.write(" = {};\n");

        // importing a module already on the stack means we may need to defer usage

        try this.module_stack.append(std.heap.c_allocator, file_id);
        defer _ = this.module_stack.pop();

        const f = try this.program.getBoundFile(file_id);

        var iter = f.import_map.iterator();
        while (iter.next()) |x| {
            if (this.external_set.get(x.key_ptr.*) != null) continue;

            try this.visit(x.value_ptr.*);
        }

        try printWrappedModule(this, f);
        this.writer.write("\n");
    }
};

fn printWrappedModule(b: *Bundler, f: *js_program.ParsedFileData) !void {
    var data = try f.ast.clone();

    var replacements = std.AutoArrayHashMap(NodeRef, NodeRef).init(std.heap.c_allocator);
    defer replacements.deinit();

    var statements = js_parser.NodeList.init(&data.nodes);
    var late_exports = std.ArrayList(NodeRef).init(std.heap.c_allocator);
    defer late_exports.deinit();

    const copyStr = struct {
        pub fn _f(comptime s: []const u8) ![]const u8 {
            const ptr = try std.heap.c_allocator.alloc(u8, s.len);
            @memcpy(ptr, s);
            return ptr;
        }
    }._f;

    const default_ident = try data.nodes.push(.{
        .kind = .default_keyword,
    });

    const x2 = try std.fmt.allocPrint(std.heap.c_allocator, "__exports_{d}", .{f.id});

    const exports_ident = try data.nodes.push(.{
        .kind = .identifier,
        .data = x2.ptr,
        .len = @intCast(x2.len),
    });

    const x4 = try copyStr("__esModule");

    const esmodule_ident = try data.nodes.push(.{
        .kind = .identifier,
        .data = x4.ptr,
        .len = @intCast(x4.len),
    });

    const createAssignmentExpression = struct {
        pub fn _f(_exports_ident: NodeRef, nodes: *js_parser.BumpAllocator(AstNode), ident: NodeRef, init: NodeRef) !NodeRef {
            const lhs = try nodes.push(.{
                .kind = .property_access_expression,
                .data = toBinaryDataPtrRefs(_exports_ident, ident),
            });
            return try nodes.push(.{
                .kind = .binary_expression,
                .data = toBinaryDataPtrRefs(lhs, init),
                .len = @intFromEnum(SyntaxKind.equals_token),
            });
        }
    }._f;

    const first_statement = maybeUnwrapRef(data.nodes.at(data.start)) orelse 0;
    var iter = NodeIterator.init(&data.nodes, first_statement);
    while (iter.nextPair()) |pair| {
        switch (pair[0].kind) {
            .import_declaration => {
                const d = getPackedData(pair[0]);
                const spec = d.right;
                if (d.left == 0 or pair[0].hasFlag(.declare)) {
                    // Side-effect import
                    const r = try data.nodes.push(.{
                        .kind = .empty_statement,
                    });
                    try replacements.put(pair[1], r);
                    continue;
                }

                const spec2 = getSlice(data.nodes.at(spec), u8);
                const key = std.hash.Wyhash.hash(0, spec2);
                const imported_file_id = f.import_map.get(key) orelse blk: {
                    const synthetic_id = b.external_imports.get(key) orelse blk2: {
                        const _id: u32 = @intCast(b.external_imports.count() + b.program.files.items.len);
                        try b.external_imports.put(std.heap.c_allocator, key, _id);
                        const exports_name2 = try std.fmt.allocPrint(std.heap.c_allocator, "__exports_{d}", .{_id});
                        const exports2_ident = try data.nodes.push(.{
                            .kind = .identifier,
                            .data = exports_name2.ptr,
                            .len = @intCast(exports_name2.len),
                        });

                        const clause = try data.nodes.push(.{
                            .kind = .namespace_import,
                            .data = @ptrFromInt(exports2_ident),
                        });

                        try statements.append(.{
                            .kind = .import_declaration,
                            .data = toBinaryDataPtrRefs(clause, spec),
                        });

                        break :blk2 _id;
                    };

                    break :blk synthetic_id;
                };

                const exports_name = try std.fmt.allocPrint(std.heap.c_allocator, "__exports_{d}", .{imported_file_id});

                const r = try data.nodes.push(.{
                    .kind = .identifier,
                    .data = exports_name.ptr,
                    .len = @intCast(exports_name.len),
                });

                const clause_node = data.nodes.at(d.left);
                const d2 = getPackedData(clause_node);
                var default_import: ?*AstNode = null;
                if (d2.left != 0) {
                    const check = try data.nodes.push(.{
                        .kind = .property_access_expression,
                        .data = toBinaryDataPtrRefs(r, esmodule_ident),
                    });

                    const default = try data.nodes.push(.{
                        .kind = .property_access_expression,
                        .data = toBinaryDataPtrRefs(r, default_ident),
                    });

                    const cond = try data.nodes.push(.{
                        .kind = .conditional_expression,
                        .data = toBinaryDataPtrRefs(check, default),
                        .len = r,
                    });
                    const decl = try data.nodes.push(.{
                        .kind = .variable_declaration,
                        .data = toBinaryDataPtrRefs(d2.left, cond),
                    });
                    const variable_statement = try data.nodes.push(.{
                        .kind = .variable_statement,
                        .flags = @intFromEnum(NodeFlags.@"const"),
                        .data = @ptrFromInt(decl),
                        .next = pair[0].next,
                    });
                    try replacements.put(pair[1], variable_statement);
                    default_import = data.nodes.at(variable_statement);
                }

                if (d2.right == 0) {
                    continue;
                }

                const bindings = data.nodes.at(d2.right);

                if (bindings.kind == .namespace_import) {
                    const decl = try data.nodes.push(.{
                        .kind = .variable_declaration,
                        .data = toBinaryDataPtrRefs(unwrapRef(bindings), r),
                    });
                    const variable_statement = try data.nodes.push(.{
                        .kind = .variable_statement,
                        .flags = @intFromEnum(NodeFlags.@"const"),
                        .data = @ptrFromInt(decl),
                        .next = pair[0].next,
                    });
                    if (default_import) |n| {
                        n.next = variable_statement;
                    } else {
                        try replacements.put(pair[1], variable_statement);
                    }
                } else {
                    var synthed = NodeList.init(&data.nodes);
                    var binding_iter = NodeIterator.init(&data.nodes, maybeUnwrapRef(bindings) orelse 0);
                    while (binding_iter.next()) |binding| {
                        const alias = getPackedData(binding).right;
                        if (alias != 0) {
                            try synthed.append(.{
                                .kind = .binding_element,
                                .data = toBinaryDataPtrRefs(alias, 0),
                                .len = getPackedData(binding).left,
                            });
                        } else {
                            try synthed.append(.{
                                .kind = .binding_element,
                                .data = toBinaryDataPtrRefs(getPackedData(binding).left, 0),
                            });
                        }
                    }

                    const pattern = try data.nodes.push(.{
                        .kind = .object_binding_pattern,
                        .data = @ptrFromInt(synthed.head),
                    });

                    const decl = try data.nodes.push(.{
                        .kind = .variable_declaration,
                        .data = toBinaryDataPtrRefs(pattern, r),
                    });
                    const variable_statement = try data.nodes.push(.{
                        .kind = .variable_statement,
                        .flags = @intFromEnum(NodeFlags.@"const"),
                        .data = @ptrFromInt(decl),
                        .next = pair[0].next,
                    });
                    if (default_import) |n| {
                        n.next = variable_statement;
                    } else {
                        try replacements.put(pair[1], variable_statement);
                    }
                }
            },
            else => {
                if (pair[0].hasFlag(.declare) or !pair[0].hasFlag(.@"export")) continue;

                // TODO: default export
                const without_export = try data.nodes.push(pair[0].*);
                data.nodes.at(without_export).flags &= ~@as(u20, @intFromEnum(NodeFlags.@"export"));

                try replacements.put(pair[1], without_export);

                if (pair[0].kind == .function_declaration) {
                    const copy = try data.nodes.push(data.nodes.at(getPackedData(pair[0]).left).*);
                    data.nodes.at(copy).location = 0;

                    const exp = try createAssignmentExpression(exports_ident, &data.nodes, copy, copy);
                    try statements.append(.{
                        .kind = .expression_statement,
                        .data = @ptrFromInt(exp),
                    });
                } else if (pair[0].kind == .class_declaration) {
                    const copy = try data.nodes.push(data.nodes.at(getPackedData(pair[0]).left).*);
                    data.nodes.at(copy).location = 0;
                    try late_exports.append(copy);

                    const exp = try createAssignmentExpression(exports_ident, &data.nodes, copy, copy);
                    const exp_statement = try data.nodes.push(.{
                        .kind = .expression_statement,
                        .data = @ptrFromInt(exp),
                        .next = pair[0].next,
                    });

                    data.nodes.at(without_export).next = exp_statement;
                } else if (pair[0].kind == .variable_statement) {
                    var tail = without_export;
                    var decls_iter = NodeIterator.init(&data.nodes, unwrapRef(pair[0]));
                    while (decls_iter.next()) |decl| {
                        const binding_ref = getPackedData(decl).left;
                        const binding = data.nodes.at(binding_ref);
                        if (binding.kind != .identifier) {
                            return error.TODO_exported_destructuring_pattern;
                        }

                        const copy = try data.nodes.push(binding.*);
                        data.nodes.at(copy).location = 0;
                        try late_exports.append(copy);

                        const exp = try createAssignmentExpression(exports_ident, &data.nodes, copy, copy);
                        const exp_statement = try data.nodes.push(.{
                            .kind = .expression_statement,
                            .data = @ptrFromInt(exp),
                            .next = pair[0].next,
                        });

                        data.nodes.at(tail).next = exp_statement;
                        tail = exp_statement;
                    }
                }
            },
        }
    }

    if (late_exports.items.len != 0) {
        const zero = try data.nodes.push(.{
            .kind = .numeric_literal,
        });
        var right: NodeRef = try data.nodes.push(.{
            .kind = .void_expression,
            .data = @ptrFromInt(zero),
        });

        for (late_exports.items) |n| {
            const exp = try createAssignmentExpression(exports_ident, &data.nodes, n, right);
            right = exp;
        }

        try statements.append(.{
            .kind = .expression_statement,
            .data = @ptrFromInt(right),
        });
    }

    if (statements.head != 0) {
        statements.appendRef(first_statement);
    } else {
        statements.head = first_statement;
    }

    const block = try data.nodes.push(.{
        .kind = .block,
        .data = if (statements.head == 0) null else @ptrFromInt(statements.head),
    });

    const arrow_fn = try data.nodes.push(.{
        .kind = .arrow_function,
        .data = toBinaryDataPtrRefs(0, block),
    });

    const arrow_fn2 = try data.nodes.push(.{
        .kind = .parenthesized_expression,
        .data = @ptrFromInt(arrow_fn),
    });

    const call_exp = try data.nodes.push(.{
        .kind = .call_expression,
        .data = toBinaryDataPtrRefs(arrow_fn2, 0),
    });

    const call_exp_statement = try data.nodes.push(.{
        .kind = .expression_statement,
        .data = @ptrFromInt(call_exp),
    });

    var printer = js_parser.Printer(js_parser.Writer, .{ .print_source_map = true, .use_replacements = true }).init(data, &b.writer);
    printer.skip_types = true;
    printer.replacements = &replacements;

    const sf = AstNode{
        .kind = .source_file,
        .data = @ptrFromInt(call_exp_statement),
    };

    try printer.visit(&sf);
}
