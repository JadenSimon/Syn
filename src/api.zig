const std = @import("std");
const js = @import("js");
const parser = @import("./parser.zig");
const Lexer = @import("./lexer.zig").Lexer;
const Program = @import("./program.zig").Program;
const AsyncProgramLoader = @import("./program.zig").AsyncProgramLoader;
const getAllocator = @import("./string_immutable.zig").getAllocator;

inline fn unwrap(comptime T: type, o: *js.Object) !*T {
    return @alignCast(@ptrCast(try o.unwrap()));
}

inline fn getSourceFile(o: *js.Object) !*parser.ParsedFile {
    return unwrap(parser.ParsedFile, o);
}

inline fn getProgram(o: *js.Object) !*Program {
    return unwrap(Program, o);
}

const WrappedFile = js.TypedWrap(*parser.ParsedFile);

pub fn createSourceFile(file_name: js.UTF8String, contents: js.ReferencedBuffer, is_lib: bool) !WrappedFile {
    const c = try getAllocator().allocSentinel(u8, file_name.data.len, 0); // XXX
    @memcpy(c, file_name.data);
    const parsed = try parser.ParsedFile.createFromBuffer(contents.data, c, is_lib, null);

    return .{ .value = parsed };
}

pub fn createSourceFileAsync(file_name: js.UTF8String, contents: ?js.ReferencedBuffer, is_lib: bool) !js.Promise(WrappedFile) {
    const text = contents orelse blk: {
        const data = try std.fs.cwd().readFileAlloc(getAllocator(), file_name.data, std.math.maxInt(u32));
        break :blk js.ReferencedBuffer{ .data = data, .source = undefined, .ref = undefined }; 
    };
    return .{try createSourceFile(file_name, text, is_lib)};
}

// not really worth it
// real projects don't benefit much from trying to pre-fetch
// because we already find most relevant files ahead of time
pub fn createSourceFileAsync2(file_name: js.UTF8String, contents: js.ReferencedBuffer, is_lib: bool, program: js.Wrapped) !js.Promise(WrappedFile) {
    const c = try getAllocator().allocSentinel(u8, file_name.data.len, 0); // XXX
    @memcpy(c, file_name.data);

    const p: *Program = @alignCast(@ptrCast(program.value));

    const Ctx = struct { *Program, []u8 };
    const d = try getAllocator().create(Ctx);
    d.* = .{p, c};

    const import_listener = try getAllocator().create(parser.ImportListener); 
    import_listener.* = .{
        .data = d,
        .cb = struct {
            pub fn f(data: *anyopaque, directives: []const parser.TripleSlashDirective, modules: []const []const u8) !void {
                _ = directives; // todo: these ones
                const ctx: *Ctx = @alignCast(@ptrCast(data));
                const prog: *Program = ctx[0];
                const loader: *AsyncProgramLoader = @alignCast(@ptrCast(prog.loader));
                for (modules) |name| {
                    const resolved = prog.resolver.resolveSpecifier(ctx[1], name) catch {
                        continue;
                    };
                    std.fs.accessAbsolute(resolved, .{}) catch continue;
                    // FIXME: this segfaults because we're not on the VM thread
                    _ = try loader.requestFile(resolved, false);
                }
            }
        }.f,
    };

    const parsed = try parser.ParsedFile.createFromBuffer(contents.data, file_name.data, is_lib, import_listener);

    return .{ .{ .value = parsed } };
}

pub fn getStartRef(o: *js.Object) !u32 {
    const p = try getSourceFile(o);

    return p.ast.start;
}

pub fn getFileName(o: *js.Object) !?js.UTF8String {
    const p = try getSourceFile(o);
    const n = p.source_name orelse return null;

    return .{ .data = @ptrCast(@constCast(n)) };
}

pub fn ptrOffset(this: *js.Receiver, left: u32, right: u32, source: []u8) !u32 {
    _ = this;
    const ptr: usize = (@as(usize, right) << 32) | left;

    return @intCast(ptr - @intFromPtr(source.ptr));
}

pub fn serialize(sf: *js.Object) !js.ArrayBuffer {
    const p = try getSourceFile(sf);
    const nodes = p.ast.nodes;
    var b = try std.ArrayList(u8).initCapacity(getAllocator(), nodes.count * 32);

    std.debug.assert(nodes.pages.items.len > 0);

    for (0..(nodes.pages.items.len - 1)) |i| {
        const z: [*]u8 = @ptrCast(nodes.pages.items[i].ptr);
        b.appendSliceAssumeCapacity(z[0..(nodes.pages.items[i].len * 32)]);
    }

    const last = nodes.pages.items[nodes.pages.items.len - 1];
    const z: [*]u8 = @ptrCast(last.ptr);
    b.appendSliceAssumeCapacity(z[0..(nodes.local_count * 32)]);

    return js.ArrayBuffer.from(b.items);
}

const PrintResult = struct {
    contents: js.ArrayBuffer,
    mappings: ?js.ArrayBuffer,
};

fn _printNode(sf: *const parser.ParsedFile, ref: u32, maybe_nodes: ?[]u8, maybe_heap: ?[]u8, options: ?parser.PrinterOptions) !PrintResult {
    var ast_data = sf.ast;
    ast_data.start = ref;

    if (maybe_nodes) |nodes| {
        const nodes_pointer: [*]parser.AstNode = @alignCast(@ptrCast(nodes.ptr));
        const nodes_slice: []parser.AstNode = nodes_pointer[0..(nodes.len / 32)];

        if (ref >= (sf.ast.nodes.count + nodes_slice.len)) {
            return error.InvalidNodeReference;
        }

        // In-place mutation
        if (maybe_heap) |heap| {
            for (0..nodes_slice.len) |i| {
                var n = &nodes_slice[i];
                switch (n.kind) {
                    .identifier, .string_literal, .private_identifier, .template_head, .template_middle, .template_tail, .no_substitution_template_literal, .regular_expression_literal => {
                        const data = if (n.data) |x| @intFromPtr(x) else 0;
                        if ((data >> 32) == 0xFF) {
                            n.data = @ptrFromInt(@intFromPtr(heap.ptr) + ((data & 0xFFFFFFFF) - ast_data.source.len));
                        }
                    },
                    // TODO: template strings
                    else => {},
                }
            }
        }

        ast_data.nodes = try ast_data.nodes.cloneWithItems(nodes_slice);
    } else {
        if (ref >= sf.ast.nodes.count) {
            return error.InvalidNodeReference;
        }
    }

    const result = try parser.printWithOptions(ast_data, options orelse .{});

    return .{
        .contents = try js.ArrayBuffer.from(@constCast(result.contents)),
        .mappings = if (result.source_map) |m| try js.ArrayBuffer.from(@constCast(m)) else null,
    };
}

pub fn printNode(sf: WrappedFile, ref: u32, maybe_nodes: ?[]u8, maybe_heap: ?[]u8, options: ?parser.PrinterOptions) !PrintResult {
    return _printNode(sf.value, ref, maybe_nodes, maybe_heap, options);
}

pub fn printNodeAsync(sf: WrappedFile, ref: u32, maybe_nodes: ?js.ReferencedBuffer, maybe_heap: ?js.ReferencedBuffer, options: ?parser.PrinterOptions) !js.Promise(PrintResult) {
    const nodes = if (maybe_nodes) |x| @constCast(x.data) else null;
    const heap = if (maybe_heap) |x| @constCast(x.data) else null;
    const result = try _printNode(sf.value, ref, nodes, heap, options);

    return .{result};
}

// TODO: make this work on synthetic/transformed files
pub fn printDeclarationFile(program: *js.Object, sf: *js.Object, options: ?parser.PrinterOptions) !PrintResult {
    _ = options;
    const p = try getProgram(program);
    const source = try getSourceFile(sf);

    const id = try p.getFileIdByPath(source.source_name orelse return error.MissingSourceFileName);
    const text = try p.printDeclarationText(id, source.ast.start);

    return .{
        .contents = try js.ArrayBuffer.from(@constCast(text)),
        .mappings = try js.ArrayBuffer.from(@constCast("{\"mappings\":\"\",\"sources\":[]}")), // FIXME
    };
}

pub fn printSynFile(program: *js.Object, sf: *js.Object, options: ?parser.PrinterOptions) !PrintResult {
    _ = options;
    const p = try getProgram(program);
    const source = try getSourceFile(sf);

    const id = try p.getFileIdByPath(source.source_name orelse return error.MissingSourceFileName);
    var replacements = try p.transformSyn(id, source.ast.start);
    defer replacements.deinit();

    const result = try parser.printWithOptions(p.getFileData(id).ast, .{ .replacements = &replacements, .transform_to_cjs = true });

    return .{
        .contents = try js.ArrayBuffer.from(@constCast(result.contents)),
        .mappings = if (result.source_map) |m| try js.ArrayBuffer.from(@constCast(m)) else null,
    };
}

pub fn printSyntheticNode(ref: u32, nodes: []u8, heap: []u8, options: ?parser.PrinterOptions) !PrintResult {
    const file_name: []const u8 = (if (options) |o| o.file_name else null) orelse &.{};
    const ast_data = parser.AstData{
        .decorators = std.AutoArrayHashMapUnmanaged(u32, u32){},
        .nodes = parser.BumpAllocator(parser.AstNode).init(getAllocator(), std.heap.page_allocator),
        .source = &.{},
        .source_name = file_name,
    };

    const sf = parser.ParsedFile{
        .ast = ast_data,
        .binder = undefined,
        .source = &.{},
        .source_name = file_name,
    };

    return _printNode(&sf, ref, nodes, heap, options);
}

// TODO: reference the containing object rather than the individual functions?
pub const CompilerHostFns = struct {
    requestSourceFile: js.Reference(js.Function), // async
    resolveFilePath: js.Reference(js.Function), // sync

    pub fn callRequestSourceFile(
        this: *const @This(),
        comptime Recv: type,
        recv: *Recv,
        cb: fn (*Recv, *parser.ParsedFile) anyerror!void,
        file_name: []const u8,
        is_lib: bool,
    ) !void {
        const f = struct {
            pub fn f(data: ?*anyopaque, o: *js.Object) !void {
                return cb(
                    @alignCast(@ptrCast(data orelse return error.MissingReceiver)),
                    try unwrap(parser.ParsedFile, o),
                );
            }
        }.f;

        const zig_cb = try js.Function.initWithData("", f, recv);

        const requestSourceFile = try this.requestSourceFile.getValue();
        const promise: *js.PromiseValue = @ptrCast(try requestSourceFile.call(.{ file_name, is_lib }));

        // The promises are kept alive elsewhere
        _ = try promise.then(zig_cb);
    }

    const ResolveMode = enum(u32) { lib, types, path, module };

    pub fn callResolveFilePath(
        this: *const @This(),
        mode: ResolveMode,
        value: []const u8,
        origin: ?[]const u8,
    ) ![]const u8 {
        const mode_val: u32 = @intFromEnum(mode);
        //std.debug.print("file \"{s}\" not found from {s}", .{value, origin orelse "<unknown>"});

        const resolveFilePath = try this.resolveFilePath.getValue();
        const result = try resolveFilePath.callWithReturnType(?js.UTF8String, .{ mode_val, value, origin }) 
            orelse return error.FileNotFound;

        return result.data;
    }
};

pub fn createCompilerHost(fns: CompilerHostFns) !js.Wrapped {
    const p = try getAllocator().create(CompilerHostFns);
    p.* = fns;

    return .{ .value = p };
}

pub fn getSourceFileByPath(program: *js.Object, file_name: js.UTF8String) !?*js.Value {
    const p = try unwrap(Program, program);
    const parsed = try p.getFileByPath(file_name.data) orelse return null;
    const handle: *js.Ref = @ptrCast(parsed.api_handle orelse return error.MissingHandle);

    return try handle.getValue(js.getCurrentEnv());
}

pub fn getAllSourceFiles(program: *js.Object) !js.Array(*js.Value) {
    const p = try unwrap(Program, program);
    var elements = try getAllocator().alloc(*js.Value, p.parsed_files.count()); // FIXME: leaks

    var i: u32 = 0;
    var iter = p.parsed_files.iterator();
    while (iter.next()) |entry| {
        const handle: *js.Ref = @ptrCast(entry.value_ptr[0].api_handle orelse return error.MissingHandle);
        elements[i] = try handle.getValue(js.getCurrentEnv());
        i += 1;
    }

    return .{ .elements = elements };
}

const ProgramOptions = struct {
    lib: ?js.Array(js.UTF8String) = null,
    lib_dir: js.UTF8String = .{ .data = @constCast(&.{}) },
    types: ?js.Array(js.UTF8String) = null,
};

pub fn createProgram(file_names: js.Array(js.UTF8String), host: *js.Object, cb: js.Reference(js.Function), options: ProgramOptions) !js.Wrapped {
    var names = try getAllocator().alloc([]u8, file_names.elements.len);
    for (file_names.elements, 0..) |n, i| {
        names[i] = n.data;
    }

    const program = try getAllocator().create(Program);
    program.* = try Program.init(getAllocator(), names, options.lib_dir.data);
    if (options.lib) |lib| {
        var default_libs = try getAllocator().alloc([]const u8, lib.elements.len);
        for (lib.elements, 0..) |el, i| {
            default_libs[i] = el.data;
        }
        program.options.default_libs = default_libs;
    }

    if (options.types) |arr| {
        var types = try getAllocator().alloc([]const u8, arr.elements.len);
        for (arr.elements, 0..) |el, i| {
            types[i] = el.data;
        }
        program.options.types = types;
    }

    if (file_names.elements.len == 0) {
        const cb2 = try cb.getValue();
        _ = try cb2.call(.{});

        return .{ .value = program };
    }

    var loader = try getAllocator().create(AsyncProgramLoader);
    loader.* = try AsyncProgramLoader.init(program, try unwrap(CompilerHostFns, host));
    program.loader = loader;

    const f = struct {
        pub fn f(ctx: ?*anyopaque) !void {
            const p: *js.Reference(js.Function) = @alignCast(@ptrCast(ctx orelse return error.MissingCallbackData));
            const cb2 = try p.getValue();
            _ = try cb2.call(.{});
        }
    }.f;

    const r = try getAllocator().create(js.Reference(js.Function));
    r.* = cb;

    try loader.loadFiles(r, f);

    return .{ .value = program };
}

pub fn waitForPromise(val: *js.Value) !*js.Value {
    return js.waitForPromise(val);
}

pub fn parseJson5(text: js.UTF8String) !*js.Value {
    return @import("./json_parser.zig").parseJson5ToJs(text.data, getAllocator());
}

pub fn getLineMap(sf: WrappedFile) !?js.ArrayBuffer {
    const lines = sf.value.ast.lines orelse return null;
    const decoded = try @import("./lexer.zig").LineMap.Decoder.decode(
        lines.positions,
        lines.count,
    );

    return try js.ArrayBuffer.from(@as([*]u8, @constCast(@ptrCast(decoded.ptr)))[0 .. decoded.len * 4]);
}

pub fn getStart(sf: WrappedFile, index: u32) u32 {
    const extra = sf.value.ast.positions orelse return 0;

    return extra.positions.at(index).full_start + getLeadingTriviaWidth(sf, index);
}

pub fn getEnd(sf: WrappedFile, index: u32) u32 {
    const extra = sf.value.ast.positions orelse return 0;

    return extra.positions.at(index).full_start + extra.positions.at(index).width;
}

pub fn getFullStart(sf: WrappedFile, index: u32) u32 {
    const extra = sf.value.ast.positions orelse return 0;

    return extra.positions.at(index).full_start;
}

pub fn getFullWidth(sf: WrappedFile, index: u32) u32 {
    const extra = sf.value.ast.positions orelse return 0;

    return extra.positions.at(index).width;
}

pub fn getLeadingTriviaWidth(sf: WrappedFile, index: u32) u32 {
    const extra = sf.value.ast.positions orelse return 0;
    const p = extra.positions.at(index);
    if (p.width == 0) return 0;

    var scanner = TriviaScanner{ .source = sf.value.source[p.full_start .. p.full_start + p.width] };

    return scanner.scan(); // We're already offset by `p.full_start`
}

const TriviaScanner = struct {
    source: []const u8,
    start: u32 = 0,
    current: u32 = 0,
    end: u32 = 0,
    pause_on_comments: bool = false,
    token: parser.SyntaxKind = .unknown,

    inline fn nextCodePoint(it: *@This()) i32 {
        const strings = @import("./string_immutable.zig");

        const cp_len = strings.wtf8ByteSequenceLengthWithInvalid(it.source.ptr[it.current]);
        const slice = if (!(cp_len + it.current > it.source.len))
            it.source[it.current .. cp_len + it.current]
        else
            "";

        const code_point = switch (slice.len) {
            0 => -1,
            1 => @as(i32, slice[0]),
            else => strings.decodeWTF8RuneTMultibyte(slice.ptr[0..4], @as(u3, @intCast(slice.len)), i32, strings.unicode_replacement),
        };

        it.end = it.current;

        it.current += if (code_point != strings.unicode_replacement)
            cp_len
        else
            1;

        return code_point;
    }

    // FIXME: doesn't handle non-ascii trivia
    pub fn scan(this: *@This()) u32 {
        this.start = this.end;

        while (true) {
            switch (this.nextCodePoint()) {
                '\r', '\n', '\t', 11, 12, 32 => continue,
                '/' => {
                    const next = this.nextCodePoint();
                    if (next == '/') {
                        comment: while (true) {
                            switch (this.nextCodePoint()) {
                                -1, '\n' => break :comment,
                                else => continue,
                            }
                        }

                        if (this.pause_on_comments) {
                            this.token = .single_line_comment_trivia;
                            break;
                        }
                    } else if (next == '*') {
                        comment: while (true) {
                            switch (this.nextCodePoint()) {
                                '*' => {
                                    if (this.nextCodePoint() == '/') {
                                        break :comment;
                                    }
                                },
                                -1 => break :comment,
                                else => continue,
                            }
                        }

                        if (this.pause_on_comments) {
                            this.token = .multi_line_comment_trivia;
                            break;
                        }
                    }
                },
                else => {
                    this.token = .unknown;
                    break;
                },
            }
        }

        return this.end;
    }
};

pub fn getFormattedDiagnostics(program: *js.Object, sf: WrappedFile) !js.UTF8String {
    const p = try unwrap(Program, program);
    const source_name = sf.value.source_name  orelse return error.MissingSourceFileName;
    const ref = try p.getFileIdByPath(source_name);

    const n = try p.files.items[ref].diagnosticsToBuf(getAllocator());

    return .{ .data = @ptrCast(@constCast(n)) };
}

const Reifier = @import("./reifier.zig").Reifier;

pub fn createReifier(program: *js.Object, type_ns: *js.Object) !js.Wrapped {
    const p = try unwrap(Program, program);
    const reifier = try getAllocator().create(Reifier);
    reifier.* = Reifier.initCurrentEnv(getAllocator(), try p.getAnalyzer(), type_ns);

    return .{ .value = reifier };
}

pub fn getReifiedType(reifier: *js.Object, sf: WrappedFile, node_ref: u32, type_params: u32) !*js.Value {
    const r = try unwrap(Reifier, reifier);
    const source_name = sf.value.source_name;
    const ref = try r.analyzer.program.getFileIdByPath(source_name orelse return error.MissingSourceFileName);
    const data = r.analyzer.program.getFileData(ref);
    try r.analyzer.program.bindModule(data);
    const result = try r.processReifyExpression(data, node_ref, type_params);

    return @ptrCast(result);
}

pub fn callTypeFunction(reifier: *js.Object, target: u32, args: js.Array(u32)) !*js.Value {
    const r = try unwrap(Reifier, reifier);
    const result = try r.evaluateTypeFunction(target, args.elements);

    return @ptrCast(result);
}

comptime {
    js.registerModule(@This());
}
