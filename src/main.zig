const std = @import("std");
const js_lexer = @import("./lexer.zig");
const js_parser = @import("./parser.zig");
const program = @import("./program.zig");

const eqlComptime = @import("./string_immutable.zig").eqlComptime;

const json_parser = @import("./json_parser.zig");

fn getSource(args: *std.process.ArgIterator) !js_lexer.Source {
    const text = args.next() orelse return error.MissingArg;

    if (eqlComptime(text, "--file")) {
        const fileName = args.next() orelse return error.MissingArg;
        const resolved = try std.fs.cwd().realpathAlloc(std.heap.c_allocator, fileName);
        // const buf = try std.fs.cwd().readFileAlloc(std.heap.c_allocator, resolved, std.math.maxInt(usize));

        return js_lexer.Source{ .contents = &.{}, .name = resolved };
    }

    return js_lexer.Source{ .contents = text };
}

fn printCounts(r: *const js_parser.BumpAllocator(js_parser.AstNode)) !void {
    var m = std.AutoArrayHashMap(js_parser.SyntaxKind, u32).init(std.heap.c_allocator);
    defer m.deinit();

    for (0..r.count) |i| {
        const n = r.at(i);
        const entry = try m.getOrPut(n.kind);
        if (!entry.found_existing) {
            entry.value_ptr.* = 1;
        } else {
            entry.value_ptr.* += 1;
        }
    }

    const C = struct {
        values: []u32,
        pub fn lessThan(ctx: @This(), a_index: usize, b_index: usize) bool {
            return ctx.values[a_index] > ctx.values[b_index];
        }
    };

    m.sort(C{ .values  = m.values() });

    var iter = m.iterator();
    while (iter.next()) |entry| {
        std.debug.print("{}: {}\n", .{ entry.key_ptr.*, entry.value_ptr.* });
    }
}

pub fn main() !void {
    // const __testJson =
    //     \\["ok",1,true, false]
    // ;

    // var p2 = try json_parser.JsoncParser.init(.{ .contents = __testJson }, std.heap.c_allocator);
    // try p2.parse();

    var args = std.process.args();
    _ = args.next();

    const s = try getSource(&args);

    // const lexer = try js_lexer.Lexer.init(s, std.heap.c_allocator);
    // var parser = js_parser.Parser.init(lexer);
    // // parser.options.allow_jsx = true;

    // const t = std.time.microTimestamp();
    // const r = try parser.parse();
    // std.debug.print("parse time {d:.3}\n", .{std.time.microTimestamp() - t});
    // _ = r;
    // // const t2 = std.time.microTimestamp();
    // //try sequentialCount(&r);
    // std.debug.print("count time {d:.3}\n", .{std.time.microTimestamp() - t2});

    if (args.next()) |a| {
        if (std.mem.eql(u8, a, "--no-print")) {
            return try binderTest(s.name orelse return error.NoName, &args, true);
        }

        var outfile: ?[]const u8 = null;
        if (std.mem.eql(u8, a, "--outfile")) {
            outfile = args.next() orelse return error.ExpectedFilename;
        }

        // const lexer = try js_lexer.Lexer.init(s, std.heap.c_allocator);
        // var parser = js_parser.Parser.init(lexer);
        // // parser.options.allow_jsx = true;

        // const t = std.time.microTimestamp();
        // var r = try parser.parse();
        // std.debug.print("parse time (us) {d:.3}\n", .{std.time.microTimestamp() - t});

        // const t2 = std.time.microTimestamp();
        // if (args.next()) |_| {
        //     const result = try js_parser.printWithSourceMap(r.data, r.root);
        //     std.debug.print("{s}\n", .{result.contents});
        // } else if (outfile) |name| {
        //     var result = try js_parser.printWithSourceMap(r.data, r.root);
        //     try std.fs.cwd().writeFile(.{
        //         .sub_path = name,
        //         .data = result.contents,
        //     });
        //     std.debug.print("emit time (us) {d:.3}\n", .{std.time.microTimestamp() - t2});
        //     const t3 = std.time.microTimestamp();
        //     const name2 = try std.fmt.allocPrint(std.heap.c_allocator, "{s}.map", .{name});
        //     try std.fs.cwd().writeFile(.{
        //         .sub_path = name2,
        //         .data = try result.source_map.toJson(std.heap.c_allocator),
        //     });
        //     std.debug.print("source map emit time (us) {d:.3}\n", .{std.time.microTimestamp() - t3});
        // }

        // var binder = js_parser.Binder.init(&r.data.nodes, std.heap.c_allocator);

        // const bind_start = std.time.microTimestamp();
        // try binder.visit(&r.root, r.root_ref);
        // std.debug.print("bind time (us) {d:.3}\n", .{std.time.microTimestamp() - bind_start});

        // return;

    }

    //try printCounts(&r);

    // if (should_print) {
    //     const t2 = std.time.microTimestamp();
    //     const result = try js_parser.printWithSourceMap(r.data, r.root);

    //     std.debug.print("print time {d:.3} (bytes: {d})\n", .{ std.time.microTimestamp() - t2, result.contents.len });
    //     std.debug.print("mappings size {}\n", .{result.source_map.mappings.items.len});

    //     //std.debug.print("count {}, total {}\n", .{ r.data.positions.?.count, r.data.nodes.count });

    //     const t3 = std.time.microTimestamp();
    //     // var f = try std.fs.cwd().createFile("x.js", .{});
    //     // defer f.close();
    //     // try f.writeAll(result.contents);
    //     // try f.writeAll("\n");
    //     // try f.writeAll(try result.source_map.toInlineComment(std.heap.c_allocator));
    //     std.debug.print("emit time {d:.3}\n", .{std.time.microTimestamp() - t3});
    // }

    if (s.name) |n| {
        try binderTest(n, &args, false);
    }
}

const VirtualFile = struct {
    name: []const u8,
    text: []const u8,
};

pub fn parseTestFile(allocator: std.mem.Allocator, text: []const u8) !std.ArrayList(VirtualFile) {
    const marker = "// @filename:";
    var files = std.ArrayList(VirtualFile).init(allocator);

    var i: usize = 0;
    var current_name: []const u8 = "main.ts";

    while (i < text.len) {
        const next_marker = std.mem.indexOfPos(u8, text, i, marker);
        if (next_marker == null) {
            const trimmed = std.mem.trim(u8, text[i..], "\r\n");
            var buf: [1024]u8 = undefined;
            const cwd = try std.process.getCwd(&buf);
            const resolved = try std.fs.path.resolve(allocator, &.{ cwd, current_name });
            std.debug.print("parsed chunked file -> {s}\n", .{resolved});
            if (trimmed.len > 0) {
                try files.append(.{
                    .name = resolved,
                    .text = trimmed,
                });
            }
            break;
        }

        const pos = next_marker.?;
        const eol = std.mem.indexOfPos(u8, text, pos, "\n") orelse text.len;
        const filename = std.mem.trim(u8, text[pos + marker.len .. eol], " \r\n");

        const chunk = std.mem.trim(u8, text[i .. pos], "\r\n");
        if (chunk.len > 0) {
            var buf: [1024]u8 = undefined;
            const cwd = try std.process.getCwd(&buf);
            const resolved = try std.fs.path.resolve(allocator, &.{ cwd, current_name });
            std.debug.print("parsed chunked file -> {s}\n", .{resolved});
            try files.append(.{
                .name = resolved,
                .text = chunk,
            });
        }

        current_name = filename;
        i = if (eol < text.len) eol + 1 else text.len;
    }

    return files;
}

fn binderTest(file_name: []const u8, args: *std.process.ArgIterator, comptime skip_print: bool, ) !void {
    const source = try std.fs.cwd().readFileAlloc(std.heap.c_allocator, file_name, std.math.maxInt(usize));
    const parsed = try parseTestFile(std.heap.c_allocator, source);


    const start = std.time.microTimestamp();
    var p2 = try program.Program.init(
        std.heap.c_allocator,
        &.{},
        "../out/lib",
    );

    var print_exported_types = false;
    if (args.next()) |x| {
        if (std.mem.eql(u8, x, "--types")) {
            print_exported_types = true;
        }
    }

    // const bundler = @import("./bundler.zig");
    // var bundler2 = try bundler.Bundler.init(&p2);

    for (parsed.items) |q| {
        try p2.addVirtualFile(q.name, q.text);
    }

  //  try bundler2.bundle(parsed.items[parsed.items.len-1].name);

    const a = try p2.getAnalyzer();

    for (parsed.items) |q| {
        const parse_start = std.time.microTimestamp();
        const id = try p2.getFileIdByPath(q.name);
        const f = try p2.getBoundFile(id);
        std.debug.print("parse+bind time {d:.3}\n", .{std.time.microTimestamp() - parse_start});
        const emit_start = std.time.microTimestamp();
        const text = try p2.printDeclarationText(id, f.ast.start);
        if (print_exported_types) {
            try printExportedTypes(f, &p2);
        }

        // try printCounts(&f.ast.nodes);

        if (!comptime skip_print) {
            std.debug.print("---- d.ts ----\n{s}\n", .{text});
        }
        std.debug.print("emit time {d:.3}\n", .{std.time.microTimestamp() - emit_start});
        std.debug.print("# of parse nodes {}\n", .{f.ast.nodes.count});

        if (f.diagnostics.items.len == 0) continue;

        std.debug.print("\n--- diagnostics ---\n",.{});
        const old_name = f.file_name;
        defer f.file_name = old_name;
        f.file_name = try std.fs.path.relative(std.heap.c_allocator, ".", file_name);
        try f.printDiagnostics();
        std.debug.print("\n-------------------\n",.{});
    }

    std.debug.print("\ntotal types: {}\n", .{a.types.count});
    if (a.types.count > 1000) {
        for (0..100) |i| {
            a.printTypeInfo(@intCast(i));
        }
    }

    std.debug.print("total time: {d:.3}\n", .{std.time.microTimestamp() - start});

    return;

    // const a = try p2.getAnalyzer();
    // const id = try p2.getFileIdByPath(file_name);
    // const f = try p2.getBoundFile(id);

    // std.debug.print("init time (us) {d:.3}\n", .{std.time.microTimestamp() - start});
    // std.debug.print("file symbol count {}\n", .{f.binder.symbols.count});

    // var total_symbols: u32 = 0;
    // var parsed_iter = a.program.parsed_files.iterator();
    // while (parsed_iter.next()) |y| {
    //     const pair = y.value_ptr.*;
    //     total_symbols += pair[0].binder.symbols.count;
    // }

    // std.debug.print("total symbol count {}\n", .{total_symbols});

    // if (comptime @import("builtin").mode == .Debug) {
    //     std.debug.print("SYMBOL DUMP\n", .{});
    //     const b = a.program.getFileData(id).binder;
    //     for (1..b.symbols.count) |i| {
    //         if (js_parser.getIdentFromSymbol(b, @intCast(i))) |ident| {
    //             std.debug.print("{} -> {s}\n", .{ i, try js_parser.getSlice(ident, u8) });
    //         }
    //     }
    // }

    // const analysis_start = std.time.microTimestamp();
    // const always_print_types = true;
    // const show_result = true; // comptime @import("builtin").mode == .Debug;

    // var prev_type_count: u32 = 0;

    // const r = f.binder.nodes;

    // if (comptime @import("builtin").mode == .Debug or always_print_types) {
    //     // std.debug.print("\nTYPES\n", .{});

    // }

    // const d1 = std.time.microTimestamp() - analysis_start;
    // const d2 = std.time.microTimestamp() - start;
    // std.debug.print("exported symbol analysis time {d:.3} (# of types {})\n", .{ d1, a.types.count });
    // std.debug.print("total time {d:.3}\n", .{d2});

    // if (comptime @import("builtin").mode == .Debug) {
    //     var i: u32 = 0;
    //     while (i < a.types.count) {
    //         a.printTypeInfo(i);
    //         i += 1;
    //     }
    // }

    // const emit_start = std.time.microTimestamp();
    // const text = try p2.printDeclarationText(id, f.ast.start);
    // std.debug.print("---- d.ts ----\n{s}\n", .{text});
    // std.debug.print("emit time {d:.3}\n", .{std.time.microTimestamp() - emit_start});
    // std.debug.print("# of types {} after emit\n", .{ a.types.count });
}

fn printExportedTypes(f: *program.ParsedFileData, p2: *program.Program) !void {
    const r = &f.ast.nodes;
    const id = f.id;
    const a = try p2.getAnalyzer();
    var iter2 = js_parser.NodeIterator.init(r, js_parser.maybeUnwrapRef(f.ast.nodes.at(f.ast.start)) orelse 0);
    while (iter2.nextPair()) |p| {
        var n = p[0];
        var ref = p[1];

        if (js_parser.hasFlag(n, .@"export")) {
            if (n.kind != .type_alias_declaration) continue;
            if (n.kind == .variable_statement) {
                ref = js_parser.unwrapRef(n); // XXX
                n = r.at(ref);
            }
            const l2 = js_parser.getLoc(r, n) orelse return error.MissingLocation;
            //std.debug.print("DECL {s}:{}:{}\n", .{ file_name, l2.line + 1, l2.col + 1 });

            const sym_ref = a.program.getFileData(id).binder.getTypeSymbol(ref) orelse
                a.program.getFileData(id).binder.getSymbol(ref) orelse return error.MissingSymbol;

            const analysis_start2 = std.time.microTimestamp();
            const t = try a.getTypeOfSymbol(try p2.getBoundFile(id), sym_ref);

            //std.debug.print("  ", .{});
            // try a.debugPrint(t);
            //std.debug.print("\n", .{});
            _ = l2;

            if (!a.isParameterizedRef(t)) {
                var resolved = t;
                var c: u32 = 0;
                while (c < 10) {
                    const next = try a.evaluateType(resolved, 1 << 0);
                    if (next == resolved) break;
                    resolved = next;
                    c += 1;
                }
                std.debug.print("   time (us) {d:.3} # of types {}\n", .{ std.time.microTimestamp() - analysis_start2, a.types.count });

                std.debug.print("\n", .{});
                std.debug.print("  RESOLVED -> ", .{});
                a._debug(resolved);
                std.debug.print("\n", .{});
            } else {
                std.debug.print("   PARAMETERIZED {any}\n", .{a.types.at(t).getKind()});
            }
        }
    }
}

// parse time 92929
// print time 37589 (bytes: 12601389)

// bind time 39400
// symbol count 213019
// time 0.16s user 0.03s system 97% cpu 0.189 total
