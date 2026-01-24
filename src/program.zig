const std = @import("std");
const strings = @import("./string_immutable.zig");
const parser = @import("./parser.zig");
const ComptimeStringMap = @import("comptime_string_map.zig").ComptimeStringMap;
const getAllocator = @import("./string_immutable.zig").getAllocator;

const SymbolRef = parser.SymbolRef;
const NodeRef = parser.NodeRef;
const NodeIterator = parser.NodeIterator;
const AstNode = parser.AstNode;
const AstData = parser.AstData;
const Binder = parser.Binder;
const ParsedFile = parser.ParsedFile;
const NodeFlags = parser.NodeFlags;
const SymbolFlags = parser.SymbolFlags;
const StringFlags = parser.StringFlags;
const BumpAllocator = parser.BumpAllocator;
const SyntaxKind = parser.SyntaxKind;
const NodeSymbolHash = parser.NodeSymbolHash;
const NodeList = parser.NodeList;
const BumpAllocatorList = parser.BumpAllocatorList;

const getPackedData = parser.getPackedData;
const unwrapRef = parser.unwrapRef;
const maybeUnwrapRef = parser.maybeUnwrapRef;
const getNumber = parser.getNumber;
const getSlice = parser.getSlice;
const getHashFromNode = parser.getHashFromNode;
const getIdentFromSymbol = parser.getIdentFromSymbol;

const isParameterDecl = parser.isParameterDecl;
const toBinaryDataPtrRefs = parser.toBinaryDataPtrRefs;

const getOrdinal = parser.getOrdinal;

const hasSymbolFlag = parser.hasSymbolFlag;

fn getHash(s: []const u8) u64 {
    return std.hash.Wyhash.hash(0, s);
}

inline fn getKeywordHash(keyword: parser.SyntaxKind) !u64 {
    return switch (keyword) {
        .this_keyword => comptime getHash("this"),
        .super_keyword => comptime getHash("super"),
        .null_keyword => comptime getHash("null"),
        .undefined_keyword => comptime getHash("undefined"),
        .number_keyword => comptime getHash("number"),
        .string_keyword => comptime getHash("string"),
        .boolean_keyword => comptime getHash("boolean"),
        .true_keyword => comptime getHash("true"),
        .false_keyword => comptime getHash("false"),
        else => error.InvalidKeyword,
    };
}

const ModuleResolver = struct {
    arena: std.heap.ArenaAllocator,
    working_dir: []const u8,

    pub fn initDefault() !@This() {
        const arena = std.heap.ArenaAllocator.init(getAllocator());
        var this = @This(){
            .arena = arena,
            .working_dir = &.{},
        };

        this.working_dir = try std.process.getCwdAlloc(this.arena.allocator());

        return this;
    }

    fn getResolveDir(this: *@This(), file_name: []const u8) !struct { should_free: bool = false, dir: []const u8 } {
        const dir = std.fs.path.dirname(file_name) orelse return .{ .dir = this.working_dir };
        if (std.fs.path.isAbsolute(dir)) return .{ .dir = dir };

        const resolved = try std.fs.path.resolve(this.arena.allocator(), &.{ this.working_dir, dir });

        return .{ .should_free = true, .dir = resolved };
    }

    fn findNodeModules(this: *@This(), origin: []const u8) anyerror!?[]const u8 {
        const resolve_dir = try this.getResolveDir(origin);
        defer if (resolve_dir.should_free) this.arena.allocator().free(resolve_dir.dir);

        return this._findNodeModules(resolve_dir.dir);
    }

    fn _findNodeModules(this: *@This(), dir: []const u8) anyerror!?[]const u8 {
        var found = false;
        const resolved = try std.fs.path.resolve(this.arena.allocator(), &.{ dir, "node_modules" });
        defer if (!found) this.arena.allocator().free(resolved);

        std.fs.accessAbsolute(resolved, .{}) catch |e| {
            if (e != std.fs.Dir.AccessError.FileNotFound) {
                return e;
            }

            return this._findNodeModules(std.fs.path.dirname(dir) orelse return null);
        };

        found = true;
        return resolved;
    }

    pub fn resolveTypesDirective(this: *@This(), origin: []const u8, name: []const u8) anyerror!?[]const u8 {
        const start = try this.findNodeModules(origin) orelse return null;

        var found = false;
        const resolved = try std.fs.path.resolve(this.arena.allocator(), &.{ start, "@types", name });
        defer if (!found) this.arena.allocator().free(resolved);

        std.fs.accessAbsolute(resolved, .{}) catch |e| {
            if (e != std.fs.Dir.AccessError.FileNotFound) {
                return e;
            }

            return this.resolveTypesDirective(std.fs.path.dirname(start) orelse return null, name);
        };

        found = true;
        return resolved;
    }

    pub fn resolveSpecifier(this: *@This(), origin: []const u8, spec: []const u8) ![]const u8 {
        if (std.fs.path.isAbsolute(spec)) return error.TODO;
        if (spec.len > 0 and spec[0] != '.') return error.TODO_bare_spec;

        const resolve_dir = try this.getResolveDir(origin);
        defer if (resolve_dir.should_free) this.arena.allocator().free(resolve_dir.dir);

        const ext = std.fs.path.extension(spec);
        const resolved = try std.fs.path.resolve(this.arena.allocator(), &.{ resolve_dir.dir, spec });
        if (strings.eqlComptime(ext, ".ts")) {
            return resolved;
        }

        defer this.arena.allocator().free(resolved);

        var stack_buf: [4096]u8 = undefined;
        const with_ext = try std.fmt.bufPrint(&stack_buf, "{s}.ts", .{resolved});

        const buf = try this.arena.allocator().alloc(u8, with_ext.len);
        @memcpy(buf, with_ext);

        return buf;
    }
};

const FileDiagnostic = struct {
    node_ref: NodeRef,
    message: []const u8,
};

pub const AsyncProgramLoader = struct {
    const HostFns = @import("./api.zig").CompilerHostFns;

    host_fns: *HostFns,
    program: *Program,
    file_requests: std.AutoArrayHashMap(u64, FileRef),
    completed: u32 = 0,

    on_done_ctx: ?*anyopaque = null,
    on_done: ?*const fn (ctx: ?*anyopaque) anyerror!void = null,

    mu: std.Thread.Mutex = std.Thread.Mutex{},

    pub fn init(program: *Program, host_fns: *HostFns) !@This() {
        return .{
            .program = program,
            .host_fns = host_fns,
            .file_requests = std.AutoArrayHashMap(u64, FileRef).init(program.allocator),
        };
    }

    pub fn loadFiles(this: *@This(), ctx: ?*anyopaque, on_done: *const fn (ctx: ?*anyopaque) anyerror!void) !void {
        this.program.did_load_async = true;

        this.on_done_ctx = ctx;
        this.on_done = on_done;

        for (this.program.options.default_libs) |f| {
            const resolved = try this.host_fns.callResolveFilePath(.lib, f, null);
            _ = try this.requestFile(resolved, true);
        }

        for (this.program.options.types) |f| {
            const resolved = try this.host_fns.callResolveFilePath(.types, f, null);
            const id = try this.requestFile(resolved, false);
            // XXX: need to clean this up
            const directive = try this.file_requests.allocator.create(parser.TripleSlashDirective);
            const name = try this.file_requests.allocator.alloc(u8, f.len);
            @memcpy(name, f);
            directive.* = .{
                .kind = .types,
                .value = name,
            };
            try this.program.associated_directives.put(id, directive);
        }

        this.program.did_load_default_libs = true;

        for (this.program.root_files) |f| {
            _ = try this.requestFile(f, false);
        }
    }

    fn onParsedFile(this: *@This(), parsed: *ParsedFile) !void {
        const n = parsed.source_name orelse return error.MissingName;
        const key = getHash(n);
        const ref = this.file_requests.get(key) orelse return error.MissingFileRef;

        try this.program.putFileData(key, ref, parsed);

        try this.evaluateDirectives(ref);

        if (!parsed.is_lib) {
            try this.resolveImports(this.program.getFileData(ref));
            try this.program.resolveAndBindModules(this.program.getFileData(ref));
        } else {
            try this.program.bindLibForAsync(this.program.getFileData(ref));
        }

        this.completed += 1;
        if (this.completed == this.file_requests.count()) {
            defer this.file_requests.deinit();

            if (this.on_done) |cb| {
                try cb(this.on_done_ctx);
            }
        }
    }

    pub fn requestFile(this: *@This(), name: []const u8, is_lib: bool) !FileRef {
        // this.mu.lock();
        // defer this.mu.unlock();

        const key = getHash(name);
        if (this.file_requests.get(key)) |ref| return ref;

        const id: FileRef = @intCast(this.program.files.items.len);
        const data = try this.program.allocator.create(ParsedFileData);
        // data.linkage = try this.program.allocator.create(ModuleLinkage);
        // data.linkage.* = ModuleLinkage{};

        try this.program.files.append(data);
        try this.file_requests.put(key, id);

        try this.host_fns.callRequestSourceFile(@This(), this, @This().onParsedFile, name, is_lib);

        return id;
    }

    fn resolveImports(this: *@This(), file: *ParsedFileData) anyerror!void {
        const origin_id = file.id;
        const origin = file.file_name orelse return error.MissingOrigin;

        var iter = file.binder.imports.iterator();
        while (iter.next()) |entry| {
            const resolved = this.host_fns.callResolveFilePath(.module, entry.value_ptr.spec, origin) catch {
                try this.program.getFileData(origin_id).unresolved_imports.append(entry.key_ptr.*);
                continue;
            };

            const id = try this.requestFile(resolved, false);
            try this.program.getFileData(origin_id).import_map.put(entry.key_ptr.*, id);
        }

        for (file.binder.exports.aliased_exports.items) |item| {
            const resolved = this.host_fns.callResolveFilePath(.module, item.spec, origin) catch |err| {
                if (err == error.FileNotFound) continue;
                return err;
            };
            _ = try this.requestFile(resolved, false);
        }
    }

    fn evaluateDirectives(this: *@This(), origin_id: u32) !void {
        const file = this.program.getFileData(origin_id);
        const origin = file.file_name orelse return error.MissingOrigin;
        const directives = file.ast.triple_slash_directives;
        for (directives) |*directive| {
            switch (directive.kind) {
                .lib => {
                    const resolved = try this.host_fns.callResolveFilePath(.lib, directive.value, origin);
                    _ = try this.requestFile(resolved, true);
                },
                .path => {
                    const resolved = try this.host_fns.callResolveFilePath(.path, directive.value, origin);
                    const id = try this.requestFile(resolved, false);
                    try this.program.associated_directives.put(id, this.program.associated_directives.get(origin_id) orelse directive);
                },
                .types => {
                    const resolved = try this.host_fns.callResolveFilePath(.types, directive.value, origin);
                    const id = try this.requestFile(resolved, false);
                    try this.program.associated_directives.put(id, directive);
                },
                .no_default_lib => {},
                else => return error.TODO,
            }
        }
    }
};

fn printSymbol(file: *ParsedFileData, sym_ref: SymbolRef) void {
    const ident = getIdentFromSymbol(file.binder, sym_ref) orelse return;

    const s = getSlice(ident, u8);
    if (parser.getLoc(&file.ast.nodes, ident)) |loc| {
        const file_name = file.file_name orelse "<unknown>";
        std.debug.print("{s} at {s}:{}:{}\n", .{ s, file_name, loc.line + 1, loc.col + 1 });
    } else {
        std.debug.print("{s}\n", .{s});
    }
}

fn printNameWithLocation(f: *ParsedFileData, ref: NodeRef) !void {
    const n = f.ast.nodes.at(ref);
    const name = getSlice(n, u8);
    const loc = parser.getLoc(&f.ast.nodes, n) orelse return error.MissingLocation;
    const file_name = f.file_name orelse return error.MissingFileName;
    std.debug.print("{s} at {s}:{}:{}\n", .{ name, file_name, loc.line + 1, loc.col + 1 });
}

const ProgramOptions = struct {
    default_libs: []const []const u8 = &.{},
    types: []const []const u8 = &.{},
};

// Roughly based off `tsc`
pub const Program = struct {
    allocator: std.mem.Allocator,

    options: ProgramOptions = .{},
    lib_dir: []const u8,
    root_files: []const []const u8,

    resolver: ModuleResolver,

    libs: std.AutoArrayHashMap(u64, FileRef),
    files: std.ArrayList(*ParsedFileData),
    parsed_files: std.AutoArrayHashMap(u64, ParsedPair),

    import_map: std.AutoArrayHashMapUnmanaged(u64, FileRef) = std.AutoArrayHashMapUnmanaged(u64, FileRef){},

    // Used for re-emits
    associated_directives: std.AutoArrayHashMap(FileRef, *const parser.TripleSlashDirective),

    ambient: Ambient,

    type_checker: ?*Analyzer = null,

    did_load_default_libs: bool = false,
    did_load_async: bool = false,

    // for testing
    virtual_files: std.AutoArrayHashMapUnmanaged(u64, []const u8) = std.AutoArrayHashMapUnmanaged(u64, []const u8){},

    loader: ?*anyopaque = null,

    pub fn init(
        allocator: std.mem.Allocator,
        root_files: []const []const u8,
        lib_dir: []const u8,
    ) !@This() {
        return .{
            .allocator = allocator,
            .root_files = root_files,
            .lib_dir = lib_dir,
            .ambient = Ambient.init(allocator),
            .resolver = try ModuleResolver.initDefault(),
            .libs = std.AutoArrayHashMap(u64, FileRef).init(allocator),
            .files = std.ArrayList(*ParsedFileData).init(allocator),
            .parsed_files = std.AutoArrayHashMap(u64, ParsedPair).init(allocator),
            .associated_directives = std.AutoArrayHashMap(FileRef, *const parser.TripleSlashDirective).init(allocator),
        };
    }

    // for testing
    pub fn addVirtualFile(this: *@This(), file_name: []const u8, data: []const u8) !void {
        try this.virtual_files.put(this.allocator, getHash(file_name), data);
    }

    pub fn getTypeChecker(this: *@This()) !*Analyzer {
        if (this.type_checker) |p| return p;

        const p = try this.allocator.create(Analyzer);
        p.* = try Analyzer.init(this);
        this.type_checker = p;

        return p;
    }

    pub fn putFileData(this: *@This(), hash: u64, id: FileRef, parsed: *ParsedFile) !void {
        const linkage = this.files.items[id].linkage;
        this.files.items[id].* = ParsedFileData.initFromParsedFile(parsed, linkage);
        this.files.items[id].id = id;
        try this.parsed_files.put(hash, .{ parsed, id });
    }

    fn parseFile(this: *@This(), file_name: []const u8, is_lib: bool, is_declaration: bool) !FileRef {
        const hash = getHash(file_name);
        if (this.parsed_files.get(hash)) |pair| {
            return pair[1];
        }

        var parsed: *ParsedFile = undefined;

        if (this.virtual_files.get(hash)) |data| {
            parsed = try ParsedFile.createFromBuffer(data, file_name, is_lib, null);
        } else {
            parsed = try ParsedFile.createFromPath(file_name, is_lib);
        }

        const id: u32 = @intCast(this.files.items.len);
        var data = try this.allocator.create(ParsedFileData);
        // data.linkage = try this.allocator.create(ModuleLinkage);
        // data.linkage.* = ModuleLinkage{};
        try this.files.append(data);
        try this.putFileData(hash, id, parsed);
        data.is_declaration = is_declaration;

        return id;
    }

    pub fn getLib(this: *@This(), name: []const u8) !*ParsedFileData {
        const hash = getHash(name);
        if (this.libs.get(hash)) |ref| {
            return this.getFileData(ref);
        }

        const basename = try std.fmt.allocPrint(this.allocator, "lib.{s}.d.ts", .{name});
        defer this.allocator.free(basename);

        const resolved = try std.fs.path.resolve(this.allocator, &.{ this.lib_dir, basename });
        const id = try this.parseFile(resolved, true, true);

        try this.libs.put(hash, id);

        return this.getFileData(id);
    }

    fn bindMapToGlobals(this: *@This(), file: *ParsedFileData, m: *std.AutoArrayHashMapUnmanaged(NodeSymbolHash, SymbolRef), comptime is_type: bool) !void {
        var dst = switch (is_type) {
            true => &this.ambient.global_types,
            false => &this.ambient.globals,
        };

        // TODO: dedupe w/ `bindLibMap`
        var iter = m.iterator();
        while (iter.next()) |entry| {
            const r = try dst.getOrPut(entry.key_ptr.*);
            var id: Ambient.GlobalRef = undefined;
            if (r.found_existing) {
                id = r.value_ptr.*;
            } else {
                id = try this.ambient.globals_allocator.push(.{});
                r.value_ptr.* = id;
            }

            try this.ambient.globals_allocator.at(id).symbols.append(
                this.ambient.globals.allocator,
                .{ .file_id = file.id, .ref = entry.value_ptr.* },
            );
        }
    }

    const BindLibMode = enum { globals, types, modules };

    pub fn bindLibMap(this: *@This(), file: *ParsedFileData, comptime mode: BindLibMode) !void {
        const m = switch (mode) {
            .globals => &file.binder.exports.symbols,
            .types => &file.binder.exports.type_symbols,
            .modules => &file.binder.ambient_modules,
        };

        var dst = switch (mode) {
            .globals => &this.ambient.globals,
            .types => &this.ambient.global_types,
            .modules => &this.ambient.modules,
        };

        var iter = m.iterator();
        while (iter.next()) |entry| {
            const r = try dst.getOrPut(entry.key_ptr.*);
            var id: Ambient.GlobalRef = undefined;
            if (r.found_existing) {
                id = r.value_ptr.*;
            } else {
                id = try this.ambient.globals_allocator.push(.{});
                r.value_ptr.* = id;
            }

            try this.ambient.globals_allocator.at(id).symbols.append(
                this.ambient.globals.allocator,
                .{ .file_id = file.id, .ref = entry.value_ptr.* },
            );

            // we only handle `types` because `declare var x: Foo` is 
            // pointless because you cannot change the type
            if (comptime mode == .types) {
                var sym = file.binder.symbols.at(entry.value_ptr.*);
                // std.debug.assert(sym.hasFlag(.global));
                std.debug.assert(sym.binding == 0);
                sym.binding = id;
            }
        }
    }

    fn maybeBindGlobalNamespace(this: *@This(), file: *ParsedFileData) !void {
        const global_ns = blk: {
            for (file.binder.namespaces.items) |*ns| {
                if (ns.is_global) {
                    break :blk ns;
                }
            }

            return;
        };

        try this.bindMapToGlobals(file, &global_ns.symbols, false);
        try this.bindMapToGlobals(file, &global_ns.type_symbols, true);
    }

    inline fn _bindLib(this: *@This(), f: *ParsedFileData) anyerror!void {
        try this.bindLibMap(f, .globals);
        try this.bindLibMap(f, .types);
        try this.bindLibMap(f, .modules);

        try this.bindGlobals(f);
    }

    fn bindLib(this: *@This(), f: *ParsedFileData) anyerror!void {
        if (f.did_analyze) return;

        f.did_analyze = true;

        try this.evaluateDirectives(f.id);

        try this._bindLib(f);
    }

    pub fn bindLibForAsync(this: *@This(), f: *ParsedFileData) anyerror!void {
        if (f.did_analyze) return;

        f.did_analyze = true;

        try this.maybeBindGlobalNamespace(f);

        try this._bindLib(f);
    }

    fn resolveImports(this: *@This(), file: *ParsedFileData) anyerror!void {
        const origin_id = file.id;
        const origin = file.file_name orelse return error.MissingFileName; 

        var iter = file.binder.imports.iterator();
        while (iter.next()) |entry| {
            const resolved = this.resolver.resolveSpecifier(origin, entry.value_ptr.spec) catch {
                try this.getFileData(origin_id).unresolved_imports.append(entry.key_ptr.*);
                continue;
            };

            const module_id = try this.getResolvedModule(resolved);
            try this.getFileData(origin_id).import_map.put(entry.key_ptr.*, module_id);
        }
    }

    fn getResolvedModule(this: *@This(), resolved: []const u8) !FileRef {
        const hash = std.hash.Wyhash.hash(0, resolved);
        if (this.parsed_files.get(hash)) |tuple| {
            return tuple[1];
        }

        return try this.loadModule(resolved, hash);
    }

    pub inline fn getFileData(this: *const @This(), id: FileRef) *ParsedFileData {
        return this.files.items[id];
    }

    fn evaluateDirectives(this: *@This(), origin_id: u32) !void {
        for (this.getFileData(origin_id).ast.triple_slash_directives) |*directive| {
            switch (directive.kind) {
                .lib => {
                    const lib = try this.getLib(directive.value);
                    try this.bindLib(lib);
                },
                .path => {
                    const resolved = blk: {
                        if (std.fs.path.isAbsolute(directive.value)) break :blk directive.value;

                        const p = this.getFileData(origin_id).file_name orelse return error.MissingImporterFileName;
                        if (std.fs.path.dirname(p)) |dir| {
                            break :blk try std.fs.path.resolve(this.allocator, &.{ dir, directive.value });
                        }
                        break :blk try std.fs.path.resolve(this.allocator, &.{ this.resolver.working_dir, directive.value });
                    };
                    const id = try this.parseFile(resolved, false, true); // d.ts file

                    try this.associated_directives.put(id, this.associated_directives.get(origin_id) orelse directive);

                    try this.maybeBindGlobalNamespace(this.getFileData(id));
                    try this.bindLib(this.getFileData(id));
                },
                .types => {
                    const origin = this.getFileData(origin_id).file_name orelse return error.MissingImporterFileName;
                    const resolved = try this.resolver.resolveTypesDirective(origin, directive.value) orelse return error.MissingTypesDirective;

                    // TODO: this has to read from `package.json`
                    std.debug.print("types dir {s} -> {s}", .{ directive.value, resolved });

                    // TODO: associate directive w/ file
                },
                .no_default_lib => {},
                else => return error.TODO,
            }
        }
    }

    fn loadDefaultLibs(this: *@This()) !void {
        if (this.did_load_default_libs) return;
        this.did_load_default_libs = true;

        for (this.options.default_libs) |name| {
            try this.bindLib(try this.getLib(name));
        }
    }

    fn bindToGlobalSymbol(this: *@This(), f: *ParsedFileData, sym_hash: NodeSymbolHash, sym_ref: SymbolRef, comptime is_type: bool) !void {
        const m = if (comptime is_type) &this.ambient.global_types else &this.ambient.globals;
        var sym = f.binder.symbols.at(sym_ref);

        const global_ref = m.get(sym_hash) orelse blk: {
            if (!comptime is_type) {
                const global_this_hash: u32 = comptime @truncate(std.hash.Wyhash.hash(0, "globalThis"));
                if (sym_hash == global_this_hash) {
                    break :blk std.math.maxInt(u32);
                }
            }

            if (comptime is_debug) {
                if (!f.is_lib) {
                    try printNameWithLocation(f, sym.binding);
                    const name = getSlice(f.ast.nodes.at(sym.binding), u8);
                    if (comptime is_type) {
                        std.debug.print("  missing type {s}\n", .{name});
                    } else {
                        std.debug.print("  missing {s}\n", .{name});
                    }
                }
            }

            // We'll bind early (without a decl) for lib files
            // FIXME: `global` augmentations should only be applied when importing a module that declares it
            const id = try this.ambient.globals_allocator.push(.{});
            try m.put(sym_hash, id);
            break :blk id;
        };

        sym.declaration = global_ref;
    }

    fn getNamespaceFromSymbol(this: *const @This(), absolute_symbol: AbsoluteSymbolRef) Binder.Namespace {
        const b = this.getFileData(absolute_symbol.file_id).binder;
        const ns_sym = b.symbols.at(absolute_symbol.ref);
        
        return b.namespaces.items[ns_sym.binding];
    }

    fn followAliasedExports(this: *@This(), imported_sym: *parser.Symbol, target_hash: u32, sym: *parser.Symbol) !void {
        if (imported_sym.next == std.math.maxInt(u32)) {
            // non-ambient export
            const f = this.getFileData(imported_sym.getOrdinal());

            // FIXME: type exports are ignored 
            const maybe = f.binder.exports.symbols.get(target_hash) orelse {
                return error.AliasedExportError;
            };

            const maybe_sym = f.binder.symbols.at(maybe);
            if (maybe_sym.hasFlag(.aliased_module)) {
                return this.followAliasedExports(maybe_sym, target_hash, sym);
            }
            
            sym.addFlag(.local);
            sym.declaration = maybe;
            sym.ordinal |= @as(u16, @intCast(imported_sym.getOrdinal()));
            return;
        }

        const items = this.ambient.globals_allocator.at(imported_sym.next).symbols.items;
        for (items) |g| {
            const b2 = this.getFileData(g.file_id).binder;
            const ns_sym2 = b2.symbols.at(g.ref);
            const ns2 = b2.namespaces.items[ns_sym2.binding];
            const maybe = ns2.exports.?.symbols.get(target_hash) orelse continue;

            const maybe_sym = b2.symbols.at(maybe);
            if (maybe_sym.hasFlag(.aliased_module)) {
                return this.followAliasedExports(maybe_sym, target_hash, sym);
            }

            sym.addFlag(.local);
            sym.declaration = maybe;
            sym.ordinal |= @as(u16, @intCast(g.file_id));
        }

        return error.MissingAliasedExportedSymbolModule;
    }

    // fn searchAliasedExports(this: *@This(), exports: *parser.Exports, sym_hash: u32) !void {

    // }

    fn bindImports(this: *@This(), f: *ParsedFileData) !void {
        var iter = f.binder.imports.iterator();
        while (iter.next()) |entry| {
            const imported_id = f.import_map.get(entry.key_ptr.*) orelse return error.MissingImport;
            // ambient module
            if (imported_id >> 31 == 1) {
                const global_ref = @as(u31, @truncate(imported_id));
                var sym_iter = entry.value_ptr.bindings.iterator();
                while (sym_iter.next()) |sym_entry| {
                    const symbols = this.ambient.globals_allocator.at(global_ref).symbols;
                    if (symbols.items.len != 1) {
                        return error.TODO_multiple_ambient_module_decls;
                    }

                    const imported_file = this.getFileData(symbols.items[0].file_id);
                    try this.processAliasedExports(imported_file);

                    const b = imported_file.binder;
                    const ns_sym = b.symbols.at(symbols.items[0].ref);
                    const ns = b.namespaces.items[ns_sym.binding];

                    const exports = ns.exports orelse return error.MissingModuleExports;


                    const imported_sym = exports.symbols.get(sym_entry.key_ptr.*) orelse exports.type_symbols.get(sym_entry.key_ptr.*) orelse {
                        printSymbol(f, sym_entry.value_ptr.*);
                        return error.MissingImportedSymbolModule;
                    };

                    var sym = f.binder.symbols.at(sym_entry.value_ptr.*);

                    const imported_sym2 = b.symbols.at(imported_sym);
                    if (imported_sym2.hasFlag(.aliased_module)) {
                        try this.followAliasedExports(imported_sym2, sym_entry.key_ptr.*, sym);
                        continue;
                    }

                    sym.declaration = imported_sym;
                    sym.ordinal |= @as(u16, @intCast(symbols.items[0].file_id));
                }

                // star imports
                var next: SymbolRef = entry.value_ptr.namespace_binding orelse 0;
                while (next != 0) {
                    var sym = f.binder.symbols.at(next);
                    next = sym.next;
                    sym.declaration = global_ref;
                    sym.ordinal |= (@as(u32, @intFromEnum(SymbolFlags.late_bound)) << 16);
                }
                continue;
            }

            const imported_file = this.getFileData(imported_id);
            try this.processAliasedExports(imported_file); // TODO: we could do this lazily if we cannot find symbols normally

            const b = imported_file.binder;

            // std.debug.print("exported symbol count -> {} [id: {}]\n", .{ imported_file.binder.exports.keys().len, imported_id });

            var sym_iter = entry.value_ptr.bindings.iterator();
            while (sym_iter.next()) |sym_entry| {
                const imported_sym = b.exports.symbols.get(sym_entry.key_ptr.*) orelse b.exports.type_symbols.get(sym_entry.key_ptr.*) orelse {
                    printSymbol(f, sym_entry.value_ptr.*);
                    return error.MissingImportedSymbol;
                };

                var sym = f.binder.symbols.at(sym_entry.value_ptr.*);

                const imported_sym2 = b.symbols.at(imported_sym);
                if (imported_sym2.hasFlag(.aliased_module)) {
                    try this.followAliasedExports(imported_sym2, sym_entry.key_ptr.*, sym);
                    continue;
                }

                sym.declaration = imported_sym;
                sym.ordinal |= @as(u16, @intCast(imported_id));
            }

            // star imports
            var next: SymbolRef = entry.value_ptr.namespace_binding orelse 0;
            while (next != 0) {
                var sym = f.binder.symbols.at(next);
                sym.ordinal |= @as(u16, @intCast(imported_id));
                next = sym.next;
            }

            // default imports
            next = entry.value_ptr.default_binding orelse 0;
            while (next != 0) {
                const imported_node = imported_file.binder.default_export orelse return error.MissingDefaultExport;
                var sym = f.binder.symbols.at(next);
                sym.declaration = (1 << 30) | imported_node; // node ref
                sym.ordinal |= @as(u16, @intCast(imported_id));
                next = sym.next;
            }
        }
    }

    fn bindGlobals(this: *@This(), f: *ParsedFileData) !void {
        {
            var iter = f.binder.unbound_symbols.iterator();
            while (iter.next()) |entry| {
                try this.bindToGlobalSymbol(f, entry.key_ptr.*, entry.value_ptr.*, false);
            }
        }
        {
            var iter = f.binder.unbound_type_symbols.iterator();
            while (iter.next()) |entry| {
                try this.bindToGlobalSymbol(f, entry.key_ptr.*, entry.value_ptr.*, true);
            }
        }
    }

    pub fn bindModule(this: *@This(), f: *ParsedFileData) !void {
        try this.resolveAndBindModules(f);

        if (f.did_bind_imports) return;
        f.did_bind_imports = true;

        if (!this.did_load_async) {
            try this.loadDefaultLibs();
            try this.evaluateDirectives(f.id);
        }

        // Finalize modules
        for (f.unresolved_imports.items) |hash| {
            const global_ref = this.ambient.modules.get(hash) orelse {
                const import = f.binder.imports.get(hash) orelse @panic("Missing import spec");
                std.debug.print("missing {s}\n", .{import.spec});
                return error.MissingModule;
            };
            try f.import_map.put(hash, (1 << 31) | global_ref);
        }

        try this.bindGlobals(f);
        try this.bindImports(f);
    }

    fn processAliasedExports(this: *@This(), f: *ParsedFileData) anyerror!void {
        if (f.did_bind_aliased_exports) return;

        f.did_bind_aliased_exports = true;

        const origin = f.file_name orelse return error.MissingFileName;
        const exports2 = &f.binder.exports;
        for (exports2.aliased_exports.items) |item| {
            const resolved = try this.resolver.resolveSpecifier(origin, item.spec);
            const file_id = try this.getResolvedModule(resolved);
            if (!this.did_load_async) {
                try this.bindModule(this.getFileData(file_id));
            }
            try this.processAliasedExports(this.getFileData(file_id));

            var sym2 = f.binder.symbols.at(item.sym);
            sym2.ordinal |= @as(u16, @intCast(file_id));

            if (item.target == 0) {
                if (item.binding == 0) {
                    // export * from 'y'
                    var iter = this.getFileData(file_id).binder.exports.symbols.iterator();

                    sym2.next = std.math.maxInt(u32);

                    while (iter.next()) |item2| {
                        try @constCast(exports2).symbols.put(f.binder.allocator, item2.key_ptr.*, item.sym);
                    }
                } else {
                    // export * as y from 'y'
                    std.debug.assert(sym2.hasFlag(.namespace));
                    try @constCast(exports2).symbols.put(f.binder.allocator, getHashFromNode(f.ast.nodes.at(sym2.binding)), item.sym);
                }
            } else {
                const sym_hash = getHashFromNode(f.ast.nodes.at(sym2.declaration));

                sym2.declaration = this.getFileData(file_id).binder.exports.symbols.get(sym_hash) 
                    orelse return error.MissingAliasedExport;

                if (item.binding == 0) {
                    // export { x } from 'y'
                    try @constCast(exports2).symbols.put(f.binder.allocator, sym_hash, item.sym);
                } else {
                    // export { x as y } from 'y'
                    const aliased_hash = getHashFromNode(f.ast.nodes.at(sym2.binding));
                    try @constCast(exports2).symbols.put(f.binder.allocator, aliased_hash, item.sym);
                }
            }
        }

        // ambient modules cannot use relative specifiers
        var module_iter = f.binder.ambient_modules.iterator();
        while (module_iter.next()) |module_entry| {
            const sym = f.binder.symbols.at(module_entry.value_ptr.*);
            std.debug.assert(sym.hasFlag(.namespace));
            const ns = f.binder.namespaces.items[sym.binding];
            // std.debug.print("re-exports {s} -> {s}\n", .{f.file_name orelse "", ns.module_specifier orelse ""});

            var exports3 = ns.exports orelse return error.MissingModuleExports;
            for (exports3.aliased_exports.items) |item| {
                const global_ref = this.ambient.modules.get(getHash(item.spec)) orelse {
                    std.debug.print("missing re-exported module '{s}'\n", .{item.spec});
                    continue;
                };

                if (item.target == 0) {
                    if (item.binding == 0) {
                        const symbols = this.ambient.globals_allocator.at(global_ref).symbols.items;
                        const x = this.getNamespaceFromSymbol(symbols[0]);
                        var iter = x.exports.?.symbols.iterator();

                        var sym2 = f.binder.symbols.at(item.sym);
                        sym2.ordinal |= @as(u16, @intCast(symbols[0].file_id));
                        sym2.next = global_ref;

                        while (iter.next()) |item2| {
                            try exports3.symbols.put(f.binder.allocator, item2.key_ptr.*, item.sym);
                        }
                    } else {
                        // exports2.symbols.put(f.binder.scopes.allocator, getHashFromNode(), );
                    }
                } else {

                }
            }
        }
    }

    pub fn resolveAndBindModules(this: *@This(), f: *ParsedFileData) !void {
        if (f.did_resolve_imports) return;
        f.did_resolve_imports = true;

        if (!this.did_load_async) {
            try this.resolveImports(f);
        }

        // We need to bind module augmentation early
        try this.bindLibMap(f, .modules);
        try this.maybeBindGlobalNamespace(f); // TODO: only do this in ambient contexts
    }

    fn loadModule(this: *@This(), file_name: []const u8, hash: u64) anyerror!FileRef {
        _ = hash; // TODO: don't compute hash twice
        const id = try this.parseFile(file_name, false, false);
        try this.resolveAndBindModules(this.getFileData(id));

        return id;
    }

    pub fn getBoundFile(this: *@This(), id: FileRef) !*ParsedFileData {
        const f = this.getFileData(id);
        if (!f.is_lib) {
            try this.bindModule(f);
        }

        return this.getFileData(id);
    }

    pub fn getFileByPath(this: *@This(), file_name: []const u8) !?*ParsedFile {
        const hash = getHash(file_name);
        if (this.parsed_files.get(hash)) |pair| return pair[0];
        return null;
    }

    pub fn getFileIdByPath(this: *@This(), file_name: []const u8) !FileRef {
        const hash = getHash(file_name);
        if (this.parsed_files.get(hash)) |pair| {
            return pair[1];
        }
        for (this.root_files) |f| {
            if (strings.eql(f, file_name)) {
                return this.parseFile(f, false, false);
            }
        }

        // for testing
        if (this.virtual_files.get(hash) != null) {
            return this.parseFile(file_name, false, false);
        }

        return error.MissingFile;
    }

    pub fn transformSyn(this: *@This(), file_id: u32, start: NodeRef) !std.AutoArrayHashMap(NodeRef, NodeRef) {
        const f = this.getFileData(file_id);
        std.debug.assert(f.ast.nodes.at(start).kind == .source_file);

        try this.bindModule(f);

        const Visitor = struct {
            const Kind = Analyzer.Kind;

            file: *ParsedFileData,
            nodes: *BumpAllocator(AstNode),
            analyzer: *Analyzer,
            replacements: *std.AutoArrayHashMap(NodeRef, NodeRef),
            is_async_ctx: bool = true, // we start in a module
            as_type_node: ?*const AstNode = null,

            fn makeIdent(self: *@This(), comptime s: []const u8) !u32 {
                const z = try getAllocator().alloc(u8, s.len);
                @memcpy(z, s);
                return try self.nodes.push(.{
                    .kind = .identifier,
                    .data = z.ptr,
                    .len = @intCast(z.len),
                });
            }

            fn typeofExp(self: *@This(), subject: u32, comptime s: []const u8) !u32 {
                const z = try getAllocator().alloc(u8, s.len);
                @memcpy(z, s);
                const rhs = try self.nodes.push(.{
                    .kind = .string_literal,
                    .data = z.ptr,
                    .len = @intCast(z.len),
                });

                const lhs = try self.nodes.push(.{
                    .kind = .type_of_expression,
                    .data = @ptrFromInt(subject),
                });

                return try self.equalsExp(lhs, rhs);
            }

            fn equalsExp(self: *@This(), subject: u32, rhs: u32) !u32 {
                return try self.nodes.push(.{
                    .kind = .binary_expression,
                    .data = toBinaryDataPtrRefs(subject, rhs),
                    .len = @intFromEnum(SyntaxKind.equals_equals_equals_token),
                });
            }

            fn notEqualsExp(self: *@This(), subject: u32, rhs: u32) !u32 {
                return try self.nodes.push(.{
                    .kind = .binary_expression,
                    .data = toBinaryDataPtrRefs(subject, rhs),
                    .len = @intFromEnum(SyntaxKind.exclamation_equals_equals_token),
                });
            }

            fn generateIsExpression(self: *@This(), subject: u32, ty: u32) anyerror!u32 {
                //const TypeRef = Analyzer.TypeRef;
                const getSlice2 = Analyzer.getSlice2;

                if (self.analyzer.isParameterizedRef(ty)) {
                    if (ty < @intFromEnum(Kind.false)) {
                        // const inner = self.analyzer.types.at(ty);
                        // if (self.analyzer.getKindOfRef(inner.slot3) == .function_literal) {
                        //     return try self.typeofExp(subject, "function");
                        // }
                    } else {
                        self.analyzer.printTypeInfo(ty);
                        return error.TODO_parameterized;
                    }
                }

                if (ty >= @intFromEnum(Kind.false)) {
                    if (ty == @intFromEnum(Kind.false)) {
                        const rhs = try self.nodes.push(.{
                            .kind = .false_keyword,
                        });
                        return try self.equalsExp(subject, rhs);
                    }
                    if (ty == @intFromEnum(Kind.true)) {
                        const rhs = try self.nodes.push(.{
                            .kind = .true_keyword,
                        });
                        return try self.equalsExp(subject, rhs);
                    }
                    if (ty == @intFromEnum(Kind.undefined)) {
                        const rhs = try self.nodes.push(.{
                            .kind = .undefined_keyword,
                        });
                        return try self.equalsExp(subject, rhs);
                    }
                    if (ty == @intFromEnum(Kind.null)) {
                        const rhs = try self.nodes.push(.{
                            .kind = .null_keyword,
                        });
                        return try self.equalsExp(subject, rhs);
                    }

                    // if (ty == @intFromEnum(Kind.void)) {
                    //     return try this.getIntrinsic("Void");
                    // }

                    // if (ty == @intFromEnum(Kind.any)) {
                    //     return try this.getIntrinsic("any");
                    // }
                    // if (ty == @intFromEnum(Kind.never)) {
                    //     return try this.getIntrinsic("never");
                    // }
                    // if (ty == @intFromEnum(Kind.unknown)) {
                    //     return try this.getIntrinsic("unknown");
                    // }

                    if (ty == @intFromEnum(Kind.string)) {
                        return try self.typeofExp(subject, "string");
                    }
                    if (ty == @intFromEnum(Kind.number)) {
                        return try self.typeofExp(subject, "number");
                    }
                    if (ty == @intFromEnum(Kind.boolean)) {
                        return try self.typeofExp(subject, "boolean");
                    }
                    if (ty == @intFromEnum(Kind.object)) {
                        return try self.typeofExp(subject, "object");
                    }
                    if (ty == @intFromEnum(Kind.symbol)) {
                        return try self.typeofExp(subject, "symbol");
                    }

                    // if (ty == @intFromEnum(Kind.empty_string)) {
                    //     return try js.String.fromUtf8(this.env, "");
                    // } else if (ty == @intFromEnum(Kind.empty_object)) {
                    //     return try this.createShape("__Object");
                    // } else if (ty == @intFromEnum(Kind.empty_tuple)) {
                    //     return try this.createShape("__Tuple");
                    // }

                    // empty_element -> undefined ?

                    // TODO
                    // if (ty >= @intFromEnum(Kind.zero)) {
                    //     const rhs = try self.nodes.push(.{

                    //     });

                    //     return try self.nodes.push(.{
                    //         .kind = .binary_expression,
                    //         .data = toBinaryDataPtrRefs(subject, rhs),
                    //         .len = @intFromEnum(SyntaxKind.equals_equals_equals_token),
                    //     });
                    // }

                    self.analyzer.printTypeInfo(ty);
                    return error.TODO_unhandled_primitve_type;
                }

                const t = self.analyzer.types.at(ty);
                switch (t.getKind()) {
                    .alias => {
                        // if (try this.hasCached(ty)) {
                        //     return try this.getCached(ty);
                        // }

                        const followed = try self.analyzer.evaluateType(ty, 1 << 0);
                        if (ty == followed) {
                            self.analyzer.printTypeInfo(ty);
                            return error.RecursiveAlias;
                        }

                        const result = try self.generateIsExpression(subject, followed);
                       // try this.setCached(ty, @ptrCast(result));

                        return result;
                    },
                    .conditional, .indexed, .keyof, .query, .mapped, .intersection => {
                        const followed = try self.analyzer.evaluateType(ty, 1 << 0 | 1 << 30);
                        if (ty == followed) {
                            self.analyzer.printTypeInfo(ty);
                            return error.Recursive;
                        }

                        return try self.generateIsExpression(subject, followed);
                    },
                    // .array => {
                    //     const o = try this.createSavedShape(ty, "__ArrayType");
                    //     try this.setField(o, "element", @alignCast(@ptrCast(try this.reifyType(t.slot0))));
                    //     return o;
                    // },
                    // .tuple => {
                    //     const o = try this.createSavedShape(ty, "__Tuple");
                    //     const types = getSlice2(t, TypeRef);
                    //     for (types) |u| {
                    //         if (u < @intFromEnum(Kind.false)) {
                    //             const t2 = self.analyzer.types.at(u);
                    //             const el_type = t2.slot1;
                    //             try this.callMethod(o, "add", .{
                    //                 @as(*js.Value, @alignCast(@ptrCast(try this.reifyType(el_type))))
                    //             });
                    //             continue;
                    //         }

                    //         try this.callMethod(o, "add", .{
                    //             @as(*js.Value, @alignCast(@ptrCast(try this.reifyType(u))))
                    //         });
                    //     }
                    //     return o;
                    // },
                    // .named_tuple_element => {
                    //     const o = try this.createSavedShape(ty, "__TupleElement");
                    //     const types = getSlice2(t, TypeRef);
                    //     for (types) |u| {
                    //         try this.callMethod(o, "add", .{
                    //             @as(*js.Value, @alignCast(@ptrCast(try this.reifyType(u))))
                    //         });
                    //     }
                    //     return o;
                    // },
                    // .@"union" => {
                    //     const o = try this.createSavedShape(ty, "__Union");
                    //     const types = getSlice2(t, TypeRef);
                    //     for (types) |u| {
                    //         try this.callMethod(o, "add", .{
                    //             @as(*js.Value, @alignCast(@ptrCast(try this.reifyType(u))))
                    //         });
                    //     }
                    //     return o;
                    // },
                    .string_literal => {
                        const name = self.analyzer.getSliceFromLiteral(ty);
                        const z = try getAllocator().alloc(u8, name.len);
                        @memcpy(z, name);
                        const lit = try self.nodes.push(.{
                            .kind = .string_literal,
                            .data = z.ptr,
                            .len = @intCast(z.len),
                        });

                        return try self.equalsExp(subject, lit);
                    },
                    // .number_literal => {
                    //     return try js.Number.createDouble(this.env, self.analyzer.getDoubleFromType(ty));
                    // },
                    .object_literal => {
                        // if (try this.hasCached(ty)) {
                        //     return try this.getCached(ty);
                        // }

                        // const o = try this.createSavedShape(ty, "__Object");
                        // try this.setCached(ty, @ptrCast(o));

                        var lhs: u32 = try self.typeofExp(subject, "object");
                        lhs = try self.nodes.push(.{
                            .kind = .binary_expression,
                            .data = toBinaryDataPtrRefs(lhs, try self.notEqualsExp(subject, try self.nodes.push(.{ .kind = .null_keyword }))),
                            .len = @intFromEnum(SyntaxKind.ampersand_ampersand_token)
                        });

                        const members = getSlice2(t, Analyzer.ObjectLiteralMember);
                        for (members) |*u| {
                            if (u.kind != .property and u.kind != .method) continue; // TODO

                            if (self.analyzer.getKindOfRef(u.name) == .symbol_literal) continue; // TODO

                            const name = self.analyzer.getSliceFromLiteral(u.name);
                            const z = try getAllocator().alloc(u8, name.len);
                            @memcpy(z, name);
                            const member = try self.nodes.push(.{
                                .kind = .identifier,
                                .data = z.ptr,
                                .len = @intCast(z.len),
                            });


                            const sub = try self.nodes.push(.{
                                .kind = .property_access_expression,
                                .data = toBinaryDataPtrRefs(subject, member),
                            });

                            const q = try self.generateIsExpression(sub, try u.getType(self.analyzer));

                            lhs = try self.nodes.push(.{
                                .kind = .binary_expression,
                                .data = toBinaryDataPtrRefs(lhs, q),
                                .len = @intFromEnum(SyntaxKind.ampersand_ampersand_token)
                            });
                        }

                        return lhs;
                    },
                    .parameterized => {
                        return try self.typeofExp(subject, "function");
                    },
                    .function_literal => {
                        return try self.typeofExp(subject, "function");
                    },
                    else => {},
                    // class
                    // template_literal
                    // module_namespace
                    // symbol_literal (only well-known symbols)
                }

                self.analyzer.printTypeInfo(ty);
                return error.TODO_unhandled_allocated_type;
            }

            fn unwrapSubject(self: *@This(), ref: NodeRef) NodeRef {
                const n = self.nodes.at(ref);
                return switch (n.kind) {
                    .parenthesized_expression => self.unwrapSubject(unwrapRef(n)),
                    .as_expression, .satisfies_expression => self.unwrapSubject(getPackedData(n).left),
                    else => ref,
                };
            }

            pub fn visit(self: *@This(), n: *const AstNode, ref: NodeRef) anyerror!void {
                switch (n.kind) {
                    .class_declaration, .class_expression, .get_accessor, .set_accessor, .parameter => {
                        const is_async = self.is_async_ctx;
                        defer self.is_async_ctx = is_async;
                        self.is_async_ctx = false;

                        try parser.forEachChild(self.nodes, n, self);
                    },
                    .function_declaration, .function_expression, .method_declaration, .arrow_function => {
                        const is_async = self.is_async_ctx;
                        defer self.is_async_ctx = is_async;
                        self.is_async_ctx = n.hasFlag(.@"async");

                        try parser.forEachChild(self.nodes, n, self);
                    },
                    .for_of_statement => {
                        const r = getPackedData(n).right;
                        const ty = try self.analyzer.evaluateType(try self.analyzer.getType(self.file, r), 1 << 0 | 1 << 30);
                        var emit_entries = false;
                        if (self.analyzer.getKindOfRef(ty) == .empty_object) {
                            emit_entries = true;
                        }
                        if (self.analyzer.getKindOfRef(ty) == .object_literal) {
                            emit_entries = true;
                            if (try self.analyzer.getKnownSymbolType("iterator")) |iter_sym_ty| {
                                const o = Analyzer.getSlice2(self.analyzer.types.at(ty), Analyzer.ObjectLiteralMember);
                                for (o) |*m| {
                                    self.analyzer.printTypeInfo(m.name);
                                    if (m.name == iter_sym_ty) {
                                        emit_entries = false;
                                        break;
                                    }
                                }
                            }
                        }

                        if (emit_entries) {
                            const clone = try self.nodes.push(n.*);
                            const e = try self.nodes.push(.{
                                .kind = .call_expression,
                                .data = toBinaryDataPtrRefs(
                                    try self.nodes.push(.{
                                        .kind = .property_access_expression,
                                        .data = toBinaryDataPtrRefs(try self.makeIdent("Object"), try self.makeIdent("entries")),
                                    }),
                                    r,
                                ),
                            });
                            self.nodes.at(clone).data = toBinaryDataPtrRefs(getPackedData(n).left, e);
                            try self.replacements.put(ref, clone);
                        }

                        try parser.forEachChild(self.nodes, n, self);
                    },
                    .as_expression => {
                        const d = getPackedData(n);

                        // TODO: just do a direct visit here after unwrapping parens
                        self.as_type_node = self.nodes.at(d.right);

                        try self.visit(self.nodes.at(d.left), d.left);
                    },
                    .throw_statement => {
                        const inner_ref = unwrapRef(n);
                        const inner = self.nodes.at(inner_ref);
                        if (inner.kind != .string_literal and inner.kind != .template_expression and inner.kind != .no_substitution_template_literal) {
                            return try parser.forEachChild(self.nodes, n, self);  
                        }

                        const z = try getAllocator().alloc(u8, "Error".len);
                        @memcpy(z, "Error");
                        const ident = try self.nodes.push(.{
                            .kind = .identifier,
                            .data = z.ptr,
                            .len = @intCast(z.len),
                        });

                        const n2 = try self.nodes.push(.{
                            .kind = .new_expression,
                            .data = toBinaryDataPtrRefs(ident, inner_ref),
                        });

                        const n3 = try self.nodes.push(n.*);
                        self.nodes.at(n3).data = @ptrFromInt(n2);

                        try self.replacements.put(ref, n3);
                    },
                    .is_expression => {
                        const d = getPackedData(n);
                        const l = try self.analyzer.getType(self.file, d.left);
                        if (l == @intFromEnum(Kind.any) or l == @intFromEnum(Kind.unknown)) {
                            const r = try self.analyzer.getType(self.file, d.right);
                            const sub = self.unwrapSubject(d.left);

                            const n2 = try self.generateIsExpression(sub, r);
                            try self.replacements.put(ref, try self.nodes.push(.{
                                .kind = .parenthesized_expression,
                                .data = @ptrFromInt(n2),
                            }));
                            return;
                        }

                        const r = try self.analyzer.getType(self.file, d.right);
                        if (l == r) {
                            try self.replacements.put(ref, try self.nodes.push(.{
                                .kind = .true_keyword,
                            }));
                            return;
                        }

                        if (!try self.analyzer.isAssignableTo(r, l)) {
                            try self.replacements.put(ref, try self.nodes.push(.{
                                .kind = .false_keyword,
                            }));
                            return;
                        }

                        var isSimpleType = false;
                        if (r == @intFromEnum(Kind.number) or r == @intFromEnum(Kind.string) or r == @intFromEnum(Kind.boolean) or r == @intFromEnum(Kind.null) or r == @intFromEnum(Kind.undefined) or r == @intFromEnum(Kind.symbol)) {
                            isSimpleType = true;
                        } else if (self.analyzer.getKindOfRef(r) == .function_literal) {
                            isSimpleType = true;
                        }

                        if (!isSimpleType and self.analyzer.getKindOfRef(l) == .@"union") {
                            const nt = try self.analyzer.excludeType(l, r);

                            if (nt == l) {
                                try self.replacements.put(ref, try self.nodes.push(.{
                                    .kind = .false_keyword,
                                }));
                                return;
                            }
                            
                            // TODO: add complexity heuristic
                            if (self.analyzer.getKindOfRef(nt) != .@"union") {
                                const sub = self.unwrapSubject(d.left);

                                const n2 = try self.generateIsExpression(sub, nt);
                                const n3 = try self.nodes.push(.{
                                    .kind = .parenthesized_expression,
                                    .data = @ptrFromInt(n2),
                                });
                                try self.replacements.put(ref, try self.nodes.push(.{
                                    .kind = .prefix_unary_expression,
                                    .data = toBinaryDataPtrRefs(@intFromEnum(SyntaxKind.exclamation_token), n3),
                                }));
                                return;
                            }
                        }

                        const sub = self.unwrapSubject(d.left);

                        const n2 = try self.generateIsExpression(sub, r);
                        try self.replacements.put(ref, try self.nodes.push(.{
                            .kind = .parenthesized_expression,
                            .data = @ptrFromInt(n2),
                        }));
                    },
                    .binary_expression => {
                        const operator = n.len;
                        if (operator == @intFromEnum(SyntaxKind.equals_equals_token) or operator == @intFromEnum(SyntaxKind.exclamation_equals_token)) {
                            const nt = if (operator == @intFromEnum(SyntaxKind.equals_equals_token)) 
                                @intFromEnum(SyntaxKind.equals_equals_equals_token) 
                            else 
                                @intFromEnum(SyntaxKind.exclamation_equals_equals_token); 
                            
                            const clone = try self.nodes.push(n.*);
                            self.nodes.at(clone).len = nt;
                            try self.replacements.put(ref, clone);

                            return try parser.forEachChild(self.nodes, n, self);
                        }
                        return try parser.forEachChild(self.nodes, n, self);
                    },
                    .call_expression => {
                        if (!self.is_async_ctx) {
                            self.as_type_node = null;
                            return try parser.forEachChild(self.nodes, n, self);
                        }

                        const as_node = self.as_type_node;
                        self.as_type_node = null;

                        if (as_node) |c| {
                            if (c.kind == .async_keyword) {
                                return try parser.forEachChild(self.nodes, n, self);
                            }

                            // TODO: as Promise...
                        }

                        const t = try self.analyzer.getType(self.file, ref);
                        const u = try self.analyzer.maybeUnwrapPromise(t);
                        if (t != u) {
                            const copy = try self.nodes.push(n.*);
                            const n2 = try self.nodes.push(.{
                                .kind = .await_expression,
                                .data = @ptrFromInt(copy),
                            });
                            try self.replacements.put(ref, n2);
                        }

                        return try parser.forEachChild(self.nodes, n, self);
                    },
                    else => {
                        try parser.forEachChild(self.nodes, n, self);
                    },
                }
            }
        };

        var r = std.AutoArrayHashMap(NodeRef, NodeRef).init(getAllocator());
        const a = try this.getTypeChecker();
        var v = Visitor{ .analyzer = a, .file = f, .nodes = &f.ast.nodes, .replacements = &r };
        try v.visit(f.ast.nodes.at(start), start);

        return r;
    }

    // TODO:
    // const a = 1
    // export default { a }
    // ---
    // declare const _default: {
    //     a: number;
    // };
    // export default _default;

    pub fn printDeclarationText(this: *@This(), file_id: u32, start: NodeRef) ![]const u8 {
        const f = this.getFileData(file_id);
        std.debug.assert(f.ast.nodes.at(start).kind == .source_file);

        try this.bindModule(f);

        const analyzer = try this.getTypeChecker();
        const printer = try analyzer.getPrinter();

        printer.imported_symbols = std.AutoArrayHashMap(u64, []const u8).init(this.allocator);

        const Converter = struct {
            file: *ParsedFileData,
            analyzer: *Analyzer,
            printer: *Analyzer.TypePrinter,

            pub fn tryTransformExternalAlias(self: *const @This(), abs_ref: u64) !bool {
                const f_id: u32 = @intCast(abs_ref >> 32);

                const f2 = self.analyzer.program.getFileData(f_id);

                const sym_ref: SymbolRef = @truncate(abs_ref);
                const external_sym = f2.binder.symbols.at(sym_ref);

                if (external_sym.hasFlag(.late_bound)) {
                    if (comptime is_debug) {
                        std.debug.print(" skipped converting late bound symbol: ", .{});
                        printSymbol(f2, sym_ref);
                    }
                    return true;
                }

                const ident = getIdentFromSymbol(f2.binder, sym_ref) orelse unreachable;
                const hash = getHashFromNode(ident);

                if (f2.binder.exports.type_symbols.get(hash) != null) {
                    const imported_path = f2.file_name orelse "";
                    const origin = self.file.file_name orelse "";
                    // LEAK
                    const spec = try std.fs.path.relativePosix(self.analyzer.allocator(), std.fs.path.dirname(origin) orelse "", imported_path);
                    const spec2 = try std.mem.concat(self.analyzer.allocator(), u8, &.{"./",std.mem.trimRight(u8, spec, ".ts")});

                    try self.printer.imported_symbols.?.put(abs_ref, spec2); // FIXME: use module spec :/
                    return true;
                }

                // the symbol might be imported by this external module
                if (external_sym.hasFlag(.imported)) {
                    if (external_sym.hasFlag(.namespace)) {
                        return error.TODO_transitive_imported_namespace;
                    }

                    const f3 = self.analyzer.program.getFileData(external_sym.getOrdinal());
                    const external_sym2 = f3.binder.symbols.at(external_sym.declaration);
                    // XXX: search for the symbol
                    if (try self.searchAmbientModules(f3, external_sym2, ident.extra_data2, external_sym.declaration, abs_ref)) |r| {
                        return r;
                    }

                    const imported_path = f3.file_name orelse "";
                    const origin = self.file.file_name orelse "";
                    // LEAK
                    const spec = try std.fs.path.relativePosix(self.analyzer.allocator(), std.fs.path.dirname(origin) orelse "", imported_path);
                    const spec2 = try std.mem.concat(self.analyzer.allocator(), u8, &.{"./",std.mem.trimRight(u8, spec, ".ts")});

                    try self.printer.imported_symbols.?.put(abs_ref, spec2); // FIXME: use module spec :/
                    return true;
                }

                // XXX: search for the symbol
                if (try self.searchAmbientModules(f2, external_sym, ident.extra_data2,sym_ref, abs_ref)) |r| {
                    return r;
                }

                return false;
            }

            fn searchAmbientModules(self: *const @This(), f2: *ParsedFileData, external_sym: *const parser.Symbol, sym_hash: u32, sym_ref: u32, abs_ref: u64) !?bool {
                var module_iter = f2.binder.ambient_modules.iterator();
                while (module_iter.next()) |module_entry| {
                    const sym = f2.binder.symbols.at(module_entry.value_ptr.*);
                    std.debug.assert(sym.hasFlag(.namespace));
                    const ns = f2.binder.namespaces.items[sym.binding];
                    const exports = ns.exports orelse continue;
                    _ = exports;

                    const sym_table = if (external_sym.hasFlag(.type)) ns.type_symbols else ns.symbols;
                    if (sym_table.get(sym_hash)) |s| {
                        if (s == sym_ref) {
                            if (self.analyzer.program.associated_directives.get(@intCast(abs_ref >> 32))) |d| {
                                var did_find = false;

                                for (self.printer.triple_slash_directives.items) |d2| {
                                    if (d2.kind == d.kind and strings.eql(d.value, d2.value)) {
                                        did_find = true;
                                        break;
                                    }
                                }

                                if (!did_find) {
                                    try self.printer.triple_slash_directives.append(d.*);
                                }
                            }
                            try self.printer.imported_symbols.?.put(abs_ref, ns.module_specifier.?);
                            return true;
                        }
                    }
                }

                return null;
            }

            fn _classLike(self: *const @This(), node: *const AstNode, t: Analyzer.TypeRef, exported: bool) !NodeRef {
                const d = getPackedData(node);
                const synthed_ref = try self.printer.toTypeNode(t);
                const synthed = self.printer.synthetic_nodes.at(synthed_ref);
                const copy = try self.printer.copyNodeFromRef(d.left, self.file.id);
                synthed.data = toBinaryDataPtrRefs(copy, getPackedData(synthed).right);
                synthed.flags |= @intFromEnum(NodeFlags.declare);
                if (exported) synthed.flags |= @intFromEnum(NodeFlags.@"export");

                return synthed_ref;
            }

            pub fn classLike(self: *const @This(), node: *const AstNode, ref: NodeRef) !NodeRef {
                const t = try self.analyzer.getType(self.file, ref);
                return self._classLike(node, t, false);
            }

            pub fn classLikeFromType(self: *const @This(), node: *const AstNode, t: Analyzer.TypeRef) !NodeRef {
                return self._classLike(node, t, true);
            }

            pub fn functionLikeFromType(self: *const @This(), node: *const AstNode, type_ref: Analyzer.TypeRef) !NodeRef {
                const synthed_ref = try self.printer.toTypeNode(type_ref);
                const synthed = self.printer.synthetic_nodes.at(synthed_ref);
                const copy = try self.printer.copyNodeFromRef(getPackedData(node).left, self.file.id);

                var flags: u20 = @intFromEnum(NodeFlags.declare);
                if (node.hasFlag(.@"export")) flags |= @intFromEnum(NodeFlags.@"export");

                var return_type = (getPackedData(synthed)).right;

                const t = self.analyzer.types.at(type_ref);
                std.debug.assert(t.getKind() == .function_literal or t.getKind() == .parameterized);
                if (t.getKind() == .function_literal) {
                    const followed = try self.analyzer.followInnerScopeAliases(t.slot3);
                    if (followed != t.slot3) {
                        return_type = try self.printer.toTypeNode(followed);
                    }
                } else if (t.getKind() == .parameterized) {
                    std.debug.assert(t.slot3 < @intFromEnum(Analyzer.Kind.false));
                    const inner = self.analyzer.types.at(t.slot3);
                    std.debug.assert(inner.getKind() == .function_literal); // TODO: could be recursive
                    const followed = try self.analyzer.followInnerScopeAliases(inner.slot3);
                    if (followed != inner.slot3) {
                        const prev_state = self.analyzer.saveState();
                        defer self.analyzer.restoreState(prev_state);

                        const parameterized = try self.analyzer.createParameterizedType(Analyzer.getSlice2(t, Analyzer.TypeRef), followed);
                        return_type = try self.printer.toTypeNode(parameterized);
                    }
                }

                return self.printer.synthetic_nodes.push(.{
                    .kind = .function_declaration,
                    .flags = flags,
                    .data = toBinaryDataPtrRefs(copy, (getPackedData(synthed)).left),
                    .extra_data = synthed.len, // type params
                    .extra_data2 = return_type,
                });
            }
        };

        const converter = Converter{
            .file = f,
            .analyzer = analyzer,
            .printer = printer,
        };

        // TODO
        // export type RegExp = 'ok'
        // export const y = /aaa/

        // export type RegExp = 'ok';
        // export declare const y: globalThis.RegExp;

        var to_keep = std.AutoHashMap(u64, bool).init(this.allocator);
        defer to_keep.deinit();

        var external_set = std.AutoHashMap(u64, bool).init(this.allocator);
        defer external_set.deinit();

        // file nodes -> synthetic nodes
        var replacements = std.AutoArrayHashMap(NodeRef, NodeRef).init(this.allocator);
        defer replacements.deinit();

        var named_imports = std.ArrayList(struct { NodeRef, NodeRef }).init(this.allocator);
        defer named_imports.deinit();

        var nodes_to_keep = std.AutoHashMap(NodeRef, bool).init(this.allocator);
        defer nodes_to_keep.deinit();


        // TODO: don't iterate over all statements on the first pass
        // Re-use the results from binding instead
        //
        // var imports_iter = f.binder.imports.iterator();
        // while (imports_iter.next()) |entry| {
        // }

        var exported_syms = f.binder.exports.symbols.iterator();
        while (exported_syms.next()) |entry| {
            const s = f.binder.symbols.at(entry.value_ptr.*);
            if (s.declaration == 0) return error.MissingExportedDecl;

            const n = f.ast.nodes.at(s.declaration);
            if (n.kind != .function_declaration) continue; // TODO

            const d = getPackedData(n);
            if (d.left == 0) return error.TODO_default_export;

            try to_keep.put((@as(u64, file_id) << 32) | f.ast.nodes.at(d.left).extra_data, true);

            const type_ref = try analyzer.getTypeOfSymbol(f, entry.value_ptr.*);
            try analyzer.gatherReferencedSymbols2(&to_keep, type_ref, f.id, &external_set);

            defer external_set.clearAndFree();

            var external_set_iter = external_set.iterator();
            while (external_set_iter.next()) |external_entry| {
                _ = try converter.tryTransformExternalAlias(external_entry.key_ptr.*);
            }

            const t = analyzer.types.at(type_ref);
            switch (t.getKind()) {
                .function_literal, .parameterized => {
                    try replacements.put(s.declaration, try converter.functionLikeFromType(n, type_ref));
                },
                .object_literal => {
                    var list = NodeList.init(&printer.synthetic_nodes);
                    for (Analyzer.getSlice2(t, Analyzer.ObjectLiteralMember)) |*el| {
                        if (el.kind != .call_signature) continue;

                        list.appendRef(try converter.functionLikeFromType(n, try el.getType(analyzer)));
                    }

                    std.debug.assert(list.head != 0);
                    try replacements.put(s.declaration, list.head);
                },
                else => return error.UnknownTypeKind,
            }
        }

        var exported_types = f.binder.exports.type_symbols.iterator();
        while (exported_types.next()) |entry| {
            const sym_ref = entry.value_ptr.*;
            const s = f.binder.symbols.at(sym_ref);
            if (s.declaration == 0) return error.MissingExportedDecl;

            const n = f.ast.nodes.at(s.declaration);
            if (n.kind == .class_declaration) continue; // TODO

           // const d = getPackedData(n);
           // try to_keep.put((@as(u64, file_id) << 32) | f.ast.nodes.at(d.left).extra_data, true);

            // const t = try analyzer.getTypeOfSymbol(f, sym_ref);
            try analyzer.gatherReferencedSymbolsFromAst(&to_keep, f, s.declaration);

            // if (n.kind == .type_alias_declaration and d.right == 0) {
            //     const ident = try printer.copyNodeFromRef(d.left, @intCast(f.id));
            //     var copy = n.*;
            //     copy.len = try printer.toTypeNode(t);
            //     copy.data = toBinaryDataPtrRefs(ident, 0);
            //     const n2 = try printer.synthetic_nodes.push(copy);
            //     try replacements.put(s.declaration, n2);
            // }
        }

        const first_statement = maybeUnwrapRef(f.ast.nodes.at(start)) orelse 0;

        var iter = NodeIterator.init(&f.ast.nodes, first_statement);
        while (iter.nextPair()) |p| {
            const n: *const AstNode = p[0];
            const ref = p[1];

            // TODO: always include ambient modifications
            if (!n.hasFlag(.@"export")) {
                if (n.kind == .import_declaration) {
                    const d = getPackedData(n);
                    const clause = f.ast.nodes.at(d.left);
                    const clause_data = getPackedData(clause);
                    if (clause_data.right == 0) continue;

                    const namespace_or_named_imports = f.ast.nodes.at(clause_data.right);
                    if (namespace_or_named_imports.kind == .named_imports) {
                        try named_imports.append(.{ ref, clause_data.right });
                    }
                } else if (n.kind == .export_declaration) {
                    try nodes_to_keep.put(ref, true);
                }
                continue;
            }

            switch (n.kind) {
                .type_alias_declaration, .interface_declaration => {
                    // const d = getPackedData(n);
                    // try to_keep.put((@as(u64, file_id) << 32) | f.ast.nodes.at(d.left).extra_data, true);
                    // const t = try analyzer.getType(f, ref);
                    // try analyzer.gatherReferencedSymbols(&to_keep, t, f.id);
                },
                .variable_statement => {
                    var decls_iter = NodeIterator.init(&f.ast.nodes, unwrapRef(n));
                    var l = NodeList.init(&printer.synthetic_nodes);
                    while (decls_iter.nextPair()) |pair| {
                        const d = getPackedData(pair[0]);
                        const binding = f.ast.nodes.at(d.left);

                        // tsc "unrolls" the binding pattern
                        //
                        // example: `export declare const { z, z2 }: { z: number, z2: boolean }`
                        // -------> `export declare const z: number, z2: boolean;`
                        if (binding.kind != .identifier) {

                            return error.TODO_exported_variable_binding;
                        }

                        try to_keep.put((@as(u64, file_id) << 32) | binding.extra_data, true);

                        var t = try analyzer.getType(f, pair[1]);
                        if (pair[0].len == 0) {
                            // inferred, try to simplify
                            t = try analyzer.maybeSimplifyType(t);
                        }
                        try analyzer.gatherReferencedSymbols2(&to_keep, t, f.id, &external_set);

                        defer external_set.clearAndFree();

                        var external_set_iter = external_set.iterator();
                        while (external_set_iter.next()) |external_entry| {
                            const did_convert = try converter.tryTransformExternalAlias(external_entry.key_ptr.*);
                            if (!did_convert) {
                                t = try analyzer.maybeResolveAlias(t);
                            } else {
                                printer.removeCacheEntry(t);
                            }
                        }

                        const synthed_ref = try printer.toTypeNode(t);
                        const copy = try printer.copyNodeFromRef(d.left, @intCast(f.id));

                        try l.append(.{
                            .kind = .variable_declaration,
                            .data = toBinaryDataPtrRefs(copy, 0),
                            .len = synthed_ref,
                        });
                    }
                    const n2 = try printer.synthetic_nodes.push(.{
                        .kind = .variable_statement,
                        .flags = @intFromEnum(NodeFlags.declare) | n.flags,
                        .data = @ptrFromInt(l.head),
                    });
                    try replacements.put(ref, n2);
                },
                // .function_declaration => {
                //     const d = getPackedData(n);
                //     if (d.left == 0) return error.TODO_default_export;

                //     try to_keep.put((@as(u64, file_id) << 32) | f.ast.nodes.at(d.left).extra_data, true);

                //     const t = try analyzer.getType(f, ref);
                //     try analyzer.gatherReferencedSymbols(&to_keep, t, f.id);

                //     const synthed_ref = try printer.toTypeNode(t);
                //     const synthed = printer.synthetic_nodes.at(synthed_ref);
                //     const copy = try printer.copyNodeFromRef(d.left, @intCast(f.id));

                //     const n2 = try printer.synthetic_nodes.push(.{
                //         .kind = .function_declaration,
                //         .flags = @intFromEnum(NodeFlags.declare) | @intFromEnum(NodeFlags.@"export"),
                //         .data = toBinaryDataPtrRefs(copy, (getPackedData(synthed)).left),
                //         .extra_data = synthed.len, // type params
                //         .extra_data2 = (getPackedData(synthed)).right, // return type
                //     });

                //     try replacements.put(ref, n2);
                // },
                .class_declaration => {
                    const d = getPackedData(n);
                    if (d.left == 0) return error.TODO_default_export;

                    try to_keep.put((@as(u64, file_id) << 32) | f.ast.nodes.at(d.left).extra_data, true);

                    const t = try analyzer.getType(f, ref);
                    try analyzer.gatherReferencedSymbols(&to_keep, t, f.id);

                    try replacements.put(ref, try converter.classLikeFromType(n, t));
                },
                else => {},
            }
        }

        var to_keep_iter = to_keep.iterator();
        while (to_keep_iter.next()) |entry| {
            const f_id: u32 = @intCast(entry.key_ptr.* >> 32);
            if (f_id != file_id) continue;

            const sym_ref: SymbolRef = @truncate(entry.key_ptr.*);
            const sym = f.binder.symbols.at(sym_ref);
            if (hasSymbolFlag(sym, .late_bound)) continue;

            if (hasSymbolFlag(sym, .imported)) {
                const ident = getIdentFromSymbol(f.binder, sym_ref) orelse unreachable;
                // std.debug.print("IMPORT {s} -> {}\n", .{ getSlice(ident, u8), ident.next });
                try nodes_to_keep.put(ident.next, true);
            } else if (sym.declaration != 0) {
                try nodes_to_keep.put(sym.declaration, true);

                // don't add overloads, they're already handled
                if (f.ast.nodes.at(sym.declaration).kind == .function_declaration) continue;

                var next = sym.next;
                while (next != 0) {
                    const s = f.binder.symbols.at(next);
                    if (s.declaration != 0) try nodes_to_keep.put(s.declaration, true);
                    next = s.next;
                }
            }
        }

        // Prune import decls
        // TODO: prune decls w/ both default import and named/namespace imports
        for (named_imports.items) |pair| {
            if (!(nodes_to_keep.get(pair[0]) orelse false)) continue;

            const r = f.ast.nodes.at(pair[1]);
            var l = NodeList.init(&printer.synthetic_nodes);
            var did_change = false;

            const count = printer.synthetic_nodes.count;
            const local_count = printer.synthetic_nodes.local_count;

            defer {
                if (!did_change) {
                    printer.synthetic_nodes.count = count;
                    printer.synthetic_nodes.local_count = local_count;
                }
            }

            var iter2 = NodeIterator.init(&f.ast.nodes, maybeUnwrapRef(r) orelse 0);

            while (iter2.nextPair()) |p| {
                const d = getPackedData(p[0]);
                const binding = if (d.right != 0) d.right else d.left;
                const ident = f.ast.nodes.at(binding);
                if (to_keep.get((@as(u64, f.id) << 32) | ident.extra_data) orelse false) {
                    try l.append(.{
                        .kind = .external_node,
                        .data = &f.ast,
                        .len = p[1],
                    });
                } else {
                    did_change = true;
                }
            }

            if (!did_change) continue;

            const decl = f.ast.nodes.at(pair[0]);
            const d = getPackedData(decl);
            const right = try printer.synthetic_nodes.push(.{
                .kind = .external_node,
                .data = &f.ast,
                .len = d.right,
            });

            const imports = try printer.synthetic_nodes.push(.{
                .kind = .named_imports,
                .data = @ptrFromInt(l.head),
            });

            const clause = try printer.synthetic_nodes.push(.{
                .kind = .import_clause,
                .data = toBinaryDataPtrRefs(0, imports), // FIXME: this removes default import
            });

            const replacement = try printer.synthetic_nodes.push(.{
                .kind = .import_declaration,
                .flags = decl.flags,
                .data = toBinaryDataPtrRefs(clause, right),
            });

            try replacements.put(pair[0], replacement);
        }

        var statements = NodeList.init(&printer.synthetic_nodes);
        var iter2 = NodeIterator.init(&f.ast.nodes, first_statement);
        while (iter2.nextPair()) |p| {
            const should_keep = nodes_to_keep.get(p[1]) orelse false;
            if (!should_keep) {
                // XXX: check for variable statment
                if (p[0].kind != .variable_statement) continue;
                if (!(nodes_to_keep.get(unwrapRef(p[0])) orelse false)) continue; // FIXME: handle multiple decls
            }

            if (replacements.get(p[1])) |r| {
                var next = r;
                while (next != 0) {
                    statements.appendRef(next);
                    next = printer.synthetic_nodes.at(next).next;
                }
            } else {
                if (p[0].kind == .class_declaration) {
                    statements.appendRef(try converter.classLike(p[0], p[1]));
                    continue;
                }

                try statements.append(.{
                    .kind = .external_node,
                    .data = &f.ast,
                    .len = p[1],
                });
            }
        }

        // const start_print_time = std.time.microTimestamp();

        const s = try parser.printInMemoryWithTypes(.{
            .source_name = f.file_name,
            .nodes = printer.synthetic_nodes,
            .decorators = f.ast.decorators,
            .source = f.ast.source,
            .positions = f.ast.positions,
            .triple_slash_directives = printer.triple_slash_directives.items,
        }, .{
            .kind = .source_file,
            .data = @ptrFromInt(statements.head),
        });

        // std.debug.print("  print time: {d:.3}\n", .{std.time.microTimestamp() - start_print_time});

        return s;
    }
};

const ModuleList = packed struct {
    const ListType = std.ArrayListUnmanaged(FileRef);
    const max_inline_elements = 7;

    buf: [max_inline_elements]FileRef = undefined,
    count: u31 = 0,
    is_allocated: bool = false,

    comptime {
        if (@sizeOf(ModuleList) != 32) {
            @compileError("Expected 32 bytes");
        }
    }

    pub fn deinit(this: *@This(), allocator: std.mem.Allocator) void {
        if (!this.is_allocated) return;

        return allocator.free(this.getAllocatedSlice());
    }

    fn appendAllocated(this: *@This(), allocator: std.mem.Allocator, value: FileRef) !void {
        std.debug.assert(this.is_allocated);

        var list = ListType{
            .items = this.getAllocatedSlice(),
            .capacity = this.buf[2],
        };

        if (list.capacity > list.items.len) {
            this.count += 1;
            return list.appendAssumeCapacity(value);
        }

        try list.append(allocator, value);
        const items_ptr: *u64 = @alignCast(@ptrCast(&this.buf));
        items_ptr.* = @intFromPtr(list.items.ptr);
        this.buf[2] = @intCast(list.capacity);
        this.count += 1;
    }

    inline fn getAllocatedSlice(this: *const @This()) []FileRef {
        const ptr: *u64 = @constCast(@alignCast(@ptrCast(&this.buf)));
        return @as([*]FileRef, @ptrFromInt(ptr.*))[0..this.count];
    }

    pub fn getSlice(this: *const @This()) []const FileRef {
        if (this.count == 0) return &.{};

        if (this.is_allocated) {
            return this.getAllocatedSlice();
        }

        return this.buf[0..this.count];
    }

    fn allocate(this: *@This(), allocator: std.mem.Allocator, capacity: u32) ![]FileRef {
        std.debug.assert(!this.is_allocated);

        this.is_allocated = true;
        const buf = try allocator.alloc(FileRef, capacity);
        @memcpy(buf, this.buf[0..this.count]);

        return buf;
    }

    pub fn append(this: *@This(), allocator: std.mem.Allocator, ref: FileRef) !void {
        if (this.count < max_inline_elements) {
            this.buf[this.count] = ref;
            this.count += 1;
            return;
        }

        if (this.count == max_inline_elements) {
            const capacity: u32 = 16;
            const buf = try this.allocate(allocator, capacity);
            const items_ptr: *u64 = @alignCast(@ptrCast(&this.buf));
            items_ptr.* = @intFromPtr(buf.ptr);
            this.buf[2] = capacity;
        }

        return this.appendAllocated(allocator, ref);
    }
};

const FileRef = u32;
const ParsedPair = struct { *parser.ParsedFile, FileRef };

const ModuleLinkage = struct {
    // aliasing to make moving to later zig versions a bit easier
    const ImportMap = std.AutoArrayHashMapUnmanaged(u64, FileRef); // spec hash -> module id
    const InverseImportMap = std.AutoArrayHashMapUnmanaged(FileRef, []const u8);

    const PendingDependents = std.ArrayListUnmanaged(FileRef);
    const CachedExports = std.AutoArrayHashMapUnmanaged(NodeSymbolHash, AbsoluteSymbolRef);

    import_map: ImportMap = ImportMap{},
    inverse_import_map: InverseImportMap = InverseImportMap{},

    pub fn addImport(this: *@This(), allocator: std.mem.Allocator, spec: []const u8, module_id: FileRef) !void {
        try this.import_map.put(allocator, getHash(spec), module_id);
        try this.inverse_import_map.put(allocator, module_id, spec);
    }

};

pub const ParsedFileData = struct {
    const CachedSymbolTypes = std.AutoArrayHashMapUnmanaged(parser.SymbolRef, Analyzer.TypeRef);
    const Errors = std.ArrayListUnmanaged(FileDiagnostic);

    id: FileRef = 0,
    file_name: ?[]const u8 = null,

    ast: parser.AstData,
    binder: *const parser.Binder,

    // Used by the analyzer
    // TODO (perf): maybe make symbols bigger and use a slot to cache
    cached_symbol_types: CachedSymbolTypes,

    is_lib: bool = false,
    is_declaration: bool = false,
    did_analyze: bool = false,
    did_top_level_cfa: bool = false,
    did_resolve_imports: bool = false,
    did_bind_imports: bool = false,
    did_bind_aliased_exports: bool = false,

    // Module data
    import_map: std.AutoArrayHashMap(u64, FileRef),
    unresolved_imports: std.ArrayList(u64),
    linkage: *ModuleLinkage,

    cfa_state: std.ArrayList(*CFAState),

    // Used for semantic errors, e.g. `break` without being in a breakable context
    // Exits without a flow label discard the environment i.e. assume it worked
    errors: Errors = .{},

    pub fn init(_allocator: std.mem.Allocator, ast: parser.AstData, binder: *const parser.Binder, linkage: *ModuleLinkage) @This() {
        return .{
            .ast = ast,
            .binder = binder,
            .cached_symbol_types = CachedSymbolTypes{},
            .import_map = std.AutoArrayHashMap(u64, FileRef).init(_allocator),
            .unresolved_imports = std.ArrayList(u64).init(_allocator),
            .cfa_state = std.ArrayList(*CFAState).init(_allocator),
            .linkage = linkage,
        };
    }

    pub fn deinit(this: @This()) void {
        _ = this;
        @panic("todo");
    }

    pub fn initFromParsedFile(file: *parser.ParsedFile, linkage: *ModuleLinkage) @This() {
        var this = @This().init(getAllocator(), file.ast, &file.binder, linkage);
        this.file_name = file.source_name;
        this.is_lib = file.is_lib;

        return this;
    }
};

const AbsoluteSymbolRef = struct { file_id: FileRef, ref: parser.SymbolRef };

const Ambient = struct {
    const GlobalRef = u32;
    const Global = struct {
        type: Analyzer.TypeRef = 0,
        symbols: std.ArrayListUnmanaged(AbsoluteSymbolRef) = std.ArrayListUnmanaged(AbsoluteSymbolRef){},
    };

    modules: std.AutoArrayHashMap(u64, GlobalRef),
    // TODO: module patterns

    globals: std.AutoArrayHashMap(parser.NodeSymbolHash, GlobalRef),
    global_types: std.AutoArrayHashMap(parser.NodeSymbolHash, GlobalRef),

    globals_allocator: parser.BumpAllocator(Global),

    pub fn init(_allocator: std.mem.Allocator) @This() {
        var globals_allocator = parser.BumpAllocator(Global).init(_allocator, std.heap.page_allocator);
        globals_allocator.preAlloc() catch unreachable;

        return .{
            .globals_allocator = globals_allocator,
            .modules = std.AutoArrayHashMap(u64, GlobalRef).init(_allocator),
            .globals = std.AutoArrayHashMap(parser.NodeSymbolHash, GlobalRef).init(_allocator),
            .global_types = std.AutoArrayHashMap(parser.NodeSymbolHash, GlobalRef).init(_allocator),
        };
    }

    pub fn deinit(this: @This()) void {
        this.modules.deinit();
        this.globals.deinit();
        this.global_types.deinit();
    }

    pub fn getGlobalSymbol(this: *const @This(), comptime name: []const u8) ?*Global {
        const hashed: u32 = comptime @truncate(std.hash.Wyhash.hash(0, name));
        const index = this.globals.get(hashed) orelse return null;

        return this.globals_allocator.at(index);
    }

    pub fn getGlobalTypeSymbol(this: *const @This(), comptime name: []const u8) ?*Global {
        return this.globals_allocator.at(this.getGlobalTypeSymbolRef(name) orelse return null);
    }

    pub fn getGlobalTypeSymbolRef(this: *const @This(), comptime name: []const u8) ?GlobalRef {
        const hashed: u32 = comptime @truncate(std.hash.Wyhash.hash(0, name));
        const index = this.global_types.get(hashed) orelse return null;

        return index;
    }
};

// CFA operates over subroutines (Module, Function, etc.)
// Narrowing from CFA _can_ transfer to arrow functions and class/function expressions
// But it won't transfer for function/class declarations due to hoisting
//
// Certain things will cause CFA to be "lost" for a given non-constant symbol:
// * yield
// * await (this is practically a yield afterall)
// * new/call expressions
//
// This is only true if the symbol is truly mutable w.r.t the yield point. A locally
// mutable symbol is effectively constant when yielding.

// TypeScript doesn't track implications e.g.
//
// function f(x: unknown, y: unknown) {
//     if (typeof x === 'number' || typeof y === 'string') {
//         if (typeof y === 'string') return;
//         x // TSC shows `unknown`
//     }
// }
//
// Another example:
//
// declare function t(): never
// function f(x: string | number) {
//     typeof x === 'number' ? x : t()
//     x // `string | number`
// }

// Assignments under a type narrowing exclude the narrowed type for subsequent analysis:
//
// function f(x: 'purple' | number) {
//     if (x === 'purple') x = 1;
//     x // `number`
// }
//
// Assignments seem to reset the type back to the declaration if the type was narrowed under assertion.
// We don't need to replicate this behavior.

const is_debug = @import("builtin").mode == .Debug;

const CFAState = struct {
    const TypeRef = Analyzer.TypeRef;

    iter: NodeIterator,
    types: std.AutoArrayHashMap(SymbolRef, TypeRef),
    termination_types: std.AutoArrayHashMap(SymbolRef, TypeRef), // Types that should be used if we do terminate execution
    continuation_types: std.AutoArrayHashMap(SymbolRef, TypeRef), // Types that should be used if we do not terminate execution
    is_unconditional: bool = true, // Type narrowing during unconditional execution is applied to the prior state
    is_branch: bool = false,
    is_expression: bool = false,

    branch: ?NodeRef = null,
    branch_continuation_types: ?std.AutoArrayHashMap(SymbolRef, std.ArrayList(TypeRef)) = null,
    tmp_types: ?std.AutoArrayHashMap(SymbolRef, TypeRef) = null,
    tmp_start: ?NodeRef = null,
    switch_subject: ?NodeRef = null,

    pub fn init(alloc: std.mem.Allocator, iter: NodeIterator) !*@This() {
        var p = try alloc.create(@This());
        p.iter = iter;
        p.types = std.AutoArrayHashMap(SymbolRef, TypeRef).init(alloc);
        p.termination_types = std.AutoArrayHashMap(SymbolRef, TypeRef).init(alloc);
        p.continuation_types = std.AutoArrayHashMap(SymbolRef, TypeRef).init(alloc);
        p.is_unconditional = true;
        p.is_branch = false;
        p.is_expression = false;
        p.branch = null;
        p.branch_continuation_types = null;
        p.tmp_types = null;
        p.tmp_start = null;
        p.switch_subject = null;        

        return p;
    }

    pub fn deinit(this: *@This()) void {
        const a = this.types.allocator;
        this.types.deinit();
        this.termination_types.deinit();
        this.continuation_types.deinit();
        // if (this.branch_continuation_types) |b| {
        //     b.deinit();
        // }
        a.destroy(this);
    }
};

pub const Analyzer = struct {
    pub const Kind = enum(u32) {
        nil = 0, // placeholder/invalid type

        string_literal,
        template_literal,
        number_literal,
        object_literal,
        bigint_literal,
        function_literal,
        symbol_literal, // `unique symbol`

        class,
        array,

        type_parameter,

        named_tuple_element,

        @"union",
        intersection,
        tuple,
        mapped,
        indexed, // T[K]
        keyof,
        query,
        alias, // Used for better `.d.ts` emits

        // Conditional types are always reduced if possible
        conditional, // T extends ...
        parameterized,

        predicate,

        module_namespace, // Special case of an object literal

        err,

        // Intrinsics
        false = 1 << 31,
        true,
        null,
        undefined,
        number,
        string,
        bigint,
        object,
        boolean, // Not technically intrinsic, this is equivalent to `true | false`
        function,
        symbol,

        empty_object,
        empty_string,
        empty_tuple,
        empty_element, // Similar to the "hole" in [1,,2]

        // XXX: `typeof globalThis` is just a really big object literal type
        // But the current impl. doesn't construct a map for object literals
        // So we'll do this for now
        global_this,

        void,

        this,

        any,
        never,
        unknown,

        // User-accessible intrinsics (order matters for `isCallableIntrinsic`)
        string_uppercase,
        string_lowercase,
        string_capitalize,
        string_uncapitalize,
        no_infer,
        this_type_marker, // TODO: typescript doesn't use `intrinsic` to mark this decl

        // Lower bits reference the type parameter id
        type_parameter_ref = 0b101 << 29,

        // 30 bit two's complement literals are treated as intrinsic
        zero = 0b11 << 30,
    };

    const Flags = enum(u24) {
        has_promise = 1 << 0,
        has_infer = 1 << 1,
        parameter_decl = 1 << 2, // XXX: helper for getting types from constructor
        allocated_list = 1 << 3,

        class_alias = 1 << 4,
        instance_alias = 1 << 5,

        abstract = 1 << 6,
        constructor = 1 << 7, // used for signatures and function literals
        spread = 1 << 8,
        infer_node = 1 << 9,

        parameterized = 1 << 10,
        has_this_type = 1 << 11,

        global = 1 << 12,

        lazy = 1 << 13,

        readonly = 1 << 16,
        optional = 1 << 17,
        non_null = 1 << 18,
    };

    // Goes into same slot as other type flags, be careful not to overlap
    const UnionFlags = enum(u24) {
        has_object_literal = 1 << 4,
        has_number_literal = 1 << 5,
        has_string_literal = 1 << 6,
        has_symbol_literal = 1 << 7,

        // shared with `Flags`
        parameterized = 1 << 10,

        // intrinsics
        has_undefined = 1 << 15,
        has_object = 1 << 19,
        has_boolean = 1 << 20,
        has_string = 1 << 21,
        has_number = 1 << 22,
        has_symbol = 1 << 23,
    };

    const ObjectLiteralFlags = enum(u24) {
        // shared with `Flags`
        parameterized = 1 << 10,

        lazy = 1 << 13,

        // synthetic type produced by FlowTyper
        refined = 1 << 23,

        pub inline fn isRefined(t: *const Type) bool {
            return (t.flags & @intFromEnum(ObjectLiteralFlags.refined)) == @intFromEnum(ObjectLiteralFlags.refined);
        }
    };

    const Type = packed struct {
        slot0: u32 = 0,
        slot1: u32 = 0,
        slot2: u32 = 0,
        slot3: u32 = 0,
        slot4: u32 = 0,
        slot5: u32 = 0,
        slot6: u32 = 0,

        // Placed below the data slots for easier alignment
        kind: u8,
        flags: u24 = 0,

        comptime {
            if (@sizeOf(Type) != 32) @compileError("Invalid type struct size");
        }

        pub inline fn init(comptime kind: Kind) @This() {
            if (comptime @intFromEnum(kind) >= @intFromEnum(Kind.false)) {
                @compileError("Cannot allocate type with kind >= `Kind.false`");
            }

            return .{ .kind = @intCast(@intFromEnum(kind)) };
        }

        pub inline fn getKind(this: *const @This()) Kind {
            return @enumFromInt(this.kind);
        }

        pub inline fn hasFlag(this: *const Type, flag: Flags) bool {
            return (this.flags & @intFromEnum(flag)) == @intFromEnum(flag);
        }

        pub const Class = struct {
            instance_type: TypeRef,
            static_type: TypeRef,
            constructor: TypeRef,
            base_class: TypeRef,

            pub inline fn fromType(t: *const Type) *align(@alignOf(Type)) const @This() {
                return @ptrCast(t);
            }
        };

        pub const Alias = struct {
            slot0: u32,
            slot1: u32,
            slot2: u32,
            file_id: u32,
            sym_ref: SymbolRef,
            next: u32,
            evaluated: u32,
            _: u32,

            pub inline fn getArgs(this: *const @This()) []TypeRef {
                return getSlice2(@ptrCast(this), TypeRef);
            }

            pub inline fn fromType(t: *const Type) *const @This() {
                return @ptrCast(t);
            }
        };
    };

    pub const TypeRef = u32;

    // All `.d.ts` files merge w/ ambient declarations but
    // only files loaded via directives expose changes.

    const Variance = enum {
        invariant,
        covariant,
        contravariant,

        pub fn invert(v: Variance) Variance {
            return switch (v) {
                .invariant => .invariant,
                .covariant => .contravariant,
                .contravariant => .covariant,
            };
        }
    };

    program: *Program,

    synthetic_strings: std.ArrayList([]const u8),

    types: parser.BumpAllocator(Type),
    canonical_types: std.AutoArrayHashMap(u64, TypeRef),

    type_args: std.AutoArrayHashMap(TypeRef, TypeRef),
    type_registers: [16]TypeRef = [_]TypeRef{0} ** 16,
    contextual_this_type: TypeRef = 0, // Used to evaluate `this` in types (not expressions)

    inferred_type_params: ?std.AutoArrayHashMapUnmanaged(TypeRef, TypeRef) = null,
    inferrence_ctx: ?TypeRef = null, // points to a type to guide inferrence

    is_const_context: bool = false, // Forced `const` context e.g. `as const` or `const T`
    is_const_variable_context: bool = false, // Implicit `const` context e.g. `const x = 1`

    // Used when analyzing parameterized types
    // We use this state to compute type parameter variance
    //
    // TODO: Type params used to constrain other type params take on the variance on the constrained type
    variance: Variance = .covariant,

    printer: ?*TypePrinter = null,

    // Used to detect recursion
    active_types: std.AutoArrayHashMapUnmanaged(u64, bool),

    current_node: if (is_debug) ?struct { *ParsedFileData, NodeRef } else void = if (is_debug) null else {},
   
    current_flow: ?*FlowTyper = null,
    label_pool: std.heap.MemoryPool(FlowTyper.Label),


    pub fn init(program: *Program) !@This() {
        var types = BumpAllocator(Type).init(program.allocator, std.heap.page_allocator);
        try types.preAlloc();
        _ = try types.push(.{ .kind = @intFromEnum(Kind.nil) });

        return .{
            .program = program,
            .types = types,
            .canonical_types = std.AutoArrayHashMap(u64, TypeRef).init(program.allocator),
            .synthetic_strings = std.ArrayList([]const u8).init(program.allocator),
            .type_args = std.AutoArrayHashMap(TypeRef, TypeRef).init(program.allocator),
            .active_types = std.AutoArrayHashMapUnmanaged(u64, bool){},
            .label_pool = std.heap.MemoryPool(FlowTyper.Label).init(program.allocator),
        };
    }

    pub fn getPrinter(this: *@This()) !*TypePrinter {
        if (this.printer) |p| return p;

        const p = try TypePrinter.init(this);
        this.printer = p;
        return p;
    }

    fn getAnalyzedFile(this: *@This(), id: u32) !*ParsedFileData {
        return this.program.getBoundFile(id);
    }

    const State = struct {
        count: u32,
        local_count: u16,
    };

    inline fn saveState(this: *const @This()) State {
        return .{
            .count = this.types.count,
            .local_count = this.types.local_count,
        };
    }

    inline fn restoreState(this: *@This(), state: State) void {
        this.types.count = state.count;
        this.types.local_count = state.local_count;
    }

    const IntrinsicTypeAliases = ComptimeStringMap(Kind, .{
        .{ "NoInfer", .no_infer },
        .{ "Lowercase", .string_lowercase },
        .{ "Uppercase", .string_uppercase },
        .{ "Capitalize", .string_capitalize },
        .{ "Uncapitalize", .string_uncapitalize },
    });

    const TypeList = struct {
        buf: [4]TypeRef = undefined,
        count: u32 = 0,
        flags: u24 = 0,

        pub fn fromSlice(analyzer: *Analyzer, slice: []const TypeRef) !@This() {
            if (slice.len > 4) return error.TODO;

            var this = @This(){ .count = @intCast(slice.len) };
            for (slice, 0..) |t, i| {
                if (this.flags == 0 and analyzer.isParameterizedRef(t)) {
                    this.flags |= @intFromEnum(Flags.parameterized);
                }
                this.buf[i] = t;
            }

            return this;
        }

        pub fn deinit(this: *@This(), _allocator: std.mem.Allocator) void {
            if (!this.isAllocated()) return;

            return _allocator.free(this.getAllocatedSlice());
        }

        fn appendAllocated(this: *@This(), analyzer: *Analyzer, value: TypeRef) !void {
            std.debug.assert(this.isAllocated());

            var list = std.ArrayListUnmanaged(TypeRef){
                .items = this.getAllocatedSlice(),
                .capacity = this.buf[2],
            };

            if (list.capacity > list.items.len) {
                this.count += 1;
                return list.appendAssumeCapacity(value);
            }

            try list.append(analyzer.allocator(), value);
            const items_ptr: *u64 = @alignCast(@ptrCast(&this.buf));
            items_ptr.* = @intFromPtr(list.items.ptr);
            this.buf[2] = @intCast(list.capacity);
            this.count += 1;
        }

        // TypeList becomes immutable after this
        fn shrinkToFit(this: *@This(), _allocator: std.mem.Allocator, slice: []TypeRef) []const TypeRef {
            std.debug.assert(this.isAllocated());

            var list = std.ArrayListUnmanaged(TypeRef){
                .items = this.getAllocatedSlice(),
                .capacity = this.buf[2],
            };

            list.shrinkAndFree(_allocator, slice.len);

            return list.items;
        }

        inline fn getAllocatedSlice(this: *const @This()) []TypeRef {
            const ptr: *u64 = @constCast(@alignCast(@ptrCast(&this.buf)));
            return @as([*]TypeRef, @ptrFromInt(ptr.*))[0..this.count];
        }

        pub fn getSlice(this: *const @This()) []const TypeRef {
            if (this.count == 0) return &.{};

            if (this.isAllocated()) {
                return this.getAllocatedSlice();
            }

            return this.buf[0..this.count];
        }

        inline fn isAllocated(this: *const @This()) bool {
            return (this.flags & @intFromEnum(Flags.allocated_list)) == @intFromEnum(Flags.allocated_list);
        }

        fn allocate(this: *@This(), _allocator: std.mem.Allocator, capacity: u32) ![]TypeRef {
            std.debug.assert(!this.isAllocated());

            this.flags |= @intFromEnum(Flags.allocated_list);
            const buf = try _allocator.alloc(TypeRef, capacity);
            @memcpy(buf, this.buf[0..this.count]);

            return buf;
        }

        pub fn append(this: *@This(), analyzer: *Analyzer, type_ref: TypeRef) !void {
            if ((this.flags & @intFromEnum(Flags.parameterized)) != @intFromEnum(Flags.parameterized) and analyzer.isParameterizedRef(type_ref)) {
                this.flags |= @intFromEnum(Flags.parameterized);
            }

            // TODO: check the perf impact of this
            // We only need to check this in class-like contexts
            if ((this.flags & @intFromEnum(Flags.has_this_type)) != @intFromEnum(Flags.has_this_type) and analyzer.hasThisType(type_ref)) {
                this.flags |= @intFromEnum(Flags.has_this_type);
            }

            if (this.count < 4) {
                this.buf[this.count] = type_ref;
                this.count += 1;
                return;
            }

            if (this.count == 4) {
                const buf = try this.allocate(analyzer.allocator(), 8);
                const items_ptr: *u64 = @alignCast(@ptrCast(&this.buf));
                items_ptr.* = @intFromPtr(buf.ptr);
                this.buf[2] = 8;
            }

            return this.appendAllocated(analyzer, type_ref);
        }

        // useful when you don't want to write to a type directly
        pub fn toAllocated(this: *@This(), _allocator: std.mem.Allocator) ![]TypeRef {
            if (this.isAllocated()) {
                return this.getAllocatedSlice();
            }

            const buf = try _allocator.alloc(TypeRef, this.count);
            @memcpy(buf, this.buf[0..this.count]);

            return buf;
        }

        pub fn getCount(this: *const @This()) u32 {
            return this.count;
        }

        pub fn swapRemove(this: *@This(), index: usize) TypeRef {
            const count = this.getCount();
            std.debug.assert(count != 0);
            std.debug.assert(index < count);

            const slice = if (this.isAllocated()) this.getAllocatedSlice() else this.buf[0..count];
            if (index == count-1) {
                this.count = index;
                return slice[index];
            }

            const el = slice[index];
            count -= 1;
            slice[index] = slice[count];
            return el;
        }

        pub fn replace(this: *@This(), index: usize, val: TypeRef) TypeRef {
            const count = this.getCount();
            std.debug.assert(count != 0);
            std.debug.assert(index < count);

            const slice = if (this.isAllocated()) this.getAllocatedSlice() else this.buf[0..count];
            const el = slice[index];
            slice[index] = val;
            return el;
        }

        pub fn getSliceFromType(t: *const Type) []TypeRef {
            if (t.hasFlag(.allocated_list)) {
                if (t.slot0 == 0) return &.{};
                const ptr: [*]TypeRef = @ptrFromInt((@as(u64, t.slot2) << 32) | t.slot1);

                return ptr[0..t.slot0];
            }

            if (t.slot0 == 0) return &.{};

            const items_ptr: [*]TypeRef = @constCast(@ptrCast(&t.slot0));

            var len: u32 = 1;
            if (t.slot1 != 0) {
                if (t.slot2 != 0) {
                    len = 3;
                } else {
                    len = 2;
                }
            }

            return items_ptr[0..len];
        }

        pub fn writeToType(this: *@This(), _allocator: std.mem.Allocator, t: *Type) !void {
            if (this.count <= 3) {
                if (this.count >= 1) {
                    t.slot0 = this.buf[0];
                }
                if (this.count >= 2) {
                    t.slot1 = this.buf[1];
                }
                if (this.count >= 3) {
                    t.slot2 = this.buf[2];
                }

                t.flags |= this.flags;

                return;
            }

            const items_ptr: *u64 = @alignCast(@ptrCast(&t.slot1));

            if (this.count <= this.buf.len) {
                const slice = try this.allocate(_allocator, this.count);
                items_ptr.* = @intFromPtr(slice.ptr);
                t.slot0 = @intCast(slice.len);
                t.flags |= this.flags;
                return;
            }

            const slice = this.shrinkToFit(_allocator, this.getAllocatedSlice());
            items_ptr.* = @intFromPtr(slice.ptr);
            t.slot0 = @intCast(slice.len);
            t.flags |= this.flags;
        }

        const ReverseIterator = struct {
            types: *TypeList,
            pos: u32, // 1-indexed

            pub fn next(this: *@This()) ?TypeRef {
                if (this.pos == 0) return null;

                this.pos -= 1;
                return this.types.getSlice()[this.pos];
            }
        };

        pub fn reverseIterator(this: *@This()) ReverseIterator {
            return .{
                .types = this,
                .pos = this.getCount(),
            };
        }
    };

    inline fn isTypeParamRef(ref: TypeRef) bool {
        return (ref & (0b111 << 29)) == @intFromEnum(Kind.type_parameter_ref);
    }

    inline fn getKindOfRef(this: *const @This(), ref: TypeRef) Kind {
        if (ref >= @intFromEnum(Kind.false)) {
            return @enumFromInt(ref);
        }

        return this.types.at(ref).getKind();
    }

    inline fn hasEvalFlag(state: u32, comptime flag: EvaluationFlags) bool {
        return (state & @intFromEnum(flag)) == @intFromEnum(flag);
    }

    inline fn followInnerScopeAliases(this: *@This(), type_ref: TypeRef) anyerror!TypeRef {
        return this.evaluateType(type_ref, @intFromEnum(EvaluationFlags.inner_scoped_aliases));
    }

    fn maybeResolveAlias(this: *@This(), type_ref: TypeRef) anyerror!TypeRef {
        if (type_ref >= @intFromEnum(Kind.false)) return type_ref;

        const t = this.types.at(type_ref);
        if (t.getKind() != .alias or t.hasFlag(.parameterized)) return type_ref;

        const key: u64 = (@as(u64, 1) << 63) | type_ref;
        if (this.active_types.contains(key)) return type_ref;

        try this.active_types.put(this.allocator(),key, true);
        defer _ = this.active_types.swapRemove(key);

        if (t.hasFlag(.class_alias)) {
            if (t.hasFlag(.global)) {
                // FIXME: we need to show the class w/ merged interfaces ?
                return this.getTypeOfGlobalSymbol(t.slot4);
            }
            return this.getTypeOfSymbol(try this.getAnalyzedFile(t.slot3), t.slot4);
        }

        const result = try this.followImmediateAlias(t, 0) orelse type_ref;

        return if (t.hasFlag(.instance_alias))
            this.getInstanceType(result, true)
        else
            result;
    }

    fn followImmediateAliasInner(this: *@This(), alias: *Type, flags: u32) anyerror!?TypeRef {
        const should_skip_inner = hasEvalFlag(flags, .inner_scoped_aliases);
        if (alias.hasFlag(.global)) {
            if (should_skip_inner) {
                return null;
            }
            if (hasEvalFlag(flags, .no_globals)) {
                return null;
            }
            return try this.getTypeOfGlobalSymbol(alias.slot4);
        }

        const f = try this.getAnalyzedFile(alias.slot3);
        if (should_skip_inner and f.binder.symbols.at(alias.slot4).hasFlag(.top_level)) {
            return null;
        }

        return try this.getTypeOfSymbol(f, alias.slot4);
    }

    fn followImmediateAlias(this: *@This(), alias: *Type, flags: u32) anyerror!?TypeRef {
        if (alias.slot5 != 0) {
            return alias.slot5;
        }

        const inner = try this.followImmediateAliasInner(alias, flags) orelse return null;
        if (inner >= @intFromEnum(Kind.false) or this.types.at(inner).getKind() != .parameterized) {
            alias.slot5 = inner;
            return inner;
        }

        const inner_type = this.types.at(inner);
        const params = getSlice2(inner_type, TypeRef);
        const args = getSlice2(alias, TypeRef);

        var resolved = std.AutoArrayHashMap(TypeRef, TypeRef).init(this.type_args.allocator);
        defer resolved.deinit();

        try resolved.ensureTotalCapacity(params.len);

        for (params, 0..) |p, i| {
            const el = if (i < args.len) args[i] else this.getTypeParamDefault(p) orelse @intFromEnum(Kind.unknown);
            if ((flags & @intFromEnum(EvaluationFlags.type_args)) != @intFromEnum(EvaluationFlags.type_args)) {
                try resolved.put(p, el);
            } else {
                const v = try this.evaluateType(el, flags);
                try resolved.put(p, v);
            }
        }

        alias.slot5 = try this.resolveWithTypeArgsInferred(inner_type, &resolved);
        return alias.slot5;
    }

    const EvaluationFlags = enum(u32) {
        all = 1 << 0,
        type_args = 1 << 1,
        no_globals = 1 << 2,
        no_objects = 1 << 3,
        inner_scoped_aliases = 1 << 20,
        for_reify = 1 << 30,
    };

    pub fn evaluateType(this: *@This(), ref: TypeRef, flags: u32) anyerror!TypeRef {
        if (ref >= @intFromEnum(Kind.false)) {
            return ref;
        }

        const t = this.types.at(ref);
        const evaluated = try switch (this.getKindOfRef(ref)) {
            .conditional => try this.tryResolveConditionalType(t.slot0, t.slot1, t.slot2, t.slot3) orelse ref, // TODO: flags
            .query => this.evaluateQuery(t, flags),
            .keyof => this.evaluateKeyOfType(t.slot0), // TODO: flags
            .indexed => this.accessType(t.slot0, t.slot1),
            .alias => {
                if (t.hasFlag(.parameterized)) return ref;
                return try this.followImmediateAlias(t, @intFromEnum(EvaluationFlags.type_args) | flags) orelse ref;
            },
            .function_literal => {
                var did_change = false;
                var resolved = std.ArrayList(TypeRef).init(this.allocator());
                defer if (!did_change) resolved.deinit();

                for (getSlice2(t, TypeRef)) |el| {
                    const v = try this.evaluateType(el, flags);
                    if (v != el) did_change = true;
                    try resolved.append(v);
                }

                const return_type = try this.evaluateType(t.slot3, flags);
                if (return_type != t.slot3) did_change = true;

                const this_type = try this.evaluateType(t.slot4, flags);
                if (this_type != t.slot4) did_change = true;

                if (!did_change) return ref;

                var flags2: u24 = 0;
                if (t.hasFlag(.constructor)) flags2 |= @intFromEnum(Flags.constructor);

                return try this.createFunctionLiteralFull(resolved.items, return_type, this_type, flags2);
            },
            .@"union" => {
                var did_change = false;
                var tmp = TempUnion.init(this.allocator());
                defer if (!did_change) tmp.deinit();
                for (getSlice2(t, TypeRef)) |el| {
                    const v = try this.evaluateType(el, flags);
                    if (v != el) did_change = true;
                    if (try this.addToUnion(&tmp, v)) |x| return x;
                }

                if (!did_change) return ref;

                return tmp.complete(this);
            },
            .intersection => {
                if (!hasEvalFlag(flags, .for_reify)) return ref;

                var tmp = std.ArrayList(ObjectLiteralMember).init(this.allocator());

                for (getSlice2(t, TypeRef)) |el| {
                    const v = try this.evaluateType(el, flags);
                    if (v >= @intFromEnum(Kind.false)) {
                        if (v == @intFromEnum(Kind.empty_object)) continue;
                        if (v == @intFromEnum(Kind.any) or v == @intFromEnum(Kind.never)) {
                            tmp.deinit();
                            return v;
                        }
                        this.printTypeInfo(v);
                        return error.TODO_primitive_intersection;
                    }
                    const t2 = this.types.at(v);
                    if (t2.getKind() != .object_literal) {
                        this.printTypeInfo(v);
                        return error.TODO_intersection_non_object;
                    }
                    const members = getSlice2(t2, ObjectLiteralMember);
                    for (members) |*m| {
                        if (m.kind != .property) continue;

                        const member_name_hash = try this.getMemberNameHash(m.name);
                        const member_type = try m.getType(this);
                        var found = false;
                        for (tmp.items) |*m2| {
                            if (member_name_hash != try this.getMemberNameHash(m2.name)) continue;

                            found = true;
                            if (m2.type != member_type) {
                                if (try this.isAssignableTo(m2.type, member_type)) {
                                    m2.type = member_type;
                                } else if (!try this.isAssignableTo(member_type, m2.type)) {
                                    m2.type = @intFromEnum(Kind.never);
                                }
                            }
                            break;
                        }
                        
                        if (!found) {
                            try tmp.append(.{
                                .name = m.name,
                                .type = member_type,
                            });
                        }
                    }
                }

                return this.createObjectLiteral(tmp.items, 0, 0);
            },
            .tuple => {
                const members = getSlice2(t, TypeRef);
                var did_change = false;
                var tmp = try std.ArrayList(TypeRef).initCapacity(this.allocator(), members.len);
                defer if (!did_change) tmp.deinit();
                for (members) |el_type| {
                    if (el_type < @intFromEnum(Kind.false) and this.getKindOfRef(el_type) == .named_tuple_element) {
                        const m = this.types.at(el_type);
                        const inner = m.slot1;
                        const v = try this.evaluateType(inner, flags);
                        if (!m.hasFlag(.spread)) {
                            if (v == inner) {
                                try tmp.append(el_type);
                                continue;
                            }
                            // if (this.isParameterizedRef(v)) flags2 |= @intFromEnum(Flags.parameterized);
                            did_change = true;
                            try tmp.append(v);
                            continue;
                        }

                        if (v == @intFromEnum(Kind.empty_tuple)) {
                            did_change = true;
                            continue;
                        }

                        if (this.isTuple(v)) {
                            did_change = true;
                            try this.reduceTupleElement(this.types.at(v), &tmp);
                            continue;
                        }

                        if (v == inner) {
                            try tmp.append(el_type);
                            continue;
                        }

                        did_change = true;
                        try tmp.append(try this.types.push(.{
                            .kind = @intFromEnum(Kind.named_tuple_element),
                            .slot0 = 0,
                            .slot1 = v,
                            .flags = @intFromEnum(Flags.spread),
                        }));

                        continue;
                    }

                    const v = try this.evaluateType(el_type, flags);
                    if (v != el_type) did_change = true;

                    try tmp.append(v);
                }

                if (!did_change) return ref;

                return this.createTupleType(tmp.items, 0);
            },
            .object_literal => {
                if (hasEvalFlag(flags, .no_objects)) return ref;

                const old_this_type = this.contextual_this_type;
                defer this.contextual_this_type = old_this_type;
                this.contextual_this_type = ref;

                const members = getSlice2(t, ObjectLiteralMember);
                var did_change = false;
                var tmp = try std.ArrayList(ObjectLiteralMember).initCapacity(this.allocator(), members.len);
                defer if (!did_change) tmp.deinit();
                for (members) |*el| {
                    const el_type = try el.getType(this);
                    const v = try this.evaluateType(el_type, flags);
                    if (v != el_type) did_change = true;
                    // if (this.isParameterizedRef(v)) flags2 |= @intFromEnum(Flags.parameterized);

                    tmp.appendAssumeCapacity(.{
                        .flags = el.flags,
                        .kind = el.kind,
                        .type = v,
                        .name = el.name,
                        .slot0 = el.slot0,
                    });
                }

                const proto = if (t.slot3 != 0) try this.evaluateType(t.slot3, flags) else 0;
                if (proto != t.slot3) did_change = true;

                if (!did_change) return ref;

                return this.createObjectLiteral(tmp.items, 0, proto);
            },
            else => ref,
        };

        return if (evaluated != ref) this.evaluateType(evaluated, flags) else evaluated;
    }

    fn isSimpleType(this: *const @This(), ref: TypeRef) bool {
        if (ref >= @intFromEnum(Kind.false)) return true;

        const t = this.types.at(ref);

        return switch (t.getKind()) {
            .alias => true, // FIXME: check for simple args too
            .type_parameter => true,
            .string_literal, .number_literal => true,
            .named_tuple_element => this.isSimpleType(t.slot1),
            .tuple, .@"union", .intersection => {
                const elements = getSlice2(t, TypeRef);
                if (elements.len > 3) return false;

                for (elements) |u| {
                    if (!this.isSimpleType(u)) return false;
                }

                return true;
            },
            else => false,
        };
    }

    pub fn maybeSimplifyType(this: *@This(), ref: TypeRef) anyerror!TypeRef {
        if (ref >= @intFromEnum(Kind.false)) return ref;
        if (this.isParameterizedRef(ref)) return ref;

        const t = try this.evaluateType(ref, @intFromEnum(EvaluationFlags.no_globals) | @intFromEnum(EvaluationFlags.no_objects));
        if (t == ref) return ref;

        if (this.isSimpleType(t)) return t;

        const u = this.types.at(ref);
        if (u.getKind() != .alias) return ref;
    
        const args = getSlice2(u, TypeRef);
        if (args.len == 0) return ref;

        var did_change = false;
        var tmp = TypeList{};
        defer if (!did_change) tmp.deinit(this.allocator());

        for (args) |a| {
            const v = try this.maybeSimplifyType(a);
            if (v != a) did_change = true;
            try tmp.append(this, v);
        }

        if (!did_change) return ref;

        return try this.createCanonicalAlias(&tmp, u.slot3, u.slot4, u.flags);
    }

    fn resolveTypeParam(this: *const @This(), ref: TypeRef) ?TypeRef {
        const register: u32 = ref & (~@intFromEnum(Kind.type_parameter_ref));
        if (register >= this.type_registers.len) {
            // TODO
            return null;
        }

        if (this.type_registers[register] == 0) return null;
        return this.type_registers[register];
    }

    fn resolveUnionOrIntersection(this: *@This(), ref: TypeRef, comptime kind: Kind) !TypeRef {
        comptime switch (kind) {
            .@"union", .intersection => {},
            else => @compileError("Invalid kind"),
        };

        var did_change = false;
        var should_deinit = true;
        const elements = getSlice2(this.types.at(ref), TypeRef);
        var arr = try std.ArrayList(TypeRef).initCapacity(this.type_args.allocator, elements.len);
        defer if (should_deinit) arr.deinit();

        var flags: u24 = 0;

        for (elements) |el| {
            const resolved = try this.resolveParameterizedType(el) orelse el;
            if (resolved != el) did_change = true;
            if (this.isParameterizedRef(resolved)) flags |= @intFromEnum(Flags.parameterized);
            try arr.append(resolved);
        }

        if (!did_change) return ref;
        should_deinit = false;

        if (comptime kind == .@"union") {
            return this.toUnion(arr.items);
        }
        
        if (arr.items.len == 2) {
            should_deinit = true;
            if (arr.items[1] == @intFromEnum(Kind.empty_object)) {
                return this.nonNullable(arr.items[0]);
            }
            return this.intersectType(arr.items[0], arr.items[1]);
        }

        std.debug.print("FIXME INTERSECTION TYPES LEN > 2\n", .{});

        return createIntersectionType(this, arr.items, flags);
    }

    fn getTypeParamDefault(this: *@This(), param: TypeRef) ?TypeRef {
        std.debug.assert(param < @intFromEnum(Kind.false));
        const t = this.types.at(param);
        std.debug.assert(t.getKind() == .type_parameter);

        return if (t.slot2 == 0) null else t.slot2;
    }

    fn getTypeParamConstraint(this: *@This(), param: TypeRef) ?TypeRef {
        std.debug.assert(param < @intFromEnum(Kind.false));
        const t = this.types.at(param);
        std.debug.assert(t.getKind() == .type_parameter);

        return if (t.slot1 == 0) null else t.slot1;
    }

    // TODO: more than 3 elements = complex
    fn isComplexType(this: *@This(), ref: TypeRef) !bool {
        _ = this;
        _ = ref;
        return true;
    }

    inline fn isCallableIntrinsic(ref: TypeRef) bool {
        return ref >= @intFromEnum(Kind.string_uppercase) and ref <= @intFromEnum(Kind.string_uncapitalize);
    }

    fn getIntrinsicStringArg(this: *@This()) !TypeRef {
        const arg0 = this.type_registers[0];
        if (arg0 == 0) return error.MissingArg;

        if (arg0 >= @intFromEnum(Kind.false)) {
            return error.TODO_string_capitalize;
        }

        const t = this.types.at(arg0);
        if (t.getKind() != .string_literal) return error.NotAString;

        if ((t.slot3 & @intFromEnum(StringFlags.two_byte)) == @intFromEnum(StringFlags.two_byte)) {
            return error.TODO_two_byte_string;
        }

        return arg0;
    }

    // TODO: use ` u_strToUpper` from ICU
    inline fn uppercaseAsciiChar(c: u8) u8 {
        if (c < 'a' or c > 'z') return c;
        return c - 32;
    }

    // TODO: use ` u_strToLower` from ICU
    inline fn lowercaseAsciiChar(c: u8) u8 {
        if (c < 'A' or c > 'Z') return c;
        return c + 32;
    }

    // Only works on ASCII strings currently
    fn capitlizeOrUncapitalize(this: *@This(), comptime capitalize: bool) !TypeRef {
        const arg0 = try this.getIntrinsicStringArg();
        const slice = this.getSliceFromLiteral(arg0);
        const u = if (comptime capitalize) uppercaseAsciiChar(slice[0]) else lowercaseAsciiChar(slice[0]);
        if (slice[0] == u) return arg0;

        // TODO: check if the same string literal exists first
        var buf = try this.allocator().alloc(u8, slice.len);
        @memcpy(buf, slice);
        buf[0] = u;
        return try this.createSyntheticStringLiteral(buf, true);
    }

    fn callIntrinsicType(this: *@This(), ref: TypeRef) !TypeRef {
        switch (@as(Kind, @enumFromInt(ref))) {
            .string_capitalize => return this.capitlizeOrUncapitalize(true),
            .string_uncapitalize => return this.capitlizeOrUncapitalize(false),
            .string_uppercase, .string_lowercase => return error.TODO_string_uppercase,
            else => unreachable,
        }
    }

    fn resolveParameterizedType(this: *@This(), ref: TypeRef) anyerror!?TypeRef {
        if (ref >= @intFromEnum(Kind.false)) {
            if (isCallableIntrinsic(ref)) {
                return try this.callIntrinsicType(ref);
            } else if (isTypeParamRef(ref)) {
                return this.resolveTypeParam(ref);
            }

            return ref;
        }

        const n = this.types.at(ref);
        if (n.getKind() == .type_parameter) {
            if (hasTypeFlag(n, .infer_node)) {
                return ref;
            }

            const constraint = if (n.slot1 != 0) try this.resolveParameterizedType(n.slot1) orelse n.slot1 else 0;
            const default = if (n.slot2 != 0) try this.resolveParameterizedType(n.slot2) orelse n.slot2 else 0;
            if (constraint == n.slot1 and default == n.slot2) return ref;

            return try this.types.push(.{
                .kind = @intFromEnum(Kind.type_parameter),
                .slot0 = n.slot0,
                .slot1 = constraint,
                .slot2 = default,
                .slot3 = n.slot3,
                .slot4 = n.slot4,
                .slot6 = n.slot6,
            });
        }

        if (!n.hasFlag(.parameterized)) return ref;

        switch (n.getKind()) {
            .alias => {
                // var target: TypeRef = undefined;
                var flags: u24 = 0;
                if (n.hasFlag(.global)) {
                    flags |= @intFromEnum(Flags.global);
                //    target = try this.getTypeOfGlobalSymbol(n.slot4);
                } else {
                //    target = try this.getTypeOfSymbol(try this.getAnalyzedFile(n.slot3), n.slot4);
                }

                // if (target >= @intFromEnum(Kind.false)) return error.TODO;

                const args = getSlice2(n, TypeRef);

                var did_change = false;
                var resolved_args = TypeList{};

                for (args) |el| {
                    const v = try this.resolveParameterizedType(el) orelse el;
                    if (v != el) did_change = true;
                    try resolved_args.append(this, v);
                }

                if (!did_change) {
                    resolved_args.deinit(this.allocator());
                    return ref;
                }

                return try this.createCanonicalAlias(&resolved_args, n.slot3, n.slot4, flags);
            },
            .@"union" => return try this.resolveUnionOrIntersection(ref, .@"union"),
            .intersection => return try this.resolveUnionOrIntersection(ref, .intersection),
            .indexed => {
                const lhs = try this.resolveParameterizedType(n.slot0) orelse n.slot0;
                const rhs = try this.resolveParameterizedType(n.slot1) orelse n.slot1;

                if (lhs == n.slot0 and rhs == n.slot1) return ref;

                var flags: u24 = 0;
                if (this.isParameterizedRef(lhs) or this.isParameterizedRef(rhs)) {
                    flags |= @intFromEnum(Flags.parameterized);
                    return try this.types.push(.{
                        .kind = @intFromEnum(Kind.indexed),
                        .flags = flags,
                        .slot0 = lhs,
                        .slot1 = rhs,
                    });
                }

                return try this.accessType(lhs, rhs);
            },
            .template_literal => {
                // TODO: maybe add cons string?
                var did_change = false;
                var can_join = true;
                const slice = getSlice2(n, TypeRef);
                var size: usize = this.getSliceFromLiteral(slice[0]).len;
                var resolved = std.ArrayList(TypeRef).init(this.type_args.allocator);
                defer resolved.deinit();

                var flags: u24 = 0;
                var i: u32 = 1;
                while (i < slice.len) : (i += 2) {
                    const v = try this.resolveParameterizedType(slice[i]) orelse slice[i];
                    if (v != slice[i]) did_change = true;

                    try resolved.append(v);

                    if (this.isParameterizedRef(v)) flags |= @intFromEnum(Flags.parameterized);

                    if (v < @intFromEnum(Kind.false) and this.types.at(v).getKind() == .@"union") return error.TODO;

                    if (can_join) {
                        if ((v >= @intFromEnum(Kind.false) and v != @intFromEnum(Kind.empty_string)) or this.types.at(v).getKind() != .string_literal) {
                            can_join = false;
                        } else {
                            size += this.getSliceFromLiteral(v).len;
                            size += this.getSliceFromLiteral(slice[i + 1]).len;
                        }
                    }
                }

                if (can_join) {
                    var buf = try std.ArrayList(u8).initCapacity(this.allocator(), size);
                    buf.appendSliceAssumeCapacity(this.getSliceFromLiteral(slice[0]));

                    for (resolved.items, 0..) |exp, j| {
                        buf.appendSliceAssumeCapacity(this.getSliceFromLiteral(exp));
                        buf.appendSliceAssumeCapacity(this.getSliceFromLiteral(slice[(j + 1) * 2]));
                    }

                    return try this.createSyntheticStringLiteral(buf.items, true);
                }

                if (!did_change) return ref;

                var resolved2 = std.ArrayList(TypeRef).init(this.allocator());
                try resolved2.ensureTotalCapacityPrecise(slice.len);

                resolved2.appendAssumeCapacity(slice[0]);
                for (resolved.items, 0..) |exp, j| {
                    resolved2.appendAssumeCapacity(exp);
                    resolved2.appendAssumeCapacity(slice[(j + 1) * 2]);
                }

                const ptr: u64 = @intFromPtr(resolved2.items.ptr);
                return try this.types.push(.{
                    .kind = @intFromEnum(Kind.template_literal),
                    .flags = flags,
                    .slot0 = @truncate(ptr),
                    .slot1 = @intCast(ptr >> 32),
                    .slot2 = @intCast(resolved2.items.len),
                });
            },
            .conditional => {
                const subject = try this.resolveParameterizedType(n.slot0) orelse return ref;

                // Typescript appears to distribute unions over conditionals if the subject starts as a type parameter
                // TODO: enable caller to control distribution by wrapping arg w/ `()` e.g. `F<(1 | 2 | 3)>`
                // Really this just means differentiating between "naked" and grouped unions. But that's a whole can of worms.
                if (subject < @intFromEnum(Kind.false) and this.types.at(subject).getKind() == .@"union" and n.slot4 == 0) {
                    if (isTypeParamRef(n.slot0)) {
                        const reg = n.slot0 - @intFromEnum(Kind.type_parameter_ref);
                        const old_value = this.type_registers[reg];
                        const elements = getSlice2(this.types.at(subject), TypeRef);
                        var arr = try std.ArrayList(TypeRef).initCapacity(this.allocator(), elements.len);
                        defer arr.deinit();
                        defer this.type_registers[reg] = old_value;

                        for (elements) |el| {
                            this.type_registers[reg] = el;
                            const constraint = try this.resolveParameterizedType(n.slot1) orelse n.slot1;

                            if (try this.tryResolveConditionalType(el, constraint, n.slot2, n.slot3)) |t| {
                                try arr.append(t);
                            } else {
                                try arr.append(try this.types.push(.{
                                    .kind = @intFromEnum(Kind.conditional),
                                    .slot0 = el,
                                    .slot1 = constraint,
                                    .slot2 = try this.resolveParameterizedType(n.slot2) orelse n.slot2,
                                    .slot3 = try this.resolveParameterizedType(n.slot3) orelse n.slot3,
                                    .flags = @intFromEnum(Flags.parameterized),
                                }));
                            }
                        }

                        return try this.toUnion(arr.items);
                    }
                }

                const constraint = try this.resolveParameterizedType(n.slot1) orelse n.slot1;

                if (try this.tryResolveConditionalType(subject, constraint, n.slot2, n.slot3)) |t| {
                    return t;
                }

                return try this.types.push(.{
                    .kind = @intFromEnum(Kind.conditional),
                    .slot0 = subject,
                    .slot1 = constraint,
                    .slot2 = try this.resolveParameterizedType(n.slot2) orelse n.slot2,
                    .slot3 = try this.resolveParameterizedType(n.slot3) orelse n.slot3,
                    .slot4 = n.slot4,
                    .flags = @intFromEnum(Flags.parameterized),
                });
            },
            .keyof => {
                const inner = try this.resolveParameterizedType(n.slot0) orelse n.slot0;
                if (inner == n.slot0) return null;

                return try this.evaluateKeyOfType(inner);
            },
            .mapped => {
                return try this.resolveMappedTypeInner(n.slot0, n.slot1, n.slot2, n.slot3) orelse return ref;
            },
            .object_literal => {
                var did_change = false;
                var should_deinit = true;
                var resolved = std.ArrayList(ObjectLiteralMember).init(this.allocator());
                defer if (should_deinit) resolved.deinit();

                var flags: u24 = 0;
                for (getSlice2(n, ObjectLiteralMember)) |*el| {
                    const t = try el.getType(this);
                    const v = try this.resolveParameterizedType(t) orelse t;
                    if (v != t) did_change = true;
                    try resolved.append(.{
                        .kind = el.kind,
                        .name = el.name,
                        .type = v,
                        .flags = el.flags,
                    });

                    if (this.isParameterizedRef(v)) flags |= @intFromEnum(Flags.parameterized);
                }

                if (!did_change) return ref;

                should_deinit = false;
                return try this.createObjectLiteral(resolved.items, flags, n.slot3);
            },
            .tuple => {
                var did_change = false;
                var should_deinit = true;
                var resolved = std.ArrayList(TypeRef).init(this.allocator());
                defer if (should_deinit) resolved.deinit();

                for (getSlice2(n, TypeRef)) |el| {
                    const v = try this.resolveParameterizedType(el) orelse el;
                    if (v != el) did_change = true;
                    try resolved.append(v);
                }

                if (!did_change) return ref;

                should_deinit = false;

                return try this.createTupleType(resolved.items, 0);
            },
            .array => {
                const v = try this.resolveParameterizedType(n.slot0) orelse return null;
                if (v == n.slot0) return ref;

                return try this.createArrayType(v);
            },
            .named_tuple_element => {
                const v = try this.resolveParameterizedType(n.slot1) orelse return null;
                if (v == n.slot1) return ref;

                var flags = n.flags;

                if (!this.isParameterizedRef(v)) {
                    flags &= ~@as(u24, @intFromEnum(Flags.parameterized));
                }

                return try this.types.push(.{
                    .kind = n.kind,
                    .flags = flags,
                    .slot0 = n.slot0,
                    .slot1 = v,
                    .slot4 = n.slot4,
                });
            },
            .function_literal => {
                var flags: u24 = 0;
                var did_change = false;
                var should_deinit = true;
                var resolved = std.ArrayList(TypeRef).init(this.allocator());
                defer if (should_deinit) resolved.deinit();

                const params = getSlice2(n, TypeRef);
                for (params, 0..) |el, i| {
                    const v = try this.resolveParameterizedType(el) orelse el;
                    if (v != el) did_change = true;
                    if (this.isParameterizedRef(v)) flags |= @intFromEnum(Flags.parameterized);

                    if (i < params.len - 1 or v >= @intFromEnum(Kind.false) or !this.types.at(v).hasFlag(.spread)) {
                        try resolved.append(v);
                        continue;
                    }

                    if (this.types.at(v).getKind() != .named_tuple_element) {
                        // TODO
                        try resolved.append(v);
                        continue;
                    }

                    const el_type = this.types.at(v).slot1;
                    if (el_type == @intFromEnum(Kind.empty_tuple)) {
                        continue;
                    } else if (this.isTuple(el_type)) {
                        try this.reduceTupleElement(this.types.at(el_type), &resolved);
                    } else {
                        try resolved.append(v);
                    }
                }

                const return_type = try this.resolveParameterizedType(try this.followInnerScopeAliases(n.slot3)) orelse n.slot3;
                if (return_type != n.slot3) did_change = true;

                const this_type = try this.resolveParameterizedType(n.slot4) orelse n.slot4;
                if (this_type != n.slot4) did_change = true;

                if (!did_change) return ref;

                if (this.isParameterizedRef(return_type) or this.isParameterizedRef(this_type)) {
                    flags |= @intFromEnum(Flags.parameterized);
                }
                if (n.hasFlag(.constructor)) {
                    flags |= @intFromEnum(Flags.constructor);
                }

                should_deinit = false;

                return try this.createFunctionLiteralFull(resolved.items, return_type, this_type, flags);
            },
            .parameterized => {
                var did_change = false;
                var should_deinit = true;

                const params = getSlice2(n, TypeRef);
                var resolved = try std.ArrayList(TypeRef).initCapacity(this.allocator(), params.len);
                defer if (should_deinit) resolved.deinit();

                for (params) |p| {
                    const t = try this.resolveParameterizedType(p) orelse p;
                    if (p != t) did_change = true;
                    resolved.appendAssumeCapacity(t);
                }

                const inner = try this.resolveParameterizedType(n.slot3) orelse n.slot3;
                if (inner != n.slot3) did_change = true;

                if (!did_change) return ref;

                should_deinit = false;
                return try this.createParameterizedType(resolved.items, inner);
            },
            .class => {
                const instance_type = try this.resolveParameterizedType(n.slot0) orelse n.slot0;
                const static_type = try this.resolveParameterizedType(n.slot1) orelse n.slot1;
                const constructor = try this.resolveParameterizedType(n.slot2) orelse n.slot2;
                const base_class = try this.resolveParameterizedType(n.slot3) orelse n.slot3;

                if (instance_type == n.slot0 and static_type == n.slot1 and constructor == n.slot2 and base_class == n.slot3) {
                    return ref;
                }

                var flags: u24 = n.flags;
                flags &= ~@as(u24, @intFromEnum(Flags.parameterized));
                if (this.isParameterizedRef(instance_type)) flags |= @intFromEnum(Flags.parameterized);
                if (this.isParameterizedRef(static_type)) flags |= @intFromEnum(Flags.parameterized);
                if (this.isParameterizedRef(constructor)) flags |= @intFromEnum(Flags.parameterized);
                if (this.isParameterizedRef(base_class)) flags |= @intFromEnum(Flags.parameterized);

                return try this.createClassType(instance_type, static_type, constructor, base_class, flags);
            },
            else => {
                std.debug.print("{any}\n", .{n.getKind()});
            },
        }

        return error.NotImplemented;
    }

    pub fn getSliceFromLiteral(this: *const @This(), ref: TypeRef) []const u8 {
        if (ref == @intFromEnum(Kind.empty_string)) return &.{};

        std.debug.assert(ref < @intFromEnum(Kind.false));

        const t = this.types.at(ref);
        std.debug.assert(t.getKind() == .string_literal);

        if (t.slot5 == 1) return this.synthetic_strings.items[t.slot0];

        const f = this.program.getFileData(t.slot0);

        return getSlice(f.ast.nodes.at(t.slot1), u8);
    }

    inline fn allocator(this: *const @This()) std.mem.Allocator {
        return this.type_args.allocator;
    }

    inline fn hasTypeFlag(n: *const Type, flag: Flags) bool {
        return (n.flags & @intFromEnum(flag)) == @intFromEnum(flag);
    }

    inline fn maybeGetTypeFromRef(this: *const @This(), ref: TypeRef) ?*const Type {
        if (ref >= @intFromEnum(Kind.false)) return null;
        return this.types.at(ref);
    }

    fn getElementFromArrayLike(this: *const @This(), arr: TypeRef, index: u32) TypeRef {
        if (arr >= @intFromEnum(Kind.false)) {
            if (arr == @intFromEnum(Kind.any)) return arr;
            if (arr == @intFromEnum(Kind.empty_tuple)) return @intFromEnum(Kind.undefined);

            return @intFromEnum(Kind.never);
        }

        const n = this.types.at(arr);
        if (n.getKind() == .array) {
            return n.slot0;
        } else if (n.getKind() == .tuple) {
            const elements = getSlice2(n, TypeRef);
            if (index >= elements.len) {
                const last = elements[elements.len - 1];
                if (last < @intFromEnum(Kind.false) and hasTypeFlag(this.types.at(last), .spread)) {
                    return this.getElementFromArrayLike(this.types.at(last).slot1, index - elements.len);
                }

                return @intFromEnum(Kind.undefined);
            }

            const el = elements[index];
            if (el >= @intFromEnum(Kind.false)) return el;
            if (hasTypeFlag(this.types.at(el), .spread)) return this.getElementFromArrayLike(this.types.at(el).slot1, 0);
            if (this.types.at(el).getKind() != .named_tuple_element) return @intFromEnum(Kind.never);
            return this.types.at(el).slot1;
        }

        return @intFromEnum(Kind.never);
    }

    fn inferTupleLikeTypes(
        this: *@This(),
        subject: []const TypeRef,
        condition: []const TypeRef,
        inferred: *std.AutoArrayHashMap(TypeRef, TypeRef),
        variance: Variance,
    ) !?bool {
        if (condition.len == 0) {
            return subject.len == 0;
        }

        const subject_has_spread = subject.len > 0 and
            if (this.maybeGetTypeFromRef(subject[subject.len - 1])) |n| hasTypeFlag(n, .spread) else false;

        const condition_has_spread = condition.len > 0 and
            if (this.maybeGetTypeFromRef(condition[condition.len - 1])) |n| hasTypeFlag(n, .spread) else false;

        const cond_end = if (condition_has_spread) condition.len - 1 else condition.len;
        const subject_end = if (subject_has_spread) subject.len - 1 else subject.len;

        if (subject.len < condition.len) {
            // First match-up all non-spread elements
            // If subject ends w/ a spread element, it must match with all remaining condition types

            for (0..subject_end) |i| {
                const did_match = try this.inferConditionalTypeWithVariance(subject[i], condition[i], inferred, variance) orelse return null;
                if (!did_match) return false;
            }

            for (subject_end..cond_end) |i| {
                const el = condition[i];

                if (variance != .contravariant) {
                    if (this.maybeGetTypeFromRef(el)) |n| {
                        if (!hasTypeFlag(n, .optional)) return false;
                    } else {
                        return false;
                    }
                }

                if (subject_has_spread) {
                    const did_match = try this.inferConditionalTypeWithVariance(subject[subject.len - 1], el, inferred, variance) orelse return null;
                    if (!did_match) return false;
                } else {
                    const ref = this.types.at(el).slot1;
                    if (this.maybeGetTypeFromRef(ref)) |n| {
                        if (hasTypeFlag(n, .infer_node)) {
                            try inferred.put(ref, @intFromEnum(Kind.unknown));
                        }
                    }
                }
            }

            if (condition_has_spread) {
                if (subject_has_spread)
                    return try this.inferConditionalTypeWithVariance(subject[subject.len - 1], condition[condition.len - 1], inferred, variance);

                const ref = this.types.at(condition[condition.len - 1]).slot1;
                if (ref >= @intFromEnum(Kind.false)) {
                    if (ref == @intFromEnum(Kind.never) or ref == @intFromEnum(Kind.any)) return true;
                    if (isTypeParamRef(ref)) {
                        if (this.inferred_type_params) |params| {
                            if (params.get(ref & (~@intFromEnum(Kind.type_parameter_ref)))) |c| {
                                return try this.inferTypeParam(@intFromEnum(Kind.empty_tuple), c, inferred, variance);
                            }
                        }
                        return null;
                    }
                }

                const t = this.maybeGetTypeFromRef(ref) orelse return null;
                if (hasTypeFlag(t, .infer_node)) {
                    try inferred.put(ref, @intFromEnum(Kind.empty_tuple));
                    return true;
                }

                return true;
            }

            return true;
        }

        for (0..cond_end) |i| {
            const did_match = try this.inferConditionalTypeWithVariance(subject[i], condition[i], inferred, variance) orelse return null;
            if (!did_match) return false;
        }

        if (condition_has_spread) {
            const rem = subject[cond_end..];
            const ref = this.types.at(condition[cond_end]).slot1;

            if (ref == @intFromEnum(Kind.any)) {
                for (rem) |x| {
                    const did_match = try this.inferConditionalTypeWithVariance(x, ref, inferred, variance) orelse return null;
                    if (!did_match) return false;
                }
                return true;
            }

            if (ref < @intFromEnum(Kind.false)) {
                const t = this.types.at(ref);
                if (t.getKind() == .type_parameter) {
                    if (!hasTypeFlag(t, .infer_node)) return null;

                    var flags: u24 = 0;
                    for (rem) |x| {
                        if (this.isParameterizedRef(x)) {
                            flags |= @intFromEnum(Flags.parameterized);
                            break;
                        }
                    }

                    const tuple = try this.createTupleType(rem, flags);
                    try inferred.put(ref, tuple);
                    return true;
                }
            }

            var flags: u24 = 0;
            for (rem) |x| {
                if (this.isParameterizedRef(x)) {
                    flags |= @intFromEnum(Flags.parameterized);
                    break;
                }
            }

            const tuple = try this.createTupleType(rem, flags);

            return try this.inferConditionalTypeWithVariance(tuple, ref, inferred, variance) orelse return null;
        }

        return subject_end == cond_end;
    }

    inline fn inferConditionalType(this: *@This(), subject: TypeRef, condition: TypeRef, inferred: *std.AutoArrayHashMap(TypeRef, TypeRef)) !?bool {
        return this.inferConditionalTypeWithVariance(subject, condition, inferred, .covariant);
    }

    fn inferTypeParam(
        this: *@This(),
        subject: TypeRef,
        condition: TypeRef,
        inferred: *std.AutoArrayHashMap(TypeRef, TypeRef),
        variance: Variance,
    ) anyerror!?bool {
        // export function foo3() { // 1 | undefined
        //     return foo4(1)
        // }

        // declare function foo4<T>(x: T): T | undefined

        const should_infer_loosely = false; // this.inferred_type_params != null;

        // TODO: we need to check the constraint too
        if (inferred.get(condition)) |t| {
            if (subject == t) return true;

            if (variance == .covariant) {
                if (try this.isAssignableTo(subject, t)) return true;

                if (try this.isAssignableTo(t, subject)) {
                    inferred.putAssumeCapacity(condition, subject);
                    return true;
                }
            } else if (variance == .contravariant) {
                if (try this.isAssignableTo(t, subject)) return true;

                if (try this.isAssignableTo(subject, t)) {
                    inferred.putAssumeCapacity(condition, subject);
                    return true;
                }
            }

            return false;
        }

        if (this.getTypeParamConstraint(condition)) |constraint| {
            // variance???
            if (!try this.isAssignableTo(subject, constraint)) {
                return false;
            }
        }

        if (!should_infer_loosely) {
            try inferred.put(condition, subject);
        } else if (subject >= @intFromEnum(Kind.zero)) {
            try inferred.put(condition, @intFromEnum(Kind.number));
        } else if (subject < @intFromEnum(Kind.false) and this.types.at(subject).getKind() == .string_literal) { // FIXME: do empty_string
            try inferred.put(condition, @intFromEnum(Kind.string));
        } else {
            try inferred.put(condition, subject);
        }

        return true;
    }

    fn inferUnion(
        this: *@This(),
        subject: TypeRef,
        condition: TypeRef,
        inferred: *std.AutoArrayHashMap(TypeRef, TypeRef),
        variance: Variance,
    ) anyerror!?bool {
        std.debug.assert(variance != .invariant);

        // TODO: check for `infer U` nodes which need special handling e.g.
        // type F<T> = T extends 1 | infer U | 3 ? U : never
        // type x1 = F<1 | 2 | 3>   // 1 | 3 | 2
        // type x2 = F<1 | 3>       // 1 | 3
        // type x3 = F<1>           // 1
        //
        // The behavior seems to always pick the broadest applicable type i.e. itself
        //
        // Actually, let's not replicate this behavior. What value does it add?
        // Let's prefer using the narrowest possible type while also not distributing
        // the union when the constraint is an inferrable union type.
        //
        // Here's what I would expect:
        // type x1 = F<1 | 2 | 3>   // 2
        // type x2 = F<1 | 3>       // never
        // type x3 = F<1>           // never

        const c = this.types.at(condition);
        const condition_slice = getSlice2(c, TypeRef);

        if (variance == .contravariant) {
            for (condition_slice) |el2| {
                const did_match = try this.inferConditionalTypeWithVariance(subject, el2, inferred, .contravariant) orelse return null;
                if (!did_match) return false;
            }
            return true;
        }

        const maybe_set = maybeGetUnionSet(c);

        for (getSlice2(this.types.at(subject), TypeRef)) |el| {
            if (maybe_set) |set| {
                if (set.contains(el)) continue;
            }
            var did_match_any = false;
            for (condition_slice) |el2| {
                const did_match = try this.inferConditionalTypeWithVariance(el, el2, inferred, .covariant) orelse return null;
                if (did_match) {
                    did_match_any = true;
                    break;
                }
            }
            if (!did_match_any) return false;
        }

        return true;
    }

    fn inferConditionalTypeWithVariance(
        this: *@This(),
        subject: TypeRef,
        condition: TypeRef,
        inferred: *std.AutoArrayHashMap(TypeRef, TypeRef),
        variance: Variance,
    ) anyerror!?bool {
        if (subject == condition) {
            return true;
        }

        if (subject < @intFromEnum(Kind.false)) {
            const t = this.types.at(subject);
            if (t.getKind() == .type_parameter or t.getKind() == .parameterized) return null;
        } else if (isTypeParamRef(subject)) return null;

        if (condition >= @intFromEnum(Kind.false)) {
            if (isTypeParamRef(condition)) {
                if (this.inferred_type_params) |params| {
                    if (params.get(condition & (~@intFromEnum(Kind.type_parameter_ref)))) |c| {
                        return try this.inferTypeParam(subject, c, inferred, variance);
                    }
                }
                return null;
            }

            if (this.isParameterizedRef(subject)) return null;

            return switch (variance) {
                .invariant => false,
                .covariant => try this.isAssignableTo(subject, condition),
                .contravariant => try this.isAssignableTo(condition, subject),
            };
        }

        const n = this.types.at(condition);

        switch (n.getKind()) {
            .alias => {
                if (subject < @intFromEnum(Kind.false)) {
                    const s = this.types.at(subject);
                    if (s.getKind() == .alias) {
                        // Fast path for followed aliases
                        if (s.slot5 != 0 and n.slot5 != 0 and s.slot5 == n.slot5) {
                            // FIXME: this needs to update `inferred` ??
                            return true;
                        }

                        if (s.slot3 == n.slot3 and s.slot4 == n.slot4) {
                            return try this.inferTupleLikeTypes(
                                getSlice2(s, TypeRef),
                                getSlice2(n, TypeRef),
                                inferred,
                                variance,
                            );
                        }

                        const r = try this.maybeResolveAlias(subject);
                        if (r != subject) {
                            return try this.inferConditionalTypeWithVariance(r, condition, inferred, variance);
                        }
                    }
                }

                const c = try this.maybeResolveAlias(condition);
                if (c == condition) {
                    if (n.hasFlag(.parameterized)) {
                        const c2 = try this.followImmediateAlias(n, 0) orelse return error.TODO_failed_to_follow_alias;
                        return try this.inferConditionalTypeWithVariance(subject, c2, inferred, variance);
                    }

                    return error.TODO_infer_alias;
                }

                return try this.inferConditionalTypeWithVariance(subject, c, inferred, variance);
            },
            .type_parameter => {
                if ((n.flags & @intFromEnum(Flags.infer_node)) == 0) {
                    return null;
                }

                return try this.inferTypeParam(subject, condition, inferred, variance);
            },
            .tuple => {
                if (subject >= @intFromEnum(Kind.false)) {
                    if (subject == @intFromEnum(Kind.empty_tuple)) {
                        return try this.inferTupleLikeTypes(&.{}, getSlice2(n, TypeRef), inferred, variance);
                    }

                    if (subject == @intFromEnum(Kind.never)) return false;
                    if (subject >= @intFromEnum(Kind.zero)) return false;

                    try this.debugPrint(subject);

                    return error.TODO;
                }

                const s = this.types.at(subject);
                if (s.getKind() == .array) return error.TODO;

                if (s.getKind() != .tuple) return false;

                return try this.inferTupleLikeTypes(getSlice2(s, TypeRef), getSlice2(n, TypeRef), inferred, variance);
            },
            .named_tuple_element => {
                if (subject >= @intFromEnum(Kind.false)) {
                    if (hasTypeFlag(n, .spread)) {
                        if (n.slot1 >= @intFromEnum(Kind.false)) {
                            return this.inferConditionalTypeWithVariance(subject, n.slot1, inferred, variance);
                        }
                        return this.inferConditionalTypeWithVariance(subject, this.types.at(n.slot1).slot0, inferred, variance);
                    }
                    return this.inferConditionalTypeWithVariance(subject, n.slot1, inferred, variance);
                }

                const s = this.types.at(subject);
                if (s.getKind() != .named_tuple_element) {
                    if (hasTypeFlag(n, .spread)) {
                        return this.inferConditionalTypeWithVariance(subject, this.types.at(n.slot1).slot0, inferred, variance);
                    }
                    return this.inferConditionalTypeWithVariance(subject, n.slot1, inferred, variance);
                }

                if (!hasTypeFlag(s, .spread) and hasTypeFlag(n, .spread)) {
                    if (hasTypeFlag(s, .optional)) {
                        const tmp = try this.toUnion(&.{ s.slot1, @intFromEnum(Kind.undefined) });
                        return this.inferConditionalTypeWithVariance(tmp, this.types.at(n.slot1).slot0, inferred, variance);
                    }
                    if (n.slot1 >= @intFromEnum(Kind.false)) {
                        return this.inferConditionalTypeWithVariance(s.slot1, n.slot1, inferred, variance);
                    }
                    return this.inferConditionalTypeWithVariance(s.slot1, this.types.at(n.slot1).slot0, inferred, variance);
                }

                if (hasTypeFlag(s, .spread) and !hasTypeFlag(n, .spread)) return false;
                if (variance == .covariant and hasTypeFlag(s, .optional) and !hasTypeFlag(n, .optional)) return false;
                if (variance == .contravariant and !hasTypeFlag(s, .optional) and hasTypeFlag(n, .optional)) return false;

                return this.inferConditionalTypeWithVariance(s.slot1, n.slot1, inferred, variance);
            },
            .function_literal => {
                if (subject >= @intFromEnum(Kind.false)) {
                    return false;
                }

                const s = this.types.at(subject);
                if (s.getKind() != .function_literal) { // TODO: other types
                    if (s.getKind() == .class) {
                        const ctor = try this.getClassCtorFromType(s);
                        return try this.inferConditionalTypeWithVariance(ctor, condition, inferred, variance);
                    }
                    if (s.getKind() == .query) {
                        if (s.slot5 != 0) {
                            return try this.inferConditionalTypeWithVariance(s.slot5, condition, inferred, variance);
                        }
                        const r = try this.getType(try this.getAnalyzedFile(s.slot3), s.slot4);
                        s.slot5 = r;
                        return try this.inferConditionalTypeWithVariance(r, condition, inferred, variance);
                    }
                    return false;
                }

                if (hasTypeFlag(n, .constructor) != hasTypeFlag(s, .constructor)) return false;

                // TODO: variance w/ `this`
                if (n.slot4 != 0) {
                    if (s.slot4 == 0) return false; // TODO: are there edge cases?

                    const did_match_this = try this.inferConditionalTypeWithVariance(s.slot4, n.slot4, inferred, variance) orelse return null;
                    if (!did_match_this) return false;
                }

                // skip matching up return types when inferring parameters (FIXME: we should only skip once...)
                // if (this.inferred_type_params == null or variance != .covariant) {

                // }

                // `void` is always assignable to as a return type
                if (n.slot3 == @intFromEnum(Kind.void) and variance == .covariant) {} else {
                    const did_match_return = try this.inferConditionalTypeWithVariance(s.slot3, n.slot3, inferred, variance) orelse return null;
                    if (!did_match_return) return false;
                }

                const did_match_params = try this.inferTupleLikeTypes(
                    getSlice2(s, TypeRef),
                    getSlice2(n, TypeRef),
                    inferred,
                    variance.invert(),
                ) orelse return null;

                if (!did_match_params) return false;

                return true;
            },

            // The way typescript handles inferring from template literals is pretty rough with several behaviors:
            // * `string` can match 0, 1, or many character based on adjacent text/patterns
            // * infer causes immediately preceding `string` elements to be 1 character e.g. `${string}${string}${infer U}` sets a min length of 2
            // * infer immediately before `string` is always 1 character with 1 or more characters following regardless of the # of `string` elements
            // * any text adjacent to groups of `string` forces them to match 1 or more characters e.g. `${string}${string}b${string}${string}` matches `aba` but not `baa` or `aab`
            //
            // Note that replacing literal text with `infer` **changes the behavior** of surrounding `string` elements. This is incredibly counterintuitive.
            //
            // We implement much simpler rules where `string` always matches 0 or more characters regardless of context.
            // TODO: add flag to switch to typescript behavior
            .template_literal => {
                if (subject >= @intFromEnum(Kind.false)) return false;

                const s = this.types.at(subject);
                if (s.getKind() == .template_literal) return error.TODO;
                if (s.getKind() != .string_literal) return false;

                const slice = getSlice2(n, TypeRef);

                const subject_text = this.getSliceFromLiteral(subject);

                var subject_offset: usize = 0;

                var tmp_inferred = std.AutoArrayHashMap(TypeRef, struct { u32, u32 }).init(inferred.allocator);
                defer tmp_inferred.deinit();

                var i: usize = 0;
                var group: ?TypeRef = null;
                var group_start: usize = 0;
                var has_group_before = false;

                while (i < slice.len) : (i += 1) {
                    const part = slice[i];

                    if (part == @intFromEnum(Kind.empty_string)) continue;

                    if (part == @intFromEnum(Kind.string)) {
                        if (group) |g| {
                            group = null;
                            try tmp_inferred.put(g, .{ @intCast(group_start), @intCast(subject_offset) });
                        }
                        has_group_before = true;
                        continue;
                    }

                    if (part >= @intFromEnum(Kind.false)) return error.TODO;

                    const p = this.types.at(part);
                    if (p.getKind() == .type_parameter) {
                        if (!hasTypeFlag(p, .infer_node)) return false;
                        if (group) |g| {
                            try tmp_inferred.put(g, .{ @intCast(group_start), @intCast(subject_offset) });
                        }

                        // TODO: the constraint on the type param affects matching
                        // e.g. `${infer U extends number}${string}` would match 123 in 123abc
                        // `boolean` is treated as `'true' | 'false'`

                        group = part;
                        group_start = subject_offset;
                        has_group_before = true;
                    } else if (p.getKind() == .string_literal) {
                        const part_text = this.getSliceFromLiteral(part);
                        var j: isize = 0;
                        while (j < part_text.len) : (j += 1) {
                            if (subject_offset == subject_text.len) return false;
                            if (part_text[@as(usize, @intCast(j))] != subject_text[subject_offset]) {
                                if (!has_group_before) return false;
                                j = -1;
                            }
                            subject_offset += 1;
                        }

                        has_group_before = false;
                        if (group) |g| {
                            group = null;
                            try tmp_inferred.put(g, .{ @intCast(group_start), @intCast(subject_offset - part_text.len) });
                        }
                    }
                }

                if (has_group_before) {
                    if (group) |g| {
                        try tmp_inferred.put(g, .{ @intCast(group_start), @intCast(subject_text.len) });
                    }
                    subject_offset = subject_text.len;
                }

                if (subject_offset != subject_text.len) return false;

                var iter = tmp_inferred.iterator();
                while (iter.next()) |entry| {
                    const p = entry.value_ptr.*;
                    const literal = try this.createSyntheticStringLiteral(subject_text[p[0]..p[1]], true);
                    try inferred.put(entry.key_ptr.*, literal);
                }

                return true;
            },
            .object_literal => {
                if (variance == .contravariant and n.slot3 == subject) {
                    return true;
                }

                // XXX: special case for `typeof globalThis`
                if (subject == @intFromEnum(Kind.global_this)) {
                    for (getSlice2(n, ObjectLiteralMember)) |*el| {
                        const h = try this.getMemberNameHash(el.name);

                        const global_ref = this.program.ambient.globals.get(h) orelse {
                            if (el.hasFlag(.optional)) {
                                continue;
                            } else {
                                return false;
                            }
                        };

                        const next = try this.getTypeOfGlobalSymbol(global_ref);
                        const did_match = try this.inferConditionalTypeWithVariance(next, try el.getType(this), inferred, variance);
                        if (!(did_match orelse return null)) {
                            return false;
                        }
                    }

                    return true;
                }

                if (subject >= @intFromEnum(Kind.false)) {
                    return false;
                }

                const s = this.types.at(subject);
                if (s.getKind() == .function_literal) {
                    // TODO: Search for call signatures
                    return error.TODO;
                }

                if (s.getKind() == .query) {
                    const next = try this.evaluateQuery(s, 0);
                    return try this.inferConditionalTypeWithVariance(next, condition, inferred, variance);
                }

                if (s.getKind() == .alias) {
                    const next = try this.maybeResolveAlias(subject);
                    if (next == subject) {
                        return error.FailedToResolveAlias;
                    }
                    return try this.inferConditionalTypeWithVariance(next, condition, inferred, variance);
                }

                if (s.getKind() == .@"union") {
                    const elements = getSlice2(s, TypeRef);

                    var did_match = false;

                    var inferred_unions = std.AutoArrayHashMapUnmanaged(TypeRef, TempUnion){};
                    defer {
                        var iter = inferred_unions.iterator();
                        while (iter.next()) |entry| {
                            entry.value_ptr.deinit();
                        }

                        inferred_unions.deinit(this.allocator());
                    }

                    for (elements) |r| {
                        var inferred_copy = try inferred.clone();
                        const matched = try this.inferConditionalTypeWithVariance(r, condition, &inferred_copy, variance) orelse false;
                        if (!matched) continue;

                        did_match = true;
                        var iter = inferred_copy.iterator();
                        while (iter.next()) |entry| {
                            const key = entry.key_ptr.*;
                            if (inferred.get(key) != null) continue;

                            const ty = entry.value_ptr.*;
                            const entry2 = try inferred_unions.getOrPut(this.allocator(), key);
                            if (entry2.found_existing) {
                                if (try this.addToUnion(entry2.value_ptr, ty)) |q| {
                                    // short-circuit
                                    try inferred.put(key, q);
                                   // entry2.value_ptr.deinit();
                                    _ = inferred_unions.swapRemove(key);
                                }
                            } else {
                                var tmp = TempUnion.init(this.allocator());
                                if (try this.addToUnion(&tmp, ty)) |q| {
                                    try inferred.put(key, q);
                                    _ = inferred_unions.swapRemove(key);
                                //    tmp.deinit();
                                } else {
                                    entry2.value_ptr.* = tmp;
                                }
                            }
                        }
                    }

                    if (!did_match) return false;

                    var iter = inferred_unions.iterator();
                    while (iter.next()) |entry| {
                        const key = entry.key_ptr.*;
                        const v = try entry.value_ptr.complete(this);
                        try inferred.put(key, v);
                        _ = inferred_unions.swapRemove(key);
                    }

                    std.debug.assert(inferred_unions.count() == 0);

                    return true;
                }

                if (s.getKind() != .object_literal) {
                    this.printTypeInfo(subject);
                    return error.TODO;
                }

                if (variance == .covariant and s.slot3 == condition) {
                    return true;
                }

                const slice2 = getSlice2(s, ObjectLiteralMember);

                for (getSlice2(n, ObjectLiteralMember)) |*el| {
                    if (el.name == 0) continue;
                    const h = try this.getMemberNameHash(el.name);

                    var did_match: bool = false;
                    for (slice2) |*el2| {
                        if (el2.name == 0) continue;
                        const h2 = try this.getMemberNameHash(el2.name);

                        if (h != h2) continue;

                        did_match = true;

                        if (try this.inferConditionalTypeWithVariance(try el2.getType(this), try el.getType(this), inferred, variance)) |x| {
                            if (!x) return false;
                        } else return null;
                    }

                    if (!did_match) {
                        if (el.hasFlag(.optional)) continue;

                        return false;
                    }
                }

                return true;
            },
            .string_literal => {
                // It's possible for two string types to not have the same reference due to flags
                const s = this.maybeGetTypeFromRef(subject) orelse return false;
                if (s.getKind() != .string_literal) {
                    this.printTypeInfo(subject);
                    return false; //error.TODO_string_literal_and_other;
                }

                return s.slot2 == n.slot2;
            },
            .@"union" => {
                if (this.maybeGetTypeFromRef(subject)) |s| {
                    if (s.getKind() == .@"union") {
                        return try this.inferUnion(subject, condition, inferred, variance);
                    }
                }

                if (variance == .invariant) return false;

                // TODO: contravariant?

                for (getSlice2(n, TypeRef)) |el| {
                    const did_match = try this.inferConditionalTypeWithVariance(subject, el, inferred, variance) orelse return null;
                    if (did_match) return true;
                }

                return false;
            },
            .array => {
                if (this.maybeGetTypeFromRef(subject)) |s| {
                    if (s.getKind() == .array) {
                        const did_match = try this.inferConditionalTypeWithVariance(s.slot0, n.slot0, inferred, variance) orelse return null;
                        return did_match;
                    }
                    return false;
                }
                std.debug.print("{any}\n", .{n.getKind()});
                return error.TODO4;
            },
            // .class => {
            //     if (subject >= @intFromEnum(Kind.false)) return false;

            //     const s = this.types.at(subject);
            //     switch (s.kind) {
            //         .class => {

            //         },
            //         .alias => {
            //             if (s.hasFlag(.instance_alias)) {

            //             }
            //             const r = try this.maybeResolveAlias(subject);
            //             if (r != subject) {
            //                 return try this.inferConditionalTypeWithVariance(r, condition, inferred, variance);
            //             }
            //             return false;
            //         },
            //         else => return false,
            //     }
            // },
            else => {
                std.debug.print("{any}\n", .{n.getKind()});
                return error.TODO3;
            },
        }
    }

    fn maybeGetClassFromTypeRef(this: *@This(), type_ref: TypeRef) !?*Type {
        const t = this.types.at(type_ref);
        switch (t.getKind()) {
            .class => return t,
            .alias => {
                if (t.hasFlag(.instance_alias)) {
                    return null;
                }
                if (t.hasFlag(.global)) {
                    this.printCurrentNode();
                    // only 1 class decl can exist, find and return it
                    return error.TODO_walk_symbols_for_class;
                }

                const resolved = try this.getTypeOfSymbol(try this.getAnalyzedFile(t.slot3), t.slot4);
                return this.types.at(resolved);
            },
            else => return null,
        }
    }

    fn getClassCtorFromType(this: *@This(), c: *Type) anyerror!TypeRef {
        if (c.slot2 != 0) return c.slot2;

        const rt = if (c.slot6 != 0) c.slot6 else c.slot0; // prefer the alias if available

        const params: []u32 = blk: {
            if (c.slot3 == 0) break :blk &.{};

            const t = try this.maybeGetClassFromTypeRef(c.slot3) orelse return error.TODO_base_class_not_a_class;
            const super_ref = try this.getClassCtorFromType(t);
            const super = this.types.at(super_ref);
            break :blk getSlice2(super, TypeRef);
        };

        c.slot2 = try this.createFunctionLiteral(params, rt, @intFromEnum(Flags.constructor));

        return c.slot2;
    }

    fn getMemberNameHash(this: *const @This(), name: TypeRef) anyerror!u32 {
        if (name == @intFromEnum(Kind.empty_string)) return 0; // TODO

        if (name >= @intFromEnum(Kind.false)) {
            if (name >= @intFromEnum(Kind.zero)) {
                const d: u30 = @intCast(name - @intFromEnum(Kind.zero));
                const v: i30 = @bitCast(d);
                var buf: [11]u8 = undefined;
                const b = try std.fmt.bufPrint(&buf, "{d}", .{v});
                return @truncate(std.hash.Wyhash.hash(0, b));
            }
            this.printCurrentNode();
            this.printTypeInfo(name);
            return notSupported(name);
        }

        const t = this.types.at(name);
        return switch (t.getKind()) {
            .string_literal => t.slot2,
            .symbol_literal => t.slot4,
            else => {
                this.printCurrentNode();
                return notSupported(t.getKind());
            },
        };
    }

    fn maybeEvaluateCondition(this: *@This(), subject: TypeRef, condition: TypeRef) !?struct { bool, std.AutoArrayHashMap(TypeRef, TypeRef) } {
        var inferred = std.AutoArrayHashMap(TypeRef, TypeRef).init(this.allocator());
        const result = try this.inferConditionalType(subject, condition, &inferred) orelse return null;

        return .{ result, inferred };
    }

    fn revertTypeArgs(this: *@This(), args: std.AutoArrayHashMap(TypeRef, TypeRef), registers: [16]TypeRef) void {
        @memcpy(&this.type_registers, &registers);
        var iter = args.iterator();
        while (iter.next()) |entry| {
            if (entry.value_ptr.* == 0) {
                _ = this.type_args.swapRemove(entry.key_ptr.*);
            } else {
                this.type_args.putAssumeCapacity(entry.key_ptr.*, entry.value_ptr.*);
            }
        }
    }

    // `when_true` and `when_false` are lazily evaluated
    fn tryResolveConditionalType(this: *@This(), subject: TypeRef, condition: TypeRef, when_true: TypeRef, when_false: TypeRef) !?u32 {
        var r = try this.maybeEvaluateCondition(subject, condition) orelse return null;
        defer r[1].deinit();

        if (!r[0]) {
            return this.resolveParameterizedType(when_false);
        }

        var tmp_args = std.AutoArrayHashMap(TypeRef, TypeRef).init(this.allocator());
        defer tmp_args.deinit();

        const registers = this.type_registers;

        var iter = r[1].iterator();
        while (iter.next()) |entry| {
            const v = this.type_args.get(entry.key_ptr.*) orelse 0;
            try tmp_args.put(entry.key_ptr.*, v);
            try this.type_args.put(entry.key_ptr.*, entry.value_ptr.*);

            const n = this.types.at(entry.key_ptr.*);
            this.type_registers[n.slot3] = entry.value_ptr.*;
        }

        defer this.revertTypeArgs(tmp_args, registers);

        return this.resolveParameterizedType(when_true);
    }

    fn createConditionalType(this: *@This(), file: *ParsedFileData, ref: NodeRef) !u32 {
        const n = file.ast.nodes.at(ref);
        const d = getPackedData(n);

        const is_subject_parenthesized = file.ast.nodes.at(d.left).kind == .parenthesized_type;

        const left = try this.getType(file, d.left);
        const right = try this.getType(file, d.right);
        const when_true = try this.getType(file, n.len);
        const when_false = try this.getType(file, n.extra_data);

        if (!this.isParameterizedRef(left) and !this.isParameterizedRef(right)) {
            if (try this.tryResolveConditionalType(left, right, when_true, when_false)) |t| {
                return t;
            }
        }

        return this.types.push(.{
            .kind = @intFromEnum(Kind.conditional),
            .slot0 = left,
            .slot1 = right,
            .slot2 = when_true,
            .slot3 = when_false,
            .slot4 = if (is_subject_parenthesized) 1 else 0,
            .flags = @intFromEnum(Flags.parameterized),
        });
    }

    fn unwrapConditionalStatement(file: *const ParsedFileData, ref: NodeRef) NodeRef {
        const n = file.ast.nodes.at(ref);
        if (n.kind == .block) {
            return maybeUnwrapRef(n) orelse 0;
        }
        return ref;
    }

    const AnalysisResult = struct {
        path_type: TypeRef,
        prior_type: TypeRef,
        termination_type: TypeRef = undefined,
        continuation_type: TypeRef = undefined,
    };

    fn pushExpressionFrame(file: *ParsedFileData) !void {
        var state = try CFAState.init(
            file.cfa_state.allocator,
            NodeIterator.init(undefined, 0),
        );
        state.is_expression = true;
        return file.cfa_state.append(state);
    }

    inline fn currentCFAFrame(file: *const ParsedFileData) *CFAState {
        return file.cfa_state.items[file.cfa_state.items.len - 1];
    }

    // TODO: cache this for unions
    fn truthyType(this: *@This(), ty: TypeRef) anyerror!TypeRef {
        if (ty > @intFromEnum(Kind.zero)) return ty;

        if (ty >= @intFromEnum(Kind.false)) {
            if (isTypeParamRef(ty)) return ty; // not sure how this got here

            switch (@as(Kind, @enumFromInt(ty))) {
                .empty_string, .zero, .false, .null, .undefined => return @intFromEnum(Kind.never),
                .boolean => return @intFromEnum(Kind.true),
                else => return ty,
            }
        }

        const n = this.types.at(ty);
        switch (n.getKind()) {
            .@"union" => {
                var did_change = false;
                var tmp = TempUnion.init(this.allocator());
                defer if (!did_change) tmp.elements.deinit();

                for (getSlice2(n, TypeRef)) |t| {
                    const u = try this.truthyType(t);
                    if (t != u) did_change = true;

                    if (u == @intFromEnum(Kind.never)) continue;
                    if (try this.addToUnion(&tmp, u)) |x| {
                        return x;
                    }
                }

                if (!did_change) return ty;

                return tmp.complete(this);
            },
            else => return ty,
        }
    }

    fn isTruthyType(this: *@This(), ty: TypeRef) anyerror!bool {
        if (ty > @intFromEnum(Kind.zero)) return true;

        if (ty >= @intFromEnum(Kind.false)) {
            return switch (@as(Kind, @enumFromInt(ty))) {
                .true, .empty_object, .empty_tuple, .function, .symbol, .global_this => true,
                else => false,
            };
        }

        const n = this.types.at(ty);
        return switch (n.getKind()) {
            .object_literal, .string_literal, .array, .function_literal, .tuple, .symbol_literal => true,
            .intersection => true, // intersections only make sense with non-primitive types
            .mapped => true,
            .parameterized => false, // FIXME: this isn't always correct
            .@"union" => {
                for (getSlice2(n, TypeRef)) |t| {
                    if (!try this.isTruthyType(t)) {
                        return false;
                    }
                }
                return true;
            },
            .alias => {
                return false; // FIXME: this is not always false, we need to inspect the resolved type
            },
            else => return false, // TODO: handle other types
        };
    }

    // fn isSameObjectLiteralType(this: *@This(), a: TypeRef, b: TypeRef) !bool {
    //     if (a == b) return true;

    //     const l = this.types.at(a);
    //     const r = this.types.at(b);
    //     if (l.flags != r.flags) return false;

    //     const lm = getSlice2(l, ObjectLiteralMember);
    //     const rm = getSlice2(r, ObjectLiteralMember);
    //     if (lm.len != rm.len) return false;
    // }

    // this sorts `members` _in-place_
    fn hashObjectLiteral(this: *@This(), flags: u24, members: []ObjectLiteralMember, proto: u32) !u64 {
        var h = std.hash.Wyhash.init(0);
        h.update(&.{@as(u8, @intFromEnum(Kind.object_literal))});
        h.update(&@as([3]u8, @bitCast(flags)));
        h.update(&@as([4]u8, @bitCast(@as(u32, @intCast(members.len)))));
        h.update(&@as([4]u8, @bitCast(proto)));

        const lessThan = struct {
            fn cmp(_: *Analyzer, a: ObjectLiteralMember, b: ObjectLiteralMember) bool {
                return a.name < b.name;
            }
        }.cmp;

        std.sort.insertion(ObjectLiteralMember, members, this, lessThan);
        
        for (members) |m| {
            h.update(&.{@intFromEnum(m.kind)});
            h.update(&@as([4]u8, @bitCast(m.name)));
            h.update(&@as([3]u8, @bitCast(m.flags)));
            if (m.isLazy()) {
                h.update(&@as([4]u8, @bitCast(m.slot0)));
            }
            h.update(&@as([4]u8, @bitCast(m.type)));
        }

        return h.final();
    }

    // Finds type narrowings (if they deviate from the inital type) for the given code
    //
    // This is done using a one-pass ("wavefront") strategy, accumulating knowledge about what a symbol can
    // possibly be as the code moves forward.
    pub const FlowTyper = struct {
        const SymbolSet = std.AutoArrayHashMapUnmanaged(SymbolRef, void);

        const SimpleHashMap = struct {
            buf: [6]u32 = undefined,
            count: u32 = 0,
            available: u32 = undefined,

            inline fn _getKeySlice(this: *@This()) []u32 {
                if (this.count <= 3) {
                    return this.buf[0..this.count];
                }

                const ptr: *u64 = @constCast(@alignCast(@ptrCast(&this.buf)));
                return @as([*]u32, @ptrFromInt(ptr.*))[0..this.buf[2]];
            }

            inline fn _getValueSlice(this: *@This()) []u32 {
                if (this.count <= 3) {
                    return this.buf[this.count..(this.count*2)];
                }

                const ptr: [*]u32 = @ptrFromInt((@as(usize, this.buf[3]) << 32) | this.buf[4]);
                return ptr[0..this.buf[5]];
            }

            inline fn getKeySlice(this: *const @This()) []const u32 {
                if (this.count <= 3) {
                    return this.buf[0..this.count];
                }

                const ptr: *const u64 = @alignCast(@ptrCast(&this.buf));
                return @as([*]const u32, @ptrFromInt(ptr.*))[0..this.buf[2]];
            }

            inline fn getValueSlice(this: *const @This()) []const u32 {
                if (this.count <= 3) {
                    return this.buf[this.count..(this.count*2)];
                }

                const ptr: [*]u32 = @ptrFromInt((@as(usize, this.buf[3]) << 32) | this.buf[4]);
                return ptr[0..this.buf[5]];
            }

            fn findEmptyIndex(this: *const @This(), key: u32, keys: []const u32) u32 {
                _ = this;
                const start: u32 = mix(key) & (@as(u32, @intCast(keys.len))-1);
  
                var i = start;
                while (i < keys.len) {
                    const k = keys[i];
                    if (k == 0) return i;
                    i += 1;
                }

                i = 0;
                while (i < start) {
                    const k = keys[i];
                    if (k == 0) return i;
                    i += 1;
                }

                unreachable;
            }

            fn findIndex(this: *const @This(), key: u32, keys: []const u32) ?u32 {
                if (this.count <= 8) {
                    for (0..this.count) |i| {
                        if (keys[i] == key) {
                            return @truncate(i);
                        }
                    }
                    return null;
                }

                const start: u32 = mix(key) & (@as(u32, @intCast(keys.len))-1);
  
                var i = start;
                while (i < keys.len) {
                    const k = keys[i];
                    if (k == 0) break;
                    if (k == key) return i;
                    i += 1;
                }

                i = 0;
                while (i < start) {
                    const k = keys[i];
                    if (k == 0) break;
                    if (k == key) return i;
                    i += 1;
                }

                return null;
            }

            fn _ensureCapacity(this: *@This(), a: std.mem.Allocator, amount: u32) !void {                
                const keys = this.getKeySlice();
                std.debug.assert(keys.len < amount);

                const k = try a.alloc(u32, amount);
                if (amount <= 8) {
                    @memcpy(k[0..this.count], keys);
                }

                const values = this.getValueSlice();
                const v = try a.alloc(u32, amount);
                if (amount <= 8) {
                    @memcpy(v[0..this.count], values);
                }

                if (amount > 8) {
                    @memset(k, 0);

                    for (0..keys.len) |i| {
                        const k2 = keys[i];
                        if (k2 == 0) continue;

                        const ind = this.findEmptyIndex(k2, k);
                        k[ind] = k2;
                        v[ind] = values[i];
                    }
                }

                {
                    const ptr: *u64 = @constCast(@alignCast(@ptrCast(&this.buf)));
                    ptr.* = @intFromPtr(k.ptr);
                    this.buf[2] = @intCast(k.len);
                }

                {
                    this.buf[3] = @truncate(@intFromPtr(v.ptr) >> 32);
                    this.buf[4] = @truncate(@intFromPtr(v.ptr));
                    this.buf[5] = @intCast(v.len);
                }

                this.available = @truncate((amount * 80) / 100);
            }

            fn _swapRemove(this: *@This(), i: u32, j: u32, keys: []u32) void {
                if (i == j) {
                    keys[j] = 0;
                    return;
                }
                const values = this._getValueSlice();
                values[j] = values[i];
                keys[j] = keys[i];
                keys[i] = 0;
            }

            pub fn swapRemove(this: *@This(), key: u32) bool {
                const keys = this._getKeySlice();
                const ind = this.findIndex(key, keys) orelse return false;

                var i = ind + 1;

                while (i < keys.len) {
                    const k = keys[i];
                    if (k == 0) {
                        this._swapRemove(i-1, ind, keys);
                        return true;
                    }
                    i += 1;
                }

                i = 0;
                while (i < ind) {
                    const k = keys[i];
                    if (k == 0) {
                        if (i == 0) {
                            this._swapRemove(@as(u32, @truncate(keys.len))-1, ind, keys);
                        } else {
                            this._swapRemove(i-1, ind, keys);
                        }
                        return true;
                    }
                    i += 1;
                }

                unreachable;
            }

            pub fn get(this: *const @This(), key: u32) ?u32 {
                const init_keys = this.getKeySlice();
                if (this.findIndex(key, init_keys)) |ind| {
                    return this.getValueSlice()[ind];
                }
                return null;
            }

            pub fn put(this: *@This(), a: std.mem.Allocator, key: u32, value: u32) !void {
                const entry = try this.getOrPut(a, key);
                entry.value_ptr.* = value;
            }

            const GetOrPutEntry = struct {
                key_ptr: *u32,
                value_ptr: *u32,
                found_existing: bool,
            };

            pub fn getOrPut(this: *@This(), a: std.mem.Allocator, key: u32) !GetOrPutEntry {
                const init_keys = this._getKeySlice();
                if (this.findIndex(key, init_keys)) |ind| {
                    return .{
                        .key_ptr = &init_keys[ind],
                        .value_ptr = &this._getValueSlice()[ind],
                        .found_existing = true,
                    };
                }

                if (this.count < 3) {
                    this.count += 1;
                    if (this.count == 1) {
                        this.buf[0] = key;
                        return .{
                            .key_ptr = &this.buf[0],
                            .value_ptr = &this.buf[1],
                            .found_existing = false,
                        };
                    } else if (this.count == 2) {
                        const v0 = this.buf[1];
                        this.buf[1] = key;
                        this.buf[2] = v0;

                        return .{
                            .key_ptr = &this.buf[1],
                            .value_ptr = &this.buf[3],
                            .found_existing = false,
                        };
                    } else {
                        const v0 = this.buf[2];
                        const v1 = this.buf[3];
                        this.buf[2] = key;
                        this.buf[3] = v0;
                        this.buf[4] = v1;

                        return .{
                            .key_ptr = &this.buf[2],
                            .value_ptr = &this.buf[5],
                            .found_existing = false,
                        };
                    }
                }

                if (this.count == 3) {
                    try this._ensureCapacity(a, 8);
                    this.count += 1;
                    const keys = this._getKeySlice();
                    keys[3] = key;
                    return .{
                        .key_ptr = &keys[3],
                        .value_ptr = &this._getValueSlice()[3],
                        .found_existing = false,
                    };
                }

                if (this.count < 8) {
                    const keys = this._getKeySlice();
                    keys[this.count] = key;
                    this.count += 1;
                    return .{
                        .key_ptr = &keys[this.count-1],
                        .value_ptr = &this._getValueSlice()[this.count-1],
                        .found_existing = false,
                    };
                }

                if (this.count > this.available) {
                    const new_amount = std.math.ceilPowerOfTwo(u32, this.count + 1) catch unreachable;
                    try this._ensureCapacity(a, new_amount);
                }

                this.count += 1;
                const keys = this._getKeySlice();
                const ind = this.findEmptyIndex(key, keys);
                keys[ind] = key;

                return .{
                    .key_ptr = &keys[ind],
                    .value_ptr = &this._getValueSlice()[ind],
                    .found_existing = false,
                };
            }

            pub fn deinit(this: *@This(), a: std.mem.Allocator) void {
                if (this.count <= 3) return;

                a.free(this._getKeySlice());
                a.free(this._getValueSlice());
            }


            const Entry = struct {
                key_ptr: *u32,
                value_ptr: *u32,
            };

            const Iterator = struct {
                m: *SimpleHashMap,
                c: u32 = 0,
                pub inline fn next(this: *@This()) ?Entry {
                    const i = this.c;
                    if (i == this.m.count) return null;
                    this.c += 1;
                    return .{
                        .key_ptr = &this.m._getKeySlice()[i],
                        .value_ptr = &this.m._getValueSlice()[i],
                    };
                }
            };

            pub fn iterator(this: *@This()) Iterator {
                return .{ .m = this };
            }

            // performs well enough
            fn mix(k: u64) u32 {
                var key = k;
                key *= 2654435761;
                key ^= key >> 33;
                return @truncate(key);
            }
        };

        fn Pool(comptime T: type) type {
            return struct {
                items: std.ArrayListUnmanaged(T) = .{},
                free_list: std.ArrayListUnmanaged(u32) = .{},

                // The returned pointer is _not_ stable! Further allocations can invalidate it.
                pub inline fn at(this: *@This(), id: u32) *T {
                    return &this.items.items[id];
                }

                pub fn create(this: *@This(), a: std.mem.Allocator, v: T) !u32 {
                    if (this.free_list.items.len > 0) {
                        const id = this.free_list.pop();
                        this.items.items[id].* = v;
                        this.items.items[id].id = id;
                        return id;
                    }

                    const id: u32 = @intCast(this.items.items.len);
                    try this.items.append(a, v);
                    this.items.items[id].id = id;

                    return id;
                }

                pub fn destroy(this: *@This(), a: std.mem.Allocator, id: u32) !void {
                    std.debug.assert(this.items.items.len > 0);

                    if (this.items.items.len-1 == id) {
                        _ = this.items.pop();
                        return;
                    }

                    // opportunistic defragment
                    while (
                        this.items.items.len > 0 and 
                        this.free_list.items.len > 0 and 
                        this.items.items.len-1 == this.free_list.getLast()
                    ) {
                        _ = this.items.pop();
                        _ = this.free_list.pop();
                        if (id >= this.items.items.len) {
                            return;
                        }
                    }
                    
                    try this.free_list.append(a, id);
                    return;
                }

                pub fn deinit(this: @This(), a: std.mem.Allocator) void {
                    this.items.deinit(a);
                    this.free_list.deinit(a);
                }
            };
        }

        const Edge = packed struct {
            const Mode = enum(u3) {
                unconditional,
                branching,
            };

            const Flags = enum(u5) {
                node_subject = 1 << 0, // interpret `subject` as a node ref
            };

            // Specialized flags
            const BranchingFlags = enum(u5) {
                nullish = 1 << 3,
                true_branch = 1 << 4, // 0 implies false
            };

            mode: Mode,
            flags: u5,
            antecedent: u24, // truncates the node ID, biased by 1
            subject: u32, // this is a symbol by default
            claim: u32,
        };

        // Stores flow state at some point in the code
        // Antecedents link back to preconditions
        const Label = struct {
            const Types = SimpleHashMap; // std.AutoArrayHashMapUnmanaged(SymbolRef, TypeRef);
            const Antecedents = std.ArrayListUnmanaged(*Label);

            types: Types = .{},
            antecedents: Antecedents = .{},
            //node_types: SimpleHashMap = .{},

            // True if the flow never joins back into the current flow
            terminal: bool = false,

            on_stack: bool = false,
            ref_count: u8 = 0,

            pub fn create(a: std.mem.Allocator) !*@This() {
                const self = try a.create(@This());
                self.types = .{};
                //self.node_types = .{};
                self.antecedents = .{};
                self.terminal = false;
                self.on_stack = false;
                self.ref_count = 0;
                return self;
            }

            pub fn create2(a: anytype) !*@This() {
                const self = try a.create();
                self.types = .{};
                //self.node_types = .{};
                self.antecedents = .{};
                self.terminal = false;
                self.on_stack = false;
                self.ref_count = 0;
                return self;
            }

            pub fn destroy(self: *@This(), a: std.mem.Allocator) void {
                self.types.deinit(a);
                //self.node_types.deinit(a);
                self.antecedents.deinit(a);
                // a.destroy(self);
            }

            pub fn addAntecedent(self: *@This(), a: std.mem.Allocator, label: *Label) !void {
                if (label.on_stack or label.terminal) return;

                if (comptime is_debug) {
                    for (self.antecedents.items) |l| {
                        if (l == label) return error.DuplicateLabel;
                    }
                }

                label.ref_count += 1;
                try self.antecedents.append(a, label);
            }

            pub fn maybeGetLinearType(self: *const @This(), sym: SymbolRef) ?TypeRef {
                if (self.types.get(sym)) |t| return t;
                if (self.antecedents.items.len != 1) return null;

                return self.antecedents.items[0].maybeGetLinearType(sym);
            }

            pub fn destroyAll(self: *@This(), typer: *FlowTyper) void {
                std.debug.assert(!self.on_stack);
                std.debug.assert(self.ref_count == 0);
                for (self.antecedents.items) |l| {
                    l.ref_count -= 1;
                    if (l.on_stack) continue;
                    if (l.ref_count == 0) {
                        l.destroyAll(typer);
                    }
                }
                self.destroy(typer.analyzer.allocator());
                typer.analyzer.label_pool.destroy(self);
            }

            pub inline fn ref(self: *@This()) void {
                self.ref_count += 1;
            }

            pub inline fn unref(self: *@This(), typer: *FlowTyper) void {
                self.ref_count -= 1;
                if (self.ref_count == 0) {
                    self.destroyAll(typer);
                }
            }

            pub fn printSymbols(self: *@This(), typer: *const FlowTyper) void {
                self._printSymbols(typer, 0);
            }

            fn _indent(indent: u32) void {
                for (0..indent+1) |_| {
                    std.debug.print(" ", .{});
                }
            }

            fn _printSymbols(self: *@This(), typer: *const FlowTyper, indent: u32) void {
                if (self.terminal) {
                    _indent(indent);
                    std.debug.print("[terminal] {} antecedents\n", .{self.antecedents.items.len});
                } else {
                    _indent(indent);
                    std.debug.print("[continuation] {} antecedents\n", .{self.antecedents.items.len});
                }

                var l_iter = self.types.iterator();
                while (l_iter.next()) |entry| {
                    _indent(indent);
                    printSymbol(typer.file, entry.key_ptr.*);
                    _indent(indent);
                    std.debug.print("  --> ", .{});
                    //typer.analyzer.printTypeInfo(entry.value_ptr.*);
                    typer.analyzer.debugPrint(entry.value_ptr.*) catch unreachable;
                    std.debug.print("\n", .{});
                }

                for (self.antecedents.items) |l| {
                    l._printSymbols(typer, indent + 2);
                }
            }
        };

        const Errors = std.ArrayListUnmanaged(FileDiagnostic);
        const LabelStack = std.ArrayListUnmanaged(*Label);

        analyzer: *Analyzer,
        file: *ParsedFileData,
        stack: LabelStack = LabelStack{},

        // must be initialized upon starting flow analysis
        // if the current flow is a lexical scope, this will be at the top of the stack
        current_flow: *Label = undefined,

        // these are set by various visitors
        // each visitor is responsible for restoring the prior value
        break_target: ?*Label = null,
        continue_target: ?*Label = null,
        return_target: ?*Label = null,
        exception_target: ?*Label = null,
        true_target: ?*Label = null,
        false_target: ?*Label = null,

        in_nullish_context: bool = false,
        is_expression_statement: bool = false,

        current_symbols: ?*SymbolSet = null,

        pub fn init(analyzer: *Analyzer, file: *ParsedFileData) FlowTyper {
            return .{
                .analyzer = analyzer,
                .file = file,
            };
        }

        pub fn visitArrowFnBody(self: *@This(), start: parser.NodeRef) !TypeRef {
            self.current_flow = try self.createLabel();
            try self.stack.append(self.analyzer.allocator(), self.current_flow);
            self.current_flow.on_stack = true;
            return self.visitExpression(start);
        }

        pub fn visitFunctionLike(self: *@This(), first_statement: parser.NodeRef) !TypeRef {
            if (first_statement == 0) {
                return @intFromEnum(Kind.void);
            }

            try self.visitEntrypoint(first_statement, true);
            const last = self.stack.pop();
            const rt = self.return_target orelse unreachable;
            if (!last.terminal) {
                if (rt.types.count == 0) {
                    return @intFromEnum(Kind.void);
                }
                const dst = try rt.types.getOrPut(self.analyzer.allocator(), 100_000_000);
                if (dst.found_existing) {
                    const v = dst.value_ptr.*;
                    if (dst.value_ptr.* != @intFromEnum(Kind.void)) {
                        dst.value_ptr.* = try self.analyzer.toUnion(&.{ v, @intFromEnum(Kind.undefined) });
                    } else {
                        dst.value_ptr.* = try self.analyzer.toUnion(&.{ v, @intFromEnum(Kind.void) });
                    }
                }
            }
            const result = rt.types.get(100_000_000) orelse @intFromEnum(Kind.void);
            self.return_target = null;
            rt.destroyAll(self);
            last.on_stack = false;
            last.destroyAll(self);
            return result;
        }

        // This should only be called with the first statement of a module or function.
        // Do not use the same struct for a different entrypoint. No re-entrancy.
        fn visitEntrypoint(self: *@This(), target: parser.NodeRef, comptime can_return: bool) !void {
            std.debug.assert(target != 0);
            std.debug.assert(self.stack.items.len == 0);

            self.current_flow = try self.createLabel();

            try self.stack.append(self.analyzer.allocator(), self.current_flow);
            self.current_flow.on_stack = true;

            if (can_return) {
                // Stores return types, use the nil symbol (id 0) for adding types. May be optimized later
                self.return_target = try self.createLabel();
            }

            try self.visitStatements(target);
        }

        fn visitStatements(self: *@This(), start: parser.NodeRef) anyerror!void {
            // all statements iterated belong to the same flow
            var iter = NodeIterator.init(&self.file.ast.nodes, start);
            while (iter.next()) |s| {
                switch (s.kind) {
                    // --- control flow ---

                    .break_statement => {
                        try self.visitBreakStatement(s);
                    },

                    .continue_statement => {
                        // TODO
                        self.current_flow.terminal = true;
                    },

                    .throw_statement => {
                        // TODO
                        self.current_flow.terminal = true;
                    },

                    .return_statement => {
                        try self.visitReturnStatement(s);
                    },

                    // --- constructs ---
                    .block => {
                        try self.visitBlock(s);
                    },
                    
                    .if_statement => {
                        const d = getPackedData(s);
                        const condition_ref = d.left;
                        const then_ref = d.right;
                        const else_ref: ?parser.NodeRef = if (s.len != 0) s.len else null;
 
                        // --- Fast path ---
                        if (else_ref == null) {
                            if (try self.maybeVisitSimpleIfStatement(condition_ref, then_ref)) |_| continue;
                        }

                        const then_label = try self.createLabel();
                        const else_label = try self.createLabel();

                        then_label.ref();
                        else_label.ref();

                        const prev_true_target = self.true_target;
                        const prev_false_target = self.false_target;

                        self.true_target = then_label;
                        self.false_target = else_label;

                        _ = try self.visitCondition(condition_ref);

                        self.true_target = prev_true_target;
                        self.false_target = prev_false_target;

                        try self.unifyAntecedents(then_label);
                        try self.unifyAntecedents(else_label);

                        const prev_flow = self.current_flow;
                        self.current_flow = then_label;
                        try self.visitStatements(then_ref);

                        self.current_flow = else_label;
                        if (else_ref) |else_stmt| {
                            try self.visitStatements(else_stmt);
                        }

                        self.current_flow = prev_flow;
                        if (!then_label.terminal and !else_label.terminal) {
                            try self.joinLabelsInto(self.current_flow, then_label, else_label);
                        } else if (!then_label.terminal) {
                            try self.writeOver(self.current_flow, then_label);
                        } else if (!else_label.terminal) {
                            try self.writeOver(self.current_flow, else_label);
                        } else {
                            self.current_flow.terminal = true;
                        }

                        then_label.unref(self);
                        else_label.unref(self);
                    },

                    .while_statement => {
                        const d = getPackedData(s);
                        const condition_ref = d.left;
                        const body_ref = d.right;

                        var assigned_syms = std.AutoArrayHashMapUnmanaged(parser.SymbolRef, void){};
                        defer assigned_syms.deinit(self.analyzer.allocator());

                        var scanner = AssignmentScanner{
                            .file = self.file,
                            .assigned = &assigned_syms,
                            .allocator = self.analyzer.allocator(),
                        };
                        try scanner.scan(body_ref);

                        for (assigned_syms.keys()) |sym| {
                            const declared_type = self.analyzer.getTypeOfSymbol(self.file, sym) catch continue;
                            try self.current_flow.types.put(self.analyzer.allocator(), sym, declared_type);
                        }

                        const body_label = try self.createLabel();
                        body_label.ref();
                        const exit_label = try self.createLabel();
                        exit_label.ref();

                        const prev_break_target = self.break_target;
                        const prev_continue_target = self.continue_target;
                        self.break_target = exit_label;
                        self.continue_target = body_label;

                        const prev_true_target = self.true_target;
                        const prev_false_target = self.false_target;
                        self.true_target = body_label;
                        self.false_target = exit_label;

                        _ = try self.visitCondition(condition_ref);

                        self.true_target = prev_true_target;
                        self.false_target = prev_false_target;

                        try self.unifyAntecedents(body_label);

                        const prev_flow = self.current_flow;
                        self.current_flow = body_label;
                        try self.visitStatements(body_ref);
                        self.current_flow = prev_flow;

                        self.break_target = prev_break_target;
                        self.continue_target = prev_continue_target;

                        try self.unifyAntecedents(exit_label);
                        try self.writeOver(self.current_flow, exit_label);

                        body_label.unref(self);
                        exit_label.unref(self);
                    },

                    .switch_statement => {
                        const d = getPackedData(s);
                        const switch_expr = self.file.ast.nodes.at(d.left);

                        // For now, only handle switching over an identifier
                        if (switch_expr.kind != .identifier) {
                            var clause_iter = NodeIterator.init(&self.file.ast.nodes, d.right);
                            while (clause_iter.next()) |clause| {
                                if (clause.len != 0) {
                                    try self.visitStatements(clause.len);
                                }
                            }
                            continue;
                        }

                        const sym = self.file.binder.getSymbol(d.left) orelse continue;
                        const switch_type = try self.getFlowType(d.left, sym);

                        const join_label = try self.createLabel();
                        join_label.ref();
                        defer join_label.unref(self);

                        const prev_break_target = self.break_target;
                        self.break_target = join_label;

                        var remaining_type = switch_type;
                        var has_default = false;
                        var all_terminal = true;

                        var case_types = TempUnion.init(self.analyzer.allocator());
                        var current_label = try self.createLabel();
                        current_label.ref();

                        var clause_iter = NodeIterator.init(&self.file.ast.nodes, d.right);
                        while (clause_iter.next()) |clause| {
                            if (clause.kind == .default_clause) {
                                has_default = true;
                                // Finalize any accumulated cases first
                                if (case_types.elements.items.len > 0) {
                                    const group_type = try case_types.complete(self.analyzer);
                                    remaining_type = try self.analyzer.excludeType(remaining_type, group_type);
                                    const l2 = try self.createLabel();
                                    try current_label.addAntecedent(self.analyzer.allocator(), l2);
                                    try l2.types.put(self.analyzer.allocator(), sym, group_type);
                                    case_types = TempUnion.init(self.analyzer.allocator());
                                }
                                try current_label.types.put(self.analyzer.allocator(), sym, remaining_type);
                            } else {
                                const case_expr_ref = unwrapRef(clause);
                                const case_type = try self.analyzer.getTypeAsConst(self.file, case_expr_ref);
                                _ = try self.analyzer.addToUnion(&case_types, case_type);
                            }

                            if (clause.len == 0 and clause_iter.ref > 0) continue;

                            if (case_types.elements.items.len > 0) {
                                const group_type = try case_types.complete(self.analyzer);
                                remaining_type = try self.analyzer.excludeType(remaining_type, group_type);
                                const l2 = try self.createLabel();
                                try current_label.addAntecedent(self.analyzer.allocator(), l2);
                                try l2.types.put(self.analyzer.allocator(), sym, group_type);     
                                case_types = TempUnion.init(self.analyzer.allocator());
                            }

                            try self.unifyAntecedents(current_label);

                            const prev_flow = self.current_flow;
                            self.current_flow = current_label;

                            try self.visitStatements(clause.len);
                            if (clause_iter.ref == 0) {
                                self.current_flow = prev_flow;
                                break;
                            }

                            const next_label = try self.createLabel();
                            next_label.ref();

                            if (!current_label.terminal) {
                                all_terminal = false;
                                try next_label.antecedents.append(self.analyzer.allocator(), current_label);
                            }

                            //current_label.unref(self);
                            self.current_flow = prev_flow;
                            current_label = next_label;
                        }

                        if (!current_label.terminal) {
                            all_terminal = false;
                            try join_label.antecedents.append(self.analyzer.allocator(), current_label);
                        }

                        self.break_target = prev_break_target;
                        const is_exhaustive = remaining_type == @intFromEnum(Kind.never);
                        if (is_exhaustive and all_terminal and join_label.antecedents.items.len == 0) {
                            self.current_flow.terminal = true;
                            try self.current_flow.types.put(self.analyzer.allocator(), sym, remaining_type);
                            continue;
                        }

                        if (!has_default and !is_exhaustive) {
                            const l2 = try self.createLabel();
                            try join_label.addAntecedent(self.analyzer.allocator(), l2);
                            try l2.types.put(self.analyzer.allocator(), sym, remaining_type);
                            all_terminal = false;
                        }

                        if (all_terminal and has_default) {
                            self.current_flow.terminal = true;
                        } else {
                            try self.unifyAntecedents(join_label);
                            try self.writeOver(self.current_flow, join_label);
                        }

                        //current_label.unref(self);
                    },

                    .for_statement => {
                        // TODO
                    },

                    .for_of_statement => {
                        // TODO
                    },

                    // ignore labeled statement for now
                    .labeled_statement => {},
                    
                    // --- effects ---

                    // ignore fn/class declarations for now, but we will eventually want to associate them 
                    // with the current flow with pessimistic invalidations on scope exit so that they 
                    // retain useful narrowings
                    .function_declaration => {},
                    .class_declaration => {},

                    .variable_statement => {
                        const is_const = s.hasFlag(.@"const");
                        var decls = NodeIterator.init(&self.file.ast.nodes, maybeUnwrapRef(s) orelse continue);
                        while (decls.next()) |decl| {
                            const d = getPackedData(decl);

                            const sym = self.file.binder.getSymbol(d.left) orelse continue;
                            if (self.current_symbols) |m| {
                                try m.put(self.analyzer.allocator(), sym, {});
                            }

                            // Respect the annotated type, do not try to narrow
                            if (is_const and decl.len != 0) continue;

                            if (d.right == 0) continue;

                            const init_type = try self.visitExpression(d.right);
                            try self.current_flow.types.put(self.analyzer.allocator(), sym, init_type);
                        }
                    },

                    .expression_statement => {
                        self.is_expression_statement = true;
                        const exp = unwrapRef(s);
                        _ = try self.visitExpression(exp); 
                    },
                    else => {},
                }
            }
        }

        fn visitBlock(self: *@This(), s: *const AstNode) !void {
            const block_start = maybeUnwrapRef(s) orelse return;

            const is_unconditional = self.stack.getLast() == self.current_flow;

            if (is_unconditional) {
                self.current_flow = try self.createLabel();
                self.current_flow.ref();
            }

            const old_symbols = self.current_symbols;
            var symbols = SymbolSet{};
            self.current_symbols = &symbols;

            try self.stack.append(self.analyzer.allocator(), self.current_flow);
            self.current_flow.on_stack = true;
            try self.visitStatements(block_start);

            const flow = self.stack.pop();
            flow.on_stack = false;

            // only prune if it's likely that the flow propagates
            if (!flow.terminal or flow.ref_count > 1) {
                var iter = symbols.iterator();
                while (iter.next()) |entry| {
                    _ = flow.types.swapRemove(entry.key_ptr.*);
                }
            }

            symbols.clearAndFree(self.analyzer.allocator());

            if (is_unconditional) {
                self.current_flow = self.stack.getLast();
                if (!flow.terminal) {
                    try self.writeOver(self.current_flow, flow);
                }

                self.current_flow.terminal = flow.terminal;
                flow.unref(self);
            }

            self.current_symbols = old_symbols;
        }

        fn visitBreakStatement(self: *@This(), s: *const AstNode) !void {
            _ = s;
            if (self.break_target) |l| {
                try self.unifyAntecedents(self.current_flow);
                const on_stack = self.current_flow.on_stack;
                self.current_flow.on_stack = false;
                try l.addAntecedent(self.analyzer.allocator(), self.current_flow);
                self.current_flow.on_stack = on_stack;
            }
            self.current_flow.terminal = true;
        }

        fn visitReturnStatement(self: *@This(), s: *const AstNode) !void {
            self.current_flow.terminal = true;

            const return_type = if (maybeUnwrapRef(s)) |exp_ref|
                try self.visitExpression(exp_ref)
            else 
                @intFromEnum(Kind.void);

            if (self.return_target) |target| {
                const dst = try target.types.getOrPut(self.analyzer.allocator(), 100_000_000);
                if (!dst.found_existing) {
                    dst.value_ptr.* = return_type;
                    return;
                }

                const val = dst.value_ptr.*;
                if (return_type == @intFromEnum(Kind.void) and val == @intFromEnum(Kind.void)) return;
                if (return_type != @intFromEnum(Kind.void) and val == @intFromEnum(Kind.void)) {
                    dst.value_ptr.* = try self.analyzer.toUnion(&.{ return_type, @intFromEnum(Kind.undefined) });
                    return;
                }
                if (return_type == @intFromEnum(Kind.void) and val != @intFromEnum(Kind.void)) {
                    dst.value_ptr.* = try self.analyzer.toUnion(&.{ val, @intFromEnum(Kind.undefined) });
                    return;
                }
                
                dst.value_ptr.* = try self.analyzer.toUnion(&.{ val, return_type });
            }
        }

        fn maybeVisitSimpleIfStatement(self: *@This(), condition_ref: NodeRef, then_ref: NodeRef) !?void {
            // TODO:
            // * binary exp (ident with literals)
            const cond = self.file.ast.nodes.at(condition_ref);
            switch (cond.kind) {
                .prefix_unary_expression,
                .identifier => {},

                else => return null,
            }


            const statement = self.file.ast.nodes.at(then_ref);
            switch (statement.kind) {
                .throw_statement,
                .continue_statement,
                .break_statement,
                .return_statement => {},
                .block => {
                    if (maybeUnwrapRef(statement)) |r| {
                        const inner = self.file.ast.nodes.at(r);
                        if (inner.next != 0) return null;
                        switch (inner.kind) {
                            .throw_statement,
                            .continue_statement,
                            .break_statement,
                            .return_statement => {},
                            else => return null,
                        }
                    }
                },
                else => return null,
            }

            switch (cond.kind) {
                .identifier => {
                    const sym = self.file.binder.getSymbol(condition_ref) orelse return null;
                    const t = try self.getFlowType(condition_ref, sym);
                    const truthy_type = try self.analyzer.truthyType(t);
                    const falsy_type = try self.analyzer.excludeType(t, truthy_type);
                    try self.visitSimpleCondition(then_ref, sym, truthy_type, falsy_type);
                },
                .prefix_unary_expression => {
                    const d2 = getPackedData(cond);
                    if (d2.left == @intFromEnum(SyntaxKind.exclamation_token)) {
                        if (self.file.binder.getSymbol(d2.right)) |sym| {
                            const t = try self.getFlowType(d2.right, sym);
                            const truthy_type = try self.analyzer.truthyType(t);
                            const falsy_type = try self.analyzer.excludeType(t, truthy_type);
                            try self.visitSimpleCondition(then_ref, sym, falsy_type, truthy_type);
                        }
                    } else {
                        return null;
                    }
                },
                else => return null,
            }
        }

        fn visitSimpleCondition(self: *@This(), node: NodeRef, sym: SymbolRef, true_type: TypeRef, false_type: TypeRef) !void {
            const then_label = try self.createLabel();
            then_label.ref();
            try then_label.types.put(self.analyzer.allocator(), sym, true_type);

            const old_flow = self.current_flow;
            self.current_flow = then_label;

            const n = self.file.ast.nodes.at(node);
            switch (n.kind) {
                .throw_statement,
                .continue_statement => {
                    self.current_flow.terminal = true;
                },
                .break_statement => try self.visitBreakStatement(n),
                .return_statement => try self.visitReturnStatement(n),
                .block => try self.visitBlock(n),
                else => _ = try self.visitExpression(node),
            }

            self.current_flow = old_flow;
            if (!then_label.terminal) {
                // TODO: handle `if (!a) a = 1`
            } else {
                try self.current_flow.types.put(self.analyzer.allocator(), sym, false_type);
            }

            then_label.unref(self);
        }

        // Note that we don't have symbols for individual members, so narrowing always affects the root symbol
        fn visitExpression(self: *@This(), ref: parser.NodeRef) anyerror!TypeRef {
            const node = self.file.ast.nodes.at(ref);

            return switch (node.kind) {
                .identifier => {
                    const sym = self.file.binder.getSymbol(ref) orelse return error.MissingSymbol;
                    return try self.getFlowType(ref, sym);
                },

                .parenthesized_expression => {
                    const inner = unwrapRef(node);
                    return self.visitExpression(inner);
                },

                .prefix_unary_expression => {
                    const d = getPackedData(node);
                    switch (@as(SyntaxKind, @enumFromInt(d.left))) {
                        // .plus_plus_token => {
                        //     return error.TODO;
                        // },
                        // .minus_minus_token => {
                        //     return error.TODO;
                        // },
                        .plus_token => {
                            _ = try self.analyzer.getTypeAsConst(self.file, ref);
                            
                            return @intFromEnum(Kind.number);
                        },
                        .minus_token => {
                            // assume numerical literal
                            return try self.analyzer.getTypeAsConst(self.file, ref);
                        },
                        else => {
                            return try self.analyzer.getType(self.file, ref);
                        },
                    }
                },

                // TODO: somewhat wasteful, exact types should only be used in specific cases
                .string_literal,
                .numeric_literal => {
                    return self.analyzer.getTypeAsConst(self.file, ref);
                },

                .binary_expression => {
                    const op = @as(SyntaxKind, @enumFromInt(node.len));
                    switch (op) {
                        .equals_token => {
                            const d = getPackedData(node);
                            const sym = self.file.binder.getSymbol(d.left) orelse return error.MissingSymbol;
                            const t = try self.visitExpression(d.right);
                            try self.current_flow.types.put(self.analyzer.allocator(), sym, t);

                            return t;
                        },
                        .question_question_equals_token => {
                            const d = getPackedData(node);
                            const sym = self.file.binder.getSymbol(d.left) orelse return error.MissingSymbol;
                            const a_type = try self.getFlowType(d.left, sym);
                            const a_nonnullable = try self.analyzer.nonNullable(a_type);

                            const b_type = try self.visitExpression(d.right);
                            const t = try self.analyzer.toUnion(&.{ a_nonnullable, b_type });
                            try self.current_flow.types.put(self.analyzer.allocator(), sym, t);

                            return t;
                        },
                        .bar_bar_token,
                        .question_question_token,
                        .ampersand_ampersand_token => {
                            const is_statement = self.is_expression_statement;
                            self.is_expression_statement = false;

                            // Create a label to capture narrowings from both branches
                            const effect_label = try self.createLabel();
                            effect_label.ref_count += 1;

                            const prev_true = self.true_target;
                            const prev_false = self.false_target;

                            self.true_target = effect_label;
                            self.false_target = effect_label;

                            const result_type = try self.visitCondition(ref);

                            self.true_target = prev_true;
                            self.false_target = prev_false;

                            if (is_statement) {
                                try self.unifyAntecedents(effect_label);
                                try self.writeOver(self.current_flow, effect_label);
                            }

                            effect_label.ref_count -= 1;
                            effect_label.destroyAll(self);

                            return result_type;
                        },
                        .slash_token,
                        .minus_token,
                        .plus_token,
                        .asterisk_token => {
                            const d = getPackedData(node);
                            _ = try self.visitExpression(d.left);
                            _ = try self.visitExpression(d.right);
                            return @intFromEnum(Kind.number);
                        },
                        else => {},
                    }
                    return try self.analyzer.getType(self.file, ref);
                },

                else => {
                    // For other expressions, just get the type directly
                    return try self.analyzer.getType(self.file, ref);
                },
            };
        }

        fn bindPropertyCondition(self: *@This(), sym: SymbolRef, subject: TypeRef, prop_name: TypeRef, true_type: TypeRef, false_type: TypeRef) !void {
            const refined1 = try self.analyzer.createRefinement(subject, .{
                .name = prop_name,
                .type = true_type,
            });

            const refined2 = try self.analyzer.createRefinement(subject, .{
                .name = prop_name,
                .type = false_type,
            });

            try self.bindConditions(sym, refined1, refined2);

            return;
        }

        // fn visitReference(self: *@This(), ref: NodeRef) anyerror!TypeRef {

        // }

        fn visitCondition(self: *@This(), ref: NodeRef) anyerror!TypeRef {
            const node = self.file.ast.nodes.at(ref);

            switch (node.kind) {
                .parenthesized_expression => {
                    const inner = unwrapRef(node);
                    return self.visitCondition(inner);
                },

                .identifier => {
                    const sym = self.file.binder.getSymbol(ref) orelse return error.MissingSymbol;

                    const current_type = try self.getFlowType(ref, sym);
                    if (current_type == @intFromEnum(Kind.any)) return current_type;
                    // TODO: or string/number ?

                    // In nullish context (??) we narrow by null/undefined, otherwise by truthiness
                    const true_type = if (self.in_nullish_context)
                        try self.analyzer.nonNullable(current_type)
                    else
                        try self.analyzer.truthyType(current_type);

                    const false_type = try self.analyzer.excludeType(current_type, true_type);

                    try self.bindConditions(sym, true_type, false_type);

                    return current_type;
                },

                // .property_access_expression => {

                // },

                .binary_expression => {
                    const d = getPackedData(node);
                    const op = @as(SyntaxKind, @enumFromInt(node.len));

                    switch (op) {
                        .equals_token => {
                            const sym = self.file.binder.getSymbol(d.left) orelse return error.MissingSymbol;
                            const t = try self.visitExpression(d.right);
                            if (t == @intFromEnum(Kind.zero)) {
                                try self.bindConditions(sym, @intFromEnum(Kind.never), t);
                                return t;
                            }

                            const falsy_type = try self.analyzer.falsyType(t);
                            try self.bindConditions(sym, t, falsy_type);

                            return t;
                        },
                        .ampersand_ampersand_token => {
                            // For `a && b`:
                            // - Result type is union of (a's falsy type, b's type)
                            // - true_target gets: a is truthy AND b is truthy
                            // - false_target gets: a is falsy OR b is falsy
                            const a_true = try self.createLabel();
                            a_true.ref_count += 1;

                            const prev_true = self.true_target;
                            const prev_false = self.false_target;

                            self.true_target = a_true;
                            const a_type = try self.visitCondition(d.left);
                            const a_falsy = try self.analyzer.falsyType(a_type);

                            self.true_target = prev_true;
                            self.false_target = prev_false;

                            try self.unifyAntecedents(a_true);

                            const saved_flow = self.current_flow;
                            self.current_flow = a_true;
                            const b_type = try self.visitCondition(d.right);
                            self.current_flow = saved_flow;
                            a_true.ref_count -= 1;
                            if (a_true.ref_count == 0) {
                                try self.addAntecedent(self.true_target.?, a_true);
                            }

                            return try self.analyzer.toUnion(&.{ a_falsy, b_type });
                        },

                        .bar_bar_token => {
                            // For `a || b`:
                            // - Result type is union of (a's truthy type, b's type)
                            // - true_target gets: a is truthy OR b is truthy
                            // - false_target gets: a is falsy AND b is falsy
                            const a_false = try self.createLabel();
                            a_false.ref_count += 1;

                            const prev_true = self.true_target;
                            const prev_false = self.false_target;

                            self.false_target = a_false;
                            const a_type = try self.visitCondition(d.left);
                            const a_truthy = try self.analyzer.truthyType(a_type);

                            self.true_target = prev_true;
                            self.false_target = prev_false;

                            if (a_type == a_truthy) {
                                if (a_type >= @intFromEnum(Kind.false)) {
                                    if (a_type == @intFromEnum(Kind.true)) {
                                        a_false.ref_count -= 1;
                                        return a_type;
                                    }
                                } else {
                                    const n = self.analyzer.types.at(a_type);
                                    switch (n.getKind()) {
                                        .@"union" => {
                                            if (n.flags & (@intFromEnum(UnionFlags.has_string) | @intFromEnum(UnionFlags.has_number)) == 0) {
                                                a_false.ref_count -= 1;
                                                return a_type;
                                            }
                                        },
                                        else => {},
                                    }
                                }
                            }

                            try self.unifyAntecedents(a_false);

                            const saved_flow = self.current_flow;
                            self.current_flow = a_false;
                            const b_type = try self.visitCondition(d.right);
                            self.current_flow = saved_flow;
                            a_false.ref_count -= 1;
                            if (a_false.ref_count == 0) {
                                try self.addAntecedent(self.false_target.?, a_false);
                            }

                            return try self.analyzer.toUnion(&.{ a_truthy, b_type });
                        },

                        .question_question_token => {
                            // For `a ?? b`:
                            // - Result type is union of (a's non-nullable type, b's type)
                            // - Narrows by null/undefined instead of truthiness
                            // - If `a` is non-null, return `a`; otherwise evaluate and return `b`
                            const a_nullish = try self.createLabel();

                            const prev_true = self.true_target;
                            const prev_false = self.false_target;
                            const prev_nullish = self.in_nullish_context;

                            self.false_target = a_nullish;
                            self.in_nullish_context = true;
                            const a_type = try self.visitCondition(d.left);
                            const a_nonnullable = try self.analyzer.nonNullable(a_type);

                            self.true_target = prev_true;
                            self.false_target = prev_false;
                            self.in_nullish_context = prev_nullish;

                            try self.unifyAntecedents(a_nullish);

                            const saved_flow = self.current_flow;
                            self.current_flow = a_nullish;
                            const b_type = try self.visitCondition(d.right);
                            self.current_flow = saved_flow;

                            return try self.analyzer.toUnion(&.{ a_nonnullable, b_type });
                        },

                        .exclamation_equals_equals_token, .exclamation_equals_token,
                        .equals_equals_token, .equals_equals_equals_token => {
                            const negated: bool = switch (op) {
                                .exclamation_equals_equals_token, .exclamation_equals_token => true,
                                .equals_equals_token, .equals_equals_equals_token => false,
                                else => unreachable,
                            };

                            const lhs = self.file.ast.nodes.at(d.left);

                            switch (lhs.kind) {
                                .identifier => {
                                    const target = self.file.binder.getSymbol(d.left) orelse return error.MissingSymbol;
                                    const true_type = try self.analyzer.getTypeAsConst(self.file, d.right);
                                    if (true_type == 0) return error.TODO;

                                    const cur = try self.getFlowType(d.left, target);
                                    const false_type = try self.analyzer.excludeType(cur, true_type);

                                    if (negated) {
                                        try self.bindConditions(target, false_type, true_type);
                                    } else {
                                        try self.bindConditions(target, true_type, false_type);
                                    }
                                    return @intFromEnum(Kind.boolean);
                                },
                                .property_access_expression => {
                                    const d2 = getPackedData(lhs);
                                    const target = self.file.binder.getSymbol(d2.left) orelse return error.MissingSymbol;

                                    const true_type = try self.analyzer.getTypeAsConst(self.file, d.right);
                                    if (true_type == 0) return error.TODO;

                                    const cur = try self.getFlowType(d2.left, target);

                                    const r = try self.analyzer.propertyNameToType(self.file, d2.right);

                                    const member_type = try self.analyzer.accessType(cur, r);
                                    const false_type = try self.analyzer.excludeType(member_type, true_type);

                                    if (negated) {
                                        try self.bindPropertyCondition(target, cur, r, false_type, true_type);
                                    } else {
                                        try self.bindPropertyCondition(target, cur, r, true_type, false_type);
                                    }

                                    return @intFromEnum(Kind.boolean);
                                },
                                .type_of_expression => {
                                    const inner = unwrapRef(lhs);
                                    const lhs_node = self.file.binder.nodes.at(inner);
                                    if (lhs_node.kind == .property_access_expression) {
                                        //const t_ref = try self.analyzer.getTypeAsConst(self.file, d.right);
                                        const typeof_type = try self.analyzer.maybeGetTypeFromTypeOfNode(self.file, d.right) 
                                            orelse return @intFromEnum(Kind.boolean);

                                        const d2 = getPackedData(lhs_node);
                                        const target = self.file.binder.getSymbol(d2.left) orelse return error.MissingSymbol;

                                        const cur = try self.getFlowType(d2.left, target);
                                        if (cur == @intFromEnum(Kind.any) and !negated) {
                                            return @intFromEnum(Kind.boolean);
                                        }

                                        const r = try self.analyzer.propertyNameToType(self.file, d2.right);

                                        const member_type = try self.analyzer.accessType(cur, r);

                                        const y = try self.analyzer.intersectType(member_type, typeof_type);
                                        const y2 = try self.analyzer.excludeType(member_type, y);

                                        if (negated) {
                                            try self.bindPropertyCondition(target, cur, r, y2, y);
                                        } else {
                                            try self.bindPropertyCondition(target, cur, r, y, y2);
                                        }

                                        return @intFromEnum(Kind.boolean);
                                    }

                                    const target = self.file.binder.getSymbol(inner) orelse return error.MissingSymbol;

                                    // const t_ref = try self.analyzer.getTypeAsConst(self.file, d.right);
                                    const typeof_type = try self.analyzer.maybeGetTypeFromTypeOfNode(self.file, d.right) 
                                        orelse return @intFromEnum(Kind.boolean);

                                    const cur = try self.getFlowType(inner, target);
                                    if (cur == @intFromEnum(Kind.any) and !negated) {
                                        // TODO: set the type
                                        return @intFromEnum(Kind.boolean);
                                    }

                                    const y = try self.analyzer.intersectType(cur, typeof_type);
                                    const y2 = try self.analyzer.excludeType(cur, y);
                                    if (negated) {
                                        try self.bindConditions(target, y2, y);
                                    } else {
                                        try self.bindConditions(target, y, y2);
                                    }

                                    return @intFromEnum(Kind.boolean);
                                },
                                else => {
                                    return try self.visitExpression(ref);
                                },
                            }
                        },

                        else => {
                            // TODO: Handle equality comparisons for type narrowing
                            return try self.visitExpression(ref);
                        },
                    }
                },

                .prefix_unary_expression => {
                    const d = getPackedData(node);
                    const op = @as(parser.SyntaxKind, @enumFromInt(d.left));

                    if (op == .exclamation_token) {
                        const inner_node = self.file.ast.nodes.at(d.right);
                        if (inner_node.kind == .prefix_unary_expression and getPackedData(inner_node).left == @intFromEnum(parser.SyntaxKind.exclamation_token)) {
                            // small optimization for `!!`
                            const ty = try self.visitCondition(getPackedData(inner_node).right);
                            if (ty == @intFromEnum(Kind.true) or ty == @intFromEnum(Kind.false)) {
                                return ty;
                            }
                            return @intFromEnum(Analyzer.Kind.boolean);
                        }

                        // swap true_target and false_target
                        const prev_true = self.true_target;
                        const prev_false = self.false_target;

                        self.true_target = prev_false;
                        self.false_target = prev_true;
                        const ty = try self.visitCondition(d.right);

                        self.true_target = prev_true;
                        self.false_target = prev_false;

                        if (ty == @intFromEnum(Kind.false)) {
                            return @intFromEnum(Kind.true);
                        } else if (ty == @intFromEnum(Kind.true)) {
                            return @intFromEnum(Kind.false);
                        }

                        // Result of `!x` is always boolean otherwise
                        return @intFromEnum(Analyzer.Kind.boolean);
                    }

                    return try self.visitExpression(ref);
                },

                else => {
                    return try self.visitExpression(ref);
                },
            }
        }

        fn bindConditions(self: *@This(), sym: SymbolRef, true_type: TypeRef, false_type: TypeRef) !void {
            if (self.true_target) |target| {
                const label = try self.createLabel();
                try label.types.put(self.analyzer.allocator(), sym, true_type);
                try self.addAntecedent(label, self.current_flow);
                try self.addAntecedent(target, label);
            } else unreachable;

            if (self.false_target) |target| {
                const label = try self.createLabel();
                try label.types.put(self.analyzer.allocator(), sym, false_type);
                try self.addAntecedent(label, self.current_flow);
                try self.addAntecedent(target, label);
            } else unreachable;
        }

        fn writeOver(self: *@This(), target: *Label, source: *Label) !void {
            var iter = source.types.iterator();
            while (iter.next()) |entry| {
                const sym = entry.key_ptr.*;
                const source_type = entry.value_ptr.*;
                const dst = try target.types.getOrPut(self.analyzer.allocator(), sym);
                dst.value_ptr.* = source_type;
            }
        }

        fn joinLabelsInto(self: *@This(), target: *Label, left: *Label, right: *Label) !void {
            try self._unifyAntecedents(target, &.{left, right});
        }

        fn _unifyAntecedents(self: *@This(), target: *Label, antecedents: []const *Label) anyerror!void {
            var symbols = SymbolSet{};
            defer symbols.deinit(self.analyzer.allocator());
            
            for (antecedents) |l| {
                var iter = l.types.iterator();
                while (iter.next()) |entry| {
                    try symbols.put(self.analyzer.allocator(), entry.key_ptr.*, {});
                }
            }

            var iter = symbols.iterator();
            outer: while (iter.next()) |entry| {
                const sym = entry.key_ptr.*;
                
                var arr = std.ArrayListUnmanaged(TypeRef){};
                defer arr.deinit(self.analyzer.allocator());

                lp: for (antecedents) |l| {
                    const t = l.types.get(sym) 
                        orelse self.maybeGetFlowType(sym) 
                        orelse continue :outer;

                    for (arr.items) |ot| {
                        if (ot == t) continue :lp;
                    }
                    try arr.append(self.analyzer.allocator(), t);
                }

                const dst = try target.types.getOrPut(self.analyzer.allocator(), sym);

                if (arr.items.len == 1) {
                    dst.value_ptr.* = arr.items[0];
                    continue;
                }
                
                dst.value_ptr.* = try self.analyzer.toUnion(arr.items);
            }
        }

        fn unifyAntecedents(self: *@This(), target: *Label) anyerror!void {
            if (target.antecedents.items.len == 0) {
                return;
            }

            if (target.antecedents.items.len == 1) {
                const l = target.antecedents.pop();
                try self.unifyAntecedents(l);
                var iter = l.types.iterator();
                while (iter.next()) |entry| {
                    const sym = entry.key_ptr.*;
                    const source_type = entry.value_ptr.*;
                    const dst = try target.types.getOrPut(self.analyzer.allocator(), sym);
                    if (!dst.found_existing) {
                        dst.value_ptr.* = source_type;
                    }
                }
                l.unref(self);
                return;
            }

            for (target.antecedents.items) |l| {
                try self.unifyAntecedents(l);
            }

            try self._unifyAntecedents(target,target.antecedents.items);

            for (target.antecedents.items) |l| {
                l.unref(self);
            }

            target.antecedents.clearAndFree(self.analyzer.allocator());
        }

        // Scans an AST subtree to find symbols that are assigned
        const AssignmentScanner = struct {
            file: *ParsedFileData,
            assigned: *std.AutoArrayHashMapUnmanaged(parser.SymbolRef, void),
            allocator: std.mem.Allocator,

            fn scan(self: *@This(), ref: parser.NodeRef) !void {
                if (ref == 0) return;
                const node = self.file.ast.nodes.at(ref);
                try self.visit(node, ref);
            }

            pub fn visit(self: *@This(), node: *const parser.AstNode, ref: parser.NodeRef) anyerror!void {
                _ = ref;
                switch (node.kind) {
                    .binary_expression => {
                        const op = @as(parser.SyntaxKind, @enumFromInt(node.len));
                        if (op == .equals_token) {
                            const d = getPackedData(node);
                            const lhs = self.file.ast.nodes.at(d.left);
                            if (lhs.kind == .identifier) {
                                if (self.file.binder.getSymbol(d.left)) |sym| {
                                    try self.assigned.put(self.allocator, sym, {});
                                }
                            }
                        }
                        return parser.forEachChild(&self.file.ast.nodes, node, self);
                    },
                    .prefix_unary_expression, .postfix_unary_expression => {
                        const d = getPackedData(node);
                        const operand_ref = if (node.kind == .prefix_unary_expression) d.right else d.left;
                        const operand = self.file.ast.nodes.at(operand_ref);
                        if (operand.kind == .identifier) {
                            if (self.file.binder.getSymbol(operand_ref)) |sym| {
                                try self.assigned.put(self.allocator, sym, {});
                            }
                        }
                        return parser.forEachChild(&self.file.ast.nodes, node, self);
                    },
                    .function_declaration, .function_expression, .arrow_function => {},
                    else => return parser.forEachChild(&self.file.ast.nodes, node, self),
                }
            }
        };

        fn getFlowType(self: *@This(), ref: parser.NodeRef, sym: parser.SymbolRef) !TypeRef {
            return self.maybeGetFlowType(sym) orelse
                try self.analyzer.getType(self.file, ref);
        }

        pub fn maybeGetFlowType(self: *@This(), sym: parser.SymbolRef) ?TypeRef {
            if (self.current_flow.maybeGetLinearType(sym)) |t| return t;

            var i: usize = self.stack.items.len-1;

            // skip the current flow if needed
            if (self.current_flow == self.stack.items[i]) {
                if (i == 0) return null;
                i -= 1;
            }

            // see if we have a narrowed type in the stack 
            // could be optimized later, not all symbols need to check all frames
            while (true) {
                if (self.stack.items[i].types.get(sym)) |t| return t;

                if (i == 0) break;
                i -= 1;
            }

            return null;
        }

        inline fn createLabel(self: *@This()) !*Label {
            //return try Label.create(self.analyzer.allocator());
            return try Label.create2(&self.analyzer.label_pool);
        }

        inline fn addAntecedent(self: *@This(), target: *Label, label: *Label) !void {
            try target.addAntecedent(self.analyzer.allocator(), label);
        }
    };

    fn analyzeExpression(this: *@This(), file: *ParsedFileData, ref: NodeRef) anyerror!?std.AutoArrayHashMap(SymbolRef, AnalysisResult) {
        const exp = file.ast.nodes.at(ref);
        
        const _n = this.current_node;
        if (comptime is_debug) {
            this.current_node = .{file, ref};
        }
        defer this.current_node = _n;
        
        if (exp.kind == .parenthesized_expression) {
            return this.analyzeExpression(file, unwrapRef(exp));
        }

        if (exp.kind == .prefix_unary_expression) {
            const d = getPackedData(exp);
            switch (@as(SyntaxKind, @enumFromInt(d.left))) {
                .exclamation_token => {
                    const inner = file.ast.nodes.at(d.right);
                    // Unwrap `!!` expressions (TODO: should we just parse this as special syntax? it's somewhat common)
                    if (inner.kind == .prefix_unary_expression) {
                        const inner_data = getPackedData(inner);
                        if (inner_data.left == @intFromEnum(SyntaxKind.exclamation_token)) {
                            return this.analyzeExpression(file, inner_data.right);
                        }
                    }

                    var r = try this.analyzeExpression(file, d.right) orelse return null;
                    var iter = r.iterator();
                    while (iter.next()) |entry| {
                        r.putAssumeCapacity(entry.key_ptr.*, .{
                            .path_type = entry.value_ptr.termination_type,
                            .prior_type = entry.value_ptr.prior_type,
                            .termination_type = entry.value_ptr.path_type,
                            .continuation_type = entry.value_ptr.termination_type,
                        });
                    }

                    return r;
                },
                else => return null, // TODO?
            }
        }

        var result = std.AutoArrayHashMap(SymbolRef, AnalysisResult).init(this.type_args.allocator);
        errdefer result.deinit();

        switch (exp.kind) {
            .identifier => {
                const target = file.binder.getSymbol(ref) orelse return null;
                const cur = try this.getType(file, ref);
                if (cur == @intFromEnum(Kind.any)) return null;

                const t = try this.truthyType(cur);

                try result.put(target, .{
                    .path_type = t,
                    .prior_type = cur,
                    .continuation_type = t,
                    .termination_type = try this.excludeType(cur, t),
                });
            },
            .call_expression => {
                const t = try this.getType(file, ref);

                comptime {
                    if (@intFromEnum(Kind.false) != @intFromEnum(Kind.true) - 1) {
                        @compileError("False should be one less than true");
                    }
                }

                // TODO: we should be able to return the type of the exp so we can
                // know if a branch will ever be executed (or will always be executed)
                if (t == @intFromEnum(Kind.false)) {
                    return null;
                } else if (t == @intFromEnum(Kind.true)) {
                    return null;
                }

                if (t > @intFromEnum(Kind.true)) return null;

                const n = this.types.at(t);
                if (n.getKind() != .predicate) return null;

                if (n.slot4 == 1) return error.TODO; // TODO: asserts x is T

                var i: u32 = 0;
                const pos = n.slot2;
                var param_iter = NodeIterator.init(&file.ast.nodes, (getPackedData(exp)).right);
                while (param_iter.nextPair()) |pair| {
                    if (i < pos) {
                        i += 1;
                        continue;
                    }

                    const sym = file.binder.getSymbol(pair[1]) orelse return null;
                    const cur = try this.getType(file, pair[1]);

                    try result.put(sym, .{
                        .path_type = n.slot1,
                        .prior_type = cur,
                        .continuation_type = n.slot1,
                        .termination_type = try this.excludeType(cur, n.slot1),
                    });

                    break;
                }
            },
            .binary_expression => {
                const op = @as(SyntaxKind, @enumFromInt(exp.len));
                const d = getPackedData(exp);

                switch (op) {
                    .ampersand_ampersand_token => {
                        var lhs = try this.analyzeExpression(file, d.left) orelse return this.analyzeExpression(file, d.right);

                        try pushExpressionFrame(file);
                        defer file.cfa_state.pop().deinit();

                        var iter = lhs.iterator();
                        while (iter.next()) |entry| {
                            try currentCFAFrame(file).types.put(entry.key_ptr.*, entry.value_ptr.path_type);
                        }

                        var rhs = try this.analyzeExpression(file, d.right) orelse return lhs;
                        defer {
                            lhs.deinit();
                            rhs.deinit();
                        }

                        var rhs_iter = rhs.iterator();
                        while (rhs_iter.next()) |entry| {
                            try result.put(entry.key_ptr.*, entry.value_ptr.*);
                            _ = lhs.swapRemove(entry.key_ptr.*);
                        }

                        var lhs_iter = lhs.iterator();
                        while (lhs_iter.next()) |entry| {
                            try result.put(entry.key_ptr.*, entry.value_ptr.*);
                        }

                        return result;
                    },
                    .bar_bar_token => {
                        // TODO: the early returns are incorrect (when lhs or rhs don't contribute, we can't just return the remainder)
                        // The exceptions are when either expression are always true. In that case, returning the remainder is correct
                        var lhs = try this.analyzeExpression(file, d.left) orelse return this.analyzeExpression(file, d.right);

                        try pushExpressionFrame(file);
                        defer file.cfa_state.pop().deinit();

                        var iter = lhs.iterator();
                        while (iter.next()) |entry| {
                            try currentCFAFrame(file).types.put(entry.key_ptr.*, entry.value_ptr.termination_type);
                        }

                        var rhs = try this.analyzeExpression(file, d.right) orelse return lhs;
                        defer {
                            lhs.deinit();
                            rhs.deinit();
                        }

                        var rhs_iter = rhs.iterator();
                        while (rhs_iter.next()) |entry| {
                            if (lhs.get(entry.key_ptr.*)) |v| {
                                const r = try this.toUnion(&.{ v.path_type, entry.value_ptr.path_type });
                                try result.put(entry.key_ptr.*, .{
                                    .path_type = r,
                                    .prior_type = v.prior_type,
                                    .continuation_type = r,
                                    .termination_type = try this.excludeType(v.prior_type, r),
                                });
                                _ = lhs.swapRemove(entry.key_ptr.*);
                            }
                        }

                        var lhs_iter = lhs.iterator();
                        while (lhs_iter.next()) |entry| {
                            if (rhs.get(entry.key_ptr.*)) |v| {
                                const r = try this.toUnion(&.{ entry.value_ptr.path_type, v.path_type });
                                try result.put(entry.key_ptr.*, .{
                                    .path_type = r,
                                    .prior_type = entry.value_ptr.path_type,
                                    .continuation_type = r,
                                    .termination_type = try this.excludeType(entry.value_ptr.path_type, r),
                                });
                            }
                        }

                        if (result.count() == 0) return null;

                        return result;
                    },
                    else => {},
                }

                const is_negated: bool = switch (op) {
                    .exclamation_equals_equals_token, .exclamation_equals_token => true,
                    .equals_equals_token, .equals_equals_equals_token => false,
                    else => return null,
                };

                const lhs = file.ast.nodes.at(d.left);

                switch (lhs.kind) {
                    .identifier => {
                        const target = file.binder.getSymbol(d.left) orelse return null;
                        const t_ref = blk: {
                            const old_ctx = this.is_const_context;
                            this.is_const_context = true;
                            defer this.is_const_context = old_ctx;
                            break :blk try this.getType(file, d.right);
                        };
                        if (t_ref == 0) return error.TODO;
                        const cur = try this.getType(file, d.left);
                        const t = if (is_negated) try this.excludeType(cur, t_ref) else t_ref;

                        if (t != cur) {
                            try result.put(target, .{
                                .path_type = t,
                                .prior_type = cur,
                                .continuation_type = t,
                                .termination_type = try this.excludeType(cur, t),
                            });
                        }
                    },
                    .type_of_expression => {
                        const inner = unwrapRef(lhs);
                        const target = file.binder.getSymbol(inner) orelse return null;

                        // This exp is only relevant if the desired type provides narrowing
                        const t_ref = blk: {
                            const old_ctx = this.is_const_context;
                            defer this.is_const_context = old_ctx;
                            this.is_const_context = true;
                            break :blk try this.getType(file, d.right);
                        };

                        if (try this.getTypeFromTypeOf(t_ref)) |x| {
                            const cur = try this.getType(file, inner);
                            if (cur == @intFromEnum(Kind.any) and !is_negated) {
                                // We want to explicitly narrow the type in this case
                                try result.put(target, .{
                                    .path_type = x,
                                    .prior_type = cur,
                                    .continuation_type = x,
                                    // Ideally this should should literally be "everything but x" but
                                    // typescript cannot represent this
                                    .termination_type = cur,
                                });

                                return result;
                            }

                            const y = try if (is_negated) this.excludeType(cur, x) else this.intersectType(cur, x);
                            if (y != cur) {
                                try result.put(target, .{
                                    .path_type = y,
                                    .prior_type = cur,
                                    .continuation_type = y,
                                    .termination_type = try this.excludeType(cur, y),
                                });
                            } else {
                                try result.put(target, .{
                                    .path_type = y,
                                    .prior_type = cur,
                                    .continuation_type = y,
                                    .termination_type = @intFromEnum(Kind.never),
                                });
                            }
                        }
                    },
                    else => {},
                }
            },
            else => {},
        }

        if (result.count() == 0) return null;

        return result;
    }

    const use_one_pass_flow = true;

    fn analyzeBody(this: *@This(), file: *ParsedFileData, start: NodeRef) !TypeRef {
        if (use_one_pass_flow) {
            var f = FlowTyper.init(this, file);
            const old_flow = this.current_flow;
            defer this.current_flow = old_flow;
            this.current_flow = &f;
            return f.visitFunctionLike(start);
        }

        const prev = file.cfa_state.items.len;
        defer {
            if (prev != file.cfa_state.items.len) {
                for (prev + 1..file.cfa_state.items.len) |_| {
                    const frame = file.cfa_state.pop();
                    frame.deinit();
                }
            }
        }

        var return_types = std.ArrayList(TypeRef).init(file.ast.nodes.pages.allocator);
        defer return_types.deinit();

        var has_empty_return = false;

        try file.cfa_state.append(
            try CFAState.init(file.cfa_state.allocator, 
            NodeIterator.init(&file.ast.nodes, start))
        );

        outer: while (file.cfa_state.items.len > prev) {
            var c = file.cfa_state.items[file.cfa_state.items.len - 1];

            if (!c.is_branch) {
                if (c.branch_continuation_types) |t| {
                    c.iter.ref = c.tmp_start orelse c.iter.ref;
                    c.types = c.tmp_types orelse c.types;

                    defer {
                        // TODO: the deinit leaks
                        // c.branch_continuation_types.?.deinit();
                        c.is_unconditional = true;
                        c.branch_continuation_types = null;
                        c.tmp_start = null;
                        c.tmp_types = null;
                    }

                    var iter = t.iterator();
                    while (iter.next()) |entry| {
                        if (entry.value_ptr.items.len == 0) {
                            try c.types.put(entry.key_ptr.*, @intFromEnum(Kind.never));
                        } else {
                            const u = try this.toUnion(entry.value_ptr.items);
                            try c.types.put(entry.key_ptr.*, u);
                        }
                    }
                }
            }

            var did_terminate = false;

            inner: while (c.iter.next()) |s| {
                switch (s.kind) {
                    .return_statement => {
                        if (maybeUnwrapRef(s)) |exp| {
                            const return_type = try this.getType(file, exp);
                            // TODO: we should not do this if any symbols depend on CFA for accurate type info
                            if (return_type == @intFromEnum(Kind.any)) {
                                return return_type;
                            }

                            try return_types.append(return_type);
                        } else {
                            has_empty_return = true;
                        }

                        did_terminate = true;
                        break :inner;
                    },

                    .throw_statement => {
                        did_terminate = true;
                        break :inner;
                    },

                    .block => {
                        const start_node = maybeUnwrapRef(s) orelse continue;
                        try file.cfa_state.append(try CFAState.init(file.cfa_state.allocator, NodeIterator.init(&file.ast.nodes, start_node)));
                        continue :outer;
                    },

                    .if_statement => {
                        const d = getPackedData(s);
                        const start_node = unwrapConditionalStatement(file, d.right);
                        try file.cfa_state.ensureUnusedCapacity(1);

                        var state = try CFAState.init(
                            file.cfa_state.allocator,
                            NodeIterator.init(&file.ast.nodes, start_node),
                        );
                        state.is_unconditional = false;
                        state.branch = if (s.len != 0) unwrapConditionalStatement(file, s.len) else null;
                        if (state.branch != null) {
                            c.is_branch = true;
                        }

                        defer file.cfa_state.appendAssumeCapacity(state);
                        const r = try this.analyzeExpression(file, d.left) orelse continue :outer;

                        var iter = r.iterator();
                        while (iter.next()) |entry| {
                            try state.types.put(entry.key_ptr.*, entry.value_ptr.path_type);
                            try state.termination_types.put(entry.key_ptr.*, entry.value_ptr.termination_type);
                            try state.continuation_types.put(
                                entry.key_ptr.*,
                                if (c.is_branch) entry.value_ptr.continuation_type else entry.value_ptr.prior_type,
                            );
                        }

                        continue :outer; // Analyze the branch before moving forward
                    },
                    else => {},
                }
            }

            const frame = file.cfa_state.pop();
            defer frame.deinit();

            const addBranch = struct {
                pub fn f(a: *ParsedFileData, t: *const std.AutoArrayHashMap(SymbolRef, TypeRef)) !void {
                    var prev_frame2 = a.cfa_state.items[a.cfa_state.items.len - 1];

                    if (prev_frame2.branch_continuation_types == null) {
                        prev_frame2.branch_continuation_types = std.AutoArrayHashMap(SymbolRef, std.ArrayList(TypeRef)).init(a.cfa_state.allocator);
                    }

                    var arr = &(prev_frame2.branch_continuation_types orelse unreachable);
                    var iter = t.iterator();
                    while (iter.next()) |entry| {
                        const r = try arr.getOrPut(entry.key_ptr.*);
                        if (!r.found_existing) {
                            r.value_ptr.* = std.ArrayList(TypeRef).init(a.cfa_state.allocator);
                        }
                        try r.value_ptr.append(entry.value_ptr.*);
                    }
                }
            }.f;

            if (did_terminate and frame.is_unconditional) {
                if (file.cfa_state.items.len > 0) {
                    var prev_frame = file.cfa_state.items[file.cfa_state.items.len - 1];
                    // Remaining frame should not continue
                    prev_frame.iter.ref = 0;
                }

                continue;
            }

            if (!did_terminate and file.cfa_state.items.len == 0 and return_types.items.len > 0) {
                has_empty_return = true; // close enough
            }

            if (!did_terminate and frame.is_unconditional) {
                if (frame.types.count() > 0 and file.cfa_state.items.len > 0) {
                    var prev_frame = file.cfa_state.items[file.cfa_state.items.len - 1];
                    if (prev_frame.is_branch) {
                        try addBranch(file, &frame.types);
                    } else {
                        var iter = frame.types.iterator();
                        while (iter.next()) |entry| {
                            try prev_frame.types.put(entry.key_ptr.*, entry.value_ptr.*);
                        }
                    }
                }
            }

            if (did_terminate and !frame.is_unconditional) {
                if (frame.termination_types.count() > 0) {
                    var prev_frame = file.cfa_state.items[file.cfa_state.items.len - 1];
                    if (prev_frame.is_branch) {
                        if (frame.branch == null) {
                            try addBranch(file, &frame.termination_types);
                        }
                    } else {
                        var iter = frame.termination_types.iterator();
                        while (iter.next()) |entry| {
                            // Nested terminations within a conditional execution that narrow the same symbol
                            // should be excluded after leaving the original branch
                            //
                            // function f(x: string | 1 | 2 | 3) {
                            //     if (typeof x === 'number') if (x === 1) return;
                            //     x // string | 2 | 3
                            // }
                            //
                            // This is equivalent to the union of termination types

                            // TODO: this could be deferred until we know if the previous frame terminates or not
                            if (!prev_frame.is_unconditional) {
                                if (prev_frame.termination_types.get(entry.key_ptr.*)) |t| {
                                    const merged = try this.toUnion(&.{ t, entry.value_ptr.* });
                                    if (merged != 0 and merged != t) {
                                        try prev_frame.continuation_types.put(entry.key_ptr.*, merged);
                                    }
                                }
                            }

                            try prev_frame.types.put(entry.key_ptr.*, entry.value_ptr.*);
                        }
                    }
                }
            }

            if (!did_terminate and !frame.is_unconditional) {
                if (frame.continuation_types.count() > 0) {
                    var prev_frame = file.cfa_state.items[file.cfa_state.items.len - 1];
                    if (prev_frame.is_branch) {
                        try addBranch(file, &frame.continuation_types);
                    } else {
                        var iter = frame.continuation_types.iterator();
                        while (iter.next()) |entry| {
                            try prev_frame.types.put(entry.key_ptr.*, entry.value_ptr.*);
                        }
                    }
                }
            }

            if (frame.branch) |n| {
                var c2 = file.cfa_state.items[file.cfa_state.items.len - 1];
                if (c2.tmp_start == null) {
                    c2.tmp_start = c2.iter.ref;
                    c2.tmp_types = try c2.types.clone();
                }

                c2.is_unconditional = false;
                c2.is_branch = true;
                c2.iter.ref = n;
                c2.types = try frame.termination_types.clone();
            } else if (file.cfa_state.items.len > 0) {
                var c2 = file.cfa_state.items[file.cfa_state.items.len - 1];
                c2.is_branch = false;
            }
        }

        if (return_types.items.len == 0) {
            return @intFromEnum(Kind.void);
        }

        if (return_types.items.len == 1) {
            if (has_empty_return) {
                return this.toUnion(&.{ return_types.items[0], @intFromEnum(Kind.undefined) });
            }
            return return_types.items[0];
        }

        if (has_empty_return) {
            try return_types.append(@intFromEnum(Kind.undefined));
        }

        return this.toUnion(return_types.items);
    }

    // todo: simplify things more?
    fn simplifyIntersectionType(this: *@This(), types: *TypeList) !TypeRef {
        if (types.getCount() == 1) {
            defer types.deinit(this.allocator());
            return types.getSlice()[0];
        }

        return this.createIntersectionType2(types, 0);
    }

    fn maybeReduceIntersectionType(this: *@This(), lhs: TypeRef, rhs: TypeRef) !?TypeRef {
        if (lhs == rhs) return lhs;

        if (lhs >= @intFromEnum(Kind.false) and rhs >= @intFromEnum(Kind.false)) {
            if (try this.isAssignableTo(lhs, rhs)) {
                return lhs;
            } else if (try this.isAssignableTo(rhs, lhs)) {
                return rhs;
            }
            return @intFromEnum(Kind.never);
        }

        return null;
    }

    fn intersectType(this: *@This(), lhs: TypeRef, rhs: TypeRef) anyerror!u32 {
        if (this.isParameterizedRef(lhs) or this.isParameterizedRef(rhs)) {
            if (lhs == rhs) return lhs;

            // TODO: don't alloc
            var elements = try this.allocator().alloc(TypeRef, 2);
            elements[0] = lhs;
            elements[1] = rhs;

            return this.createIntersectionType(elements, @intFromEnum(Flags.parameterized));
        }

        // TODO: skip these checks if both are unions
        if (try this.isAssignableTo(lhs, rhs)) {
            return lhs;
        } else if (try this.isAssignableTo(rhs, lhs)) {
            return rhs;
        }

        if (lhs >= @intFromEnum(Kind.false)) {
            return lhs; // TODO
        }

        const lhs_type = this.types.at(lhs);
        if (lhs_type.getKind() != .@"union") {
            if (lhs_type.getKind() == .object_literal and rhs < @intFromEnum(Kind.false)) {
                const rhs_type = this.types.at(rhs);
                if (rhs_type.getKind() != .object_literal) {
                    if (rhs_type.getKind() == .alias) {
                        const followed = try this.maybeResolveAlias(rhs);
                        if (followed != rhs) {
                            return try this.intersectType(lhs, followed);
                        }
                    }
                    this.printTypeInfo(rhs);
                    return error.TODO;
                }
                // TODO: don't alloc
                var elements = try this.allocator().alloc(TypeRef, 2);
                elements[0] = lhs;
                elements[1] = rhs;
                return this.createIntersectionType(elements, 0);
            }
            if (lhs_type.getKind() == .alias) {
                const followed = try this.maybeResolveAlias(lhs);
                if (followed != lhs) {
                    return try this.intersectType(followed, rhs);
                }
            }

            std.debug.print("TODO (intersectType): ", .{});
            this.printTypeInfo(rhs);
            return lhs; // TODO
        }

        const elements = getSlice2(lhs_type, TypeRef);
        var arr = try std.ArrayList(TypeRef).initCapacity(this.allocator(), elements.len);
        errdefer arr.deinit();

        for (elements) |el| {
            if (try this.isAssignableTo(el, rhs)) {
                try arr.append(el);
            }
        }

        if (arr.items.len == 0) {
            return @intFromEnum(Kind.never);
        } else if (arr.items.len == 1) {
            defer arr.deinit();
            return arr.items[0];
        }

        const x: u64 = @intFromPtr(arr.items.ptr);
        return this.types.push(.{
            .kind = @intFromEnum(Kind.@"union"),
            // was this supposed to be intersection ??
            // .kind = @intFromEnum(Kind.intersection),
            .slot0 = @truncate(x),
            .slot1 = @intCast(x >> 32),
            .slot2 = @intCast(arr.items.len),
        });
    }

    fn nonNullable(this: *@This(), ref: TypeRef) !TypeRef {
        if (isNullLike(ref)) return @intFromEnum(Kind.never);
        if (isTypeParamRef(ref)) {
            const arr = try this.allocator().alloc(TypeRef, 2);
            arr[0] = ref;
            arr[1] = @intFromEnum(Kind.empty_object);

            return this.createIntersectionType(arr, @intFromEnum(Flags.parameterized));
        }

        if (ref >= @intFromEnum(Kind.false)) return ref;

        const t = this.types.at(ref);
        if (t.getKind() != .@"union") return ref;

        var count: u32 = 0;
        var first: TypeRef = 0;
        const slice = getSlice2(t, TypeRef);
        for (slice) |el| {
            if (!isNullLike(el)) {
                if (count == 0) first = el;
                count += 1;
            }
        }

        if (count == 0) {
            return @intFromEnum(Kind.never);
        } else if (count == 1) {
            return first;
        }

        var tmp = try TempUnion.initCapacity(this.allocator(), count);
        errdefer tmp.deinit();
        tmp.flags = t.flags;
        tmp.removeFlag(.has_undefined);

        for (slice) |el| {
            if (!isNullLike(el)) try tmp.appendChecked(el);
        }

        return tmp.complete(this);
    }

    pub fn falsyType(this: *@This(), from: TypeRef) anyerror!u32 {
        if (from >= @intFromEnum(Kind.false)) {
            if (from == @intFromEnum(Kind.zero) or from == @intFromEnum(Kind.false) or from == @intFromEnum(Kind.null) or from == @intFromEnum(Kind.undefined) or from == @intFromEnum(Kind.empty_string)) {
                return from;
            }
            if (from == @intFromEnum(Kind.number)) {
                return @intFromEnum(Kind.zero);
            }
            if (from == @intFromEnum(Kind.boolean)) {
                return @intFromEnum(Kind.false);
            }
            return this.excludeType(from, try this.truthyType(from));
        }

        const from_type = this.types.at(from);
        switch (from_type.getKind()) {
            .@"union" => {
                var tmp = try TempUnion.initCapacity(this.allocator(), from_type.slot2);
                errdefer tmp.deinit();

                for (getSlice2(from_type, TypeRef)) |el| {
                    if (try this.addToUnion(&tmp, try this.falsyType(el))) |x| {
                        return x;
                    }
                }

                return tmp.complete(this);
            },
            else => {},
        }

        return this.excludeType(from, try this.truthyType(from));
    }

    // This should be equivalent to `type Exclude<T, U> = T extends U ? never : T
    pub fn excludeType(this: *@This(), from: TypeRef, excluded: TypeRef) anyerror!u32 {
        if (from == excluded) {
            return @intFromEnum(Kind.never);
        }

        if (from >= @intFromEnum(Kind.false)) {
            if (from == @intFromEnum(Kind.boolean)) {
                if (excluded == @intFromEnum(Kind.true)) {
                    return @intFromEnum(Kind.false);
                } else if (excluded == @intFromEnum(Kind.false)) {
                    return @intFromEnum(Kind.true);
                }
            }
            if (from == @intFromEnum(Kind.any)) {
                return from;
            }
            if (try this.isAssignableTo(from, excluded)) {
                return @intFromEnum(Kind.never);
            }
            return from;
        }

        const from_type = this.types.at(from);
        switch (from_type.getKind()) {
            .@"union" => {},
            .alias, .query, .keyof => {
                const evaluated = try this.evaluateType(from, @intFromEnum(EvaluationFlags.all));
                if (evaluated == from) return error.TODO;

                const result = try this.excludeType(evaluated, excluded);

                return if (result != evaluated) result else from;
            },
            else => return from, // TODO
        }

        var tmp = try TempUnion.initCapacity(this.allocator(), from_type.slot2 - 1);
        errdefer tmp.deinit();

        for (getSlice2(from_type, TypeRef)) |el| {
            if (try this.isAssignableTo(el, excluded)) continue;

            if (tmp.elements.items.len == from_type.slot2 - 1) {
                tmp.deinit();
                return from;
            }

            if (el >= @intFromEnum(Kind.false)) {
                if (el == @intFromEnum(Kind.boolean)) {
                    tmp.addFlag(.has_boolean);
                } else if (el == @intFromEnum(Kind.undefined)) {
                    tmp.addFlag(.has_undefined);
                } else if (el == @intFromEnum(Kind.number)) {
                    tmp.addFlag(.has_number);
                } else if (el == @intFromEnum(Kind.string)) {
                    tmp.addFlag(.has_string);
                } else if (el == @intFromEnum(Kind.symbol)) {
                    tmp.addFlag(.has_symbol);
                } else if (el == @intFromEnum(Kind.empty_string)) {
                    tmp.addFlag(.has_string_literal);
                } else if (el >= @intFromEnum(Kind.zero)) {
                    tmp.addFlag(.has_number_literal);
                }
            }
            // FIXME: need to handle object/number/string literals for flags too

            try tmp.appendChecked(el);
        }

        return tmp.complete(this);
    }

    fn isAssignableTo(this: *@This(), src: TypeRef, dst: TypeRef) anyerror!bool {
        if (src == dst) return true;

        if (src < @intFromEnum(Kind.false)) {
            const n = this.types.at(src);
            if (dst >= @intFromEnum(Kind.false)) {
                if (dst == @intFromEnum(Kind.any)) return true;
                if (dst == @intFromEnum(Kind.unknown)) return true;
                if (dst == @intFromEnum(Kind.never)) return false;

                if (n.getKind() == .string_literal and dst == @intFromEnum(Kind.string)) {
                    return true;
                }
                if (n.getKind() == .number_literal and dst == @intFromEnum(Kind.number)) {
                    return true;
                }
                if (n.getKind() == .function_literal and dst == @intFromEnum(Kind.function)) {
                    return true;
                }
                if (n.getKind() == .symbol_literal and dst == @intFromEnum(Kind.symbol)) {
                    return true;
                }
                if ((n.getKind() == .object_literal or n.getKind() == .function_literal) and dst == @intFromEnum(Kind.object)) {
                    return true;
                }
                if (n.getKind() == .tuple and dst == @intFromEnum(Kind.empty_tuple)) {
                    return true;
                }

                // tuples are assignable to arrays are assignable Object

                // TODO: objects w/ call signatures are assignable to function
            }

            if (n.getKind() == .object_literal and n.slot3 != 0) {
                if (try this.isAssignableTo(n.slot3, dst)) {
                    return true;
                }
            }

            if (n.getKind() == .named_tuple_element) {
                return this.isAssignableTo(n.slot1, dst);
            }

            // Potential fast path for aliases
            if (n.getKind() == .alias) {
                if (dst < @intFromEnum(Kind.false) and this.types.at(dst).getKind() == .alias) {
                    const dst_type = this.types.at(dst);
                    if (dst_type.slot3 == n.slot3 and dst_type.slot4 == n.slot4) {
                        const src_args = getSlice2(n, TypeRef);
                        const dst_args = getSlice2(dst_type, TypeRef);
                        if (src_args.len == 0 and dst_args.len == 0) return true;
                        // TODO: compute variance of type params during construction of paramterized type
                    }
                }

                const followed = try this.maybeResolveAlias(src);
                if (followed != src) {
                    return try this.isAssignableTo(followed, dst);
                }
            }

            if (dst >= @intFromEnum(Kind.false)) return false;
        }

        if (dst < @intFromEnum(Kind.false)) {
            if (src == @intFromEnum(Kind.any)) return true;
            
            const dst_type = this.types.at(dst);
            switch (dst_type.getKind()) {
                .named_tuple_element => return this.isAssignableTo(src, dst_type.slot1),
                .tuple => {

                },
                .array => {
                    if (dst_type.slot0 == @intFromEnum(Kind.any)) {
                        if (src == @intFromEnum(Kind.empty_tuple)) return true;
                        if (src < @intFromEnum(Kind.false)) {
                            switch (this.getKindOfRef(src)) {
                                .array, .tuple => return true,
                                else => {},
                            }
                        }
                    }
                },
                .object_literal => {
                    const src_kind = this.getKindOfRef(src);
                    if (src_kind != .object_literal and src_kind != .empty_object) return false;

                    for (getSlice2(dst_type, ObjectLiteralMember)) |*m| {
                        if (m.kind != .index) continue;

                        if (!m.isLazy() and m.type == @intFromEnum(Kind.any)) {
                            return true;
                        }
                    }
                },
                .function_literal => {},
                .@"union" => {
                    // TODO: check if src is a union
                    for (getSlice2(dst_type, u32)) |el| {
                        if (el == src) return true;
                    }

                    return false;
                },
                .intersection => {},
                .empty_object => {
                    const n = this.maybeGetTypeFromRef(src) orelse return !isNullLike(src);
                    if (n.getKind() != .@"union") return true;

                    for (getSlice2(n, u32)) |el| {
                        if (isNullLike(el)) return false;
                    }
                    return true;
                },
                .alias => {
                    const followed = try this.maybeResolveAlias(dst);
                    if (followed != dst) {
                        return try this.isAssignableTo(src, followed);
                    }
                },
                else => {},
            }
            return false;
        }

        if (dst == @intFromEnum(Kind.any)) return true;
        if (dst == @intFromEnum(Kind.unknown)) return true;
        if (dst == @intFromEnum(Kind.never)) return false;
        if (src == @intFromEnum(Kind.any)) return true;
        // this should be added?
        // if (src == @intFromEnum(Kind.never)) return true;

        if (src >= @intFromEnum(Kind.zero) and dst == @intFromEnum(Kind.number)) return true;
        if (src == @intFromEnum(Kind.empty_string) and dst == @intFromEnum(Kind.string)) return true;
        if (src == @intFromEnum(Kind.empty_object) and dst == @intFromEnum(Kind.object)) return true;
        if (src == @intFromEnum(Kind.function) and (dst == @intFromEnum(Kind.object) or dst == @intFromEnum(Kind.empty_object))) return true;

        if ((src == @intFromEnum(Kind.false) or src == @intFromEnum(Kind.true)) and dst == @intFromEnum(Kind.boolean)) return true;
        if (src == @intFromEnum(Kind.undefined) and dst == @intFromEnum(Kind.void)) return true;

        return false; // TODO: objects, functions (incl. variance)
    }

    fn isNullLike(t: TypeRef) bool {
        return t == @intFromEnum(Kind.undefined) or t == @intFromEnum(Kind.null) or t == @intFromEnum(Kind.void);
    }

    // fast path for inline string literals
    fn maybeGetTypeFromTypeOfNode(this: *@This(), file: *ParsedFileData, ref: NodeRef) !?TypeRef {
        _ = this;

        const n = file.ast.nodes.at(ref);
        if (n.kind != .string_literal) return null;

        const Types = ComptimeStringMap(Kind, .{
            .{ "symbol", .symbol },
            .{ "string", .string },
            .{ "number", .number },
            .{ "object", .object },
            .{ "boolean", .boolean },
            .{ "function", .function },
            .{ "undefined", .undefined },
        });

        if (Types.get(getSlice(n, u8))) |x| {
            return @intFromEnum(x);
        }

        return null;
    }

    fn getTypeFromTypeOf(this: *@This(), ref: TypeRef) anyerror!?u32 {
        if (ref >= @intFromEnum(Kind.false)) {
            return null;
        }

        // TODO: maybe type params if feeling fancy
        const t = this.types.at(ref);
        switch (t.getKind()) {
            .string_literal => {
                const text = this.getSliceFromLiteral(ref);

                if (strings.eqlComptime(text, "number")) {
                    return @intFromEnum(Kind.number);
                }
                if (strings.eqlComptime(text, "string")) {
                    return @intFromEnum(Kind.string);
                }
                if (strings.eqlComptime(text, "boolean")) {
                    return @intFromEnum(Kind.boolean);
                }
                if (strings.eqlComptime(text, "undefined")) {
                    return @intFromEnum(Kind.undefined);
                }
                if (strings.eqlComptime(text, "symbol")) {
                    return @intFromEnum(Kind.symbol);
                }
                if (strings.eqlComptime(text, "function")) {
                    return @intFromEnum(Kind.function);
                }
                if (strings.eqlComptime(text, "object")) {
                    return @intFromEnum(Kind.object); // nullish object
                }
            },
            .@"union" => {
                var tmp = TempUnion.init(this.allocator());
                var should_deinit = true;
                defer if (should_deinit) tmp.deinit();

                for (getSlice2(t, TypeRef)) |r| {
                    const el_type = try this.getTypeFromTypeOf(r) orelse continue;
                    if (try this.addToUnion(&tmp, el_type)) |_| return null;
                }

                if (tmp.elements.items.len == 0) return null;
                should_deinit = false;

                return try tmp.complete(this);
            },
            else => {},
        }

        return null;
    }

    // Hoisting means CFA should be done on function/class declarations prior to the statements
    // Variable decl initializers should be treated as assignments for CFA

    pub inline fn isParameterizedRef(this: *const @This(), ref: TypeRef) bool {
        if (ref == 0) return false;
        if (ref >= @intFromEnum(Kind.false)) return isTypeParamRef(ref);
        return hasTypeFlag(this.types.at(ref), .parameterized);
    }

    pub inline fn hasThisType(this: *const @This(), ref: TypeRef) bool {
        if (ref == 0) return false;
        if (ref >= @intFromEnum(Kind.false)) return ref == @intFromEnum(Kind.this);
        return hasTypeFlag(this.types.at(ref), .has_this_type);
    }

    inline fn maybeGetUnion(this: *const @This(), ref: TypeRef) ?[]const TypeRef {
        if (ref >= @intFromEnum(Kind.false)) return null;

        const n = this.types.at(ref);
        if (n.getKind() != .@"union") return null;

        return getSlice2(n, TypeRef);
    }

    inline fn isTuple(this: *const @This(), ref: TypeRef) bool {
        return ref < @intFromEnum(Kind.false) and this.types.at(ref).getKind() == .tuple;
    }

    fn reduceTupleElement(this: *@This(), t: *const Type, arr: *std.ArrayList(TypeRef)) anyerror!void {
        for (getSlice2(t, TypeRef)) |el| {
            if (el < @intFromEnum(Kind.false) and this.types.at(el).getKind() == .named_tuple_element) {
                const u = this.types.at(el);
                if (hasTypeFlag(u, .spread)) {
                    if (u.slot1 == @intFromEnum(Kind.empty_tuple)) continue;

                    if (this.isTuple(u.slot1)) {
                        try this.reduceTupleElement(this.types.at(u.slot1), arr);
                        continue;
                    }
                }
                try arr.append(el);
                continue;
            }

            try arr.append(try this.types.push(.{
                .kind = @intFromEnum(Kind.named_tuple_element),
                .slot0 = 0,
                .slot1 = el,
                .flags = if (this.isParameterizedRef(el)) @intFromEnum(Flags.parameterized) else 0,
            }));
        }
    }

    // Pefer widening implicit `const` types here
    // e.g. `const x = 1; function foo(y = x); // (y: number) => any`
    //
    // We do this by always widening unless we see an explicit `as` expression
    //
    // This is not 1:1 w/ `tsc`. We diverge with `const x = 1 as const`.
    // `tsc` shows `1` for `y`, we show `number`
    fn getInferredParamType(this: *@This(), file: *ParsedFileData, initializer: NodeRef) !TypeRef {
        const n = file.ast.nodes.at(initializer);
        const type_ref = try this.getType(file, initializer);

        // TODO: unwrap intermediate expressions
        if (n.kind == .as_expression) {
            return type_ref;
        }

        if (type_ref >= @intFromEnum(Kind.zero)) {
            return @intFromEnum(Kind.number);
        }

        return switch (this.getKindOfRef(type_ref)) {
            .true, .false => @intFromEnum(Kind.boolean),
            .number_literal => @intFromEnum(Kind.number),
            .string_literal, .template_literal => @intFromEnum(Kind.string),
            else => type_ref,
        };
    }

    const Params = struct {
        flags: u24 = 0,
        this_type: TypeRef,
        params: []const TypeRef,
    };

    fn getParamsWithThisType(this: *@This(), file: *ParsedFileData, start: NodeRef) !Params {
        if (start == 0) {
            return .{ .this_type = 0, .params = &.{} };
        }

        const old_variance = this.variance;
        defer this.variance = old_variance;
        this.variance = this.variance.invert();

        var total_flags: u24 = 0;
        var is_first_param = true;
        var this_type: NodeRef = 0;
        var params_iter = NodeIterator.init(&file.ast.nodes, start);

        var params = std.ArrayList(TypeRef).init(this.allocator());

        while (params_iter.next()) |p| {
            var flags: u24 = 0;
            if (p.hasFlag(.optional)) flags |= @intFromEnum(Flags.optional);
            if (isParameterDecl(p)) flags |= @intFromEnum(Flags.parameter_decl); // TODO: this only matters for constructors...

            const d = getPackedData(p);
            if (p.len != 0) {
                const z = try this.getType(file, p.len);
                const is_rest_arg = p.hasFlag(.generator); // TODO: this is only valid for the last param
                if (is_rest_arg) {
                    // Tuple types are reduced into parameters
                    if (z == @intFromEnum(Kind.empty_tuple)) continue;

                    if (this.isTuple(z)) {
                        try this.reduceTupleElement(this.types.at(z), &params);
                        continue;
                    }

                    flags |= @intFromEnum(Flags.spread);
                } else {
                    if (d.right != 0) flags |= @intFromEnum(Flags.optional);
                }

                if (this.isParameterizedRef(z)) {
                    flags |= @intFromEnum(Flags.parameterized);
                    total_flags |= @intFromEnum(Flags.parameterized);
                }

                if (is_first_param) {
                    is_first_param = false;
                    if (file.ast.nodes.at(d.left).kind == .this_keyword) {
                        this_type = z;
                        try file.cached_symbol_types.put(this.allocator(), file.ast.nodes.at(d.left).extra_data, z);
                        continue;
                    }
                }

                try params.append(try this.createNamedTupleTypeFromIdent(
                    @intCast(file.id),
                    d.left,
                    z,
                    flags
                ));
            } else {
                const t = blk: {
                    if (d.right != 0) {
                        break :blk try this.getInferredParamType(file, d.right);
                    }

                    if (this.maybeGetParamFromInferrenceCtx(@intCast(params.items.len))) |inferred| {
                        // we have to put inferred types here so they propagate
                        try file.cached_symbol_types.put(this.allocator(), file.ast.nodes.at(d.left).extra_data, inferred);
                        break :blk inferred;
                    }

                    break :blk @intFromEnum(Kind.any);
                };

                if (d.right != 0) flags |= @intFromEnum(Flags.optional);
                if (this.isParameterizedRef(t)) {
                    flags |= @intFromEnum(Flags.parameterized);
                    total_flags |= @intFromEnum(Flags.parameterized);
                }
                try params.append(try this.createNamedTupleTypeFromIdent(
                    @intCast(file.id),
                    d.left,
                    t,
                    flags,
                ));
            }
        }

        // CALLER MUST FREE .PARAMS

        return .{
            .flags = total_flags,
            .this_type = this_type,
            .params = try params.toOwnedSlice(),
        };
    }

    fn getTypeParams(this: *@This(), file: *ParsedFileData, start: NodeRef) ![]const TypeRef {
        var type_params = std.ArrayList(TypeRef).init(this.type_args.allocator);
        var type_iter = NodeIterator.init(&file.ast.nodes, start);
        while (type_iter.nextPair()) |p| {
            try type_params.append(try this.getType(file, p[1]));
        }

        return type_params.items; // LEAKS
    }

    fn maybeGetParamFromInferrenceCtx(this: *@This(), pos: u32) ?TypeRef {
        const pt = this.inferrence_ctx orelse return null;
        if (this.getKindOfRef(pt) != .function_literal) return null;

        const t = this.types.at(pt);
        const params = getSlice2(t, TypeRef);
        if (pos >= params.len) return null;

        const param = params[pos];
        if (this.getKindOfRef(param) != .named_tuple_element) return null; // TODO?

        return this.types.at(param).slot1;
    }

    fn getSignature(this: *@This(), file: *ParsedFileData, ref: NodeRef) !u32 {
        var flags: u24 = 0;
        const n = file.ast.nodes.at(ref);
        switch (n.kind) {
            .arrow_function => {
                const d = getPackedData(n);

                const params_with_this = try this.getParamsWithThisType(file, d.left);
                var return_type: u32 = 0;
                if (n.len != 0) {
                    return_type = try this.getType(file, n.len);
                } else {
                    const block_or_exp = file.ast.nodes.at(d.right);
                    if (block_or_exp.kind == .block) {
                        return_type = try this.analyzeBody(file, maybeUnwrapRef(block_or_exp) orelse 0);
                        if (n.hasFlag(.@"async")) {
                            return_type = try this.normalizeAsyncReturnType(return_type);
                        }
                        // TODO: generator, async generator -> Iterable, AsyncIterable
                        if (n.hasFlag(.generator)) {
                            return error.TODO_generator;
                        }
                    } else {
                        return_type = try this.getType(file, d.right);
                    }
                }

                flags |= params_with_this.flags;
                if (this.isParameterizedRef(return_type)) flags |= @intFromEnum(Flags.parameterized);

                const inner = try this.createFunctionLiteral(params_with_this.params, return_type, flags);

                if (n.extra_data == 0) {
                    return inner;
                }

                const type_params = try this.getTypeParams(file, n.extra_data);
                return this.createParameterizedType(type_params, inner);
            },
            .function_expression, .function_declaration, .method_declaration => {
                const d = getPackedData(n);

                const params_with_this = try this.getParamsWithThisType(file, d.right);
                var return_type: u32 = 0;
                if (n.extra_data2 != 0) {
                    return_type = try this.getType(file, n.extra_data2);
                } else if (n.len != 0) {
                    // TODO: assert block
                    const block = file.ast.nodes.at(n.len);
                    return_type = try this.analyzeBody(file, maybeUnwrapRef(block) orelse 0);
                    if (n.hasFlag(.@"async")) {
                        return_type = try this.normalizeAsyncReturnType(return_type);
                    }
                } else {
                    return_type = @intFromEnum(Kind.any);
                }

                flags |= params_with_this.flags;
                if (this.isParameterizedRef(return_type)) flags |= @intFromEnum(Flags.parameterized);

                const inner = try this.createCanonicalFunctionLiteral(params_with_this, return_type, flags);
                if (n.extra_data == 0) {
                    return inner;
                }

                const type_params = try this.getTypeParams(file, n.extra_data);
                return this.createParameterizedType(type_params, inner);
            },
            else => {
                std.debug.print("{any}\n", .{n.kind});
                return error.NotFunctionLike;
            },
        }
    }

    inline fn getPromiseType(this: *@This(), inner: TypeRef) !TypeRef {
        const args = try TypeList.fromSlice(this, &.{inner});
        return try this.getGlobalTypeSymbolAlias("Promise", args) orelse error.MissingPromise;
    }

    fn maybeUnwrapPromise(this: *@This(), ref: TypeRef) anyerror!?TypeRef {
        if (ref >= @intFromEnum(Kind.false)) return null;
        if (this.getKindOfRef(ref) != .alias) return null;

        const t = this.types.at(ref);
        const p = this.program.ambient.getGlobalTypeSymbol("Promise") orelse return error.MissingPromise;
        if (p.symbols.items.len > 1) {
            if (!t.hasFlag(.global)) {
                return null;
            }
            const p2 = this.program.ambient.getGlobalTypeSymbolRef("Promise") orelse unreachable;
            if (t.slot4 == p2) {
                const s = getSlice2(t, TypeRef);
                if (s.len == 1) {
                    return try this.maybeUnwrapPromise(s[0]) orelse s[0];
                }
            }
            return null; // TODO?
        }

        if (p.symbols.items.len == 0) return null;

        const sym = p.symbols.items[0];
        if (t.slot3 == sym.file_id and t.slot4 == sym.ref) {
            const s = getSlice2(t, TypeRef);
            if (s.len == 1) {
                return try this.maybeUnwrapPromise(s[0]) orelse s[0];
            }
        }

        return null; // TODO
    }

    fn normalizeAsyncReturnType(this: *@This(), base: TypeRef) !TypeRef {
        // async functions effectively have this applied over their return type:
        // `T extends Promise<infer U> ? Promise<U> : Promise<T>`
        //
        // Where `U` has already been unwrapped.
        //
        // async/await is common enough that it makes sense to have a dedicated impl.

        if (base >= @intFromEnum(Kind.false)) {
            return this.getPromiseType(base);
        }

        return switch (this.getKindOfRef(base)) {
            .alias => {
                const inner = try this.maybeUnwrapPromise(base) orelse return this.getPromiseType(base);

                return this.getPromiseType(inner);
            },
            .@"union" => {
                const t = this.types.at(base);
                var did_change = false;
                var tmp = TempUnion.init(this.allocator());
                defer if (!did_change) tmp.deinit();
                for (getSlice2(t, TypeRef)) |el| {
                    const v = try this.maybeUnwrapPromise(el) orelse el;
                    if (v != el) did_change = true;
                    if (try this.addToUnion(&tmp, v)) |x| return x;
                }

                if (!did_change) return this.getPromiseType(base);

                return this.getPromiseType(try tmp.complete(this));
            },
            //.intersection => {
            // If Promise is apart of the intersection then we want to use that component
            // Multiple promises intersected will have their inner parts intersected
            //},
            else => this.getPromiseType(base),
        };
    }

    const TypeSet = std.AutoArrayHashMap(TypeRef, bool);

    // Used to construct a union
    const TempUnion = struct {
        has_unknown: bool = false,

        flags: u24 = 0,
        elements: std.ArrayList(u32),
        xor_hash: u64 = 0,

        set: ?*TypeSet = null,

        pub fn init(alloc: std.mem.Allocator) @This() {
            return .{
                .elements = std.ArrayList(TypeRef).init(alloc),
            };
        }

        pub fn initCapacity(alloc: std.mem.Allocator, count: u32) !@This() {
            var this = @This(){
                .elements = try std.ArrayList(TypeRef).initCapacity(alloc, count),
            };

            if (count >= allocated_set_threshold) {
                this.set = try this.allocateSet(count);
            }

            return this;
        }

        inline fn hasFlag(this: *const @This(), comptime flag: UnionFlags) bool {
            return (this.flags & @intFromEnum(flag)) == @intFromEnum(flag);
        }

        inline fn addFlag(this: *@This(), comptime flag: UnionFlags) void {
            this.flags |= @intFromEnum(flag);
        }

        inline fn removeFlag(this: *@This(), comptime flag: UnionFlags) void {
            this.flags &= @intFromEnum(flag);
        }

        pub fn fromUnion(analyzer: *Analyzer, t: *const Type) !@This() {
            const prev = getSlice2(t, TypeRef);
            const slice = try analyzer.allocator().alloc(TypeRef, prev.len);
            @memcpy(slice, prev);

            var this = @This(){
                .elements = std.ArrayList(TypeRef).fromOwnedSlice(analyzer.allocator(), slice),
                .xor_hash = (@as(u64, t.slot3) << 32) | t.slot4,
                .flags = t.flags,
            };

            if (maybeGetUnionSet(t)) |set| {
                _ = try this.allocateSet(0);
                this.set.?.* = try set.clone();
            }

            return this;
        }

        pub fn deinit(this: *@This()) void {
            this.elements.deinit();
            if (this.set) |s| s.deinit();
        }

        // do NOT remove any of the flag-based types
        inline fn removeAt(this: *@This(), index: usize) void {
            const el = this.elements.swapRemove(index);
            this.xor_hash ^= std.hash.Wyhash.hash(0, &@as([4]u8, @bitCast(el)));

            if (this.set) |set| {
                _ = set.swapRemove(el);
            }
        }

        // In-place mutation
        pub fn filter(this: *@This(), analyzer: *Analyzer, comptime kind: Kind) !void {
            var j: usize = this.elements.items.len - 1;
            while (true) {
                const el = this.elements.items[j];
                if (el < @intFromEnum(Kind.false)) {
                    const n = analyzer.types.at(el);
                    if (n.getKind() == kind) {
                        this.removeAt(j);
                    }
                    if (j == 0) break;
                    j -= 1;
                    continue;
                }

                if (comptime kind == .string_literal) {
                    // TODO: use bitset for all common non-allocated types
                    if (el == @intFromEnum(Kind.empty_string)) {
                        this.removeAt(j);
                    }
                } else if (comptime kind == .number_literal) {
                    if (el >= @intFromEnum(Kind.zero)) {
                        this.removeAt(j);
                    }
                } else {
                    if (el == @intFromEnum(kind)) this.removeAt(j);
                }

                if (j == 0) break;
                j -= 1;
            }
        }

        pub inline fn contains(this: *const @This(), t: TypeRef) bool {
            if (this.set) |s| {
                return s.contains(t);
            }

            for (this.elements.items) |el| {
                if (el == t) return true;
            }

            return false;
        }

        fn allocateSet(this: *@This(), capacity: usize) !*TypeSet {
            std.debug.assert(this.set == null);

            const s = try this.elements.allocator.create(TypeSet);
            s.* = TypeSet.init(this.elements.allocator);
            try s.ensureTotalCapacity(capacity);

            this.set = s;

            return s;
        }

        pub inline fn appendChecked(this: *@This(), t: TypeRef) !void {
            try this.elements.append(t);
            this.xor_hash ^= std.hash.Wyhash.hash(0, &@as([4]u8, @bitCast(t)));

            if (this.elements.items.len >= allocated_set_threshold and this.set == null) {
                const s = try this.allocateSet(this.elements.items.len * 2);

                for (this.elements.items) |el| s.putAssumeCapacity(el, true);
            } else if (this.set) |s| {
                try s.put(t, true);
            }
        }

        pub fn complete(this: *@This(), analyzer: *Analyzer) !TypeRef {
            if (this.elements.items.len == 0) {
                if (this.has_unknown) {
                    return @intFromEnum(Kind.unknown);
                }
                return @intFromEnum(Kind.never);
            }

            if (this.elements.items.len == 1) {
                defer this.elements.deinit();
                return this.elements.items[0];
            }

            const key = this.xor_hash;
            if (analyzer.canonical_types.get(key)) |t| {
                if (this.set) |s| s.deinit();
                defer this.elements.deinit();
                return t;
            }

            this.elements.shrinkAndFree(this.elements.items.len);

            const x: u64 = @intFromPtr(this.elements.items.ptr);
            const t = try analyzer.types.push(.{
                .kind = @intFromEnum(Kind.@"union"),
                .slot0 = @truncate(x),
                .slot1 = @intCast(x >> 32),
                .slot2 = @intCast(this.elements.items.len),
                .slot3 = @intCast(this.xor_hash >> 32),
                .slot4 = @truncate(this.xor_hash),
                .flags = this.flags,
            });

            if (this.set) |s| {
                s.shrinkAndFree(s.count());
                analyzer.types.at(t).slot5 = @truncate(@intFromPtr(s) >> 32);
                analyzer.types.at(t).slot6 = @truncate(@intFromPtr(s));
            }

            try analyzer.canonical_types.put(key, t);
            return t;
        }

        const allocated_set_threshold = 16;
    };

    inline fn maybeGetUnionSet(t: *const Type) ?*const TypeSet {
        std.debug.assert(t.getKind() == .@"union");

        if (t.slot5 == 0) {
            return null;
        }

        return @ptrFromInt((@as(u64, t.slot5) << 32) | t.slot6);
    }

    inline fn unionContains(t: *const Type, elements: []const TypeRef, needle: TypeRef) bool {
        if (maybeGetUnionSet(t)) |set| {
            return set.contains(needle);
        }

        for (elements) |el| {
            if (el == t) return true;
        }

        return false;
    }

    fn addToUnion(this: *@This(), tmp: *TempUnion, t: u32) anyerror!?u32 {
        if (t == 0) return null;

        if (t < @intFromEnum(Kind.false)) {
            const k = this.types.at(t);
            if (k.getKind() == .@"union") {
                if (tmp.elements.items.len == 0) {
                    tmp.deinit();
                    tmp.* = try TempUnion.fromUnion(this, k);
                    return null;
                }

                const from = getSlice2(k, TypeRef);

                // TODO: a potentially faster way is to check elements before adding to
                // avoid checking elements that we know cannot possibly overlap
                if (from.len >= (tmp.elements.items.len * 2)) {
                    var new_tmp = try TempUnion.fromUnion(this, k);
                    for (tmp.elements.items) |el| {
                        if (try this.addToUnion(&new_tmp, el)) |q| {
                            tmp.deinit();
                            return q;
                        }
                    }

                    tmp.deinit();
                    tmp.* = new_tmp;

                    return null;
                }

                for (from) |el| {
                    if (try this.addToUnion(tmp, el)) |q| return q;
                }
            } else {
                if (k.getKind() == .object_literal) {
                    if (!tmp.hasFlag(.parameterized) and hasTypeFlag(k, .parameterized)) {
                        tmp.addFlag(.parameterized);
                    }

                    if (!tmp.hasFlag(.has_object_literal)) {
                        tmp.addFlag(.has_object_literal);
                        try tmp.appendChecked(t);
                        return null;
                    }
                } else if (k.getKind() == .string_literal) {
                    if (tmp.hasFlag(.has_string)) return null;
                    if (!tmp.hasFlag(.has_string_literal)) {
                        tmp.addFlag(.has_string_literal);
                        try tmp.appendChecked(t);
                        return null;
                    }
                } else if (k.getKind() == .symbol_literal) {
                    if (tmp.hasFlag(.has_symbol)) return null;
                    if (!tmp.hasFlag(.has_symbol_literal)) {
                        tmp.addFlag(.has_symbol_literal);
                        try tmp.appendChecked(t);
                        return null;
                    }
                } else if (!tmp.hasFlag(.parameterized) and hasTypeFlag(k, .parameterized)) {
                    tmp.addFlag(.parameterized);
                }

                if (tmp.contains(t)) return null;
                try tmp.appendChecked(t);
            }
        } else if (t >= @intFromEnum(Kind.zero)) {
            if (tmp.hasFlag(.has_number)) return null; // Do not add literal numbers if we have `number`

            if (!tmp.hasFlag(.has_number_literal)) {
                tmp.addFlag(.has_number_literal);
                try tmp.appendChecked(t);

                return null;
            }

            if (tmp.contains(t)) return null;
            try tmp.appendChecked(t);
        } else {
            if (t == @intFromEnum(Kind.unknown)) {
                tmp.has_unknown = true;
            } else if (t == @intFromEnum(Kind.never)) {
                return null; // no-op
            } else if (t == @intFromEnum(Kind.any)) {
                tmp.deinit();
                return t; // replaces the union
            } else if (t == @intFromEnum(Kind.empty_string)) {
                if (tmp.hasFlag(.has_string)) return null;
                if (!tmp.hasFlag(.has_string_literal)) {
                    tmp.addFlag(.has_string_literal);
                    try tmp.appendChecked(t);
                    return null;
                }

                if (tmp.contains(t)) return null;
                try tmp.appendChecked(t);
            } else if (t == @intFromEnum(Kind.string)) {
                if (tmp.hasFlag(.has_string)) return null;
                tmp.addFlag(.has_string);

                if (!tmp.hasFlag(.has_string_literal)) {
                    try tmp.appendChecked(t);
                    return null;
                }

                try tmp.filter(this, .string_literal);
                tmp.removeFlag(.has_string_literal);

                try tmp.appendChecked(t);

                return null;
            } else if (t == @intFromEnum(Kind.number)) {
                if (tmp.hasFlag(.has_number)) return null;
                tmp.addFlag(.has_number);

                if (!tmp.hasFlag(.has_number_literal)) {
                    try tmp.appendChecked(t);
                    return null;
                }

                try tmp.filter(this, .number_literal);
                tmp.removeFlag(.has_number_literal);

                try tmp.appendChecked(t);

                return null;
            } else if (t == @intFromEnum(Kind.boolean)) {
                if (tmp.hasFlag(.has_boolean)) return null;
                tmp.addFlag(.has_boolean);

                if (tmp.elements.items.len > 0) {
                    var j: usize = tmp.elements.items.len - 1;
                    while (true) {
                        const el = tmp.elements.items[j];
                        if (el == @intFromEnum(Kind.false) or el == @intFromEnum(Kind.true)) {
                            tmp.removeAt(j);
                        }
                        if (j == 0) break;
                        j -= 1;
                    }
                }

                try tmp.appendChecked(t);

                return null;
            } else if (t == @intFromEnum(Kind.symbol)) {
                if (tmp.hasFlag(.has_symbol)) return null;
                tmp.addFlag(.has_symbol);

                if (!tmp.hasFlag(.has_symbol_literal)) {
                    try tmp.appendChecked(t);
                    return null;
                }

                try tmp.filter(this, .symbol_literal);
                tmp.removeFlag(.has_symbol_literal);

                try tmp.appendChecked(t);

                return null;
            } else if (t == @intFromEnum(Kind.true) or t == @intFromEnum(Kind.false)) {
                if (tmp.hasFlag(.has_boolean)) return null;
                if (tmp.contains(t)) return null;

                const opposite = if (t == @intFromEnum(Kind.true)) @intFromEnum(Kind.false) else @intFromEnum(Kind.true);

                if (tmp.contains(opposite)) {
                    var j: usize = tmp.elements.items.len - 1;
                    while (true) {
                        const el = tmp.elements.items[j];
                        if (el == opposite) {
                            tmp.removeAt(j);
                            break;
                        }
                        if (j == 0) break;
                        j -= 1;
                    }
                    tmp.addFlag(.has_boolean);
                    try tmp.appendChecked(@intFromEnum(Kind.boolean));
                } else {
                    try tmp.appendChecked(t);
                }            
            } else if (t == @intFromEnum(Kind.undefined)) {
                if (tmp.hasFlag(.has_undefined)) return null;
                tmp.addFlag(.has_undefined);
                try tmp.appendChecked(t);
            } else {
                if (tmp.contains(t)) return null;
                try tmp.appendChecked(t);

                if (!tmp.hasFlag(.parameterized) and isTypeParamRef(t)) {
                    tmp.addFlag(.parameterized);
                }
            }
        }

        return null;
    }

    // TODO: we should compute an ordered hash of the union after adding types
    // fn toCanonicalUnion(this: *@This(), types: []const TypeRef) !u32 {
    //     var tmp = TempUnion{ .elements = std.ArrayList(u32).init(this.allocator()) };

    //     for (types) |t| {
    //         if (try this.addToUnion(&tmp, t)) |x| return x;
    //     }

    //     return tmp.complete(this);
    // }

    fn toUnion(this: *@This(), types: []const TypeRef) !u32 {
        std.debug.assert(types.len > 1);

        if (types.len == 2) {
            if (types[0] == types[1]) {
                return types[0];
            }
            if (types[0] == @intFromEnum(Kind.boolean)) {
                if (types[1] == @intFromEnum(Kind.false)) {
                    return types[0];
                } else if (types[1] == @intFromEnum(Kind.true)) {
                    return types[0];
                }
            } else if (types[1] == @intFromEnum(Kind.boolean)) {
                if (types[0] == @intFromEnum(Kind.false)) {
                    return types[1];
                } else if (types[0] == @intFromEnum(Kind.true)) {
                    return types[1];
                }
            } else if (types[0] == @intFromEnum(Kind.false) and types[1] == @intFromEnum(Kind.true)) {
                return @intFromEnum(Kind.boolean);
            } else if (types[1] == @intFromEnum(Kind.false) and types[0] == @intFromEnum(Kind.true)) {
                return @intFromEnum(Kind.boolean);
            } else if (types[1] == @intFromEnum(Kind.never)) {
                return types[0];
            } else if (types[0] == @intFromEnum(Kind.never)) {
                return types[1];
            }
        }

        var tmp = try TempUnion.initCapacity(this.allocator(), @intCast(types.len));

        for (types) |t| {
            if (try this.addToUnion(&tmp, t)) |x| return x;
        }

        return tmp.complete(this);
    }

    fn maybeGetSimpleType(this: *@This(), f: *ParsedFileData, node_ref: NodeRef) anyerror!?TypeRef {
        if (node_ref == 0) {
            return @intFromEnum(Kind.any);
        }

        const n = f.ast.nodes.at(node_ref);
        return switch (n.kind) {
            .object_keyword => @intFromEnum(Kind.object),
            .boolean_keyword =>  @intFromEnum(Kind.boolean),
            .number_keyword => @intFromEnum(Kind.number),
            .string_keyword =>  @intFromEnum(Kind.string),
            .any_keyword => @intFromEnum(Kind.any),
            .null_keyword => @intFromEnum(Kind.null),
            .never_keyword => @intFromEnum(Kind.never),
            .unknown_keyword => @intFromEnum(Kind.unknown),
            .void_keyword => @intFromEnum(Kind.void),
            .undefined_keyword => @intFromEnum(Kind.undefined),
            .symbol_keyword => @intFromEnum(Kind.symbol),
            .type_literal => {
                if (n.data == null) return @intFromEnum(Kind.empty_object);
                return null;
            },
            .union_type => {
                const d = getPackedData(n);
                if (d.left == 0) return try this.maybeGetSimpleType(f, d.right);
                if (d.right == 0) return try this.maybeGetSimpleType(f, d.left);

                const l = try this.maybeGetSimpleType(f, d.left) orelse return null;
                const r = try this.maybeGetSimpleType(f, d.right) orelse return null;

                return try this.toUnion(&.{l, r});
            },
            else => null,
        };
    }

    // TODO: needs refinement
    pub const ObjectLiteralMember = packed struct {
        const Subkind = enum(u8) {
            property,
            method,
            getter,
            setter,
            index,
            call_signature,
        };

        kind: Subkind = .property,
        flags: u24 = 0, // Uses NodeFlags, upper 4 bits used for lazy eval
        name: TypeRef,
        type: TypeRef,
        slot0: u32 = undefined, // Scratch space

        pub const private_ident_flag = @as(u24, 1) << 22;
        pub const lazy_flag = @as(u24, 1) << 23;

        pub fn initLazy(kind: Subkind, name: TypeRef, file_id: u32, node_ref: NodeRef, flags: u24) @This() {
            return .{
                .kind = kind,
                .name = name,
                .type = node_ref,
                .flags = flags | lazy_flag,
                .slot0 = file_id,
            };
        }

        pub inline fn isPublic(this: *const @This()) bool {
            const protected_or_private: u24 = comptime @intFromEnum(NodeFlags.protected) | @intFromEnum(NodeFlags.private) | private_ident_flag;

            return this.flags & protected_or_private == 0;
        }

        pub inline fn hasFlag(this: *@This(), flag: NodeFlags) bool {
            return this.flags & @intFromEnum(flag) == @intFromEnum(flag);
        }

        pub inline fn isLazy(this: *const @This()) bool {
            return this.flags & lazy_flag == lazy_flag;
        }

        pub inline fn getType(this: *@This(), analyzer: *Analyzer) !TypeRef {
            if (!this.isLazy()) {
                return this.type;
            }

            const t = try analyzer.getMemberType(try analyzer.getAnalyzedFile(this.slot0), this.type);
            this.type = t;
            this.flags &= ~@as(u23, 0);

            return t;
        }
    };

    fn propertyNameToType(this: *@This(), file: *ParsedFileData, ref: NodeRef) !TypeRef {
        const n = file.ast.nodes.at(ref);
        switch (n.kind) {
            .identifier, .private_identifier => {
                const s = getSlice(n, u8);
                return this.createStringLiteral(file.id, ref, s, 0);
            },
            .string_literal => {
                const s = getSlice(n, u8);
                return this.createStringLiteral(file.id, ref, s, @intFromEnum(StringFlags.single_quote));
            },
            .computed_property_name => {
                return this.getType(file, unwrapRef(n));
            },
            .numeric_literal => {
                return this.getNumericLiteralType(n, false);
            },
            else => return notSupported(n.kind),
        }
    }

    fn resolveObjectType(this: *@This(), file: *ParsedFileData, first_member: NodeRef, flags_init: u24, proto: TypeRef) !u32 {
        if (first_member == 0) return @intFromEnum(Kind.empty_object);

        var flags = flags_init;
        var iter = NodeIterator.init(&file.ast.nodes, first_member);
        var members = std.ArrayList(ObjectLiteralMember).init(this.allocator());

        while (iter.nextPair()) |p| {
            switch (p[0].kind) {
                .call_signature, .construct_signature => {
                    const d2 = getPackedData(p[0]);
                    const type_params = try this.getTypeParams(file, p[0].len);
                    const params_with_this = try this.getParamsWithThisType(file, d2.left);
                    const return_type = if (d2.right == 0) @intFromEnum(Kind.any) else try this.getType(file, d2.right);
                    var fn_flags: u24 = params_with_this.flags;
                    fn_flags |= if (type_params.len > 0) @intFromEnum(Flags.parameterized) else 0;
                    fn_flags |= if (p[0].kind == .construct_signature) @intFromEnum(Flags.constructor) else 0;
                    if (this.isParameterizedRef(return_type)) fn_flags |= @intFromEnum(Flags.parameterized);

                    const inner = try this.createFunctionLiteralFull(params_with_this.params, return_type, params_with_this.this_type, fn_flags);

                    const t = if (type_params.len > 0) try this.createParameterizedType(type_params, inner) else inner;

                    const flags2: u20 = if (p[0].kind == .construct_signature) @intFromEnum(Flags.constructor) else 0;

                    if (this.isParameterizedRef(t)) flags |= @intFromEnum(Flags.parameterized);

                    try members.append(.{
                        .kind = .call_signature,
                        .flags = flags2,
                        .name = 0,
                        .type = t,
                    });
                },
                .index_signature => {
                    const d2 = getPackedData(p[0]);
                    const name = try this.getType(file, d2.right);
                    const ty = try this.getType(file, p[0].len);

                    if (this.isParameterizedRef(name) or this.isParameterizedRef(ty)) flags |= @intFromEnum(Flags.parameterized);

                    try members.append(.{
                        .kind = .index,
                        .flags = p[0].flags,
                        .name = name,
                        .type = ty,
                    });
                },
                .property_signature => {
                    const d2 = getPackedData(p[0]);
                    const name = try this.propertyNameToType(file, d2.left);
                    if (name == @intFromEnum(Kind.any)) {
                        // FIXME: print location
                        return error.InvalidMemberName_any;
                    }
                    if (name == @intFromEnum(Kind.undefined)) {
                        // FIXME: print location
                        return error.InvalidMemberName_undefined;
                    }

                    if (try this.maybeGetSimpleType(file, d2.right)) |t| {
                        try members.append(.{
                            .kind = .property,
                            .name = name,  
                            .type = t,
                            .flags = p[0].flags,
                        });
                        continue;
                    }

                    // if (file.ast.nodes.at(d2.right).kind == .identifier) {
                    //     const ident = file.ast.nodes.at(d2.right);
                    //     const sym = file.binder.symbols.at(ident.extra_data);
                    //     if (sym.hasFlag(.type_parameter)) {
                    //         flags |= @intFromEnum(Flags.parameterized);
                    //         const t: TypeRef = @intFromEnum(Kind.type_parameter_ref) | getOrdinal(sym);
                    //         try members.append(.{
                    //             .kind = .property,
                    //             .name = name,  
                    //             .type = t,
                    //         });
                    //         continue;
                    //     }
                    // }

                    if (try this.peekIsParameterized(file, p[1])) flags |= @intFromEnum(Flags.parameterized);
                    flags |= @intFromEnum(ObjectLiteralFlags.lazy);

                    try members.append(ObjectLiteralMember.initLazy(.property, name, file.id, p[1], p[0].flags));
                },
                .method_signature => {
                    const d2 = getPackedData(p[0]);
                    const name = try this.propertyNameToType(file, d2.left);

                    if (p[0].extra_data != 0) {
                        if (try this.peekIsParameterized(file, p[1])) flags |= @intFromEnum(Flags.parameterized);
                        flags |= @intFromEnum(ObjectLiteralFlags.lazy);

                        try members.append(ObjectLiteralMember.initLazy(.method, name, file.id, p[1], p[0].flags));

                        continue;
                    }

                    const ty = try this.getType(file, p[0].len);
                    const params_with_this = try this.getParamsWithThisType(file, d2.right);
                    if (this.isParameterizedRef(ty)) flags |= @intFromEnum(Flags.parameterized);

                    const inner = try this.createFunctionLiteralFull(params_with_this.params, ty, params_with_this.this_type, 0);

                    try members.append(.{
                        .kind = .method,
                        .flags = p[0].flags,
                        .name = name,
                        .type = inner,
                    });
                },
                else => {
                    if (comptime is_debug) {
                        std.debug.print("MISSING OBJ IMPL {any}\n", .{p[0].kind});
                    }
                },
            }
        }

        return this.createObjectLiteral(members.items, flags, proto);
    }

    // Union types can distribute over conditional types
    //
    // function f<T>() {
    //     type X<U> = U extends string ? T extends number ? T : never : T extends string ? 'hi' : never
    //     return {} as X<'1' | 2>
    // }
    //
    // type Z<T> = ReturnType<typeof f<T>>
    // type Check<T, U> = T extends U ? true : false
    //
    // type R1 = Check<Z<1>, 1>         // true
    // type R2 = Check<Z<'bye'>, 'hi'>  // true
    // type R3 = Check<Z<'bye'>, 1>     // false

    pub fn resolveWithTypeArgsSlice(this: *@This(), base: *const Type, args: []const TypeRef) !TypeRef {
        const registers = this.type_registers;
        defer @memcpy(&this.type_registers, &registers);

        const params = getSlice2(base, TypeRef);

        for (params, 0..) |p, i| {
            const val = if (i < args.len) args[i] else this.getTypeParamDefault(p) orelse @intFromEnum(Kind.unknown);
            this.type_registers[this.types.at(p).slot3] = val;
        }

        return try this.resolveParameterizedType(base.slot3) orelse return error.FailedToResolve;
    }

    fn resolveWithTypeArgsInferred(this: *@This(), base: *const Type, inferred: *const std.AutoArrayHashMap(TypeRef, TypeRef)) !TypeRef {
        var tmp_args = std.AutoArrayHashMap(TypeRef, TypeRef).init(this.type_args.allocator);
        const registers = this.type_registers;

        defer tmp_args.deinit();
        defer this.revertTypeArgs(tmp_args, registers);

        const params = getSlice2(base, TypeRef);

        // var iter = inferred.iterator();
        // while (iter.next()) |entry| {
        //     const prev = this.type_args.get(entry.key_ptr.*) orelse 0;
        //     const val = entry.value_ptr.*;

        //     try tmp_args.put(entry.key_ptr.*, prev);
        //     try this.type_args.put(entry.key_ptr.*, val);

        //     const reg = this.types.at(entry.key_ptr.*).slot3;
        //     this.type_registers[reg] = val;
        // }

        for (params) |p| {
            const prev = this.type_args.get(p) orelse 0;
            const val = inferred.get(p) orelse (this.getTypeParamDefault(p) orelse @intFromEnum(Kind.unknown));
            try tmp_args.put(p, prev);
            try this.type_args.put(p, val);

            const reg = this.types.at(p).slot3;
            this.type_registers[reg] = val;
        }

        return try this.resolveParameterizedType(base.slot3) orelse return error.FailedToResolve;
    }

    fn resolveWithTypeArgs(this: *@This(), file: *ParsedFileData, base: *const Type, start_node: NodeRef) !TypeRef {
        const params = getSlice2(base, TypeRef);
        const registers = this.type_registers;

        var iter = NodeIterator.init(&file.ast.nodes, start_node);
        var i: u32 = 0;

        var tmp_args = std.AutoArrayHashMap(TypeRef, TypeRef).init(this.type_args.allocator);
        defer tmp_args.deinit();
        defer this.revertTypeArgs(tmp_args, registers);

        while (iter.nextRef()) |r| {
            const prev = this.type_args.get(params[i]) orelse 0;
            const val = try this.getType(file, r);

            try tmp_args.put(params[i], prev);
            try this.type_args.put(params[i], val);

            const reg = this.types.at(params[i]).slot3;
            this.type_registers[reg] = val;

            i += 1;
        }

        return try this.resolveParameterizedType(base.slot3) orelse return error.FailedToResolve;
    }

    fn createParameterizedType(this: *@This(), params: []const TypeRef, inner: TypeRef) !TypeRef {
        const x: u64 = @intFromPtr(params.ptr);
        return this.types.push(.{
            .kind = @intFromEnum(Kind.parameterized),
            .slot0 = @truncate(x),
            .slot1 = @intCast(x >> 32),
            .slot2 = @intCast(params.len),
            .slot3 = inner,
            .flags = @intFromEnum(Flags.parameterized),
        });
    }

    // type X<T, U extends string> = { [P in keyof T as `${P & string}_${U}`]: string }
    // type F = <const T extends string>() => X<{ foo: number }, T>
    // declare const x: F
    // type Y = ReturnType<typeof x<'a' | 'b'>> // `{ foo_a: string; foo_b: string }`

    // Unresolved mapped types are always parameterized
    fn createMappedType(this: *@This(), param: TypeRef, exp: TypeRef, rename: TypeRef, node_flags: u32) !TypeRef {
        return this.types.push(.{
            .kind = @intFromEnum(Kind.mapped),
            .slot0 = param,
            .slot1 = exp,
            .slot2 = rename,
            .slot3 = node_flags,
            .flags = @intFromEnum(Flags.parameterized),
        });
    }

    fn createArrayType(this: *@This(), element: TypeRef) !TypeRef {
        const flags = if (this.isParameterizedRef(element)) @intFromEnum(Flags.parameterized) else 0;
       
        var h = std.hash.Wyhash.init(0);
        h.update(&.{@as(u8, @intFromEnum(Kind.array))});
        h.update(&@as([3]u8, @bitCast(flags)));
        h.update(&@as([4]u8, @bitCast(element)));

        const key = h.final();
        if (this.canonical_types.get(key)) |t| return t;
        
        const t = try this.types.push(.{
            .kind = @intFromEnum(Kind.array),
            .slot0 = element,
            .flags = flags,
        });

        try this.canonical_types.put(key, t);

        return t;
    }

    fn createTupleType(this: *@This(), elements: []const TypeRef, flags: u24) !TypeRef {
        if (elements.len == 0) return @intFromEnum(Kind.empty_tuple);

        const x: u64 = @intFromPtr(elements.ptr);
        return try this.types.push(.{
            .kind = @intFromEnum(Kind.tuple),
            .flags = flags,
            .slot0 = @truncate(x),
            .slot1 = @intCast(x >> 32),
            .slot2 = @intCast(elements.len),
        });
    }

    fn _createObjectLiteral(this: *@This(), elements: []const ObjectLiteralMember, flags: u24, proto: TypeRef) !TypeRef {
        const x: u64 = @intFromPtr(elements.ptr);
        return this.types.push(.{
            .kind = @intFromEnum(Kind.object_literal),
            .flags = flags,
            .slot0 = @truncate(x),
            .slot1 = @intCast(x >> 32),
            .slot2 = @intCast(elements.len),
            .slot3 = proto,
        });
    }

    fn createObjectLiteral(this: *@This(), elements: []ObjectLiteralMember, flags: u24, proto: TypeRef) !TypeRef {
        if (elements.len == 0 and proto == 0) return @intFromEnum(Kind.empty_object);
        if ((flags & @intFromEnum(ObjectLiteralFlags.lazy)) == @intFromEnum(ObjectLiteralFlags.lazy)) {
            return try this._createObjectLiteral(elements, flags, proto);
        }

        const h = try this.hashObjectLiteral(flags, elements, proto);
        if (this.canonical_types.get(h)) |c| {
            //tmp.deinit();
            // FIXME: we don't clean-up any of the allocated types
            return c;
        }
        const c = try this._createObjectLiteral(elements, flags, proto);
        try this.canonical_types.put(h, c);
        return c;
    }

    fn createRefinement(this: *@This(), source_ref: TypeRef, member: ObjectLiteralMember) anyerror!TypeRef {
        const resolved = try this.maybeResolveAlias(source_ref);
        const source = this.types.at(resolved);

        if (source.getKind() == .@"union") {
            const elements = getSlice2(source, TypeRef);
            const hash = try this.getMemberNameHash(member.name);

            var tmp = TempUnion.init(this.allocator());

            for (elements) |el| {
                const prop_type = try this.accessTypeWithHash(el, member.name, hash, false) orelse {
                    continue;
                };

                if (member.type == prop_type) {
                    // add directly
                    if (try this.addToUnion(&tmp, el)) |x| return x;
                    continue;
                } 

                if (!try this.isAssignableTo(member.type, prop_type)) continue;

                const refined = try this.createRefinement(el, member);
                if (try this.addToUnion(&tmp, refined)) |x| return x;
            }

            return tmp.complete(this);
        }

        if (source.getKind() == .object_literal) {
            const members = getSlice2(source, ObjectLiteralMember);
            if (!ObjectLiteralFlags.isRefined(source)) {
                for (members) |*m| {
                    if (m.name != member.name) continue;

                    if (m.kind == member.kind and try m.getType(this) == member.type) {
                        return source_ref;
                    } else break;
                }
  
                // create a new refined type, setting the source as the proto
                var arr = try this.allocator().alloc(ObjectLiteralMember, 1);
                arr[0] = member;

                return this.createObjectLiteral(arr, source.flags | @intFromEnum(ObjectLiteralFlags.refined), source_ref);
            }

            for (members, 0..) |m, i| {
                if (m.name != member.name) continue;

                if (m.type == member.type and member.kind == m.kind and m.flags == member.flags) {
                    return source_ref;
                }

                const copy = try this.allocator().alloc(ObjectLiteralMember, members.len);
                @memcpy(copy, members);
                members[i] = member;

                return this.createObjectLiteral(members, source.flags, source.slot3);
            }

            var copy = try this.allocator().alloc(ObjectLiteralMember, members.len+1);
            @memcpy(copy[0..members.len], members);
            members[members.len] = member;

            return this.createObjectLiteral(members, source.flags, source.slot3);
        }

        return notSupported(source.getKind());
    }


    fn hashFunctionLiteral(params: []const TypeRef, return_type: TypeRef, this_type: TypeRef, flags: u24) u64 {
        var h = std.hash.Wyhash.init(0);
        h.update(&.{@as(u8, @intFromEnum(Kind.function_literal))});
        h.update(&@as([3]u8, @bitCast(flags)));
        h.update(&@as([4]u8, @bitCast(@as(u32, @intCast(params.len)))));

        for (params) |p| {
            h.update(&@as([4]u8, @bitCast(p)));
        }

        h.update(&@as([4]u8, @bitCast(return_type)));
        h.update(&@as([4]u8, @bitCast(this_type)));

        return h.final();
    }

    fn createCanonicalFunctionLiteral(this: *@This(), params: Params, return_type: TypeRef, flags: u24) !TypeRef {
        const key = hashFunctionLiteral(params.params, return_type, params.this_type, flags);
        if (this.canonical_types.get(key)) |t| {
            this.allocator().free(params.params);
            return t;
        }

        const t = try this.createFunctionLiteralFull(params.params, return_type, params.this_type, flags);
        try this.canonical_types.put(key, t);

        return t;
    }

    fn createFunctionLiteralFull(this: *@This(), params: []const TypeRef, return_type: TypeRef, this_type: TypeRef, flags: u24) !TypeRef {
        const _flags = flags | (if (this.hasThisType(return_type)) @intFromEnum(Flags.has_this_type) else 0); // XXX
        const x: u64 = @intFromPtr(params.ptr);
        return this.types.push(.{
            .kind = @intFromEnum(Kind.function_literal),
            .flags = _flags,
            .slot0 = @truncate(x),
            .slot1 = @intCast(x >> 32),
            .slot2 = @intCast(params.len),
            .slot3 = return_type,
            .slot4 = this_type,
        });
    }

    fn createFunctionLiteral(this: *@This(), params: []const TypeRef, return_type: TypeRef, flags: u24) !TypeRef {
        return this.createFunctionLiteralFull(params, return_type, 0, flags);
    }

    fn createClassType(this: *@This(), instance_type: TypeRef, static_type: TypeRef, constructor: TypeRef, base_class: TypeRef, flags: u24) !TypeRef {
        return this.types.push(.{
            .kind = @intFromEnum(Kind.class),
            .flags = flags,
            .slot0 = instance_type,
            .slot1 = static_type,
            .slot2 = constructor,
            .slot3 = base_class,
        });
    }

    fn createIntersectionType(this: *@This(), elements: []const TypeRef, flags: u24) !TypeRef {
        std.debug.assert(elements.len > 1);
        const x: u64 = @intFromPtr(elements.ptr);
        return this.types.push(.{
            .kind = @intFromEnum(Kind.intersection),
            .flags = flags,
            .slot0 = @truncate(x),
            .slot1 = @intCast(x >> 32),
            .slot2 = @intCast(elements.len),
        });
    }

    fn createIntersectionType2(this: *@This(), elements: *TypeList, flags: u24) !TypeRef {
        const _elements = try elements.toAllocated(this.allocator());
        std.debug.assert(_elements.len > 1);
        const x: u64 = @intFromPtr(_elements.ptr);
        return this.types.push(.{
            .kind = @intFromEnum(Kind.intersection),
            .flags = flags | elements.flags,
            .slot0 = @truncate(x),
            .slot1 = @intCast(x >> 32),
            .slot2 = @intCast(_elements.len),
        });
    }

    fn createNamedTupleTypeFromIdent(this: *@This(), file_id: u32, ident: NodeRef, ty: TypeRef, flags: u24) !TypeRef {
        const ident_hash = this.program.files.items[file_id].binder.nodes.at(ident).extra_data2;
        var h = std.hash.Wyhash.init(0);
        h.update(&.{@as(u8, @intFromEnum(Kind.named_tuple_element))});
        h.update(&@as([3]u8, @bitCast(flags)));
        h.update(&@as([4]u8, @bitCast(ident_hash)));
        h.update(&@as([4]u8, @bitCast(ty)));

        const key = h.final();
        if (this.canonical_types.get(key)) |t| return t;
        
        const t = try this.types.push(.{
            .kind = @intFromEnum(Kind.named_tuple_element),
            .slot0 = ident,
            .slot1 = ty,
            .slot4 = file_id,
            .flags = flags,
        });

        try this.canonical_types.put(key, t);
        return t;
    }

    fn createAliasNoArgs(this: *@This(), file_id: u32, sym: SymbolRef, flags: u24) !TypeRef {
        return this.createCanonicalAlias(@constCast(&.{}), file_id, sym, flags);
    }

    // `slot4` can be a global symbol ref, `slot3` will be maxInt(u32) in this case
    // `slot5` caches the immediate substitution
    // `slot6` caches the full evaluation
    fn createCanonicalAlias(this: *@This(), args: *TypeList, file_id: u32, sym_ref: SymbolRef, flags: u24) !TypeRef {
        std.debug.assert(sym_ref != 0);

        var h = std.hash.Wyhash.init(0);
        h.update(&.{@as(u8, @intFromEnum(Kind.alias))});
        h.update(&@as([3]u8, @bitCast(flags)));

        for (args.getSlice()) |a| h.update(&@as([4]u8, @bitCast(a)));
        h.update(&@as([4]u8, @bitCast(file_id)));
        h.update(&@as([4]u8, @bitCast(sym_ref)));

        const hash = h.final();
        if (this.canonical_types.get(hash)) |t| {
            args.deinit(this.allocator());
            return t;
        }

        var t = Type.init(.alias);
        t.flags = flags;
        t.slot3 = file_id;
        t.slot4 = sym_ref;

        try args.writeToType(this.allocator(), &t);

        const ref = try this.types.push(t);
        try this.canonical_types.put(hash, ref);

        return ref;
    }

    fn applyMappedFlags(mapping_flags: u32, element_flags: u24) u24 {
        var result = element_flags;

        if (mapping_flags & @intFromEnum(NodeFlags.minus_readonly) == @intFromEnum(NodeFlags.minus_readonly)) {
            result &= ~@as(u24, @intFromEnum(NodeFlags.readonly));
        } else if (mapping_flags & @intFromEnum(NodeFlags.readonly) == @intFromEnum(NodeFlags.readonly)) {
            result |= @as(u24, @intFromEnum(NodeFlags.readonly));
        }

        if (mapping_flags & @intFromEnum(NodeFlags.minus_optional) == @intFromEnum(NodeFlags.minus_optional)) {
            result &= ~@as(u24, @intFromEnum(NodeFlags.optional));
        } else if (mapping_flags & @intFromEnum(NodeFlags.optional) == @intFromEnum(NodeFlags.optional)) {
            result |= @as(u24, @intFromEnum(NodeFlags.optional));
        }

        return result;
    }

    fn mappedTypeWorker(this: *@This(), reg: u32, values: []const TypeRef, value_flags: ?[]const u24, exp: TypeRef, rename: TypeRef, node_flags: u32) !?TypeRef {
        // Mapping to the same name results in a union of types at that field
        // It always maps to a property regardless of the source (method, accessor, etc.)
        const TempMember = struct {
            name: TypeRef,
            ty: TypeRef,
            flags: u24,
            ty_list: ?std.ArrayList(TypeRef) = null,

            pub fn addType(self: *@This(), ty: TypeRef) !void {
                var l = if (self.ty_list) |*x| x else blk: {
                    var types = try std.ArrayList(TypeRef).initCapacity(getAllocator(), 2);
                    types.appendAssumeCapacity(self.ty);
                    self.ty_list = types;

                    break :blk &(self.ty_list orelse unreachable);
                };

                try l.append(ty);
            }

            pub fn deinit(self: *@This()) void {
                if (self.ty_list) |*l| l.deinit();
            }

            pub fn complete(self: *@This(), analyzer: *Analyzer) !ObjectLiteralMember {
                defer self.deinit();

                const t = if (self.ty_list) |l| try analyzer.toUnion(l.items) else self.ty;

                // TODO: check for more things (e.g. union)
                if (self.name == @intFromEnum(Kind.string) or self.name == @intFromEnum(Kind.number) or self.name == @intFromEnum(Kind.symbol)) {
                    return .{
                        .kind = .index,
                        .name = self.name,
                        .type = t,
                        .flags = self.flags,
                    };
                }

                return .{
                    .kind = .property,
                    .name = self.name,
                    .type = t,
                    .flags = self.flags,
                };
            }
        };

        var members = try std.ArrayList(TempMember).initCapacity(this.type_args.allocator, values.len);
        defer members.deinit(); // TODO (LEAK): doesn't clean up elements on error or early return

        const old_value = this.type_registers[reg];
        defer this.type_registers[reg] = old_value;

        outer: for (values, 0..) |v, i| {
            this.type_registers[reg] = v;

            const f = try this.resolveParameterizedType(exp) orelse exp;
            if (this.isParameterizedRef(f)) return null;

            const mapped_flags = applyMappedFlags(
                node_flags,
                if (value_flags) |arr| arr[i] else 0,
            );

            if (rename != 0) {
                const r = try this.resolveParameterizedType(rename) orelse rename;
                if (this.isParameterizedRef(r)) return null;

                for (members.items) |*m| {
                    if (m.name == r) {
                        try m.addType(f);
                        continue :outer;
                    }
                }

                members.appendAssumeCapacity(.{
                    .name = r,
                    .ty = f,
                    .flags = mapped_flags,
                });
            } else {
                members.appendAssumeCapacity(.{
                    .name = v,
                    .ty = f,
                    .flags = mapped_flags,
                });
            }
        }

        var resolved = try std.ArrayList(ObjectLiteralMember).initCapacity(members.allocator, members.items.len);
        errdefer resolved.deinit();

        for (members.items) |*m| {
            resolved.appendAssumeCapacity(try m.complete(this));
        }

        return try createObjectLiteral(this, resolved.items, 0, 0);
    }

    fn resolveMappedTypeInner(this: *@This(), param: TypeRef, exp: TypeRef, rename: TypeRef, node_flags: u32) !?TypeRef {
        const p = this.types.at(param);
        const constraint = try this.resolveParameterizedType(p.slot1) orelse p.slot1;
        const c = try this.maybeResolveAlias(constraint);

        const t: ?TypeRef = blk: {
            if (this.isParameterizedRef(c)) break :blk null;

            const slice = this.maybeGetUnion(c) orelse &.{c};
            // TODO: preserve modifiers when we know the constraint is a subtype of `keyof T`

            break :blk try this.mappedTypeWorker(p.slot3, slice, null, exp, rename, node_flags);
        };

        return t orelse {
            const e = try this.resolveParameterizedType(exp) orelse exp;
            const r = if (rename == 0) rename else try this.resolveParameterizedType(rename) orelse rename;
            if (p.slot1 == constraint and exp == e and rename == r) return null;

            return error.TODO_mapped_type;
            //return try this.createMappedType(p, e, r, node_flags);
        };
    }

    fn resolveMappedType(this: *@This(), param: TypeRef, exp: TypeRef, rename: TypeRef, node_flags: u32) !TypeRef {
        return try this.resolveMappedTypeInner(param, exp, rename, node_flags) orelse
            this.createMappedType(param, exp, rename, node_flags);
    }

    // type X<T> = T extends keyof infer U ? U : never
    // type Y = X<'a' | 'b'> // { a: any } | { b: any }

    // type A = [1, 2, 3]
    // type B = keyof A
    // type F<T> = number extends T ? T : T extends number ? T : never
    // type C = F<B> // keyof A ??????

    fn keyofObjectLiteral(this: *@This(), tmp: *TempUnion, type_ref: TypeRef) !?TypeRef {
        for (getSlice2(this.types.at(type_ref), ObjectLiteralMember)) |m| {
            const key_type = switch (m.kind) {
                .call_signature => continue,
                .property, .getter, .setter, .method => blk: {
                    if (!m.isPublic()) continue;

                    break :blk m.name;
                },
                .index => blk: {
                    if (m.name >= @intFromEnum(Kind.false)) {
                        if (m.name == @intFromEnum(Kind.string)) {
                            // TODO: apparently this should be `string | number` if the
                            // type didn't come from a mapped type?
                            break :blk m.name;
                        }
                        break :blk m.name;
                    }

                    const n2 = this.types.at(m.name);
                    if (n2.getKind() == .named_tuple_element) break :blk n2.slot1;
                    break :blk m.name;
                },
            };

            if (try this.addToUnion(tmp, key_type)) |x| return x;

            if (tmp.hasFlag(.has_number) and tmp.hasFlag(.has_string) and tmp.hasFlag(.has_symbol)) break; // TODO: caller doesn't know to complete the union
        }

        return null;
    }

    fn evaluateKeyOfType(this: *@This(), r: TypeRef) anyerror!TypeRef {
        // TODO: `keyof` works on non-object types if the type has an associated proto e.g. `number` and `string`
        // `keyof` gathers keys from the prototype chain
        if (r >= @intFromEnum(Kind.false)) {
            if (r == @intFromEnum(Kind.any)) {
                return this.toUnion(&.{
                    @intFromEnum(Kind.number),
                    @intFromEnum(Kind.string),
                    @intFromEnum(Kind.symbol),
                });
            }
            if (r == @intFromEnum(Kind.empty_object)) {
                return @intFromEnum(Kind.never);
            }
            if (r == @intFromEnum(Kind.this)) {
                if (this.contextual_this_type == 0) {
                    return error.MissingContextualThisType;
                }
                if (this.contextual_this_type == @intFromEnum(Kind.this)) {
                    return this.types.push(.{
                        .kind = @intFromEnum(Kind.keyof),
                        .slot0 = this.contextual_this_type,
                    });
                }
                return this.evaluateKeyOfType(this.contextual_this_type);
            }
            this.printTypeInfo(r);
            return error.TODO;
        }

        const n = this.types.at(r);
        switch (n.getKind()) {
            .object_literal => {
                var tmp = TempUnion.init(this.type_args.allocator);
                if (try this.keyofObjectLiteral(&tmp, r)) |x| return x;

                var proto = n.slot3;
                while (proto != 0) {
                    const instance = try this.maybeResolveAlias(proto);
                    if (instance < @intFromEnum(Kind.false) and this.types.at(instance).getKind() == .object_literal) {
                        if (try this.keyofObjectLiteral(&tmp, instance)) |x| return x;
                        proto = this.types.at(instance).slot3;
                    } else {
                        break;
                    }
                }

                return tmp.complete(this);
            },
            .intersection => {
                // TODO: this shouldn't need to allocate a union for each member of the intersection
                var tmp = TempUnion.init(this.type_args.allocator);
                for (getSlice2(n, TypeRef)) |el| {
                    const key_type = try this.evaluateKeyOfType(el);
                    if (try this.addToUnion(&tmp, key_type)) |x| return x;
                }

                return tmp.complete(this);
            },
            .alias => {
                const followed = try this.maybeResolveAlias(r);
                if (followed == r) {
                    // TODO: instance aliases don't need to be directly followed
                    this.printTypeInfo(r);
                    return error.TODO_keyof_unfollowable_alias;
                }

                return this.evaluateKeyOfType(followed);
            },
            // .array => {
            //     const ty = try this.getGlobalTypeSymbolAlias("Array", .{}) orelse error.MissingArray;

            // },
            // TODO: union should be all common keys
            // .@"union" => {

            // },
            else => return notSupported(n.getKind()),
        }
    }

    fn createStringLiteral(this: *@This(), file_id: u32, ref: NodeRef, text: []const u8, string_flags: u24) !TypeRef {
        if (text.len == 0) {
            return @intFromEnum(Kind.empty_string);
        }

        // FIXME: too many hashes
        const slot2: u32 = @truncate(std.hash.Wyhash.hash(0, text));

        var h = std.hash.Wyhash.init(0);
        const bits: [4]u8 = @bitCast(@intFromEnum(Kind.string_literal));
        h.update(&bits);
        h.update(&@as([4]u8, @bitCast(slot2)));
        h.update(&@as([3]u8, @bitCast(string_flags)));

        const key = h.final();
        if (this.canonical_types.get(key)) |t| {
            return t;
        }

        const t = try this.types.push(.{
            .kind = @intFromEnum(Kind.string_literal),
            .slot0 = file_id,
            .slot1 = ref,
            .slot2 = slot2,
            .slot3 = string_flags,
        });

        try this.canonical_types.put(key, t);
        return t;
    }

    fn createSyntheticStringLiteral(this: *@This(), text: []const u8, comptime deinit: bool) !TypeRef {
        if (text.len == 0) {
            return @intFromEnum(Kind.empty_string);
        }

        const string_flags: u24 = @intFromEnum(StringFlags.single_quote);

        // FIXME: too many hashes
        const slot2: u32 = @truncate(std.hash.Wyhash.hash(0, text));

        var h = std.hash.Wyhash.init(0);
        const bits: [4]u8 = @bitCast(@intFromEnum(Kind.string_literal));
        h.update(&bits);
        h.update(&@as([4]u8, @bitCast(slot2)));
        h.update(&@as([3]u8, @bitCast(string_flags)));

        const key = h.final();
        if (this.canonical_types.get(key)) |t| {
            if (deinit) {
                this.allocator().free(text);
            }
            return t;
        }

        const index = this.synthetic_strings.items.len;
        try this.synthetic_strings.append(text);

        const t = try this.types.push(.{
            .kind = @intFromEnum(Kind.string_literal),
            .slot0 = @intCast(index),
            .slot2 = slot2,
            .slot3 = string_flags,
            .slot5 = 1,
        });

        try this.canonical_types.put(key, t);
        return t;
    }

    fn getMemberType(this: *@This(), file: *ParsedFileData, ref: NodeRef) !TypeRef {
        const n = file.ast.nodes.at(ref);
        switch (n.kind) {
            .property_signature => {
                return try this.getType(file, getPackedData(n).right);
            },
            .property_declaration => {
                if (n.len != 0) {
                    return this.getType(file, n.len);
                }

                const data = getPackedData(n);

                return if (data.right != 0)
                    try this.getType(file, data.right)
                else
                    @intFromEnum(Kind.any);
            },
            .method_signature => {
                var flags: u24 = 0;
                const d2 = getPackedData(n);
                const ty = try this.getType(file, n.len);
                const params_with_this = try this.getParamsWithThisType(file, d2.right);
                if (this.isParameterizedRef(ty)) flags |= @intFromEnum(Flags.parameterized);
                const inner = try this.createFunctionLiteralFull(params_with_this.params, ty, params_with_this.this_type, flags);

                if (n.extra_data != 0) {
                    return this.createParameterizedType(try this.getTypeParams(file, n.extra_data), inner);
                }

                return inner;
            },
            .method_declaration => return try this.getSignature(file, ref),
            else => {
                return error.TODO_getMemberType;
            },
        }
    }

    // XXX: do this during binding? That impl. would be cleaner and faster, this one is brittle
    fn peekIsParameterized(this: *@This(), file: *ParsedFileData, ref: NodeRef) anyerror!bool {
        if (ref == 0) return false;

        const n = file.ast.nodes.at(ref);
        switch (n.kind) {
            .identifier => {
                if (file.binder.getTypeSymbol(ref)) |s| {
                    return file.binder.symbols.at(s).hasFlag(.type_parameter);
                }
            },
            .parenthesized_type => return this.peekIsParameterized(file, unwrapRef(n)),
            .property_signature => {
                if (try this.peekIsParameterized(file, getPackedData(n).right)) {
                    return true;
                }
            },
            .property_declaration => {
                if (try this.peekIsParameterized(file, n.len)) {
                    return true;
                }

                // TODO: check computed name
                // TODO: check init (not needed for .d.ts files)
            },
            .function_type => {
                var iter = NodeIterator.init(&file.ast.nodes, getPackedData(n).left);
                while (iter.nextRef()) |r| {
                    if (try this.peekIsParameterized(file, r)) {
                        return true;
                    }
                }
                if (try this.peekIsParameterized(file, getPackedData(n).right)) {
                    return true;
                }
            },
            .union_type, .intersection_type, .indexed_access_type => {
                if (try this.peekIsParameterized(file, getPackedData(n).left)) {
                    return true;
                }
                if (try this.peekIsParameterized(file, getPackedData(n).right)) {
                    return true;
                }
            },
            .parameter => {
                if (try this.peekIsParameterized(file, n.len)) {
                    return true;
                }
            },
            .method_signature => {
                var iter = NodeIterator.init(&file.ast.nodes, getPackedData(n).right);
                while (iter.nextRef()) |r| {
                    if (try this.peekIsParameterized(file, r)) {
                        return true;
                    }
                }
                if (try this.peekIsParameterized(file, n.len)) {
                    return true;
                }
            },
            .method_declaration => {
                var iter = NodeIterator.init(&file.ast.nodes, getPackedData(n).right);
                while (iter.nextRef()) |r| {
                    if (try this.peekIsParameterized(file, r)) {
                        return true;
                    }
                }

                if (try this.peekIsParameterized(file, n.extra_data2)) {
                    return true;
                }
            },
            .expression_with_type_arguments, .type_reference => {
                var iter = NodeIterator.init(&file.ast.nodes, getPackedData(n).right);
                while (iter.nextRef()) |r| {
                    if (try this.peekIsParameterized(file, r)) {
                        return true;
                    }
                }
            },
            .type_literal => {
                const first_member = maybeUnwrapRef(n) orelse return false;
                var iter = NodeIterator.init(&file.ast.nodes, first_member);
                while (iter.nextRef()) |r| {
                    if (try this.peekIsParameterized(file, r)) {
                        return true;
                    }
                }
            },
            .template_literal_type => {
                // TODO: loop over
            },
            else => {},
        }

        return false;
    }

    fn getClassType(this: *@This(), file: *ParsedFileData, ref: NodeRef) !TypeRef {
        const n = file.ast.nodes.at(ref);
        const d = getPackedData(n);

        var instance_flags: u24 = 0;
        var static_flags: u24 = 0;

        var constructor: TypeRef = 0;

        var instance_members = std.ArrayList(ObjectLiteralMember).init(this.type_args.allocator);
        var static_members = std.ArrayList(ObjectLiteralMember).init(this.type_args.allocator);

        var instance_alias: TypeRef = 0;
        if (d.left != 0) {
            const sym_ref = file.ast.nodes.at(d.left).extra_data;
            instance_alias = try this.createAliasNoArgs(file.id, sym_ref, @intFromEnum(Flags.instance_alias));
        }

        // TODO: all classes have `prototype` in their static members
        // the proto should include non-static methods and accessors

        var body_iter = NodeIterator.init(&file.ast.nodes, d.right);
        while (body_iter.nextPair()) |pair| {
            const is_static = pair[0].hasFlag(.static);
            const members = if (is_static) &static_members else &instance_members;
            const flags = if (is_static) &static_flags else &instance_flags;

            switch (pair[0].kind) {
                .constructor => {
                    const data = getPackedData(pair[0]);

                    const params_with_this = try this.getParamsWithThisType(file, data.left);
                    for (params_with_this.params) |param| {
                        const x = this.types.at(param);
                        if (hasTypeFlag(x, .parameter_decl)) try instance_members.append(.{
                            .kind = .property,
                            .name = try this.propertyNameToType(file, x.slot0),
                            .type = x.slot1,
                        });
                    }

                    // FIXME: overloads
                    // FIXME: parameterized check
                    constructor = try this.createFunctionLiteral(params_with_this.params, instance_alias, @intFromEnum(Flags.constructor));
                },
                .property_declaration => {
                    const data = getPackedData(pair[0]);
                    if (try this.peekIsParameterized(file, pair[1])) {
                        flags.* |= @intFromEnum(Flags.parameterized);
                    }

                    var member_flags: u24 = pair[0].flags;
                    if (file.ast.nodes.at(data.left).kind == .private_identifier) {
                        member_flags |= ObjectLiteralMember.private_ident_flag;
                    }

                    const name = try this.propertyNameToType(file, data.left);
                    try members.append(ObjectLiteralMember.initLazy(.property, name, file.id, pair[1], member_flags));
                },
                .method_declaration => {
                    if (try this.peekIsParameterized(file, pair[1])) {
                        flags.* |= @intFromEnum(Flags.parameterized);
                    }

                    const name_ref = (getPackedData(pair[0])).left;
                    var member_flags: u24 = pair[0].flags;
                    if (file.ast.nodes.at(name_ref).kind == .private_identifier) {
                        member_flags |= ObjectLiteralMember.private_ident_flag;
                    }

                    const name = try this.propertyNameToType(file, name_ref);
                    try members.append(ObjectLiteralMember.initLazy(.method, name, file.id, pair[1], member_flags));
                },
                else => return error.TODO,
            }
        }

        const base_class = if (n.len != 0) try this.getType(file, n.len) else 0;

        const instance_proto = if (base_class != 0) try this.getInstanceType(base_class, false) else 0;

        const instance_type = try this.createObjectLiteral(instance_members.items, instance_flags, instance_proto);
        const static_type = try this.createObjectLiteral(static_members.items, static_flags, 0);

        var flags: u24 = 0;
        if (this.isParameterizedRef(instance_type) or this.isParameterizedRef(static_type)) flags |= @intFromEnum(Flags.parameterized);

        const type_ref = try this.createClassType(instance_type, static_type, constructor, base_class, flags);

        if (instance_alias != 0) {
            this.types.at(type_ref).slot6 = instance_alias;
        }

        return type_ref;
        // len is extends
        // right is body
        // extra_data is type params
        // extra_data2 is implements
    }

    // Interfaces merge with classes by augmenting the instance type
    // Namepsaces turn functions into objects with call signatures
    // There will never be more than 1 class declaration
    //
    // Type parameters are merged individually but must not conflict with each other.
    // Example:
    //
    // class Foo2<T, U extends string> {}
    // interface Foo2<T extends string, U = string> {}
    //
    // Results in <T extends string, U extends string = string>
    const Merged = struct {
        const TempTypeParam = struct {
            ref: TypeRef,
            param: Type,
            did_change: bool = false,
        };

        analyzer: *Analyzer,
        class_type: TypeRef = 0,
        type_params: std.ArrayListUnmanaged(TempTypeParam) = std.ArrayListUnmanaged(TempTypeParam){},
        elements: std.ArrayListUnmanaged(ObjectLiteralMember) = std.ArrayListUnmanaged(ObjectLiteralMember){},

        fn addTypeParams(this: *@This(), params: []const TypeRef) !void {
            if (this.type_params.items.len == 0) {
                try this.type_params.ensureTotalCapacityPrecise(this.analyzer.allocator(), params.len);
                for (params) |ref| {
                    this.type_params.appendAssumeCapacity(.{
                        .ref = ref,
                        .param = this.analyzer.types.at(ref).*,
                    });
                }
                return;
            } else if (params.len != this.type_params.items.len) {
                // Semantic error
                return error.MismatchedTypeParams;
            }

            // FIXME: check for conflicts
            for (params, 0..) |ref, i| {
                const p1 = this.analyzer.types.at(ref);
                const p2 = &this.type_params.items[i];
                if (p1.slot1 != 0 and p2.param.slot1 == 0) {
                    p2.param.slot1 = p1.slot1;
                    p2.did_change = true;
                }
                if (p1.slot2 != 0 and p2.param.slot2 == 0) {
                    p2.param.slot2 = p1.slot2;
                    p2.did_change = true;
                }
            }
        }

        pub fn addType(this: *@This(), type_ref: TypeRef) !void {
            if (type_ref >= @intFromEnum(Kind.false)) {
                return error.TODO_merge_primitive_decl;
            }

            const k = this.analyzer.types.at(type_ref);
            switch (k.getKind()) {
                .object_literal => {
                    // TODO: validate each addition
                    const members = getSlice2(k, ObjectLiteralMember);
                    try this.elements.ensureUnusedCapacity(this.analyzer.allocator(), members.len);

                    for (getSlice2(k, ObjectLiteralMember)) |el| {
                        this.elements.appendAssumeCapacity(el);
                    }
                },
                .function_literal => {
                    try this.elements.append(this.analyzer.allocator(), .{
                        .kind = .call_signature,
                        .name = 0,
                        .type = type_ref,
                    });
                },
                .class => {
                    std.debug.assert(this.class_type == 0);
                    this.class_type = type_ref;
                },
                .module_namespace => {
                    // TODO
                },
                .parameterized => {
                    const inner_ref = k.slot3;
                    if (inner_ref >= @intFromEnum(Kind.false)) {
                        if (inner_ref == @intFromEnum(Kind.empty_object)) {
                            try this.addTypeParams(getSlice2(k, TypeRef));
                            return;
                        }

                        this.analyzer.printTypeInfo(inner_ref);
                        return error.TODO_global_parameterized_primitive_merge;
                    }

                    const inner = this.analyzer.types.at(inner_ref);

                    if (inner.getKind() == .function_literal) {
                        try this.elements.append(this.analyzer.allocator(), .{
                            .kind = .call_signature,
                            .name = 0,
                            .type = type_ref,
                        });
                    } else if (inner.getKind() == .object_literal) {
                        try this.addTypeParams(getSlice2(k, TypeRef));
                        for (getSlice2(inner, ObjectLiteralMember)) |el| {
                            try this.elements.append(this.analyzer.allocator(), el);
                        }
                    } else if (inner.getKind() == .class) {
                        try this.addTypeParams(getSlice2(k, TypeRef));
                        this.class_type = inner_ref;
                    } else {
                        _ = try notSupported(inner.getKind());
                    }
                },
                else => {
                    _ = try notSupported(k.getKind());
                },
            }
        }

        pub fn complete(this: *@This()) !TypeRef {
            if (this.class_type != 0) {
                // TODO
                std.debug.print("TODO: class_type", .{});
                return this.class_type;
            }

            if (this.type_params.items.len != 0) {
                defer this.type_params.deinit(this.analyzer.allocator());

                // TODO (perf): micro-optimization: re-use first type param list if no params change
                const inner = try this.analyzer.createObjectLiteral(this.elements.items, @intFromEnum(Flags.parameterized), 0);
                const params = try this.analyzer.allocator().alloc(TypeRef, this.type_params.items.len);
                for (this.type_params.items, 0..) |p, i| {
                    if (!p.did_change) {
                        params[i] = p.ref;
                    } else {
                        params[i] = try this.analyzer.types.push(p.param);
                    }
                }

                return try this.analyzer.createParameterizedType(params, inner);
            }

            return try this.analyzer.createObjectLiteral(this.elements.items, 0, 0);
        }
    };

    inline fn getTypeOfAbsoluteSymbol(this: *@This(), abs_ref: AbsoluteSymbolRef) !TypeRef {
        // we do not want to treat immediate symbols as global when finding their type
        return this._getTypeOfSymbol(this.program.getFileData(abs_ref.file_id), abs_ref.ref, false);
    }

    fn getTypeOfGlobalSymbol(this: *@This(), global_ref: Ambient.GlobalRef) !TypeRef {
        const g = this.program.ambient.globals_allocator.at(global_ref);

        if (g.type != 0) return g.type;

        if (g.symbols.items.len == 1) {
            g.type = try this.getTypeOfAbsoluteSymbol(g.symbols.items[0]);
            return g.type;
        }

        if (g.symbols.items.len == 0) {
            return @intFromEnum(Kind.any);
            // this.printCurrentNode();
            // return error.MissingGlobalDeclarations;
        }

        var merged = Merged{ .analyzer = this };
        for (g.symbols.items) |abs_ref| {
            try merged.addType(try this.getTypeOfAbsoluteSymbol(abs_ref));
        }

        g.type = try merged.complete();

        return g.type;
    }

    fn getTypeOfLateBoundSymbol(this: *@This(), file: *ParsedFileData, sym: *const parser.Symbol, sym_ref: SymbolRef) !TypeRef {
        // Check for `globalThis`
        if (sym.declaration == std.math.maxInt(u32)) {
            return @intFromEnum(Kind.global_this);
        }

        if (sym.hasFlag(.local)) {
            return this.getTypeOfSymbol(this.program.getFileData(sym.getOrdinal()),sym.declaration);
        }

        if (hasSymbolFlag(sym, .imported) and hasSymbolFlag(sym, .namespace)) {
            const g = this.program.ambient.globals_allocator.at(sym.declaration);
            const f = this.program.getFileData(g.symbols.items[0].file_id);
            const r = g.symbols.items[0].ref;

            // Ambient module
            const t = try this.types.push(.{
                .kind = @intFromEnum(Kind.module_namespace),
                .slot0 = 0,
                .slot1 = sym_ref,
                .slot2 = file.id,
                .slot3 = f.binder.symbols.at(r).binding,
                .slot4 = f.id,
                .slot5 = 1,
            });

            try file.cached_symbol_types.put(this.allocator(), sym_ref, t);

            return t;
        }

        return this.getTypeOfGlobalSymbol(sym.declaration);
    }

    fn _getTypeOfSymbol(this: *@This(), file: *ParsedFileData, sym_ref: SymbolRef, comptime follow_global: bool) anyerror!TypeRef {
        const sym = file.binder.symbols.at(sym_ref);

        if (comptime follow_global) {
            if (hasSymbolFlag(sym, .global)) {
                return this.getTypeOfGlobalSymbol(sym.binding);
            }
        }

        if (file.cached_symbol_types.get(sym_ref)) |t| return t;

        if (hasSymbolFlag(sym, .late_bound)) {
            return this.getTypeOfLateBoundSymbol(file, sym, sym_ref);
        }

        if (hasSymbolFlag(sym, .imported)) {
            if (hasSymbolFlag(sym, .namespace)) {
                const t = try this.types.push(.{
                    .kind = @intFromEnum(Kind.module_namespace),
                    .slot0 = getOrdinal(sym),
                    .slot1 = sym_ref,
                    .slot2 = @intCast(file.id),
                });

                try file.cached_symbol_types.put(this.allocator(), sym_ref, t);

                return t;
            }

            const imported = this.program.getFileData(getOrdinal(sym));
            if (sym.declaration >> 30 == 1) {
                return this.getType(imported, sym.declaration & ~@as(u32, 0b11 << 30));
            }

            return this.getTypeOfSymbol(imported, sym.declaration);
        }

        if (sym.declaration == 0) return @intFromEnum(Kind.any);

        const t = try if (sym.next != 0)
            this.getTypeMultipleDeclarations(file, sym)
        else
            this.getType(file, sym.declaration);

        try file.cached_symbol_types.put(this.allocator(), sym_ref, t);

        return t;
    }

    pub fn getTypeOfSymbol(this: *@This(), file: *ParsedFileData, sym_ref: SymbolRef) anyerror!TypeRef {
        return this._getTypeOfSymbol(file, sym_ref, true);
    }

    fn getTypeMultipleDeclarations(this: *@This(), file: *ParsedFileData, sym: *const parser.Symbol) !TypeRef {
        const d = file.ast.nodes.at(sym.declaration);

        // Potentially overloaded, ignore the last decl (which is the first in the list)
        if (d.kind == .function_declaration) {
            var list = TypeList{};
            defer list.deinit(this.allocator());

            var next: SymbolRef = sym.next;
            while (next != 0) {
                const s = file.binder.symbols.at(next);
                if (s.declaration == 0) {
                    return error.MissingDeclaration;
                }

                try list.append(this, try this.getType(file, s.declaration));

                next = s.next;
            }

            if (list.count == 1) {
                return list.getSlice()[0];
            }

            var members = try this.allocator().alloc(ObjectLiteralMember, list.count);
            for (list.getSlice(), 0..) |t, i| {
                // Construct in reverse order
                members[members.len - (i + 1)] = .{
                    .kind = .call_signature,
                    .name = 0,
                    .type = t,
                };
            }

            return this.createObjectLiteral(members, list.flags, 0);
        }

        var merged = Merged{ .analyzer = this };
        try merged.addType(try this.getType(file, sym.declaration));

        var next: SymbolRef = sym.next;
        while (next != 0) {
            const s = file.binder.symbols.at(next);
            if (s.declaration == 0) {
                return error.MissingDeclaration;
            }

            try merged.addType(try this.getType(file, s.declaration));
            next = s.next;
        }

        return merged.complete();
    }

    fn getTypeArgs(this: *@This(), file: *ParsedFileData, args_start: NodeRef) !TypeList {
        var args = TypeList{};
        var iter = NodeIterator.init(&file.ast.nodes, args_start);
        while (iter.nextRef()) |r| {
            const t = try this.getType(file, r);
            try args.append(this, t);
        }

        return args;
    }

    fn evaluateQuery(this: *@This(), t: *Type, _: u32) !TypeRef {
        if (t.slot5 != 0) return t.slot5;

        const f = this.program.getFileData(t.slot3);
        const base = try this.getType(f, t.slot4);
        if (base >= @intFromEnum(Kind.false) or this.types.at(base).getKind() != .parameterized) {
            t.slot5 = base;
            return base;
        }

        t.slot5 = try this.resolveWithTypeArgsSlice(this.types.at(base), getSlice2(t, TypeRef));
        return t.slot5;
    }

    fn resolveQueryNode(this: *@This(), file: *ParsedFileData, ref: NodeRef) !TypeRef {
        const node = file.ast.nodes.at(ref);
        const base = try this.getType(file, unwrapRef(node));
        if (node.len == 0) return base;
        if (base >= @intFromEnum(Kind.false)) return error.NotParameterizedPrimitive;

        const resolved = try this.maybeResolveAlias(base);
        const base_type = this.types.at(resolved);
        if (base_type.getKind() != .parameterized) return error.NotParameterized;

        return this.resolveWithTypeArgs(file, base_type, node.len);
    }

    pub inline fn createParameterizedTypeFromParams(this: *@This(), file: *ParsedFileData, type_params_start: NodeRef, inner: TypeRef) anyerror!TypeRef {
        var params = std.ArrayList(TypeRef).init(this.type_args.allocator);
        var iter = NodeIterator.init(&file.ast.nodes, type_params_start);
        while (iter.nextPair()) |p| {
            try params.append(try this.getType(file, p[1]));
        }

        return try this.createParameterizedType(params.items, inner);
    }

    inline fn maybeGetCfaType(file: *ParsedFileData, sym_ref: SymbolRef) ?TypeRef {
        for (0..file.cfa_state.items.len) |i| {
            if (file.cfa_state.items[file.cfa_state.items.len - (i + 1)].types.get(sym_ref)) |t| {
                return t;
            }
        }
        return null;
    }

    fn getThisType(this: *@This(), file: *ParsedFileData, node: *const AstNode) !TypeRef {
        if (node.extra_data == 0) {
            return @intFromEnum(Kind.this);
        }

        if (maybeGetCfaType(file, node.extra_data)) |t| return t;
        if (file.cached_symbol_types.get(node.extra_data)) |t| return t;

        const type_ref = try this.getTypeOfSymbol(file, node.extra_data);
        if (type_ref < @intFromEnum(Kind.false)) {
            const t = this.types.at(type_ref);
            if (t.getKind() == .function_literal) {
                return t.slot4;
            }

            // This can potentially expose a bare parameter
            if (t.getKind() == .parameterized) {
                if (t.slot3 < @intFromEnum(Kind.false) and this.types.at(t.slot3).getKind() == .function_literal) {
                    return this.types.at(t.slot3).slot4;
                }
            }
        }

        return error.TODO_this_type;
    }

    pub inline fn getTypeAsConst(this: *@This(), file: *ParsedFileData, ref: NodeRef) anyerror!TypeRef {
        const old_ctx = this.is_const_context;
        this.is_const_context = true;
        defer this.is_const_context = old_ctx;
        return try this.getType(file, ref);
    }

    pub fn getType(this: *@This(), file: *ParsedFileData, ref: NodeRef) anyerror!TypeRef {
        const node = file.ast.nodes.at(ref);

        const _n = this.current_node;
        if (comptime is_debug) {
            this.current_node = .{file, ref};
        }
        defer this.current_node = _n;
        
        switch (node.kind) {
            .object_keyword => {
                return @intFromEnum(Kind.object);
            },
            .boolean_keyword => {
                return @intFromEnum(Kind.boolean);
            },
            .number_keyword => {
                return @intFromEnum(Kind.number);
            },
            .string_keyword => {
                return @intFromEnum(Kind.string);
            },
            .any_keyword => {
                return @intFromEnum(Kind.any);
            },
            .null_keyword => {
                return @intFromEnum(Kind.null);
            },
            .never_keyword => {
                return @intFromEnum(Kind.never);
            },
            .unknown_keyword => {
                return @intFromEnum(Kind.unknown);
            },
            .void_keyword => {
                return @intFromEnum(Kind.void);
            },
            .undefined_keyword => {
                return @intFromEnum(Kind.undefined);
            },
            .symbol_keyword => {
                return @intFromEnum(Kind.symbol);
            },
            .this_keyword => return this.getThisType(file, node),
            .qualified_name => {
                return this.resolveTypeFromQualifiedName(file, node);
            },
            .mapped_type => {
                const d = getPackedData(node);
                const param = try this.getType(file, d.left);

                // ` as ...`
                const rename = if (node.len != 0) try this.getType(file, node.len) else 0;

                const exp = try this.getType(file, d.right);

                const p = this.types.at(param);
                if (this.isParameterizedRef(p.slot1)) {
                    return this.createMappedType(param, exp, rename, node.flags);
                }

                return this.resolveMappedType(param, exp, rename, node.flags);
            },
            .expression_statement => {
                return this.getType(file, unwrapRef(node));
            },
            .type_query => {
                var args = try this.getTypeArgs(file, node.len);

                // We automatically reduce relevant type queries during CFA
                const inner_ref = unwrapRef(node);
                if (file.binder.getSymbol(inner_ref)) |sym_ref| {
                    if (maybeGetCfaType(file, sym_ref)) |t| {
                        if (t >= @intFromEnum(Kind.false)) return t;

                        if (this.types.at(t).getKind() == .parameterized) {
                            defer args.deinit(this.allocator());
                            return this.resolveWithTypeArgsSlice(this.types.at(t), args.getSlice());
                        }

                        return t;
                    }
                }

                if (node.len == 0) {
                    return this.types.push(.{
                        .kind = @intFromEnum(Kind.query),
                        .slot3 = @intCast(file.id),
                        .slot4 = inner_ref,
                    });
                }

                var t = Type{
                    .kind = @intFromEnum(Kind.query),
                    .slot3 = @intCast(file.id),
                    .slot4 = inner_ref,
                };

                try args.writeToType(this.allocator(), &t);

                return this.types.push(t);
            },
            .type_predicate => {
                const d = getPackedData(node);
                const sym = file.binder.getSymbol(d.left) orelse return error.MissingSymbol;
                const t = try this.getType(file, d.right);

                return this.types.push(.{
                    .kind = @intFromEnum(Kind.predicate),
                    .slot0 = sym,
                    .slot1 = t,
                    .slot2 = @as(u16, @truncate(file.binder.symbols.at(sym).ordinal)),
                    .slot3 = @intCast(file.id),
                    .slot4 = node.len,
                });
            },
            .type_operator => {
                const d = getPackedData(node);

                switch (@as(SyntaxKind, @enumFromInt(d.left))) {
                    .key_of_keyword => {
                        const t = try this.getType(file, d.right);

                        if (this.isParameterizedRef(t)) {
                            return this.types.push(.{
                                .kind = @intFromEnum(Kind.keyof),
                                .slot0 = t,
                                .flags = @intFromEnum(Flags.parameterized),
                            });
                        }

                        return this.evaluateKeyOfType(t);
                    },
                    .unique_keyword => {
                        const rhs = file.ast.nodes.at(d.right);
                        if (rhs.kind != .symbol_keyword) {
                            return error.InvalidUniqueType;
                        }

                        // XXX: FIXME: not exactly robust but it'll do
                        const prefix = "@@unique symbol#";
                        var h = std.hash.Wyhash.init(0);
                        h.update(prefix);
                        h.update(&@as([4]u8, @bitCast(this.types.count)));

                        return this.types.push(.{
                            .kind = @intFromEnum(Kind.symbol_literal),
                            .slot0 = this.types.count,
                            .slot1 = file.id,
                            .slot2 = ref,
                            .slot4 = @truncate(h.final()),
                        });
                    },
                    // This should for array/tuple literal types on the rhs to have a `readonly` flag.
                    // Make sure to handle caching!
                    .readonly_keyword => {
                        // FIXME: implement this
                        const t = try this.getType(file, d.right);
                        return t;
                        // return error.TODO;
                    },
                    .reify_keyword => {
                        return @intFromEnum(Kind.any); 
                    },
                    else => {},
                }
            },
            .array_type => {
                const t = try this.getType(file, unwrapRef(node));

                var h = std.hash.Wyhash.init(0);
                h.update(&@as([4]u8, @bitCast(@intFromEnum(Kind.array))));
                h.update(&@as([4]u8, @bitCast(t)));
                const hash = h.final();

                if (this.canonical_types.get(hash)) |r| {
                    return r;
                }

                const r = try this.createArrayType(t);
                try this.canonical_types.put(hash, r);
                return r;
            },
            .named_tuple_member => {
                const d = getPackedData(node);
                const inner = try this.getType(file, d.right);
                var flags: u24 = 0;
                if (node.hasFlag(.generator)) flags |= @intFromEnum(Flags.spread);
                if (node.hasFlag(.optional)) flags |= @intFromEnum(Flags.optional);
                if (this.isParameterizedRef(inner)) flags |= @intFromEnum(Flags.parameterized);

                return this.createNamedTupleTypeFromIdent(file.id, d.left, inner, flags);
            },
            .tuple_type => {
                // TODO: canonical type
                var flags: u24 = 0;
                var elements = std.ArrayList(TypeRef).init(this.type_args.allocator);
                var iter = NodeIterator.init(&file.ast.nodes, maybeUnwrapRef(node) orelse 0);
                while (iter.nextPair()) |p| {
                    switch (p[0].kind) {
                        .rest_type, .optional_type => {
                            const inner = try this.getType(file, unwrapRef(p[0]));
                            if (p[0].kind == .rest_type and (this.isTuple(inner) or inner == @intFromEnum(Kind.empty_tuple))) {
                                if (inner == @intFromEnum(Kind.empty_tuple)) continue;

                                if (this.isParameterizedRef(inner)) flags |= @intFromEnum(Flags.parameterized);

                                try this.reduceTupleElement(this.types.at(inner), &elements);
                                continue;
                            }

                            var element_flags: u24 = 0;
                            if (p[0].kind == .rest_type) element_flags |= @intFromEnum(Flags.spread);
                            if (p[0].kind == .optional_type) element_flags |= @intFromEnum(Flags.optional);
                            if (this.isParameterizedRef(inner)) {
                                flags |= @intFromEnum(Flags.parameterized);
                                element_flags |= @intFromEnum(Flags.parameterized);
                            }

                            try elements.append(
                                try this.createNamedTupleTypeFromIdent(file.id, 0, inner, element_flags),
                            );
                        },
                        else => {
                            const t = try this.getType(file, p[1]);
                            if (this.isParameterizedRef(t)) flags |= @intFromEnum(Flags.parameterized);

                            if (t < @intFromEnum(Kind.false) and hasTypeFlag(this.types.at(t), .spread)) {
                                const u = this.types.at(t).slot1;
                                if (u == @intFromEnum(Kind.empty_tuple)) continue;

                                if (this.isTuple(u)) {
                                    try this.reduceTupleElement(this.types.at(u), &elements);
                                } else {
                                    try elements.append(t);
                                }
                            } else {
                                try elements.append(t);
                            }
                        },
                    }
                }

                return this.createTupleType(elements.items, flags);
            },
            .identifier => {
                // Equivalent to `type_reference` with no args
                if ((node.flags >> 19) == 1) {
                    const r = try this.resolveTypeFromSymbol(file, ref, 0);

                    return r.ty;
                }

                if (file.binder.getSymbol(ref)) |sym_ref| {
                    const r = blk: {
                        if (maybeGetCfaType(file, sym_ref)) |t| break :blk t;

                        // XXX: each file should have a flow
                        if (this.current_flow) |f| {
                            if (f.file.id == file.id) {
                                if (f.maybeGetFlowType(sym_ref)) |t| break :blk t;
                            }
                        }

                        break :blk try this.getTypeOfSymbol(file, sym_ref);
                    };

                    if (r < @intFromEnum(Kind.false)) {
                        const t = this.types.at(r);
                        if (t.getKind() == .class) {
                            return this.createAliasNoArgs(file.id, sym_ref, @intFromEnum(Flags.class_alias));
                        } else if (t.getKind() == .parameterized) {
                            if (t.slot3 < @intFromEnum(Kind.false) and this.types.at(t.slot3).getKind() == .class) {
                                return this.createAliasNoArgs(file.id, sym_ref, @intFromEnum(Flags.class_alias));
                            }
                        }
                    }

                    return r;
                }
            },
            .parameter => {
                if (node.len != 0) {
                    return this.getType(file, node.len);
                }

                const d = getPackedData(node);
                if (d.right == 0) {
                    return @intFromEnum(Kind.any);
                }

                return this.getType(file, d.right);
            },
            .type_literal => {
                const start = maybeUnwrapRef(node) orelse return @intFromEnum(Kind.empty_object);              

                return this.resolveObjectType(file, start, 0, 0);
            },
            .indexed_access_type => {
                const d = getPackedData(node);
                const lhs = try this.getType(file, d.left);

                // Small optimization: if we see something like `T[keyof T]` then
                // we can potentially skip instantiating `keyof T`

                const rhs = try this.getType(file, d.right);
                if (lhs == 0 or rhs == 0) {
                    std.debug.print("{}, {}\n", .{ lhs, rhs });
                    return error.TODO;
                }

                if (this.isParameterizedRef(lhs) or this.isParameterizedRef(rhs)) {
                    return this.types.push(.{
                        .kind = @intFromEnum(Kind.indexed),
                        .slot0 = lhs,
                        .slot1 = rhs,
                        .flags = @intFromEnum(Flags.parameterized),
                    });
                }

                return this.accessType(lhs, rhs);
            },
            .infer_type => {
                const t = try this.getType(file, unwrapRef(node));
                this.types.at(t).flags |= @intFromEnum(Flags.infer_node);
                return t;
            },
            .type_parameter => {
                const d = getPackedData(node);
                const sym = file.binder.getTypeSymbol(ref) orelse return error.MissingTypeParam;

                const right = try this.getType(file, d.right);
                const default: u32 = if (node.len != 0) try this.getType(file, node.len) else 0;

                const t = try this.types.push(.{
                    .kind = @intFromEnum(Kind.type_parameter),
                    .slot0 = sym,
                    .slot1 = right,
                    .slot2 = default,
                    .slot3 = @as(u16, @truncate(file.binder.symbols.at(sym).ordinal)),
                    .slot4 = @intCast(file.id),
                    .slot6 = node.flags,
                    .flags = if (this.isParameterizedRef(right) or this.isParameterizedRef(default)) @intFromEnum(Flags.parameterized) else 0,
                });

                return t;
            },
            .interface_declaration => {
                const d = getPackedData(node);

                // FIXME: dedupe
                const sym_ref = file.binder.getTypeSymbol(ref) orelse return error.MissingSymbol;
                if (file.cached_symbol_types.get(sym_ref)) |t| return t;
                const key: u64 = (@as(u64, file.id) << 32) | sym_ref;

                const should_add = !(this.active_types.get(key) orelse false);

                if (should_add) try this.active_types.put(this.allocator(), key, true);
                defer {
                    if (should_add and !this.active_types.swapRemove(key)) @panic("failed to remove key");
                }

                // Each ref will contribute to this type's prototype which we will represent as an intersection
                // This is a restricted intersection because we do not permit narrowing

                var extends = std.ArrayList(TypeRef).init(this.allocator());
                var should_deinit = true;
                defer if (should_deinit) extends.deinit();

                var extends_iter = NodeIterator.init(&file.ast.nodes, node.extra_data);
                while (extends_iter.nextPair()) |pair| {
                    try extends.append(try this.getType(file, pair[1]));
                }

                const proto: TypeRef = blk: {
                    if (extends.items.len == 0) break :blk 0;
                    if (extends.items.len == 1) break :blk extends.items[0];
                    should_deinit = false;

                    break :blk try this.createIntersectionType(extends.items, 0);
                };

                const inner = try this.resolveObjectType(file, d.right, 0, proto);
                if (node.len != 0) {
                    const params = try this.getTypeParams(file, node.len);
                    const r = try this.createParameterizedType(params, inner);

                    return r;
                }

                return inner;
            },
            .enum_declaration => {
                // note: enum types are special case
                // their type symbol is essentially a namespace over their primitive type (see `keyof X`)
                // but their value symbol is an object, which is why `keyof typeof X` gives you the fields
                const d = getPackedData(node);
                var members = std.ArrayListUnmanaged(ObjectLiteralMember){};
                var iter = NodeIterator.init(&file.ast.nodes, d.right);

                const old_context = this.is_const_context;
                defer this.is_const_context = old_context;
                this.is_const_context = true;

                var prior_val: ?TypeRef = null;

                while (iter.next()) |m| {
                    const d2 = getPackedData(m);
                    const lhs = file.ast.nodes.at(d2.left);
                    std.debug.assert(lhs.kind == .identifier); // TODO: binder needs to treat `['ok']` as a symbol in enum declarations

                    const z = blk: {
                        if (d2.right != 0) {
                            break :blk try this.getType(file, d2.right);
                        }
                        if (prior_val) |x| {
                            const y: f64 = this.getDoubleFromType(x);
                            break :blk try this.numberToType(f64, y + 1);
                        }
                        break :blk @intFromEnum(Kind.zero);
                    };

                    prior_val = z;
                    try file.cached_symbol_types.put(this.allocator(), lhs.extra_data, z);

                    try members.append(this.allocator(), .{
                        .kind = .property,
                        .name = try this.propertyNameToType(file, d2.left),
                        .type = z,
                    });
                }

                return try this.createObjectLiteral(members.items, 0, 0);
            },
            .as_expression => {
                const d = getPackedData(node);
                if (file.ast.nodes.at(d.right).kind == .const_keyword) {
                    const old_context = this.is_const_context;
                    defer this.is_const_context = old_context;
                    this.is_const_context = true;

                    return this.getType(file, d.left);
                }

                return this.getType(file, d.right);
            },
            .satisfies_expression => {
                const d = getPackedData(node);

                return this.getType(file, d.left);
            },
            .parenthesized_type => {
                return this.getType(file, unwrapRef(node));
            },
            .import_type => {
                // Binder should attach relevant symbol to rhs
                return error.TODO;
            },
            .constructor_type, .function_type => {
                const d = getPackedData(node);
                var params = std.ArrayList(TypeRef).init(this.type_args.allocator);
                var iter = NodeIterator.init(&file.ast.nodes, d.left);
                var flags: u24 = 0;

                if (node.kind == .constructor_type) {
                    flags |= @intFromEnum(Flags.constructor);
                    if (node.hasFlag(.abstract)) flags |= @intFromEnum(Flags.abstract);
                }

                var this_type: TypeRef = 0;

                while (iter.nextPair()) |p| {
                    const d2 = getPackedData(p[0]);
                    const z = try this.getType(file, p[0].len);
                    const is_parameterized = this.isParameterizedRef(z);

                    // Only possible for `function_type`
                    if (file.ast.nodes.at(d2.left).kind == .this_keyword) {
                        if (is_parameterized) flags |= @intFromEnum(Flags.parameterized);
                        this_type = z;
                        continue;
                    }

                    var param_flags: u24 = 0;
                    if (p[0].hasFlag(.generator)) param_flags |= @intFromEnum(Flags.spread);
                    if (p[0].hasFlag(.optional)) param_flags |= @intFromEnum(Flags.optional);

                    if (is_parameterized) {
                        flags |= @intFromEnum(Flags.parameterized);
                        param_flags |= @intFromEnum(Flags.parameterized);
                    }

                    try params.append(
                        try this.createNamedTupleTypeFromIdent(file.id, d2.left, z, param_flags),
                    );
                }

                const return_type = try this.getType(file, d.right);
                if (this.isParameterizedRef(return_type)) flags |= @intFromEnum(Flags.parameterized);

                const inner = try this.createFunctionLiteralFull(params.items, return_type, this_type, flags);
                if (node.len == 0) {
                    return inner;
                }

                const type_params = try this.getTypeParams(file, node.len);

                return this.createParameterizedType(type_params, inner);
            },
            .expression_with_type_arguments => {
                const d = getPackedData(node);
                const lhs = file.ast.nodes.at(d.left);
                if (lhs.kind == .call_expression) {
                    return this.getType(file, d.left);
                }
                if (lhs.kind != .identifier) return error.TODO_exp_with_type_args;

                const sym_ref = lhs.extra_data;
                if (d.right == 0) {
                    return this.createAliasNoArgs(file.id, sym_ref, 0);
                }

                var args = try this.getTypeArgs(file, d.right);

                return this.createCanonicalAlias(&args, file.id, sym_ref, 0);
            },
            .type_reference => {
                const d = getPackedData(node);

                const resolved = try this.resolveTypeFromSymbol(file, d.left, d.right);
                if (resolved.is_alias) return resolved.ty;

                const left = resolved.ty;

                if (left >= @intFromEnum(Kind.false)) return left;

                const ty = this.types.at(left);
                if (d.right == 0) {
                    // TODO: assign default values for parameterized types
                    if (ty.getKind() == .parameterized) {
                        this.printTypeInfo(left);
                        this.printCurrentNode();
                        return error.TODO;
                    }

                    if (ty.getKind() == .class) return ty.slot0;

                    return left;
                }

                if (ty.getKind() != .parameterized) {
                    return error.NotParameterized;
                }

                return this.resolveWithTypeArgs(file, ty, d.right);
            },
            .type_alias_declaration => {
                const sym_ref = file.binder.getTypeSymbol(ref) orelse return error.MissingSymbol;

                const d = getPackedData(node);

                if (file.ast.nodes.at(node.len).kind == .intrinsic_keyword) {
                    const name = getSlice(file.ast.nodes.at(d.left), u8);
                    const k = IntrinsicTypeAliases.get(name) orelse return error.MissingIntrinsic;

                    if (d.right == 0) return @intFromEnum(k);

                    const params = try this.getTypeParams(file, d.right);
                    const t = try this.createParameterizedType(params, @intFromEnum(k));

                    return t;
                }

                const key: u64 = (@as(u64, file.id) << 32) | sym_ref;

                const should_add = !(this.active_types.get(key) orelse false);

                if (should_add) try this.active_types.put(this.allocator(), key, true);
                defer {
                    if (should_add and !this.active_types.swapRemove(key)) @panic("failed to remove key");
                }

                const inner = try this.getType(file, node.len);
                if (d.right == 0) {
                    return inner;
                }

                // if (inner != 0 and inner < @intFromEnum(Kind.false)) {
                //     this.types.at(inner).flags |= @intFromEnum(Flags.parameterized);
                // }

                return this.createParameterizedTypeFromParams(file, d.right, inner);
            },
            .intersection_type => {
                const d = getPackedData(node);

                const rhs = try this.getType(file, d.right);

                if (d.left == 0) return rhs;
                const lhs = try this.getType(file, d.left);

                if (rhs == lhs) return lhs;

                if (rhs == @intFromEnum(Kind.empty_object)) return this.nonNullable(lhs);
                if (lhs == @intFromEnum(Kind.empty_object)) return this.nonNullable(rhs);

                return this.intersectType(lhs, rhs);
            },
            .union_type => {
                const d = getPackedData(node);

                if (d.left == 0) {
                    return this.getType(file, d.right);
                }

                if (d.right == 0) {
                    return this.getType(file, d.left);
                }

                var tmp = TempUnion.init(this.allocator());

                if (try this.addToUnion(&tmp, try this.getType(file, d.right))) |x| {
                    return x;
                }

                var left_ref = d.left;
                while (left_ref != 0) {
                    const left = file.ast.nodes.at(left_ref);
                    if (left.kind != .union_type) {
                        if (try this.addToUnion(&tmp, try this.getType(file, left_ref))) |x| {
                            return x;
                        }
                        break;
                    }

                    if (try this.addToUnion(&tmp, try this.getType(file, getPackedData(left).right))) |x| {
                        return x;
                    }

                    left_ref = getPackedData(left).left;
                }

                return tmp.complete(this);
            },
            .literal_type => {
                const old_state = this.is_const_context;
                defer this.is_const_context = old_state;
                this.is_const_context = true;

                return this.getType(file, unwrapRef(node));
            },
            .template_literal_type => {
                var elements = std.ArrayList(TypeRef).init(this.allocator());
                const head = file.ast.nodes.at(unwrapRef(node));
                try elements.append(try this.createStringLiteral(file.id, unwrapRef(node), getSlice(head, u8), @intFromEnum(StringFlags.single_quote)));

                var flags: u24 = 0;
                var next: NodeRef = head.next;
                while (next != 0) {
                    const span = file.ast.nodes.at(next);
                    const d = getPackedData(span);
                    const exp = try this.getType(file, d.left);
                    if (this.isParameterizedRef(exp)) flags |= @intFromEnum(Flags.parameterized);
                    try elements.append(exp);
                    try elements.append(try this.createStringLiteral(file.id, d.right, getSlice(file.ast.nodes.at(d.right), u8), @intFromEnum(StringFlags.single_quote)));

                    next = span.next;
                }

                const x: u64 = @intFromPtr(elements.items.ptr);
                return try this.types.push(.{
                    .kind = @intFromEnum(Kind.template_literal),
                    .flags = flags,
                    .slot0 = @truncate(x),
                    .slot1 = @intCast(x >> 32),
                    .slot2 = @intCast(elements.items.len),
                });
            },
            .no_substitution_template_literal, .string_literal => {
                if (!this.is_const_context and !this.is_const_variable_context) return @intFromEnum(Kind.string);

                const s = getSlice(node, u8);
                return this.createStringLiteral(file.id, ref, s, @intFromEnum(StringFlags.single_quote));
            },
            .numeric_literal => return this.getNumericLiteralType(node, false),
            .variable_declaration => {
                if (node.len != 0) {
                    return this.getType(file, node.len);
                }

                const d = getPackedData(node);
                if (d.right == 0) {
                    return @intFromEnum(Kind.any);
                }

                const old_context = this.is_const_variable_context;
                this.is_const_variable_context = if (file.binder.getSymbol(d.left)) |s|
                    hasSymbolFlag(file.binder.symbols.at(s), .@"const")
                else
                    false;

                defer this.is_const_variable_context = old_context;

                return this.getType(file, d.right);
            },
            .class_declaration, .class_expression => {
                const inner = try this.getClassType(file, ref);
                if (node.extra_data == 0) return inner;

                const type_params = try this.getTypeParams(file, node.extra_data);
                return this.createParameterizedType(type_params, inner);
            },
            .function_declaration, .function_expression, .arrow_function => return this.getSignature(file, ref),
            .conditional_type => return this.createConditionalType(file, ref),
            else => return this.inferType(file, ref),
        }

        return notSupported(node.kind);
    }

    fn tryGetTypeFromNamespace(this: *@This(), abs_ref: AbsoluteSymbolRef, hash: u32) !?TypeRef {
        const f = this.program.getFileData(abs_ref.file_id);
        const ns_sym = f.binder.symbols.at(abs_ref.ref);
        const ns = f.binder.namespaces.items[ns_sym.binding];
        const sym_ref = ns.type_symbols.get(hash) orelse return null;

        return try this.getTypeOfSymbol(f, sym_ref);
    }

    fn resolveTypeFromQualifiedName(this: *@This(), file: *ParsedFileData, node: *const AstNode) !TypeRef {
        const d = getPackedData(node);
        const subject = file.ast.nodes.at(d.left);
        if (subject.kind != .identifier) return notSupported(subject.kind);
        if (subject.extra_data == 0) return error.MissingSymbol;

        const ns_sym = file.binder.symbols.at(subject.extra_data);
        if (hasSymbolFlag(ns_sym, .late_bound)) {
            const hash = getHashFromNode(file.ast.nodes.at(d.right));
            const g = this.program.ambient.globals_allocator.at(ns_sym.declaration);
            if (g.symbols.items.len > 1) {
                for (g.symbols.items) |item| {
                    const ty = try this.tryGetTypeFromNamespace(item, hash) orelse continue;
                    return ty;
                }

                this.printCurrentNode();
                return error.SymbolNotInMergedNamespace;
            }

            return try this.tryGetTypeFromNamespace(g.symbols.items[0], hash) orelse return error.SymbolNotInNamespace;
        }

        if (hasSymbolFlag(ns_sym, .imported)) {
            const f = this.program.getFileData(getOrdinal(ns_sym));
            if (hasSymbolFlag(ns_sym, .namespace)) {
                // star export
                const sym_ref = f.binder.exports.type_symbols.get(getHashFromNode(file.ast.nodes.at(d.right))) orelse return error.SymbolNotInNamespace;

                return this.getTypeOfSymbol(f, sym_ref);
            }

            const ns_sym2 = f.binder.symbols.at(ns_sym.declaration);
            const ns2 = f.binder.namespaces.items[ns_sym2.binding];
            const sym_ref = ns2.type_symbols.get(getHashFromNode(file.ast.nodes.at(d.right))) orelse return error.SymbolNotInNamespace;

            return this.getTypeOfSymbol(f, sym_ref);
        }

        // TODO: namespaces can be merged w/ other declarations so we need to search the chain during binding
        if (!hasSymbolFlag(ns_sym, .namespace)) return error.NotANamespace;

        const ns = file.binder.namespaces.items[ns_sym.binding];
        const sym_ref = ns.type_symbols.get(getHashFromNode(file.ast.nodes.at(d.right))) orelse return error.SymbolNotInNamespace;

        return this.getTypeOfSymbol(file, sym_ref);
    }

    fn resolveTypeFromSymbol(this: *@This(), file: *ParsedFileData, ref: NodeRef, args_start: NodeRef) !struct { is_alias: bool = false, ty: TypeRef } {
        const node = file.ast.nodes.at(ref);
        // perf: we can skip this check in some cases
        if (node.kind == .qualified_name) {
            return .{ .ty = try this.resolveTypeFromQualifiedName(file, node) };
        }

        const s = file.binder.getTypeSymbol(ref) orelse return error.MissingSymbol;
        const sym = file.binder.symbols.at(s);

        if (sym.hasFlag(.type_parameter)) {
            return .{ .ty = @intFromEnum(Kind.type_parameter_ref) | getOrdinal(sym) };
        }

        if (sym.hasFlag(.late_bound) and !sym.hasFlag(.imported)) {
            const g_ref = this.program.ambient.global_types.get(getHashFromNode(node)) orelse {
                this.printCurrentNode();
                return error.MissingGlobalTypeSymbol;
            };

            var ty: TypeRef = undefined;

            if (args_start == 0) {
                ty = try this.createCanonicalAlias(@constCast(&TypeList{}), std.math.maxInt(u32), g_ref, @intFromEnum(Flags.global));
            }  else {
                var args = TypeList{};
                var iter = NodeIterator.init(&file.ast.nodes, args_start);
                while (iter.nextRef()) |r| {
                    const t = try this.getType(file, r);
                    try args.append(this, t);
                }

                ty = try this.createCanonicalAlias(@constCast(&args), std.math.maxInt(u32), g_ref, @intFromEnum(Flags.global));
            }

            return .{
                .is_alias = true,
                .ty = ty,
            };
        }

        var flags: u24 = 0;
        if (sym.declaration != 0 and !sym.hasFlag(.late_bound)) {
            const decl = file.ast.nodes.at(sym.declaration);
            if (decl.kind == .class_declaration or decl.kind == .class_expression) {
                flags |= @intFromEnum(Flags.instance_alias);
            }
        }

        if (args_start == 0) {
            return .{
                .is_alias = true,
                .ty = try this.createAliasNoArgs(file.id, s, flags),
            };
        }

        var args = TypeList{};
        var iter = NodeIterator.init(&file.ast.nodes, args_start);
        while (iter.nextRef()) |r| {
            const t = try this.getType(file, r);
            try args.append(this, t);
        }

        return .{
            .is_alias = true,
            .ty = try this.createCanonicalAlias(&args, file.id, s, flags),
        };
    }

    const max_inline_number = std.math.pow(i64, 2, 29);

    fn createNumericLiteral(this: *@This(), val: u64) !TypeRef {
        return this.types.push(.{
            .kind = @intFromEnum(Kind.number_literal),
            .slot0 = @intCast(val >> 32),
            .slot1 = @truncate(val),
        });
    }

    fn numberToType(this: *@This(), comptime NumberType: type, val: NumberType) anyerror!TypeRef {
        if (NumberType == i30) {
            return @intFromEnum(Kind.zero) | @as(u30, @bitCast(val));
        }
        if (NumberType == i32) {
            if (val >= -max_inline_number and val < max_inline_number) {
                const v: i30 = @intCast(val);
                return @intFromEnum(Kind.zero) | @as(u30, @bitCast(v));
            }
            return this.numberToType(f64, @floatFromInt(val));
        }
        if (NumberType == f64) {
            const val2: i64 = @intFromFloat(val);
            if (@as(i64, @intFromFloat(@trunc(val))) != val2) {
                return this.createNumericLiteral(@bitCast(val));
            }
            if (val2 >= -max_inline_number and val2 < max_inline_number) {
                const v: i30 = @intCast(val2);
                return @intFromEnum(Kind.zero) | @as(u30, @bitCast(v));
            }
            return this.createNumericLiteral(@bitCast(val));
        }
        if (NumberType == u32) {
            return this.numberToType(i32, @intCast(val));
        }

        @compileError("TODO");
    }

    fn getNumericLiteralType(this: *@This(), node: *const AstNode, comptime is_negative: bool) !TypeRef {
        if (!this.is_const_context and !this.is_const_variable_context) return @intFromEnum(Kind.number);

        const v = getNumber(node);

        return this.numberToType(f64, if (comptime is_negative) -v else v);

        // const val = getNumber(node);
        // const val2: i64 = @intFromFloat(val);
        // if (@as(i64, @intFromFloat(@trunc(val))) != val2) return error.TODO;

        // if (@abs(val2) < std.math.pow(u64, 2, 29)) {
        //     const v: i30 = @intCast(if (is_negative) -val2 else val2);
        //     return @intFromEnum(Kind.zero) | @as(u30, @bitCast(v));
        // }

        // // The least-significant set bit in the mantissa determines how large the exponent must be to be a valid integer
        // const bits: u64 = @bitCast(val);
        // const exp = (bits & (0b11111111111 << 52)) >> 52;
        // if (exp < 0) {
        //     return 0; // less than 0
        // }
        // if ((bits & (0b11111111111 << 52)) >> 52 == 1023) {
        //     // TODO: sign bit
        //     const z: u64 = @bitCast(bits & 0xFFFF FFFF FFFF F);
        //     if (z < std.math.pow(u64, 2, 26)) {
        //         std.debug.print("{}\n", .{z});
        //         return @intCast(@intFromEnum(Kind.zero) + z);
        //     }
        // }
    }

    fn getNumberFromInlineType(ref: TypeRef) i30 {
        std.debug.assert(ref >= @intFromEnum(Kind.zero));

        return @as(i30, @bitCast(@as(u30, @intCast(ref & ~@as(u32, @intFromEnum(Kind.zero))))));
    }

    fn getInt32FromType(this: *const @This(), ref: TypeRef) i32 {
        if (ref >= @intFromEnum(Kind.zero)) {
            return @intCast(getNumberFromInlineType(ref));
        }

        const t = this.types.at(ref);
        const val: f64 = @bitCast((@as(u64, t.slot0) << 32) | t.slot1);
        return @intFromFloat(val);
    }

    pub fn getDoubleFromType(this: *const @This(), ref: TypeRef) f64 {
        if (ref >= @intFromEnum(Kind.zero)) {
            return @floatFromInt(getNumberFromInlineType(ref));
        }

        const t = this.types.at(ref);
        return @bitCast((@as(u64, t.slot0) << 32) | t.slot1);
    }

    fn getReturnType(this: *@This(), ref: TypeRef) !u32 {
        if (ref >= @intFromEnum(Kind.false)) {
            return error.NotAFunction;
        }

        const n = this.types.at(ref);
        if (n.getKind() != .function_literal) {
            std.debug.print("{any}\n", .{n.kind});
            return error.NotAFunction;
        }

        return n.slot3;
    }

    fn getHashComptime(comptime data: []const u8) u32 {
        return @intCast(std.hash.Wyhash.hash(0, data));
    }

    const name_hash = getHashComptime("name");
    const length_hash = getHashComptime("length");

    fn maybeAccessModule(this: *@This(), subject: TypeRef, element: TypeRef, comptime is_type: bool) !?TypeRef {
        if (subject == @intFromEnum(Kind.any)) return subject;
        if (element == @intFromEnum(Kind.any)) return element;

        if (subject >= @intFromEnum(Kind.false)) return null;
        const s = this.types.at(subject);
        if (s.getKind() != .module_namespace) return null;

        const hash = try this.getMemberNameHash(element);

        if (s.slot5 == 1) { // XXX: ambient module
            const f = this.program.getFileData(s.slot4);
            const ns = f.binder.namespaces.items[s.slot3];
            const m = if (comptime is_type) ns.type_symbols else ns.symbols;
            const sym_ref = m.get(hash) orelse return null;

            return try this.getTypeOfSymbol(f, sym_ref);
        }

        const f = this.program.getFileData(s.slot0);
        const m = if (comptime is_type) f.binder.exports.type_symbols else f.binder.exports.symbols;
        const sym_ref = m.get(hash) orelse return null;

        return try this.getTypeOfSymbol(f, sym_ref);
    }

    fn accessType(this: *@This(), subject: TypeRef, element: TypeRef) anyerror!TypeRef {
        if (subject == @intFromEnum(Kind.any)) return subject;
        if (element == @intFromEnum(Kind.any)) return element;
        if (element == @intFromEnum(Kind.never)) return element;

        if (element < @intFromEnum(Kind.false)) {
            const t = this.types.at(element);
            switch (t.getKind()) {
                .@"union" => {
                    var tmp = TempUnion.init(this.allocator());
                    for (getSlice2(t, TypeRef)) |el| {
                        const u = try this.accessType(subject, el);
                        if (try this.addToUnion(&tmp, u)) |x| {
                            return x;
                        }
                    }
                    return tmp.complete(this);
                },
                .alias => {
                    const r = try this.maybeResolveAlias(element);
                    if (r == element) return error.TODO_failed_resolve_alias;
                    return this.accessType(subject, r);
                },
                .keyof => {
                    const r = try this.evaluateType(element, @intFromEnum(EvaluationFlags.all));
                    if (r == element) return error.TODO_failed_eval_keyof;
                    return this.accessType(subject, r);
                },
                else => {},
            }
        }
        const hash = try this.getMemberNameHash(element);

        return try this.accessTypeWithHash(subject, element, hash, true) orelse @intFromEnum(Kind.undefined);
    }

    fn accessTypeWithHash(this: *@This(), subject: TypeRef, element: TypeRef, hash: NodeSymbolHash, set_this_type: bool) anyerror!?TypeRef {
        if (subject >= @intFromEnum(Kind.false)) {
            if (subject == @intFromEnum(Kind.never)) return subject;
            if (subject == @intFromEnum(Kind.empty_object)) return null;
            if (subject == @intFromEnum(Kind.this)) {
                if (this.contextual_this_type == 0 or this.contextual_this_type == @intFromEnum(Kind.this)) {
                    return error.InvalidThisType;
                }
                return this.accessTypeWithHash(this.contextual_this_type, element, hash, false);
            }

            // we are accessing the primitive protos
            if (subject == @intFromEnum(Kind.string)) {
                const s = try this.getGlobalTypeSymbolAlias("String", .{}) orelse return error.MissingString;
                return this.accessTypeWithHash(s, element, hash, set_this_type);
            }

            this.printTypeInfo(subject);
            this.printTypeInfo(element);
            this.printCurrentNode();
            return error.TODO;
        }

        const s = this.types.at(subject);
        if (s.getKind() == .alias) {
            const r = try this.maybeResolveAlias(subject);
            if (r == subject) {
                this.printTypeInfo(r);
                return error.FailedToResolveAlias;
            }

            return this.accessTypeWithHash(r, element, hash, set_this_type);
        }
        if (s.getKind() == .query) {
            return this.accessTypeWithHash(try this.evaluateQuery(s, 0), element, hash, set_this_type);
        }
        if (s.getKind() == .array) {
            // FIXME: we need to check the element type and see that it's a number
            return s.slot0;
        }
        if (s.getKind() == .tuple) {
            const slice = getSlice2(s, TypeRef);
            if (element >= @intFromEnum(Kind.zero)) {
                const ind = getNumberFromInlineType(element);
                if (ind < 0) return @intFromEnum(Kind.never);
                if (ind >= slice.len) return @intFromEnum(Kind.never);

                const el = slice[@intCast(ind)];
                if (el >= @intFromEnum(Kind.false)) return el;
                if (this.getKindOfRef(el) == .named_tuple_element) {
                    return this.types.at(el).slot1;
                }
                return el;
            }
            return error.TODO_index_into_tuple;
        }
        if (s.getKind() == .intersection) {
            var tmp = TypeList{};
            outer: for (getSlice2(s, TypeRef)) |t| {
                if (try this.accessTypeWithHash(t, element, hash, set_this_type)) |t2| {
                    if (t2 == @intFromEnum(Kind.any) or t2 == @intFromEnum(Kind.never)) {
                        tmp.deinit(this.allocator());
                        return t2;
                    }
                    if (tmp.getCount() <= 4) {
                        const slice = tmp.getSlice();
                        for (slice) |ot| {
                            if (ot == t2) continue :outer;
                        }

                        if (slice.len == 1 and slice[0] >= @intFromEnum(Kind.false)) {
                            if (t2 >= @intFromEnum(Kind.false)) {
                                if (try this.isAssignableTo(t2, slice[0])) {
                                    // noop
                                } else if (try this.isAssignableTo(slice[0], t2)) {
                                    _ = tmp.replace(0, t2);
                                } else {
                                    tmp.deinit(this.allocator());
                                    return @intFromEnum(Kind.never);
                                }
                                continue :outer;
                            } else {
                                if (this.getKindOfRef(t2) == .@"union") {
                                    if (try this.maybeIntersectUnion(t2, slice[0])) |t3| {
                                        if (t3 == @intFromEnum(Kind.any) or t3 == @intFromEnum(Kind.never)) {
                                            tmp.deinit(this.allocator());
                                            return @intFromEnum(Kind.never);
                                        }
                                        _ = tmp.replace(0, t3);
                                        continue :outer;
                                    }
                                }
                            }
                        }
                    }
                    try tmp.append(this, t2);
                }
            }
            if (tmp.getCount() == 0) return null; // same as the prop not existing on a literal
            
            return try this.simplifyIntersectionType(&tmp);
        }
        if (s.getKind() == .@"union") {
            var tmp = TempUnion.init(this.allocator());
            for (getSlice2(s, TypeRef)) |t| {
                if (try this.accessTypeWithHash(t, element, hash, set_this_type)) |t2| {
                    if (try this.addToUnion(&tmp, t2)) |x| return x;
                }
            }

            return try tmp.complete(this);
        }

        if (s.getKind() != .object_literal) return try notSupported(s.getKind());

        const old_this_type = this.contextual_this_type;
        defer this.contextual_this_type = old_this_type;
        if (set_this_type) this.contextual_this_type = subject;

        for (getSlice2(s, ObjectLiteralMember)) |*el| {
            if (el.kind == .call_signature) continue;

            if (el.kind == .index) {
                if (try this.isAssignableTo(element, el.name)) {
                    return try el.getType(this);
                }
                continue;
            }

            const h = try this.getMemberNameHash(el.name);
            if (h == hash) {
                if (el.hasFlag(.optional)) {
                    return try this.toUnion(&.{
                        try el.getType(this),
                        @intFromEnum(Kind.undefined),
                    });
                }
                return try el.getType(this);
            }
        }

        if (s.slot3 != 0 and s.slot3 != @intFromEnum(Kind.null)) {
            return this.accessTypeWithHash(s.slot3, element, hash, false);
        }

        return null;
    }

    fn maybeIntersectUnion(this: *@This(), union_ref: TypeRef, ref: TypeRef) !?TypeRef {
        var tmp = TempUnion.init(this.allocator());
        for (getSlice2(this.types.at(union_ref), TypeRef)) |t| {
            const t2 = blk: {
                if (try this.isAssignableTo(t, ref)) {
                    break :blk t;
                } else if (try this.isAssignableTo(ref, t)) {
                    break :blk ref;
                } else {
                    if (t < @intFromEnum(Kind.false)) {
                        // bailout
                        tmp.deinit();
                        return null;
                    }

                    continue;
                }
            };
    
            if (try this.addToUnion(&tmp, t2)) |y| {
                return y;
            }
        }
        return try tmp.complete(this);
    }

    fn getInstanceType(this: *@This(), type_ref: TypeRef, comptime follow_instance_alias: bool) anyerror!TypeRef {
        if (type_ref >= @intFromEnum(Kind.false)) return error.CannotInstantiate;

        const t = this.types.at(type_ref);

        switch (t.getKind()) {
            .alias => {
                if (!comptime follow_instance_alias) {
                    if (t.hasFlag(.class_alias)) {
                        var flags: u24 = @intFromEnum(Flags.instance_alias);
                        if (t.hasFlag(.global)) {
                            flags |= @intFromEnum(Flags.global);
                        }
                        return this.createAliasNoArgs(t.slot3, t.slot4, flags);
                    }
                }
                return this.getInstanceType(try this.maybeResolveAlias(type_ref), follow_instance_alias);
            },
            .class => {
                if (!comptime follow_instance_alias) {
                    if (t.slot6 != 0) {
                        return t.slot6;
                    }
                }
                return t.slot0;
            },
            .object_literal => {
                // TODO: multiple construct signatures
                for (getSlice2(t, ObjectLiteralMember)) |*m| {
                    if (m.kind == .call_signature) {
                        const f = this.types.at(try m.getType(this));
                        if (hasTypeFlag(f, .constructor)) {
                            return f.slot3;
                        }
                    }
                }

                return error.TODO_missing_construct_signature;
            },
            else => {},
        }

        std.debug.print("{any}\n", .{t.getKind()});
        return error.TODO_get_instance_type;
    }

    pub fn getKnownSymbolType(this: *@This(), comptime name: []const u8) !?TypeRef {
        const g = this.program.ambient.getGlobalTypeSymbolRef("SymbolConstructor") orelse return null;
        const gt = try this.getTypeOfGlobalSymbol(g);
        const et = try this.createSyntheticStringLiteral(name, false);

        return try this.accessType(gt, et);
    }

    fn getGlobalTypeSymbolAlias(this: *@This(), comptime name: []const u8, args: TypeList) !?TypeRef {
        const g = this.program.ambient.getGlobalTypeSymbol(name) orelse return null;
        if (g.symbols.items.len == 1) {
            const abs_ref = g.symbols.items[0];
            if (args.count == 0) {
                return try this.createAliasNoArgs(abs_ref.file_id, abs_ref.ref, 0);
            }

            return try this.createCanonicalAlias(@constCast(&args), abs_ref.file_id, abs_ref.ref, 0);
        }

        const g_ref = this.program.ambient.getGlobalTypeSymbolRef(name) orelse return null;

        if (args.count > 0) {
            return try this.createCanonicalAlias(@constCast(&args), std.math.maxInt(u32), g_ref, @intFromEnum(Flags.global));
        } 

        return try this.getTypeOfGlobalSymbol(g_ref);
    }

    fn getRegExpType(this: *@This(), file: *ParsedFileData, ref: NodeRef) !TypeRef {
        _ = file;
        _ = ref;

        return try this.getGlobalTypeSymbolAlias("RegExp", .{}) orelse @intFromEnum(Kind.empty_object);
    }

    fn maybeGetInt32FromType(this: *@This(), ref: TypeRef) ?i32 {
        if (ref >= @intFromEnum(Kind.false)) {
            if (ref >= @intFromEnum(Kind.zero)) {
                return @intCast(getNumberFromInlineType(ref));
            }
            return null;
        }
        const t = this.types.at(ref);
        if (t.getKind() == .number_literal) {
            return this.getInt32FromType(ref);
        }
        return null;
    }

    fn castShiftAmount(v: i32) u5 {
        if (v >= 0) {
            return @intCast(v & 0x1F);
        }
        return @intCast(@as(u32, @bitCast(v)) & 0x1F);
    }

    fn doInt32Op(this: *@This(), file: *ParsedFileData, exp: *const AstNode) !TypeRef {
        const d = getPackedData(exp);
        const _lhs = try this.getType(file, d.left);
        if (_lhs == @intFromEnum(Kind.number)) {
            return @intFromEnum(Kind.number);
        }
        const lhs = this.maybeGetInt32FromType(_lhs) orelse return error.NotIn32;

        const _rhs = try this.getType(file, d.right);
        if (_rhs == @intFromEnum(Kind.number)) {
            return @intFromEnum(Kind.number);
        }
        const rhs = this.maybeGetInt32FromType(_rhs) orelse return error.NotIn32;

        const op: SyntaxKind = @enumFromInt(exp.len);
        const v: i32 = switch (op) {
            .bar_token => lhs | rhs,
            .caret_token => lhs ^ rhs,
            .ampersand_token => lhs & rhs,
            .less_than_less_than_token => lhs << castShiftAmount(rhs),
            .greater_than_greater_than_token => lhs >> castShiftAmount(rhs),
            // TODO: `(1 << 31) >>> 0` should equal 2^31
            .greater_than_greater_than_greater_than_token => @bitCast(@as(u32, @bitCast(lhs)) >> castShiftAmount(rhs)), // >>>
            else => return notSupported(op),
        };

        return this.numberToType(i32, v);
    }

    fn inferTypeBinaryExp(this: *@This(), file: *ParsedFileData, exp: *const AstNode) !TypeRef {
        const op: SyntaxKind = @enumFromInt(exp.len);
        const d = getPackedData(exp);
        switch (op) {
            .plus_token => {
                const lhs = try this.getType(file, d.left);
                if (lhs == @intFromEnum(Kind.string)) {
                    return @intFromEnum(Kind.string);
                }
                const rhs = try this.getType(file, d.right);
                if (rhs == @intFromEnum(Kind.string)) {
                    return @intFromEnum(Kind.string);
                }

                if (lhs == @intFromEnum(Kind.number) or rhs == @intFromEnum(Kind.number)) return @intFromEnum(Kind.number);
                if (!this.is_const_context and !this.is_const_variable_context) return @intFromEnum(Kind.number);

                if (lhs >= @intFromEnum(Kind.zero) and rhs >= @intFromEnum(Kind.zero)) {
                    const l = getNumberFromInlineType(lhs);
                    const r = getNumberFromInlineType(rhs);

                    return @as(u32, @intFromEnum(Kind.zero)) | @as(u30, @bitCast(l + r));
                }

                return @intFromEnum(Kind.number);
            },
            .minus_token => {
                // FIXME: should be semantic error if either type isn't a number
                return @intFromEnum(Kind.number);
            },
            .bar_token, .caret_token, .ampersand_token, .less_than_less_than_token, .greater_than_greater_than_token, .greater_than_greater_than_greater_than_token => {
                if (!this.is_const_context) return @intFromEnum(Kind.number);

                return this.doInt32Op(file, exp);
            },
            .bar_bar_token => {
                const lhs = try this.getType(file, d.left);
                if (try this.isTruthyType(lhs)) {
                    return lhs;
                }
                const rhs = try this.getType(file, d.right);
                if (lhs == rhs) {
                    return lhs;
                }

                return this.toUnion(&.{ lhs, rhs });
            },
            // .ampersand_ampersand_token => {

            // },
            // assignments
            .equals_token => {
                return this.getType(file, d.right);
            },
            .question_question_equals_token => {
                // FIXME: we can only apply `nonNullable` if rhs is also non-nullable
                const lhs = try this.getType(file, d.left);
                return try this.nonNullable(lhs);
            },
            .question_question_token => {
                // FIXME: it's better to check if lhs is nullish first
                const lhs = try this.getType(file, d.left);
                const x = try this.nonNullable(lhs);
                return try this.toUnion(&.{ x, try this.getType(file, d.right) });
            },
            else => {
                std.debug.print("missing op implementation: {any}\n", .{op});
                this.printCurrentNode();
                return error.TODO;
            }
        }
    }

    fn inferTypeCallExpParameterized(this: *@This(), file: *ParsedFileData, n: *const Type, param_start: NodeRef) !?TypeRef {
        var args = std.ArrayListUnmanaged(TypeRef){};
        defer args.deinit(this.allocator());

        const fn_params = getSlice2(this.types.at(n.slot3), TypeRef);

        try args.ensureTotalCapacity(this.allocator(), fn_params.len);

        this.is_const_variable_context = false;

        var iter = NodeIterator.init(&file.ast.nodes, param_start);
        while (iter.nextPair()) |p| {
            if (args.items.len >= fn_params.len) break; // user provided too many args, TODO: emit diag

            // TODO: apply `const` here from type params for inferrence
            switch (p[0].kind) {
                .numeric_literal, .string_literal, .template_expression => {
                    this.is_const_context = true;
                },
                else => {
                    this.is_const_context = false;
                },
            }

            const old_ctx = this.inferrence_ctx;
            defer this.inferrence_ctx = old_ctx;
            const pt = fn_params[args.items.len];
            std.debug.assert(this.getKindOfRef(pt) == .named_tuple_element);
            this.inferrence_ctx = this.types.at(pt).slot1;

            const ty = try this.getType(file, p[1]);
            try args.append(this.allocator(), ty);
        }

        // No type args, infer them
        const type_params = getSlice2(n, TypeRef);

        // Type args can be inferred both from the arguments and the expected return type
        const prev_inferred = this.inferred_type_params;
        defer this.inferred_type_params = prev_inferred;

        var should_infer = std.AutoArrayHashMapUnmanaged(u32, TypeRef){};
        defer should_infer.deinit(this.allocator());

        try should_infer.ensureTotalCapacity(this.allocator(), type_params.len);
        for (type_params) |p| should_infer.putAssumeCapacity(this.types.at(p).slot3, p);

        this.inferred_type_params = should_infer;

        var inferred = std.AutoArrayHashMap(TypeRef, TypeRef).init(this.type_args.allocator);
        defer inferred.deinit();

        const did_infer = try this.inferTupleLikeTypes(args.items, fn_params, &inferred, .covariant) orelse return null;
        if (!did_infer) return null;

        const resolved = try this.resolveWithTypeArgsInferred(n, &inferred);

        return resolved;
    }

    pub fn inferType(this: *@This(), file: *ParsedFileData, ref: NodeRef) anyerror!TypeRef {
        const exp = file.ast.nodes.at(ref);

        switch (exp.kind) {
            .parenthesized_expression => {
                const inner = unwrapRef(exp);
                return try this.getType(file, inner);
            },
            .await_expression => {
                const inner = unwrapRef(exp);
                const inner_ty = try this.getType(file, inner);

                return try this.maybeUnwrapPromise(inner_ty) orelse inner_ty;
            },
            .prefix_unary_expression => {
                const d = getPackedData(exp);
                return switch (@as(SyntaxKind, @enumFromInt(d.left))) {
                    // TODO: return boolean type unless the expression is exactly true or false, in which case invert it
                    .exclamation_token => @intFromEnum(Kind.boolean),
                    .plus_token => @intFromEnum(Kind.number),
                    .minus_token => blk: {
                        if (!this.is_const_context) break :blk @intFromEnum(Kind.number);

                        const r = file.ast.nodes.at(d.right);
                        if (r.kind == .numeric_literal) break :blk try this.getNumericLiteralType(r, true);

                        break :blk @intFromEnum(Kind.number); // This is the current typescript behavior apparently
                    },
                    else => notSupported(@as(SyntaxKind, @enumFromInt(d.left))),
                };
            },
            .element_access_expression => {
                const d = getPackedData(exp);
                const l = try this.getType(file, d.left);

                const r = blk: {
                    const old_state = this.is_const_context;
                    defer this.is_const_context = old_state;
                    this.is_const_context = true;
                    break :blk try this.getType(file, d.right);
                };

                if (try this.maybeAccessModule(l, r, false)) |t| {
                    return t;
                }

                return this.accessType(l, r);
            },
            .property_access_expression => {
                const d = getPackedData(exp);
                const l = try this.getType(file, d.left);
                const r = try this.propertyNameToType(file, d.right);

                if (try this.maybeAccessModule(l, r, false)) |t| {
                    return t;
                }

                // FIXME: we only need to evaluate `this` when `this` escapes
                var t = try this.accessType(l, r);
                while (this.hasThisType(t)) {
                    const old_this_type = this.contextual_this_type;
                    defer this.contextual_this_type = old_this_type;
                    this.contextual_this_type = l;

                    t = try this.evaluateType(t, @intFromEnum(EvaluationFlags.all));
                }

                return t;
            },
            .binary_expression => {
                return this.inferTypeBinaryExp(file, exp);
            },
            .new_expression => {
                const d = getPackedData(exp);
                const type_ref = try this.getType(file, d.left);

                // TODO: infer parameterized class type

                return this.getInstanceType(type_ref, false);
            },
            .call_expression => {
                const d = getPackedData(exp);
                const t = try this.getType(file, d.left);
                if (t >= @intFromEnum(Kind.false)) {
                    if (t == @intFromEnum(Kind.never)) {
                        return t;
                    }
                    this.printTypeInfo(t);
                    this.printCurrentNode();
                    return error.NotAFunctionType;
                }

                //const is_optional = hasFlag(exp, .optional);
                //const is_non_nullable = hasFlag(exp, .non_null);

                // Optional chain should only do something if the expression type is a union w/ `undefined`/`null`/`void`

                const n = this.types.at(t);
                if (n.getKind() == .parameterized) {
                    if (n.slot3 >= @intFromEnum(Kind.false) or this.types.at(n.slot3).getKind() != .function_literal)
                        return error.NotAParameterizedFunction;

                    if (exp.len != 0) {
                        const resolved = try this.resolveWithTypeArgs(file, n, exp.len);

                        return this.getReturnType(resolved);
                    }

                    const resolved = try this.inferTypeCallExpParameterized(file, n, d.right) orelse return error.FailedToInferCallExp;
                    return try this.getReturnType(resolved);
                }

                if (n.getKind() != .function_literal) {
                    if (n.getKind() == .object_literal) {
                        var args = std.ArrayListUnmanaged(NodeRef){};
                        defer args.deinit(this.allocator());

                        var type_args = std.ArrayListUnmanaged(NodeRef){};
                        defer type_args.deinit(this.allocator());

                        {
                            var iter = NodeIterator.init(&file.ast.nodes, d.right);
                            while (iter.nextRef()) |p| {
                                try args.append(this.allocator(), p);
                            }
                        }

                        {
                            var iter = NodeIterator.init(&file.ast.nodes, exp.len);
                            while (iter.nextRef()) |p| {
                                try type_args.append(this.allocator(), p);
                            }
                        }

                        if (try this.findCallSignature(n, file, args.items, d.right, type_args.items)) |x| {
                            return this.types.at(x).slot3;
                        }
                    }

                    std.debug.print("{any}\n", .{n.getKind()});
                    try this.debugPrint(t);
                    return error.NotAFunction;
                }

                return n.slot3;
            },
            .conditional_expression => {
                // TODO: handle always true and always false cases
                const d = getPackedData(exp);
                if (try this.analyzeExpression(file, d.left)) |res| {
                    const when_true = blk: {
                        try pushExpressionFrame(file);
                        defer file.cfa_state.pop().deinit();

                        var iter = res.iterator();
                        while (iter.next()) |entry| {
                            try currentCFAFrame(file).types.put(entry.key_ptr.*, entry.value_ptr.path_type);
                        }

                        break :blk try this.getType(file, d.right);
                    };

                    const when_false = blk: {
                        try pushExpressionFrame(file);
                        defer file.cfa_state.pop().deinit();

                        var iter = res.iterator();
                        while (iter.next()) |entry| {
                            try currentCFAFrame(file).types.put(entry.key_ptr.*, entry.value_ptr.termination_type);
                        }

                        break :blk try this.getType(file, exp.len);
                    };

                    return this.toUnion(&.{ when_true, when_false });
                }

                const when_true = try this.getType(file, d.right);
                const when_false = try this.getType(file, exp.len);

                return this.toUnion(&.{ when_true, when_false });
            },
            .true_keyword => {
                if (!this.is_const_context and !this.is_const_variable_context and exp.len == 0) return @intFromEnum(Kind.boolean);

                return @intFromEnum(Kind.true);
            },
            .false_keyword => {
                if (!this.is_const_context and !this.is_const_variable_context and exp.len == 0) return @intFromEnum(Kind.boolean);

                return @intFromEnum(Kind.false);
            },
            .template_expression => {
                if (!this.is_const_context and !this.is_const_variable_context) return @intFromEnum(Kind.string);

                return @intFromEnum(Kind.string);
                // return error.TODO;
            },
            .regular_expression_literal => return this.getRegExpType(file, ref),
            .array_literal_expression => {
                var iter = NodeIterator.init(&file.ast.nodes, maybeUnwrapRef(exp) orelse 0);

                if (this.is_const_context) {
                    var flags: u24 = @intFromEnum(Flags.readonly);
                    var elements = std.ArrayList(TypeRef).init(this.allocator());

                    while (iter.nextPair()) |pair| {
                        if (pair[0].kind == .spread_element) {
                            return error.TODO;
                        }

                        const t = try this.getType(file, pair[1]);
                        try elements.append(t);

                        if (this.isParameterizedRef(t)) flags |= @intFromEnum(Flags.parameterized);
                    }

                    return this.createTupleType(elements.items, flags);
                }

                const old_variable_context = this.is_const_variable_context;
                defer this.is_const_variable_context = old_variable_context;
                this.is_const_variable_context = false;

                var tmp = TempUnion.init(this.allocator());
                while (iter.nextPair()) |pair| {
                    if (pair[0].kind == .spread_element) {
                        const t = try this.getType(file, unwrapRef(pair[0]));
                        if (t == @intFromEnum(Kind.any)) {
                            defer tmp.deinit();
                            return this.createArrayType(@intFromEnum(Kind.any));
                        }

                        if (t >= @intFromEnum(Kind.false)) return error.TODO;
                        const n = this.types.at(t);
                        switch (n.getKind()) {
                            .array => {
                                if (try this.addToUnion(&tmp, n.slot0)) |x| {
                                    return this.createArrayType(x);
                                }
                            },
                            .tuple => {
                                for (getSlice2(n, TypeRef)) |el| {
                                    const inner = blk: {
                                        if (el < @intFromEnum(Kind.false)) {
                                            const nty = this.types.at(el);
                                            if (hasTypeFlag(nty, .spread)) break :blk nty.slot1; // TODO: reduce this?
                                            if (nty.getKind() == .named_tuple_element) break :blk nty.slot1;
                                        }
                                        break :blk el;
                                    };

                                    if (try this.addToUnion(&tmp, inner)) |x| {
                                        return this.createArrayType(x);
                                    }
                                }
                            },
                            else => return notSupported(n.getKind()), // TODO
                        }
                        continue;
                    }

                    const t = try this.getType(file, pair[1]);
                    if (try this.addToUnion(&tmp, t)) |x| {
                        return this.createArrayType(x);
                    }
                }

                return this.createArrayType(try tmp.complete(this));
            },
            .object_literal_expression => {
                const node_start = maybeUnwrapRef(exp) orelse return @intFromEnum(Kind.empty_object);

                var elements = std.ArrayList(ObjectLiteralMember).init(this.type_args.allocator);
                var iter = NodeIterator.init(&file.ast.nodes, node_start);

                var accessors = std.AutoArrayHashMapUnmanaged(u32, struct {
                    name: TypeRef,
                    get: ?TypeRef = null,
                    set: ?TypeRef = null,
                }){};

                defer accessors.deinit(this.allocator());

                const old_variable_context = this.is_const_variable_context;
                defer this.is_const_variable_context = old_variable_context;
                this.is_const_variable_context = false;

                while (iter.nextPair()) |pair| {
                    const n = pair[0];
                    switch (n.kind) {
                        .shorthand_property_assignment => {
                            const ident = unwrapRef(n);
                            const t = try this.getType(file, ident);
                            try elements.append(.{
                                .kind = .property,
                                .name = try this.propertyNameToType(file, ident),
                                .type = t,
                            });
                        },
                        .property_assignment => {
                            const d = getPackedData(n);
                            const name = file.ast.nodes.at(d.left);
                            if (name.kind == .identifier or name.kind == .string_literal) {
                                const t = try this.getType(file, d.right);
                                try elements.append(.{
                                    .kind = .property,
                                    .name = try this.propertyNameToType(file, d.left),
                                    .type = t,
                                });
                                continue;
                            }

                            if (name.kind == .numeric_literal) {
                                return error.TODO_Numeric_literal;
                            }

                            if (name.kind == .computed_property_name) {
                                const t = try this.getType(file, unwrapRef(name));
                                if (t >= @intFromEnum(Kind.false)) {
                                    if (t >= @intFromEnum(Kind.zero)) {
                                        return error.TODO_Numeric_literal;
                                    }

                                    switch (@as(Kind, @enumFromInt(t))) {
                                        .string, .number, .symbol => {
                                            try elements.append(.{
                                                .kind = .index,
                                                .name = t,
                                                .type = try this.getType(file, d.right),
                                                .flags = n.flags,
                                            });
                                            continue;
                                        },
                                        else => return notSupported(@as(Kind, @enumFromInt(t))),
                                    }
                                }

                                // TODO: unions, string literals, unique symbols
                            }

                            return notSupported(name.kind);
                        },
                        .spread_assignment => {
                            const t = try this.getType(file, unwrapRef(n));
                            if (t == @intFromEnum(Kind.any)) {
                                elements.deinit();
                                return t;
                            }

                            // not an object = this is an error
                            if (t >= @intFromEnum(Kind.false)) {
                                if (t == @intFromEnum(Kind.empty_object)) continue;

                                // TODO: what does TS do here?
                                // if (t == @intFromEnum(Kind.object)) continue;

                                return @intFromEnum(Kind.never);
                            }

                            // TODO: do all object types
                            const type_node = this.types.at(t);
                            if (type_node.getKind() != .object_literal) {
                                return @intFromEnum(Kind.never);
                            }

                            // direct clone
                            if (iter.ref == 0 and elements.items.len == 0) {
                                return t;
                            }

                            // TODO: these should override preceding members
                            // except in the case where an added field is optional
                            for (getSlice2(type_node, ObjectLiteralMember)) |el| {
                                if (el.kind == .call_signature or el.kind == .method or el.kind == .setter) continue;
                                if (el.kind == .getter) {
                                    try elements.append(.{
                                        .kind = .property,
                                        .flags = el.flags,
                                        .name = el.name,
                                        .type = el.type,
                                    });
                                    continue;
                                }
                                try elements.append(el);
                            }

                            if (type_node.slot3 != 0 and type_node.slot3 < @intFromEnum(Kind.false)) {
                                const proto = this.types.at(type_node.slot3);
                                if (proto.getKind() == .object_literal) {
                                    var set = std.AutoArrayHashMapUnmanaged(TypeRef, void){};
                                    defer set.deinit(this.allocator());

                                    const shadows = getSlice2(type_node, ObjectLiteralMember);
                                    try set.ensureTotalCapacity(this.allocator(), shadows.len);
                                    for (shadows) |m| {
                                        if (m.kind == .index or m.kind == .call_signature or m.kind == .method or m.kind == .setter) continue;
                                        set.putAssumeCapacity(m.name, {});
                                    }

                                    for (getSlice2(proto, ObjectLiteralMember)) |el| {
                                        if (el.kind == .call_signature or el.kind == .method or el.kind == .setter) continue;
                                        if (el.kind != .index) {
                                            if (set.get(el.name)) |_| continue;
                                        }
                                        if (el.kind == .getter) {
                                            try elements.append(.{
                                                .kind = .property,
                                                .flags = el.flags,
                                                .name = el.name,
                                                .type = el.type,
                                            });
                                            continue;
                                        }
                                        try elements.append(el);
                                    }
                                } else {
                                    return error.TODO;
                                }
                            }
                        },
                        .method_declaration => {
                            const t = try this.getSignature(file, pair[1]);
                            try elements.append(.{
                                .kind = .method,
                                .name = try this.propertyNameToType(file, (getPackedData(n)).left),
                                .type = t,
                            });
                        },
                        .set_accessor => {
                            const d = getPackedData(n);
                            if (d.right == 0) return error.MissingParameter;

                            const name = try this.propertyNameToType(file, d.left);
                            const hash = try this.getMemberNameHash(name);
                            const t: TypeRef = blk: {
                                const p = file.ast.nodes.at(d.right);
                                const d2 = getPackedData(p);
                                if (d2.right != 0) return error.CannotHaveInit;

                                if (p.len == 0) break :blk @intFromEnum(Kind.empty_element);

                                break :blk try this.getType(file, p.len);
                            };

                            if (accessors.getEntry(hash)) |entry| {
                                entry.value_ptr.set = t;
                            } else {
                                try accessors.put(this.allocator(), hash, .{
                                    .name = name,
                                    .set = t,
                                });
                            }
                        },
                        .get_accessor => {
                            const d = getPackedData(n);
                            const name = try this.propertyNameToType(file, d.left);
                            const hash = try this.getMemberNameHash(name);
                            const t: TypeRef = blk: {
                                if (n.extra_data != 0) break :blk try this.getType(file, n.extra_data);
                                if (n.len != 0) break :blk try this.analyzeBody(file, unwrapRef(file.ast.nodes.at(n.len)));

                                break :blk @intFromEnum(Kind.empty_element);
                            };

                            if (accessors.getEntry(hash)) |entry| {
                                entry.value_ptr.get = t;
                            } else {
                                try accessors.put(this.allocator(), hash, .{
                                    .name = name,
                                    .get = t,
                                });
                            }
                        },
                        else => return error.TODO,
                    }
                }

                var accesors_iter = accessors.iterator();
                while (accesors_iter.next()) |entry| {
                    const s = entry.value_ptr.*;
                    const t: TypeRef = blk: {
                        const t1 = s.get orelse break :blk s.set.?;
                        const t2 = s.set orelse break :blk s.get.?;
                        if (t1 == @intFromEnum(Kind.empty_element)) break :blk t2;
                        if (t2 == @intFromEnum(Kind.empty_element)) break :blk t1;
                        if (t1 == t2) break :blk t1;
                        // TODO: We shouldn't merge if the setter has `unknown` or `any` but the getter returns something concrete

                        // Pick the widest type (TODO: validate the else condition)
                        if (try this.isAssignableTo(t1, t2) and !try this.isAssignableTo(t2, t1)) {
                            break :blk t2;
                        } else if (try this.isAssignableTo(t2, t1) and !try this.isAssignableTo(t1, t2)) {
                            break :blk t1;
                        } else {
                            return error.TODO;
                        }
                    };

                    try elements.append(.{
                        .kind = .property,
                        .name = s.name,
                        .type = t,
                    });
                }

                const obj_hash = try this.hashObjectLiteral(0, elements.items, 0);
                if (this.canonical_types.get(obj_hash)) |t| {
                    elements.deinit();
                    return t;
                }

                const x: u64 = @intFromPtr(elements.items.ptr);
                const t = try this.types.push(.{
                    .kind = @intFromEnum(Kind.object_literal),
                    .slot0 = @truncate(x),
                    .slot1 = @intCast(x >> 32),
                    .slot2 = @intCast(elements.items.len),
                });

                try this.canonical_types.put(obj_hash, t);

                return t;
            },
            .binding_element => {
                const pt = try this.getType(file, exp.extra_data);
                if (pt == 0) return 0; // TODO

                const d = getPackedData(exp);
                if (exp.extra_data2 != 0) {
                    const p = try this.numberToType(u32, exp.extra_data2 - 1);
                    return try this.accessType(pt, p);
                }

                const p = try this.propertyNameToType(file, if (exp.len != 0) exp.len else d.left);
                return try this.accessType(pt, p);
            },
            .tagged_template_expression => {
                const d = getPackedData(exp);
                // TODO: dedupe w/ call_expression
                const t = try this.getType(file, d.left);
                if (t >= @intFromEnum(Kind.false)) {
                    if (t == @intFromEnum(Kind.never)) {
                        return t;
                    }
                    this.printTypeInfo(t);
                    this.printCurrentNode();
                    return error.NotAFunctionType;
                }

                return this.types.at(t).slot3;
            },
            else => {},
        }

        return 0;
    }

    fn findCallSignature(this: *@This(), subject: *const Type, file: *ParsedFileData, args: []const NodeRef, args_start: NodeRef, type_args: []const NodeRef) !?TypeRef {
        const members = getSlice2(subject, ObjectLiteralMember);
        outer: for (members) |*m| {
            if (m.kind != .call_signature) continue;

            const fn_ty = try m.getType(this);
            const fn_ty_kind = this.getKindOfRef(fn_ty);
            if (fn_ty_kind == .parameterized) {
                const s = this.types.at(fn_ty);

                if (type_args.len != 0) continue; // TODO: check type args against type params

                std.debug.assert(this.getKindOfRef(s.slot3) == .function_literal);

                return try this.inferTypeCallExpParameterized(file, s, args_start) orelse continue;
            }
    
            if (fn_ty_kind != .function_literal) continue;

            const s = this.types.at(fn_ty);

            const s_params = getSlice2(s, TypeRef);
            if (args.len > s_params.len) {
                if (s_params.len == 0) continue;
                const last = s_params[s_params.len-1];
                const n = this.types.at(last);
                if (!n.hasFlag(.spread)) continue;

                // todo: check the spread type
                continue;
            }

            if (args.len != s_params.len) {
                continue; // TODO
            }

            if (args.len > 0) {
                const n = file.ast.nodes.at(args[args.len-1]);
                if (n.kind == .spread_element) continue; // TODO
            }

            for (args, 0..) |p, i| {
                const st = s_params[i];

                const old_ctx = this.inferrence_ctx;
                defer this.inferrence_ctx = old_ctx;
                std.debug.assert(this.getKindOfRef(st) == .named_tuple_element);
                this.inferrence_ctx = this.types.at(st).slot1;

                const pt = try this.getType(file, p);
                if (!try this.isAssignableTo(pt, st)) continue :outer;
            }

            return fn_ty;
        }

        return null;
    }

    fn notSupported(n: anytype) !u32 {
        std.debug.print("  not supported: {any}\n\n", .{n});
        return error.Unsupported;
    }

    pub inline fn getSlice2(t: *const Type, comptime T: type) []T {
        if (T == TypeRef) {
            switch (t.getKind()) {
                .alias, .query => return TypeList.getSliceFromType(t),
                else => {},
            }
        }

        if (t.slot2 == 0) return &.{};

        const ptr: [*]T = @ptrFromInt((@as(u64, t.slot1) << 32) | t.slot0);

        return ptr[0..t.slot2];
    }

    const SymbolCollector = struct {
        analyzer: *Analyzer,
        set: *std.AutoHashMap(u64, bool),
        alias_set: std.AutoArrayHashMapUnmanaged(TypeRef, void) = std.AutoArrayHashMapUnmanaged(TypeRef, void){},

        file_ref: ?FileRef = null,
        external_set: ?*std.AutoHashMap(u64, bool) = null,

        pub fn gatherReferencedSymbolsFromAst(this: *@This(), f: *ParsedFileData, node_ref: NodeRef) anyerror!void {
            const Visitor = struct {
                file: *ParsedFileData,
                nodes: *const BumpAllocator(AstNode),
                collector: *SymbolCollector,

                // Returns true if we should visit nodes associated with this identifier
                fn visitIdent(self: *@This(), n: *const AstNode) !bool {
                    if (n.extra_data == 0) {
                        return false;
                    }

                    if (!self.file.binder.symbols.at(n.extra_data).hasFlag(.top_level)) {
                        return false;
                    }

                    const key: u64 = (@as(u64, self.file.id) << 32) | n.extra_data;
                    const entry = try self.collector.set.getOrPut(key);

                    entry.value_ptr.* = true;
                    return true;
                }

                pub fn visit(self: *@This(), n: *const AstNode, _: NodeRef) anyerror!void {
                    switch (n.kind) {
                        .block => {},
                        .identifier => {
                            _ = try self.visitIdent(n);
                        },
                        .function_declaration, .class_declaration, .interface_declaration, .type_alias_declaration, .variable_declaration => {
                            std.debug.assert(getPackedData(n).left != 0);
                            const should_continue = try self.visitIdent(self.nodes.at(getPackedData(n).left));
                            if (should_continue) {
                                return parser.forEachChild(self.nodes, n, self);
                            }
                        },
                        else => {
                            try parser.forEachChild(self.nodes, n, self);
                        },
                    }
                }
            };

            var v = Visitor{ .collector = this, .file = f, .nodes = &f.ast.nodes };
            try v.visit(f.ast.nodes.at(node_ref), node_ref);
        }

        pub fn gatherReferencedSymbols(this: *@This(), type_ref: TypeRef) anyerror!void {
            if (type_ref == 0 or type_ref >= @intFromEnum(Kind.false)) {
                return;
            }

            const t = this.analyzer.types.at(type_ref);
            switch (t.getKind()) {
                .alias => {
                    const args = getSlice2(t, TypeRef);
                    const key: u64 = (@as(u64, t.slot3) << 32) | t.slot4;
                    const entry = try this.set.getOrPut(key);
                    if (entry.found_existing) {
                        if (args.len > 0) {
                            const visited = try this.alias_set.getOrPut(this.set.allocator, type_ref);
                            if (visited.found_existing) return;

                            for (args) |arg| {
                                try this.gatherReferencedSymbols(arg);
                            }
                        }

                        return;
                    }

                    if (args.len > 0) {
                        try this.alias_set.putNoClobber(this.set.allocator, type_ref, {});

                        for (args) |arg| {
                            try this.gatherReferencedSymbols(arg);
                        }
                    }

                    if (t.hasFlag(.global)) {
                        return;
                    }

                    // Skip visiting external symbols
                    if (this.file_ref) |ref| {
                        if (ref != t.slot3) {
                            if (this.external_set) |s| {
                                if (!this.analyzer.program.getFileData(t.slot3).is_lib) {
                                    try s.put(key, true);
                                }
                            }

                            return;
                        }
                    }

                    const inner = try this.analyzer.getTypeOfSymbol(try this.analyzer.getAnalyzedFile(t.slot3), t.slot4);
                    try this.gatherReferencedSymbols(inner);
                },
                .query => {
                    const f = this.analyzer.program.getFileData(t.slot3);
                    var n = f.ast.nodes.at(t.slot4);
                    while (true) {
                        switch (n.kind) {
                            .identifier => {
                                try this.set.put((@as(u64, t.slot3) << 32) | n.extra_data, true);
                                break;
                            },
                            .property_access_expression => {
                                const d = getPackedData(n);
                                n = f.ast.nodes.at(d.left);
                            },
                            else => break,
                        }
                    }
                    for (getSlice2(t, TypeRef)) |arg| {
                        try this.gatherReferencedSymbols(arg);
                    }
                },
                .parameterized => {
                    for (getSlice2(t, TypeRef)) |param| {
                        try this.gatherReferencedSymbols(param);
                    }
                    try this.gatherReferencedSymbols(t.slot3);
                },
                .function_literal => {
                    for (getSlice2(t, TypeRef)) |param| {
                        try this.gatherReferencedSymbols(param);
                    }
                    try this.gatherReferencedSymbols(t.slot3);
                    try this.gatherReferencedSymbols(t.slot4);
                },
                .named_tuple_element => {
                    try this.gatherReferencedSymbols(t.slot1);
                },
                .conditional => {
                    try this.gatherReferencedSymbols(t.slot0);
                    try this.gatherReferencedSymbols(t.slot1);
                    try this.gatherReferencedSymbols(t.slot2);
                    try this.gatherReferencedSymbols(t.slot3);
                },
                .object_literal => {
                    for (getSlice2(t, ObjectLiteralMember)) |*m| {
                        try this.gatherReferencedSymbols(m.name);
                        if (!m.isLazy()) {
                            try this.gatherReferencedSymbols(try m.getType(this.analyzer));
                            continue;
                        }

                        // FIXME: we need to evaluate the type if it must be inferred
                        const f = this.analyzer.program.getFileData(m.slot0);
                        try this.gatherReferencedSymbolsFromAst(f, m.type);
                    }
                },
                .@"union", .intersection => {
                    for (getSlice2(t, TypeRef)) |el| {
                        try this.gatherReferencedSymbols(el);
                    }
                },
                .keyof => {
                    try this.gatherReferencedSymbols(t.slot0);
                },
                .class => {
                    const c = Analyzer.Type.Class.fromType(t);
                    try this.gatherReferencedSymbols(c.instance_type);
                    try this.gatherReferencedSymbols(c.static_type);
                    try this.gatherReferencedSymbols(c.base_class);
                },
                .indexed => {
                    try this.gatherReferencedSymbols(t.slot0);
                    try this.gatherReferencedSymbols(t.slot1);
                },

                // TODO
                .mapped => {},
                .type_parameter => {},
                else => {},
            }
        }
    };

    pub fn gatherReferencedSymbols(this: *@This(), set: *std.AutoHashMap(u64, bool), type_ref: TypeRef, file_ref: ?FileRef) anyerror!void {
        var c = SymbolCollector{ .analyzer = this, .set = set, .file_ref = file_ref };
        try c.gatherReferencedSymbols(type_ref);
    }

    pub fn gatherReferencedSymbols2(this: *@This(), set: *std.AutoHashMap(u64, bool), type_ref: TypeRef, file_ref: ?FileRef, external_set: *std.AutoHashMap(u64, bool)) anyerror!void {
        var c = SymbolCollector{
            .analyzer = this,
            .set = set,
            .file_ref = file_ref,
            .external_set = external_set,
        };
        try c.gatherReferencedSymbols(type_ref);
    }

    pub fn gatherReferencedSymbolsFromAst(this: *@This(), set: *std.AutoHashMap(u64, bool), f: *ParsedFileData, node_ref: TypeRef) anyerror!void {
        var c = SymbolCollector{ .analyzer = this, .set = set, .file_ref = f.id };
        try c.gatherReferencedSymbolsFromAst(f, node_ref);
    }

    pub fn printCurrentNode(this: *const @This()) void {
        if (comptime is_debug) {
            if (this.current_node) |p| {
                printNameWithLocation(p[0], p[1]) catch unreachable;
            }
        }
    }

    fn getIdentFromAlias(this: *const @This(), t: *const Type) ?*const AstNode {
        if (t.hasFlag(.global)) {
            const g = this.program.ambient.globals_allocator.at(t.slot4);
            std.debug.assert(g.symbols.items.len > 0);
            const abs_ref = g.symbols.items[0];
            return getIdentFromSymbol(this.program.files.items[abs_ref.file_id].binder, abs_ref.ref);
        }
        return getIdentFromSymbol(this.program.files.items[t.slot3].binder, t.slot4);
    }

    fn getTypeListSize(t: *const Type) u32 {
        if (t.hasFlag(.allocated_list)) return t.slot0;
        if (t.slot0 == 0) return 0;
        if (t.slot1 == 0) return 1;
        if (t.slot2 == 0) return 2;
        return 3;
    }

    pub fn printTypeInfo(this: *const @This(), ty: TypeRef) void {
        if (ty >= @intFromEnum(Kind.false)) {
            if (ty >= @intFromEnum(Kind.zero)) {
                const d: u30 = @intCast(ty - @intFromEnum(Kind.zero));
                const v: i30 = @bitCast(d);
                return std.debug.print("[number] {}\n", .{v});
            }
            if (isTypeParamRef(ty)) {
                const register: u32 = ty & (~@intFromEnum(Kind.type_parameter_ref));
                return std.debug.print("[type param ref] {}\n", .{register});
            }
            std.debug.print("[primitive] {any}\n", .{@as(Kind, @enumFromInt(ty))});
            return;
        }

        const t = this.types.at(ty);
        if (t.getKind() == .type_parameter) {
            const ident = getIdentFromSymbol(this.program.files.items[t.slot4].binder, t.slot0) orelse unreachable;
            const name = getSlice(ident, u8);
            return std.debug.print("[type param] {s} @ reg{}\n", .{ name, t.slot3 });
        } else if (t.getKind() == .alias) {
            const ident = this.getIdentFromAlias(t) orelse unreachable;
            const name = getSlice(ident, u8);
            if (t.hasFlag(.global)) {
                std.debug.print("[global] ", .{});
            }
            if (t.hasFlag(.parameterized)) {
                std.debug.print("[parameterized] ", .{});
            }
            const len = getTypeListSize(t);
            std.debug.print("[alias] {s}<len:{}> ", .{name, len});
            if (t.hasFlag(.instance_alias)) {
                return std.debug.print("[instance] ", .{});
            } else if (t.hasFlag(.class_alias)) {
                return std.debug.print("[class] ", .{});
            }
            return std.debug.print("\n", .{});
        }
        if (t.hasFlag(.parameterized)) {
            return std.debug.print("[ref] {any} [parameterized]\n", .{t.getKind()});
        }
        std.debug.print("[ref] {any}\n", .{t.getKind()});
        if (t.getKind() == .function_literal) {
            std.debug.print("  rt: ", .{});
            this.printTypeInfo(t.slot3);
        }
        if (t.getKind() == .@"union") {
            const elements = getSlice2(t, TypeRef);
            if (elements.len > 10) {
                std.debug.print("  element count: {}", .{elements.len});
                return;
            }

            for (elements) |el| {
                std.debug.print("  ", .{});
                this.printTypeInfo(el);
            }
        }
    }

    pub fn debugPrint(this: *@This(), ty: TypeRef) !void {
        const p = try this.getPrinter();
        const x = try p.toTypeNode(ty);
        if (x == 0) return;

        const f = this.program.getFileData(0);
        const s = try parser.printInMemoryWithTypes(.{
            .source_name = f.file_name,
            .nodes = p.synthetic_nodes,
            .decorators = f.ast.decorators,
            .source = f.ast.source,
            .positions = f.ast.positions,
        }, p.synthetic_nodes.at(x).*);

        std.debug.print("{s}", .{s});
    }

    fn _debug(this: *@This(), ty: u32) !void {
        try this.debugPrint(ty);
        std.debug.print("\n", .{});
    }

    const TypePrinter = struct {
        analyzer: *Analyzer,
        synthetic_nodes: parser.BumpAllocator(AstNode),
        cached_type_to_node: std.AutoArrayHashMap(TypeRef, NodeRef),
        cached_string_types: std.AutoArrayHashMap(TypeRef, NodeRef),

        can_print_identifier: bool = false,

        imported_symbols: ?std.AutoArrayHashMap(u64, []const u8) = null,
        triple_slash_directives: std.ArrayList(parser.TripleSlashDirective), // XXX: used by `printDeclarationText`

        pub fn init(analyzer: *Analyzer) !*@This() {
            var synthetic_nodes = parser.BumpAllocator(AstNode).init(analyzer.allocator(), std.heap.page_allocator);
            try synthetic_nodes.preAlloc();
            _ = try synthetic_nodes.push(.{ .kind = .start });

            const p = try analyzer.allocator().create(@This());
            p.* = .{
                .analyzer = analyzer,
                .synthetic_nodes = synthetic_nodes,
                .cached_type_to_node = std.AutoArrayHashMap(TypeRef, NodeRef).init(analyzer.allocator()),
                .cached_string_types = std.AutoArrayHashMap(TypeRef, NodeRef).init(analyzer.allocator()),
                .triple_slash_directives = std.ArrayList(parser.TripleSlashDirective).init(analyzer.allocator()),
            };

            return p;
        }

        pub fn toTypeNode(this: *@This(), ty: u32) !NodeRef {
            if (isTypeParamRef(ty)) {
                return this._toTypeNode(ty);
            }

            if (ty < @intFromEnum(Kind.false) and this.analyzer.types.at(ty).getKind() == .string_literal) {
                return this._toTypeNode(ty);
            }

            if (this.cached_type_to_node.get(ty)) |r| {
                return r;
            }

            const n = try this._toTypeNode(ty);
            try this.cached_type_to_node.put(ty, n);

            return n;
        }

        pub fn removeCacheEntry(this: *@This(), ty: u32) void {
            _ = this.cached_type_to_node.swapRemove(ty);
        }

        fn toIdentLike(this: *@This(), ty: u32) !NodeRef {
            const old_state = this.can_print_identifier;
            defer this.can_print_identifier = old_state;

            this.can_print_identifier = true;

            return this.toTypeNode(ty);
        }

        fn parenthesizeType(this: *@This(), ref: NodeRef) !NodeRef {
            return this.synthetic_nodes.push(.{
                .kind = .parenthesized_type,
                .data = @ptrFromInt(ref),
            });
        }

        fn maybeParenthesizeTypeForUnionLike(this: *@This(), ref: NodeRef, comptime parent_kind: SyntaxKind) !NodeRef {
            comptime switch (parent_kind) {
                .union_type, .intersection_type, .conditional_type => {},
                else => @compileError("Invalid kind"),
            };

            return switch (this.synthetic_nodes.at(ref).kind) {
                .function_type, .conditional_type => this.parenthesizeType(ref),
                .infer_type => {
                    // `infer U extends number` needs to be parenthesized
                    const inner = this.synthetic_nodes.at(unwrapRef(this.synthetic_nodes.at(ref)));
                    if ((getPackedData(inner)).right != 0) {
                        return this.parenthesizeType(ref);
                    }
                    return ref;
                },
                .intersection_type => if (comptime parent_kind == .union_type) this.parenthesizeType(ref) else ref,
                .union_type => if (comptime parent_kind == .intersection_type) this.parenthesizeType(ref) else ref,
                else => ref,
            };
        }

        fn copyNodePair(this: *@This(), n: *const AstNode, file_id: u32) !?*anyopaque {
            const d = getPackedData(n);
            const l = try this.copyNodeFromRef(d.left, file_id);
            const r = try this.copyNodeFromRef(d.right, file_id);
            return toBinaryDataPtrRefs(l, r);
        }

        fn getFile(this: *@This(), id: FileRef) !*ParsedFileData {
            return this.analyzer.getAnalyzedFile(id);
        }

        // you could also clear `next` apart of `BumpAllocatorList`, this would be slightly fewer instructions
        inline fn copyNodeNoNext(this: *@This(), n: *const AstNode) !NodeRef{ 
            var copy = n.*;
            copy.next = 0;
            return this.synthetic_nodes.push(copy);
        }

        fn copyNodeFromRef(this: *@This(), ref: NodeRef, file_id: u32) anyerror!NodeRef {
            std.debug.assert(ref != 0);
            const n = this.analyzer.program.getFileData(file_id).ast.nodes.at(ref);
            return switch (n.kind) {
                .identifier, .this_keyword, .string_literal => this.copyNodeNoNext(n),
                .property_access_expression => this.synthetic_nodes.push(.{
                    .kind = n.kind,
                    .flags = n.flags,
                    .data = try this.copyNodePair(n, file_id),
                }),
                else => {
                    std.debug.print("TODO {any}\n", .{n.kind});
                    return error.TODO;
                },
            };
        }

        fn copyIdentFromSymbol(this: *@This(), file_id: u32, ref: SymbolRef) !NodeRef {
            const f = this.analyzer.program.getFileData(file_id);
            const sym = f.binder.symbols.at(ref);
            if (sym.binding != 0 and !sym.hasFlag(.global)) {
                const ident = f.ast.nodes.at(sym.binding);

                return this.copyNodeNoNext(ident);
            }

            const decl = f.ast.nodes.at(sym.declaration);
            const d = getPackedData(decl);
            const ident = f.ast.nodes.at(d.left);

            return this.copyNodeNoNext(ident);
        }

        fn convertTemplatePart(this: *@This(), ref: TypeRef, kind: SyntaxKind) !NodeRef {
            if (ref == @intFromEnum(Kind.empty_string)) return this.synthetic_nodes.push(.{
                .kind = kind,
                .data = null,
                .len = 0,
            });

            const s = this.analyzer.types.at(ref);
            if (s.slot5 == 1) {
                const slice = this.analyzer.synthetic_strings.items[s.slot0];

                return this.synthetic_nodes.push(.{
                    .kind = kind,
                    .data = slice.ptr,
                    .len = @intCast(slice.len),
                });
            }

            const t = this.analyzer.program.getFileData(s.slot0).ast.nodes.at(s.slot1);

            return this.synthetic_nodes.push(.{
                .kind = kind,
                .data = t.data,
                .len = t.len,
            });
        }

        fn convertUnionOrIntersection(this: *@This(), t: *const Type, comptime kind: SyntaxKind) !NodeRef {
            comptime switch (kind) {
                .union_type, .intersection_type => {},
                else => @compileError("Invalid kind"),
            };

            const slice = getSlice2(t, TypeRef);
            std.debug.assert(slice.len >= 2);

            var n = try this.synthetic_nodes.push(.{
                .kind = kind,
                .data = toBinaryDataPtrRefs(
                    try this.maybeParenthesizeTypeForUnionLike(try this.toTypeNode(slice[0]), kind),
                    try this.maybeParenthesizeTypeForUnionLike(try this.toTypeNode(slice[1]), kind),
                ),
            });

            for (slice[2..]) |el| {
                const u = try this.maybeParenthesizeTypeForUnionLike(try this.toTypeNode(el), kind);
                const v = try this.synthetic_nodes.push(.{
                    .kind = kind,
                    .data = toBinaryDataPtrRefs(n, u),
                });
                n = v;
            }

            return n;
        }

        fn copyIdentFromAlias(this: *@This(), k: *const Type) !NodeRef {
            const ident = blk: {
                if (k.hasFlag(.global)) {
                    const g = this.analyzer.program.ambient.globals_allocator.at(k.slot4);
                    std.debug.assert(g.symbols.items.len > 0);
                    const abs_ref = g.symbols.items[0];
                    break :blk getIdentFromSymbol(this.analyzer.program.files.items[abs_ref.file_id].binder, abs_ref.ref)
                        orelse return error.NoIdentFound;
                }

                const f = this.analyzer.program.getFileData(k.slot3);
                const sym = f.binder.symbols.at(k.slot4);
                if (sym.hasFlag(.late_bound)) {
                    const g = this.analyzer.program.ambient.globals_allocator.at(sym.declaration);
                    // The declaration might not be available
                    if (g.symbols.items.len > 0) {
                        const b = this.analyzer.program.getFileData(g.symbols.items[0].file_id).binder;
                        break :blk getIdentFromSymbol(b, g.symbols.items[0].ref) orelse return error.NoIdentFound;
                    }
                }
                break :blk getIdentFromSymbol(f.binder, k.slot4) orelse return error.NoIdentFound;
            };
            return try this.copyNodeNoNext(ident);
        }

        fn _toTypeNode(this: *@This(), ty: u32) anyerror!NodeRef {
            if (ty == 0) return 0;

            if (isTypeParamRef(ty)) {
                const r = this.analyzer.resolveTypeParam(ty) orelse {
                    if (is_debug) {
                        const missing = "(MISSING)";
                        const x = try getAllocator().alloc(u8, missing.len);
                        @memcpy(x, missing);
                        return try this.synthetic_nodes.push(.{
                            .kind = .identifier,
                            .data = x.ptr,
                            .len = @intCast(x.len),
                        });
                    }

                    std.debug.print("  {}\n", .{ty - @intFromEnum(Kind.type_parameter_ref)});
                    for (0..this.analyzer.type_registers.len) |i| {
                        const ref = this.analyzer.type_registers[i];
                        if (ref == 0) continue;
                        std.debug.print("  {}: {}\n", .{ i, ref });
                    }

                    return error.MissingTypeParam;
                };
                const k = this.analyzer.types.at(r);

                return this.copyIdentFromSymbol(k.slot4, k.slot0);
            }

            if (ty >= @intFromEnum(Kind.zero)) {
                const z: i30 = @bitCast(@as(u30, @intCast(ty & ~@as(u30, 0))));
                const val: f64 = @floatFromInt(z);

                return this.synthetic_nodes.push(.{
                    .kind = .numeric_literal,
                    .data = if (val == 0) null else @ptrFromInt(@as(u64, @bitCast(val))),
                });
            } else if (ty < @intFromEnum(Kind.false)) {
                const k = this.analyzer.types.at(ty);

                switch (k.getKind()) {
                    .alias => {
                        const copy = try this.copyIdentFromAlias(k);

                        var l = BumpAllocatorList(AstNode).init(&this.synthetic_nodes);

                        for (getSlice2(k, TypeRef)) |el| {
                            const n = try this.toTypeNode(el);
                            l.appendRef(n);
                        }

                        // Rewrite imported aliases into import types
                        if (this.imported_symbols) |m| {
                            const key = (@as(u64, k.slot3) << 32) | k.slot4;
                            if (m.get(key)) |spec| {
                                const spec_ref = try this.synthetic_nodes.push(.{
                                    .kind = .string_literal,
                                    .data = spec.ptr,
                                    .len = @intCast(spec.len),
                                    .flags = @intFromEnum(StringFlags.double_quote),
                                });

                                return this.synthetic_nodes.push(.{
                                    .kind = .import_type,
                                    .data = toBinaryDataPtrRefs(spec_ref, copy),
                                    .len = l.head,
                                });
                            }
                        }

                        // typeof X
                        if (k.hasFlag(.class_alias)) {
                            return this.synthetic_nodes.push(.{
                                .kind = .type_query,
                                .data = @ptrFromInt(copy),
                                .len = l.head,
                            });
                        }

                        return this.synthetic_nodes.push(.{
                            .kind = .type_reference,
                            .data = toBinaryDataPtrRefs(copy, l.head),
                        });
                    },
                    .query => {
                        const copy = try this.copyNodeFromRef(k.slot4, k.slot3);
                        var l = BumpAllocatorList(AstNode).init(&this.synthetic_nodes);

                        for (getSlice2(k, TypeRef)) |el| {
                            const n = try this.toTypeNode(el);
                            l.appendRef(n);
                        }

                        return this.synthetic_nodes.push(.{
                            .kind = .type_query,
                            .data = @ptrFromInt(copy),
                            .len = l.head,
                        });
                    },
                    .string_literal => {
                        const key = (k.slot3 << 29) | ty;
                        if (this.cached_string_types.get(key)) |n| return n;

                        if (k.slot5 == 1) {
                            const slice = this.analyzer.synthetic_strings.items[k.slot0];

                            const ref = try this.synthetic_nodes.push(.{
                                .kind = .string_literal,
                                .data = slice.ptr,
                                .len = @intCast(slice.len),
                                .flags = @intFromEnum(StringFlags.single_quote),
                            });

                            try this.cached_string_types.put(key, ref);
                            return ref;
                        }

                        const f = this.analyzer.program.getFileData(k.slot0);
                        const n = f.ast.nodes.at(k.slot1);
                        if (k.slot3 == 0 and !this.can_print_identifier) {
                            // TODO: we might remove the two-byte flag
                            const ref = try this.synthetic_nodes.push(.{
                                .kind = .string_literal,
                                .data = n.data,
                                .len = n.len,
                                .flags = @intFromEnum(StringFlags.single_quote),
                            });
                            const new_key = (@as(u32, @intFromEnum(StringFlags.single_quote)) << 29) | ty;
                            try this.cached_string_types.put(new_key, ref);
                            return ref;
                        }

                        const ref = try this.copyNodeNoNext(n);

                        if (n.kind == .template_head or n.kind == .template_middle or n.kind == .template_tail) {
                            this.synthetic_nodes.at(ref).kind = .string_literal;
                            this.synthetic_nodes.at(ref).flags = @intFromEnum(StringFlags.single_quote);
                        }

                        try this.cached_string_types.put(key, ref);
                        return ref;
                    },
                    // `typeof Symbol()`
                    .symbol_literal => {
                        return this.synthetic_nodes.push(.{
                            .kind = .type_operator,
                            .data = toBinaryDataPtrRefs(
                                @intFromEnum(SyntaxKind.unique_keyword),
                                try this.synthetic_nodes.push(.{
                                    .kind = .symbol_keyword,
                                }),
                            ),
                        });
                    },
                    .number_literal => {
                        const f: u64 = @bitCast((@as(u64, k.slot0) << 32) | k.slot1);
                        return this.synthetic_nodes.push(.{
                            .kind = .numeric_literal,
                            .data = @ptrFromInt(f),
                        });
                    },
                    .array => {
                        const ref = try this.toTypeNode(k.slot0);
                        const n = this.synthetic_nodes.at(ref);
                        switch (n.kind) {
                            .union_type, .function_type, .type_operator, .infer_type => {
                                return this.synthetic_nodes.push(.{
                                    .kind = .array_type,
                                    .data = @ptrFromInt(try this.parenthesizeType(ref)),
                                });
                            },
                            else => {},
                        }

                        return this.synthetic_nodes.push(.{
                            .kind = .array_type,
                            .data = @ptrFromInt(ref),
                        });
                    },
                    .tuple => {
                        var l = BumpAllocatorList(AstNode).init(&this.synthetic_nodes);

                        for (getSlice2(k, TypeRef)) |el| {
                            const n = try this.toTypeNode(el);
                            l.appendRef(n);
                        }

                        const ref = try this.synthetic_nodes.push(.{
                            .kind = .tuple_type,
                            .data = if (l.head != 0) @ptrFromInt(l.head) else null,
                        });

                        if (!hasTypeFlag(k, .readonly)) return ref;

                        return this.synthetic_nodes.push(.{
                            .kind = .type_operator,
                            .data = toBinaryDataPtrRefs(@intFromEnum(SyntaxKind.readonly_keyword), ref),
                        });
                    },
                    .template_literal => {
                        var l = BumpAllocatorList(AstNode).init(&this.synthetic_nodes);
                        const slice = getSlice2(k, TypeRef);

                        const head = try this.convertTemplatePart(slice[0], .template_head);
                        l.appendRef(head);

                        var i: u32 = 1;
                        while (i < slice.len) : (i += 2) {
                            const exp = try this.toTypeNode(slice[i]);

                            const text = try this.convertTemplatePart(slice[i + 1], if (i == slice.len - 2) .template_tail else .template_middle);
                            const span = try this.synthetic_nodes.push(.{
                                .kind = .template_literal_type_span,
                                .data = toBinaryDataPtrRefs(exp, text),
                            });

                            l.appendRef(span);
                        }
                        return this.synthetic_nodes.push(.{
                            .kind = .template_literal_type,
                            .data = @ptrFromInt(l.head),
                        });
                    },
                    .indexed => {
                        const lhs = try this.toTypeNode(k.slot0);
                        const rhs = try this.toTypeNode(k.slot1);

                        return this.synthetic_nodes.push(.{
                            .kind = .indexed_access_type,
                            .data = toBinaryDataPtrRefs(lhs, rhs),
                        });
                    },
                    .intersection => return this.convertUnionOrIntersection(k, .intersection_type),
                    .@"union" => return this.convertUnionOrIntersection(k, .union_type),
                    .class => {
                        var l = BumpAllocatorList(AstNode).init(&this.synthetic_nodes);

                        const visitor = struct {
                            pub fn visit(list: *BumpAllocatorList(AstNode), printer: *TypePrinter, obj: *const Type, comptime is_static: bool) !void {
                                for (getSlice2(obj, ObjectLiteralMember)) |*el| {
                                    //if (el.flags & protected_or_private != 0) continue;

                                    if (el.isLazy()) {
                                        const file = printer.analyzer.program.getFileData(el.slot0);
                                        try list.append(.{
                                            .kind = .external_node,
                                            .data = &file.ast,
                                            .len = el.type,
                                        });
                                        continue;
                                    }

                                    const t = try printer.toTypeNode(try el.getType(printer.analyzer));
                                    const copy = try printer.toIdentLike(el.name);

                                    var flags: u20 = @truncate(el.flags);
                                    flags &= ~@as(u20, @intFromEnum(NodeFlags.public));
                                    if (is_static) flags |= @intFromEnum(NodeFlags.static);

                                    if (el.kind == .method) {
                                        const d = getPackedData(printer.synthetic_nodes.at(t));
                                        try list.append(.{
                                            .kind = .method_declaration,
                                            .data = toBinaryDataPtrRefs(copy, d.left),
                                            .extra_data = printer.synthetic_nodes.at(t).len,
                                            .extra_data2 = d.right,
                                            .flags = flags,
                                        });
                                        continue;
                                    }

                                    try list.append(.{
                                        .kind = .property_declaration,
                                        .data = toBinaryDataPtrRefs(copy, 0),
                                        .len = t,
                                        .flags = flags,
                                    });
                                }
                            }
                        }.visit;

                        if (k.slot0 != @intFromEnum(Kind.empty_object)) {
                            try visitor(&l, this, this.analyzer.types.at(k.slot0), false);
                        }

                        if (k.slot1 != @intFromEnum(Kind.empty_object)) {
                            try visitor(&l, this, this.analyzer.types.at(k.slot1), true);
                        }

                        // TODO: we have to generate a synthetic decl if the resulting type node isn't `expression_with_type_arguments`
                        const extends = try this.toTypeNode(k.slot3);
                        if (extends != 0) {
                            const extends_node = this.synthetic_nodes.at(extends);
                            switch (extends_node.kind) {
                                .expression_with_type_arguments => {},
                                .type_reference => {
                                    extends_node.kind = .expression_with_type_arguments;
                                },
                                else => return error.TODO_computed_base_class,
                            }
                        }

                        return this.synthetic_nodes.push(.{
                            .kind = .class_declaration,
                            .data = if (l.head == 0) null else toBinaryDataPtrRefs(0, l.head),
                            .len = extends,
                        });
                    },
                    .function_literal => {
                        var l = BumpAllocatorList(AstNode).init(&this.synthetic_nodes);
                        if (k.slot4 != 0) {
                            const this_keyword = try this.synthetic_nodes.push(.{
                                .kind = .this_keyword,
                            });

                            try l.append(.{
                                .kind = .parameter,
                                .data = toBinaryDataPtrRefs(this_keyword, 0),
                                .len = try this.toTypeNode(k.slot4),
                            });
                        }

                        const params = getSlice2(k, TypeRef);
                        for (params) |z| {
                            const p = this.analyzer.types.at(z);
                            const copy = try this.copyNodeFromRef(p.slot0, p.slot4);

                            var flags: u20 = 0;
                            if (hasTypeFlag(p, .optional)) flags |= @intFromEnum(NodeFlags.optional);
                            if (hasTypeFlag(p, .spread)) flags |= @intFromEnum(NodeFlags.generator);

                            const param_type_node = try this.toTypeNode(p.slot1);

                            try l.append(.{
                                .kind = .parameter,
                                .data = toBinaryDataPtrRefs(copy, 0),
                                .len = param_type_node,
                                .flags = flags,
                            });
                        }

                        if (hasTypeFlag(k, .constructor)) {
                            return this.synthetic_nodes.push(.{
                                .kind = .constructor_type,
                                .data = toBinaryDataPtrRefs(l.head, try this.toTypeNode(k.slot3)),
                                .flags = if (hasTypeFlag(k, .abstract)) @intFromEnum(NodeFlags.abstract) else 0,
                            });
                        }

                        return this.synthetic_nodes.push(.{
                            .kind = .function_type,
                            .data = toBinaryDataPtrRefs(l.head, try this.toTypeNode(k.slot3)),
                        });
                    },
                    .named_tuple_element => {
                        const inner = try this.toTypeNode(k.slot1);

                        if (k.slot0 == 0) {
                            if (hasTypeFlag(k, .spread)) {
                                return this.synthetic_nodes.push(.{
                                    .kind = .rest_type,
                                    .data = @ptrFromInt(inner),
                                });
                            }

                            if (hasTypeFlag(k, .optional)) {
                                return this.synthetic_nodes.push(.{
                                    .kind = .optional_type,
                                    .data = @ptrFromInt(inner),
                                });
                            }

                            return inner;
                        }

                        var flags: u20 = 0;
                        if (hasTypeFlag(k, .spread)) flags |= @intFromEnum(NodeFlags.generator);
                        if (hasTypeFlag(k, .optional)) flags |= @intFromEnum(NodeFlags.optional);

                        const copy = try this.copyNodeFromRef(k.slot0, k.slot4);

                        return this.synthetic_nodes.push(.{
                            .kind = .named_tuple_member,
                            .data = toBinaryDataPtrRefs(copy, inner),
                            .flags = flags,
                        });
                    },
                    .object_literal => {
                        const old_this_type = this.analyzer.contextual_this_type;
                        defer this.analyzer.contextual_this_type = old_this_type;
                        this.analyzer.contextual_this_type = @intFromEnum(Kind.this);

                        // this.analyzer.printTypeInfo(ty);
                        // std.debug.print("{}, {s} -> {} [flags: {}]\n", .{
                        //     ty,
                        //     if (getSlice2(k, ObjectLiteralMember)[0].isLazy()) "y" else "n",
                        //     try this.analyzer.hashObjectLiteral(k.flags, getSlice2(k, ObjectLiteralMember)),
                        //     k.flags,
                        // });

                        var l = BumpAllocatorList(AstNode).init(&this.synthetic_nodes);
                        for (getSlice2(k, ObjectLiteralMember)) |*el| {
                            if (!el.isPublic()) continue;

                            const t = try this.toTypeNode(try el.getType(this.analyzer));

                            if (el.kind == .call_signature) {
                                const n = this.synthetic_nodes.at(t);
                                try l.append(.{
                                    .kind = if (el.flags & @intFromEnum(Flags.constructor) == 0) .call_signature else .construct_signature,
                                    .data = n.data,
                                    .len = n.len,
                                });
                                continue;
                            }

                            const flags: u20 = @intCast(el.flags);

                            if (el.kind == .index) {
                                const index_type = try this.toTypeNode(el.name);
                                try l.append(.{
                                    .kind = .index_signature,
                                    .data = toBinaryDataPtrRefs(0, index_type),
                                    .len = t,
                                    .flags = flags,
                                });
                                continue;
                            }

                            const copy = try this.toIdentLike(el.name);
                            // std.debug.print("{s}\n", .{getSlice(this.synthetic_nodes.at(copy), u8)});

                            if (el.kind == .method) {
                                const d = getPackedData(this.synthetic_nodes.at(t));
                                try l.append(.{
                                    .kind = .method_signature,
                                    .data = toBinaryDataPtrRefs(copy, d.left),
                                    .len = d.right,
                                    .extra_data = this.synthetic_nodes.at(t).len,
                                    .flags = flags,
                                });
                                continue;
                            }

                            try l.append(.{
                                .kind = .property_signature,
                                .data = toBinaryDataPtrRefs(copy, t),
                                .flags = flags,
                            });
                        }

                        return this.synthetic_nodes.push(.{
                            .kind = .type_literal,
                            .data = @ptrFromInt(l.head),
                        });
                    },
                    .conditional => {
                        // `condition` may set registers during conversion
                        const registers = this.analyzer.type_registers;
                        defer this.analyzer.type_registers = registers;

                        const inner_subject = try this.toTypeNode(k.slot0);

                        const subject = if (k.slot4 == 1)
                            try this.parenthesizeType(inner_subject)
                        else
                            try this.maybeParenthesizeTypeForUnionLike(inner_subject, .conditional_type);

                        const condition = try this.toTypeNode(k.slot1);
                        const when_true = try this.toTypeNode(k.slot2);
                        const when_false = try this.toTypeNode(k.slot3);

                        return this.synthetic_nodes.push(.{
                            .kind = .conditional_type,
                            .data = toBinaryDataPtrRefs(subject, condition),
                            .len = when_true,
                            .extra_data = when_false,
                        });
                    },
                    .parameterized => {
                        var tmp_args = std.AutoArrayHashMap(TypeRef, TypeRef).init(this.analyzer.allocator());
                        defer tmp_args.deinit();

                        const registers = this.analyzer.type_registers;
                        defer this.analyzer.revertTypeArgs(tmp_args, registers);

                        var l = BumpAllocatorList(AstNode).init(&this.synthetic_nodes);
                        for (getSlice2(k, TypeRef)) |p| {
                            const param = this.analyzer.types.at(p);
                            this.analyzer.type_registers[param.slot3] = p;

                            const n = try this.toTypeNode(p);
                            l.appendRef(n);
                        }

                        const t = try this.toTypeNode(k.slot3);
                        const n = this.synthetic_nodes.at(t);
                        switch (n.kind) {
                            .function_type => n.len = l.head,
                            .class_declaration => n.extra_data = l.head,
                            else => {},
                        }

                        return t;
                    },
                    .mapped => {
                        const param = try this.toTypeNode(k.slot0);

                        const reg = this.analyzer.types.at(k.slot0).slot3;
                        const old_value = this.analyzer.type_registers[reg];
                        defer this.analyzer.type_registers[reg] = old_value;

                        this.analyzer.type_registers[reg] = k.slot0;

                        const exp = try this.toTypeNode(k.slot1);
                        const rename = try this.toTypeNode(k.slot2);

                        return this.synthetic_nodes.push(.{
                            .kind = .mapped_type,
                            .data = toBinaryDataPtrRefs(param, exp),
                            .len = rename,
                            .flags = @intCast(k.slot3),
                        });
                    },
                    .keyof => {
                        var inner = try this.toTypeNode(k.slot0);
                        if (this.synthetic_nodes.at(inner).kind == .union_type or this.synthetic_nodes.at(inner).kind == .intersection_type) {
                            const wrapped = try this.parenthesizeType(inner);
                            inner = wrapped;
                        }

                        return this.synthetic_nodes.push(.{
                            .kind = .type_operator,
                            .data = toBinaryDataPtrRefs(@intFromEnum(SyntaxKind.key_of_keyword), inner),
                        });
                    },
                    .predicate => {
                        const ident = try this.copyIdentFromSymbol(k.slot3, k.slot0);
                        const t = try this.toTypeNode(k.slot1);

                        return this.synthetic_nodes.push(.{
                            .kind = .type_predicate,
                            .data = toBinaryDataPtrRefs(ident, t),
                            .len = k.slot4,
                        });
                    },
                    .type_parameter => {
                        const ident = try this.copyIdentFromSymbol(k.slot4, k.slot0);
                        const constraint = if (k.slot1 != 0) try this.toTypeNode(k.slot1) else 0;
                        const inner = try this.synthetic_nodes.push(.{
                            .kind = .type_parameter,
                            .data = toBinaryDataPtrRefs(ident, constraint),
                        });

                        if (!hasTypeFlag(k, .infer_node)) {
                            return inner;
                        }

                        this.analyzer.type_registers[k.slot3] = ty;

                        return this.synthetic_nodes.push(.{
                            .kind = .infer_type,
                            .data = @ptrFromInt(inner),
                        });
                    },
                    else => {
                        std.debug.print("{any} ", .{k.getKind()});
                    },
                }
            } else {
                // TODO: we technically need to handle `readonly []`
                if (ty == @intFromEnum(Kind.empty_tuple)) {
                    return this.synthetic_nodes.push(.{
                        .kind = .tuple_type,
                        .data = null,
                    });
                }

                if (ty == @intFromEnum(Kind.empty_object)) {
                    return this.synthetic_nodes.push(.{
                        .kind = .type_literal,
                        .data = null,
                    });
                }

                if (ty == @intFromEnum(Kind.empty_string)) {
                    return this.synthetic_nodes.push(.{
                        .kind = .string_literal,
                        .data = null,
                        .len = 0,
                        .flags = @intFromEnum(StringFlags.single_quote),
                    });
                }

                const k: SyntaxKind = switch (@as(Kind, @enumFromInt(ty))) {
                    .this => .this_keyword,
                    .true => .true_keyword,
                    .false => .false_keyword,
                    .boolean => .boolean_keyword,
                    .string => .string_keyword,
                    .number => .number_keyword,
                    .symbol => .symbol_keyword,
                    .object => .object_keyword,
                    .undefined => .undefined_keyword,
                    .void => .void_keyword,
                    .never => .never_keyword,
                    .unknown => .unknown_keyword,
                    .any => .any_keyword,
                    .null => .null_keyword,
                    else => {
                        std.debug.print("{}\n", .{ty});
                        return error.TODO;
                    },
                };

                return this.synthetic_nodes.push(.{
                    .kind = k,
                });
            }

            std.debug.print("{}\n", .{ty});

            return error.TODO2;
        }
    };
};
