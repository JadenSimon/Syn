const std = @import("std");
const parser = @import("./parser.zig");
const Lexer = @import("./lexer.zig").Lexer;
const Program = @import("./program.zig").Program;
const AsyncProgramLoader = @import("./program.zig").AsyncProgramLoader;
const getAllocator = @import("./string_immutable.zig").getAllocator;

export fn alloc(size: usize) [*]u8 {
    const b = getAllocator().alloc(u8, size) catch @panic("Failed to allocate");
    return b.ptr;
}

export fn free(ptr: [*]u8, size: usize) void {
    getAllocator().free(ptr[0..size]);
}

export fn clearArena() void {
    @import("./string_immutable.zig").resetWasmArena();
}

export fn transformSyn(source: [*:0]const u8, synth: bool) [*:0]const u8 {
    return _transformSyn(source, synth) catch @panic("Failed to transform");
}

fn _transformSyn(source: [*:0]const u8, synth: bool) ![*:0]const u8 {
    var program = try Program.init(getAllocator(), &.{}, "");
    const len = std.mem.len(source);
    try program.addVirtualFile("example.syn", source[0..len]);

    const id = try program.getFileIdByPath("example.syn");
    var replacements = try program.transformSyn(id, program.getFileData(id).ast.start);
    defer replacements.deinit();

    var opt = parser.PrinterOptions{};
    opt.replacements = &replacements;

    const result = try parser.printWithOptions(program.getFileData(id).ast, opt);
    //defer getAllocator().free(result.contents);
    if (synth) {
        const reparsed = try parser.ParsedFile.createFromBuffer(result.contents, null, false, null);
        const res = try @import("./synth_helper.zig").SynthInstrumenter.transform(&reparsed.ast, &reparsed.binder);
        const str: [:0]const u8 = try getAllocator().dupeZ(u8, res.contents);
        return str;
    }
    const str: [:0]const u8 = try getAllocator().dupeZ(u8, result.contents);
    return str;
}

// -Wl,--initial-memory=16777216 // 16 * 1024 * 1024
// zig build-exe -target wasm32-freestanding-musl -fno-entry --export=transformSyn --export=alloc --export=free --export=clearArena wasm-main.zig -freference-trace --initial-memory=16777216 -femit-bin=compiler.wasm
// zig build-exe -target wasm32-freestanding-musl -fno-entry -OReleaseSmall -fstrip --export=transformSyn --export=alloc --export=free --export=clearArena wasm-main.zig --initial-memory=16777216 -femit-bin=compiler.wasm
