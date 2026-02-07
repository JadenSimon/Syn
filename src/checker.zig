const std = @import("std");
const parser = @import("./parser.zig");
const program = @import("./program.zig");

const Kind = program.Analyzer.Kind;
const TypeRef =  program.Analyzer.TypeRef;
const NodeRef = parser.NodeRef;

const getSlice2 = program.Analyzer.getSlice2;

pub const Checker = struct {
    file: *program.ParsedFileData,    
    analyzer: *program.Analyzer,
    allocator: std.mem.Allocator,

    pub fn init(file: *program.ParsedFileData, allocator: std.mem.Allocator, analyzer: *program.Analyzer) @This() {
        return .{
            .file = file,
            .allocator = allocator,
            .analyzer = analyzer,
        };
    }

    pub fn checkVariableDeclaration(this: *@This(), ref: NodeRef) !TypeRef {
        
    }
};
