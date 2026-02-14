const std = @import("std");
const js_lexer = @import("./lexer.zig");
const ComptimeStringMap = @import("comptime_string_map.zig").ComptimeStringMap;
const strings = @import("./string_immutable.zig");
const getAllocator = @import("./string_immutable.zig").getAllocator;

const is_debug = @import("builtin").mode == .Debug;
const enable_conditional_bindings = true;

// First 512 kinds are reserved for the `typescript` API
pub const SyntaxKind = enum(u10) {
    unknown = 0,
    end_of_file_token = 1,
    single_line_comment_trivia = 2,
    multi_line_comment_trivia = 3,
    new_line_trivia = 4,
    whitespace_trivia = 5,
    shebang_trivia = 6,
    conflict_marker_trivia = 7,
    non_text_file_marker_trivia = 8,
    numeric_literal = 9,
    bigint_literal = 10,
    string_literal = 11,
    jsx_text = 12,
    jsx_text_all_white_spaces = 13,
    regular_expression_literal = 14,
    no_substitution_template_literal = 15,
    template_head = 16,
    template_middle = 17,
    template_tail = 18,
    open_brace_token = 19,
    close_brace_token = 20,
    open_paren_token = 21,
    close_paren_token = 22,
    open_bracket_token = 23,
    close_bracket_token = 24,
    dot_token = 25,
    dot_dot_dot_token = 26,
    semicolon_token = 27,
    comma_token = 28,
    question_dot_token = 29,
    less_than_token = 30,
    less_than_slash_token = 31,
    greater_than_token = 32,
    less_than_equals_token = 33,
    greater_than_equals_token = 34,
    equals_equals_token = 35,
    exclamation_equals_token = 36,
    equals_equals_equals_token = 37,
    exclamation_equals_equals_token = 38,
    equals_greater_than_token = 39,
    plus_token = 40,
    minus_token = 41,
    asterisk_token = 42,
    asterisk_asterisk_token = 43,
    slash_token = 44,
    percent_token = 45,
    plus_plus_token = 46,
    minus_minus_token = 47,
    less_than_less_than_token = 48,
    greater_than_greater_than_token = 49,
    greater_than_greater_than_greater_than_token = 50,
    ampersand_token = 51,
    bar_token = 52,
    caret_token = 53,
    exclamation_token = 54,
    tilde_token = 55,
    ampersand_ampersand_token = 56,
    bar_bar_token = 57,
    question_token = 58,
    colon_token = 59,
    at_token = 60,
    question_question_token = 61,
    backtick_token = 62,
    hash_token = 63,
    equals_token = 64,
    plus_equals_token = 65,
    minus_equals_token = 66,
    asterisk_equals_token = 67,
    asterisk_asterisk_equals_token = 68,
    slash_equals_token = 69,
    percent_equals_token = 70,
    less_than_less_than_equals_token = 71,
    greater_than_greater_than_equals_token = 72,
    greater_than_greater_than_greater_than_equals_token = 73,
    ampersand_equals_token = 74,
    bar_equals_token = 75,
    bar_bar_equals_token = 76,
    ampersand_ampersand_equals_token = 77,
    question_question_equals_token = 78,
    caret_equals_token = 79,
    identifier = 80,
    private_identifier = 81,
    break_keyword = 83,
    case_keyword = 84,
    catch_keyword = 85,
    class_keyword = 86,
    const_keyword = 87,
    continue_keyword = 88,
    debugger_keyword = 89,
    default_keyword = 90,
    delete_keyword = 91,
    do_keyword = 92,
    else_keyword = 93,
    enum_keyword = 94,
    export_keyword = 95,
    extends_keyword = 96,
    false_keyword = 97,
    finally_keyword = 98,
    for_keyword = 99,
    function_keyword = 100,
    if_keyword = 101,
    import_keyword = 102,
    in_keyword = 103,
    instanceof_keyword = 104,
    new_keyword = 105,
    null_keyword = 106,
    return_keyword = 107,
    super_keyword = 108,
    switch_keyword = 109,
    this_keyword = 110,
    throw_keyword = 111,
    true_keyword = 112,
    try_keyword = 113,
    typeof_keyword = 114,
    var_keyword = 115,
    void_keyword = 116,
    while_keyword = 117,
    with_keyword = 118,
    implements_keyword = 119,
    interface_keyword = 120,
    let_keyword = 121,
    package_keyword = 122,
    private_keyword = 123,
    protected_keyword = 124,
    public_keyword = 125,
    static_keyword = 126,
    yield_keyword = 127,
    abstract_keyword = 128,
    accessor_keyword = 129,
    as_keyword = 130,
    asserts_keyword = 131,
    assert_keyword = 132,
    any_keyword = 133,
    async_keyword = 134,
    await_keyword = 135,
    boolean_keyword = 136,
    constructor_keyword = 137,
    declare_keyword = 138,
    get_keyword = 139,
    infer_keyword = 140,
    intrinsic_keyword = 141,
    is_keyword = 142,
    key_of_keyword = 143,
    module_keyword = 144,
    namespace_keyword = 145,
    never_keyword = 146,
    out_keyword = 147,
    readonly_keyword = 148,
    require_keyword = 149,
    number_keyword = 150,
    object_keyword = 151,
    satisfies_keyword = 152,
    set_keyword = 153,
    string_keyword = 154,
    symbol_keyword = 155,
    type_keyword = 156,
    undefined_keyword = 157,
    unique_keyword = 158,
    unknown_keyword = 159,
    using_keyword = 160,
    from_keyword = 161,
    global_keyword = 162,
    bigint_keyword = 163,
    override_keyword = 164,
    of_keyword = 165,
    qualified_name = 166,
    computed_property_name = 167,
    type_parameter = 168,
    parameter = 169,
    decorator = 170,
    property_signature = 171,
    property_declaration = 172,
    method_signature = 173,
    method_declaration = 174,
    class_static_block_declaration = 175,
    constructor = 176,
    get_accessor = 177,
    set_accessor = 178,
    call_signature = 179,
    construct_signature = 180,
    index_signature = 181,
    type_predicate = 182,
    type_reference = 183,
    function_type = 184,
    constructor_type = 185,
    type_query = 186,
    type_literal = 187,
    array_type = 188,
    tuple_type = 189,
    optional_type = 190,
    rest_type = 191,
    union_type = 192,
    intersection_type = 193,
    conditional_type = 194,
    infer_type = 195,
    parenthesized_type = 196,
    this_type = 197,
    type_operator = 198,
    indexed_access_type = 199,
    mapped_type = 200,
    literal_type = 201,
    named_tuple_member = 202,
    template_literal_type = 203,
    template_literal_type_span = 204,
    import_type = 205,
    object_binding_pattern = 206,
    array_binding_pattern = 207,
    binding_element = 208,
    array_literal_expression = 209,
    object_literal_expression = 210,
    property_access_expression = 211,
    element_access_expression = 212,
    call_expression = 213,
    new_expression = 214,
    tagged_template_expression = 215,
    type_assertion_expression = 216,
    parenthesized_expression = 217,
    function_expression = 218,
    arrow_function = 219,
    delete_expression = 220,
    typeof_expression = 221,
    void_expression = 222,
    await_expression = 223,
    prefix_unary_expression = 224,
    postfix_unary_expression = 225,
    binary_expression = 226,
    conditional_expression = 227,
    template_expression = 228,
    yield_expression = 229,
    spread_element = 230,
    class_expression = 231,
    omitted_expression = 232,
    expression_with_type_arguments = 233,
    as_expression = 234,
    non_null_expression = 235,
    meta_property = 236,
    synthetic_expression = 237,
    satisfies_expression = 238,
    template_span = 239,
    semicolon_class_element = 240,
    block = 241,
    empty_statement = 242,
    variable_statement = 243,
    expression_statement = 244,
    if_statement = 245,
    do_statement = 246,
    while_statement = 247,
    for_statement = 248,
    for_in_statement = 249,
    for_of_statement = 250,
    continue_statement = 251,
    break_statement = 252,
    return_statement = 253,
    with_statement = 254,
    switch_statement = 255,
    labeled_statement = 256,
    throw_statement = 257,
    try_statement = 258,
    debugger_statement = 259,
    variable_declaration = 260,
    variable_declaration_list = 261,
    function_declaration = 262,
    class_declaration = 263,
    interface_declaration = 264,
    type_alias_declaration = 265,
    enum_declaration = 266,
    module_declaration = 267,
    module_block = 268,
    case_block = 269,
    namespace_export_declaration = 270,
    import_equals_declaration = 271,
    import_declaration = 272,
    import_clause = 273,
    namespace_import = 274,
    named_imports = 275,
    import_specifier = 276,
    export_assignment = 277,
    export_declaration = 278,
    named_exports = 279,
    namespace_export = 280,
    export_specifier = 281,
    missing_declaration = 282,
    external_module_reference = 283,
    jsx_element = 284,
    jsx_self_closing_element = 285,
    jsx_opening_element = 286,
    jsx_closing_element = 287,
    jsx_fragment = 288,
    jsx_opening_fragment = 289,
    jsx_closing_fragment = 290,
    jsx_attribute = 291,
    jsx_attributes = 292,
    jsx_spread_attribute = 293,
    jsx_expression = 294,
    jsx_namespaced_name = 295,
    case_clause = 296,
    default_clause = 297,
    heritage_clause = 298,
    catch_clause = 299,
    import_attributes = 300,
    import_attribute = 301,
    import_type_assertion_container = 302,
    property_assignment = 303,
    shorthand_property_assignment = 304,
    spread_assignment = 305,
    enum_member = 306,
    source_file = 307,
    bundle = 308,
    js_doc_type_expression = 309,
    js_doc_name_reference = 310,
    js_doc_member_name = 311,
    js_doc_all_type = 312,
    js_doc_unknown_type = 313,
    js_doc_nullable_type = 314,
    js_doc_non_nullable_type = 315,
    js_doc_optional_type = 316,
    js_doc_function_type = 317,
    js_doc_variadic_type = 318,
    js_doc_namepath_type = 319,
    js_doc_comment = 320,
    js_doc_text = 321,
    js_doc_type_literal = 322,
    js_doc_signature = 323,
    js_doc_link = 324,
    js_doc_link_code = 325,
    js_doc_link_plain = 326,
    js_doc_tag = 327,
    js_doc_augments_tag = 328,
    js_doc_implements_tag = 329,
    js_doc_author_tag = 330,
    js_doc_deprecated_tag = 331,
    js_doc_class_tag = 332,
    js_doc_public_tag = 333,
    js_doc_private_tag = 334,
    js_doc_protected_tag = 335,
    js_doc_readonly_tag = 336,
    js_doc_override_tag = 337,
    js_doc_callback_tag = 338,
    js_doc_overload_tag = 339,
    js_doc_enum_tag = 340,
    js_doc_parameter_tag = 341,
    js_doc_return_tag = 342,
    js_doc_this_tag = 343,
    js_doc_type_tag = 344,
    js_doc_template_tag = 345,
    js_doc_typedef_tag = 346,
    js_doc_see_tag = 347,
    js_doc_property_tag = 348,
    js_doc_throws_tag = 349,
    js_doc_satisfies_tag = 350,
    js_doc_import_tag = 351,
    syntax_list = 352,
    not_emitted_statement = 353,
    partially_emitted_expression = 354,
    comma_list_expression = 355,
    synthetic_reference_expression = 356,

    is_expression = 690,
    reify_keyword = 698,
    reify_expression = 699,
    defer_statement = 700,

    // Used to stitch multiple ASTs together
    // Node contains a pointer to `AstData` and the start node
    external_node = 1000,
    verbatim_node = 1001, // Writes text out directly

    start = 1022,
    parse_error = 1023,
};

const Op = struct {
    pub const Level = enum(u6) {
        lowest,
        comma,
        spread,
        yield,
        assign,
        conditional,
        nullish_coalescing,
        logical_or,
        logical_and,
        bitwise_or,
        bitwise_xor,
        bitwise_and,
        equals,
        compare,
        shift,
        add,
        multiply,
        exponentiation,
        prefix,
        postfix,
        new,
        call,
        member,

        pub inline fn lt(self: Level, b: Level) bool {
            return @intFromEnum(self) < @intFromEnum(b);
        }
        pub inline fn gt(self: Level, b: Level) bool {
            return @intFromEnum(self) > @intFromEnum(b);
        }
        pub inline fn gte(self: Level, b: Level) bool {
            return @intFromEnum(self) >= @intFromEnum(b);
        }
        pub inline fn lte(self: Level, b: Level) bool {
            return @intFromEnum(self) <= @intFromEnum(b);
        }
        pub inline fn eql(self: Level, b: Level) bool {
            return @intFromEnum(self) == @intFromEnum(b);
        }
    };
};

pub const TypeKeywords = ComptimeStringMap(SyntaxKind, .{
    .{ "symbol", .symbol_keyword },
    .{ "string", .string_keyword },
    .{ "number", .number_keyword },
    .{ "boolean", .boolean_keyword },
    .{ "bigint", .bigint_keyword },
    .{ "object", .object_keyword },
    .{ "any", .any_keyword },
    .{ "never", .never_keyword },
    .{ "unknown", .unknown_keyword },
    .{ "intrinsic", .intrinsic_keyword },
});

pub const TypeOperators = ComptimeStringMap(SyntaxKind, .{
    .{ "infer", .infer_keyword },
    .{ "keyof", .key_of_keyword },
    .{ "unique", .unique_keyword },
    .{ "readonly", .readonly_keyword },
});



// Flags are per-kind and are used for any binary semantics
// Good examples of this are:
// * Variable declaration keywords (`const`, `let`, `var`, `using`)
// * `export` and `declare` modifiers
// * Class member modifiers
// * Variable decl non-null assertions (`let foo!: string`)
//
// Some of these flags are 1:1 with the `typescript` API for simplicity, but
// perfect compatibility is not the goal. The `typescript` API primarily uses
// node flags to help with binding or type checking whereas we use it to reduce
// the number of nodes parsed.
pub const NodeFlags = enum(u22) {
    none = 0,
    let = 1 << 0,
    @"const" = 1 << 1,
    using = 1 << 2,
    @"async" = 1 << 3,
    await_using = (1 << 2) | (1 << 3),

    @"export" = 1 << 7,
    declare = 1 << 8, // typescript-only, this also means "type only" for import/export
    default_export = (1 << 1) | (1 << 7), // `export` with `const` on class/fn decls means default

    // Member modifiers
    static = 1 << 9,
    public = 1 << 10,
    protected = 1 << 11,
    private = 1 << 12,
    readonly = 1 << 13,
    abstract = 1 << 14,
    override = 1 << 15,

    // For functions
    generator = 1 << 16, // Means "spread element" or "rest arg" in other cases

    // For property/element access and call expressions, this is an optional chain `?.`
    // For param decls or signature decls, this is just an optional field/param `?`
    optional = 1 << 17,

    // For variable decls and property decls, parsed as a `!` before the type
    // This is called "definite assignment assertion" for variable/props
    // Can also be applied to expression nodes when a terminating `!` is parsed
    non_null = 1 << 18,

    // Used by the binder to mark nodes that reference type parameters transitively
    // TODO: implement or delete this
    parameterized = 1 << 19,

    // Negation mapped type modifiers
    minus_readonly = (1 << 9) | (1 << 13),
    minus_optional = (1 << 10) | (1 << 17),

    // upper two bits are reserved
};

pub const StringFlags = enum(u20) {
    single_quote = 1 << 0,
    double_quote = 1 << 1,
    two_byte = 1 << 2,

    // TODO: compute this in the lexer (should be practically free)
    // If set, this literal can be converted to a number
    numeric = 1 << 3,

    external = 1 << 4, // `data` is a pointer

    synthetic = 1 << 6, // Implies escaping is needed
};

const TypeParamFlags = enum(u20) {
    @"const" = 1 << 0,
    variance_in = 1 << 1,
    variance_out = 1 << 2,
};

pub const NodeRef = u32;

pub const AstNode = packed struct {
    kind: SyntaxKind,
    flags: u22 = 0,
    next: NodeRef = 0,
    data: ?*const anyopaque = null, // TODO: make this u64
    len: u32 = 0,
    extra_data: u32 = 0,
    extra_data2: u32 = 0,
    location: u32 = 0,

    pub inline fn hasFlag(this: *const AstNode, flag: NodeFlags) bool {
        return (this.flags & @intFromEnum(flag)) == @intFromEnum(flag);
    }
};

const AstNodeWithTrivia = packed struct {
    kind: SyntaxKind,
    flags: u22 = 0,
    next: NodeRef = 0,
    data: ?*const anyopaque = null, // TODO: make this u64
    len: u32 = 0,
    extra_data: u32 = 0,
    extra_data2: u32 = 0,
    location: u32 = 0,
    full_start: u32 = 0,
    width: u32 = 0,

    pub inline fn hasFlag(this: *const @This(), flag: NodeFlags) bool {
        return (this.flags & @intFromEnum(flag)) == @intFromEnum(flag);
    }

    pub inline fn toAstNode(this: @This()) AstNode {
        // Shouldn't add extra instructions for `ReleaseFast`
        return @bitCast(@as([10]u32, @bitCast(this))[0..8].*);
    }
};

// pub const AstNode = packed struct {
//     kind: SyntaxKind,
//     flags: u22 = 0,
//     slot0: u32 = 0,
//     slot1: u32 = 0,
//     slot2: u32 = 0,
//     slot3: u32 = 0,
//     slot4: u32 = 0,
//     slot5: u32 = 0,
//     slot6: u32 = 0, // start
// };

comptime {
    if (@sizeOf(AstNode) != 32) {
        @compileError("Invalid node size");
    }
}

pub const TripleSlashDirective = struct {
    const Kind = enum {
        unknown,
        lib,
        path,
        types,

        // Very misleading directive. Should be interpretted as "this is a default lib".
        no_default_lib,
    };

    kind: Kind = .unknown,
    value: []const u8 = &.{},
};

pub const AstData = struct {
    start: NodeRef = 0,
    source: []const u8,
    nodes: BumpAllocator(AstNode),
    decorators: NodeMap = .{},
    positions: ?js_lexer.PositionsWriter = null,
    lines: ?js_lexer.LineMap = null,
    triple_slash_directives: []const TripleSlashDirective = &.{},
    source_name: ?[]const u8 = null,

    pub fn deinit(this: @This()) void {
        _ = this; // TODO
    }

    pub inline fn getSliceFromRef(this: *const @This(), ref: NodeRef) []const u8 {
        return getSourceSlice(this.source, this.nodes.at(ref));
    }

    // Does _not_ copy `source`
    pub fn clone(this: *const @This()) !@This() {
        const nodes = try this.nodes.cloneAndPad();
        var cloned: AstData = this.*;
        cloned.nodes = nodes;

        return cloned;
    }
};

pub const ParseResult = struct {
    root: AstNode,
    root_ref: NodeRef,
    data: AstData,
    errors: ?*anyopaque = null,
};

const BinaryExpData = packed struct {
    left: NodeRef,
    right: NodeRef,
};

comptime {
    if (@sizeOf(BinaryExpData) > 8) {
        @compileError("BinaryExpData must fit in 8 bytes");
    }
}

comptime {
    if (@sizeOf(usize) != 8) {
        @compileError("32-bit systems are not supported");
    }
}

// upper 2 bits describes interpretation of subsequent bits
// 00 --- 8 bits for col, 22 bits for line
// 01 --- 12 bits for col, 18 bits for line
// 10 --- 18 bits for col, 12 bits for line
// 11 --- 22 bits for col, 8 bits for line

const max_location: u32 = @intCast(std.math.pow(u64, 2, 32) - 1);

pub inline fn encodeLocation(line: u32, col: u32) u32 {
    // using `col` tends to have better branch prediction on source code
    if (col < 0xFF) {
        return (col << 22) | line;
    } else if (col < 0x3FFF) {
        return (0b01 << 30) | (col << 18) | line;
    } else if (col < 0x3FFFF) {
        return (0b10 << 30) | (col << 12) | line;
    } else if (col < 0x3FFFFF) {
        return (0b11 << 30) | (col << 8) | line;
    }

    return max_location;
}

pub inline fn decodeLocation(loc: u32) struct { line: u32, col: u32 } {
    // if (loc == max_location) {
    //     @panic("max loc");
    // }

    return switch ((loc >> 30)) {
        0b00 => .{
            .line = loc & 0x3FFFFF,
            .col = (loc >> 22) & 0xFF,
        },
        0b01 => .{
            .line = loc & 0x3FFFF,
            .col = (loc >> 18) & 0x3FF,
        },
        0b10 => .{
            .line = loc & 0xFFF,
            .col = (loc >> 12) & 0x3FFFF,
        },
        0b11 => .{
            .line = loc & 0xFF,
            .col = (loc >> 8) & 0x3FFFFF,
        },
        else => unreachable,
    };
}

// SyntaxError: In strict mode code, functions can only be declared at top level or inside a block.
// if (true) function foo() {}

const NodeMap = std.AutoArrayHashMapUnmanaged(NodeRef, NodeRef);

pub fn BumpAllocator(comptime T: type) type {
    comptime {
        if (std.mem.page_size % @sizeOf(T) != 0) {
            @compileLog(@sizeOf(T));
            @compileError("Size of type should evenly divide the page size");
        }
    }

    return struct {
        pub const items_per_page = @divExact(std.mem.page_size, @sizeOf(T));

        local_count: u16 = 0,
        pages: std.ArrayList([]T),
        page_allocator: std.mem.Allocator,

        pub fn init(allocator: std.mem.Allocator, page_allocator: std.mem.Allocator) @This() {
            return .{
                .pages = std.ArrayList([]T).init(allocator),
                .page_allocator = page_allocator,
            };
        }

        pub fn deinit(this: *@This()) void {
            // TODO
            _ = this;
        }

        fn addUnmanagedItems(this: *@This(), buf: []T) !void {
            var b = buf;
            if (this.local_count != items_per_page and this.pages.items.len != 0) {
                var p = this.pages.items[this.pages.items.len - 1];
                const rem: usize = @min(items_per_page - this.local_count, b.len);
                for (0..rem) |i| {
                    p[this.local_count + i] = buf[i];
                }

                b = b[rem..];
                this.local_count += @intCast(rem);
            }

            if (b.len == 0) {
                return;
            }

            var numPages = b.len / items_per_page;
            if (@rem(b.len, items_per_page) != 0) {
                numPages += 1;
            }

            // TODO: mark these pages so we don't try to free them
            var i: usize = 0;
            while (i < numPages) {
                const r = @min(((i + 1) * items_per_page), b.len);
                const page = b[(i * items_per_page)..r];
                try this.pages.append(page);
                i += 1;
            }

            // Move the count so we don't write into these "external" pages
            try this.addPage();
            this.local_count = 0;
        }

        pub fn clone(this: *const @This()) !@This() {
            return .{
                .local_count = this.local_count,
                .page_allocator = this.page_allocator,
                .pages = try this.pages.clone(),
            };
        }

        // Pad so we don't mutate an existing page
        pub fn cloneAndPad(this: *const @This()) !@This() {
            var c = try this.clone();
            if (c.local_count > 0) {
                while (c.local_count < items_per_page) {
                    c.local_count += 1;
                }

                try c.addPage();
                c.local_count = 0;
            }

            return c;
        }

        pub fn cloneWithItems(this: *const @This(), items: []T) !@This() {
            var c = try this.clone();
            try c.addUnmanagedItems(items);

            return c;
        }

        inline fn addPage(this: *@This()) !void {
            std.debug.assert((this.count() / items_per_page) == this.pages.items.len);
            const page = try this.page_allocator.alloc(T, items_per_page);
            try this.pages.append(page);
        }

        pub fn preAlloc(this: *@This()) !void {
            if (this.pages.items.len != 0) {
                return;
            }

            try this.pages.ensureTotalCapacity(4);
            try this.addPage();
        }

        pub inline fn prefetch(this: *const @This(), index: u32) void {
            @prefetch(this.at(index), .{});
        }

        pub inline fn push(this: *@This(), value: T) !u32 {
            std.debug.assert(this.local_count < items_per_page);

            const ret = this.count();
            var p = this.pages.items[this.pages.items.len - 1];
            p[this.local_count] = value;
            this.local_count += 1;

            // we're allocating ahead of time for fewer data deps, the downside
            // is we sometimes allocate a page we don't need
            if (this.local_count == items_per_page) {
                try this.addPage();
                this.local_count = 0;
            }

            return ret;
        }

        pub fn alloc(this: *@This()) !*T {
            std.debug.assert(this.local_count < items_per_page);

            var p = this.pages.items[this.pages.items.len - 1];
            const ptr = &p[this.local_count];
            this.local_count += 1;

            if (this.local_count == items_per_page) {
                try this.addPage();
                this.local_count = 0;
            }

            return ptr;
        }

        pub inline fn at(this: *const @This(), index: usize) *T {
            std.debug.assert(index < this.count());

            const page = index / items_per_page;
            const offset = @rem(index, items_per_page);

            return &this.pages.items[page][offset];
        }

        pub inline fn count(this: *const @This()) u32 {
            if (this.pages.items.len == 0) return 0;
            return (@as(u32, @truncate(this.pages.items.len - 1)) * items_per_page) + this.local_count;
        }
    };
}

pub fn BumpAllocatorList(comptime T: type) type {
    return struct {
        head: NodeRef = 0,
        prev: NodeRef = 0,

        allocator: *BumpAllocator(T),

        pub fn init(alloctor: *BumpAllocator(T)) @This() {
            return .{ .allocator = alloctor };
        }

        pub fn append(this: *@This(), val: T) !void {
            const ref = try this.allocator.push(val);

            return this.appendRef(ref);
        }

        pub inline fn appendRef(this: *@This(), ref: NodeRef) void {
            if (comptime is_debug) {
                var x = this.head;
                while (x != 0) {
                    if (x == ref) @panic("Recursive appendRef");
                    x = this.allocator.at(x).next;
                }
            }

            if (this.prev != 0) {
                this.allocator.at(this.prev).next = ref;
            } else {
                this.head = ref;
            }

            this.prev = ref;
        }

        pub fn reset(this: *@This()) void {
            this.head = 0;
            this.prev = 0;
        }
    };
}

pub const NodeList = BumpAllocatorList(AstNode);

const ParserContextState = enum {
    none,
    class,
    object,
    interface,

    jsx_element,
    jsx_children,
};

pub inline fn toBinaryDataPtrRefs(left: NodeRef, right: NodeRef) *anyopaque {
    return @ptrFromInt((@as(u64, right) << 32) | left);
}

pub inline fn toBinaryDataPtrRefsMaybeNull(left: NodeRef, right: NodeRef) ?*anyopaque {
    if (left == 0 and right == 0) return null;
    return @ptrFromInt((@as(u64, right) << 32) | left);
}

inline fn toIdentNode(ident: []const u8) AstNode {
    return .{
        .kind = .identifier,
        .data = ident.ptr,
        .len = @intCast(ident.len),
        .extra_data2 = @truncate(std.hash.Wyhash.hash(0, ident)),
    };
}

inline fn toIdentNode2(ident: []const u8) AstNodeWithTrivia {
    return .{
        .kind = .identifier,
        .data = ident.ptr,
        .len = @intCast(ident.len),
        .extra_data2 = @truncate(std.hash.Wyhash.hash(0, ident)),
    };
}

pub const NodeSymbolHash = u32; // may change to u42 or u52 later
pub inline fn getHashFromNode(node: *const AstNode) NodeSymbolHash {
    std.debug.assert(node.extra_data2 != 0);

    return node.extra_data2;
}

inline fn getHashFromModuleNode(node: *const AstNode) !NodeSymbolHash {
    if (node.kind == .string_literal) {
        return @truncate(std.hash.Wyhash.hash(1, getSlice(node, u8)));
    }
    return getHashFromNode(node);
}

pub const ImportListener = struct {
    data: *anyopaque,
    cb: *const fn (data: *anyopaque, directives: []const TripleSlashDirective, modules: []const []const u8) anyerror!void,

    pub fn call(this: *@This(), directives: []const TripleSlashDirective, modules: []const []const u8) anyerror!void {
        return this.cb(this.data, directives, modules);
    }
};

const ParserOptions = packed struct {
    allow_jsx: bool = false,
    is_syn: bool = false,
};

fn Parser_(comptime skip_trivia: bool) type {
    const is_declaration_file = true;
    // const is_synx_file = true;

    const AstNode_ = AstNodeWithTrivia;

    return struct {
        const ParserType = @This();
        const NodeList_ = struct {
            head: NodeRef = 0,
            prev: NodeRef = 0,

            parser: *ParserType,

            pub fn init(p: *ParserType) @This() {
                return .{ .parser = p };
            }

            pub inline fn append(this: *@This(), val: AstNode_) !void {
                const ref = try this.parser.node_allocator.push(val.toAstNode());

                if (!comptime skip_trivia) {
                    try this.parser.positions.append(val.full_start, val.width);
                }

                return this.appendRef(ref);
            }

            pub inline fn appendRef(this: *@This(), ref: NodeRef) void {
                if (this.prev != 0) {
                    this.parser.node_allocator.at(this.prev).next = ref;
                } else {
                    this.head = ref;
                }

                this.prev = ref;
            }

            pub fn reset(this: *@This()) void {
                this.head = 0;
                this.prev = 0;
            }
        };

        lexer: js_lexer.Lexer,
        node_allocator: BumpAllocator(AstNode),
        positions: js_lexer.PositionsWriter,

        context_state: ParserContextState = .none,

        decorators: NodeMap,
        options: ParserOptions = .{},
        import_listener: ?*ImportListener = null,

        pub fn init(lexer: js_lexer.Lexer) @This() {
            var node_allocator = BumpAllocator(AstNode).init(lexer.allocator, std.heap.page_allocator);
            node_allocator.preAlloc() catch unreachable;

            var this = @This(){
                .lexer = lexer,
                .decorators = NodeMap{},
                .node_allocator = node_allocator,
                .positions = js_lexer.PositionsWriter.init(),
            };

            if (lexer.source.name) |x| {
                if (strings.endsWithComptime(x, ".syn")) {
                    this.options.is_syn = true;
                    this.options.allow_jsx = true;
                }
            }

            return this;
        }

        inline fn getLocation(this: *const @This()) u32 {
            return encodeLocation(this.lexer.line_map.count, @as(u32, @intCast(this.lexer.start - this.lexer.last_line)));
        }

        inline fn pushNode(this: *@This(), n: AstNode_) !NodeRef {
            const r = try this.node_allocator.push(n.toAstNode());
            if (!comptime skip_trivia) {
                try this.positions.append(n.full_start, n.width);
            }
            return r;
        }

        inline fn next(this: *@This()) !void {
            try switch (this.context_state) {
                .none => this.lexer.next(),
                .class => this.lexer.nextInClassScope(),
                .interface => this.lexer.nextInInterfaceScope(),
                else => unreachable,
            };
        }

        inline fn expect(this: *@This(), comptime token: js_lexer.T) !void {
            try switch (this.context_state) {
                .none => this.lexer.expect(token),
                .class => this.lexer.expectInClassScope(token),
                .interface => this.lexer.expectInInterfaceScope(token),
                else => unreachable,
            };
        }

        inline fn getFullWidth(this: *const @This()) u32 {
            return @as(u32, @intCast(this.lexer.end)) - this.lexer.full_start;
        }

        inline fn toIdentNodeWithLocation(ident: []const u8, location: u32, full_start: u32, width: u32) AstNode_ {
            return .{
                .kind = .identifier,
                .data = ident.ptr,
                // .data = @ptrFromInt(offset),
                .len = @intCast(ident.len),
                .location = location,
                // extra_data is used by the binder to place symbol refs
                .extra_data2 = @truncate(std.hash.Wyhash.hash(0, ident)),
                .full_start = full_start,
                .width = width,
            };
        }

        inline fn parseIdentifierNode(this: *@This()) !AstNode_ {
            const n = toIdentNodeWithLocation(this.lexer.identifier, this.getLocation(), this.lexer.full_start, this.getFullWidth());
            try this.next();

            return n;
        }

        inline fn parseIdentifier(this: *@This()) !NodeRef {
            return this.pushNode(try this.parseIdentifierNode());
        }

        fn parsePrivateIdentifier(this: *@This()) !NodeRef {
            const index = try this.pushNode(.{
                .kind = .private_identifier,
                .data = this.lexer.identifier.ptr,
                .len = @intCast(this.lexer.identifier.len),
                .location = this.getLocation(),
                .extra_data2 = @truncate(std.hash.Wyhash.hash(0, this.lexer.identifier)),
            });

            try this.lexer.next();

            return index;
        }

        fn parseRemainingTypeNode(this: *@This(), node: AstNode_, level: Op.Level) !AstNode_ {
            var left = node;

            while (true) {
                switch (this.lexer.token) {
                    .t_bar => {
                        if (level.gte(.bitwise_or)) {
                            return left;
                        }

                        try this.lexer.next();
                        const data = try this.toBinaryDataPtr(left, try this.parseTypeNodeWithLevel(.bitwise_or));
                        left = .{
                            .kind = .union_type,
                            .data = data,
                        };
                    },
                    .t_ampersand => {
                        if (level.gte(.bitwise_and)) {
                            return left;
                        }

                        try this.lexer.next();
                        // Referencing `left` within the assignment expression seems to cause problems w/ compiled Zig code
                        const data = try this.toBinaryDataPtr(left, try this.parseTypeNodeWithLevel(.bitwise_and));
                        left = .{
                            .kind = .intersection_type,
                            .data = data,
                        };
                    },
                    .t_extends => {
                        try this.lexer.next();

                        const check_type = try this.pushNode(left);
                        const extends_type = try this.parseType();

                        try this.lexer.expect(.t_question);
                        const when_true = try this.parseType();

                        try this.lexer.expect(.t_colon);
                        const when_false = try this.parseType();

                        left = .{
                            .kind = .conditional_type,
                            .data = toBinaryDataPtrRefs(check_type, extends_type),
                            .len = when_true,
                            .extra_data = when_false,
                        };
                    },
                    .t_dot => {
                        try this.lexer.nextNoKeywords();
                        const lhs = try this.pushNode(left);
                        const rhs = try this.parseIdentifier();

                        left = .{
                            .kind = .qualified_name,
                            .data = toBinaryDataPtrRefs(lhs, rhs),
                        };
                    },
                    .t_open_bracket => {
                        if (this.lexer.has_newline_before) {
                            // foo: number
                            // [bar]: number
                            return left;
                        }

                        try this.lexer.next();

                        if (this.lexer.token == .t_close_bracket) {
                            try this.next();
                            const ref = try this.pushNode(left);

                            left = .{
                                .kind = .array_type,
                                .data = @ptrFromInt(ref),
                            };
                            continue;
                        }

                        const lhs = try this.pushNode(left);
                        const rhs = try this.parseType();
                        try this.lexer.expect(.t_close_bracket);
                        // Indexed access type
                        left = .{
                            .kind = .indexed_access_type,
                            .data = toBinaryDataPtrRefs(lhs, rhs),
                        };
                    },
                    .t_less_than => {
                        if (this.lexer.has_newline_before or level.gte(.member)) return left;

                        const name = try this.pushNode(left);
                        const typeArguments = try this.parseTypeArgs();

                        left = .{
                            .kind = .type_reference,
                            .data = toBinaryDataPtrRefs(name, typeArguments),
                        };
                    },
                    else => return left,
                }
            }
        }

        fn parseTypeArrowFn(this: *@This()) !AstNode_ {
            var typeParameters: NodeRef = 0;
            if (this.lexer.token == .t_less_than) {
                typeParameters = try this.parseTypeParams();
            }

            const parameters = try this.parseParameters();

            try this.lexer.expect(.t_equals_greater_than);

            const return_type = try this.parseType();

            return .{
                .kind = .function_type,
                .data = toBinaryDataPtrRefs(parameters, return_type),
                .len = typeParameters,
            };
        }

        fn parsePropertySignature(this: *@This(), name: NodeRef, baseFlags: u22) !AstNode_ {
            var flags = baseFlags;
            if (this.lexer.token == .t_question) {
                try this.lexer.next();
                flags |= @intFromEnum(NodeFlags.optional);
            }

            var ty: NodeRef = 0;
            if (this.lexer.token == .t_colon) {
                try this.lexer.next();
                ty = try this.parseType();
            }

            return .{
                .kind = .property_signature,
                .data = toBinaryDataPtrRefs(name, ty),
                .flags = flags,
            };
        }

        fn finishMappedType(this: *@This(), exp: AstNode_, as_clause: NodeRef, flags_init: u22) !AstNode_ {
            var flags = flags_init;
            const d: BinaryExpData = @bitCast(@intFromPtr(exp.data orelse return error.MissingData));

            const parameter = try this.pushNode(.{
                .kind = .type_parameter,
                .data = toBinaryDataPtrRefs(d.left, d.right),
                // .len = as_clause,
            });

            if (this.lexer.token == .t_minus) {
                try this.lexer.next();
                try this.lexer.expect(.t_question);
                flags |= @intFromEnum(NodeFlags.minus_optional);
            } else if (this.lexer.token == .t_plus) {
                try this.lexer.next();
                try this.lexer.expect(.t_question);
                flags |= @intFromEnum(NodeFlags.optional);
            } else if (this.lexer.token == .t_question) {
                try this.lexer.next();
                flags |= @intFromEnum(NodeFlags.optional);
            }

            try this.lexer.expect(.t_colon);

            const ty = try this.parseType();
            if (this.lexer.token == .t_semicolon) try this.lexer.next();

            try this.lexer.expect(.t_close_brace);

            return .{
                .kind = .mapped_type,
                .data = toBinaryDataPtrRefs(parameter, ty),
                .len = as_clause,
                .flags = flags,
            };
        }

        fn parseMappedOrLiteralType(this: *@This()) !AstNode_ {
            const prev_state = this.context_state;
            this.context_state = .interface;
            defer this.context_state = prev_state;

            try this.expect(.t_open_brace);

            var members = NodeList_.init(this);

            while (this.lexer.token != .t_close_brace) {
                var flags: u22 = 0;

                if (this.lexer.token == .t_new) {
                    try this.lexer.next();

                    const typeParameters = if (this.lexer.token == .t_less_than) try this.parseTypeParams() else 0;
                    const parameters = try this.parseParameters();

                    var return_type: NodeRef = 0;
                    if (this.lexer.token == .t_colon) {
                        try this.lexer.next();
                        return_type = try this.parseType();
                    }

                    try members.append(.{
                        .kind = .construct_signature,
                        .data = toBinaryDataPtrRefs(typeParameters, parameters),
                        .len = return_type,
                    });

                    if (this.lexer.token == .t_comma) {
                        try this.next();
                    } else if (this.lexer.token == .t_semicolon) {
                        try this.next();
                    }
                    continue;
                }

                if (this.lexer.token == .t_open_paren or this.lexer.token == .t_less_than) {
                    try members.append(try this.parseCallSignature(false));
                    if (this.lexer.token == .t_comma) {
                        try this.next();
                    } else if (this.lexer.token == .t_semicolon) {
                        try this.next();
                    }
                    continue;
                }

                // Most likely `-readonly` for a mapped type
                if (this.lexer.token == .t_minus) {
                    try this.lexer.nextInInterfaceScopeModifier();
                    try this.expect(.t_readonly);
                    flags |= @intFromEnum(NodeFlags.minus_readonly);
                } else if (this.lexer.token == .t_plus) {
                    try this.lexer.nextInInterfaceScopeModifier();
                    try this.expect(.t_readonly);
                    flags |= @intFromEnum(NodeFlags.readonly);
                } else if (this.lexer.token == .t_readonly) {
                    // FIXME: this can be an ident
                    try this.next();
                    flags |= @intFromEnum(NodeFlags.readonly);
                }

                // Check for mapped type
                if (members.head == 0 and this.lexer.token == .t_open_bracket) {
                    try this.lexer.next();
                    const exp = blk: {
                        if (this.lexer.token != .t_identifier) {
                            break :blk try this.parseExpressionWithLevel(.lowest);
                        }

                        const ident = try this.parseIdentifierNode();
                        if (this.lexer.token != .t_in) {
                            // Index signature
                            if (this.lexer.token == .t_colon) {
                                const name = try this.pushNode(ident);
                                try members.append(try this.finishIndexSignature(name, flags));

                                if (this.lexer.token == .t_comma) {
                                    try this.lexer.next();
                                } else if (this.lexer.token == .t_semicolon) {
                                    try this.lexer.next();
                                }

                                continue;
                            }
                            break :blk try this.parseRemainingExpression(.lowest, ident);
                        }

                        try this.lexer.next();
                        const right = try this.parseTypeNode();

                        break :blk AstNode_{
                            .kind = .binary_expression,
                            .data = try this.toBinaryDataPtr(ident, right),
                            .len = @intFromEnum(SyntaxKind.in_keyword),
                        };
                    };

                    // Definitely a mapped type now (maybe this can be broken still?)
                    if (this.lexer.isContextualKeyword("as")) {
                        try this.lexer.next();
                        const as_clause = try this.parseType();
                        try this.lexer.expect(.t_close_bracket);

                        return this.finishMappedType(exp, as_clause, flags);
                    }

                    try this.lexer.expect(.t_close_bracket);

                    if (exp.kind == .binary_expression and exp.len == @intFromEnum(SyntaxKind.in_keyword) and this.lexer.token != .t_open_paren) {
                        return this.finishMappedType(exp, 0, flags);
                    }

                    // Parse as a prop signature
                    const name = try this.pushNode(.{
                        .kind = .computed_property_name,
                        .data = @ptrFromInt(try this.pushNode(exp)),
                    });
                    const n = try this.parsePropertySignature(name, flags);
                    try members.append(n);

                    if (this.lexer.token == .t_comma) {
                        try this.lexer.next();
                    } else if (this.lexer.token == .t_semicolon) {
                        try this.lexer.next();
                    }

                    continue;
                }

                const name = try this.parseMemberName();

                if (this.lexer.token == .t_question) {
                    try this.lexer.next();
                    const n = try this.parsePropertySignature(name, flags | @intFromEnum(NodeFlags.optional));

                    try members.append(n);
                } else if (this.lexer.token == .t_colon) {
                    const n = try this.parsePropertySignature(name, flags);

                    try members.append(n);
                } else if (this.lexer.token == .t_open_paren or this.lexer.token == .t_less_than) {
                    // Method signature

                    var typeParameters: NodeRef = 0;
                    if (this.lexer.token == .t_less_than) {
                        typeParameters = try this.parseTypeParams();
                    }
                    const parameters = try this.parseParameters();

                    if (this.lexer.token == .t_question) {
                        try this.lexer.next();
                        flags |= @intFromEnum(NodeFlags.optional);
                    }

                    // You can technically omit the return type
                    var return_type: NodeRef = 0;
                    if (this.lexer.token == .t_colon) {
                        try this.lexer.next();
                        return_type = try this.parseType();
                    }

                    try members.append(.{
                        .kind = .method_signature,
                        .data = toBinaryDataPtrRefs(name, parameters),
                        .len = return_type,
                        .extra_data = typeParameters,
                        .flags = flags,
                    });
                }

                if (this.lexer.token == .t_comma) {
                    try this.next();
                } else if (this.lexer.token == .t_semicolon) {
                    try this.next();
                }
            }

            // Reset state early
            this.context_state = prev_state;
            try this.expect(.t_close_brace);

            return .{
                .kind = .type_literal,
                .data = @ptrFromInt(members.head),
            };
        }

        fn parseTupleType(this: *@This()) !AstNode_ {
            try this.lexer.expect(.t_open_bracket);

            var list = NodeList_.init(this);
            while (this.lexer.token != .t_close_bracket) {
                var is_rest_type = false;
                if (this.lexer.token == .t_dot_dot_dot) {
                    is_rest_type = true;
                    try this.lexer.next();
                }

                const addNode = struct {
                    pub fn f(parser: *Parser, l: *NodeList_, n: AstNode_, is_rest: bool, is_optional: bool) !void {
                        if (!is_rest and !is_optional) return l.append(n);

                        const ref = try parser.pushNode(n);
                        if (is_optional) {
                            // you can't have both
                            if (is_rest) {
                                return error.SyntaxError;
                            }

                            return l.append(.{
                                .kind = .optional_type,
                                .data = @ptrFromInt(ref),
                            });
                        }

                        return l.append(.{
                            .kind = .rest_type,
                            .data = @ptrFromInt(ref),
                        });
                    }
                }.f;

                if (try this.isNamedTupleElement()) {
                    const node = try this.parseIdentifierNode();
                    var flags: u22 = 0;
                    if (this.lexer.token == .t_question) {
                        try this.lexer.next();
                        flags = @intFromEnum(NodeFlags.optional);
                    }

                    try this.lexer.next();

                    const left = try this.pushNode(node);
                    const right = try this.parseType();

                    if (is_rest_type) flags |= @intFromEnum(NodeFlags.generator);

                    try list.append(.{
                        .kind = .named_tuple_member,
                        .data = toBinaryDataPtrRefs(left, right),
                        .flags = flags,
                    });
                } else {
                    const node = try this.parseTypeNode();
                    if (this.lexer.token == .t_question) {
                        try this.lexer.next();
                        try addNode(this, &list, node, is_rest_type, true);
                    } else {
                        try addNode(this, &list, node, is_rest_type, false);
                    }
                }

                if (this.lexer.token == .t_close_bracket) break;
                try this.lexer.expect(.t_comma);
            }

            try this.lexer.expect(.t_close_bracket);

            return .{
                .kind = .tuple_type,
                .data = @ptrFromInt(list.head),
            };
        }

        fn parsePredicate(this: *@This(), ident: AstNode_, has_asserts: bool) !AstNode_ {
            try this.lexer.next();

            const n = try this.pushNode(ident);
            const rhs = try this.parseType();

            return .{
                .kind = .type_predicate,
                .data = toBinaryDataPtrRefs(n, rhs),
                .len = if (has_asserts) 1 else 0,
            };
        }

        fn parseTypeNodeBase(this: *@This()) !AstNode_ {
            // Skip leading `&` and |` which are used in multiline type defs
            if (this.lexer.token == .t_bar or this.lexer.token == .t_ampersand) {
                try this.lexer.next();
            }

            switch (this.lexer.token) {
                .t_const => {
                    try this.next();
                    return .{ .kind = .const_keyword };
                },
                .t_async => {
                    try this.next();
                    return .{ .kind = .async_keyword };
                },
                .t_void => {
                    try this.next();
                    return .{ .kind = .void_keyword };
                },
                // .len = 1 means this is a type keyword (used for true/false)
                .t_true => {
                    try this.next();
                    return .{ .kind = .true_keyword, .len = 1 };
                },
                .t_false => {
                    try this.next();
                    return .{ .kind = .false_keyword, .len = 1 };
                },
                .t_null => {
                    try this.next();
                    return .{ .kind = .null_keyword };
                },
                .t_undefined => {
                    try this.next();
                    return .{ .kind = .undefined_keyword };
                },
                .t_this => {
                    try this.next();

                    if (this.lexer.isContextualKeyword("is")) {
                        return this.parsePredicate(.{ .kind = .this_keyword }, false);
                    }

                    return .{ .kind = .this_keyword };
                },
                .t_reify => {
                    try this.next();
                    const rhs = try this.pushNode(
                        try this.parseTypeNodeWithLevel(.bitwise_or),
                    );

                    return .{
                        .kind = .type_operator,
                        .data = toBinaryDataPtrRefs(@intFromEnum(SyntaxKind.reify_keyword), rhs),
                    };
                },
                .t_identifier => {
                    if (TypeKeywords.get(this.lexer.identifier)) |k| {
                        try this.next();

                        if (this.lexer.isContextualKeyword("is")) {
                            return this.parsePredicate(.{ .kind = k }, false);
                        }

                        return .{ .kind = k };
                    }

                    if (TypeOperators.get(this.lexer.identifier)) |k| {
                        try this.next();

                        switch (k) {
                            .infer_keyword => {
                                const rhs = try this.parseOneTypeParam();

                                return .{
                                    .kind = .infer_type,
                                    .data = @ptrFromInt(rhs),
                                };
                            },
                            .key_of_keyword, .unique_keyword, .readonly_keyword => {
                                const rhs = try this.pushNode(
                                    try this.parseTypeNodeWithLevel(.bitwise_or),
                                );

                                return .{
                                    .kind = .type_operator,
                                    .data = toBinaryDataPtrRefs(@intFromEnum(k), rhs),
                                };
                            },
                            else => unreachable,
                        }
                    }

                    // TODO: `asserts` and `is` should only be checked in the `return_type` slot
                    if (this.lexer.isContextualKeyword("asserts")) {
                        try this.lexer.next();
                        const ident = try this.parseIdentifierNode();
                        if (this.lexer.isContextualKeyword("is")) {
                            return this.parsePredicate(ident, true);
                        }

                        return .{
                            .kind = .type_predicate,
                            .data = toBinaryDataPtrRefs(try this.pushNode(ident), 0),
                            .len = 1, // has_asserts
                        };
                    }

                    var ident = try this.parseIdentifierNode();
                    ident.flags = 1 << 19; // Marks the node as a "type" node

                    if (this.lexer.isContextualKeyword("is")) {
                        return this.parsePredicate(ident, false);
                    }

                    if (this.lexer.token == .t_new) {
                        const s = getSlice(&ident.toAstNode(), u8);
                        if (strings.eqlComptime(s, "abstract")) {
                            try this.lexer.next();

                            var f = try this.parseTypeArrowFn();
                            f.kind = .constructor_type;
                            f.flags |= @intFromEnum(NodeFlags.abstract);

                            return f;
                        }
                    }

                    return ident;
                },
                .t_typeof => {
                    try this.lexer.next();
                    const val = try this.pushNode(try this.parseExpressionWithLevel(.shift));
                    if (this.lexer.token == .t_less_than) {
                        const typeArgs = try this.parseTypeArgs();

                        return .{
                            .kind = .type_query,
                            .data = @ptrFromInt(val),
                            .len = typeArgs,
                        };
                    }

                    return .{
                        .kind = .type_query,
                        .data = @ptrFromInt(val),
                    };
                },
                .t_minus => {
                    try this.lexer.next();
                    const val = try this.parseNumericLiteralBase(true);

                    return .{
                        .kind = .literal_type,
                        .data = @ptrFromInt(try this.pushNode(val)),
                    };
                },
                .t_numeric_literal => {
                    const val = try this.parseNumericLiteral();

                    return .{
                        .kind = .literal_type,
                        .data = @ptrFromInt(try this.pushNode(val)),
                    };
                },
                .t_string_literal, .t_no_substitution_template_literal => {
                    const val = try this.parseStringLiteralLike();

                    return .{
                        .kind = .literal_type,
                        .data = @ptrFromInt(try this.pushNode(val)),
                    };
                },
                .t_template_head => {
                    const parts = try this.parseTemplateParts(true);

                    return .{
                        .kind = .template_literal_type,
                        .data = @ptrFromInt(parts),
                    };
                },
                .t_new => {
                    try this.lexer.next();

                    var n = try this.parseTypeArrowFn();
                    n.kind = .constructor_type;

                    return n;
                },
                // Tuple type
                .t_open_bracket => return this.parseTupleType(),
                // Could be type literal or mapped type
                .t_open_brace => {
                    if (try this.isMappedType()) {
                        return this.parseMappedOrLiteralType();
                    }

                    const members = try this.parseTypeMembers();

                    return .{
                        .kind = .type_literal,
                        .data = @ptrFromInt(members),
                    };
                },
                // Could be parenthesized type or arrow fn
                .t_open_paren => {
                    if (try this.isArrowFn(.lowest, true)) {
                        return this.parseTypeArrowFn();
                    }

                    try this.lexer.next();
                    const exp = try this.parseType();
                    try this.lexer.expect(.t_close_paren);

                    return .{
                        .kind = .parenthesized_type,
                        .data = @ptrFromInt(exp),
                    };
                },
                // Must be an arrow fn
                .t_less_than => return this.parseTypeArrowFn(),
                .t_import => {
                    try this.lexer.next();
                    try this.lexer.expect(.t_open_paren);

                    const spec = try this.parseStringLiteralLikeKind(.string_literal);
                    const spec_ref = try this.pushNode(spec);

                    try this.lexer.expect(.t_close_paren);

                    if (this.lexer.token != .t_dot) {
                        // Invalid type
                        return .{
                            .kind = .import_type,
                            .data = toBinaryDataPtrRefs(spec_ref, 0),
                        };
                    }

                    try this.lexer.next();

                    // tsc seems to only parse idents and qualified names here but we'll just keep it simple
                    const qualifier = try this.pushNode(try this.parseTypeNodeWithLevel(.member));

                    var type_args: NodeRef = 0;
                    if (!this.lexer.has_newline_before and this.lexer.token == .t_less_than) {
                        type_args = try this.parseTypeArgs();
                    }

                    return .{
                        .kind = .import_type,
                        .data = toBinaryDataPtrRefs(spec_ref, qualifier),
                        .len = type_args,
                    };
                },
                else => {},
            }

            return this.emitParseError("");
        }

        fn parseTypeNodeWithLevel(this: *Parser, level: Op.Level) anyerror!AstNode_ {
            const exp = try this.parseTypeNodeBase();

            return this.parseRemainingTypeNode(exp, level);
        }

        inline fn parseTypeNode(this: *Parser) !AstNode_ {
            return this.parseTypeNodeWithLevel(.lowest);
        }

        // Arrow functions with a single type param are ambiguous with tags in JSX
        // A trailing comma or a constraint on the type param means it's definitely an arrow fn
        fn isTSArrowFnJSX(this: *@This()) !bool {
            var oldLexer = std.mem.toBytes(this.lexer);
            defer this.lexer = std.mem.bytesToValue(@TypeOf(this.lexer), &oldLexer);

            try this.lexer.next();
            if (this.lexer.token == .t_const) {
                try this.lexer.next();
            }
            if (this.lexer.token == .t_identifier) {
                const ident = this.lexer.identifier;

                // Syn does not allow JSX components with individual capital letters
                if (ident.len == 1 and this.options.is_syn) {
                    switch (ident[0]) {
                        'A'...'Z' => return true,
                        else => {},
                    }
                }

                try this.lexer.next();
                if (this.lexer.token == .t_comma or this.lexer.token == .t_equals) {
                    return true;
                } else if (this.lexer.token == .t_extends) {
                    try this.lexer.next();
                    return this.lexer.token != .t_equals and this.lexer.token != .t_greater_than;
                }
            }

            return false;
        }

        fn shouldFollowTypeArgumentsInExpression(this: *@This()) !bool {
            std.debug.assert(this.lexer.token == .t_less_than);

            var oldLexer = std.mem.toBytes(this.lexer);
            defer this.lexer = std.mem.bytesToValue(@TypeOf(this.lexer), &oldLexer);

            try this.lexer.next();

            var depth: u32 = 1;
            var paren_depth: u32 = 0;
            var brace_depth: u32 = 0;
            var bracket_depth: u32 = 0;
            var prev_token: ?js_lexer.T = null;
            while (depth > 0) {
                switch (this.lexer.token) {
                    // Many operators are invalid in this context
                    .t_plus, .t_tilde, .t_slash => return false,
                    .t_ampersand_ampersand, .t_bar_bar => return false,
                    .t_for, .t_delete, .t_yield, .t_return, .t_break, .t_if, .t_continue => {
                        if (prev_token) |t| {
                            if (t != .t_dot) {
                                return false;
                            }
                        } else {
                            return false;
                        }
                    },
                    .t_semicolon => {
                        if (brace_depth == 0) return false;
                    },
                    .t_open_bracket => {
                        bracket_depth += 1;
                    },
                    .t_close_bracket => {
                        if (bracket_depth == 0) return false;
                        bracket_depth -= 1;
                    },
                    .t_open_brace => {
                        brace_depth += 1;
                    },
                    .t_close_brace => {
                        if (brace_depth == 0) return false;
                        brace_depth -= 1;
                    },
                    .t_open_paren => {
                        paren_depth += 1;
                    },
                    .t_close_paren => {
                        if (paren_depth == 0) return false;
                        paren_depth -= 1;
                    },
                    .t_less_than => {
                        depth += 1;
                    },
                    .t_greater_than => {
                        if (depth == 0) return false;
                        depth -= 1;
                    },
                    .t_greater_than_greater_than => {
                        if (depth <= 1) return false;
                        depth -= 2;
                    },
                    .t_greater_than_greater_than_greater_than => {
                        if (depth <= 2) return false;
                        depth -= 3;
                    },
                    else => {},
                }

                prev_token = this.lexer.token;
                try if (depth == 0) this.lexer.next() else this.lexer.nextInTypeLookahead();
            }

            if (depth != 0 or paren_depth != 0 or brace_depth != 0 or bracket_depth != 0) return false;

            return this.canFollowTypeArgumentsInExpression();
        }

        fn parseType(this: *Parser) anyerror!NodeRef {
            return this.pushNode(try this.parseTypeNode());
        }

        fn canFollowTypeArgumentsInExpression(this: *@This()) bool {
            return switch (this.lexer.token) {
                // These are the only tokens can legally follow a type argument list. So we
                // definitely want to treat them as type arg lists.
                .t_open_paren, // foo<x>(
                .t_no_substitution_template_literal, // foo<T> `...`
                // foo<T> `...${100}...`
                .t_template_head,
                => true,

                // A type argument list followed by `<` never makes sense, and a type argument list followed
                // by `>` is ambiguous with a (re-scanned) `>>` operator, so we disqualify both. Also, in
                // this context, `+` and `-` are unary operators, not binary operators.
                .t_less_than,
                .t_greater_than,
                .t_plus,
                .t_minus,
                // TypeScript always sees "t_greater_than" instead of these tokens since
                // their scanner works a little differently than our lexer. So since
                // "t_greater_than" is forbidden above, we also forbid these too.
                .t_greater_than_equals,
                .t_greater_than_greater_than,
                .t_greater_than_greater_than_equals,
                .t_greater_than_greater_than_greater_than,
                .t_greater_than_greater_than_greater_than_equals,
                => false,

                // We favor the type argument list interpretation when it is immediately followed by
                // a line break, a binary operator, or something that can't start an expression.
                else => this.lexer.has_newline_before or this.isBinaryOperator() or !this.isStartOfExpression(),
            };
        }

        fn isBinaryOperator(this: *@This()) bool {
            return switch (this.lexer.token) {
                .t_in,
                .t_question_question,
                .t_bar_bar,
                .t_ampersand_ampersand,
                .t_bar,
                .t_caret,
                .t_ampersand,
                .t_equals_equals,
                .t_exclamation_equals,
                .t_equals_equals_equals,
                .t_exclamation_equals_equals,
                .t_less_than,
                .t_greater_than,
                .t_less_than_equals,
                .t_greater_than_equals,
                .t_instanceof,
                .t_less_than_less_than,
                .t_greater_than_greater_than,
                .t_greater_than_greater_than_greater_than,
                .t_plus,
                .t_minus,
                .t_asterisk,
                .t_slash,
                .t_percent,
                .t_asterisk_asterisk,
                => true,
                .t_identifier => this.lexer.isContextualKeyword("as") or this.lexer.isContextualKeyword("satisfies"),
                else => false,
            };
        }

        fn nextTokenIsOpenParenOrLessThanOrDot(this: *@This()) bool {
            this.lexer.next() catch {};

            return switch (this.lexer.token) {
                .t_open_paren, .t_less_than, .t_dot => true,
                else => false,
            };
        }

        fn isStartOfLeftHandSideExpression(this: *@This()) bool {
            return switch (this.lexer.token) {
                .t_this,
                .t_super,
                .t_null,
                .t_true,
                .t_false,
                .t_numeric_literal,
                .t_big_integer_literal,
                .t_string_literal,
                .t_no_substitution_template_literal,
                .t_template_head,
                .t_open_paren,
                .t_open_bracket,
                .t_open_brace,
                .t_function,
                .t_class,
                .t_new,
                .t_slash,
                .t_slash_equals,
                .t_identifier,
                => true,
                .t_import => this.nextTokenIsOpenParenOrLessThanOrDot(),
                else => false,
            };
        }

        fn isStartOfExpression(this: *@This()) bool {
            if (this.isStartOfLeftHandSideExpression())
                return true;

            switch (this.lexer.token) {
                .t_plus,
                .t_minus,
                .t_tilde,
                .t_exclamation,
                .t_delete,
                .t_typeof,
                .t_void,
                .t_reify,
                .t_plus_plus,
                .t_minus_minus,
                .t_less_than,
                .t_private_identifier,
                .t_at,
                .t_yield,
                .t_await,
                => return true,
                else => {
                    if (this.isBinaryOperator()) {
                        return true;
                    }

                    return false;
                },
            }
        }

        fn printLocation(this: *@This()) void {
            if (!comptime is_debug) return;

            const line = this.lexer.line_map.count;
            const col = @as(u32, @intCast(this.lexer.start - this.lexer.last_line));
            std.debug.print("  {any} _____ {s}\n\n", .{ this.lexer.token, this.lexer.getContext(50) });
            std.debug.print("at {s}:{}:{}\n", .{ this.lexer.source.name orelse "", line + 1, col + 1 });
        }

        fn emitParseError(this: *@This(), msg: []const u8) !AstNode_ {
            @setCold(true);

            if (msg.len == 0) {
                return error.Backtrack;
            }
            if (comptime is_debug) {
                std.debug.print("{s}\n", .{msg});
                this.printLocation();
            }

            return .{ .kind = .parse_error };
        }

        fn pushError(this: *@This(), msg: []const u8) !NodeRef {
            return this.pushNode(try this.emitParseError(msg));
        }

        fn parsePropertyAccess(this: *@This(), target: NodeRef, comptime is_optional_chain: bool) !AstNode_ {
            var member: NodeRef = 0;
            if (this.lexer.token == .t_identifier) {
                member = try this.parseIdentifier();
            } else if (this.lexer.token == .t_private_identifier) {
                member = try this.parsePrivateIdentifier();
            } else {
                member = try this.pushError("Unexpected member");
                try this.lexer.next();
            }

            return .{
                .kind = .property_access_expression,
                .data = toBinaryDataPtrRefs(target, member),
                .flags = if (is_optional_chain) @intFromEnum(NodeFlags.optional) else 0,
            };
        }

        fn parseElementAccess(this: *@This(), target: NodeRef, comptime is_optional_chain: bool) !AstNode_ {
            try this.lexer.expect(.t_open_bracket);
            const member = try this.parseExpression();
            try this.lexer.expect(.t_close_bracket);

            return .{
                .kind = .element_access_expression,
                .data = toBinaryDataPtrRefs(target, member),
                .flags = if (is_optional_chain) @intFromEnum(NodeFlags.optional) else 0,
            };
        }

        inline fn expectTypeStart(this: *@This()) !void {
            if (this.lexer.token == .t_less_than_less_than) {
                try this.lexer.next();
            } else {
                try this.lexer.expect(.t_less_than);
            }
        }

        fn maybeParseTypeEnd(this: *@This()) !bool {
            // XXX: FIXME: the first 2 checks do not use conditionally aware rescans
            if (this.lexer.token == .t_greater_than_greater_than_greater_than) {
                try this.lexer.rescanGreaterThanGreaterThanGreaterThan();
                return true;
            }

            if (this.lexer.token == .t_greater_than_greater_than) {
                try this.lexer.rescanGreaterThanGreaterThan();
                return true;
            }

            if (this.lexer.token == .t_greater_than) {
                try this.next();
                return true;
            }

            return false;
        }

        fn parseTypeArgs(this: *Parser) anyerror!NodeRef {
            try this.expectTypeStart();
            var args = NodeList_.init(this);

            if (this.lexer.token == .t_greater_than) {
                // parse error
                try this.lexer.next();
                return 0;
            }

            while (true) {
                try args.append(try this.parseTypeNode());
                if (try this.maybeParseTypeEnd()) break;
                try this.lexer.expect(.t_comma);
            }

            return args.head;
        }

        fn parseTypeArgsInJSXElement(this: *Parser) !struct { bool, NodeRef } {
            try this.expectTypeStart();
            var args = NodeList_.init(this);
            var is_terminated = false;

            while (true) {
                if (this.lexer.token == .t_greater_than) {
                    try this.lexer.nextInsideJSXElement();
                    break;
                }

                if (this.lexer.token == .t_greater_than_greater_than) {
                    try this.lexer.nextJSXElementChild();
                    is_terminated = true;
                    break;
                }

                if (args.head != 0) {
                    try this.lexer.expectInsideJSXElement(.t_comma);
                }

                try args.append(try this.parseTypeNode());
            }

            return .{ is_terminated, args.head };
        }

        fn parseTypeParamNode(this: *Parser) !AstNode_ {
            var flags: u22 = 0;
            if (this.lexer.token == .t_const) {
                try this.lexer.next();
                flags |= @intFromEnum(TypeParamFlags.@"const");
            }

            if (this.lexer.token == .t_in) {
                try this.lexer.next();
                flags |= @intFromEnum(TypeParamFlags.variance_in);
            }

            if (this.lexer.isContextualKeyword("out")) {
                try this.lexer.next();
                flags |= @intFromEnum(TypeParamFlags.variance_out);
            }

            const name = try this.parseIdentifier();

            var constraint: NodeRef = 0;
            if (this.lexer.token == .t_extends) {
                try this.lexer.next();
                constraint = try this.parseType();
            }

            var default: NodeRef = 0;
            if (this.lexer.token == .t_equals) {
                try this.lexer.next();
                default = try this.parseType();
            }

            return .{
                .kind = .type_parameter,
                .data = toBinaryDataPtrRefs(name, constraint),
                .len = default,
                .flags = flags,
            };
        }

        fn parseOneTypeParam(this: *@This()) !NodeRef {
            return this.pushNode(try this.parseTypeParamNode());
        }

        fn parseTypeParams(this: *@This()) !NodeRef {
            try this.expectTypeStart();

            if (this.lexer.token == .t_greater_than) {
                // parse error
                try this.lexer.next();
                return 0;
            }

            var params = NodeList_.init(this);

            while (true) {
                if (try this.maybeParseTypeEnd()) break;
                try params.append(try this.parseTypeParamNode());
                if (try this.maybeParseTypeEnd()) break;
                try this.lexer.expect(.t_comma);
            }

            return params.head;
        }

        fn parseCallArgs(this: *@This()) !NodeRef {
            const old_state = this.context_state;
            defer this.context_state = old_state;
            this.context_state = .none;

            try this.lexer.expect(.t_open_paren);
            var args = NodeList_.init(this);

            while (this.lexer.token != .t_close_paren) {
                if (this.lexer.token == .t_dot_dot_dot) {
                    try this.lexer.next();
                    const exp = try this.pushNode(try this.parseExpressionWithLevel(.comma));
                    try args.append(.{
                        .kind = .spread_element,
                        .data = @ptrFromInt(exp),
                    });
                } else {
                    const exp = try this.parseExpressionWithLevel(.comma);
                    try args.append(exp);
                }

                if (this.lexer.token == .t_close_paren) break;
                try this.lexer.expect(.t_comma);
            }

            try this.lexer.expect(.t_close_paren);

            return args.head;
        }

        inline fn getCurrentBinaryParseLevel(this: *Parser) Op.Level {
            return switch (this.lexer.token) {
                .t_comma => .comma,
                .t_question_question => .nullish_coalescing,
                .t_plus, .t_minus => .add,
                .t_asterisk, .t_slash, .t_percent => .multiply,
                .t_asterisk_asterisk => .exponentiation,

                .t_equals, .t_plus_equals, .t_minus_equals, .t_asterisk_asterisk_equals, .t_asterisk_equals, .t_percent_equals, .t_greater_than_greater_than_equals, .t_greater_than_greater_than_greater_than_equals, .t_question_question_equals, .t_bar_bar_equals, .t_ampersand_ampersand_equals, .t_bar_equals, .t_ampersand_equals, .t_caret_equals, .t_slash_equals => .assign,

                .t_equals_equals, .t_exclamation_equals, .t_equals_equals_equals, .t_exclamation_equals_equals, .t_less_than_less_than_equals => .equals,

                .t_in, .t_instanceof, .t_less_than, .t_less_than_equals, .t_greater_than, .t_greater_than_equals => .compare,

                .t_less_than_less_than, .t_greater_than_greater_than, .t_greater_than_greater_than_greater_than => .shift,

                .t_bar => .bitwise_or,
                .t_ampersand => .bitwise_and,
                .t_caret => .bitwise_xor,
                .t_bar_bar => .logical_or,
                .t_ampersand_ampersand => .logical_and,

                else => {
                    if (comptime is_debug) {
                        std.debug.print("Missing token in getCurrentBinaryParseLevel: {?}\n", .{this.lexer.token});
                    }
                    unreachable;
                },
            };
        }

        inline fn tokenToSyntaxKind(this: *@This()) SyntaxKind {
            return switch (this.lexer.token) {
                .t_in => .in_keyword,
                .t_instanceof => .instanceof_keyword,
                .t_comma => .comma_token,
                .t_question_question => .question_question_token,
                .t_plus => .plus_token,
                .t_minus => .minus_token,
                .t_asterisk => .asterisk_token,
                .t_slash => .slash_token,
                .t_percent => .percent_token,
                .t_asterisk_asterisk => .asterisk_asterisk_token,
                .t_equals => .equals_token,
                .t_plus_equals => .plus_equals_token,
                .t_minus_equals => .minus_equals_token,
                .t_asterisk_asterisk_equals => .asterisk_asterisk_equals_token,
                .t_asterisk_equals => .asterisk_equals_token,
                .t_percent_equals => .percent_equals_token,
                .t_greater_than_greater_than_equals => .greater_than_greater_than_equals_token,
                .t_greater_than_greater_than_greater_than_equals => .greater_than_greater_than_greater_than_equals_token,
                .t_question_question_equals => .question_question_equals_token,
                .t_bar_bar_equals => .bar_bar_equals_token,
                .t_ampersand_ampersand_equals => .ampersand_ampersand_equals_token,
                .t_bar_equals => .bar_equals_token,
                .t_ampersand_equals => .ampersand_equals_token,
                .t_caret_equals => .caret_equals_token,
                .t_slash_equals => .slash_equals_token,
                .t_equals_equals => .equals_equals_token,
                .t_exclamation_equals => .exclamation_equals_token,
                .t_equals_equals_equals => .equals_equals_equals_token,
                .t_exclamation_equals_equals => .exclamation_equals_equals_token,
                .t_less_than_less_than_equals => .less_than_less_than_equals_token,
                .t_less_than => .less_than_token,
                .t_less_than_equals => .less_than_equals_token,
                .t_greater_than => .greater_than_token,
                .t_greater_than_equals => .greater_than_equals_token,
                .t_less_than_less_than => .less_than_less_than_token,
                .t_greater_than_greater_than => .greater_than_greater_than_token,
                .t_greater_than_greater_than_greater_than => .greater_than_greater_than_greater_than_token,
                .t_bar => .bar_token,
                .t_ampersand => .ampersand_token,
                .t_caret => .caret_token,
                .t_bar_bar => .bar_bar_token,
                .t_ampersand_ampersand => .ampersand_ampersand_token,
                else => {
                    if (comptime is_debug) {
                        std.debug.print("Missing token in tokenToSyntaxKind: {?}\n", .{this.lexer.token});
                    }
                    unreachable;
                },
            };
        }

        inline fn toBinaryDataPtr(this: *@This(), left: AstNode_, right: AstNode_) !*anyopaque {
            return toBinaryDataPtrRefs(try this.pushNode(left), try this.pushNode(right));
        }

        inline fn parseRemainingBinaryExpression(this: *@This(), level: Op.Level, left: AstNode_, comptime is_logical_op: bool) !?AstNode_ {
            const expLevel = this.getCurrentBinaryParseLevel();
            if (level.gte(expLevel)) {
                return null;
            }

            // TODO: this is a parse error
            // if ((expLevel == .logical_or or expLevel == .logical_and) and level.eql(.nullish_coalescing)) {
            // }

            // const nextLevel: Op.Level = if (expLevel == .assign) .yield else if (expLevel == .exponentiation) .multiply else expLevel;
            const nextLevel: Op.Level = switch (expLevel) {
                .assign => .yield,
                .exponentiation => .multiply,
                else => expLevel,
            };

            const operator = this.tokenToSyntaxKind();

            try this.lexer.next();
            const lhs = try this.pushNode(left);
            const rhs = try this.pushNode(try this.parseExpressionWithLevel(nextLevel));

            if (comptime is_logical_op) {
                std.debug.assert(expLevel == .logical_or or expLevel == .logical_and);
                if (level.lt(.nullish_coalescing)) {
                    return try this.parseRemainingExpression(.logical_or, .{
                        .kind = .binary_expression,
                        .data = toBinaryDataPtrRefs(lhs, rhs),
                        .len = @intFromEnum(operator),
                    });
                }
            }

            return .{
                .kind = .binary_expression,
                .data = toBinaryDataPtrRefs(lhs, rhs),
                .len = @intFromEnum(operator),
            };
        }

        fn parseRemainingExpression(this: *Parser, level: Op.Level, start: AstNode_) anyerror!AstNode_ {
            var left = start;
            var type_args: NodeRef = 0;
            while (true) {
                switch (this.lexer.token) {
                    .t_dot => {
                        try this.lexer.nextNoKeywords();
                        const n = try this.parsePropertyAccess(try this.pushNode(left), false);
                        left = n;
                    },
                    .t_open_bracket => {
                        const n = try this.parseElementAccess(try this.pushNode(left), false);
                        left = n;
                    },
                    .t_open_paren => {
                        if (level.gte(.call)) {
                            return left;
                        }

                        const lhs = try this.pushNode(left);
                        const args = try this.parseCallArgs();

                        left = .{
                            .kind = .call_expression,
                            .data = toBinaryDataPtrRefs(lhs, args),
                            .len = type_args,
                        };

                        type_args = 0;
                    },
                    .t_question_dot => {
                        try this.lexer.nextNoKeywords();
                        switch (this.lexer.token) {
                            .t_open_bracket => {
                                const n = try this.parseElementAccess(try this.pushNode(left), true);
                                left = n;
                            },
                            .t_less_than => {
                                const lhs = try this.pushNode(left);
                                const typeArguments = try this.parseTypeArgs();
                                const args = try this.parseCallArgs();

                                left = .{
                                    .kind = .call_expression,
                                    .data = toBinaryDataPtrRefs(lhs, args),
                                    .len = typeArguments,
                                    .flags = @intFromEnum(NodeFlags.optional),
                                };
                            },
                            .t_open_paren => {
                                const lhs = try this.pushNode(left);
                                const args = try this.parseCallArgs();

                                left = .{
                                    .kind = .call_expression,
                                    .data = toBinaryDataPtrRefs(lhs, args),
                                    .flags = @intFromEnum(NodeFlags.optional),
                                };
                            },
                            else => {
                                const n = try this.parsePropertyAccess(try this.pushNode(left), true);
                                left = n;
                            },
                        }
                    },
                    .t_question => {
                        if (level.gte(.conditional)) {
                            return left;
                        }

                        const expression = try this.pushNode(left);

                        try this.lexer.next();
                        const when_true = try this.pushNode(try this.parseExpressionWithLevel(.comma));

                        try this.lexer.expect(.t_colon);
                        const when_false = try this.pushNode(try this.parseExpressionWithLevel(.comma));

                        left = .{
                            .kind = .conditional_expression,
                            .data = toBinaryDataPtrRefs(expression, when_true),
                            .len = when_false,
                        };
                    },
                    .t_exclamation => {
                        if (this.lexer.has_newline_before or level.gte(.postfix)) {
                            return left;
                        }

                        try this.lexer.next();
                        left.flags |= @intFromEnum(NodeFlags.non_null);
                    },
                    .t_minus_minus, .t_plus_plus => {
                        if (this.lexer.has_newline_before or level.gte(.postfix)) {
                            return left;
                        }

                        const operator: SyntaxKind = if (this.lexer.token == .t_minus_minus) .minus_minus_token else .plus_plus_token;

                        try this.lexer.next();
                        const operand = try this.pushNode(left);

                        left = .{
                            .kind = .postfix_unary_expression,
                            .data = toBinaryDataPtrRefs(operand, @intFromEnum(operator)),
                        };
                    },
                    .t_no_substitution_template_literal => {
                        // Tagged template
                        const tag = try this.pushNode(left);
                        const template = try this.pushNode(try this.parseStringLiteralLike());

                        left = .{
                            .kind = .tagged_template_expression,
                            .data = toBinaryDataPtrRefs(tag, template),
                        };
                    },
                    .t_template_head => {
                        // Tagged template
                        const tag = try this.pushNode(left);
                        const parts = try this.parseTemplateParts(false);
                        const template = try this.pushNode(.{
                            .kind = .template_expression,
                            .data = @ptrFromInt(parts),
                        });

                        left = .{
                            .kind = .tagged_template_expression,
                            .data = toBinaryDataPtrRefs(tag, template),
                        };
                    },

                    .t_less_than => {
                        if (level.gte(.compare)) {
                            return left;
                        }

                        if (try this.shouldFollowTypeArgumentsInExpression()) {
                            type_args = try this.parseTypeArgs();
                            continue;
                        }

                        if (try this.parseRemainingBinaryExpression(level, left, false)) |l| {
                            left = l;
                        } else {
                            return left;
                        }
                    },

                    .t_bar_bar, .t_ampersand_ampersand => {
                        if (try this.parseRemainingBinaryExpression(level, left, true)) |l| {
                            left = l;
                        } else {
                            return left;
                        }
                    },

                    .t_in, .t_instanceof, .t_comma, .t_question_question, .t_plus, .t_minus, .t_asterisk, .t_slash, .t_percent, .t_asterisk_asterisk, .t_equals, .t_plus_equals, .t_minus_equals, .t_asterisk_asterisk_equals, .t_asterisk_equals, .t_percent_equals, .t_greater_than_greater_than_equals, .t_greater_than_greater_than_greater_than_equals, .t_question_question_equals, .t_bar_bar_equals, .t_ampersand_ampersand_equals, .t_bar_equals, .t_ampersand_equals, .t_caret_equals, .t_slash_equals, .t_equals_equals, .t_exclamation_equals, .t_equals_equals_equals, .t_exclamation_equals_equals, .t_less_than_less_than_equals, .t_less_than_equals, .t_greater_than, .t_greater_than_equals, .t_less_than_less_than, .t_greater_than_greater_than, .t_greater_than_greater_than_greater_than, .t_bar, .t_ampersand, .t_caret => {
                        if (try this.parseRemainingBinaryExpression(level, left, false)) |l| {
                            left = l;
                        } else {
                            return left;
                        }
                    },

                    else => {
                        if (!level.lt(.compare) or this.lexer.has_newline_before or this.lexer.token != .t_identifier) {
                            return left;
                        }

                        const T = enum {
                            t_as,
                            t_is,
                            t_satisfies,
                        };

                        const TypeRelationKeywords = ComptimeStringMap(T, .{
                            .{ "as", .t_as },
                            .{ "is", .t_is },
                            .{ "satisfies", .t_satisfies },
                        });

                        const op = TypeRelationKeywords.get(this.lexer.identifier) orelse return left;
                        try this.lexer.next();

                        const lhs = try this.pushNode(left);
                        const rhs = try this.parseType();

                        const kind: SyntaxKind = switch (op) {
                            .t_is => .is_expression,
                            .t_as => .as_expression,
                            .t_satisfies => .satisfies_expression,
                        };

                        left = .{
                            .kind = kind,
                            .data = toBinaryDataPtrRefs(lhs, rhs),
                        };

                        // These tokens are not allowed to follow a cast expression. This isn't
                        // an outright error because it may be on a new line, in which case it's
                        // the start of a new expression when it's after a cast:
                        //
                        //   x = y as z
                        //   (something);
                        //
                        switch (this.lexer.token) {
                            .t_plus_plus,
                            .t_minus_minus,
                            .t_no_substitution_template_literal,
                            .t_template_head,
                            .t_open_paren,
                            .t_open_bracket,
                            .t_question_dot,
                            => {
                                return left;
                            },
                            else => {},
                        }

                        if (this.lexer.token.isAssign()) {
                            return left;
                        }
                        continue;
                    },
                }
            }
        }

        inline fn maybeScanRegexp(this: *@This(), lookbehind: js_lexer.T) !void {
            switch (lookbehind) {
                .t_open_paren, .t_open_bracket, .t_colon, .t_equals, .t_question_question_equals, .t_bar_bar_equals, .t_comma, .t_ampersand_ampersand, .t_bar_bar, .t_question_question, .t_question => {
                    try this.lexer.scanRegExp();
                },
                else => {},
            }
        }

        fn scanTemplate(this: *@This(), lookbehind: *js_lexer.T) !void {
            lookbehind.* = this.lexer.token;
            try this.lexer.nextNoKeywords();

            var brace_depth: u32 = 1;
            while (true) {
                if (this.lexer.token == .t_close_brace) {
                    brace_depth -= 1;
                    if (brace_depth == 0) {
                        try this.lexer.rescanCloseBraceAsTemplateToken();
                        if (this.lexer.token == .t_template_tail) return;
                        brace_depth += 1;
                    }
                } else if (this.lexer.token == .t_open_brace) {
                    brace_depth += 1;
                } else if (this.lexer.token == .t_template_head) {
                    try this.scanTemplate(lookbehind);
                } else if (this.lexer.token == .t_slash or this.lexer.token == .t_slash_equals) {
                    try this.maybeScanRegexp(lookbehind.*);
                }

                lookbehind.* = this.lexer.token;
                try this.lexer.nextNoKeywords();
            }
        }

        // Uses lookahead
        fn isArrowFn(this: *@This(), level: Op.Level, comptime is_type: bool) anyerror!bool {
            // should start at open paren
            var oldLexer = std.mem.toBytes(this.lexer);
            defer this.lexer = std.mem.bytesToValue(@TypeOf(this.lexer), &oldLexer);

            try this.lexer.nextNoKeywords();

            // First make sure that the first token is possibly a param binding
            switch (this.lexer.token) {
                .t_identifier => {
                    try this.lexer.nextNoKeywords();
                    switch (this.lexer.token) {
                        .t_close_paren, .t_comma, .t_equals => {},
                        // ts only
                        .t_colon => return true, // must always be an arrow fn
                        .t_question => {
                            try this.lexer.nextNoKeywords();
                            if (this.lexer.token != .t_comma and this.lexer.token != .t_colon) return false;
                        },
                        else => return false,
                    }
                },
                .t_open_brace, .t_open_bracket, .t_close_paren, .t_dot_dot_dot => {},
                else => return false,
            }

            var depth: u32 = 1;
            var lookbehind: js_lexer.T = this.lexer.token;
            while (true) {
                if (this.lexer.token == .t_open_paren) {
                    depth += 1;
                } else if (this.lexer.token == .t_close_paren) {
                    depth -= 1;
                    if (depth == 0) break;
                } else if (this.lexer.token == .t_template_head) {
                    try this.scanTemplate(&lookbehind);
                } else if (this.lexer.token == .t_slash or this.lexer.token == .t_slash_equals) {
                    try this.maybeScanRegexp(lookbehind);
                }

                lookbehind = this.lexer.token;
                try this.lexer.nextNoKeywords();
            }

            try this.lexer.nextNoKeywords();

            if (this.lexer.token == .t_equals_greater_than) {
                return true;
            }

            if (this.lexer.token == .t_colon) {
                if (comptime is_type) return false;
                if (level == .lowest) return true;

                // TODO: it's possible to skip this check in many cases w/ additional parse state
                // this check is only needed in "bare" when_true expressions

                try this.lexer.next();

                //const count = this.node_allocator.count;
                const local_count = this.node_allocator.local_count;
                // const pos_count = this.positions.positions.count(); // XXX
                const pos_local_count = this.positions.positions.local_count; // XXX
                defer {
                    //this.node_allocator.count = count;
                    this.node_allocator.local_count = local_count;
                    //this.positions.positions.count = pos_count;
                    this.positions.positions.local_count = pos_local_count;
                }

                // this will be reset by the earlier defer                
                this.lexer.print_expect = false;

                _ = this.parseTypeNode() catch return false;

                return this.lexer.token == .t_equals_greater_than;
            }

            return false;
        }

        // Uses lookahead
        fn isIndexSignature(this: *@This()) !bool {
            var oldLexer = std.mem.toBytes(this.lexer);
            defer this.lexer = std.mem.bytesToValue(@TypeOf(this.lexer), &oldLexer);

            try this.lexer.expect(.t_open_bracket);

            if (this.lexer.token != .t_identifier) {
                return false;
            }

            try this.lexer.next();

            if (this.lexer.token != .t_colon) {
                return false;
            }

            return true;
        }

        // Uses lookahead
        fn isMappedType(this: *@This()) !bool {
            var oldLexer = std.mem.toBytes(this.lexer);
            defer this.lexer = std.mem.bytesToValue(@TypeOf(this.lexer), &oldLexer);

            const prev_state = this.context_state;
            this.context_state = .interface;
            defer this.context_state = prev_state;

            try this.expect(.t_open_brace);

            if (this.lexer.token == .t_minus) return true;

            if (this.lexer.token == .t_readonly) {
                try this.next();
            }

            if (this.lexer.token != .t_open_bracket) return false;
            try this.next();

            if (this.lexer.token != .t_identifier) return false;
            try this.lexer.next();

            return this.lexer.token == .t_in;
        }

        // Uses lookahead
        fn isModifier(this: *@This()) !bool {
            var oldLexer = std.mem.toBytes(this.lexer);
            defer this.lexer = std.mem.bytesToValue(@TypeOf(this.lexer), &oldLexer);

            try this.lexer.nextNoKeywords();

            return this.lexer.token == .t_identifier or this.lexer.token == .t_open_bracket;
        }

        // Uses lookahead
        fn isAsyncModifier(this: *@This()) !bool {
            var oldLexer = std.mem.toBytes(this.lexer);
            defer this.lexer = std.mem.bytesToValue(@TypeOf(this.lexer), &oldLexer);

            try this.lexer.nextNoKeywords();

            return switch (this.lexer.token) {
                .t_colon, .t_comma, .t_close_brace => false,
                .t_identifier, .t_open_bracket, .t_asterisk => true,
                else => {
                    // parse error
                    return false;
                },
            };
        }

        // Uses lookahead
        fn isNamedTupleElement(this: *@This()) !bool {
            var oldLexer = std.mem.toBytes(this.lexer);
            defer this.lexer = std.mem.bytesToValue(@TypeOf(this.lexer), &oldLexer);

            try this.lexer.nextNoKeywords();

            return switch (this.lexer.token) {
                .t_question => {
                    try this.lexer.nextNoKeywords();
                    return this.lexer.token == .t_colon;
                },
                .t_colon => true,
                else => false,
            };
        }

        fn parseParenthesizedExpr(this: *@This()) !AstNode_ {
            const location = this.getLocation();
            try this.lexer.expect(.t_open_paren);
            const data = try this.parseExpression();
            try this.lexer.expect(.t_close_paren);

            return .{
                .kind = .parenthesized_expression,
                .data = @ptrFromInt(data),
                .location = location,
            };
        }

        fn parseArrowFnBody(this: *@This()) !NodeRef {
            try this.lexer.expect(.t_equals_greater_than);

            if (this.lexer.token == .t_open_brace) {
                return this.parseBlock();
            }

            return this.pushNode(try this.parseExpressionWithLevel(.comma));
        }

        fn parseArrowFn(this: *@This(), full_start: u32, flags: u22) !AstNode_ {
            const location = this.getLocation();
            var typeParameters: NodeRef = 0;
            if (this.lexer.token == .t_less_than) {
                typeParameters = try this.parseTypeParams();
            }

            const parameters = try this.parseParameters();

            var return_type: NodeRef = 0;
            if (this.lexer.token == .t_colon) {
                try this.lexer.next();
                return_type = try this.parseType();
            }

            const body = try this.parseArrowFnBody();

            return .{
                .kind = .arrow_function,
                .data = toBinaryDataPtrRefs(parameters, body),
                .len = return_type,
                .flags = flags,
                .extra_data = typeParameters,
                .full_start = full_start,
                .location = location,
                .width = @as(u32, @intCast(this.lexer.full_start)) - full_start,
            };
        }

        fn parseSingleParamArrowFn(this: *@This(), ident: AstNode_, full_start: u32, flags: u22) !AstNode_ {
            const location = this.getLocation();
            const param_binding = try this.pushNode(ident);
            const param = try this.pushNode(.{
                .kind = .parameter,
                .data = toBinaryDataPtrRefs(param_binding, 0),
            });
            const body = try this.parseArrowFnBody();

            return .{
                .kind = .arrow_function,
                .data = toBinaryDataPtrRefs(param, body),
                .flags = flags | @intFromEnum(NodeFlags.let),
                .full_start = full_start,
                .location = location,
                .width = @as(u32, @intCast(this.lexer.full_start)) - full_start,
            };
        }

        fn parseJSXText(this: *Parser) !AstNode_ {
            const slice = this.lexer.string_literal_slice;
            try this.lexer.nextJSXElementChild();

            return .{
                .kind = .jsx_text,
                .data = slice.ptr,
                .len = @intCast(slice.len),
            };
        }

        fn parseJSXElementStringLiteral(this: *Parser) !AstNode_ {
            const slice = this.lexer.string_literal_slice;
            try this.lexer.nextInsideJSXElement();

            return .{
                .kind = .string_literal,
                .data = slice.ptr,
                .len = @intCast(slice.len),
                .flags = if (this.lexer.string_literal_is_ascii) 0 else @intFromEnum(StringFlags.two_byte),
            };
        }

        fn parseStringLiteralLikeKind(this: *Parser, kind: SyntaxKind) !AstNode_ {
            var flags: u22 = if (this.lexer.raw()[0] == '"') @intFromEnum(StringFlags.double_quote) else @intFromEnum(StringFlags.single_quote);

            if (!this.lexer.string_literal_is_ascii) {
                flags |= @intFromEnum(StringFlags.two_byte);
            }

            const full_start = this.lexer.full_start;
            const width = this.getFullWidth();
            const location = this.getLocation();
            const slice = this.lexer.string_literal_slice;
            try this.next();

            return .{
                .kind = kind,
                .data = slice.ptr,
                .len = @intCast(slice.len),
                .location = location,
                .flags = flags,
                .full_start = full_start,
                .width = width,
            };
        }

        // Assumes either t_string_literal or t_no_substitution_template_literal
        inline fn parseStringLiteralLike(this: *@This()) !AstNode_ {
            const kind: SyntaxKind = if (this.lexer.token == .t_string_literal) .string_literal else .no_substitution_template_literal;

            return this.parseStringLiteralLikeKind(kind);
        }

        fn parseYieldExpression(this: *@This()) !AstNode_ {
            try this.lexer.expect(.t_yield);

            // yield*\n[1,2,3] should be parsed as a single expression
            var is_star = false;
            if (this.lexer.token == .t_asterisk) {
                if (this.lexer.has_newline_before) {
                    try this.lexer.next();
                    return this.emitParseError("");
                }

                is_star = true;
                try this.lexer.next();
            }

            var n = AstNode_{
                .kind = .yield_expression,
                .flags = if (is_star) @intFromEnum(NodeFlags.generator) else 0,
            };

            if (!is_star and this.lexer.has_newline_before) {
                return n;
            }

            if (try this.maybeParseExpression(.yield)) |ref| {
                n.data = @ptrFromInt(ref);
            }

            return n;
        }

        fn parsePrefixExpression(this: *@This(), comptime kind: SyntaxKind) !AstNode_ {
            try this.lexer.next();
            const exp = try this.parseExpressionWithLevel(.prefix);

            return .{
                .kind = kind,
                .data = @ptrFromInt(try this.pushNode(exp)),
            };
        }

        fn parsePrefixUnaryExpression(this: *@This()) !AstNode_ {
            const operator: SyntaxKind = switch (this.lexer.token) {
                .t_plus_plus => .plus_plus_token,
                .t_minus_minus => .minus_minus_token,
                .t_plus => .plus_token,
                .t_minus => .minus_token,
                .t_tilde => .tilde_token,
                .t_exclamation => .exclamation_token,
                else => return error.InvalidToken,
            };

            try this.lexer.next();
            const exp = try this.parseExpressionWithLevel(.prefix);

            return .{
                .kind = .prefix_unary_expression,
                .data = toBinaryDataPtrRefs(@intFromEnum(operator), try this.pushNode(exp)),
            };
        }

        fn parseFnDeclaration(this: *@This(), full_start: u32, flags: u22) !AstNode_ {
            return this.parseFnDecl(full_start, flags, .function_declaration);
        }

        fn parseFnExpression(this: *@This(), full_start: u32, flags: u22) !AstNode_ {
            return this.parseFnDecl(full_start, flags, .function_expression);
        }

        fn parseAsyncExpression(this: *@This(), level: Op.Level) !AstNode_ {
            const ident = this.lexer.identifier;
            const full_start = this.lexer.full_start;
            try this.lexer.next();

            const flags: u22 = @intFromEnum(NodeFlags.@"async");

            // Greedily parse function expressions
            if (this.lexer.token == .t_function and !this.lexer.has_newline_before) {
                return this.parseFnExpression(full_start, flags);
            }

            if (this.lexer.has_newline_before or level.eql(.member)) {
                return toIdentNode2(ident);
            }

            switch (this.lexer.token) {
                .t_equals_greater_than => {
                    if (level.gt(.assign)) {
                        return toIdentNode2(ident);
                    }

                    return this.parseSingleParamArrowFn(toIdentNode2(ident), full_start, flags);
                },
                .t_identifier => {
                    if (level.gt(.assign)) {
                        return toIdentNode2(ident);
                    }

                    return this.parseSingleParamArrowFn(try this.parseIdentifierNode(), full_start, flags);
                },
                .t_open_paren => {
                    return this.parseArrowFn(full_start, flags);
                },
                .t_less_than => {
                    return this.parseArrowFn(full_start, flags);
                },
                else => return toIdentNode2(ident),
            }
        }

        fn parseMetaProperty(this: *@This(), target: SyntaxKind) !AstNode_ {
            try this.lexer.expect(.t_dot);

            const ref = try switch (this.lexer.token) {
                .t_identifier => this.parseIdentifier(),
                .t_private_identifier => this.parsePrivateIdentifier(),
                else => {
                    std.debug.print("{any}", .{this.lexer.token});
                    return error.SyntaxError;
                },
            };

            return .{
                .kind = .meta_property,
                .data = toBinaryDataPtrRefs(@intFromEnum(target), ref),
            };
        }

        // Always the head followed by 1 or more spans
        fn parseTemplateParts(this: *@This(), comptime is_type: bool) !NodeRef {
            var parts = NodeList_.init(this);
            try parts.append(try this.parseStringLiteralLikeKind(.template_head));

            while (true) {
                const exp = try if (is_type) this.parseType() else this.parseExpression();
                try this.lexer.rescanCloseBraceAsTemplateToken();

                if (this.lexer.token == .t_template_tail) {
                    const tail = try this.pushNode(try this.parseStringLiteralLikeKind(.template_tail));
                    try parts.append(.{
                        .kind = if (is_type) .template_literal_type_span else .template_span,
                        .data = toBinaryDataPtrRefs(exp, tail),
                    });
                    break;
                }

                if (this.lexer.token != .t_template_middle) {
                    std.debug.print("{any}", .{this.lexer.token});
                    return error.SyntaxError;
                }

                const middle = try this.pushNode(try this.parseStringLiteralLikeKind(.template_middle));
                try parts.append(.{
                    .kind = if (is_type) .template_literal_type_span else .template_span,
                    .data = toBinaryDataPtrRefs(exp, middle),
                });
            }

            return parts.head;
        }

        fn parseJSXChildren(this: *@This()) !NodeList_ {
            var children = NodeList_.init(this);
            while (true) {
                if (this.lexer.token == .t_less_than) {
                    try this.lexer.nextInsideJSXElement();
                    if (this.lexer.token == .t_slash) {
                        try this.lexer.nextInsideJSXElement();
                        break;
                    }

                    // Must be tag or fragment
                    if (this.lexer.token == .t_greater_than) {
                        try this.lexer.nextInsideJSXElement();
                        try children.append(try this.parseJSXElement());
                        continue;
                    }

                    try children.append(try this.parseJSXElement());
                    continue;
                }

                if (this.lexer.token == .t_open_brace) {
                    // FIXME: handle empty expression
                    try this.lexer.nextInsideJSXElement();
                    const exp = try this.parseExpression();
                    try children.append(.{
                        .kind = .jsx_expression,
                        .data = @ptrFromInt(exp),
                    });
                    try this.lexer.expectJSXElementChild(.t_close_brace);
                    continue;
                }

                if (this.lexer.token == .t_string_literal) {
                    try children.append(try this.parseJSXText());
                } else {
                    std.debug.print("{any}", .{this.lexer.token});
                    return error.ParserError;
                }
            }

            return children;
        }

        fn printDebug(this: *@This(), node: AstNode_) !void {
            try print(.{
                .nodes = this.node_allocator,
                .decorators = this.decorators,
                .extra_data = this.extra_data_allocator,
            }, node);
            std.debug.print("\n", .{});
        }

        // Assumes `<` has already been parsed
        fn parseJSXElement(this: *@This()) anyerror!AstNode_ {
            const openingTag = try this.parseExpressionWithLevel(.compare);
            const tag = try this.pushNode(openingTag);

            var scan_attributes = true;
            var typeArguments: NodeRef = 0;
            if (this.lexer.token == .t_less_than) {
                const r = try this.parseTypeArgsInJSXElement();
                scan_attributes = !r[0];
                typeArguments = r[1];
            }

            var self_closing = false;
            var attributes = NodeList_.init(this);

            while (scan_attributes) {
                if (this.lexer.token == .t_slash) {
                    try this.lexer.nextInsideJSXElement();
                    try this.lexer.expect(.t_greater_than);
                    self_closing = true;
                    break;
                }

                if (this.lexer.token == .t_greater_than) {
                    try this.lexer.nextJSXElementChild();
                    break;
                }

                // spread attribute
                if (this.lexer.token == .t_open_brace) {
                    try this.lexer.next();
                    try this.lexer.expect(.t_dot_dot_dot);
                    const exp = try this.parseExpression();
                    try attributes.append(.{
                        .kind = .jsx_spread_attribute,
                        .data = @ptrFromInt(exp),
                    });
                    try this.lexer.expectInsideJSXElement(.t_close_brace);
                    continue;
                }

                // ident
                const n = try this.pushNode(toIdentNode2(this.lexer.identifier));
                try this.lexer.nextInsideJSXElement();

                var value: NodeRef = 0;
                if (this.lexer.token == .t_equals) {
                    try this.lexer.nextInsideJSXElement();
                    if (this.lexer.token == .t_open_brace) {
                        try this.lexer.next();
                        const exp = try this.parseExpression();
                        value = try this.pushNode(.{
                            .kind = .jsx_expression,
                            .data = @ptrFromInt(exp),
                        });
                        try this.lexer.expectInsideJSXElement(.t_close_brace);
                    } else if (this.lexer.token == .t_string_literal) {
                        value = try this.pushNode(try this.parseJSXElementStringLiteral());
                    } else {
                        // try this.lexer.expectInsideJSXElement(.t_less_than);
                        // value = try this.pushNode(try this.parseJSXElementOrFragment());
                        return error.ParserError;
                    }
                }

                try attributes.append(.{
                    .kind = .jsx_attribute,
                    .data = toBinaryDataPtrRefs(n, value),
                });
            }

            var attributes_ref: NodeRef = 0;
            if (attributes.head != 0) {
                attributes_ref = try this.pushNode(.{
                    .kind = .jsx_attributes,
                    .data = @ptrFromInt(attributes.head),
                });
            }

            if (self_closing) {
                return .{
                    .kind = .jsx_self_closing_element,
                    .data = toBinaryDataPtrRefs(tag, attributes_ref),
                    .len = typeArguments,
                };
            }

            const opening = try this.pushNode(.{
                .kind = .jsx_opening_element,
                .data = toBinaryDataPtrRefs(tag, attributes_ref),
                .len = typeArguments,
            });

            var children = try this.parseJSXChildren();
            const exp = try this.parseExpressionWithLevel(.compare);
            const closing = try this.pushNode(exp);
            try this.lexer.expect(.t_greater_than);

            try children.append(.{
                .kind = .jsx_closing_element,
                .data = @ptrFromInt(closing),
            });

            this.node_allocator.at(opening).next = children.head;

            return .{
                .kind = .jsx_element,
                .data = @ptrFromInt(opening),
            };
        }

        fn parseJSXFragment(this: *@This()) anyerror!AstNode_ {
            const children = try this.parseJSXChildren();
            try this.lexer.expect(.t_greater_than);

            return .{
                .kind = .jsx_fragment,
                .data = @ptrFromInt(children.head),
            };
        }

        fn parseJSXElementOrFragment(this: *@This()) !AstNode_ {
            if (this.lexer.token == .t_greater_than) {
                try this.lexer.nextJSXElementChild();

                return this.parseJSXFragment();
            }

            return this.parseJSXElement();
        }

        fn parseArrowFnOrJSX(this: *@This()) !AstNode_ {
            if (!this.options.allow_jsx or (this.isTSArrowFnJSX() catch false)) {
                // we don't support legacy type assertions e.g. <T>foo
                return this.parseArrowFn(this.lexer.full_start, 0);
            }

            try this.lexer.expect(.t_less_than);

            if (this.lexer.token == .t_greater_than) {
                try this.lexer.nextJSXElementChild();

                return this.parseJSXFragment();
            }

            return this.parseJSXElement();
        }

        fn parseRemainingImportExpression(this: *@This()) !AstNode_ {
            if (this.lexer.token == .t_dot) {
                return this.parseMetaProperty(.import_keyword);
            }

            const exp = try this.pushNode(.{
                .kind = .import_keyword,
            });
            const arguments = try this.parseCallArgs();

            return .{
                .kind = .call_expression,
                .data = toBinaryDataPtrRefs(exp, arguments),
            };
        }

        fn parseNumericLiteralBase(this: *@This(), comptime is_negative: bool) !AstNode_ {
            const location = this.getLocation(); // TODO: misses minus sign
            const v: usize = @bitCast(if (is_negative) -this.lexer.number else this.lexer.number);
            try this.next();

            return .{
                .kind = .numeric_literal,
                .data = @ptrFromInt(v),
                .location = location,
            };
        }

        inline fn parseNumericLiteral(this: *@This()) !AstNode_ {
            return this.parseNumericLiteralBase(false);
        }

        fn parseExpressionBase(this: *@This(), level: Op.Level) !AstNode_ {
            switch (this.lexer.token) {
                .t_async => {
                    return this.parseAsyncExpression(level);
                },
                .t_await => {
                    try this.lexer.next();
                    const right = try this.pushNode(try this.parseExpressionWithLevel(level));

                    return .{
                        .kind = .await_expression,
                        .data = @ptrFromInt(right),
                    };
                },
                .t_identifier => {
                    const full_start = this.lexer.full_start;
                    const ident = try this.parseIdentifierNode();
                    if (this.lexer.token == .t_equals_greater_than and level.lte(.assign)) {
                        return this.parseSingleParamArrowFn(ident, full_start, 0);
                    }

                    return ident;
                },
                .t_import => {
                    try this.lexer.next();
                    return this.parseRemainingImportExpression();
                },
                .t_false => {
                    try this.next();
                    return .{ .kind = .false_keyword };
                },
                .t_true => {
                    try this.next();
                    return .{ .kind = .true_keyword };
                },
                .t_null => {
                    try this.next();
                    return .{ .kind = .null_keyword };
                },
                .t_undefined => {
                    try this.next();
                    return .{ .kind = .undefined_keyword };
                },
                .t_numeric_literal => return this.parseNumericLiteral(),
                .t_big_integer_literal => {
                    const value = this.lexer.identifier;
                    try this.next();
                    return .{
                        .kind = .bigint_literal,
                        .data = value.ptr,
                        .len = @intCast(value.len),
                    };
                },
                .t_slash, .t_slash_equals => {
                    try this.lexer.scanRegExp();

                    defer this.lexer.regex_flags_start = null;
                    const value = this.lexer.raw();
                    try this.next();

                    return .{
                        .kind = .regular_expression_literal,
                        .data = value.ptr,
                        .len = @intCast(value.len),
                        .extra_data = this.lexer.regex_flags_start orelse 0,
                    };
                },
                .t_template_head => {
                    const parts = try this.parseTemplateParts(false);

                    return .{
                        .kind = .template_expression,
                        .data = @ptrFromInt(parts),
                    };
                },
                .t_string_literal, .t_no_substitution_template_literal => {
                    return this.parseStringLiteralLike();
                },
                .t_open_paren => {
                    if (level.gt(.assign)) {
                        return this.parseParenthesizedExpr();
                    }

                    if (!(try this.isArrowFn(level, false))) {
                        return this.parseParenthesizedExpr();
                    }

                    return this.parseArrowFn(this.lexer.full_start, 0);
                },
                .t_open_brace => {
                    return this.parseObjectLiteral();
                },
                .t_open_bracket => {
                    return this.parseArrayLiteral();
                },
                .t_yield => {
                    return this.parseYieldExpression();
                },
                .t_reify => {
                    try this.lexer.next();
                    if (this.lexer.token != .t_less_than) {
                        const t = try this.parseType();
                        return .{ 
                            .kind = .reify_expression, 
                            .data = @ptrFromInt(t),
                        };
                    }
                    const type_params = try this.parseTypeParams();
                    const t = try this.parseType();
                    return .{ 
                        .kind = .reify_expression, 
                        .data = @ptrFromInt(t),
                        .len = type_params,
                    };
                },
                .t_delete => return this.parsePrefixExpression(.delete_expression),
                .t_void => return this.parsePrefixExpression(.void_expression),
                .t_typeof => return this.parsePrefixExpression(.typeof_expression),

                .t_plus, .t_minus, .t_plus_plus, .t_minus_minus, .t_tilde, .t_exclamation => return this.parsePrefixUnaryExpression(),

                .t_function => return this.parseFnExpression(this.lexer.full_start, 0),
                .t_class => {
                    return this.parseClassNode(this.lexer.full_start, .class_expression);
                },
                .t_new => {
                    // Needed for source maps
                    const location = this.getLocation();
                    try this.lexer.next();

                    // `new` can be used as a subject in a property access expression in some contexts
                    // typescript refers to this expression as a `MetaProperty`
                    if (this.lexer.token == .t_dot) {
                        return this.parseMetaProperty(.new_keyword);
                    }

                    const exp = try this.parseExpressionWithLevel(.member);

                    const expression = try this.pushNode(exp);
                    const typeArguments = if (this.lexer.token == .t_less_than) try this.parseTypeArgs() else 0;
                    const arguments = if (this.lexer.token == .t_open_paren) try this.parseCallArgs() else 0;

                    return .{
                        .kind = .new_expression,
                        .data = toBinaryDataPtrRefs(expression, arguments),
                        .len = typeArguments,
                        .location = location,
                    };
                },
                .t_this => {
                    const location = this.getLocation();
                    const full_start = this.lexer.full_start;
                    const width = this.getFullWidth();
                    try this.lexer.next();

                    return .{ .kind = .this_keyword, .location = location, .width = width, .full_start = full_start };
                },
                .t_super => {
                    try this.lexer.next();

                    switch (this.lexer.token) {
                        .t_open_paren => {
                            if (level.lt(.call)) {
                                return .{ .kind = .super_keyword };
                            }
                        },
                        .t_dot, .t_open_bracket => {
                            return .{ .kind = .super_keyword };
                        },
                        else => {},
                    }

                    return this.emitParseError("Unexpected \"super\"");
                },
                .t_less_than => {
                    return this.parseArrowFnOrJSX();
                },
                .t_debugger => {
                    try this.lexer.next();
                    return .{ .kind = .debugger_keyword };
                },
                else => {
                    try this.lexer.next();
                    return this.emitParseError("");
                },
            }
        }

        inline fn parseExpressionInner(this: *@This()) !AstNode_ {
            const n = try this.parseExpressionBase(.lowest);

            return this.parseRemainingExpression(.lowest, n);
        }

        fn parseExpressionWithLevel(this: *@This(), level: Op.Level) anyerror!AstNode_ {
            const n = try this.parseExpressionBase(level);

            return try this.parseRemainingExpression(level, n);
        }

        fn maybeParseExpression(this: *@This(), level: Op.Level) !?NodeRef {
            return switch (this.lexer.token) {
                .t_close_brace, .t_close_paren, .t_close_bracket, .t_colon, .t_comma, .t_semicolon => null,
                else => {
                    const exp = try this.parseExpressionWithLevel(level);

                    return try this.pushNode(exp);
                },
            };
        }

        fn parseExpression(this: *@This()) anyerror!NodeRef {
            const n = try this.parseExpressionInner();
            return this.pushNode(n);
        }

        fn parseArrayBindingElement(this: *@This()) anyerror!AstNode_ {
            var name: NodeRef = 0;
            var initializer: NodeRef = 0;
            var flags: u22 = 0;

            switch (this.lexer.token) {
                .t_identifier => {
                    name = try this.parseIdentifier();
                },
                .t_dot_dot_dot => {
                    flags |= @intFromEnum(NodeFlags.generator);
                    try this.lexer.next();
                    if (this.lexer.token != .t_identifier) {
                        name = try this.pushError("Invalid binding name");
                        try this.lexer.next();
                    } else {
                        name = try this.parseIdentifier();
                    }
                },
                .t_open_brace => {
                    name = try this.parseObjectBindingPattern();
                },
                .t_open_bracket => {
                    name = try this.parseArrayBindingPattern();
                },
                else => {
                    this.printLocation();
                    try this.lexer.next();
                    return this.emitParseError("Invalid binding element");
                },
            }

            if (this.lexer.token == .t_equals) {
                try this.lexer.next();

                if (flags != 0) {
                    // Parse the expression but this node is invalid
                }

                initializer = try this.pushNode(try this.parseExpressionWithLevel(.comma));
            }

            return .{
                .kind = .binding_element,
                .data = toBinaryDataPtrRefs(name, initializer),
                .len = 0,
                .flags = flags,
            };
        }

        fn parseObjectBindingElement(this: *@This()) anyerror!AstNode_ {
            var name: NodeRef = 0;
            var property: NodeRef = 0;
            var initializer: NodeRef = 0;
            var flags: u22 = 0;

            switch (this.lexer.token) {
                .t_identifier => {
                    name = try this.parseIdentifier();

                    if (this.lexer.token == .t_colon) {
                        try this.lexer.next();
                        property = name;
                        name = try this.parseBinding();
                    }
                },
                .t_dot_dot_dot => {
                    flags |= @intFromEnum(NodeFlags.generator);
                    try this.lexer.next();
                    if (this.lexer.token != .t_identifier) {
                        name = try this.pushError("Invalid binding name");
                        try this.lexer.next();
                    } else {
                        name = try this.parseIdentifier();
                    }
                },
                .t_open_bracket, .t_numeric_literal, .t_string_literal => {
                    property = try this.parseMemberName();
                    try this.lexer.expect(.t_colon);
                    name = try this.parseBinding();
                },
                else => {
                    this.printLocation();
                    try this.lexer.next();
                    return this.emitParseError("Invalid binding element");
                },
            }

            if (this.lexer.token == .t_equals) {
                try this.lexer.next();

                if (flags > 0) {
                    // Parse the expression but this node is invalid
                }

                initializer = try this.pushNode(try this.parseExpressionWithLevel(.comma));
            }

            return .{
                .kind = .binding_element,
                .data = toBinaryDataPtrRefs(name, initializer),
                .len = property,
                .flags = flags,
            };
        }

        fn parseObjectBindingPattern(this: *@This()) !NodeRef {
            // try this.lexer.expect(.t_open_brace);
            try this.lexer.nextNoKeywords();

            var elements = NodeList_.init(this);

            while (this.lexer.token != .t_close_brace) {
                if (this.lexer.token == .t_comma) {
                    try this.lexer.nextNoKeywords();
                    continue;
                }
                try elements.append(try this.parseObjectBindingElement());
            }

            try this.lexer.expect(.t_close_brace);

            return this.pushNode(.{
                .kind = .object_binding_pattern,
                .data = @ptrFromInt(elements.head),
            });
        }

        fn parseArrayBindingPattern(this: *@This()) !NodeRef {
            try this.lexer.expect(.t_open_bracket);

            var elements = NodeList_.init(this);

            while (this.lexer.token != .t_close_bracket) {
                if (this.lexer.token == .t_comma) {
                    try this.lexer.next();
                    try elements.append(.{ .kind = .omitted_expression });
                } else {
                    try elements.append(try this.parseArrayBindingElement());
                    if (this.lexer.token == .t_comma) {
                        try this.lexer.next();
                    }
                }
            }

            try this.lexer.expect(.t_close_bracket);

            return this.pushNode(.{
                .kind = .array_binding_pattern,
                .data = @ptrFromInt(elements.head),
            });
        }

        // Binding can be one of:
        // * Identifier
        // * Array binding pattern
        // * Object binding pattern
        inline fn parseBinding(this: *@This()) !NodeRef {
            return switch (this.lexer.token) {
                .t_async => this.parseIdentifier(), // XXX
                .t_identifier => this.parseIdentifier(),
                .t_open_brace => this.parseObjectBindingPattern(),
                .t_open_bracket => this.parseArrayBindingPattern(),
                else => this.pushError("Invalid binding"),
            };
        }

        fn parseArrayLiteral(this: *@This()) !AstNode_ {
            // try this.lexer.expect(.t_open_bracket);
            const location = this.getLocation();
            try this.lexer.next();

            var elements = NodeList_.init(this);
            var emit_omitted_expression = true;

            while (this.lexer.token != .t_close_bracket) {
                if (this.lexer.token == .t_dot_dot_dot) {
                    try this.lexer.next();
                    const exp = try this.pushNode(try this.parseExpressionWithLevel(.comma));
                    try elements.append(.{
                        .kind = .spread_element,
                        .data = @ptrFromInt(exp),
                    });
                    emit_omitted_expression = false;
                } else if (this.lexer.token == .t_comma) {
                    try this.lexer.next();
                    if (emit_omitted_expression) {
                        try elements.append(.{
                            .kind = .omitted_expression,
                        });
                    }
                    emit_omitted_expression = true;
                } else {
                    try elements.append(try this.parseExpressionWithLevel(.comma));
                    emit_omitted_expression = false;
                }
            }

            try this.next();

            return .{
                .kind = .array_literal_expression,
                .data = @ptrFromInt(elements.head),
                .location = location,
            };
        }

        fn parseMethodOrPropertyAfterName(this: *@This(), name: NodeRef) !AstNode_ {
            if (this.lexer.token == .t_colon) {
                try this.lexer.next();
                const initializer = try this.pushNode(try this.parseExpressionWithLevel(.comma));

                return .{
                    .kind = .property_assignment, // TODO: if this is a type, it should be property_signature
                    .data = toBinaryDataPtrRefs(name, initializer),
                };
            } else if (this.lexer.token == .t_open_paren) {
                return this.parseMethodDeclWithName(name, 0);
            } else if (this.lexer.token == .t_question) {
                try this.lexer.next();
                return this.parseMethodDeclWithName(name, @intFromEnum(NodeFlags.optional));
            }

            return .{
                .kind = .shorthand_property_assignment,
                .data = @ptrFromInt(name),
            };
        }

        inline fn parseObjectLiteralMember(this: *@This()) !AstNode_ {
            if (this.lexer.token == .t_identifier) {
                const s = this.lexer.identifier;
                const ident = toIdentNodeWithLocation(s, this.getLocation(), this.lexer.full_start, this.getFullWidth());
                try this.lexer.nextNoKeywords();

                if (this.lexer.token == .t_identifier or this.lexer.token == .t_open_bracket or this.lexer.token == .t_string_literal or this.lexer.token == .t_asterisk) {
                    if (js_lexer.ObjectLiteralKeywords.get(s)) |t| {
                        switch (t) {
                            .t_get => {
                                return this.parseGetAccessor();
                            },
                            .t_set => {
                                return this.parseSetAccessor();
                            },
                            .t_async => {
                                if (this.lexer.token == .t_asterisk) {
                                    try this.lexer.nextNoKeywords();
                                    return try this.parseMethodDecl(@intFromEnum(NodeFlags.@"async") | @intFromEnum(NodeFlags.generator));
                                }

                                return try this.parseMethodDecl(@intFromEnum(NodeFlags.@"async"));
                            },
                            else => unreachable,
                        }
                    }
                    return this.emitParseError("Property expected");
                }

                return this.parseMethodOrPropertyAfterName(try this.pushNode(ident));
            }

            if (this.lexer.token == .t_asterisk) {
                try this.lexer.nextNoKeywords();
                return try this.parseMethodDecl(@intFromEnum(NodeFlags.generator));
            }

            if (this.lexer.token == .t_dot_dot_dot) {
                try this.lexer.next();
                const exp = try this.pushNode(try this.parseExpressionWithLevel(.comma));
                return .{
                    .kind = .spread_assignment,
                    .data = @ptrFromInt(exp),
                };
            } 

            if (this.lexer.token == .t_string_literal) {
                const lit = try this.parseStringLiteralLikeKind(.string_literal);
                const ident = try this.pushNode(lit);
                return try this.parseMethodOrPropertyAfterName(ident);
            }

            if (this.lexer.token == .t_numeric_literal) {
                const ident = try this.parseNumericLiteral();
                return try this.parseMethodOrPropertyAfterName(try this.pushNode(ident)); // TODO: methods are invalid here
            }

            try this.lexer.expect(.t_open_bracket);
            const exp = try this.parseExpression();
            try this.lexer.expect(.t_close_bracket);

            const name = try this.pushNode(.{
                .kind = .computed_property_name,
                .data = @ptrFromInt(exp),
            });

            // TODO: shorthand is invalid here
            return try this.parseMethodOrPropertyAfterName(name);
        }

        fn parseObjectLiteral(this: *@This()) !AstNode_ {
            // try this.expect(.t_open_brace);
            try this.lexer.nextNoKeywords();

            var elements = NodeList_.init(this);

            while (this.lexer.token != .t_close_brace) {
                try elements.append(try this.parseObjectLiteralMember());
                if (this.lexer.token == .t_comma) {
                    try this.lexer.nextNoKeywords();
                }
            }

            try this.expect(.t_close_brace);

            return .{
                .kind = .object_literal_expression,
                .data = @ptrFromInt(elements.head),
            };
        }

        fn parseVariableDeclLike(this: *@This()) !AstNode_ {
            var binding: NodeRef = 0;
            var ty: NodeRef = 0;
            var initializer: NodeRef = 0;
            var flags: u22 = 0;

            switch (this.lexer.token) {
                .t_identifier, .t_open_brace, .t_open_bracket => {
                    binding = try this.parseBinding();

                    if (this.lexer.token == .t_exclamation) {
                        try this.lexer.next();
                        flags |= @intFromEnum(NodeFlags.non_null);
                    }

                    if (this.lexer.token == .t_colon) {
                        try this.lexer.next();
                        ty = try this.parseType();
                    }

                    if (this.lexer.token == .t_equals) {
                        try this.lexer.next();
                        initializer = try this.pushNode(try this.parseExpressionWithLevel(.comma));
                    }
                },
                else => return error.NotVariableLike,
            }

            return .{
                .kind = .variable_declaration,
                .data = toBinaryDataPtrRefs(binding, initializer),
                .len = ty,
                .flags = flags,
            };
        }

        fn parseVariableDeclaration(this: *@This()) !AstNode_ {
            return switch (this.lexer.token) {
                .t_identifier, .t_open_brace, .t_open_bracket => this.parseVariableDeclLike(),
                else => {
                    try this.lexer.next();
                    return this.emitParseError("");
                },
            };
        }

        fn parseVariableStatement(this: *@This(), flags: u22) !AstNode_ {
            const first = try this.parseVariableDeclaration();
            var decls = NodeList_.init(this);
            try decls.append(first);

            while (this.lexer.token == .t_comma) {
                try this.lexer.next();
                try decls.append(try this.parseVariableDeclaration());
            }

            return .{
                .kind = .variable_statement,
                .data = @ptrFromInt(decls.head),
                .flags = flags,
            };
        }

        fn parseBlockStatementsLinked(this: *@This()) anyerror!NodeRef {
            try this.lexer.expect(.t_open_brace);

            if (this.lexer.token == .t_close_brace) {
                try this.next();
                return 0;
            }

            const old_state = this.context_state;
            this.context_state = .none;

            var list = NodeList_.init(this);
            while (this.lexer.token != .t_end_of_file) {
                const n = try this.parseStatement();
                try list.append(n);

                if (this.lexer.token == .t_close_brace) {
                    this.context_state = old_state;
                    try this.next();
                    break;
                }
            }

            return list.head;
        }

        fn parseBlockNode(this: *@This()) anyerror!AstNode_ {
            const ref = try this.parseBlockStatementsLinked();

            return .{
                .kind = .block,
                .data = @ptrFromInt(ref),
            };
        }

        inline fn parseBlock(this: *@This()) anyerror!NodeRef {
            return this.pushNode(try this.parseBlockNode());
        }

        fn parseParam(this: *@This(), flagsInit: u22) !AstNode_ {
            var flags: u22 = flagsInit;
            if (this.lexer.token == .t_dot_dot_dot) {
                try this.lexer.next();
                flags |= @intFromEnum(NodeFlags.generator);
            }

            const binding = blk: {
                if (this.lexer.token == .t_this) {
                    try this.lexer.next();
                    flags |= @intFromEnum(NodeFlags.declare);

                    break :blk try this.pushNode(.{ .kind = .this_keyword });
                } else {
                    break :blk try this.parseBinding();
                }
            };

            if (this.lexer.token == .t_question) {
                try this.lexer.next();
                flags |= @intFromEnum(NodeFlags.optional);
            }

            var ty: NodeRef = 0;
            if (this.lexer.token == .t_colon) {
                try this.lexer.next();
                ty = try this.parseType();
            }

            var initializer: NodeRef = 0;
            if (this.lexer.token == .t_equals) {
                try this.lexer.next();
                initializer = try this.pushNode(try this.parseExpressionWithLevel(.comma));
            }

            return .{
                .kind = .parameter,
                .data = toBinaryDataPtrRefs(binding, initializer),
                .len = ty,
                .flags = flags,
            };
        }

        fn parseParameters(this: *@This()) !NodeRef {
            try this.expect(.t_open_paren);

            var params = NodeList_.init(this);
            while (this.lexer.token != .t_close_paren) {
                const p = try this.parseParam(0);
                try params.append(p);

                if (this.lexer.token == .t_close_paren) break;
                try this.expect(.t_comma);
            }

            try this.next();

            return params.head;
        }

        fn parseConstructorParameters(this: *@This()) !NodeRef {
            try this.lexer.expectInConstructorParameterList(.t_open_paren);

            var params = NodeList_.init(this);
            while (this.lexer.token != .t_close_paren) {
                var flags: u22 = 0;
                if (this.lexer.token == .t_public) {
                    try this.lexer.nextInConstructorParameterList();
                    flags |= @intFromEnum(NodeFlags.public);
                } else if (this.lexer.token == .t_protected) {
                    try this.lexer.nextInConstructorParameterList();
                    flags |= @intFromEnum(NodeFlags.protected);
                } else if (this.lexer.token == .t_private) {
                    try this.lexer.nextInConstructorParameterList();
                    flags |= @intFromEnum(NodeFlags.private);
                }

                if (this.lexer.token == .t_readonly) {
                    try this.lexer.nextInConstructorParameterList();
                    flags |= @intFromEnum(NodeFlags.readonly);
                }

                const p = try this.parseParam(flags);
                try params.append(p);

                if (this.lexer.token == .t_close_paren) break;
                try this.lexer.expectInConstructorParameterList(.t_comma);
            }

            try this.next();

            return params.head;
        }

        fn parseFnDecl(this: *@This(), full_start: u32, flags_init: u22, comptime kind: SyntaxKind) !AstNode_ {
            try this.lexer.expect(.t_function);

            var flags = flags_init;
            if (this.lexer.token == .t_asterisk) {
                try this.lexer.next();
                flags |= @intFromEnum(NodeFlags.generator);
            }

            // we won't have an ident for `export default function () { }`
            var name: NodeRef = 0;
            if (this.lexer.token == .t_identifier) {
                name = try this.parseIdentifier();
            }

            var typeParameters: NodeRef = 0;
            if (this.lexer.token == .t_less_than) {
                typeParameters = try this.parseTypeParams();
            }

            const parameters = try this.parseParameters();

            var return_type: NodeRef = 0;
            if (this.lexer.token == .t_colon) {
                try this.lexer.next();
                return_type = try this.parseType();
            }

            var body: NodeRef = 0;
            if (this.lexer.token == .t_open_brace) {
                body = try this.parseBlock();
            } else {
                try this.maybeEatSemi();
            }

            return .{
                .kind = kind,
                .data = if (name == 0 and parameters == 0) null else toBinaryDataPtrRefs(name, parameters),
                .len = body,
                .flags = flags,
                .extra_data = typeParameters,
                .extra_data2 = return_type,
                .full_start = full_start,
                .width = @as(u32, @intCast(this.lexer.full_start)) - full_start,
            };
        }

        fn parseExprWithTypeArgs(this: *@This()) !AstNode_ {
            const expression = try this.pushNode(try this.parseExpressionWithLevel(.compare));
            var typeArguments: NodeRef = 0;
            if (this.lexer.token == .t_less_than or this.lexer.token == .t_less_than_less_than) {
                typeArguments = try this.parseTypeArgs();
            }

            // Reinterpret as a call exp
            if (this.lexer.token == .t_open_paren) {
                const call_args = try this.parseCallArgs();
                const call_exp = try this.pushNode(.{
                    .kind = .call_expression,
                    .data = toBinaryDataPtrRefs(expression, call_args),
                    .len = typeArguments,
                });

                typeArguments = 0;
                if (this.lexer.token == .t_less_than or this.lexer.token == .t_less_than_less_than) {
                    typeArguments = try this.parseTypeArgs();
                }

                return .{
                    .kind = .expression_with_type_arguments,
                    .data = toBinaryDataPtrRefs(call_exp, typeArguments),
                };
            }

            return .{
                .kind = .expression_with_type_arguments,
                .data = toBinaryDataPtrRefs(expression, typeArguments),
            };
        }

        fn parseCallSignature(this: *@This(), comptime has_new: bool) !AstNode_ {
            var type_parameters: NodeRef = 0;
            if (this.lexer.token == .t_less_than) {
                type_parameters = try this.parseTypeParams();
            }

            const params = try this.parseParameters();

            var return_type: NodeRef = 0;
            if (this.lexer.token == .t_colon) {
                try this.lexer.next();
                return_type = try this.parseType();
            }

            return .{
                .kind = if (comptime has_new) .construct_signature else .call_signature,
                .data = if (params > 0 or return_type > 0) toBinaryDataPtrRefs(params, return_type) else null,
                .len = type_parameters,
            };
        }

        fn finishIndexSignature(this: *@This(), name: NodeRef, flags: u22) !AstNode_ {
            try this.lexer.expect(.t_colon);
            const exp = try this.parseType();
            try this.lexer.expect(.t_close_bracket);
            try this.lexer.expect(.t_colon);
            const ty = try this.parseType();

            return .{
                .kind = .index_signature,
                .data = toBinaryDataPtrRefs(name, exp),
                .len = ty,
                .flags = flags,
            };
        }

        pub const ModifierKeywords = ComptimeStringMap(js_lexer.T, .{
            .{ "readonly", .t_readonly },
            .{ "get", .t_get },
            .{ "set", .t_set },
        });

        fn parseTypeMember(this: *@This()) !AstNode_ {
            // other code might not call `nextNoKeywords` 
            if (this.lexer.isIdentifierOrKeyword()) {
                const s = this.lexer.identifier;
                const ident = toIdentNodeWithLocation(s, this.getLocation(), this.lexer.full_start, this.getFullWidth());
                try this.lexer.nextNoKeywords();

                if (!this.lexer.has_newline_before and (this.lexer.token == .t_identifier or this.lexer.token == .t_open_bracket or this.lexer.token == .t_string_literal or this.lexer.token == .t_numeric_literal)) {
                    if (ModifierKeywords.get(s)) |t| {
                        switch (t) {
                            .t_readonly => {
                                if (this.lexer.token == .t_open_bracket) {
                                    if (try this.isIndexSignature()) {
                                        try this.lexer.next();
                                        const name = try this.parseIdentifier();
                                        return this.finishIndexSignature(name, @intFromEnum(NodeFlags.readonly));
                                    }
                                }
                                var n = try this.parsePropOrMethodDecl();
                                n.flags |= @intFromEnum(NodeFlags.readonly);
                                return n;
                            },
                            .t_get => {
                                return this.parseGetAccessor();
                            },
                            .t_set => {
                                return this.parseSetAccessor();
                            },
                            else => unreachable,
                        }
                    }
                    return this.emitParseError("Property or signature expected");
                }

                if (this.lexer.token == .t_open_paren or this.lexer.token == .t_less_than) {
                    if (strings.eqlComptime(s, "new")) {
                        return this.parseCallSignature(true);
                    }
                }

                return this.parsePropOrMethodDeclWithName(try this.pushNode(ident));
            }

            if (this.lexer.token == .t_open_bracket) {
                if (try this.isIndexSignature()) {
                    try this.lexer.next();
                    const name = try this.parseIdentifier();
                    return this.finishIndexSignature(name, 0);
                }

                return this.parsePropOrMethodDecl();
            }
    
            if (this.lexer.token == .t_open_paren or this.lexer.token == .t_less_than) {
                return this.parseCallSignature(false);
            }

            return this.parsePropOrMethodDecl();
        }

        fn parseTypeMembers(this: *@This()) !NodeRef {
            const prev_state = this.context_state;
            this.context_state = .interface;
            defer this.context_state = prev_state;

            try this.lexer.expectInInterfaceScope(.t_open_brace);
            var members = NodeList_.init(this);

            while (this.lexer.token != .t_close_brace) {
                try members.append(try this.parseTypeMember());
                if (this.lexer.token == .t_semicolon or this.lexer.token == .t_comma) {
                    try this.lexer.nextNoKeywords();
                }
            }

            try this.lexer.expect(.t_close_brace);

            return members.head;
        }

        fn parseMemberName(this: *@This()) !NodeRef {
            return switch (this.lexer.token) {
                .t_identifier => this.parseIdentifier(),
                .t_private_identifier => this.parsePrivateIdentifier(),
                .t_string_literal => this.pushNode(try this.parseStringLiteralLike()),
                .t_numeric_literal => this.pushNode(try this.parseNumericLiteral()),
                .t_open_bracket => {
                    try this.lexer.expect(.t_open_bracket);
                    const exp = try this.parseExpression();
                    try this.lexer.expect(.t_close_bracket);

                    return this.pushNode(.{
                        .kind = .computed_property_name,
                        .data = @ptrFromInt(exp),
                    });
                },
                else => {
                    try this.lexer.next();
                    return this.pushError("Invalid member name");
                },
            };
        }

        fn parseGetAccessor(this: *@This()) !AstNode_ {
            const name = try this.parseMemberName();

            // TODO: parse type params and emit an error if found

            const params = try this.parseParameters();

            var return_type: NodeRef = 0;
            if (this.lexer.token == .t_colon) {
                try this.lexer.next();
                return_type = try this.parseType();
            }

            var body: NodeRef = 0;
            if (this.lexer.token == .t_open_brace) {
                body = try this.parseBlock();
            } else {
                try this.maybeEatSemi();
            }

            return .{
                .kind = .get_accessor,
                .data = toBinaryDataPtrRefs(name, params),
                .len = body,
                .extra_data = return_type,
            };
        }

        fn parseSetAccessor(this: *@This()) !AstNode_ {
            const name = try this.parseMemberName();
            // TODO: parse type params and emit an error if found

            const params = try this.parseParameters();

            var return_type: NodeRef = 0;
            if (this.lexer.token == .t_colon) {
                try this.lexer.next();
                return_type = try this.parseType();
            }

            var body: NodeRef = 0;
            if (this.lexer.token == .t_open_brace) {
                body = try this.parseBlock();
            } else {
                try this.maybeEatSemi();
            }

            return .{
                .kind = .set_accessor,
                .data = toBinaryDataPtrRefs(name, params),
                .len = body,
                .extra_data = return_type,
            };
        }

        fn parsePropertyDeclWithName(this: *@This(), name: NodeRef) !AstNode_ {
            var flags: u22 = 0;

            if (this.lexer.token == .t_exclamation) {
                try this.lexer.next();
                flags |= @intFromEnum(NodeFlags.non_null);

                // TODO: parse error if missing a type annotation (colon)
                // TODO: parse error if assigned to
            }

            if (this.lexer.token == .t_question) {
                try this.lexer.next();
                flags |= @intFromEnum(NodeFlags.optional);

                // TODO: parse error if missing a type annotation (colon)
            }

            var ty: NodeRef = 0;
            if (this.lexer.token == .t_colon) {
                try this.lexer.next();
                ty = try this.parseType();
            }

            if (this.context_state == .interface) {
                return .{
                    .kind = .property_signature,
                    .data = toBinaryDataPtrRefs(name, ty),
                    .flags = flags,
                };
            }

            var initializer: NodeRef = 0;
            if (this.lexer.token == .t_equals) {
                try this.lexer.next();
                initializer = try this.parseExpression();
            }

            try this.maybeEatSemi();

            return .{
                .kind = .property_declaration,
                .data = toBinaryDataPtrRefs(name, initializer),
                .len = ty,
                .flags = flags,
            };
        }

        fn parseMethodDeclWithName(this: *@This(), name: NodeRef, flags: u22) !AstNode_ {
            const typeParameters = if (this.lexer.token == .t_less_than) try this.parseTypeParams() else 0;
            const parameters = try this.parseParameters();

            var return_type: NodeRef = 0;
            if (this.lexer.token == .t_colon) {
                try this.lexer.next();
                return_type = try this.parseType();
            }

            if (this.context_state == .interface) {
                return .{
                    .kind = .method_signature,
                    .data = toBinaryDataPtrRefs(name, parameters),
                    .len = return_type,
                    .extra_data = typeParameters,
                    .flags = flags,
                };
            }

            var body: NodeRef = 0;
            if (this.lexer.token == .t_open_brace) {
                body = try this.parseBlock();
            } else {
                if (this.lexer.token == .t_semicolon) {
                    try this.lexer.nextInClassScopeFlag();
                }
            }

            return .{
                .kind = .method_declaration,
                .data = toBinaryDataPtrRefs(name, parameters),
                .len = body,
                .flags = flags,
                .extra_data = typeParameters,
                .extra_data2 = return_type,
            };
        }

        fn parsePropertyDecl(this: *@This()) !AstNode_ {
            const name = try this.parseMemberName();

            return this.parsePropertyDeclWithName(name);
        }

        fn parseMethodDecl(this: *@This(), flags: u22) !AstNode_ {
            const name = try this.parseMemberName();

            return this.parseMethodDeclWithName(name, flags);
        }

        fn parseNumberProp(this: *@This()) !AstNode_ {
            const name = try this.pushNode(try this.parseNumericLiteral());

            return this.parsePropertyDeclWithName(name);
        }

        fn parsePropOrMethodDeclWithName(this: *@This(), name: u32) !AstNode_ {
            if (this.lexer.token == .t_open_paren or this.lexer.token == .t_less_than) {
                return this.parseMethodDeclWithName(name, 0);
            } else if (this.lexer.token == .t_question) {
                try this.lexer.next();
                if (this.lexer.token == .t_open_paren or this.lexer.token == .t_less_than) {
                    return this.parseMethodDeclWithName(name, @intFromEnum(NodeFlags.optional));
                }
                var n = try this.parsePropertyDeclWithName(name);
                n.flags |= @intFromEnum(NodeFlags.optional);
                return n;
            }

            return this.parsePropertyDeclWithName(name);
        }

        inline fn parsePropOrMethodDecl(this: *@This()) !AstNode_ {
            const name = try this.parseMemberName();

            return this.parsePropOrMethodDeclWithName(name);
        }

        fn parseConstructor(this: *@This()) !AstNode_ {
            // TODO: error if type params are present

            const params = try this.parseConstructorParameters();

            if (this.lexer.token == .t_colon) {
                // invalid typescript
                try this.lexer.next();
                _ = try this.parseType();
            }

            var body: NodeRef = 0;
            if (this.lexer.token == .t_open_brace) {
                body = try this.parseBlock();
            } else {
                try this.maybeEatSemi();
            }

            if (params == 0 and body == 0) {
                return .{
                    .kind = .constructor,
                    .data = null,
                };
            }

            return .{
                .kind = .constructor,
                .data = toBinaryDataPtrRefs(params, body),
            };
        }

        fn parseDecoratorNode(this: *@This()) !AstNode_ {
            try this.lexer.expect(.t_at);

            const exp = try this.parseExpressionWithLevel(.call);

            return .{
                .kind = .decorator,
                .data = @ptrFromInt(try this.pushNode(exp)),
            };
        }

        fn parseModifierOrMember(this: *@This(), flags: *u22, members: *NodeList_, comptime flag: NodeFlags) !void {
            const ident = toIdentNodeWithLocation(this.lexer.identifier, this.getLocation(), this.lexer.full_start, this.getFullWidth());
            try this.lexer.nextInClassScopeFlag(); // XXX: TODO: remove the "newline" hack

            if (this.lexer.has_newline_before) {
                var decl = try this.parsePropertyDeclWithName(try this.pushNode(ident));
                decl.flags |= flags.*;
                flags.* = 0;
                return members.append(decl);
            }

            if (comptime flag == .static) {
                if (this.lexer.token == .t_open_brace) {
                    const block = try this.parseBlockStatementsLinked();
                    flags.* = 0;
                    return members.append(.{
                        .kind = .class_static_block_declaration,
                        .data = @ptrFromInt(block),
                    });
                }
            }

            switch (this.lexer.token) {
                .t_less_than, .t_open_paren => {
                    const decl = try this.parseMethodDeclWithName(try this.pushNode(ident), flags.*);
                    flags.* = 0;
                    return members.append(decl);
                },
                .t_colon, .t_question => {
                    var decl = try this.parsePropertyDeclWithName(try this.pushNode(ident));
                    decl.flags |= flags.*;
                    flags.* = 0;
                    return members.append(decl);
                },
                else => {
                    flags.* |= @intFromEnum(flag);
                },
            }
        }

        fn parseClassMembers(this: *@This()) !NodeRef {
            const prior_state = this.context_state;
            this.context_state = .class;
            defer this.context_state = prior_state;

            try this.lexer.expectInClassScope(.t_open_brace);
            var members = NodeList_.init(this);
            var flags: u22 = 0;

            var decorators = NodeList_.init(this);

            while (this.lexer.token != .t_close_brace) {
                switch (this.lexer.token) {
                    .t_at => {
                        try decorators.append(try this.parseDecoratorNode());
                    },
                    .t_semicolon => {
                        try this.lexer.nextInClassScope();
                        try members.append(.{
                            .kind = .semicolon_class_element,
                        });
                    },
                    .t_public => {
                        try parseModifierOrMember(this, &flags, &members, .public);
                    },
                    .t_protected => {
                        try parseModifierOrMember(this, &flags, &members, .protected);
                    },
                    .t_private => {
                        try parseModifierOrMember(this, &flags, &members, .private);
                    },
                    .t_async => {
                        try parseModifierOrMember(this, &flags, &members, .@"async");
                    },
                    .t_override => {
                        try parseModifierOrMember(this, &flags, &members, .override);
                    },
                    .t_abstract => {
                        try parseModifierOrMember(this, &flags, &members, .abstract);
                    },
                    .t_readonly => {
                        try parseModifierOrMember(this, &flags, &members, .readonly);
                    },
                    .t_static => {
                        try parseModifierOrMember(this, &flags, &members, .static);
                    },
                    .t_asterisk => {
                        try this.lexer.nextInClassScopeFlag();
                        flags |= @intFromEnum(NodeFlags.generator);
                    },
                    // These _could_ be actual identifiers rather than accessor decls
                    .t_get, .t_set => {
                        const is_set = this.lexer.token == .t_set;
                        const ident = this.lexer.identifier;
                        const location = this.getLocation();
                        const full_start = this.lexer.full_start;
                        const width = this.getFullWidth();
                        try this.lexer.nextInClassScope();

                        // Parse as prop decl
                        if (this.lexer.token == .t_equals or this.lexer.token == .t_colon or this.lexer.token == .t_semicolon) {
                            const name = try this.pushNode(toIdentNodeWithLocation(ident, location, full_start, width));
                            const n = try this.parsePropertyDeclWithName(name);
                            try members.append(n);
                            flags = 0;
                        } else if (this.lexer.token == .t_open_paren or this.lexer.token == .t_less_than) {
                            // Parse as method decl
                            const name = try this.pushNode(toIdentNodeWithLocation(ident, location, full_start, width));
                            const n = try this.parseMethodDeclWithName(name, flags);
                            try members.append(n);
                            flags = 0;
                        } else {
                            var n = try if (is_set) this.parseSetAccessor() else this.parseGetAccessor();
                            n.flags = flags;
                            try members.append(n);
                            flags = 0;
                        }
                    },
                    .t_identifier => {
                        if (this.lexer.isContextualKeyword("constructor")) {
                            try this.lexer.next();
                            const n = try this.parseConstructor();
                            try members.append(n);
                            flags = 0;
                        } else {
                            var n = try this.parsePropOrMethodDecl();
                            n.flags = flags;
                            try members.append(n);
                            flags = 0;

                            if (decorators.head != 0) {
                                try this.decorators.put(this.lexer.allocator, members.prev, decorators.head);
                                decorators.reset();
                            }
                        }
                    },
                    .t_string_literal => {
                        // 'constructor'() is parsed as the class ctor
                        if (this.lexer.string_literal_is_ascii and strings.eqlComptime(this.lexer.string_literal_slice, "constructor")) {
                            try this.lexer.next();
                            const n = try this.parseConstructor();
                            try members.append(n);
                            flags = 0;
                        } else {
                            var n = try this.parsePropOrMethodDecl();
                            n.flags = flags;
                            try members.append(n);
                            flags = 0;

                            if (decorators.head != 0) {
                                try this.decorators.put(this.lexer.allocator, members.prev, decorators.head);
                                decorators.reset();
                            }
                        }
                    },
                    .t_private_identifier, .t_open_bracket => {
                        var n = try this.parsePropOrMethodDecl();
                        n.flags = flags;
                        try members.append(n);
                        flags = 0;

                        if (decorators.head != 0) {
                            try this.decorators.put(this.lexer.allocator, members.prev, decorators.head);
                            decorators.reset();
                        }
                    },
                    else => {
                        if (this.lexer.token.isKeyword()) {
                            var n = try this.parsePropOrMethodDeclWithName(try this.parseIdentifier());
                            n.flags = flags;
                            try members.append(n);
                            flags = 0;

                            if (decorators.head != 0) {
                                try this.decorators.put(this.lexer.allocator, members.prev, decorators.head);
                                decorators.reset();
                            }

                            continue;
                        }

                        _ = try this.emitParseError("");
                    },
                }
            }

            try this.lexer.expect(.t_close_brace);

            return members.head;
        }

        fn parseClassNode(this: *@This(), full_start: u32, comptime kind: SyntaxKind) !AstNode_ {
            try this.lexer.expect(.t_class);

            var name: NodeRef = 0;
            if (this.lexer.token == .t_identifier) {
                name = try this.parseIdentifier();
            }

            var typeParameters: NodeRef = 0;
            if (this.lexer.token == .t_less_than or this.lexer.token == .t_less_than_less_than) {
                typeParameters = try this.parseTypeParams();
            }

            var extendsClause: NodeRef = 0;
            if (this.lexer.token == .t_extends) {
                try this.lexer.next();
                extendsClause = try this.pushNode(try this.parseExprWithTypeArgs());
            }

            var implementsClauses = NodeList_.init(this);
            if (this.lexer.token == .t_implements) {
                try this.lexer.next();

                while (true) {
                    try implementsClauses.append(try this.parseExprWithTypeArgs());
                    if (this.lexer.token != .t_comma) break;
                    try this.lexer.next();

                    // if (this.lexer.token == .t_open_brace) {
                    //     _ = try this.pushError("Trailing comma not allowed");
                    //     break;
                    // }
                }
            }

            const members = try this.parseClassMembers();

            return .{
                .kind = kind,
                .data = if (name == 0 and members == 0) null else toBinaryDataPtrRefs(name, members),
                .len = extendsClause,
                .extra_data = typeParameters,
                .extra_data2 = implementsClauses.head,
                .full_start = full_start,
                .width = @as(u32, @intCast(this.lexer.full_start)) - full_start,
            };
        }

        inline fn maybeEatSemi(this: *@This()) !void {
            if (this.lexer.token == .t_semicolon) {
                try this.next();
            }
        }

        fn parseTypeAliasDecl(this: *@This()) !AstNode_ {
            const binding = try this.parseIdentifier();

            var typeParameters: NodeRef = 0;
            if (this.lexer.token == .t_less_than or this.lexer.token == .t_less_than_less_than) {
                typeParameters = try this.parseTypeParams();
            }

            try this.lexer.expect(.t_equals);
            const value = try this.parseType();

            return .{
                .kind = .type_alias_declaration,
                .data = toBinaryDataPtrRefs(binding, typeParameters),
                .len = value,
            };
        }

        fn parseInterfaceDecl(this: *@This()) !AstNode_ {
            const name = try this.parseIdentifier();

            var typeParameters: NodeRef = 0;
            if (this.lexer.token == .t_less_than or this.lexer.token == .t_less_than_less_than) {
                typeParameters = try this.parseTypeParams();
            }

            var extendsClauses: NodeRef = 0;
            if (this.lexer.token == .t_extends) {
                try this.lexer.next();
                var clauses = NodeList_.init(this);
                while (this.lexer.token != .t_open_brace) {
                    const exp = try this.parseExprWithTypeArgs();
                    try clauses.append(exp);

                    if (this.lexer.token == .t_open_brace) break;
                    try this.lexer.expect(.t_comma);
                }
                extendsClauses = clauses.head;
            }

            const members = try this.parseTypeMembers();

            return .{
                .kind = .interface_declaration,
                .data = toBinaryDataPtrRefs(name, members),
                .len = typeParameters,
                .extra_data = extendsClauses,
            };
        }

        fn parseEnumDecl(this: *@This()) !AstNode_ {
            const decl_name = try this.parseIdentifier();

            try this.lexer.expect(.t_open_brace);

            var members = NodeList_.init(this);
            while (this.lexer.token != .t_close_brace) {
                var name: NodeRef = 0;
                // Only ident/string literals allowed
                if (this.lexer.token == .t_identifier) {
                    name = try this.parseIdentifier();
                } else if (this.lexer.token == .t_string_literal) {
                    name = try this.pushNode(try this.parseStringLiteralLike());
                } else {
                    // parse error
                    try this.lexer.next();
                    continue;
                }

                var initializer: NodeRef = 0;
                if (this.lexer.token == .t_equals) {
                    try this.lexer.next();
                    initializer = try this.pushNode(try this.parseExpressionWithLevel(.comma));
                }

                try members.append(.{
                    .kind = .enum_member,
                    .data = toBinaryDataPtrRefs(name, initializer),
                });

                if (this.lexer.token == .t_close_brace) break;
                try this.lexer.expect(.t_comma);
            }

            try this.lexer.next();

            return .{
                .kind = .enum_declaration,
                .data = toBinaryDataPtrRefs(decl_name, members.head),
            };
        }

        fn finishForStatement(this: *@This(), initializer: NodeRef) !AstNode_ {
            try this.lexer.expect(.t_semicolon);

            var condition: NodeRef = 0;
            if (this.lexer.token != .t_semicolon) {
                condition = try this.pushNode(try this.parseExpressionStatement());
            } else {
                // to handle `for(;;)`
                try this.lexer.next();
            }

            var incrementor: NodeRef = 0;
            if (this.lexer.token != .t_close_paren) {
                incrementor = try this.pushNode(try this.parseExpressionStatement());
            }

            try this.lexer.expect(.t_close_paren);
            const statement = try this.pushNode(try this.parseStatement());

            return .{
                .kind = .for_statement,
                .data = toBinaryDataPtrRefsMaybeNull(initializer, condition),
                .len = incrementor,
                .extra_data = statement,
            };
        }

        fn finishForOfStatement(this: *@This(), initializer: NodeRef, is_async: bool) !AstNode_ {
            try this.lexer.expectContextualKeyword("of");

            const expression = try this.parseExpression();

            try this.lexer.expect(.t_close_paren);
            const statement = try this.pushNode(try this.parseStatement());

            return .{
                .kind = .for_of_statement,
                .data = toBinaryDataPtrRefs(initializer, expression),
                .len = statement,
                .flags = if (is_async) @intFromEnum(NodeFlags.@"async") else 0,
            };
        }

        fn finishForInStatement(this: *@This(), initializer: NodeRef) !AstNode_ {
            try this.lexer.expect(.t_in);
            return this.finishForInStatementWithExp(initializer, try this.parseExpression());
        }

        fn finishForInStatementWithExp(this: *@This(), initializer: NodeRef, exp: NodeRef) !AstNode_ {
            try this.lexer.expect(.t_close_paren);
            const statement = try this.pushNode(try this.parseStatement());

            return .{
                .kind = .for_in_statement,
                .data = toBinaryDataPtrRefs(initializer, exp),
                .len = statement,
            };
        }

        fn parseForStatement(this: *@This()) !AstNode_ {
            try this.lexer.expect(.t_for);

            var is_async = false;
            if (this.lexer.token == .t_await) {
                try this.lexer.next();
                is_async = true;
            }

            try this.lexer.expect(.t_open_paren);

            // It's definitely a normal for statement with no initializer
            if (this.lexer.token == .t_semicolon) {
                return this.finishForStatement(0);
            }

            // `ForOf` and `ForIn` must have a variable binding on the LHS
            // This is equivalent to an assignment statement
            //
            // The normal `For` statement is allowed to have anything in the initializer position
            var flags: u22 = 0;

            if (this.lexer.token == .t_let) {
                try this.lexer.next();
                flags |= @intFromEnum(NodeFlags.let);
            } else if (this.lexer.token == .t_const) {
                try this.lexer.next();
                flags |= @intFromEnum(NodeFlags.@"const");
            } else if (this.lexer.token == .t_var) {
                try this.lexer.next();
                flags |= @intFromEnum(NodeFlags.override); // XXX: needed because we branch on `flags`
            } else if (this.lexer.token == .t_await) {
                // Could be an await expression for normal for loops
                try this.lexer.next();
                if (this.lexer.isContextualKeyword("using")) {
                    try this.lexer.next();
                    flags |= @intFromEnum(NodeFlags.await_using);
                } else {
                    // Parse remainder as a normal for statement
                    const n = try this.pushNode(.{
                        .kind = .await_expression,
                        .data = @ptrFromInt(try this.parseExpression()),
                    });

                    return this.finishForStatement(n);
                }
            } else if (this.lexer.isContextualKeyword("using")) {
                try this.lexer.next();
                flags |= @intFromEnum(NodeFlags.using);
            }

            // We're parsed all binding modifiers
            // If we have any, then we should expect a variable statement
            // Otherwise any expression that can appear on the LHS of an assignment statement can appear here

            if (flags != 0) {
                const statement = try this.parseVariableStatement(flags);
                if (this.lexer.token == .t_semicolon) {
                    return this.finishForStatement(try this.pushNode(statement));
                }
                if (this.lexer.token == .t_in) {
                    return this.finishForInStatement(try this.pushNode(statement));
                }

                return this.finishForOfStatement(try this.pushNode(statement), is_async);
            }

            const exp = try this.parseExpressionWithLevel(.lowest);
            if (this.lexer.token == .t_semicolon) {
                return this.finishForStatement(try this.pushNode(exp));
            }

            // We probably parsed `in` as a binary expression
            if (this.lexer.token == .t_close_paren) {
                if (exp.kind == .binary_expression and exp.len == @intFromEnum(SyntaxKind.in_keyword)) {
                    const d: BinaryExpData = @bitCast(@intFromPtr(exp.data orelse return error.MissingData));

                    return this.finishForInStatementWithExp(d.left, d.right);
                }

                // Bad parse
                try this.lexer.expect(.t_close_paren);

                return this.emitParseError("");
            }

            if (this.lexer.isContextualKeyword("of")) {
                return this.finishForOfStatement(try this.pushNode(exp), is_async);
            }

            // Bad parse
            try this.lexer.expect(.t_close_paren);

            return this.emitParseError("");
        }

        fn parseSwitchCase(this: *@This()) !AstNode_ {
            var statements = NodeList_.init(this);

            var n: AstNode_ = .{ .kind = .case_clause };
            if (this.lexer.token == .t_default) {
                try this.lexer.next();
                try this.lexer.expect(.t_colon);
                n.kind = .default_clause;
            } else if (this.lexer.token == .t_case) {
                try this.lexer.next();
                if (this.options.is_syn and this.lexer.isContextualKeyword("is")) {
                    try this.lexer.next();
                    n.data = @ptrFromInt(try this.parseType());
                    n.flags |= @intFromEnum(NodeFlags.declare);
                } else {
                    n.data = @ptrFromInt(try this.parseExpression());
                }
                try this.lexer.expect(.t_colon);
            } else {
                std.debug.print("{any}", .{this.lexer.token});
                return error.SyntaxError;
            }

            while (true) {
                if (this.lexer.token == .t_default or this.lexer.token == .t_case or this.lexer.token == .t_close_brace) break;

                try statements.append(try this.parseStatement());
            }

            n.len = statements.head;

            return n;
        }

        fn parseSwitchStatement(this: *@This()) !AstNode_ {
            try this.lexer.expect(.t_switch);

            try this.lexer.expect(.t_open_paren);
            const expression = try this.parseExpOrBinding();
            try this.lexer.expect(.t_close_paren);

            try this.lexer.expect(.t_open_brace);

            var clauses = NodeList_.init(this);

            while (this.lexer.token != .t_close_brace) {
                try clauses.append(try this.parseSwitchCase());
            }

            try this.lexer.expect(.t_close_brace);

            return .{
                .kind = .switch_statement,
                .data = toBinaryDataPtrRefs(expression, clauses.head),
            };
        }

        fn parseExpressionStatement(this: *@This()) !AstNode_ {
            const exp = try this.parseExpression();
            try this.maybeEatSemi();

            return .{
                .kind = .expression_statement,
                .data = @ptrFromInt(exp),
            };
        }

        fn parseModuleAttributes(this: *@This()) !AstNode_ {
            const exp = try this.parseObjectLiteral();

            // TODO: validate the literal

            return .{
                .kind = .import_attributes,
                .data = exp.data,
            };
        }

        fn parseNamedModuleBindings(this: *@This(), comptime is_export: bool) !NodeRef {
            try this.lexer.expect(.t_open_brace);

            var elements = NodeList_.init(this);

            while (this.lexer.token != .t_close_brace) {
                var type_only = false;
                if (this.lexer.isContextualKeyword("type")) {
                    try this.lexer.next();
                    type_only = true;
                }

                const ident: NodeRef = blk: {
                    if (is_export and this.lexer.token == .t_default) {
                        try this.lexer.next();
                        break :blk try this.pushNode(.{ .kind = .default_keyword });
                    }

                    break :blk try this.parseIdentifier();
                };

                const alias: NodeRef = blk: {
                    if (this.lexer.isContextualKeyword("as")) {
                        try this.lexer.next();
                        break :blk try this.parseIdentifier();
                    } else {
                        break :blk 0;
                    }
                };

                try elements.append(.{
                    .kind = if (is_export) .export_specifier else .import_specifier,
                    .data = toBinaryDataPtrRefs(ident, alias),
                    .flags = if (type_only) @intFromEnum(NodeFlags.declare) else 0,
                });

                if (this.lexer.token == .t_close_brace) break;
                try this.lexer.expect(.t_comma);
            }

            try this.lexer.expect(.t_close_brace);

            return elements.head;
        }

        fn parseImportBindings(this: *@This()) !AstNode_ {
            if (this.lexer.token == .t_asterisk) {
                // Namespace import
                try this.lexer.next();
                try this.lexer.expectContextualKeyword("as");
                const ident = try this.parseIdentifier();

                return .{
                    .kind = .namespace_import,
                    .data = @ptrFromInt(ident),
                };
            }

            const imports = try this.parseNamedModuleBindings(false);

            return .{
                .kind = .named_imports,
                .data = @ptrFromInt(imports),
            };
        }

        fn parseImportEquals(this: *@This(), name: NodeRef, flags: u22) !AstNode_ {
            try this.lexer.next();

            var rhs: NodeRef = undefined;
            if (this.lexer.isContextualKeyword("require")) {
                var ident = try this.parseIdentifierNode();
                ident.flags = 1 << 19; // Mark as a type

                if (this.lexer.token == .t_open_paren) {
                    try this.lexer.next();
                    const spec = try this.parseStringLiteralLikeKind(.string_literal);
                    try this.lexer.expect(.t_close_paren);

                    rhs = try this.pushNode(.{
                        .kind = .external_module_reference,
                        .data = @ptrFromInt(try this.pushNode(spec)),
                    });
                } else {
                    rhs = try this.pushNode(try this.parseRemainingTypeNode(ident, .lowest));
                }
            } else {
                rhs = try this.parseType();
            }

            try this.maybeEatSemi();

            return .{
                .kind = .import_equals_declaration,
                .data = toBinaryDataPtrRefs(name, rhs),
                .flags = flags,
            };
        }

        fn parseImportClause(this: *@This()) !AstNode_ {
            var name: NodeRef = 0;
            var bindings: NodeRef = 0;

            if (this.lexer.token == .t_identifier) {
                name = try this.parseIdentifier();

                if (this.lexer.token == .t_comma) {
                    try this.lexer.next();
                    bindings = try this.pushNode(try this.parseImportBindings());
                } else if (this.lexer.token == .t_equals) {
                    // We want to bail out on `import X = ...`
                    return this.parseImportEquals(name, 0);
                }
            } else if (this.lexer.token == .t_asterisk or this.lexer.token == .t_open_brace) {
                bindings = try this.pushNode(try this.parseImportBindings());
            } else {
                std.debug.print("{any}", .{this.lexer.token});
                return error.SyntaxError;
            }

            return .{
                .kind = .import_clause,
                .data = toBinaryDataPtrRefs(name, bindings),
            };
        }

        fn parseImportDeclaration(this: *@This(), full_start: u32) !AstNode_ {
            var clause: NodeRef = 0;
            var specifier: NodeRef = 0;
            var attributes: NodeRef = 0;

            // `type` can only be used on an import without both parts of the clause
            var type_only = false;
            if (this.lexer.isContextualKeyword("type")) {
                try this.lexer.next();
                type_only = true;
            }

            if (this.lexer.token == .t_string_literal) {
                // Side-effect import
                specifier = try this.pushNode(try this.parseStringLiteralLike());
            } else {
                const clause_node = try this.parseImportClause();
                if (clause_node.kind == .import_equals_declaration) {
                    return clause_node;
                }

                clause = try this.pushNode(clause_node);

                try this.lexer.expectContextualKeyword("from");

                if (this.lexer.token != .t_string_literal) {
                    std.debug.print("{any}", .{this.lexer.token});
                    return error.SyntaxError;
                }

                specifier = try this.pushNode(try this.parseStringLiteralLike());
            }

            if (this.lexer.token == .t_with) {
                try this.lexer.next();
                attributes = try this.pushNode(try this.parseModuleAttributes());
            }

            try this.maybeEatSemi();

            return .{
                .kind = .import_declaration,
                .data = toBinaryDataPtrRefs(clause, specifier),
                .len = attributes,
                .flags = if (type_only) @intFromEnum(NodeFlags.declare) else 0,
                .full_start = full_start,
                .width = @as(u32, @intCast(this.lexer.full_start)) - full_start,
            };
        }

        fn parseExportDeclaration(this: *@This(), type_only: bool) !AstNode_ {
            var clause: NodeRef = 0;
            var specifier: NodeRef = 0;
            var attributes: NodeRef = 0;

            var try_parse_with = false;

            if (this.lexer.token == .t_asterisk) {
                try this.lexer.next();

                if (this.lexer.isContextualKeyword("as")) {
                    try this.lexer.next();
                    clause = try this.pushNode(.{
                        .kind = .namespace_export,
                        .data = @ptrFromInt(try this.parseIdentifier()),
                    });
                }

                try_parse_with = true;
                try this.lexer.expectContextualKeyword("from");
                specifier = try this.pushNode(try this.parseStringLiteralLike());
            } else if (this.lexer.token == .t_open_brace) {
                const exports = try this.parseNamedModuleBindings(true);

                clause = try this.pushNode(.{
                    .kind = .named_exports,
                    .data = @ptrFromInt(exports),
                });

                // We might not have `from`
                if (this.lexer.isContextualKeyword("from")) {
                    try_parse_with = true;
                    try this.lexer.next();
                    specifier = try this.pushNode(try this.parseStringLiteralLike());
                }
            }

            if (try_parse_with and this.lexer.token == .t_with) {
                try this.lexer.next();
                attributes = try this.pushNode(try this.parseModuleAttributes());
            }

            return .{
                .kind = .export_declaration,
                .data = toBinaryDataPtrRefs(clause, specifier),
                .len = attributes,
                .flags = if (type_only) @intFromEnum(NodeFlags.declare) else 0,
            };
        }

        // Decorators
        // * Decorator expressions are restricted to a chain of variables, property access with . but not [], and calls (). To use an arbitrary expression as a decorator, @(expression) is an escape hatch.
        // * Class expressions may be decorated, not just class declarations.
        // * Class decorators may exclusively come before, or after, export/export default.

        // ASI
        // http://www.ecma-international.org/ecma-262/7.0/index.html#sec-rules-of-automatic-semicolon-insertion

        // UpdateExpression :
        //     LeftHandSideExpression [no LineTerminator here] ++
        //     LeftHandSideExpression [no LineTerminator here] --

        // ContinueStatement :
        //     continue ;
        //     continue [no LineTerminator here] LabelIdentifier ;

        // BreakStatement :
        //     break ;
        //     break [no LineTerminator here] LabelIdentifier ;

        // ReturnStatement :
        //     return ;
        //     return [no LineTerminator here] Expression ;

        // ThrowStatement :
        //     throw [no LineTerminator here] Expression ;

        // ArrowFunction :
        //     ArrowParameters [no LineTerminator here] => ConciseBody

        // YieldExpression :
        //     yield [no LineTerminator here] * AssignmentExpression
        //     yield [no LineTerminator here] AssignmentExpression

        fn parseLocalDeclaration(this: *@This(), full_start: u32, flagsInit: u22) anyerror!AstNode_ {
            var flags = flagsInit;

            switch (this.lexer.token) {
                .t_const => {
                    try this.lexer.next();
                    flags |= @intFromEnum(NodeFlags.@"const");

                    if (this.lexer.token == .t_enum) {
                        try this.lexer.next();
                        var n = try this.parseEnumDecl();
                        n.flags = flags;

                        return n;
                    }

                    const n = try this.parseVariableStatement(flags);
                    try this.maybeEatSemi();
                    return n;
                },
                .t_let, .t_var => {
                    if (this.lexer.token == .t_let) {
                        flags |= @intFromEnum(NodeFlags.let);
                    }
                    try this.lexer.next();

                    const n = try this.parseVariableStatement(flags);
                    try this.maybeEatSemi();
                    return n;
                },
                .t_function => {
                    return this.parseFnDecl(full_start, flags, .function_declaration);
                },
                .t_class => {
                    var n = try this.parseClassNode(full_start, .class_declaration);
                    n.flags = flags;

                    return n;
                },
                .t_interface => {
                    try this.lexer.next();
                    var n = try this.parseInterfaceDecl();
                    n.flags = flags;

                    return n;
                },
                .t_enum => {
                    try this.lexer.next();
                    var n = try this.parseEnumDecl();
                    n.flags = flags;

                    return n;
                },
                .t_async => {
                    try this.lexer.next();
                    flags |= @intFromEnum(NodeFlags.@"async");

                    return this.parseLocalDeclaration(full_start, flags);
                },
                .t_identifier => {
                    if (this.lexer.isContextualKeyword("declare")) {
                        try this.lexer.next();
                        return this.parseLocalDeclaration(full_start, flags | @intFromEnum(NodeFlags.declare));
                    }

                    if (this.lexer.isContextualKeyword("namespace")) {
                        try this.lexer.next();

                        return this.parseNamespaceOrModule(0, .namespace);
                    }

                    if (this.lexer.isContextualKeyword("module")) {
                        try this.lexer.next();

                        return this.parseNamespaceOrModule(0, .none);
                    }

                    if (this.lexer.isContextualKeyword("abstract")) {
                        // Double abstract = error
                        if (flags & @intFromEnum(NodeFlags.abstract) == @intFromEnum(NodeFlags.abstract)) {
                            _ = try this.emitParseError("Double abstract");
                        } else {
                            flags |= @intFromEnum(NodeFlags.abstract);
                        }
                        try this.lexer.next();

                        return this.parseLocalDeclaration(full_start, flags);
                    }

                    // TODO: global is valid here if already in an ambient context
                },
                else => {},
            }

            return this.emitParseError("");
        }

        const ModuleDeclarationSubtype = enum {
            none,
            global,
            namespace,
        };

        fn parseNamespaceOrModule(this: *@This(), flags: u22, comptime subtype: ModuleDeclarationSubtype) !AstNode_ {
            const is_declared = flags & @intFromEnum(NodeFlags.declare) == @intFromEnum(NodeFlags.declare);
            const name = switch (subtype) {
                .none => try if (this.lexer.token == .t_identifier)
                    this.parseIdentifier()
                else
                    this.pushNode(try this.parseStringLiteralLike()),
                .namespace => try this.parseIdentifier(),
                .global => 0,
            };

            var block: NodeRef = 0;
            if (!is_declared or this.lexer.token == .t_open_brace) {
                block = try this.parseBlock();
            } else if (this.lexer.token == .t_semicolon) {
                try this.lexer.next();
            }

            return .{
                .kind = .module_declaration,
                .data = toBinaryDataPtrRefs(name, block),
                .flags = if (subtype == .namespace) flags | @intFromEnum(NodeFlags.let) else flags,
            };
        }

        fn parseExpressionStatementFromIdent(this: *@This(), ident: AstNode_) !AstNode_ {
            const n = try this.parseRemainingExpression(.lowest, ident);
            try this.maybeEatSemi();

            return .{
                .kind = .expression_statement,
                .data = @ptrFromInt(try this.pushNode(n)),
            };
        }

        fn parseDeferStatement(this: *@This()) !AstNode_ {
            const ident = try this.parseIdentifierNode();
            switch (this.lexer.token) {
                .t_open_brace => {
                    const statement = try this.parseBlock();

                    return .{
                        .kind = .defer_statement,
                        .data = @ptrFromInt(statement),
                    };
                },
                .t_if, .t_while, .t_for, .t_do, .t_switch, .t_try => {
                    const statement = try this.parseStatement();

                    return .{
                        .kind = .defer_statement,
                        .data = @ptrFromInt(try this.pushNode(statement)),
                    };
                },
                .t_identifier, .t_this, .t_super, .t_void, .t_await, .t_delete, .t_typeof, .t_plus_plus, .t_minus_minus => {
                    const statement = try this.parseExpressionStatement();

                    return .{
                        .kind = .defer_statement,
                        .data = @ptrFromInt(try this.pushNode(statement)),
                    };
                },
                .t_open_paren => {
                    // Any whitespace means it's a defer statement rather than a call expression
                    // Not ideal, but I'd wager that this aligns with the vast majority of
                    // user expectations. It's possible that this parsing behavior isn't backwards
                    // compatible with all existing JS code. So it will likely need to be revisited
                    // for `defer` to become apart of the official spec.
                    if (this.lexer.full_start != this.lexer.start) {
                        const statement = try this.parseExpressionStatement();

                        return .{
                            .kind = .defer_statement,
                            .data = @ptrFromInt(try this.pushNode(statement)),
                        };
                    }
                },
                else => {},
            }

            // `defer` might be a label
            if (this.lexer.token != .t_colon) {
                const n = try this.parseRemainingExpression(.lowest, ident);
                try this.maybeEatSemi();

                return .{
                    .kind = .expression_statement,
                    .data = @ptrFromInt(try this.pushNode(n)),
                };
            }

            try this.lexer.next();

            return .{
                .kind = .labeled_statement,
                .data = try this.toBinaryDataPtr(ident, try this.parseStatement()),
            };
        }

        fn parseCatchClause(this: *@This()) !NodeRef {
            try this.lexer.next();
            var variable_decl: NodeRef = 0;
            if (this.lexer.token == .t_open_paren) {
                try this.lexer.next();
                const decl = try this.parseVariableDeclLike();
                variable_decl = try this.pushNode(decl);

                try this.lexer.expect(.t_close_paren);
            }

            const block = try this.parseBlock();
            return try this.pushNode(.{
                .kind = .catch_clause,
                .data = toBinaryDataPtrRefs(variable_decl, block),
            });
        }

        inline fn parseExpOrBinding(this: *@This()) !NodeRef {
            if (!comptime enable_conditional_bindings) {
                return this.parseExpression();
            }

            return switch (this.lexer.token) {
                .t_let, .t_const => {
                    const flags: u22 = @intFromEnum(if (this.lexer.token == .t_let) NodeFlags.let else NodeFlags.@"const");
                    try this.lexer.next();
                    return this.pushNode(try this.parseVariableStatement(flags));
                },
                else => this.parseExpression(),
            };
        }

        fn parseStatement(this: *@This()) !AstNode_ {
            const full_start = this.lexer.full_start;
            switch (this.lexer.token) {
                .t_semicolon => {
                    try this.lexer.next();
                    return .{ .kind = .empty_statement };
                },
                .t_throw => {
                    try this.lexer.next();

                    const exp = try this.parseExpression();
                    try this.maybeEatSemi();

                    return .{
                        .kind = .throw_statement,
                        .data = @ptrFromInt(exp),
                    };
                },
                .t_open_brace => return this.parseBlockNode(),
                .t_import => {
                    try this.lexer.next();

                    if (this.lexer.token == .t_dot or this.lexer.token == .t_open_paren) {
                        return this.parseRemainingImportExpression();
                    }

                    return this.parseImportDeclaration(full_start);
                },
                .t_export => {
                    try this.lexer.next();

                    // Legacy TS syntax (equivalent to default export)
                    if (this.lexer.token == .t_equals) {
                        try this.lexer.next();
                        const exp = try this.parseExpressionStatement();

                        return .{
                            .kind = .export_assignment,
                            .data = @ptrFromInt(try this.pushNode(exp)),
                            .len = 1, // isImportEquals
                        };
                    }

                    var is_default = false;
                    if (this.lexer.token == .t_default) {
                        try this.lexer.next();
                        is_default = true;
                    }

                    // Parse a statement
                    if (is_default) {
                        if (this.lexer.token == .t_class or this.lexer.token == .t_function) {
                            return this.parseLocalDeclaration(full_start, @intFromEnum(NodeFlags.default_export));
                        }

                        return .{
                            .kind = .export_assignment,
                            .data = @ptrFromInt(try this.pushNode(try this.parseExpressionStatement())),
                        };
                    }

                    // Could be `export type <ident> =`
                    if (this.lexer.isContextualKeyword("type")) {
                        try this.lexer.next();

                        if (this.lexer.token == .t_identifier) {
                            var n = try this.parseTypeAliasDecl();
                            n.flags |= @intFromEnum(NodeFlags.@"export");

                            return n;
                        }

                        // FIXME: This should be an error?
                        return this.parseExportDeclaration(true);
                    }

                    if (this.lexer.token == .t_open_brace or this.lexer.token == .t_asterisk) {
                        return this.parseExportDeclaration(false);
                    }

                    // Legacy TS syntax (equivalent to reexport)
                    if (this.lexer.token == .t_import) {
                        try this.lexer.next();
                        const name = try this.parseIdentifier();

                        return this.parseImportEquals(name, @intFromEnum(NodeFlags.@"export"));

                        // try this.lexer.expect(.t_equals);
                        // const right = try this.parseType();
                        // try this.maybeEatSemi();

                        // return .{
                        //     .kind = .import_equals_declaration,
                        //     .flags = @intFromEnum(NodeFlags.@"export"),
                        //     .data = toBinaryDataPtrRefs(ident, right),
                        // };
                    }

                    return this.parseLocalDeclaration(full_start, @intFromEnum(NodeFlags.@"export"));
                },
                .t_class, .t_function, .t_const, .t_let, .t_var => return this.parseLocalDeclaration(full_start, 0),
                .t_yield => return this.parseExpressionStatement(),
                .t_reify => return this.parseExpressionStatement(),
                .t_interface => {
                    try this.lexer.next();
                    return this.parseInterfaceDecl();
                },
                .t_enum => {
                    try this.lexer.next();
                    return this.parseEnumDecl();
                },
                .t_async => {
                    try this.lexer.next();

                    if (this.lexer.token == .t_function) {
                        return this.parseFnDeclaration(full_start, @intFromEnum(NodeFlags.@"async"));
                    }

                    return this.parseArrowFn(full_start, @intFromEnum(NodeFlags.@"async"));
                },
                .t_await => {
                    try this.lexer.next();

                    if (this.lexer.isContextualKeyword("using")) {
                        try this.lexer.next();
                        const n = try this.parseVariableStatement(@intFromEnum(NodeFlags.await_using));
                        try this.maybeEatSemi();
                        return n;
                    }

                    const n = AstNode_{ .kind = .await_expression, .data = @ptrFromInt(try this.parseExpression()) };
                    try this.maybeEatSemi();

                    return n;
                },
                .t_identifier => {
                    if (js_lexer.ExtraKeyword.List.get(this.lexer.identifier)) |k| {
                        switch (k) {
                            .t_type => {
                                const ident = try this.parseIdentifierNode();
                                if (this.lexer.token != .t_identifier) {
                                    return this.parseExpressionStatementFromIdent(ident);
                                }

                                return this.parseTypeAliasDecl();
                            },
                            .t_using => {
                                const ident = try this.parseIdentifierNode();
                                if (this.lexer.token != .t_identifier) {
                                    return this.parseExpressionStatementFromIdent(ident);
                                }
                                const n = try this.parseVariableStatement(@intFromEnum(NodeFlags.using));
                                try this.maybeEatSemi();
                                return n;
                            },
                            // Legacy typescript feature
                            .t_namespace => {
                                const ident = try this.parseIdentifierNode();
                                if (this.lexer.token != .t_identifier) {
                                    return this.parseExpressionStatementFromIdent(ident);
                                }

                                return this.parseNamespaceOrModule(0, .namespace);
                            },
                            .t_module => {
                                const ident = try this.parseIdentifierNode();
                                if (this.lexer.token != .t_string_literal and this.lexer.token != .t_identifier) {
                                    return this.parseExpressionStatementFromIdent(ident);
                                }

                                return this.parseNamespaceOrModule(0, .none);
                            },
                            .t_abstract => {
                                // Technically doesn't handle labels `abstract: `...
                                // This applies to the other branches too

                                const ident = try this.parseIdentifierNode();

                                if (this.lexer.token == .t_class) {
                                    return this.parseLocalDeclaration(full_start, @intFromEnum(NodeFlags.abstract));
                                }

                                return this.parseExpressionStatementFromIdent(ident);
                            },
                            .t_declare => {
                                try this.lexer.next();

                                // This is allowed but I'm not sure what it does
                                if (this.lexer.isContextualKeyword("type")) {
                                    try this.lexer.next();

                                    return this.parseTypeAliasDecl();
                                }

                                if (this.lexer.isContextualKeyword("namespace")) {
                                    try this.lexer.next();

                                    return this.parseNamespaceOrModule(@intFromEnum(NodeFlags.declare), .namespace);
                                }

                                if (this.lexer.isContextualKeyword("module")) {
                                    try this.lexer.next();

                                    return this.parseNamespaceOrModule(@intFromEnum(NodeFlags.declare), .none);
                                }

                                if (this.lexer.isContextualKeyword("global")) {
                                    try this.lexer.next();

                                    return this.parseNamespaceOrModule(@intFromEnum(NodeFlags.declare), .global);
                                }

                                return this.parseLocalDeclaration(full_start, @intFromEnum(NodeFlags.declare));
                            },
                            .t_defer => return this.parseDeferStatement(),
                        }
                    }

                    const ident = try this.parseIdentifierNode();
                    if (this.lexer.token != .t_colon) {
                        if (comptime is_declaration_file) {
                            if (this.lexer.token == .t_open_brace and strings.eqlComptime(getSlice(&ident.toAstNode(), u8), "global")) {
                                return this.parseNamespaceOrModule(0, .global);
                            }
                        }

                        return this.parseExpressionStatementFromIdent(ident);
                    }

                    try this.lexer.next();

                    return .{
                        .kind = .labeled_statement,
                        .data = try this.toBinaryDataPtr(ident, try this.parseStatement()),
                    };
                },
                .t_for => return this.parseForStatement(),
                .t_switch => return this.parseSwitchStatement(),
                .t_while => {
                    try this.lexer.next();
                    try this.lexer.expect(.t_open_paren);
                    const expression = try this.parseExpOrBinding();
                    try this.lexer.expect(.t_close_paren);
                    const statement = try this.pushNode(try this.parseStatement());

                    return .{
                        .kind = .while_statement,
                        .data = toBinaryDataPtrRefs(expression, statement),
                    };
                },
                .t_do => {
                    try this.lexer.next();
                    const statement = try this.pushNode(try this.parseStatement());
                    try this.lexer.expect(.t_while);
                    try this.lexer.expect(.t_open_paren);
                    const expression = try this.parseExpression();
                    try this.lexer.expect(.t_close_paren);
                    try this.maybeEatSemi();

                    return .{
                        .kind = .do_statement,
                        .data = toBinaryDataPtrRefs(statement, expression),
                    };
                },
                .t_if => {
                    try this.lexer.next();
                    try this.lexer.expect(.t_open_paren);
                    const expression = try this.parseExpOrBinding();
                    try this.lexer.expect(.t_close_paren);
                    const thenStatement = try this.pushNode(try this.parseStatement());

                    var elseStatement: NodeRef = 0;
                    if (this.lexer.token == .t_else) {
                        try this.lexer.next();
                        elseStatement = try this.pushNode(try this.parseStatement());
                    }

                    return .{
                        .kind = .if_statement,
                        .data = toBinaryDataPtrRefs(expression, thenStatement),
                        .len = elseStatement,
                    };
                },
                .t_try => {
                    try this.lexer.next();
                    const tryBlock = try this.parseBlock();

                    const catchClause: NodeRef = try this.parseCatchClause();

                    var finallyBlock: NodeRef = 0;
                    if (this.lexer.token == .t_finally) {
                        try this.lexer.next();
                        finallyBlock = try this.parseBlock();
                    }

                    return .{
                        .kind = .try_statement,
                        .data = toBinaryDataPtrRefs(tryBlock, catchClause),
                        .len = finallyBlock,
                    };
                },
                .t_return => {
                    try this.lexer.next();
                    var n = AstNode_{ .kind = .return_statement };
                    if (this.lexer.token == .t_semicolon) {
                        try this.lexer.next();
                        return n;
                    }

                    if (this.lexer.has_newline_before or this.lexer.token == .t_close_brace) {
                        return n;
                    }

                    n.data = @ptrFromInt(try this.parseExpression());
                    try this.maybeEatSemi();

                    return n;
                },
                .t_continue => {
                    try this.lexer.next();
                    var n = AstNode_{ .kind = .continue_statement };
                    if (!this.lexer.has_newline_before and this.lexer.token == .t_identifier) {
                        n.data = @ptrFromInt(try this.parseIdentifier());
                    }

                    try this.maybeEatSemi();

                    return n;
                },
                .t_break => {
                    try this.lexer.next();
                    var n = AstNode_{ .kind = .break_statement };
                    if (!this.lexer.has_newline_before and this.lexer.token == .t_identifier) {
                        n.data = @ptrFromInt(try this.parseIdentifier());
                    }

                    try this.maybeEatSemi();

                    return n;
                },
                .t_with => {
                    // deprecated syntax
                    try this.lexer.next();
                    try this.lexer.expect(.t_open_paren);
                    const exp = try this.parseExpression();
                    try this.lexer.expect(.t_close_paren);
                    const statement = try this.pushNode(try this.parseStatement());

                    return .{
                        .kind = .with_statement,
                        .data = toBinaryDataPtrRefs(exp, statement),
                    };
                },
                else => return this.parseExpressionStatement(),
            }

            return this.emitParseError("");
        }

        fn maybeParseTripleSlashDirective(allocator: std.mem.Allocator, text: []const u8) !TripleSlashDirective {
            // /// <reference lib="decorators" />
            // /// <reference types="synapse" />
            // /// <reference no-default-lib="true"/>

            var l = try js_lexer.Lexer.init(.{ .contents = text }, allocator);
            l.print_expect = false;
            defer l.deinit();

            try l.expectInsideJSXElement(.t_less_than);
            if (!l.isContextualKeyword("reference")) return error.InvalidPragma;
            try l.nextInsideJSXElement();

            var kind: TripleSlashDirective.Kind = undefined;
            if (l.isContextualKeyword("types")) {
                kind = .types;
            } else if (l.isContextualKeyword("lib")) {
                kind = .lib;
            } else if (l.isContextualKeyword("no-default-lib")) {
                kind = .no_default_lib;
            } else if (l.isContextualKeyword("path")) {
                kind = .path;
            } else {
                return error.InvalidArgName;
            }

            try l.nextInsideJSXElement();
            try l.expectInsideJSXElement(.t_equals);
            if (l.token != .t_string_literal) return error.InvalidArgValue;

            const value = l.string_literal_slice;
            try l.nextInsideJSXElement();
            try l.expectInsideJSXElement(.t_slash);
            try l.expectInsideJSXElement(.t_greater_than);

            return .{ .kind = kind, .value = value };
        }

        pub fn parse(this: *@This()) !ParseResult {
            try this.lexer.preAllocate();
            _ = try this.pushNode(.{ .kind = .start });

            var directives = std.ArrayList(TripleSlashDirective).init(this.node_allocator.pages.allocator);

            while (this.lexer.token != .t_end_of_file) {
                switch (this.lexer.token) {
                    .t_single_line_comment => {
                        const text = this.lexer.source.contents[this.lexer.start..this.lexer.end];
                        if (text.len > 2 and text[2] == '/') {
                            if (maybeParseTripleSlashDirective(getAllocator(), text[3..]) catch null) |d| {
                                try directives.append(d);
                            }
                        }
                    },
                    .t_hashbang, .t_multi_line_comment => {},
                    else => {
                        if (!comptime skip_trivia) {
                            // Reset so we include all pre-parsed trivia
                            this.lexer.full_start = 0;
                        }
                        break;
                    },
                }
                try this.lexer.next();
            }

            this.lexer.pause_on_comments = false;

            var statements = NodeList_.init(this);

            if (this.import_listener) |listener| {
                var imports = std.ArrayListUnmanaged([]const u8){};
                while (this.lexer.token == .t_import or this.lexer.token == .t_export) {
                    const n = try this.parseStatement();
                    try statements.append(n);

                    if (n.kind == .export_declaration or n.kind == .import_declaration) {
                        const d = getPackedData(&n.toAstNode());
                        if (d.right != 0) {
                            const slice = getSlice(this.node_allocator.at(d.right), u8);
                            try imports.append(this.lexer.allocator, slice);
                        }
                    }
                }

                if (imports.items.len > 0 or directives.items.len > 0) {
                    defer imports.deinit(this.lexer.allocator);
                    try listener.call(directives.items, imports.items);
                }
            }

            while (this.lexer.token != .t_end_of_file) {
                if (comptime is_debug) {
                    const n = this.parseStatement() catch |err| {
                        this.printLocation();
                        return err;
                    };
                    try statements.append(n);
                } else {
                    const n = try this.parseStatement();
                    try statements.append(n);
                }
            }

            // var i: u32 = 0;
            // while (i < this.node_allocator.count) {
            //     std.debug.print("{any}\n",.{this.node_allocator.at(i).kind});
            //     i += 1;
            // }

            const root: AstNode_ = .{
                .kind = .source_file,
                .data = @ptrFromInt(statements.head),
            };

            const root_ref = try this.pushNode(root);

            return .{
                .root = root.toAstNode(),
                .root_ref = root_ref,
                .data = .{
                    .start = root_ref,
                    .source = this.lexer.source.contents,
                    .source_name = this.lexer.source.name,
                    .nodes = this.node_allocator,
                    .decorators = this.decorators,
                    .positions = this.positions,
                    .lines = if (comptime skip_trivia or @TypeOf(this.lexer.line_map) == void) null else this.lexer.line_map,
                    .triple_slash_directives = directives.items,
                },
            };
        }
    };
}

pub const Parser = Parser_(false);

pub const Factory = struct {
    nodes: *BumpAllocator(AstNode),

    pub inline fn cloneNode(this: *@This(), n: *const AstNode) !NodeRef {
        return this.nodes.push(n.*);
    }

    pub inline fn cloneNodeRef(this: *@This(), ref: NodeRef) !NodeRef {
        return this.cloneNode(this.nodes.at(ref));
    }

    pub fn createIdentifier(this: *@This(), text: []const u8) !NodeRef {
        const t = try getAllocator().dupe(u8, text);
        return this.createIdentifierAllocated(t);
    }

    pub fn createIdentifierAllocated(this: *@This(), text: []const u8) !NodeRef {
        return this.nodes.push(.{
            .kind = .identifier,
            .data = text.ptr,
            .len = @intCast(text.len),
        });
    }

    pub fn createStringLiteral(this: *@This(), text: []const u8) !NodeRef {
        const t = try getAllocator().dupe(u8, text);
        return this.createStringLiteralAllocated(t);
    }

    pub fn createStringLiteralAllocated(this: *@This(), text: []const u8) !NodeRef {
        return this.nodes.push(.{
            .kind = .string_literal,
            .data = text.ptr,
            .len = @intCast(text.len),
        });
    }

    pub fn createNumericLiteral(this: *@This(), val: anytype) !NodeRef {
        const d: u64 = switch (@TypeOf(val)) {
            f64 => @bitCast(val),
            i32, i64, u32, usize => @bitCast(@as(f64, @floatFromInt(val))),
            else => @compileError("Unhandled type"),
        };

        return this.nodes.push(.{
            .kind = .numeric_literal,
            .data = if (d == 0) null else @ptrFromInt(d),
        });
    }

    pub fn createTrue(this: *@This()) !NodeRef {
        return this.nodes.push(.{ .kind = .true_keyword });
    }

    pub fn createFalse(this: *@This()) !NodeRef {
        return this.nodes.push(.{ .kind = .false_keyword });
    }

    pub fn createNull(this: *@This()) !NodeRef {
        return this.nodes.push(.{ .kind = .null_keyword });
    }

    pub fn createUndefined(this: *@This()) !NodeRef {
        return this.nodes.push(.{ .kind = .undefined_keyword });
    }

    pub fn createPropertyAccessExpression(this: *@This(), subject: NodeRef, member: anytype) !NodeRef {
        const right = switch (@TypeOf(member)) {
            NodeRef => member,
            []const u8 => try this.createIdentifier(member),
            else => blk: {
                if (comptime isComptimeString(@TypeOf(member))) {
                    break :blk try this.createIdentifier(member);
                }
                @compileLog(@TypeOf(member));
                @compileError("Unhandled type");
            },
        };

        return this.nodes.push(.{
            .kind = .property_access_expression,
            .data = toBinaryDataPtrRefs(subject, right),
        });
    }

    pub fn createElementAccessExpression(this: *@This(), subject: NodeRef, arg: anytype) !NodeRef {
        const right = switch (@TypeOf(arg)) {
            NodeRef => arg,
            []const u8 => try this.createStringLiteral(arg),
            f64, usize, comptime_int => try this.createNumericLiteral(arg),
            else => blk: {
                if (comptime isComptimeString(@TypeOf(arg))) {
                    break :blk try this.createStringLiteral(arg);
                }
                @compileLog(@TypeOf(arg));
                @compileError("Unhandled type");
            },
        };

        return this.nodes.push(.{
            .kind = .element_access_expression,
            .data = toBinaryDataPtrRefs(subject, right),
        });
    }

    fn createList(this: *@This(), arg: []const NodeRef) !NodeRef {
        var list = NodeList.init(this.nodes);
        for (arg) |el| list.appendRef(el);

        return list.head;
    }

    fn isComptimeString(comptime T: type) bool {
        switch (@typeInfo(T)) {
            .Pointer => |p| {
                if (!p.is_const or p.size != .One) return false;

                switch (@typeInfo(p.child)) {
                    .Array => |s| {
                        if (s.child != u8) return false;

                        return true;
                    },
                    else => return false,
                }
            },
            else => return false,
        }
    }

    fn isAnonymousNodeList(comptime T: type) bool {
        switch (@typeInfo(T)) {
            .Pointer => |p| {
                if (!p.is_const or p.size != .One) return false;

                switch (@typeInfo(p.child)) {
                    .Struct => |s| {
                        if (!s.is_tuple) return false;

                        inline for (s.fields) |f| {
                            if (f.type != u32) return false;
                        }

                        return true;
                    },
                    else => return false,
                }
            },
            else => return false,
        }
    }

    fn maybeCreateList(this: *@This(), arg: anytype) !NodeRef {
        return switch (@TypeOf(arg)) {
            comptime_int, NodeRef => arg,
            []const NodeRef => this.createList(arg),
            else => {
                if (comptime isAnonymousNodeList(@TypeOf(arg))) {
                    return this.createList(arg);
                }
                
                @compileLog(@TypeOf(arg));
                @compileError("Unhandled type");
            },
        };
    }

    fn maybeCreateBlock(this: *@This(), arg: anytype) !NodeRef {
        return switch (@TypeOf(arg)) {
            comptime_int => arg, // TODO: assert 0?
            NodeRef => this.assertKind(arg, .block),
            []const NodeRef => this.createBlock(arg),
            else => {
                if (comptime isAnonymousNodeList(@TypeOf(arg))) {
                    return this.createBlock(arg);
                }
                
                @compileLog(@TypeOf(arg));
                @compileError("Unhandled type");
            },
        };
    }

    pub fn createCallExpression(this: *@This(), expression: NodeRef, args: anytype) !NodeRef {
        return this.nodes.push(.{
            .kind = .call_expression,
            .data = toBinaryDataPtrRefs(expression, try this.maybeCreateList(args)),
        });
    }

    pub fn createNewExpression(this: *@This(), expression: NodeRef, args: anytype) !NodeRef {
        return this.nodes.push(.{
            .kind = .new_expression,
            .data = toBinaryDataPtrRefs(expression, try this.maybeCreateList(args)),
        });
    }

    pub fn createBinaryExpression(this: *@This(), left: NodeRef, operator: SyntaxKind, right: NodeRef) !NodeRef {
        return this.nodes.push(.{
            .kind = .binary_expression,
            .data = toBinaryDataPtrRefs(left, right),
            .len = @intFromEnum(operator),
        });
    }

    pub fn createPrefixUnaryExpression(this: *@This(), operator: SyntaxKind, operand: NodeRef) !NodeRef {
        return this.nodes.push(.{
            .kind = .prefix_unary_expression,
            .data = toBinaryDataPtrRefs(@intFromEnum(operator), operand),
        });
    }

    pub fn createPostfixUnaryExpression(this: *@This(), operand: NodeRef, operator: SyntaxKind) !NodeRef {
        return this.nodes.push(.{
            .kind = .postfix_unary_expression,
            .data = toBinaryDataPtrRefs(operand, @intFromEnum(operator)),
        });
    }

    pub fn createConditionalExpression(this: *@This(), condition: NodeRef, when_true: NodeRef, when_false: NodeRef) !NodeRef {
        return this.nodes.push(.{
            .kind = .conditional_expression,
            .data = toBinaryDataPtrRefs(condition, when_true),
            .len = when_false
        });
    }

    pub fn createParenthesizedExpression(this: *@This(), expression: NodeRef) !NodeRef {
        return this.nodes.push(.{
            .kind = .parenthesized_expression,
            .data = @ptrFromInt(expression),
        });
    }

    pub fn createAwaitExpression(this: *@This(), expression: NodeRef) !NodeRef {
        return this.nodes.push(.{
            .kind = .await_expression,
            .data = @ptrFromInt(expression),
        });
    }

    pub fn createTypeofExpression(this: *@This(), expression: NodeRef) !NodeRef {
        return this.nodes.push(.{
            .kind = .typeof_expression,
            .data = @ptrFromInt(expression),
        });
    }

    pub fn createSpreadElement(this: *@This(), expression: NodeRef) !NodeRef {
        return this.nodes.push(.{
            .kind = .spread_element,
            .data = @ptrFromInt(expression),
        });
    }

    pub fn createArrayLiteralExpression(this: *@This(), elements: NodeRef) !NodeRef {
        return this.nodes.push(.{
            .kind = .array_literal_expression,
            .data = @ptrFromInt(elements),
        });
    }

    pub fn createObjectLiteralExpression(this: *@This(), properties: NodeRef) !NodeRef {
        return this.nodes.push(.{
            .kind = .object_literal_expression,
            .data = @ptrFromInt(properties),
        });
    }

    pub fn createArrowFunction(this: *@This(), params: NodeRef, body: NodeRef, flags: u22) !NodeRef {
        return this.nodes.push(.{
            .kind = .arrow_function,
            .data = toBinaryDataPtrRefs(params, body),
            .flags = flags,
        });
    }

    pub fn createAsExpression(this: *@This(), expression: NodeRef, type_node: NodeRef) !NodeRef {
        return this.nodes.push(.{
            .kind = .as_expression,
            .data = toBinaryDataPtrRefs(expression, type_node),
        });
    }

    pub fn createBlock(this: *@This(), statements: anytype) !NodeRef {
        return this.nodes.push(.{
            .kind = .block,
            .data = @ptrFromInt(try this.maybeCreateList(statements)),
        });
    }

    pub fn createExpressionStatement(this: *@This(), expression: NodeRef) !NodeRef {
        return this.nodes.push(.{
            .kind = .expression_statement,
            .data = @ptrFromInt(expression),
        });
    }

    pub fn createReturnStatement(this: *@This(), expression: NodeRef) !NodeRef {
        return this.nodes.push(.{
            .kind = .return_statement,
            .data = @ptrFromInt(expression),
        });
    }

    pub fn createThrowStatement(this: *@This(), expression: NodeRef) !NodeRef {
        return this.nodes.push(.{
            .kind = .throw_statement,
            .data = @ptrFromInt(expression),
        });
    }

    pub fn createIfStatement(this: *@This(), condition: NodeRef, then_stmt: NodeRef, else_stmt: NodeRef) !NodeRef {
        return this.nodes.push(.{
            .kind = .if_statement,
            .data = toBinaryDataPtrRefs(condition, then_stmt),
            .len = else_stmt,
        });
    }

    pub fn createVariableStatement(this: *@This(), declarations: NodeRef, flags: u22) !NodeRef {
        return this.nodes.push(.{
            .kind = .variable_statement,
            .data = @ptrFromInt(declarations),
            .flags = flags,
        });
    }

    pub fn createVariableDeclaration(this: *@This(), name: NodeRef, type_node: NodeRef, initializer: NodeRef) !NodeRef {
        return this.nodes.push(.{
            .kind = .variable_declaration,
            .data = toBinaryDataPtrRefs(name, initializer),
            .len = type_node,
        });
    }

    pub fn createVariableDeclarationSimple(this: *@This(), name: NodeRef, initializer: NodeRef) !NodeRef {
        return this.createVariableDeclaration(name, 0, initializer);
    }

    pub fn createCatchClause(this: *@This(), binding: anytype, statements: anytype) !NodeRef {
        const left = switch (@TypeOf(binding)) {
            NodeRef => this.assertKind(binding, .variable_declaration),
            else => @compileError("Unhandled type"),
        };

        const right = try this.maybeCreateBlock(statements);

        return this.nodes.push(.{
            .kind = .catch_clause,
            .data = toBinaryDataPtrRefs(left, right),
        });
    }

    pub fn createTryStatement(this: *@This(), statements: anytype, catch_clause: NodeRef, finally_block: anytype) !NodeRef {
        return this.nodes.push(.{
            .kind = .try_statement,
            .data = toBinaryDataPtrRefs(
                try this.maybeCreateBlock(statements),
                catch_clause
            ),
            .len = try this.maybeCreateBlock(finally_block),
        });
    }

    pub fn createParameter(this: *@This(), name: NodeRef, initializer: NodeRef) !NodeRef {
        return this.nodes.push(.{
            .kind = .parameter,
            .data = toBinaryDataPtrRefs(name, initializer),
            // .len = type_node,
        });
    }

    pub fn createFunctionDeclaration(this: *@This(), name: NodeRef, params: anytype, body: anytype) !NodeRef {
        return this.nodes.push(.{
            .kind = .function_declaration,
            .data = toBinaryDataPtrRefs(name, try this.maybeCreateList(params)),
            .len = try this.maybeCreateBlock(body),
        });
    }

    pub fn createPropertyAssignment(this: *@This(), name: NodeRef, initializer: NodeRef) !NodeRef {
        return this.nodes.push(.{
            .kind = .property_assignment,
            .data = toBinaryDataPtrRefs(name, initializer),
        });
    }

    pub fn createShorthandPropertyAssignment(this: *@This(), name: NodeRef) !NodeRef {
        return this.nodes.push(.{
            .kind = .shorthand_property_assignment,
            .data = @ptrFromInt(name),
        });
    }

    pub fn createTypeReference(this: *@This(), name: NodeRef, type_args: NodeRef) !NodeRef {
        return this.nodes.push(.{
            .kind = .type_reference,
            .data = toBinaryDataPtrRefs(name, type_args),
        });
    }

    pub fn createArrayType(this: *@This(), element_type: NodeRef) !NodeRef {
        return this.nodes.push(.{
            .kind = .array_type,
            .data = @ptrFromInt(element_type),
        });
    }

    pub fn createKeywordType(this: *@This(), keyword: SyntaxKind) !NodeRef {
        return this.nodes.push(.{ .kind = keyword });
    }

    pub fn createBreakStatement(this: *@This()) !NodeRef {
        return this.nodes.push(.{ .kind = .break_statement });
    }

    pub fn createContinueStatement(this: *@This()) !NodeRef {
        return this.nodes.push(.{ .kind = .continue_statement });
    }

    pub fn createCaseClause(this: *@This(), expression: NodeRef, statements: NodeRef) !NodeRef {
        return this.nodes.push(.{
            .kind = .case_clause,
            .data = @ptrFromInt(expression),
            .len = statements,
        });
    }

    pub fn createDefaultClause(this: *@This(), statements: NodeRef) !NodeRef {
        return this.nodes.push(.{
            .kind = .default_clause,
            .len = statements,
        });
    }

    pub fn createSwitchStatement(this: *@This(), expression: NodeRef, clauses: NodeRef) !NodeRef {
        return this.nodes.push(.{
            .kind = .switch_statement,
            .data = toBinaryDataPtrRefs(expression, clauses),
        });
    }

    inline fn assertKind(this: *@This(), ref: NodeRef, kind: SyntaxKind) NodeRef {
        std.debug.assert(this.nodes.at(ref).kind == kind);
        return ref;
    }
};

const HelperTags = enum {
    known_symbol,
    using,
    call_dispose,
    @"defer",
};

inline fn getHelper(tag: HelperTags) []const u8 {
    return switch (tag) {
        .known_symbol => __knownSymbol,
        .using => __using,
        .call_dispose => __callDispose,
        .@"defer" => __defer,
    };
}

// These implementations come from `esbuild`
const __knownSymbol =
    \\function __knownSymbol(name, symbol) {
    \\  return (symbol = Symbol[name]) ? symbol : Symbol.for("Symbol." + name);
    \\}
;

const __using =
    \\function __using(stack, value, async) {
    \\  if (value != null) {
    \\    if (typeof value !== "object" && typeof value !== "function")
    \\      throw TypeError("Object expected");
    \\    var dispose;
    \\    if (async)
    \\      dispose = value[__knownSymbol("asyncDispose")];
    \\    if (dispose === void 0)
    \\      dispose = value[__knownSymbol("dispose")];
    \\    if (typeof dispose !== "function")
    \\      throw TypeError("Object not disposable");
    \\    stack.push([async, dispose, value]);
    \\  } else if (async) {
    \\    stack.push([async]);
    \\  }
    \\  return value;
    \\}
;

const __callDispose =
    \\function __callDispose(stack, error, hasError) {
    \\  var E = typeof SuppressedError === "function" ? SuppressedError : function(e, s, m, _2) {
    \\    return _2 = Error(m), _2.name = "SuppressedError", _2.error = e, _2.suppressed = s, _2;
    \\  };
    \\  var fail = (e) => error = hasError ? new E(e, error, "An error was suppressed during disposal") : (hasError = true, e);
    \\  var next = (it) => {
    \\    while (it = stack.pop()) {
    \\      try {
    \\        var result = it[1] && it[1].call(it[2]);
    \\        if (it[0])
    \\          return Promise.resolve(result).then(next, (e) => (fail(e), next()));
    \\      } catch (e) {
    \\        fail(e);
    \\      }
    \\    }
    \\    if (hasError)
    \\      throw error;
    \\  };
    \\  return next();
    \\}
;

const __defer =
    \\function __defer(stack, value, async) {
    \\  if (typeof value !== "function")
    \\      throw TypeError("Function expected");
    \\  stack.push([async, value]);
    \\}
;

const __esm_exports_helper = "Object.defineProperty(exports, \"__esModule\", { value: true });";

pub fn isParameterDecl(p: *const AstNode) bool {
    return (hasFlag(p, .private) or hasFlag(p, .public) or hasFlag(p, .protected) or hasFlag(p, .override) or hasFlag(p, .readonly));
}

fn isBoundControlFlow(nodes: *const BumpAllocator(AstNode), n: *const AstNode) bool {
    if (n.kind != .if_statement and n.kind != .while_statement) return false;

    return nodes.at(getPackedData(n).left).kind == .variable_statement; 
}

pub const Transformer = struct {
    allocator: *BumpAllocator(AstNode),
    tmp_counter: u32 = 0,

    fn parameters(this: *@This(), names: [][]const u8) !NodeRef {
        if (names.len == 0) {
            return 0;
        }

        var list = NodeList.init(this.allocator);
        for (names) |n| {
            const ident = try this.createIdent(n);
            try list.append(.{ .kind = .parameter, .data = toBinaryDataPtrRefs(ident, 0) });
        }

        return list.head;
    }

    fn createBool(val: bool) AstNode {
        if (val) {
            return .{ .kind = .true_keyword };
        } else {
            return .{ .kind = .false_keyword };
        }
    }

    fn createIdent(this: *@This(), text: []const u8) !NodeRef {
        return this.allocator.push(toIdentNode(text));
    }

    fn createString(this: *@This(), text: []const u8) !NodeRef {
        return this.allocator.push(.{
            .kind = .string_literal,
            .data = text.ptr,
            .len = @intCast(text.len),
        });
    }

    fn elementAccess(this: *@This(), left: NodeRef, right: NodeRef) !NodeRef {
        return this.allocator.push(.{
            .kind = .element_access_expression,
            .data = toBinaryDataPtrRefs(left, right),
        });
    }

    fn block(this: *@This(), statements: []const AstNode) !AstNode {
        var list = NodeList.init(this.allocator);
        for (statements) |n| {
            try list.append(n);
        }

        return .{
            .kind = .block,
            .data = @ptrFromInt(list.head),
        };
    }

    fn callExpression(this: *@This(), expression: AstNode, arguments: []const AstNode) !AstNode {
        const exp = try this.allocator.push(expression);
        var list = NodeList.init(this.allocator);
        for (arguments) |n| {
            try list.append(n);
        }

        return .{
            .kind = .call_expression,
            .data = toBinaryDataPtrRefs(exp, list.head),
        };
    }

    fn catchClause(this: *@This(), binding: []const u8, statements: []const AstNode) !AstNode {
        const left = try this.allocator.push(try this.variableDecl(binding, null));
        const right = try this.allocator.push(try this.block(statements));

        return .{
            .kind = .catch_clause,
            .data = toBinaryDataPtrRefs(left, right),
        };
    }

    fn variableDecl(this: *@This(), name: []const u8, initializer: ?AstNode) !AstNode {
        const left = try this.createIdent(name);
        const right = if (initializer) |n| try this.allocator.push(n) else 0;

        return .{
            .kind = .variable_declaration,
            .data = toBinaryDataPtrRefs(left, right),
        };
    }

    fn variableDeclFromNode(this: *@This(), binding: NodeRef, initializer: ?AstNode) !AstNode {
        const right = if (initializer) |n| try this.allocator.push(n) else 0;

        return .{
            .kind = .variable_declaration,
            .data = toBinaryDataPtrRefs(binding, right),
        };
    }

    fn variableStatement(this: *@This(), decls: []const AstNode) !AstNode {
        var list = NodeList.init(this.allocator);
        for (decls) |n| {
            try list.append(n);
        }

        return .{
            .kind = .variable_statement,
            .data = @ptrFromInt(list.head),
        };
    }

    pub fn wrapUsing(this: *@This(), exp: NodeRef) !AstNode {
        return this.callExpression(
            toIdentNode("__using"),
            &.{ toIdentNode("_stack"), this.allocator.at(exp).* },
        );
    }

    fn awaitExpression(this: *@This(), exp: AstNode) !AstNode {
        return .{
            .kind = .await_expression,
            .data = @ptrFromInt(try this.allocator.push(exp)),
        };
    }

    fn binaryExpression(this: *@This(), left: AstNode, operator: SyntaxKind, right: AstNode) !AstNode {
        const l = try this.allocator.push(left);
        const r = try this.allocator.push(right);

        return .{
            .kind = .binary_expression,
            .data = toBinaryDataPtrRefs(l, r),
            .len = @intFromEnum(operator),
        };
    }

    pub fn usingScopeFinally(this: *@This(), is_async: bool) !AstNode {
        const call_dispose = try this.callExpression(toIdentNode("__callDispose"), &.{
            toIdentNode("_stack"),
            toIdentNode("_error"),
            toIdentNode("_hasError"),
        });

        if (!is_async) {
            return this.block(&.{call_dispose});
        }

        const promise = try this.variableStatement(&.{
            try this.variableDecl("_promise", call_dispose),
        });

        return this.block(&.{
            promise,
            try this.binaryExpression(
                toIdentNode("_promise"),
                .ampersand_ampersand_token,
                try this.awaitExpression(toIdentNode("_promise")),
            ),
        });
    }

    pub fn usingCatchClause(this: *@This()) !AstNode {
        return this.catchClause("_", &.{
            try this.variableStatement(&.{
                try this.variableDecl("_error", toIdentNode("_")),
                try this.variableDecl("_hasError", createBool(true)),
            }),
        });
    }

    pub fn transformUsingScope(this: *@This(), statement: NodeRef, is_async: bool) !AstNode {
        const catch_clause = try this.usingCatchClause();
        const finally_block = try this.usingScopeFinally(is_async);
        const try_statement = AstNode{
            .kind = .try_statement,
            .data = toBinaryDataPtrRefs(
                try this.allocator.push(.{
                    .kind = .block,
                    .data = @ptrFromInt(statement),
                }),
                try this.allocator.push(catch_clause),
            ),
            .len = try this.allocator.push(finally_block),
        };

        var stack = try this.stackStatement();
        stack.next = try this.allocator.push(try_statement);

        return stack;
    }

    pub fn stackStatement(this: *@This()) !AstNode {
        var s = try this.variableStatement(&.{
            try this.variableDecl("_stack", .{
                .kind = .array_literal_expression,
            }),
        });

        s.flags = @intFromEnum(NodeFlags.@"const");

        return s;
    }

    pub fn wrapDefer(this: *@This(), exp: NodeRef) !AstNode {
        const arrow_fn = AstNode{
            .kind = .arrow_function,
            .data = toBinaryDataPtrRefs(0, exp),
        };

        return this.callExpression(
            toIdentNode("__defer"),
            &.{ toIdentNode("_stack"), arrow_fn },
        );
    }

    // `statement` is the deferred statement
    pub fn maybeTransformAsyncDefer(this: *@This(), statement: NodeRef) !struct { bool, NodeRef } {
        var r = statement;
        var n = this.allocator.at(r);
        var is_statement = true;
        if (n.kind == .expression_statement) {
            is_statement = false;
            r = unwrapRef(n);
            n = this.allocator.at(r);
            if (n.kind == .await_expression) {
                return .{ true, unwrapRef(n) };
            }
        }

        const is_block = n.kind == .block;

        const Visitor = struct {
            awaited: u32 = 0,
            nodes: *const BumpAllocator(AstNode),

            pub fn visit(self: *@This(), node: *const AstNode, _: NodeRef) anyerror!void {
                if (node.kind == .await_expression) {
                    self.awaited += 1;
                    return;
                }

                // TODO: `for await`

                return forEachChild(self.nodes, node, self);
            }
        };

        // TODO: this needs to split out sync execution from async
        var visitor = Visitor{ .nodes = this.allocator };
        try forEachChild(this.allocator, n, &visitor);

        if (visitor.awaited == 0) {
            if (is_statement and !is_block) {
                return .{
                    false,
                    try this.allocator.push(.{
                        .kind = .block,
                        .data = @ptrFromInt(r),
                    }),
                };
            }

            return .{ false, r };
        }

        if (is_block) {
            return .{ true, r };
        }

        return .{
            true,
            try this.allocator.push(.{
                .kind = .block,
                .data = @ptrFromInt(r),
            }),
        };
    }

    fn prependStatement(this: *@This(), target_ref: NodeRef, statement: NodeRef) !NodeRef {
        const target = this.allocator.at(target_ref);
        if (target.kind == .block) {
            const first = maybeUnwrapRef(target) orelse 0;
            this.allocator.at(statement).next = first;
            var copy = target.*;
            copy.data = @ptrFromInt(statement);
            return this.allocator.push(copy);
        }

        this.allocator.at(statement).next = target_ref;

        return try this.allocator.push(.{
            .kind = .block,
            .data = @ptrFromInt(statement),
        });
    }

    pub fn transformIfOrWhile(this: *@This(), n: *const AstNode) !NodeRef {
        const d = getPackedData(n);
        const l = this.allocator.at(d.left);
        std.debug.assert(l.kind == .variable_statement);

        const tmp_name = try std.fmt.allocPrint(getAllocator(), "_tmp_{d}", .{this.tmp_counter});
        this.tmp_counter += 1;

        const decl = this.allocator.at(unwrapRef(l));
        std.debug.assert(decl.kind == .variable_declaration);

        const tmp_ident = try this.createIdent(tmp_name);
        const init = this.allocator.at(getPackedData(decl).right).*;
        const tmp_decl = try this.variableDeclFromNode(tmp_ident, if (n.kind == .if_statement) init else null);
        var tmp_stmt = try this.variableStatement(&.{tmp_decl});
        tmp_stmt.flags |= @intFromEnum(if (n.kind == .if_statement) NodeFlags.@"const" else NodeFlags.let);
        
        // adds `const { x } = _tmp_0` to the executing statement
        var inner_decl = decl.*;
        inner_decl.data = toBinaryDataPtrRefs(getPackedData(decl).left, tmp_ident);

        var inner_statement = l.*;
        inner_statement.data = @ptrFromInt(try this.allocator.push(inner_decl));
        const bound_statement = try this.prependStatement(d.right, try this.allocator.push(inner_statement));

        const nonNullishCheck = try this.binaryExpression(this.allocator.at(tmp_ident).*, .exclamation_equals_token, .{ .kind = .null_keyword });

        // TODO: destructuring should also check for property existence, optimized by type analysis

        // copy the origin if/while statement and replace the exp + target statement with the injected binding
        const subject = if (n.kind == .if_statement) 
           // tmp_ident 
            try this.allocator.push(nonNullishCheck)
        else 
            // TODO: do we ever need parens here on the init exp?
            // while (_tmp_0 = foo(), _tmp_0 != null)
            try this.allocator.push(
                try this.binaryExpression(
                try this.binaryExpression(this.allocator.at(tmp_ident).*, .equals_token, init),
                    .comma_token, 
                    nonNullishCheck
                )
            );

        var copy = n.*;
        copy.data = toBinaryDataPtrRefs(subject, bound_statement);

        // set the next statement of the tmp binding to the modified control flow
        tmp_stmt.next = try this.allocator.push(copy);

        return try this.allocator.push(tmp_stmt);
    }

    fn unwrapExpressionStatement(this: *const @This(), exp: NodeRef) !NodeRef {
        var n = exp;
        while (true) {
            const next = this.allocator.at(n);
            switch (next.kind) {
                .parenthesized_expression => n = unwrapRef(next),
                else => break,
            }
        }
        return n;
    }

    fn refToNumber(ref: NodeRef) AstNode {
        const v: f64 = @floatFromInt(ref);
        return .{
            .kind = .numeric_literal,
            .data = @ptrFromInt(@as(usize, @bitCast(v))),
        };
    }

    pub fn transformReifyExpression(this: *@This(), exp: NodeRef, type_params: NodeRef) !AstNode {
        if (type_params != 0) {
            return this.callExpression(
                toIdentNode("__reify"),
                &.{ toIdentNode("__filename"), refToNumber(exp), refToNumber(type_params) },
            );
        }

        return this.callExpression(
            toIdentNode("__reify"),
            &.{ toIdentNode("__filename"), refToNumber(exp) },
        );
    }

    // Any parameter with TS modifiers needs a `this` assignment
    pub fn transformConstructor(this: *@This(), d: BinaryExpData, has_super: bool) !NodeRef {
        var idents = std.ArrayList(NodeRef).init(getAllocator());
        defer idents.deinit();

        var param_iter = NodeIterator.init(this.allocator, d.left);
        while (param_iter.next()) |p| {
            if (isParameterDecl(p)) {
                try idents.append((getPackedData(p)).left);
            }
        }

        if (idents.items.len == 0) {
            return d.right;
        }

        var body = NodeList.init(this.allocator);

        std.debug.assert(d.right != 0);

        var rhs = maybeUnwrapRef(this.allocator.at(d.right)) orelse 0;
        if (has_super) {
            var body_iter = NodeIterator.init(this.allocator, rhs);
            while (body_iter.next()) |stmt| {
                try body.append(stmt.*);

                if (stmt.kind != .expression_statement) continue;

                // you can (unfortunately) hide the `super` call in intermediate expressions
                const inner = this.allocator.at(try this.unwrapExpressionStatement(unwrapRef(stmt)));

                if (inner.kind != .call_expression) continue;

                const d2 = getPackedData(inner);
                if (this.allocator.at(d2.left).kind == .super_keyword) {
                    rhs = stmt.next;
                    break;
                }
            }
        }

        for (idents.items) |ref| {
            const exp = try this.allocator.push(
                try this.binaryExpression(.{
                    .kind = .property_access_expression,
                    .data = toBinaryDataPtrRefs(
                        try this.allocator.push(.{ .kind = .this_keyword }),
                        try this.allocator.push(this.allocator.at(ref).*),
                    ),
                }, .equals_token, this.allocator.at(ref).*),
            );

            try body.append(.{
                .kind = .expression_statement,
                .data = @ptrFromInt(exp),
            });
        }

        this.allocator.at(body.prev).next = rhs;

        return this.allocator.push(.{
            .kind = .block,
            .data = @ptrFromInt(body.head),
        });
    }
};

pub inline fn unwrapRef(node: *const AstNode) NodeRef {
    return @intCast(@intFromPtr(node.data orelse {
        if (comptime is_debug) {
            std.debug.print("MISSING DATA {}\n", .{node.kind});
        }
        unreachable;
    }));
}

pub inline fn maybeUnwrapRef(node: *const AstNode) ?NodeRef {
    return @intCast(@intFromPtr(node.data orelse return null));
}

pub inline fn getPackedData(node: *const AstNode) BinaryExpData {
    if (node.data) |p| {
        const v = @as(u64, @intFromPtr(p));
        return .{
            .left = @truncate(v),
            .right = @intCast(v >> 32),
        };
    }
    return .{ .left = 0, .right = 0 };
}

pub inline fn hasFlag(node: *const AstNode, flag: NodeFlags) bool {
    return (node.flags & @intFromEnum(flag)) == @intFromEnum(flag);
}

pub inline fn hasTypeParamFlag(node: *const AstNode, flag: TypeParamFlags) bool {
    return (node.flags & @intFromEnum(flag)) == @intFromEnum(flag);
}

pub inline fn hasStringFlag(node: *const AstNode, flag: StringFlags) bool {
    return (node.flags & @intFromEnum(flag)) == @intFromEnum(flag);
}

pub inline fn getSlice(node: *const AstNode, comptime T: type) []const T {
    if (node.len == 0) {
        return &.{};
    }

    const ptr: [*]const T = @alignCast(@ptrCast(node.data orelse unreachable));

    return ptr[0..node.len];
}

pub inline fn getSourceSlice(source: []const u8, node: *const AstNode) []const u8 {
    const offset = @intFromPtr(node.data orelse unreachable);
    return source[offset..(offset + node.len)];
}

fn syntaxKindToString(kind: SyntaxKind) []const u8 {
    return switch (kind) {
        .for_keyword => "for",
        .object_keyword => "object",
        .default_keyword => "default",
        .abstract_keyword => "abstract",
        .accessor_keyword => "accessor",
        .string_keyword => "string",
        .number_keyword => "number",
        .boolean_keyword => "boolean",
        .true_keyword => "true",
        .false_keyword => "false",
        .symbol_keyword => "symbol",
        .intrinsic_keyword => "intrinsic",
        .unique_keyword => "unique",
        .null_keyword => "null",
        .void_keyword => "void",
        .this_keyword => "this",
        .super_keyword => "super",
        .any_keyword => "any",
        .unknown_keyword => "unknown",
        .undefined_keyword => "undefined",
        .const_keyword => "const",
        .never_keyword => "never",
        .delete_keyword => "delete",
        .yield_keyword => "yield",
        .private_keyword => "private",
        .protected_keyword => "protected",
        .public_keyword => "public",
        .readonly_keyword => "readonly",
        .key_of_keyword => "keyof",
        .typeof_keyword => "typeof",
        .instanceof_keyword => "instanceof",
        .new_keyword => "new",
        .import_keyword => "import",
        .in_keyword => "in",
        .infer_keyword => "infer",
        .debugger_keyword => "debugger",
        .reify_keyword => "reify",
        .comma_token => ",",
        .plus_token => "+",
        .equals_token => "=",
        .less_than_token => "<",
        .greater_than_token => ">",
        .minus_token => "-",
        .asterisk_token => "*",
        .plus_plus_token => "++",
        .minus_minus_token => "--",
        .asterisk_asterisk_token => "**",
        .asterisk_equals_token => "*=",
        .minus_equals_token => "-=",
        .asterisk_asterisk_equals_token => "**=",
        .exclamation_equals_token => "!=",
        .equals_equals_equals_token => "===",
        .exclamation_equals_equals_token => "!==",
        .question_question_equals_token => "??=",
        .question_question_token => "??",
        .bar_bar_token => "||",
        .bar_bar_equals_token => "||=",
        .ampersand_ampersand_token => "&&",
        .ampersand_ampersand_equals_token => "&&=",
        .equals_equals_token => "==",
        .plus_equals_token => "+=",
        .slash_token => "/",
        .greater_than_greater_than_token => ">>",
        .ampersand_token => "&",
        .bar_token => "|",
        .greater_than_equals_token => ">=",
        .less_than_less_than_token => "<<",
        .less_than_equals_token => "<=",
        .tilde_token => "~",
        .exclamation_token => "!",
        .percent_token => "%",
        // TODO: everything else
        else => {
            if (comptime @import("builtin").mode == .Debug) {
                std.debug.print("{any}", .{kind});
                std.debug.print("\n", .{});
                std.debug.print("\n", .{});
            }
            unreachable;
        },
    };
}

pub const NodeIterator = struct {
    ref: NodeRef,
    nodes: *const BumpAllocator(AstNode),

    pub fn init(nodes: *const BumpAllocator(AstNode), start: NodeRef) @This() {
        return .{
            .ref = start,
            .nodes = nodes,
        };
    }

    pub fn next(this: *@This()) ?*const AstNode {
        if (this.ref == 0) {
            return null;
        }

        const n = this.nodes.at(this.ref);
        this.ref = n.next;

        return n;
    }

    pub fn nextRef(this: *@This()) ?NodeRef {
        if (this.ref == 0) {
            return null;
        }

        const ref = this.ref;
        const n = this.nodes.at(ref);
        this.ref = n.next;

        return ref;
    }

    pub fn nextPair(this: *@This()) ?struct { *const AstNode, NodeRef } {
        if (this.ref == 0) {
            return null;
        }

        const ref = this.ref;
        const n = this.nodes.at(ref);
        this.ref = n.next;

        return .{ n, ref };
    }

    pub fn computeLength(this: *@This()) u32 {
        const start = this.ref;
        var c: u32 = 0;
        while (this.next()) |_| {
            c += 1;
        }
        this.ref = start;
        return c;
    }
};

inline fn visitList(nodes: *const BumpAllocator(AstNode), start: NodeRef, visitor: anytype) !void {
    return _visitList(nodes, start, visitor, false);
}

inline fn _visitList(nodes: *const BumpAllocator(AstNode), start: NodeRef, visitor: anytype, comptime prefetched: bool) !void {
    var ref = start;
    while (ref != 0) {
        const n = nodes.at(ref);
        const r = n.next;
        if (comptime prefetched) {
            nodes.prefetch(r);
        }
        try visitor.visit(n, ref);
        ref = r;
    }
}

pub fn forEachChild(
    nodes: *const BumpAllocator(AstNode),
    node: *const AstNode,
    visitor: anytype, // fn visit(anytype, *const AstNode, NodeRef) anyerror!void;
) !void {
    return switch (node.kind) {
        .source_file => {
            // source file benefits the most from pre-fetching
            try _visitList(nodes, maybeUnwrapRef(node) orelse 0, visitor, true);
        },
        .block, .variable_statement, .object_literal_expression, .array_literal_expression, .template_expression, .class_static_block_declaration => {
            try visitList(nodes, maybeUnwrapRef(node) orelse 0, visitor);
        },
        .expression_statement, .await_expression, .delete_expression, .parenthesized_expression, .typeof_expression, .spread_element => {
            const ref = unwrapRef(node);
            try visitor.visit(nodes.at(ref), ref);
        },
        .return_statement, .yield_expression => {
            if (maybeUnwrapRef(node)) |ref| {
                try visitor.visit(nodes.at(ref), ref);
            }
        },
        .function_declaration, .function_expression => {
            const d = getPackedData(node);
            // d.left - name
            // extra_data - type params
            // extra_data2 - return type
            try visitList(nodes, d.right, visitor);

            if (node.len != 0) {
                try visitor.visit(nodes.at(node.len), node.len);
            }
        },
        .call_expression, .new_expression => {
            const d = getPackedData(node);
            try visitor.visit(nodes.at(d.left), d.left);
            try visitList(nodes, d.right, visitor);
        },
        .variable_declaration => {
            const d = getPackedData(node);
            try visitor.visit(nodes.at(d.left), d.left);
            try visitor.visit(nodes.at(d.right), d.right);
        },
        .while_statement, .do_statement, .satisfies_expression, .as_expression, .binary_expression, .property_access_expression, .element_access_expression, .tagged_template_expression => {
            const d = getPackedData(node);
            try visitor.visit(nodes.at(d.left), d.left);
            try visitor.visit(nodes.at(d.right), d.right);
        },
        .prefix_unary_expression => {
            const d = getPackedData(node);
            try visitor.visit(nodes.at(d.right), d.right);
        },
        .postfix_unary_expression => {
            const d = getPackedData(node);
            try visitor.visit(nodes.at(d.left), d.left);
        },
        .if_statement => {
            const d = getPackedData(node);
            try visitor.visit(nodes.at(d.left), d.left);
            try visitor.visit(nodes.at(d.right), d.right);

            if (node.len != 0) {
                try visitor.visit(nodes.at(node.len), node.len);
            }
        },
        .template_span => {
            const d = getPackedData(node);
            try visitor.visit(nodes.at(d.left), d.left);
        },
        .conditional_expression => {
            const d = getPackedData(node);
            try visitor.visit(nodes.at(d.left), d.left);
            try visitor.visit(nodes.at(d.right), d.right);
            try visitor.visit(nodes.at(node.len), node.len);
        },
        .try_statement => {
            const d = getPackedData(node);
            try visitor.visit(nodes.at(d.left), d.left); // try block
            if (d.right != 0) try visitor.visit(nodes.at(d.right), d.right); // catch clause
            if (node.len != 0) try visitor.visit(nodes.at(node.len), node.len); // finally block
        },
        .catch_clause => {
            const d = getPackedData(node);
            if (d.left != 0) try visitor.visit(nodes.at(d.left), d.left);
            try visitor.visit(nodes.at(d.right), d.right);
        },
        .case_clause, .default_clause => {
            try visitList(nodes, node.len, visitor);
        },
        .switch_statement => {
            const d = getPackedData(node);
            try visitor.visit(nodes.at(d.left), d.left); // expression
            try visitList(nodes, d.right, visitor); // clauses
        },
        // constructor

        // Types
        .type_alias_declaration => {
            const d = getPackedData(node);
            try visitor.visit(nodes.at(d.left), d.left);
            try visitList(nodes, d.right, visitor);
            try visitor.visit(nodes.at(node.len), node.len);
        },
        .interface_declaration => {
            const d = getPackedData(node);
            try visitor.visit(nodes.at(d.left), d.left); // name
            try visitList(nodes, node.len, visitor); // type params
            try visitList(nodes, node.extra_data, visitor); // extends
            try visitList(nodes, d.right, visitor); // body
        },
        .property_signature => {
            const d = getPackedData(node);
            try visitor.visit(nodes.at(d.left), d.left);
            if (d.right != 0) {
                try visitor.visit(nodes.at(d.right), d.right);
            }
        },
        .method_declaration => {
            const d = getPackedData(node);
            try visitor.visit(nodes.at(d.left), d.left); // name
            try visitList(nodes, node.extra_data, visitor); // type params
            try visitList(nodes, d.right, visitor); // params
            if (node.extra_data2 != 0) {
                try visitor.visit(nodes.at(node.extra_data2), node.extra_data2); // return type
            }
            if (node.len != 0) {
                try visitor.visit(nodes.at(node.len), node.len); // body
            }
        },
        .method_signature => {
            const d = getPackedData(node);
            try visitor.visit(nodes.at(d.left), d.left); // name
            try visitList(nodes, node.extra_data, visitor); // type params
            try visitList(nodes, d.right, visitor); // params
            if (node.len != 0) {
                try visitor.visit(nodes.at(node.len), node.len); // return type
            }
        },
        .infer_type, .parenthesized_type, .rest_type, .computed_property_name => {
            const r = unwrapRef(node);
            try visitor.visit(nodes.at(r), r);
        },
        .template_literal_type_span => {
            const d = getPackedData(node);
            try visitor.visit(nodes.at(d.left), d.left);
        },
        .type_query => {
            const r = unwrapRef(node);
            try visitor.visit(nodes.at(r), r);
            try visitList(nodes, node.len, visitor);
        },
        .function_type, .constructor_type => {
            const d = getPackedData(node);
            try visitList(nodes, node.len, visitor);
            try visitList(nodes, d.left, visitor);
            if (d.right != 0) {
                try visitor.visit(nodes.at(d.right), d.right);
            }
        },
        .union_type, .intersection_type => {
            const d = getPackedData(node);
            if (d.left != 0) {
                try visitor.visit(nodes.at(d.left), d.left);
            }
            try visitor.visit(nodes.at(d.right), d.right);
        },
        .qualified_name, .indexed_access_type, .type_predicate => {
            const d = getPackedData(node);
            try visitor.visit(nodes.at(d.left), d.left);
            try visitor.visit(nodes.at(d.right), d.right);
        },
        .named_tuple_member => {
            const d = getPackedData(node);
            try visitor.visit(nodes.at(d.right), d.right);
        },
        .type_operator => {
            const d = getPackedData(node);
            try visitor.visit(nodes.at(d.right), d.right);
        },
        .tuple_type, .template_literal_type, .type_literal => {
            try visitList(nodes, maybeUnwrapRef(node) orelse 0, visitor);
        },
        .type_reference, .expression_with_type_arguments => {
            const d = getPackedData(node);
            try visitor.visit(nodes.at(d.left), d.left);
            try visitList(nodes, d.right, visitor);
        },
        .type_parameter => {
            const d = getPackedData(node);
            try visitor.visit(nodes.at(d.left), d.left);

            if (d.right != 0) {
                try visitor.visit(nodes.at(d.right), d.right);
            }

            if (node.len != 0) {
                try visitor.visit(nodes.at(node.len), node.len);
            }
        },
        .conditional_type => {
            const d = getPackedData(node);
            try visitor.visit(nodes.at(d.left), d.left);
            try visitor.visit(nodes.at(d.right), d.right);
            try visitor.visit(nodes.at(node.len), node.len);
            try visitor.visit(nodes.at(node.extra_data), node.extra_data);
        },
        .mapped_type => {
            const d = getPackedData(node);
            try visitor.visit(nodes.at(d.left), d.left);
            try visitor.visit(nodes.at(d.right), d.right);
            if (node.len != 0) {
                try visitor.visit(nodes.at(node.len), node.len);
            }
        },
        else => {},
    };
}

pub const SymbolRef = u32;
pub const Symbol = struct {
    // The `declaration` slot is treated as a generic place to describe where the symbol comes from
    //
    // Local symbols point to declarations within a file, otherwise the program linker uses this for a few cases:
    //    1. marks `ordinal` as "late bound" and sets `declaration` to the global ref
    //    2. sets `ordinal` to point to the relevant file_id and:
    //      2a. refs an external symbol (named imports)
    //      2b. refs the default export node (default import)
    //      3c. leaves `declaration` unchanged (star imports)
    declaration: NodeRef = 0,

    // Relevant for destructuring and import/export specifiers
    // For namespaces, this is an index into the `namespaces` array
    // **Type params use this to cache their own type**
    binding: u32 = 0,
    next: u32 = 0, // Only used for locally merged declarations

    // This is used for flags for non-params
    ordinal: u32 = 0, // Used for type parameters, this is similar to register allocation

    pub inline fn hasFlag(this: *const Symbol, flag: SymbolFlags) bool {
        return hasSymbolFlag(this, flag);
    }

    pub inline fn getOrdinal(this: *const Symbol) u16 {
        return @as(u16, @truncate(this.ordinal)) & ~@as(u16, 0);
    }

    pub inline fn addFlag(this: *Symbol, flag: SymbolFlags) void {
        this.ordinal |= @as(u32, @intFromEnum(flag)) << 16;
    }

    pub inline fn removeFlag(this: *Symbol, flag: SymbolFlags) void {
        this.ordinal &= ~(@as(u32, @intFromEnum(flag)) << 16);
    }
};

pub const SymbolFlags = enum(u16) {
    local = 1 << 1,
    global = 1 << 2, // only valid for `lib` files

    not_type = 1 << 3, // Only relevant for imports

    top_level = 1 << 4,
    skip_narrowing = 1 << 5, // added during type analysis

    parameter = 1 << 8,
    late_bound = 1 << 10,
    imported = 1 << 11,
    exported = 1 << 12,
    @"const" = 1 << 13,
    namespace = 1 << 14,
    type = 1 << 15,

    @"enum" = (1 << 8) | (1 << 14), // parameter | namespace
    type_parameter = (1 << 8) | (1 << 15), // parameter | type
    aliased_module = (1 << 10) | (1 << 12) | (1 << 14), // late_bound | exported | namespace 
};

pub inline fn hasSymbolFlag(sym: *const Symbol, flag: SymbolFlags) bool {
    return ((sym.ordinal >> 16) & @intFromEnum(flag)) == @intFromEnum(flag);
}

pub inline fn getOrdinal(sym: *const Symbol) u16 {
    return @as(u16, @truncate(sym.ordinal)) & ~@as(u16, 0);
}

const bind_types = true;

pub const Exports = struct {
    const default_hash: u32 = @truncate(std.hash.Wyhash.hash(0, "default"));

    // namespace re-exports don't include the default export
    const AliasedExport = struct {
        is_type: bool = false,
        spec: []const u8,
        sym: SymbolRef,
        binding: u32 = 0, // what we export as
        target: u32 = 0, // what we are exporting, 0 is "*" export
    };

    // these are processed during program construction and mutate the symbols
    aliased_exports: std.ArrayListUnmanaged(AliasedExport) = .{},

    symbols: Binder.SymbolTable = .{},
    type_symbols: Binder.SymbolTable = .{},

    // the default export can be a synthetic symbol!
    //      `export default { a: 1 }`
    // or
    //      `export default function() {}
};

// Constructs two separate symbol tables for a given parse tree
// The second table is for types, the first is everything else
// Two tables are needed to make things like this work:
//
// type x = string
// type f = (x: x | number) => typeof x
// type z = ReturnType<f> // string | number
//
pub const Binder = struct {
    pub const SymbolTable = std.AutoArrayHashMapUnmanaged(NodeSymbolHash, SymbolRef); // @import("./program.zig").Analyzer.FlowTyper.SimpleHashMap; 
    const Scopes = std.ArrayListUnmanaged(SymbolTable);

    const warm_stack_depth = 16;

    pub const Namespace = struct {
        const global_hash: u32 = @truncate(std.hash.Wyhash.hash(0, "global"));

        symbols: SymbolTable = undefined,
        type_symbols: SymbolTable = undefined,
        module_specifier: ?[]const u8 = null,
        is_global: bool = false,

        exports: ?*Exports = null, // only present if `module_specifier` is non-null
    };

    const ImportedModule = struct {
        spec: []const u8,
        bindings: SymbolTable = .{},

        // These are linked-lists for multiple bindings
        default_binding: ?SymbolRef = null,
        namespace_binding: ?SymbolRef = null,
    };

    allocator: std.mem.Allocator,

    nodes: *const BumpAllocator(AstNode),

    symbols: BumpAllocator(Symbol),

    scopes: Scopes,
    type_scopes: Scopes,

    this_scope: std.ArrayListUnmanaged(SymbolRef) = .{},

    unbound_symbols: SymbolTable = .{},
    unbound_type_symbols: SymbolTable = .{},

    namespaces: std.ArrayListUnmanaged(Namespace) = .{},
    ambient_modules: std.AutoArrayHashMapUnmanaged(u64, SymbolRef) = .{},

    default_export: ?NodeRef = null,

    exports: Exports = Exports{},

    imports: std.AutoArrayHashMapUnmanaged(u64, ImportedModule) = .{},

    type_ordinals: u32 = 0,
    param_ordinals: u32 = 0,

    is_lib: bool = false,

    should_hoist: bool = false,
    should_export: bool = false,
    should_mark_const: bool = false,

    infer_scope: ?u32 = null,

    ns: u32 = 0, // this is always 1-indexed relative to `namespaces`

    // ONLY USED FOR DEBUGGING/LOGGING
    source_name: ?[]const u8 = null,

    pub fn init(nodes: *const BumpAllocator(AstNode), allocator: std.mem.Allocator) @This() {
        var symbol_allocator = BumpAllocator(Symbol).init(allocator, std.heap.page_allocator);
        symbol_allocator.preAlloc() catch unreachable;
        _ = symbol_allocator.push(.{}) catch unreachable;

        var this_scope = std.ArrayListUnmanaged(SymbolRef){};
        this_scope.ensureTotalCapacity(allocator, warm_stack_depth) catch unreachable;

        return .{
            .allocator = allocator,
            .nodes = nodes,
            .symbols = symbol_allocator,
            .scopes = initScopes(allocator),
            .type_scopes = initScopes(allocator),
            .this_scope = this_scope,
        };
    }

    pub fn deinit(this: @This()) void {
        _ = this; // TODO
    }

    inline fn initScopes(allocator: std.mem.Allocator) Scopes {
        var scopes: Scopes = .{};
        scopes.ensureTotalCapacity(allocator, warm_stack_depth) catch unreachable;
        for (0..warm_stack_depth) |i| {
            scopes.allocatedSlice()[i] = .{};
        }
        return scopes;
    }

    inline fn bindDecl(this: *@This(), ident: *AstNode, decl: NodeRef) !void {
        return this.bindDeclWithBinding(ident, decl, 0);
    }

    inline fn bindDeclWithBinding(this: *@This(), ident: *AstNode, decl: NodeRef, binding: NodeRef) !void {
        const flags_or_ordinal = if (this.should_mark_const)
            @as(u32, @intFromEnum(SymbolFlags.@"const")) << 16
        else if (comptime bind_types) this.param_ordinals else 0;

        return this.bindDeclWithFlagsOrOrdinal(ident, decl, binding, flags_or_ordinal, false);
    }

    fn bindDeclWithFlagsOrOrdinal(this: *@This(), ident: *AstNode, decl: NodeRef, binding: NodeRef, flags_or_ordinal: u32, comptime is_type_like: bool) !void {
        var extra_flags: u32 = 0;
        if (this.scopes.items.len == 1) {
            extra_flags |= @as(u32, @intFromEnum(SymbolFlags.top_level)) << 16;
        }

        var current_scope = &this.scopes.items[this.scopes.items.len - 1];
        const hash = getHashFromNode(ident);

        // We might return early from here
        if (comptime bind_types) {
            // Potentially merge symbols, constructing a linked list in reverse order
            // FIXME: we don't need to check this for some declarations
            if (current_scope.get(hash)) |sym_ref| {
                var from = this.symbols.at(sym_ref);
                const s = try this.symbols.push(from.*);

                from.declaration = decl;
                from.binding = binding;
                from.ordinal = flags_or_ordinal | extra_flags;
                from.next = s;

                ident.extra_data = sym_ref;

                return;
            }
        }

        const s = try this.symbols.push(.{
            .declaration = decl,
            .binding = binding,
            .ordinal = flags_or_ordinal | extra_flags,
        });

        // if (getLoc(this.nodes, ident)) |loc| {
        //     std.debug.print("[{s}:{}:{}]: symbol decl {s} -> {}\n", .{
        //         this.source_name orelse "<unknown>",
        //         loc.line + 1,
        //         loc.col + 1,
        //         getSlice(ident, u8),
        //         s,
        //     });
        // }

        if (this.should_export) {
            try this.pushExport(hash, s);
        }

        try current_scope.put(this.allocator, hash, s);
        ident.extra_data = s;

        if (comptime is_type_like) {
            try this.type_scopes.items[this.type_scopes.items.len - 1].put(this.allocator, hash, s);
            if (this.should_export) {
                try this.pushTypeExport(hash, s);
                // try this.type_exports.put(hash, s);
            }
        }
    }

    fn bindTypeSymbolWithFlagsAndOrdinal(this: *@This(), ident_ref: NodeRef, decl: NodeRef, flags_and_ordinal: u32) !void {
        var extra_flags: u32 = 0;

        const depth = this.type_scopes.items.len;
        if (depth == 1) {
            extra_flags |= @as(u32, @intFromEnum(SymbolFlags.top_level)) << 16;
        }

        var current_scope = &this.type_scopes.items[depth - 1];

        var ident = this.nodes.at(ident_ref);
        const hash = getHashFromNode(ident);

        if (current_scope.get(hash)) |prev| {
            var sym = this.symbols.at(prev);
            const next = sym.next;

            sym.next = try this.symbols.push(.{
                .declaration = decl,
                .next = next,
                .ordinal = flags_and_ordinal | extra_flags,
            });

            ident.extra_data = prev;
        } else {
            if (this.is_lib and depth == 1) {
                const s = try this.symbols.push(.{
                    .declaration = decl,
                    .ordinal = flags_and_ordinal | extra_flags | (@as(u32, @intFromEnum(SymbolFlags.global)) << 16),
                });
                ident.extra_data = s;
                try current_scope.put(this.allocator, hash, s);
                return;
            }

            const s = try this.symbols.push(.{
                .declaration = decl,
                .ordinal = flags_and_ordinal | extra_flags,
            });


            // if (getLoc(this.nodes, ident)) |loc| {
            //     std.debug.print("BOUND [{s}:{}:{}]: symbol decl {s} -> {} [hash: {}] [depth: {}] [type]\n", .{
            //         this.source_name orelse "<unknown>",
            //         loc.line + 1,
            //         loc.col + 1,
            //         getSlice(ident, u8),
            //         s,
            //         hash,
            //         this.type_scopes.items.len,
            //     });
            // }

            try current_scope.put(this.allocator, hash, s);

            ident.extra_data = s;

            if (this.should_export) {
                try this.pushTypeExport(hash, s);
                //try this.type_exports.put(hash, s);
            }
        }
    }

    inline fn bindTypeSymbolWithOrdinal(this: *@This(), ident_ref: NodeRef, decl: NodeRef, ordinal: u32) !void {
        std.debug.assert(ordinal < 65536);

        const flags: u16 = @intFromEnum(SymbolFlags.type_parameter);
        const flags_and_ordinal: u32 = (@as(u32, flags) << 16) | ordinal;

        return this.bindTypeSymbolWithFlagsAndOrdinal(ident_ref, decl, flags_and_ordinal);
    }

    inline fn bindTypeSymbol(this: *@This(), ident_ref: NodeRef, decl: NodeRef) !void {
        return this.bindTypeSymbolWithFlagsAndOrdinal(ident_ref, decl, @as(u32, @intFromEnum(SymbolFlags.type)) << 16);
    }

    fn getOrAddImportedModule(this: *@This(), spec: []const u8) !*ImportedModule {
        const key = std.hash.Wyhash.hash(0, spec);
        const module_entry = try this.imports.getOrPut(this.allocator, key);
        if (!module_entry.found_existing) {
            module_entry.value_ptr.* = ImportedModule{
                .spec = spec,
            };
        }

        return module_entry.value_ptr;
    }

    fn bindImport(this: *@This(), module: *ImportedModule, binding: NodeRef, property_name: ?[]const u8, is_type_only: bool, is_namespace: bool) !void {
        var flags = @as(u32, @intFromEnum(SymbolFlags.imported)) << 16;
        if (is_namespace) flags |= @as(u32, @intFromEnum(SymbolFlags.namespace)) << 16;

        var ident = this.nodes.at(binding);

        const s = try this.symbols.push(.{
            .declaration = 0,
            .binding = binding,
            .ordinal = flags,
        });

        ident.extra_data = s;

        var current_type_scope = &this.type_scopes.items[this.type_scopes.items.len - 1];
        try current_type_scope.put(this.allocator, getHashFromNode(ident), s);

        if (property_name) |name| {
            const hash: u32 = @truncate(std.hash.Wyhash.hash(0, name));
            try module.bindings.put(this.allocator, hash, s);
        } else if (is_namespace) {
            if (module.namespace_binding) |b| {
                var prev = this.symbols.at(b);
                this.symbols.at(s).next = prev.next;
                prev.next = s;
            } else {
                module.namespace_binding = s;
            }
        } else {
            // const hash: u32 = comptime @truncate(std.hash.Wyhash.hash(0, "default"));
            // try module.bindings.put(this.allocator, hash, s);
            if (module.default_binding) |b| {
                var prev = this.symbols.at(b);
                this.symbols.at(s).next = prev.next;
                prev.next = s;
            } else {
                module.default_binding = s;
            }
        }

        if (is_type_only) return; // TODO: need flag? we can't differentiate when linking

        var current_scope = &this.scopes.items[this.scopes.items.len - 1];
        try current_scope.put(this.allocator, getHashFromNode(ident), s);
    }

    inline fn pushExport(this: *@This(), hash: u32, s: SymbolRef) !void {
        if (this.ns != 0) {
            if (this.namespaces.items[this.ns - 1].exports == null) return;
            try this.namespaces.items[this.ns - 1].exports.?.symbols.put(this.allocator, hash, s);
        } else {
            try this.exports.symbols.put(this.allocator, hash, s);
        }
    }

    inline fn pushTypeExport(this: *@This(), hash: u32, s: SymbolRef) !void {
        if (this.ns != 0) {
            if (this.namespaces.items[this.ns - 1].exports == null) return;
            try this.namespaces.items[this.ns - 1].exports.?.type_symbols.put(this.allocator, hash, s);
        } else {
            try this.exports.type_symbols.put(this.allocator, hash, s);
        }
    }

    fn lazyBindThis(this: *@This(), this_keyword: *AstNode) !void {
        const depth = this.this_scope.items.len;
        if (depth == 0) return;

        this_keyword.extra_data = blk: {
            if ((this.this_scope.items[depth - 1] >> 31) != 1) break :blk this.this_scope.items[depth - 1];

            const s = try this.symbols.push(.{
                .declaration = this.this_scope.items[depth - 1] & ~@as(u31, 0),
            });

            this.this_scope.items[depth - 1] = s;
            break :blk s;
        };
    }

    fn _addUnbound(this: *@This(), ident_ref: NodeRef, comptime is_type: bool) !void {
        var ident = this.nodes.at(ident_ref);
        var m = if (comptime is_type) &this.unbound_type_symbols else &this.unbound_symbols;
        const hash = getHashFromNode(ident);

        if (m.get(hash)) |s| {
            ident.extra_data = s;
        } else {
            var flags = comptime (@as(u32, @intFromEnum(SymbolFlags.top_level)) << 16) | (@as(u32, @intFromEnum(SymbolFlags.late_bound)) << 16);
            if (comptime is_type) {
                flags |= @as(u32, @intFromEnum(SymbolFlags.type)) << 16;
            }

            const s = try this.symbols.push(.{
                .binding = ident_ref,
                .ordinal = flags,
            });
            try m.put(this.allocator, hash, s);
            ident.extra_data = s;

            // if (getLoc(this.nodes, ident)) |loc| {
            //     std.debug.print("[{s}:{}:{}]: symbol decl {s} -> {} [hash: {}] [depth: {}] {s}\n", .{
            //         this.source_name orelse "<unknown>",
            //         loc.line + 1,
            //         loc.col + 1,
            //         getSlice(ident, u8),
            //         s,
            //         hash,
            //         this.type_scopes.items.len,
            //         if (is_type) "[type]" else "",
            //     });
            // }

            // add the unbound symbol to the scope to speed up subsequent binding
            // TODO: perhaps this should only be done at specific depths e.g. 2, 4, 6
            // another approach is to check unbound symbols directly after say, 2 attempts
            //
            // var scope = if (comptime is_type) 
            //     &this.scopes.items[this.scopes.items.len - 1] 
            // else 
            //     &this.type_scopes.items[this.type_scopes.items.len - 1];

            // try scope.put(this.allocator, getHashFromNode(ident), s);
        }
    }

    inline fn addUnboundSymbol(this: *@This(), ident_ref: NodeRef) !void {
        return this._addUnbound(ident_ref, false);
    }

    inline fn addUnboundTypeSymbol(this: *@This(), ident_ref: NodeRef) !void {
        return this._addUnbound(ident_ref, true);
    }

    inline fn shouldExport(this: *@This(), node: *const AstNode) bool {
        _ = this;
        return hasFlag(node, .@"export");
    }

    fn _findSymbol(this: *const @This(), ident: *const AstNode, comptime is_type: bool) ?SymbolRef {
        const h = getHashFromNode(ident);
        const scopes = if (comptime is_type) this.type_scopes else this.scopes;

        std.debug.assert(scopes.items.len != 0);

        var i: usize = scopes.items.len - 1;
        while (true) {
            if (scopes.items[i].get(h)) |ref| {
                if (comptime bind_types and !is_type) {
                    // TODO: reserve the upper bit of scoped refs to mark imported symbols.
                    // Then when an imported symbol is only used as a value (not a type) we 
                    // can confidently say that the symbol _isn't_ a type in this context. Mark 
                    // the symbol as "not a type" and then remove the bit from the reference.
                }
                return ref;
            }
            if (i == 0) break;
            i -= 1;
        }

        return null;
    }

    inline fn findSymbol(this: *const @This(), ident: *const AstNode) ?SymbolRef {
        return this._findSymbol(ident, false);
    }

    inline fn findTypeSymbol(this: *const @This(), ident: *const AstNode) ?SymbolRef {
        return this._findSymbol(ident, true);
    }

    fn visitParams(this: *@This(), start: NodeRef) !void {
        const old_ordinals = this.param_ordinals;
        defer {
            if (comptime bind_types) this.param_ordinals = old_ordinals;
        }
        if (comptime bind_types) this.param_ordinals = 0;

        var iter = NodeIterator.init(this.nodes, start);
        while (iter.nextPair()) |pair| {
            const param = getPackedData(pair[0]);
            if (bind_types and this.param_ordinals == 0) {
                const binding = this.nodes.at(param.left);
                if (binding.kind == .this_keyword) {
                    // TODO: we should directly instantiate the symbol and link to the ambient one
                    try this.lazyBindThis(binding);
                    try this.visitType(pair[0].len);
                    this.param_ordinals += 1;

                    continue;
                }
            }

            try this.visitBinding(param.left, pair[1]);

            if (comptime bind_types) {
                if (pair[0].len != 0) {
                    try this.visitType(pair[0].len);
                }
            }

            if (param.right != 0) {
                try this.visitRef(param.right);
            }

            if (comptime bind_types) this.param_ordinals += 1;
        }
    }

    fn visitBinding(this: *@This(), ref: NodeRef, decl: NodeRef) anyerror!void {
        const binding = this.nodes.at(ref);

        switch (binding.kind) {
            .this_keyword => {}, // TODO: emit error
            .identifier => return this.bindDeclWithBinding(binding, decl, ref),
            .binding_element => {
                binding.extra_data = decl;
                const d = getPackedData(binding);
                return this.visitBinding(d.left, ref);
            },
            .object_binding_pattern => {
                var iter = NodeIterator.init(this.nodes, maybeUnwrapRef(binding) orelse 0);
                while (iter.nextRef()) |x| {
                    try this.visitBinding(x, decl);
                }
            },
            .array_binding_pattern => {
                var pos: u32 = 0;
                var iter = NodeIterator.init(this.nodes, maybeUnwrapRef(binding) orelse 0);
                while (iter.nextPair()) |pair| {
                    pos += 1;
                    if (pair[0].kind != .binding_element) {
                        // needed for omitted exp e.g. `let [x,,z] = [1,2,3]`
                        continue; 
                    }
                    this.nodes.at(pair[1]).extra_data2 = pos; // 1-indexed into the array, tells us we're working with an array binding pattern
                    try this.visitBinding(pair[1], decl);
                }
            },
            else => {
                if (comptime is_debug) {
                    std.debug.print("{any}\n", .{binding.kind});
                }
                unreachable;
            },
        }
    }

    // TODO: use this
    fn maybeVisitBindingInitializers(this: *@This(), ref: NodeRef) anyerror!void {
        const binding = this.nodes.at(ref);

        switch (binding.kind) {
            .binding_element => {
                const r = getPackedData(binding).right;
                if (r != 0) {
                    try this.maybeVisitBindingInitializers(r);
                }
            },
            .object_binding_pattern => {
                var iter = NodeIterator.init(this.nodes, maybeUnwrapRef(binding) orelse return);
                while (iter.nextRef()) |x| {
                    try this.maybeVisitBindingInitializers(x);
                }
            },
            .array_binding_pattern => {
                var iter = NodeIterator.init(this.nodes, maybeUnwrapRef(binding) orelse return);
                while (iter.nextRef()) |x| {
                    try this.maybeVisitBindingInitializers(x);
                }
            },
            else => {},
        }
    }

    // Steps:
    // 1. Bind all declarations (hoisting)
    // 2. Visit nodes
    // 3. Deallocate scope

    fn visitLexicalScope(this: *@This(), node: *const AstNode) !void {
        try this.pushScope();
        defer this.popScope();

        if (comptime bind_types) try this.pushTypeScope();
        defer if (comptime bind_types) this.popTypeScope();

        switch (node.kind) {
            .arrow_function => {
                const d = getPackedData(node);

                const should_visit_types = bind_types and (node.extra_data != 0 or node.len != 0);
                if (should_visit_types) try this.pushTypeScope();
                defer if (should_visit_types) this.popTypeScope();

                const prev_type_ordinals = this.type_ordinals;
                defer if (should_visit_types) {
                    this.type_ordinals = prev_type_ordinals;
                };

                if (should_visit_types) try this.visitTypeParams(node.extra_data);

                try this.visitParams(d.left);

                if (should_visit_types) try this.visitType(node.len);

                const body = this.nodes.at(d.right);
                if (body.kind == .block) {
                    return this.hoistAndVisit(body);
                } else {
                    return this.visit(body, d.right);
                }
            },
            .get_accessor, .set_accessor => {
                const d = getPackedData(node);

                try this.visitParams(d.right);

                if (comptime bind_types) try this.visitType(node.extra_data);
                if (node.len != 0) return this.hoistAndVisit(this.nodes.at(node.len));
            },
            .function_expression, .function_declaration, .method_declaration => {
                const d = getPackedData(node);

                const prev_type_ordinals = this.type_ordinals;
                defer if (comptime bind_types) {
                    this.type_ordinals = prev_type_ordinals;
                };

                if (comptime bind_types) {
                    try this.visitTypeParams(node.extra_data);
                }

                try this.visitParams(d.right);

                if (comptime bind_types) {
                    try this.visitType(node.extra_data2);
                }

                if (node.len != 0) {
                    return this.hoistAndVisit(this.nodes.at(node.len));
                }
            },
            .constructor => {
                const d = getPackedData(node);
                try this.visitParams(d.left);

                if (d.right != 0) {
                    return this.hoistAndVisit(this.nodes.at(d.right));
                }
            },
            else => return this.hoistAndVisit(node),
        }
    }

    fn visitClassScope(this: *@This(), node: *const AstNode, ref: NodeRef) !void {
        const has_type_params = bind_types and node.extra_data != 0;
        const prev_ordinals = this.type_ordinals;
        defer this.type_ordinals = prev_ordinals;

        if (has_type_params) try this.pushTypeScope();
        defer {
            if (has_type_params) this.popTypeScope();
        }

        if (has_type_params) try this.visitTypeParams(node.extra_data);

        // The extends clause is visited prior to the body
        // `super` takes on the result of the expression
        if (node.len != 0) {
            try this.visit(this.nodes.at(node.len), node.len);
        }

        try this.pushThisScope(ref);
        defer this.popThisScope();

        var static_this: SymbolRef = 0;

        const d = getPackedData(node);
        var iter = NodeIterator.init(this.nodes, d.right);
        while (iter.next()) |el| {
            const is_static = hasFlag(el, .static) or el.kind == .class_static_block_declaration;
            if (is_static and static_this == 0) {
                static_this = try this.symbols.push(.{
                    .declaration = ref,
                    .ordinal = 1 << 31, // static flag
                });
            }

            if (is_static) try this.this_scope.append(this.allocator, static_this);
            defer if (is_static) this.popThisScope();

            switch (el.kind) {
                .property_declaration => {
                    const d2 = getPackedData(el);
                    if (d2.right != 0) {
                        try this.visit(this.nodes.at(d2.right), d2.right);
                    }

                    if (comptime bind_types) {
                        try this.visitType(el.len);
                    }
                },
                .method_declaration, .constructor, .class_static_block_declaration => try this.visitLexicalScope(el),
                else => {},
            }
        }
    }

    fn hoistAndVisit(this: *@This(), node: *const AstNode) !void {
        {
            this.should_hoist = true;
            defer this.should_hoist = false;
            try forEachChild(this.nodes, node, this);
        }

        return forEachChild(this.nodes, node, this);
    }

    // Hoisted declarations is something we can compute during parsing given that it's associated with lexical scopes
    // Blocks and top-level scope nodes have plenty of space
    fn hoist(this: *@This(), node: *const AstNode, ref: NodeRef) !void {
        switch (node.kind) {
            .class_declaration => {
                const d = getPackedData(node);
                if (d.left != 0) {
                    this.should_export = this.shouldExport(node);
                    defer this.should_export = false;

                    const ident = this.nodes.at(d.left);
                    try this.bindDeclWithFlagsOrOrdinal(ident, ref, 0, 0, true);
                } else if (hasFlag(node, .default_export)) {
                    this.default_export = ref;
                }
            },
            .function_declaration => {
                const d = getPackedData(node);
                if (d.left != 0) {
                    this.should_export = this.shouldExport(node);
                    defer this.should_export = false;

                    const ident = this.nodes.at(d.left);
                    try this.bindDecl(ident, ref);
                } else if (hasFlag(node, .default_export)) {
                    this.default_export = ref;
                }
            },
            .enum_declaration => {
                const d = getPackedData(node);
                if (d.left != 0) {
                    this.should_export = this.shouldExport(node);
                    defer this.should_export = false;

                    const ident = this.nodes.at(d.left);
                    try this.bindDeclWithFlagsOrOrdinal(ident, ref, 0, @as(u32, @intFromEnum(SymbolFlags.@"enum")) << 16, true);
                } else if (hasFlag(node, .default_export)) {
                    this.default_export = ref;
                }
            },
            .variable_statement => {
                this.should_mark_const = hasFlag(node, .@"const");
                defer this.should_mark_const = false;

                this.should_export = this.shouldExport(node);
                defer this.should_export = false;

                var iter = NodeIterator.init(this.nodes, maybeUnwrapRef(node) orelse 0);
                while (iter.nextPair()) |pair| {
                    const d = getPackedData(pair[0]);
                    try this.visitBinding(d.left, pair[1]);
                }
            },
            .export_declaration => {
                if (!comptime bind_types) {
                    if (hasFlag(node, .declare)) return; // export types
                }

                const d = getPackedData(node);
                if (d.right == 0) return;

                // Re-exports
                if (this.ns != 0) {
                    if (this.namespaces.items[this.ns - 1].exports == null) {
                        // SyntaxError
                        return;
                    }
                }

                var aliased_exports = if (this.ns != 0)
                    &this.namespaces.items[this.ns - 1].exports.?.aliased_exports 
                else 
                    &this.exports.aliased_exports;
    
                const spec = getSlice(this.nodes.at(d.right), u8);

                if (d.left != 0) {
                    const clause = this.nodes.at(d.left);
                    if (clause.kind == .namespace_export) {
                        // export * as foo from 'foo'
                        const binding = unwrapRef(clause);
                        const sym = try this.symbols.push(.{
                            .binding = binding,
                            .ordinal = @as(u32, @intFromEnum(SymbolFlags.exported) | @intFromEnum(SymbolFlags.imported) | @intFromEnum(SymbolFlags.namespace)) << 16
                        });

                        try aliased_exports.append(this.allocator, .{
                            .spec = spec,
                            .binding = binding,
                            .sym = sym,
                        });
                    } else {
                        std.debug.assert(clause.kind == .named_exports);
                        // export { foo as bar, default as x, y } from './x'
                        // todo: you can technically do `export {} from './x'`, need to still import the module?
                        var r = maybeUnwrapRef(clause) orelse 0;
                        while (r != 0) {
                            const spec_node = this.nodes.at(r);
                            const export_data = getPackedData(spec_node);
                            const sym = try this.symbols.push(.{
                                .binding = export_data.right, // this is zero if not renamed
                                .declaration = export_data.left,
                                .ordinal = (@as(u32, @intFromEnum(SymbolFlags.exported)) << 16) | (@as(u32, @intFromEnum(SymbolFlags.imported)) << 16),
                            });

                            try aliased_exports.append(this.allocator, .{
                                .spec = spec,
                                .target = export_data.left,
                                .binding = export_data.right, // this is zero if not renamed
                                .sym = sym,
                            });
                            r = spec_node.next;
                        }
                    }
                }  else {
                    // otherwise it is `export * from 'foo'`
                    const sym = try this.symbols.push(.{
                        .binding = d.right,
                        .declaration = ref,
                        .ordinal = (@as(u32, @intFromEnum(SymbolFlags.aliased_module)) << 16),
                    });

                    try aliased_exports.append(this.allocator, .{
                        .spec = spec,
                        .sym = sym,
                    });
                }
            },
            .import_declaration => {
                if (!comptime bind_types) {
                    if (hasFlag(node, .declare)) { // import type
                        return;
                    }
                }

                const d = getPackedData(node);
                if (d.left == 0) { // no import clause
                    // TODO: we still should track side-effect imports because they can affect ambient state
                    return;
                }

                const clause = this.nodes.at(d.left);
                const clause_data = getPackedData(clause);

                const module = try this.getOrAddImportedModule(getSlice(this.nodes.at(d.right), u8));

                // default import
                if (clause_data.left != 0) {
                    this.nodes.at(clause_data.left).next = ref; // XXX
                    try this.bindImport(module, clause_data.left, null, hasFlag(node, .declare), false);
                }

                // named imports or namespace import
                if (clause_data.right != 0) {
                    const imports = this.nodes.at(clause_data.right);
                    if (imports.kind == .namespace_import) {
                        const binding_ref = unwrapRef(imports);
                        this.nodes.at(binding_ref).next = ref; // XXX

                        return this.bindImport(
                            module,
                            binding_ref,
                            null,
                            hasFlag(node, .declare),
                            true,
                        );
                    }

                    var iter = NodeIterator.init(this.nodes, maybeUnwrapRef(imports) orelse 0);
                    while (iter.next()) |s| {
                        const is_type = hasFlag(node, .declare) or hasFlag(s, .declare);

                        if (!comptime bind_types) {
                            if (is_type) continue;
                        }

                        const d3 = getPackedData(s);
                        const binding_ref = if (d3.right != 0) d3.right else d3.left;
                        const prop = getSlice(this.nodes.at(d3.left), u8);
                        this.nodes.at(binding_ref).next = ref; // XXX

                        try this.bindImport(
                            module,
                            binding_ref,
                            prop,
                            is_type,
                            false,
                        );
                    }
                }
            },
            .module_declaration => {
                const d = getPackedData(node);
                const is_module = d.left != 0 and !hasFlag(node, .let) and this.nodes.at(d.left).kind == .string_literal;
                const hash = if (d.left == 0) Namespace.global_hash else try getHashFromModuleNode(this.nodes.at(d.left));

                var has_non_ns_symbol = false;
                if (this.scopes.items[this.scopes.items.len - 1].get(hash)) |sym_ref| {
                    var sym = this.symbols.at(sym_ref);
                    while (!sym.hasFlag(.namespace)) {
                        if (sym.next == 0) break;
                        sym = this.symbols.at(sym.next);
                    }

                    if (sym.hasFlag(.namespace)) {
                        var ns = &this.namespaces.items[sym.binding];

                        const prior_ns = this.ns;
                        this.ns = sym.binding + 1;
                        defer this.ns = prior_ns;

                        try this.setScope(ns.symbols);
                        try this.setTypeScope(ns.type_symbols);

                        try forEachChild(this.nodes, this.nodes.at(d.right), this);

                        ns.symbols = this.takeScope();
                        ns.type_symbols = this.takeTypeScope();

                        return;
                    }

                    has_non_ns_symbol = true;
                }

                const ns_id: u32 = @intCast(this.namespaces.items.len);

                try this.namespaces.append(this.allocator, .{ .is_global = d.left == 0 });

                var ns = &this.namespaces.items[ns_id];

                if (is_module) {
                    ns.exports = try this.allocator.create(Exports);
                    ns.exports.?.* = Exports{};
                    ns.module_specifier = getSlice(this.nodes.at(d.left), u8);
                }

                {
                    const prior_ns = this.ns;
                    this.ns = ns_id + 1;
                    defer this.ns = prior_ns;

                    try this.pushScope();
                    errdefer this.popScope();

                    try this.pushTypeScope();
                    errdefer this.popTypeScope();

                    try forEachChild(this.nodes, this.nodes.at(d.right), this);

                    // need to reassign
                    ns = &this.namespaces.items[ns_id];
                    ns.symbols = this.takeScope();
                    ns.type_symbols = this.takeTypeScope();
                }

                const s = try this.symbols.push(.{
                    .declaration = ref,
                    .binding = ns_id,
                    .ordinal = @as(u32, @intFromEnum(SymbolFlags.namespace)) << @as(u32, 16),
                });

                if (is_module) {
                    try this.ambient_modules.put(this.allocator, std.hash.Wyhash.hash(0, ns.module_specifier orelse unreachable), s);
                }

                if (has_non_ns_symbol) {
                    const sym_ref = this.scopes.items[this.scopes.items.len - 1].get(hash) orelse unreachable;
                    this.symbols.at(s).next = this.symbols.at(sym_ref).next;
                    this.symbols.at(sym_ref).next = s;
                } else {
                    try this.scopes.items[this.scopes.items.len - 1].put(this.allocator, hash, s);
                    if (!is_module and this.shouldExport(node)) {
                        try this.pushExport(hash, s);
                       // try this.exports.put(hash, s);
                    }
                }
            },
            .interface_declaration, .type_alias_declaration => {
                if (!comptime bind_types) return;

                this.should_export = this.shouldExport(node);
                defer this.should_export = false;

                const d = getPackedData(node);
                try this.bindTypeSymbol(d.left, ref);
            },
            .case_clause, .default_clause => {
                var iter = NodeIterator.init(this.nodes, node.len);
                while (iter.nextPair()) |pair| try this.hoist(pair[0], pair[1]);
                if (node.next != 0) try this.hoist(this.nodes.at(node.next), node.next);
            },
            .import_equals_declaration => {
                this.should_export = this.shouldExport(node);
                defer this.should_export = false;

                const d = getPackedData(node);
                const rhs = this.nodes.at(d.right);
                if (rhs.kind != .external_module_reference) {
                    return this.visitRef(d.left);
                }

                const r = this.nodes.at(d.right);
                const module = try this.getOrAddImportedModule(getSlice(this.nodes.at(unwrapRef(r)), u8));

                // default import
                this.nodes.at(d.left).next = ref; // XXX
                try this.bindImport(module, d.left, null, false, false);
            },
            else => {},
        }
    }

    fn visitTypeParams(this: *@This(), params: NodeRef) !void {
        // We have to do two passes because you can reference any type param in the constraint/default position
        {
            var iter = NodeIterator.init(this.nodes, params);
            while (iter.nextPair()) |pair| {
                const d = getPackedData(pair[0]);
                try this.bindTypeSymbolWithOrdinal(d.left, pair[1], this.type_ordinals);
                this.type_ordinals += 1;
            }
        }

        {
            var iter = NodeIterator.init(this.nodes, params);
            while (iter.next()) |n| {
                const d = getPackedData(n);
                try this.visitType(d.right);
                try this.visitType(n.len);
            }
        }
    }

    fn visitType(this: *@This(), ref: NodeRef) anyerror!void {
        if (!comptime bind_types) return;

        if (ref == 0) return;

        // for (0..this.type_scopes.items.len) |_| {
        //     std.debug.print("  ", .{});
        // }

        // std.debug.print("{any}\n", .{this.nodes.at(ref).kind});

        const node = this.nodes.at(ref);
        switch (node.kind) {
            .identifier => {
                if (this.findTypeSymbol(node)) |s| {
                    node.extra_data = s;
                } else {
                    try this.addUnboundTypeSymbol(ref);
                }
            },
            // .this_keyword => {
            //     try this.lazyBindThis(node);
            // },
            .type_operator => {
                const d = getPackedData(node);
                try this.visitType(d.right);
            },
            .array_type, .parenthesized_type => {
                try this.visitType(unwrapRef(node));
            },
            .mapped_type => {
                try this.pushTypeScope();
                defer this.popTypeScope();

                const prev_ordinals = this.type_ordinals;
                defer this.type_ordinals = prev_ordinals;

                const d = getPackedData(node);
                try this.visitTypeParams(d.left);
                try this.visitType(d.right);
                try this.visitType(node.len);
            },
            .type_reference => {
                const d = getPackedData(node);
                try this.visitType(d.left);
                var iter = NodeIterator.init(this.nodes, d.right);
                while (iter.nextPair()) |pair| {
                    try this.visitType(pair[1]);
                }
            },
            .type_alias_declaration => {
                const d = getPackedData(node);

                try this.pushTypeScope();
                defer this.popTypeScope();

                const prev_ordinals = this.type_ordinals;
                defer this.type_ordinals = prev_ordinals;

                try this.visitTypeParams(d.right);
                try this.visitType(node.len);
            },
            .qualified_name => {
                const d = getPackedData(node);
                try this.visitRef(d.left);
            },
            .property_signature => {
                const d = getPackedData(node);
                if (this.nodes.at(d.left).kind == .computed_property_name) {
                    try this.visitRef(unwrapRef(this.nodes.at(d.left)));
                }
                try this.visitType(d.right);
            },
            .method_signature => {
                const d = getPackedData(node);
                if (this.nodes.at(d.left).kind == .computed_property_name) {
                    try this.visitRef(unwrapRef(this.nodes.at(d.left)));
                }

                try this.pushTypeScope();
                defer this.popTypeScope();

                const prev_ordinals = this.type_ordinals;
                defer this.type_ordinals = prev_ordinals;

                try this.visitTypeParams(node.extra_data);
                try this.visitParams(d.right);

                try this.visitType(node.len); // return_type
            },
            .index_signature => {
                const d = getPackedData(node);
                try this.visitType(d.right);
                try this.visitType(node.len);
            },
            .indexed_access_type => {
                const d = getPackedData(node);
                try this.visitType(d.left);
                try this.visitType(d.right);
            },
            .call_signature, .construct_signature => {
                try this.pushScope();
                defer this.popScope();

                try this.pushTypeScope();
                defer this.popTypeScope();

                const prev_ordinals = this.type_ordinals;
                defer this.type_ordinals = prev_ordinals;

                const d = getPackedData(node);
                try this.visitTypeParams(node.len);

                try this.visitParams(d.left);
                try this.visitType(d.right);
            },
            .interface_declaration => {
                const d = getPackedData(node);

                try this.pushTypeScope();
                defer this.popTypeScope();

                const prev_ordinals = this.type_ordinals;
                defer this.type_ordinals = prev_ordinals;

                try this.visitTypeParams(node.len);

                var iter = NodeIterator.init(this.nodes, d.right);
                while (iter.nextPair()) |p| {
                    try this.visitType(p[1]);
                }

                var extends_iter = NodeIterator.init(this.nodes, node.extra_data);
                while (extends_iter.nextRef()) |r| try this.visitType(r);
            },
            .tuple_type => {
                var iter = NodeIterator.init(this.nodes, maybeUnwrapRef(node) orelse 0);
                while (iter.nextPair()) |pair| {
                    switch (pair[0].kind) {
                        .rest_type, .optional_type => try this.visitType(unwrapRef(pair[0])),
                        .named_tuple_member => {
                            const d = getPackedData(pair[0]);
                            try this.visitType(d.right);
                        },
                        else => try this.visitType(pair[1]),
                    }
                }
            },
            .conditional_type => {
                const d = getPackedData(node);
                try this.visitType(d.left);

                // We add a scope to handle `infer`
                // No need to hoist because we will always visit the type params before the true/false expressions
                try this.pushTypeScope();
                defer this.popTypeScope();

                const prev_ordinals = this.type_ordinals;
                defer this.type_ordinals = prev_ordinals;

                {
                    const prev_infer_scope = this.infer_scope;
                    defer this.infer_scope = prev_infer_scope;

                    this.infer_scope = @as(u32, @intCast(this.type_scopes.items.len)) - 1;
                    try this.visitType(d.right);
                }

                try this.visitType(node.len);
                try this.visitType(node.extra_data);
            },
            .infer_type => {
                const param_ref = unwrapRef(node);
                const param = this.nodes.at(param_ref);

                // multiple type params w/ the same binding _are_ allowed in the same scope,
                // but they must all have the same constraint.
                // No constraint on one decl will use a constraint on another if present.
                // type X<T> = `T extends { u: infer U } | { v: infer U extends number } ? U : never`
                // type Y = X<{ u: string }> // `never`
                const d = getPackedData(param);

                const infer_scope = this.infer_scope orelse return error.InvalidInferLocation;

                const scope = &this.type_scopes.items[infer_scope];
                const ident = this.nodes.at(d.left);
                if (scope.get(ident.extra_data2)) |prev| {
                    var s = this.symbols.at(prev);
                    const n2 = this.nodes.at(s.declaration);

                    // Replace it w/ the more constrained param
                    if ((getPackedData(n2)).right == 0 and d.right != 0) {
                        s.declaration = ref;
                        s.binding = d.left;
                    }
                    // try this.types_symbol_map.put(ref, prev);
                    ident.extra_data = prev;
                } else {
                    std.debug.assert(this.type_ordinals < 65536);

                    const flags: u16 = @intFromEnum(SymbolFlags.type_parameter);
                    const flags_and_ordinal: u32 = (@as(u32, flags) << 16) | this.type_ordinals;

                    const s = try this.symbols.push(.{
                        .declaration = param_ref,
                        .ordinal = flags_and_ordinal,
                    });

                    try scope.put(this.allocator, getHashFromNode(ident), s);
                    ident.extra_data = s;
                    this.type_ordinals += 1;
                }

                try this.visitType(d.right);
                try this.visitType(param.len);
            },
            .type_parameter => {
                // Visiting this node means invalid syntax
                return error.UnexpectedTypeParameter;
            },
            .union_type, .intersection_type => {
                const d = getPackedData(node);
                try this.visitType(d.left);
                try this.visitType(d.right);
            },
            .type_literal => {
                var iter = NodeIterator.init(this.nodes, maybeUnwrapRef(node) orelse 0);
                while (iter.nextRef()) |r| try this.visitType(r);
            },
            .expression_with_type_arguments => {
                const d = getPackedData(node);
                try this.visitType(d.left);
                var iter = NodeIterator.init(this.nodes, d.right);
                while (iter.nextRef()) |r| try this.visitType(r);
            },
            .type_query => {
                const exp = unwrapRef(node);
                try this.visitRef(exp);
                var iter = NodeIterator.init(this.nodes, node.len);
                while (iter.nextRef()) |r| try this.visitType(r);
            },
            .type_predicate => {
                const d = getPackedData(node);
                try this.visitRef(d.left);
                try this.visitType(d.right);
            },
            .function_type, .constructor_type => {
                const d = getPackedData(node);

                const has_params = d.left != 0;
                const has_type_params = node.len != 0;
                if (has_params) try this.pushScope();
                defer if (has_params) this.popScope();

                if (has_type_params) try this.pushTypeScope();
                defer if (has_type_params) this.popTypeScope();

                const prev_ordinals = this.type_ordinals;
                defer this.type_ordinals = prev_ordinals;

                if (has_type_params) try this.visitTypeParams(node.len);

                if (has_params) try this.visitParams(d.left);

                try this.visitType(d.right);
            },
            .template_literal_type => {
                const head_ref = unwrapRef(node);
                var next: NodeRef = this.nodes.at(head_ref).next;
                while (next != 0) {
                    const span = this.nodes.at(next);
                    const d = getPackedData(span);
                    try this.visitType(d.left);
                    next = span.next;
                }
            },
            else => {},
        }
    }

    inline fn _pushIntoScopes(this: *@This(), scopes: *Scopes) !void {
        if (scopes.items.len < warm_stack_depth) {
            scopes.items.len += 1;
        } else {
            try scopes.append(this.allocator, .{});
        }
    }

    inline fn _setIntoScopes(this: *@This(), scopes: *Scopes, table: SymbolTable) !void {
        const len = scopes.items.len;
        if (len < warm_stack_depth) {
            var s = scopes.allocatedSlice();
            s[len].clearAndFree(this.allocator);
            s[len] = table;
            scopes.items.len += 1;
        } else {
            try scopes.append(this.allocator, table);
        }
    }

    inline fn _takeFromScopes(this: *@This(), scopes: *Scopes) SymbolTable {
        _ = this;
        std.debug.assert(scopes.items.len > 0);
        const ind = scopes.items.len - 1;
        const val = scopes.items[ind];
        scopes.items[ind] = .{};
        scopes.items.len -= 1;
        return val;
    }

    inline fn _popFromScopes(this: *@This(), scopes: *Scopes) void {
        const len = scopes.items.len;
        std.debug.assert(len > 0);
        if (len < warm_stack_depth) {
            scopes.items[len-1].clearRetainingCapacity();
            scopes.items.len -= 1;
        } else {
            var s = scopes.pop();
            s.deinit(this.allocator);
        }
    }

    inline fn pushScope(this: *@This()) !void {
        return this._pushIntoScopes(&this.scopes);
    }

    inline fn setScope(this: *@This(), table: SymbolTable) !void {
        return this._setIntoScopes(&this.scopes, table);
    }

    inline fn popScope(this: *@This()) void {
        return this._popFromScopes(&this.scopes);
    }

    inline fn takeScope(this: *@This()) SymbolTable {
        return this._takeFromScopes(&this.scopes);
    }

    inline fn pushTypeScope(this: *@This()) !void {
        return this._pushIntoScopes(&this.type_scopes);
    }

    inline fn setTypeScope(this: *@This(), table: SymbolTable) !void {
        return this._setIntoScopes(&this.type_scopes, table);
    }

    inline fn popTypeScope(this: *@This()) void {
        return this._popFromScopes(&this.type_scopes);
    }

    inline fn takeTypeScope(this: *@This()) SymbolTable {
        return this._takeFromScopes(&this.type_scopes);
    }

    inline fn pushThisScope(this: *@This(), decl: NodeRef) !void {
        const len = this.this_scope.items.len;
        // the symbol is created lazily. if there is no `this`, no symbol is needed
        // this uses the msb to track if the symbol has been allocated
        const d = decl | (1 << 31); 
        if (len < warm_stack_depth) {
            this.this_scope.allocatedSlice()[len] = d;
            this.this_scope.items.len = len + 1;
        } else {
            try this.this_scope.append(this.allocator, d);
        }
    }

    inline fn popThisScope(this: *@This()) void {
        const len = this.this_scope.items.len;
        std.debug.assert(len > 0);
        if (len < warm_stack_depth) {
            this.this_scope.items.len = len - 1;
        } else {
            _ = this.this_scope.pop();
        }
    }

    inline fn visitRef(this: *@This(), ref: NodeRef) !void {
        return this.visit(this.nodes.at(ref), ref);
    }

    // The lhs of an assignment expression sometimes needs to be interpretted as a binding pattern
    // if we see an array or object literal, treat it as a binding (this isn't needed right now)

    pub fn visit(this: *@This(), node: *const AstNode, ref: NodeRef) anyerror!void {
        if (this.should_hoist) {
            return this.hoist(node, ref);
        }

        switch (node.kind) {
            .identifier => {
                if (this.findSymbol(node)) |s| {
                    this.nodes.at(ref).extra_data = s;
                } else {
                    try this.addUnboundSymbol(ref);
                }
            },
            .this_keyword => {
                try this.lazyBindThis(this.nodes.at(ref));
            },
            .import_declaration => {}, // Already hoisted
            .type_alias_declaration, .interface_declaration => {
                if (!comptime bind_types) return;
                return this.visitType(ref);
            },
            .export_assignment => {
                const inner = unwrapRef(node);
                this.default_export = inner;
                try this.visitRef(inner);
            },
            .import_equals_declaration => {
                const d = getPackedData(node);
                const rhs = this.nodes.at(d.right);
                if (rhs.kind != .external_module_reference) {
                    return this.visitRef(d.right);
                }
            },
            .is_expression => {
                const d = getPackedData(node);
                try this.visitRef(d.left);
                try this.visitType(d.right);  
            },
            .reify_expression => {
                const prev_ordinals = this.type_ordinals;
                defer this.type_ordinals = prev_ordinals;

                const has_type_params = node.len != 0;

                if (has_type_params) try this.pushTypeScope();
                defer if (has_type_params) this.popTypeScope();

                if (has_type_params) try this.visitTypeParams(node.len);

                return this.visitType(unwrapRef(node));
            },
            .enum_declaration => {
                const d = getPackedData(node);

                // Enum scope
                try this.pushScope();
                defer this.popScope();

                // Two passes because enum members can reference each other
                // Though it's considered invalid to use a member out-of-order
                {
                    var iter = NodeIterator.init(this.nodes, d.right);
                    while (iter.next()) |el| {
                        const d2 = getPackedData(el);
                        try this.bindDeclWithBinding(this.nodes.at(d2.left), ref, d2.left);
                    }
                }

                {
                    var iter = NodeIterator.init(this.nodes, d.right);
                    while (iter.next()) |el| {
                        const d2 = getPackedData(el);
                        try this.visit(this.nodes.at(d2.right), d2.right);
                    }
                }
            },
            .expression_with_type_arguments => {
                const d = getPackedData(node);
                try this.visitRef(d.left);
                if (comptime bind_types) {
                    var iter = NodeIterator.init(this.nodes, d.right);
                    while (iter.nextRef()) |r| try this.visitType(r);
                }
            },
            .call_expression => {
                const d = getPackedData(node);
                try this.visitRef(d.left);
                var iter = NodeIterator.init(this.nodes, d.right);
                while (iter.nextRef()) |r| try this.visitRef(r);
                if (comptime bind_types) {
                    var types_iter = NodeIterator.init(this.nodes, node.len);
                    while (types_iter.nextRef()) |r| try this.visitType(r);
                }
            },
            .as_expression, .satisfies_expression => {
                const d = getPackedData(node);
                try this.visitRef(d.left);
                try this.visitType(d.right);
            },
            .variable_declaration => {
                const d = getPackedData(node);
                try this.visitType(node.len);
                if (d.right != 0) {
                    try this.visitRef(d.right);
                }
            },
            .if_statement, .while_statement => {
                if (!comptime enable_conditional_bindings) {
                    return forEachChild(this.nodes, node, this);
                }
                const d = getPackedData(node);
                const exp = this.nodes.at(d.left);
                if (exp.kind != .variable_statement) {
                    return forEachChild(this.nodes, node, this);
                }

                const decl = unwrapRef(exp);
                const decl_node = this.nodes.at(decl);
                const q = getPackedData(decl_node);

                // visit type/initializer first, meaning `if (const x = x)` is currently possible...
                try this.visitType(decl_node.len);
                if (q.right != 0) {
                    try this.visitRef(q.right);
                }

                {
                    try this.pushScope();
                    defer this.popScope();

                    try this.visitBinding(q.left, decl);   // conditional binding
                    try this.visitRef(d.right);                      // `thenStatement` or `statement`
                }

                // elseStatement
                if (node.kind == .if_statement and node.len != 0) {
                    try this.visitRef(node.len);
                }
            },
            .property_access_expression => {
                const d = getPackedData(node);
                try this.visit(this.nodes.at(d.left), d.left);
            },
            .object_literal_expression => {
                var iter = NodeIterator.init(this.nodes, maybeUnwrapRef(node) orelse 0);
                while (iter.next()) |el| {
                    switch (el.kind) {
                        .spread_assignment, .shorthand_property_assignment => {
                            try this.visitRef(unwrapRef(el));
                        },
                        .property_assignment => {
                            const d = getPackedData(el);
                            if (this.nodes.at(d.left).kind == .computed_property_name) {
                                try this.visitRef(unwrapRef(this.nodes.at(d.left)));
                            }
                            try this.visitRef(d.right);
                        },
                        .method_declaration, .set_accessor, .get_accessor => {
                            const d = getPackedData(el);
                            if (this.nodes.at(d.left).kind == .computed_property_name) {
                                try this.visitRef(unwrapRef(this.nodes.at(d.left)));
                            }
                            return try this.visitLexicalScope(el);
                        },
                        else => return error.TODO,
                    }
                }
            },
            .try_statement => {
                const d = getPackedData(node);
                try this.visitRef(d.left);

                // catch
                if (d.right != 0) {
                    const clause = getPackedData(this.nodes.at(d.right));
                    if (clause.left != 0) {
                        try this.pushScope();
                        defer this.popScope();

                        const decl = this.nodes.at(clause.left);
                        const q = getPackedData(decl);
                        try this.visitBinding(q.left, clause.left);
                        if (decl.len != 0) {
                            try this.visitType(decl.len);
                        }

                        try this.visitRef(clause.right);
                    } else {
                        try this.visitRef(clause.right);
                    }
                }

                // finally
                if (node.len != 0) {
                    try this.visitRef(node.len);
                }
            },
            // First val is either a variable statement or a binding w/ `for` statements
            .for_statement => {
                const d = getPackedData(node);
                const f = this.nodes.at(d.left);
                if (f.kind != .variable_statement) {
                    if (d.left != 0) {
                        try this.visitRef(d.left);
                    }
                    if (d.right != 0) {
                        try this.visitRef(d.right);
                    }
                    if (node.len != 0) {
                        try this.visitRef(node.len);
                    }
                    return this.visitRef(node.extra_data);
                }

                try this.pushScope();
                defer this.popScope();

                try this.hoist(this.nodes.at(d.left), d.left);

                if (d.right != 0) {
                    try this.visitRef(d.right);
                }
                if (node.len != 0) {
                    try this.visitRef(node.len);
                }

                return this.visitRef(node.extra_data);
            },
            .for_in_statement, .for_of_statement => {
                const d = getPackedData(node);
                const f = this.nodes.at(d.left);
                if (f.kind != .variable_statement) {
                    try this.visitRef(d.left);
                    try this.visitRef(d.right);
                    return this.visitRef(node.len);
                }

                try this.visitRef(d.right);

                try this.pushScope();
                defer this.popScope();

                try this.hoist(this.nodes.at(d.left), d.left);

                return this.visitRef(node.len);
            },
            .case_clause, .default_clause => {
                if (node.data) |d| {
                    if (node.hasFlag(.declare)) {
                        try this.visitType(@intCast(@intFromPtr(d)));
                    } else {
                        try this.visitRef(@intCast(@intFromPtr(d)));
                    }
                }

                var iter = NodeIterator.init(this.nodes, node.len);
                while (iter.nextPair()) |pair| try this.visit(pair[0], pair[1]);
                if (node.next != 0) try this.visitRef(node.next);
            },
            .switch_statement => {
                const d = getPackedData(node);

                if (this.nodes.at(d.left).kind == .variable_statement) {
                    const decl = unwrapRef(this.nodes.at(d.left));
                    const decl_node = this.nodes.at(decl);
                    const q = getPackedData(decl_node);

                    // visit type/initializer first
                    try this.visitType(decl_node.len);
                    if (q.right != 0) {
                        try this.visitRef(q.right);
                    }

                    try this.pushScope();
                    defer this.popScope();

                    try this.visitBinding(q.left, decl);

                    return this.visitRef(d.right);
                }

                try this.visitRef(d.left);

                try this.pushScope();
                defer this.popScope();

                try this.hoist(this.nodes.at(d.right), d.right);

                return this.visitRef(d.right);
            },
            .export_declaration => {
                if (!comptime bind_types) {
                    if (hasFlag(node, .declare)) return;
                }

                const d = getPackedData(node);

                // Re-exports
                if (d.right != 0) {
                    return;
                }

                if (d.left == 0) return;

                const clause = this.nodes.at(d.left);
                if (clause.kind != .named_exports) {
                    // todo ?
                    return;
                }

                var iter = NodeIterator.init(this.nodes, maybeUnwrapRef(clause) orelse 0);
                while (iter.nextPair()) |p| {
                    const binding = p[0];
                    try this.visit(binding, p[1]);

                    if (hasFlag(node, .declare) or hasFlag(binding, .declare)) {
                        try this.visitType(p[1]);
                    }
                }
            },
            .module_declaration => {
                // Everything in the block has already been hoisted
                const d = getPackedData(node);
                const hash = if (d.left == 0) Namespace.global_hash else try getHashFromModuleNode(this.nodes.at(d.left));
                const sym_ref = this.scopes.items[this.scopes.items.len - 1].get(hash) orelse return error.MissingNamespaceSymbol;

                var sym = this.symbols.at(sym_ref);
                while (!sym.hasFlag(.namespace)) {
                    if (sym.next == 0) break;
                    sym = this.symbols.at(sym.next);
                }

                if (!sym.hasFlag(.namespace)) {
                    if (d.left == 0) std.debug.print("global\n", .{}) else std.debug.print("{s}\n", .{getSlice(this.nodes.at(d.left), u8)});
                }

                std.debug.assert(sym.hasFlag(.namespace));

                // `ns` already hoisted, so it cannot mutate here
                const ns = this.namespaces.items[sym.binding];

                try this.setScope(ns.symbols);
                defer _ = this.takeScope();

                try this.setTypeScope(ns.type_symbols);
                defer _ = this.takeTypeScope();

                var iter = NodeIterator.init(this.nodes, maybeUnwrapRef(this.nodes.at(d.right)) orelse 0);
                while (iter.nextRef()) |r| try this.visitRef(r);
            },
            .source_file => {
                try this.pushThisScope(ref);
                defer this.popThisScope();

                try this.pushScope();
                defer {
                    if (!this.is_lib)
                        this.popScope()
                    else
                        this.exports.symbols = this.takeScope();
                }

                if (comptime bind_types) try this.pushTypeScope();
                defer if (comptime bind_types) {
                    if (!this.is_lib)
                        this.popTypeScope()
                    else
                        this.exports.type_symbols = this.takeTypeScope();
                };

                return try this.hoistAndVisit(node);
            },
            .function_declaration => {
                try this.pushThisScope(ref);
                defer this.popThisScope();

                return this.visitLexicalScope(node);
            },
            .function_expression => {
                const needs_extra_scope = getPackedData(node).left != 0;

                // This should create 3 symbols
                // const f = function f() {
                //     function f() {}
                // }
                if (needs_extra_scope) {
                    try this.pushScope();
                    try this.bindDecl(this.nodes.at(getPackedData(node).left), ref);
                }
                defer {
                    if (needs_extra_scope) this.popScope();
                }

                try this.pushThisScope(ref);
                defer this.popThisScope();

                return this.visitLexicalScope(node);
            },
            .block, .arrow_function => return this.visitLexicalScope(node),
            .class_expression, .class_declaration => {
                // TODO: we should wait until we check type param before pushing another scope
                const needs_extra_scope = node.kind == .class_expression and getPackedData(node).left != 0;

                // This should create 3 symbols
                // const f = class f {
                //   static { class f {} }
                // }
                if (needs_extra_scope) {
                    try this.pushScope();
                    if (comptime bind_types) try this.pushTypeScope();
                    try this.bindDeclWithFlagsOrOrdinal(this.nodes.at(getPackedData(node).left), ref, 0, 0, true);
                }
                defer {
                    if (needs_extra_scope) {
                        this.popScope();
                        if (comptime bind_types) this.popTypeScope();
                    }
                }

                return this.visitClassScope(node, ref);
            },
            else => return forEachChild(this.nodes, node, this),
        }
    }

    pub fn getSymbol(this: *const @This(), ref: NodeRef) ?SymbolRef {
        const n = this.nodes.at(ref);
        return switch (n.kind) {
            .identifier => n.extra_data,
            .this_keyword => if (n.extra_data != 0) n.extra_data else null,
            .function_declaration, .class_declaration, .variable_declaration, .parameter => this.getSymbol(getPackedData(n).left),
            else => null,
        };
    }

    pub fn getTypeSymbol(this: *const @This(), ref: NodeRef) ?SymbolRef {
        const n = this.nodes.at(ref);
        return switch (n.kind) {
            .identifier => n.extra_data,
            .parenthesized_type => this.getTypeSymbol(unwrapRef(n)),
            .class_declaration, .type_alias_declaration, .interface_declaration, .enum_declaration, .type_parameter => this.getTypeSymbol(getPackedData(n).left),
            else => null,
        };
    }
};

pub fn getLoc(nodes: *const BumpAllocator(AstNode), n: *const AstNode) ?struct { line: u32, col: u32 } {
    switch (n.kind) {
        .identifier, .numeric_literal, .string_literal, .arrow_function, .array_literal_expression => {
            const x = decodeLocation(n.location);

            return .{
                .line = x.line,
                .col = x.col,
            };
        },
        .class_declaration, .function_declaration => {
            const d = getPackedData(n);
            if (d.left != 0) {
                return getLoc(nodes, nodes.at(d.left));
            }
        },
        .await_expression => {
            // TODO: add location to `await`, this is dead code currently
            if (n.location != 0) {
                const x = decodeLocation(n.location);

                return .{
                    .line = x.line,
                    .col = x.col,
                };
            }
            return getLoc(nodes, nodes.at(unwrapRef(n)));
        },
        .typeof_expression => {
            // FIXME: add location to typeof
            return getLoc(nodes, nodes.at(unwrapRef(n)));
        },
        .as_expression,
        .element_access_expression, .binary_expression,
        .property_access_expression, .call_expression, .qualified_name,
        .type_alias_declaration, .enum_declaration, .interface_declaration, .variable_declaration, .parameter, .type_parameter, .module_declaration => {
            const d = getPackedData(n);
            return getLoc(nodes, nodes.at(d.left));
        },
        else => {
            if (n.location != 0) {
                const x = decodeLocation(n.location);
                return .{
                    .line = x.line,
                    .col = x.col,
                };
            }
            std.debug.print("missing location {}\n", .{n.kind});
        },
    }

    return null;
}

pub fn getIdentFromSymbol(binder: *const Binder, ref: SymbolRef) ?*const AstNode {
    std.debug.assert(ref != 0);

    const sym = binder.symbols.at(ref);
    if (sym.binding != 0 and !sym.hasFlag(.global) and !sym.hasFlag(.type_parameter)) {
        return binder.nodes.at(sym.binding);
    }

    std.debug.assert(!hasSymbolFlag(sym, .late_bound));

    if (sym.declaration == 0) return null;
    const decl = binder.nodes.at(sym.declaration);

    switch (decl.kind) {
        .type_alias_declaration, .interface_declaration, .class_declaration, .function_declaration => {
            const d = getPackedData(decl);

            return if (d.left == 0) null else binder.nodes.at(d.left);
        },
        .variable_declaration, .parameter, .type_parameter => {
            const d = getPackedData(decl);

            return binder.nodes.at(d.left);
        },
        else => {},
    }

    return null;
}

pub const ParsedFile = struct {
    ast: AstData,
    binder: Binder,
    source: []const u8,
    source_name: ?[]const u8 = null,
    owns_source: bool = false,
    is_lib: bool = false,

    // Used to store the associated JS object
    api_handle: ?*anyopaque = null,

    const allocator = getAllocator();

    fn log(this: *@This(), comptime fmt: []const u8, args: anytype) void {
        if (comptime true) return;
        const name = this.source_name orelse return;
        const new_fmt = comptime "{s}: " ++ fmt ++ "\n";
        std.debug.print(new_fmt, .{name} ++ args);
    }

    pub fn deinit(this: *@This()) void {
        this.binder.deinit();
        this.ast.deinit();

        if (this.owns_source) allocator.free(this.source);

        allocator.destroy(this);
    }

    pub fn createFromBuffer(_buf: []const u8, source_name: ?[]const u8, is_lib: bool, listener: ?*ImportListener) !*@This() {
        const buf = if (comptime @import("builtin").os.tag != .windows) _buf else blk: {
            const bom = strings.BOM.detect(_buf) orelse break :blk _buf;

            break :blk try strings.BOM.convertToUTF8(bom, allocator, _buf);
        };

        if (buf.len == 0) {
            // TODO: add test for this
            var this = try allocator.create(@This());
            var nodes = BumpAllocator(AstNode).init(allocator, std.heap.page_allocator);
            try nodes.preAlloc();
            _ = try nodes.push(.{ .kind = .start });
            const root_ref = try nodes.push(.{
                .kind = .source_file,
                .data = null, 
            });
            this.* = .{
                .ast = .{
                    .start = root_ref,
                    .nodes = nodes,
                    .source = buf,
                    .source_name = source_name,
                    .decorators = NodeMap{},
                },
                .binder = Binder.init(&this.ast.nodes, allocator),
                .source = buf,
                .source_name = source_name,
                .is_lib = is_lib,
            };
            return this;
        }

        const parse_start = std.time.microTimestamp();
        const lexer = try js_lexer.Lexer.init(.{ .contents = buf, .name = source_name }, allocator);

        var parser = Parser.init(lexer);
        parser.import_listener = listener;

        const result = try parser.parse();
        const parse_time = std.time.microTimestamp() - parse_start;

        var this = try allocator.create(@This());
        this.* = .{
            .ast = result.data,
            .binder = Binder.init(&this.ast.nodes, allocator),
            .source = buf,
            .source_name = source_name,
            .is_lib = is_lib,
        };

        this.binder.is_lib = is_lib;
        this.binder.source_name = source_name; // For logging

        this.log("parse time {d:.3} (# of nodes {})", .{ parse_time, result.data.nodes.count() });

        const bind_start = std.time.microTimestamp();
        try this.binder.visit(&result.root, result.root_ref);
        this.log("bind time {d:.3} (# of symbols {})", .{ std.time.microTimestamp() - bind_start, this.binder.symbols.count() });

        return this;
    }

    pub fn createFromPath(file_path: []const u8, is_lib: bool) !*@This() {
        const buf = std.fs.cwd().readFileAlloc(allocator, file_path, std.math.maxInt(u32)) catch |err| {
            std.debug.print("failed to open file {s}: {any}\n", .{file_path, err});
            return err;
        };
        var this = try @This().createFromBuffer(buf, file_path, is_lib, null);
        this.owns_source = true;

        return this;
    }
};

pub fn getNumber(node: *const AstNode) f64 {
    if (node.data) |d| {
        const val: u64 = @intFromPtr(d);

        return @bitCast(val);
    }

    return 0;
}

// Options are mostly aligned with `tsc`
pub const PrinterOptions = struct {
    skip_types: bool = true,
    remove_comments: bool = true,
    // new_line_kind: LF | CRLF
    omit_trailing_semicolon: bool = false,
    // no_emit_helpers: bool = false,

    // handlers: ?*anyopaque = null,
    transform_to_cjs: bool = false,

    // Source maps
    emit_source_map: bool = false,
    inline_source_map: bool = false,
    source_map_root_dir: ?[]const u8 = null,
    file_name: ?[]const u8 = null,

    replacements: ?*anyopaque = null,
};

pub const ComptimePrinterOptions = struct {
    print_source_map: bool = false,
    use_replacements: bool = false,
    use_symbol_replacements: bool = false,
};

pub fn Printer(comptime Sink: type, comptime opt: ComptimePrinterOptions) type {
    return _Printer(Sink, opt.print_source_map, opt.use_replacements, opt.use_symbol_replacements);
}

pub fn _Printer(comptime Sink: type, comptime print_source_map: bool, comptime use_replacements: bool, comptime use_symbol_replacements: bool) type {
    const Scope = enum {
        none,
        using,
        async_using,
    };

    return struct {
        data: AstData,
        indent: u16 = 0,
        needs_newline: bool = false,

        sink: *Sink,

        skip_types: bool = false,
        is_using_scope: bool = false,
        needs_super: bool = false,

        scope: Scope = .none,

        source_map: SourceMap,

        helpers: u32 = 0,

        transformer: ?Transformer = null,
        replacements: ?*std.AutoArrayHashMap(NodeRef, NodeRef) = null,

        symbol_replacements: if (use_symbol_replacements) *const std.AutoArrayHashMapUnmanaged(SymbolRef, NodeRef) else void = undefined,

        inline fn addHelper(this: *@This(), tag: HelperTags) void {
            this.helpers |= 1 << @intFromEnum(tag);
        }

        fn printHelpers(this: *@This()) void {
            inline for (@typeInfo(HelperTags).Enum.fields) |field| {
                if ((this.helpers & (1 << field.value)) != 0) {
                    this.print(getHelper(@field(HelperTags, field.name)));
                    this.needs_newline = true;
                }
            }
        }

        pub fn init(data: AstData, sink: *Sink) @This() {
            return .{
                .data = data,
                .sink = sink,
                .source_map = SourceMap.init(getAllocator()),
            };
        }

        fn print(this: *@This(), text: []const u8) void {
            if (this.needs_newline) {
                this.printLine();
                this.needs_newline = false;
            }

            this.sink.write(text);
        }

        fn printSignature(this: *@This(), n: *const AstNode) !void {
            const d = getPackedData(n);
            try this.printTypeParams(n.len);
            try this.printParams(d.left);
            try this.maybePrintType(d.right);
        }

        fn printLine(this: *@This()) void {
            this.sink.write("\n");

            if (comptime print_source_map) {
                this.source_map.endLine(this.sink.pos()) catch unreachable;
            }

            if (this.indent == 0) return;

            if (this.indent == 2) {
                this.sink.write("  ");
            } else if (this.indent == 4) {
                this.sink.write("    ");
            } else if (this.indent == 6) {
                this.sink.write("      ");
            } else if (this.indent == 8) {
                this.sink.write("        ");
            } else {
                var tmp = this.indent;
                while (tmp >= 8) : (tmp -= 8) this.sink.write("        ");
                while (tmp >= 4) : (tmp -= 4) this.sink.write("    ");
                while (tmp >= 2) : (tmp -= 2) this.sink.write("  ");
                if (tmp == 1) this.sink.write(" ");
            }
        }

        inline fn printNumber(this: *@This(), val: anytype) !void {
            const f: f64 = @bitCast(val);
            var buf: [64]u8 = undefined;
            this.print(try std.fmt.bufPrint(&buf, "{d}", .{f}));
        }

        inline fn toIterator(this: *@This(), ref: NodeRef) NodeIterator {
            return NodeIterator.init(&this.data.nodes, ref);
        }

        inline fn visitRef(this: *@This(), ref: usize) !void {
            return this.visit(this.getNode(@intCast(ref)));
        }

        inline fn visitRefUnwrapStatement(this: *@This(), ref: NodeRef) !void {
            const n = this.getNode(ref);
            if (n.kind == .expression_statement) {
                return this.visitRef(unwrapRef(n));
            }

            return this.visit(n);
        }

        inline fn getTransformer(this: *@This()) *Transformer {
            if (this.transformer) |*t| return t;
            this.transformer = .{ .allocator = &this.data.nodes };

            return &this.transformer.?;
        }

        inline fn getNode(this: *@This(), ref: NodeRef) *const AstNode {
            if (comptime use_replacements) {
                const maybe_ref = if (this.replacements) |m| m.get(ref) else null;
                if (maybe_ref) |r| {
                    return this.data.nodes.at(r);
                }
            }
            return this.data.nodes.at(ref);
        }

        fn _visitLinkedList(this: *@This(), start: NodeRef, comptime open: []const u8, comptime close: []const u8, comptime sep: []const u8, comptime multi_line: bool) !void {
            if (comptime open.len > 0) {
                this.print(open);
            }

            if (comptime multi_line) {
                this.needs_newline = start != 0;
                this.indent += 2;
            }

            var ref = start;
            while (ref != 0) {
                const n = this.getNode(ref);
                ref = n.next;

                if (this.skip_types and n.hasFlag(.declare)) continue;

                try this.visit(n);

                if (comptime sep.len > 0) {
                    if (ref != 0) {
                        this.print(sep);
                    }
                }

                if (comptime multi_line) {
                    this.needs_newline = true;
                }
            }

            if (comptime multi_line) {
                this.indent -= 2;
            }

            if (comptime close.len > 0) {
                this.print(close);
            }
        }

        inline fn printParams(this: *@This(), params: NodeRef) !void {
            return this._visitLinkedList(params, "(", ")", ", ", false);
        }

        inline fn printTypeParams(this: *@This(), args: NodeRef) !void {
            if (this.skip_types) return;
            if (args == 0) return;
            return this._visitLinkedList(args, "<", ">", ", ", false);
        }

        inline fn printTypeArgs(this: *@This(), args: NodeRef) !void {
            if (this.skip_types) return;
            if (args == 0) return;
            return this._visitLinkedList(args, "<", ">", ", ", false);
        }

        inline fn printModifiers(this: *@This(), n: *const AstNode) void {
            if (this.skip_types) {
                if (hasFlag(n, .static)) {
                    this.print("static ");
                }
                return;
            }

            if (hasFlag(n, .public)) {
                this.print("public ");
            } else if (hasFlag(n, .protected)) {
                this.print("protected ");
            } else if (hasFlag(n, .private)) {
                this.print("private ");
            }

            if (hasFlag(n, .static)) {
                this.print("static ");
            }

            // Does not apply to methods
            if (hasFlag(n, .readonly)) {
                this.print("readonly ");
            }
        }

        fn maybePrint(this: *@This(), comptime prefix: []const u8, ref: NodeRef) !void {
            if (ref != 0) {
                this.print(prefix);
                try this.visitRef(ref);
            }
        }

        inline fn maybePrintType(this: *@This(), ref: NodeRef) !void {
            if (this.skip_types) return;
            return this.maybePrint(": ", ref);
        }

        inline fn maybePrintInit(this: *@This(), ref: NodeRef) !void {
            return this.maybePrint(" = ", ref);
        }

        inline fn maybePrintInitNoSpace(this: *@This(), ref: NodeRef) !void {
            return this.maybePrint("=", ref);
        }

        inline fn printCallExp(this: *@This(), n: *const AstNode) !void {
            const d = getPackedData(n);
            try this.visitRef(d.left);
            if (hasFlag(n, .optional)) {
                this.print("?.");
            }

            if (!this.skip_types) try this.printTypeArgs(n.len);

            try this._visitLinkedList(d.right, "(", ")", ", ", false);
        }

        inline fn printUnaryPrefixExp(this: *@This(), comptime operator: []const u8, n: *const AstNode) !void {
            this.print(operator);
            try this.visitRef(unwrapRef(n));
        }

        inline fn printTypeParameter(this: *@This(), n: *const AstNode, comptime constraint_prefix: []const u8) !void {
            if (hasTypeParamFlag(n, .@"const")) {
                this.print("const ");
            }

            if (hasTypeParamFlag(n, .variance_in)) {
                this.print("in ");
            }

            if (hasTypeParamFlag(n, .variance_out)) {
                this.print("out ");
            }

            const d = getPackedData(n);
            try this.visitRef(d.left);
            if (d.right != 0) {
                this.print(constraint_prefix);
                try this.visitRef(d.right);
            }
            try this.maybePrintInit(n.len);
        }

        fn printPackedBinaryExp(this: *@This(), n: *const AstNode, comptime operator: []const u8) !void {
            const d = getPackedData(n);
            try this.visitRef(d.left);
            this.print(operator);
            try this.visitRef(d.right);
        }

        inline fn printFunctionType(this: *@This(), n: *const AstNode) !void {
            const d = getPackedData(n);
            try this.printTypeParams(n.len);
            try this.printParams(d.left);
            this.print(" => ");
            try this.visitRef(d.right);
        }

        fn printEscapedString(this: *@This(), s: []const u8, comptime quote_char: u8) void {
            var i: usize = 0;
            var j: usize = 0;
            while (i < s.len) : (i += 1) {
                switch (s[i]) {
                    '\\' => {
                        i += 1;
                        // TODO: skip over unicode escape
                    },
                    '\n', '\r', '\t' => {
                        this.print(s[j..i]);
                        this.print("\\n");
                        j = i + 1;
                    },
                    quote_char => {
                        this.print(s[j..i]);
                        this.print(&.{ '\\', quote_char });
                        j = i + 1;
                    },
                    else => {},
                }
            }

            if (j < i) {
                this.print(s[j..i]);
            }
        }

        // TODO: re-use w/ JSXText
        fn printString(this: *@This(), n: *const AstNode) !void {
            const quote_char = if (hasStringFlag(n, .double_quote)) "\"" else "'";
            this.print(quote_char);

            if (hasStringFlag(n, .synthetic)) {
                if (hasStringFlag(n, .double_quote)) {
                    this.printEscapedString(getSlice(n, u8), '"');
                } else {
                    this.printEscapedString(getSlice(n, u8), '\'');
                }
            } else {
                this.print(getSlice(n, u8));
            }

            this.print(quote_char);
        }

        inline fn printFnModifiers(this: *@This(), n: *const AstNode) !void {
            if (hasFlag(n, .@"export")) {
                this.print("export ");

                if (hasFlag(n, .@"const")) {
                    this.print("default ");
                }
            }

            if (hasFlag(n, .declare)) {
                this.print("declare ");
            }

            if (hasFlag(n, .@"async")) {
                this.print("async ");
            }

            this.print("function");

            if (hasFlag(n, .generator)) {
                this.print("*");
            }
        }

        fn visitUsingOrDeferScope(this: *@This(), ref: NodeRef) !void {
            var t = this.getTransformer();
            const is_async = this.needsAsyncUsingScope(ref);

            this.addHelper(.call_dispose);
            this.addHelper(.known_symbol);

            try this.visit(&(try t.stackStatement()));
            this.needs_newline = true;

            const old_scope = this.scope;
            this.scope = if (is_async) .async_using else .using;
            defer this.scope = old_scope;

            this.print("try {");
            this.needs_newline = true;

            {
                this.indent += 2;
                defer this.indent -= 2;

                var ref2 = ref;
                while (ref2 != 0) {
                    const n = this.getNode(ref2);
                    ref2 = n.next;

                    if (this.skip_types and hasFlag(n, .declare)) continue;

                    if (comptime enable_conditional_bindings) {
                        if (isBoundControlFlow(&this.data.nodes, n)) {
                            const r = try this.getTransformer().transformIfOrWhile(n);
                            try this.visitRef(r);
                            ref2 = this.data.nodes.at(r).next;
                            this.needs_newline = true;
                            continue;
                        }
                    }

                    try this.visit(n);

                    this.needs_newline = true;
                }
            }

            this.print("}");

            // try this._visitLinkedList(ref, "try {", "}", "", true);
            this.print(" catch");
            try this.visit(&(try t.usingCatchClause()));
            this.print(" finally ");
            try this.visit(&(try t.usingScopeFinally(is_async)));
            this.needs_newline = true;
        }

        fn needsAsyncUsingScope(this: *@This(), ref: NodeRef) bool {
            var iter = this.toIterator(ref);
            while (iter.next()) |n| {
                switch (n.kind) {
                    .variable_statement => {
                        if (hasFlag(n, .await_using)) {
                            return true;
                        }
                    },
                    .defer_statement => {
                        const x = this.getTransformer().maybeTransformAsyncDefer(unwrapRef(n)) catch continue; // XXX: not optimal

                        if (x[0]) return true;
                    },
                    else => {},
                }
            }

            return false;
        }

        inline fn printMappingSegment(this: *@This(), n: *const AstNode) !void {
            if (!comptime print_source_map) return;
            if (n.location == 0) return;

            // Need to do this for accurate source maps
            if (this.needs_newline) {
                this.printLine();
                this.needs_newline = false;
            }

            const decoded = decodeLocation(n.location);
            try this.source_map.encodeSegment(this.sink.pos(), decoded.line, decoded.col);
        }

        pub fn visit(this: *@This(), n: *const AstNode) anyerror!void {
            @setEvalBranchQuota(10_000);

            switch (n.kind) {
                .identifier, .private_identifier => {
                    try this.printMappingSegment(n);

                    if (comptime use_symbol_replacements) {
                        const symbol_id = n.extra_data;
                        if (symbol_id != 0) {
                            if (this.symbol_replacements.get(symbol_id)) |t| {
                                return try this.visitRef(t);
                            }
                        }
                    }

                    this.print(getSlice(n, u8));
                },
                .object_keyword, .const_keyword, .undefined_keyword, .symbol_keyword, .void_keyword, .never_keyword, .unknown_keyword, .any_keyword, .for_keyword, .new_keyword, .null_keyword, .import_keyword, .default_keyword, .number_keyword, .string_keyword, .boolean_keyword, .false_keyword, .true_keyword, .super_keyword, .this_keyword, .debugger_keyword => {
                    this.print(syntaxKindToString(n.kind));
                },
                .indexed_access_type => {
                    const d = getPackedData(n);
                    try this.visitRef(d.left);
                    this.print("[");
                    try this.visitRef(d.right);
                    this.print("]");
                },
                .regular_expression_literal => {
                    this.print(getSlice(n, u8));
                },
                .no_substitution_template_literal => {
                    const s = getSlice(n, u8);
                    this.print("`");
                    this.print(s);
                    this.print("`");
                },
                .string_literal => {
                    try this.printString(n);
                },
                .template_head => {
                    this.print("`");
                    this.print(getSlice(n, u8));
                    this.print("${");
                },
                .template_middle => {
                    this.print("}");
                    this.print(getSlice(n, u8));
                    this.print("${");
                },
                .template_tail => {
                    this.print("}");
                    this.print(getSlice(n, u8));
                    this.print("`");
                },
                // Why does typescript distinguish these two? No clue.
                .template_span, .template_literal_type_span => {
                    const d = getPackedData(n);
                    try this.visitRef(d.left);
                    try this.visitRef(d.right);
                },
                .template_expression, .template_literal_type => {
                    var ref = unwrapRef(n);
                    while (ref != 0) {
                        const span = this.getNode(ref);
                        try this.visit(span);
                        ref = span.next;
                    }
                },
                .tagged_template_expression => {
                    const d = getPackedData(n);
                    try this.visitRef(d.left);
                    try this.visitRef(d.right);
                },
                .jsx_expression => {
                    this.print("{");
                    try this.visitRef(unwrapRef(n));
                    this.print("}");
                },
                .jsx_spread_attribute => {
                    this.print("{...");
                    try this.visitRef(unwrapRef(n));
                    this.print("}");
                },
                .jsx_attribute => {
                    const d = getPackedData(n);
                    try this.visitRef(d.left);
                    try this.maybePrintInitNoSpace(d.right);
                },
                .jsx_attributes => {
                    try this._visitLinkedList(unwrapRef(n), "", "", " ", false);
                },
                .jsx_text => {
                    this.print(getSlice(n, u8));
                },
                .jsx_element => {
                    // first/last refs are opening/closing elements, middle are children
                    try this._visitLinkedList(maybeUnwrapRef(n) orelse 0, "", "", "", false);
                },
                .jsx_opening_element => {
                    const d = getPackedData(n);
                    this.print("<");
                    try this.visitRef(d.left);
                    try this.printTypeArgs(n.len);
                    try this.maybePrint(" ", d.right);
                    this.print(">");
                },
                .jsx_self_closing_element => {
                    const d = getPackedData(n);
                    this.print("<");
                    try this.visitRef(d.left);
                    try this.printTypeArgs(n.len);
                    try this.maybePrint(" ", d.right);
                    this.print("/>");
                },
                .jsx_closing_element => {
                    this.print("</");
                    try this.visitRef(unwrapRef(n));
                    this.print(">");
                },
                .jsx_fragment => {
                    try this._visitLinkedList(maybeUnwrapRef(n) orelse 0, "<>", "</>", "", false);
                    this.print("</>");
                },
                .numeric_literal => {
                    if (n.data) |d| {
                        const val: u64 = @intFromPtr(d);
                        const f: f64 = @bitCast(val);
                        var buf: [64]u8 = undefined;
                        this.print(try std.fmt.bufPrint(&buf, "{d}", .{f}));
                    } else {
                        this.print("0");
                    }
                },
                .qualified_name => {
                    const d = getPackedData(n);
                    try this.visitRef(d.left);
                    this.print(".");
                    try this.visitRef(d.right);
                },
                .property_access_expression => {
                    const d = getPackedData(n);
                    try this.visitRef(d.left);
                    if (hasFlag(n, .optional)) {
                        this.print("?.");
                    } else {
                        this.print(".");
                    }
                    try this.visitRef(d.right);
                },
                .element_access_expression => {
                    const d = getPackedData(n);
                    try this.visitRef(d.left);
                    if (hasFlag(n, .optional)) {
                        this.print("?.");
                    }
                    this.print("[");
                    try this.visitRef(d.right);
                    this.print("]");
                },
                .for_statement => {
                    const d = getPackedData(n);
                    this.print("for (");
                    if (d.left != 0) {
                        try this.visitRef(d.left);
                    }
                    this.print("; ");
                    if (d.right != 0) {
                        try this.visitRefUnwrapStatement(d.right);
                    }
                    this.print("; ");
                    if (n.len != 0) {
                        try this.visitRefUnwrapStatement(n.len);
                    }
                    this.print(") ");
                    try this.visitRef(n.extra_data);
                },
                .for_of_statement => {
                    const d = getPackedData(n);
                    if (n.hasFlag(.@"async")) {
                        this.print("for await (");
                    } else {
                        this.print("for (");
                    }
                    try this.visitRef(d.left);
                    this.print(" of ");
                    try this.visitRef(d.right);
                    this.print(") ");
                    try this.visitRef(n.len);
                },
                .for_in_statement => {
                    const d = getPackedData(n);
                    this.print("for (");
                    try this.visitRef(d.left);
                    this.print(" in ");
                    try this.visitRef(d.right);
                    this.print(") ");
                    try this.visitRef(n.len);
                },
                .case_clause => {
                    this.print("case ");
                    try this.visitRef(unwrapRef(n));
                    this.print(": ");
                    if (n.len != 0 and this.data.nodes.at(n.len).kind == .block) {
                        try this.visitRef(n.len); // print block on the same line
                    } else {
                        try this._visitLinkedList(n.len, "", "", "", true);
                    }
                },
                .default_clause => {
                    this.print("default: ");
                    try this._visitLinkedList(n.len, "", "", "", true);
                },
                .catch_clause => {
                    const d = getPackedData(n);
                    if (d.left != 0) {
                        this.print("(");
                        try this.visitRef(d.left);
                        this.print(")");
                    }

                    this.print(" ");
                    try this.visitRef(d.right);
                },
                .try_statement => {
                    const d = getPackedData(n);
                    this.print("try ");
                    try this.visitRef(d.left);
                    try this.maybePrint(" catch", d.right);
                    try this.maybePrint(" finally ", n.len);
                },
                .if_statement => {
                    const d = getPackedData(n);
                    this.print("if (");
                    try this.visitRef(d.left);
                    this.print(") ");
                    try this.visitRef(d.right);
                    // for ASI
                    this.needs_newline = true;
                    try this.maybePrint(" else ", n.len);
                },
                .switch_statement => {
                    const d = getPackedData(n);
                    this.print("switch (");
                    try this.visitRef(d.left);
                    this.print(")");
                    try this._visitLinkedList(d.right, " {", "}", "", true);
                },
                .break_statement => {
                    if (n.data) |p| {
                        this.print("break ");
                        try this.visitRef(@intFromPtr(p));
                    } else {
                        this.print("break");
                    }
                },
                .continue_statement => {
                    if (n.data) |p| {
                        this.print("continue ");
                        try this.visitRef(@intFromPtr(p));
                    } else {
                        this.print("continue");
                    }
                },
                .empty_statement => {},
                .while_statement => {
                    const d = getPackedData(n);
                    this.print("while (");
                    try this.visitRef(d.left);
                    this.print(") ");
                    try this.visitRef(d.right);
                },
                .do_statement => {
                    const d = getPackedData(n);
                    this.print("do ");
                    try this.visitRef(d.left);

                    this.print(" while (");
                    try this.visitRef(d.right);
                    this.print(")");
                },
                .prefix_unary_expression => {
                    const d = getPackedData(n);
                    this.print(syntaxKindToString(@enumFromInt(d.left)));
                    try this.visitRef(d.right);
                },
                .postfix_unary_expression => {
                    const d = getPackedData(n);
                    try this.visitRef(d.left);
                    this.print(syntaxKindToString(@enumFromInt(d.right)));
                },
                .as_expression => {
                    if (this.skip_types) {
                        const d = getPackedData(n);
                        return this.visitRef(d.left);
                    }
                    try this.printPackedBinaryExp(n, " as ");
                },
                .satisfies_expression => {
                    if (this.skip_types) {
                        const d = getPackedData(n);
                        return this.visitRef(d.left);
                    }
                    try this.printPackedBinaryExp(n, " satisfies ");
                },
                .binary_expression => {
                    const d = getPackedData(n);

                    try this.visitRef(d.left);
                    this.print(" ");
                    this.print(syntaxKindToString(@enumFromInt(n.len)));
                    this.print(" ");
                    try this.visitRef(d.right);
                },
                .expression_statement => {
                    try this.visitRef(unwrapRef(n));
                    this.print(";"); // TODO: only emit semicolon when it's needed (ASI)
                },
                .type_query, .typeof_expression => {
                    this.print("typeof ");
                    try this.visitRef(unwrapRef(n));

                    // Only relevant for `type_query`
                    if (n.len != 0) try this.printTypeArgs(n.len);
                },
                .reify_expression => {
                    // this.print("reify ");
                    const call_exp = try this.getTransformer().transformReifyExpression(unwrapRef(n), n.len);
                    try this.visit(&call_exp);
                },
                .void_expression => {
                    this.print("void ");
                    try this.visitRef(unwrapRef(n));
                },
                .delete_expression => {
                    this.print("delete ");
                    try this.visitRef(unwrapRef(n));
                },
                .await_expression => {
                    this.print("await ");
                    try this.visitRef(unwrapRef(n));
                },
                .return_statement => {
                    this.print("return ");
                    if (n.data != null) try this.visitRef(unwrapRef(n));
                },
                .yield_expression => {
                    if (hasFlag(n, .generator)) {
                        this.print("yield* ");
                    } else {
                        this.print("yield ");
                    }
                    if (n.data != null) try this.visitRef(unwrapRef(n));
                },
                .parenthesized_expression => {
                    if (this.needs_newline) {
                        this.print(";");
                    }
                    this.print("(");
                    try this.visitRef(unwrapRef(n));
                    this.print(")");
                },
                .parenthesized_type => {
                    this.print("(");
                    try this.visitRef(unwrapRef(n));
                    this.print(")");
                },
                .not_emitted_statement => {},
                .omitted_expression => {}, // appears in expressions like `[,]`
                .array_literal_expression => { 
                    this.print("[");

                    var ref = maybeUnwrapRef(n) orelse 0;
                    while (ref != 0) {
                        const el = this.getNode(ref);
                        ref = el.next;

                        try this.visit(el);

                        if (ref != 0) {
                            this.print(", ");
                        } else if (el.kind == .omitted_expression) {
                            this.print(",");
                        }
                    }

                    this.print("]");
                },
                .property_assignment => {
                    const d = getPackedData(n);
                    try this.visitRef(d.left);
                    this.print(": ");
                    try this.visitRef(d.right);
                },
                .object_literal_expression => {
                    if (n.data) |p| {
                        try this._visitLinkedList(@intCast(@intFromPtr(p)), "{", "}", ",", true);
                    } else {
                        this.print("{}");
                    }
                },
                .arrow_function => {
                    if (hasFlag(n, .@"async")) {
                        this.print("async ");
                    }

                    const d = getPackedData(n);

                    // single param arrow fn
                    if (hasFlag(n, .let)) {
                        try this.visitRef(d.left);
                        try this.maybePrint(" => ", d.right);
                    } else {
                        try this.printTypeParams(n.extra_data);
                        try this.printParams(d.left);
                        try this.maybePrintType(n.len);
                        try this.maybePrint(" => ", d.right);
                    }
                },
                .array_type => {
                    try this.visitRef(unwrapRef(n));
                    this.print("[]");
                },
                .named_tuple_member => {
                    const d = getPackedData(n);
                    if (hasFlag(n, .generator)) { // spread
                        this.print("...");
                    }
                    try this.visitRef(d.left);
                    if (hasFlag(n, .optional)) {
                        this.print("?");
                    }
                    this.print(": ");
                    try this.visitRef(d.right);
                },
                .literal_type => {
                    try this.visitRef(unwrapRef(n));
                },
                .tuple_type => {
                    const first: NodeRef = maybeUnwrapRef(n) orelse 0;
                    try this._visitLinkedList(first, "[", "]", ", ", false);
                },
                .rest_type => {
                    this.print("...");
                    try this.visitRef(unwrapRef(n));
                },
                .optional_type => {
                    try this.visitRef(unwrapRef(n));
                    this.print("?");
                },
                .union_type => {
                    // Synthethic list
                    if (n.flags == 1) {
                        return this._visitLinkedList(unwrapRef(n), "", "", " | ", false);
                    }
                    const d = getPackedData(n);
                    try this.visitRef(d.left);
                    this.print(" | ");
                    try this.visitRef(d.right);
                },
                .intersection_type => {
                    // Synthethic list
                    if (n.flags == 1) {
                        return this._visitLinkedList(unwrapRef(n), "", "", " & ", false);
                    }
                    const d = getPackedData(n);
                    try this.visitRef(d.left);
                    this.print(" & ");
                    try this.visitRef(d.right);
                },
                .type_literal => {
                    if (maybeUnwrapRef(n)) |r| {
                        if (this.getNode(r).next != 0) {
                            try this._visitLinkedList(r, "{", "}", ", ", true);
                        } else {
                            try this._visitLinkedList(r, "{ ", " }", ", ", false);
                        }
                    } else {
                        this.print("{}");
                    }
                },
                .type_parameter => {
                    try this.printTypeParameter(n, " extends ");
                },
                .type_reference, .expression_with_type_arguments => {
                    const d = getPackedData(n);
                    try this.visitRef(d.left);
                    try this.printTypeArgs(d.right);
                },
                .import_type => {
                    const d = getPackedData(n);
                    this.print("import(");
                    try this.visitRef(d.left);
                    this.print(").");
                    try this.visitRef(d.right);
                    try this.printTypeArgs(n.len);
                },
                .import_equals_declaration => {
                    if (n.hasFlag(.@"export")) {
                        this.print("export ");
                    }

                    const d = getPackedData(n);
                    this.print("import ");
                    try this.visitRef(d.left);
                    this.print(" = ");
                    try this.visitRef(d.right);
                },
                .external_module_reference => {
                    this.print("require(");
                    try this.visitRef(unwrapRef(n));
                    this.print(")");
                },
                .constructor => {
                    const d = getPackedData(n);

                    if (!this.skip_types) {
                        this.printModifiers(n);
                    } else if (d.right == 0) {
                        return;
                    }

                    this.print("constructor");
                    try this.printParams(d.left);

                    if (!this.skip_types) {
                        return this.maybePrint(" ", d.right);
                    }

                    const body = try this.getTransformer().transformConstructor(d, this.needs_super);

                    return this.maybePrint(" ", body);
                },
                .class_static_block_declaration => {
                    this.print("static ");
                    try this._visitLinkedList(maybeUnwrapRef(n) orelse 0, "{", "}", "", true);
                },
                .method_signature => {
                    // this.printModifiers(n);

                    const d = getPackedData(n);
                    try this.visitRef(d.left);
                    if (n.hasFlag(.optional)) {
                        this.print("?");
                    }
                    try this.printTypeParams(n.extra_data);
                    try this.printParams(d.right);
                    try this.maybePrintType(n.len);
                },
                .method_declaration => {
                    // Probably an overload
                    if (this.skip_types and n.len == 0) return;

                    this.printModifiers(n);

                    if (hasFlag(n, .override)) {
                        this.print("override ");
                    }

                    if (hasFlag(n, .@"async")) {
                        this.print("async ");
                    }

                    if (hasFlag(n, .generator)) {
                        this.print("*");
                    }

                    const d = getPackedData(n);
                    try this.visitRef(d.left);
                    if (this.skip_types) {
                        try this.printParams(d.right);
                    } else {
                        try this.printTypeParams(n.extra_data);
                        try this.printParams(d.right);
                        try this.maybePrintType(n.extra_data2);
                    }

                    if (n.len != 0) {
                        this.print(" ");
                        try this.visitRef(n.len);
                    }
                },
                .property_signature => {
                    this.printModifiers(n);

                    const d = getPackedData(n);
                    try this.visitRef(d.left);
                    if (hasFlag(n, .optional)) this.print("?");
                    try this.maybePrintType(d.right);
                },
                .property_declaration => {
                    this.printModifiers(n);

                    const d = getPackedData(n);
                    try this.visitRef(d.left);
                    if (!this.skip_types and hasFlag(n, .optional) and n.len != 0) this.print("?");
                    try this.maybePrintType(n.len);
                    try this.maybePrintInit(d.right);
                    this.print(";");
                },
                .get_accessor => {
                    this.printModifiers(n);

                    const d = getPackedData(n);
                    this.print("get ");
                    try this.visitRef(d.left);
                    try this.printParams(d.right);
                    try this.maybePrintType(n.extra_data);
                    try this.maybePrint(" ", n.len);
                },
                .set_accessor => {
                    this.printModifiers(n);

                    const d = getPackedData(n);
                    this.print("set ");
                    try this.visitRef(d.left);
                    try this.printParams(d.right);
                    try this.maybePrintType(n.extra_data);
                    try this.maybePrint(" ", n.len);
                },
                .parameter => {
                    const d = getPackedData(n);
                    if (n.hasFlag(.generator)) {
                        this.print("...");
                    }

                    try this.visitRef(d.left);
                    if (!this.skip_types and n.hasFlag(.optional)) {
                        this.print("?");
                    }
                    try this.maybePrintType(n.len);
                    try this.maybePrintInit(d.right);
                },
                .decorator => {
                    this.print("@");
                    try this.visitRef(unwrapRef(n));
                },
                .semicolon_class_element => {
                    this.print(";");
                },
                .class_expression, .class_declaration => {
                    if (this.skip_types and hasFlag(n, .declare)) return;

                    const d = getPackedData(n);

                    if (hasFlag(n, .@"export")) {
                        this.print("export ");

                        if (hasFlag(n, .@"const")) {
                            this.print("default ");
                        }
                    }

                    if (hasFlag(n, .declare)) {
                        this.print("declare ");
                    }

                    this.print("class");

                    if (d.left != 0) {
                        this.print(" ");
                        try this.visitRef(d.left);
                    }

                    try this.printTypeParams(n.extra_data);

                    const has_extends = n.len != 0;
                    if (has_extends) {
                        this.print(" extends ");
                        try this.visitRef(n.len);
                    }

                    if (!this.skip_types and n.extra_data2 != 0) {
                        this.print(" implements ");
                        try this._visitLinkedList(n.extra_data2, "", "", ", ", false);
                    }

                    if (d.right == 0) {
                        return this.print(" {}");
                    }

                    this.print(" {");
                    this.indent += 2;

                    const old_needs_super = this.needs_super;
                    this.needs_super = has_extends;
                    defer this.needs_super = old_needs_super;

                    var ref = d.right;
                    while (ref != 0) {
                        this.needs_newline = true;
                        if (this.data.decorators.get(ref)) |start| {
                            this.indent -= 2;
                            try this._visitLinkedList(start, "", "", "", true);
                            this.indent += 2;
                        }

                        const n2 = this.data.nodes.at(ref);
                        try this.visit(n2);
                        ref = n2.next;
                        this.needs_newline = true;
                    }

                    this.indent -= 2;
                    this.print("}");
                },
                .export_specifier, .import_specifier => {
                    if (hasFlag(n, .declare)) {
                        this.print("type ");
                    }

                    const d = getPackedData(n);
                    try this.visitRef(d.left);
                    if (d.right != 0) {
                        this.print(" as ");
                        try this.visitRef(d.right);
                    }
                },
                .named_exports, .named_imports => {
                    if (n.data) |p| {
                        try this._visitLinkedList(@intCast(@intFromPtr(p)), "{ ", " }", ", ", false);
                    } else {
                        this.print("{ }");
                    }
                },
                .namespace_import => {
                    this.print("* as ");
                    try this.visitRef(unwrapRef(n));
                },
                .import_clause => {
                    const d = getPackedData(n);

                    if (d.left != 0) {
                        try this.visitRef(d.left);

                        if (d.right != 0) {
                            this.print(", ");
                            try this.visitRef(d.right);
                        }
                    } else {
                        try this.visitRef(d.right);
                    }
                },
                .import_attributes => {
                    if (n.data) |p| {
                        try this._visitLinkedList(@intCast(@intFromPtr(p)), "{ ", " }", ", ", false);
                    } else {
                        this.print("{}");
                    }
                },
                .import_declaration => {
                    if (this.skip_types and hasFlag(n, .declare)) return;

                    const d = getPackedData(n);
                    this.print("import ");

                    if (hasFlag(n, .declare)) {
                        this.print("type ");
                    }

                    if (d.left != 0) {
                        try this.visitRef(d.left);
                        this.print(" from ");
                    }

                    try this.visitRef(d.right);
                    try this.maybePrint(" with ", n.len);
                },
                .namespace_export => {
                    this.print("* as ");
                    try this.visitRef(unwrapRef(n));
                },
                .export_assignment => {
                    if (n.len == 1) {
                        this.print("export = ");
                        return this.visitRef(unwrapRef(n));
                    }

                    this.print("export default ");
                    try this.visitRef(unwrapRef(n));
                },
                .export_declaration => { // Structure is slightly different from `import_declaration`
                    if (this.skip_types and hasFlag(n, .declare)) return;

                    const d = getPackedData(n);

                    this.print("export ");

                    if (hasFlag(n, .declare)) {
                        this.print("type ");
                    }

                    if (d.left != 0) {
                        try this.visitRef(d.left);
                    } else {
                        this.print("*");
                    }

                    try this.maybePrint(" from ", d.right);
                    try this.maybePrint(" with ", n.len);
                },
                .function_expression, .function_declaration => {
                    if (this.skip_types and hasFlag(n, .declare)) return;
                    if (this.skip_types and n.len == 0) return;

                    const d = getPackedData(n);

                    if (hasFlag(n, .@"export")) {
                        this.print("export ");

                        if (hasFlag(n, .@"const")) {
                            this.print("default ");
                        }
                    }

                    if (hasFlag(n, .declare)) {
                        this.print("declare ");
                    }

                    if (hasFlag(n, .@"async")) {
                        this.print("async ");
                    }

                    this.print("function");

                    if (hasFlag(n, .generator)) {
                        this.print("*");
                    }

                    try this.maybePrint(" ", d.left);

                    if (this.skip_types) {
                        if (d.right != 0 and this.getNode(d.right).hasFlag(.declare)) {
                            try this.printParams(this.getNode(d.right).next);
                        } else {
                            try this.printParams(d.right);
                        }
                    } else {
                        try this.printTypeParams(n.extra_data);
                        try this.printParams(d.right);
                        try this.maybePrintType(n.extra_data2);
                    }

                    if (n.len == 0) {
                        this.print(";");
                    } else {
                        this.print(" ");
                        try this.visitRef(n.len);
                    }
                },
                .shorthand_property_assignment => {
                    if (comptime use_symbol_replacements) {
                        const n2 = try this.getNode(unwrapRef(n));
                        const symbol_id = n2.extra_data;
                        if (symbol_id != 0) {
                            if (this.symbol_replacements.get(symbol_id)) |t| {
                                this.print(getSlice(n2, u8));
                                this.print(": ");
                                return try this.visitRef(t);
                            }
                        }
                    }
                    try this.visitRef(unwrapRef(n));
                },
                .spread_assignment, .spread_element => {
                    this.print("...");
                    try this.visitRef(unwrapRef(n));
                },
                .meta_property => {
                    const d = getPackedData(n);
                    this.print(syntaxKindToString(@enumFromInt(d.left)));
                    this.print(".");
                    try this.visitRef(d.right);
                },
                .throw_statement => {
                    this.print("throw ");
                    try this.visitRef(unwrapRef(n));
                },
                .new_expression => {
                    try this.printMappingSegment(n);

                    this.print("new ");

                    const d = getPackedData(n);
                    try this.visitRef(d.left);
                    try this.printTypeArgs(n.len);
                    try this._visitLinkedList(d.right, "(", ")", ", ", false);
                },
                .call_expression => {
                    try this.printCallExp(n);
                },
                .type_predicate => {
                    if (n.len == 1) {
                        this.print("asserts ");
                    }
                    const d = getPackedData(n);
                    try this.visitRef(d.left);
                    if (d.right != 0) {
                        this.print(" is ");
                        try this.visitRef(d.right);
                    } else {
                        std.debug.assert(n.len == 1);
                    }
                },
                .infer_type => {
                    this.print(syntaxKindToString(.infer_keyword));
                    this.print(" ");
                    try this.visitRef(unwrapRef(n));
                },
                .type_operator => {
                    const d = getPackedData(n);
                    this.print(syntaxKindToString(@enumFromInt(d.left)));
                    this.print(" ");
                    try this.visitRef(d.right);
                },
                .conditional_expression => {
                    const d = getPackedData(n);
                    try this.visitRef(d.left);
                    // this.indent += 2;
                    // defer this.indent -= 2;
                    // this.needs_newline = true;
                    this.print(" ? ");
                    try this.visitRef(d.right);
                    // this.needs_newline = true;
                    this.print(" : ");
                    try this.visitRef(n.len);
                },
                .defer_statement => {
                    this.addHelper(.@"defer");

                    const inner = unwrapRef(n);
                    const x = try this.getTransformer().maybeTransformAsyncDefer(inner);

                    if (x[0]) {
                        this.print("__defer(_stack, async () => ");
                    } else {
                        this.print("__defer(_stack, () => ");
                    }

                    try this.visitRef(x[1]);

                    if (x[0]) {
                        this.print(", true)");
                    } else {
                        this.print(")");
                    }
                },
                .block => {
                    const p = n.data orelse return this.print("{}");

                    const old_scope = this.scope;
                    this.scope = .none;
                    defer this.scope = old_scope;

                    this.print("{");
                    defer this.print("}");
                    this.indent += 2;
                    defer this.indent -= 2;
                    this.needs_newline = true;

                    var ref: NodeRef = @intCast(@intFromPtr(p));
                    while (ref != 0) {
                        const u = this.getNode(ref);
                        const needs_using_scope = u.kind == .defer_statement or (u.kind == .variable_statement and hasFlag(u, .using));
                        if (needs_using_scope) {
                            if (comptime enable_conditional_bindings) {
                                const has_binding = u.kind == .defer_statement and isBoundControlFlow(&this.data.nodes, this.getNode(unwrapRef(u)));
                                if (has_binding) {
                                    const n3 = try this.data.nodes.push(.{
                                        .kind = .defer_statement,
                                        .data = @ptrFromInt(try this.getTransformer().transformIfOrWhile(this.getNode(unwrapRef(u)))),
                                        .next = u.next,
                                    });
                                    return this.visitUsingOrDeferScope(n3);
                                }
                            }
                            return this.visitUsingOrDeferScope(ref);
                        }

                        if (comptime enable_conditional_bindings) {
                            if (u.kind == .if_statement and this.getNode(getPackedData(u).left).kind == .variable_statement) {
                                const transformed = try this.getTransformer().transformIfOrWhile(u);
                                try this.visitRef(transformed);
                                ref = this.getNode(transformed).next;
                                this.needs_newline = true;
                                continue;
                            }
                        }

                        try this.visit(u);
                        ref = u.next;
                        this.needs_newline = true;
                    }
                },
                .mapped_type => {
                    const d = getPackedData(n);
                    this.print("{ ");

                    if (hasFlag(n, .minus_readonly)) {
                        this.print("-readonly ");
                    } else if (hasFlag(n, .readonly)) {
                        this.print("readonly ");
                    }

                    this.print("[");

                    try this.printTypeParameter(this.getNode(d.left), " in ");

                    if (n.len != 0) {
                        this.print(" as ");
                        try this.visitRef(n.len);
                    }

                    if (hasFlag(n, .minus_optional)) {
                        this.print("]-?: ");
                    } else if (hasFlag(n, .optional)) {
                        this.print("]?: ");
                    } else {
                        this.print("]: ");
                    }

                    try this.visitRef(d.right);
                    this.print(" }");
                },
                .index_signature => {
                    const d = getPackedData(n);
                    this.print("[");
                    if (d.left == 0) {
                        this.print("x"); // Mimics typescript
                    } else {
                        try this.visitRef(d.left);
                    }
                    this.print(": ");
                    try this.visitRef(d.right);
                    this.print("]: ");
                    try this.visitRef(n.len);
                },
                .construct_signature => {
                    this.print("new ");
                    try this.printSignature(n);
                },
                .call_signature => {
                    try this.printSignature(n);
                },
                .function_type => {
                    const d = getPackedData(n);
                    try this.printTypeParams(n.len);
                    try this.printParams(d.left);
                    this.print(" => ");
                    try this.visitRef(d.right);
                },
                .constructor_type => {
                    if (n.hasFlag(.abstract)) {
                        this.print("abstract ");
                    }
                    this.print("new ");
                    try this.printFunctionType(n);
                },
                .conditional_type => {
                    const d = getPackedData(n);
                    try this.visitRef(d.left);
                    this.print(" extends ");
                    try this.visitRef(d.right);
                    this.print(" ? ");
                    try this.visitRef(n.len);
                    this.print(" : ");
                    try this.visitRef(n.extra_data);
                },
                .enum_member => {
                    const d = getPackedData(n);
                    try this.visitRef(d.left);
                    try this.maybePrintInit(d.right);
                },
                .module_declaration => {
                    if (this.skip_types) return;

                    const d = getPackedData(n);

                    if (hasFlag(n, .@"export")) {
                        this.print("export ");
                    }

                    if (hasFlag(n, .declare)) {
                        this.print("declare ");
                    }

                    if (hasFlag(n, .let)) {
                        this.print("namespace ");
                    } else {
                        this.print("module ");
                    }

                    if (d.left == 0) {
                        this.print("global");
                    } else {
                        try this.visitRef(d.left);
                    }

                    if (d.right == 0) return this.print(";");

                    this.print(" ");
                    try this.visitRef(d.right);
                },
                .enum_declaration => {
                    if (this.skip_types) {
                        if (hasFlag(n, .@"const")) {
                            return;
                        }

                        if (hasFlag(n, .@"export")) {
                            this.print("export ");
                        }

                        this.print("const ");

                        const d = getPackedData(n);
                        const ident = this.getNode(@intCast(d.left));

                        try this.visit(ident);

                        const ident_slice = getSlice(ident, u8);

                        this.print(" = {}");
                        this.needs_newline = true;

                        var v: f64 = 0;
                        var ref = d.right;
                        while (ref != 0) {
                            const n2 = this.getNode(ref);
                            ref = n2.next;

                            const d2 = getPackedData(n2);

                            // Foo[Foo["Bar"] = 1] = "Bar"

                            this.print(ident_slice);
                            this.print("[");
                            this.print(ident_slice);
                            this.print("[\"");
                            try this.visitRef(d2.left);
                            this.print("\"] = ");
                            if (d2.right != 0) {
                                // TODO: gotta eval `d2.right`
                                try this.printNumber(v);
                            } else {
                                try this.printNumber(v);
                                v += 1;
                            }
                            this.print("] = \"");
                            try this.visitRef(d2.left);
                            this.print("\"");
                            this.needs_newline = true;
                        }
                        return;
                    }

                    if (hasFlag(n, .@"export")) {
                        this.print("export ");
                    }

                    if (hasFlag(n, .declare)) {
                        this.print("declare ");
                    }

                    if (hasFlag(n, .@"const")) {
                        this.print("const ");
                    }

                    this.print("enum ");

                    const d = getPackedData(n);
                    try this.visitRef(d.left);
                    try this._visitLinkedList(d.right, " {", "}", ",", true);
                },
                .interface_declaration => {
                    if (this.skip_types) return;

                    const d = getPackedData(n);

                    if (hasFlag(n, .@"export")) {
                        this.print("export ");
                    }

                    this.print("interface ");

                    try this.visitRef(d.left);

                    try this.printTypeParams(n.len);

                    if (n.extra_data != 0) {
                        this.print(" extends ");
                        try this._visitLinkedList(n.extra_data, "", "", ", ", false);
                    }

                    try this._visitLinkedList(d.right, " {", "}", "", true);
                },
                .type_alias_declaration => {
                    if (this.skip_types) return;

                    if (hasFlag(n, .@"export")) {
                        this.print("export ");
                    }

                    const d = getPackedData(n);
                    this.print("type ");
                    try this.visitRef(d.left);
                    try this.printTypeParams(d.right);

                    this.print(" = ");
                    try this.visitRef(n.len);
                    this.print(";");
                },
                .labeled_statement => {
                    const d = getPackedData(n);
                    try this.visitRef(d.left);
                    this.print(": ");
                    try this.visitRef(d.right);
                },
                .variable_statement, .variable_declaration_list => {
                    if (this.skip_types and hasFlag(n, .declare)) return;

                    if (hasFlag(n, .@"export")) {
                        this.print("export ");
                    }

                    if (hasFlag(n, .declare)) {
                        this.print("declare ");
                    }

                    if (hasFlag(n, .let)) {
                        this.print("let ");
                    } else if (hasFlag(n, .@"const")) {
                        this.print("const ");
                    } else if (hasFlag(n, .await_using)) {
                        this.print("await using ");
                    } else if (hasFlag(n, .using)) {
                        if (this.scope == .none) {
                            this.print("using ");
                        } else {
                            this.print("const ");
                            this.addHelper(.using);

                            var ref = unwrapRef(n);
                            while (ref != 0) {
                                const u = this.getNode(ref);
                                const d = getPackedData(u);
                                try this.visitRef(d.left);

                                const w = try this.getTransformer().wrapUsing(d.right);

                                this.print(" = ");
                                try this.visit(&w);

                                ref = u.next;
                            }

                            return;
                        }
                    } else {
                        this.print("var ");
                    }

                    try this._visitLinkedList(unwrapRef(n), "", "", ", ", false);
                },
                .variable_declaration => {
                    const d = getPackedData(n);
                    try this.visitRef(d.left);
                    try this.maybePrintType(n.len);
                    try this.maybePrintInit(d.right);
                },
                .computed_property_name => {
                    this.print("[");
                    try this.visitRef(unwrapRef(n));
                    this.print("]");
                },
                .binding_element => {
                    const d = getPackedData(n);

                    if (hasFlag(n, .generator)) {
                        this.print("...");
                    }

                    if (n.len != 0) {
                        try this.visitRef(n.len);
                        this.print(": ");
                    }

                    try this.visitRef(d.left);

                    if (d.right != 0) {
                        this.print(" = ");
                        try this.visitRef(d.right);
                    }
                },
                .object_binding_pattern => {
                    try this._visitLinkedList(maybeUnwrapRef(n) orelse 0, "{ ", " }", ", ", false);
                },
                .array_binding_pattern => {
                    try this._visitLinkedList(maybeUnwrapRef(n) orelse 0, "[", "]", ", ", false);
                },
                .source_file => {
                    if (comptime print_source_map) {
                        if (this.data.source_name) |source_name| {
                            try this.source_map.setSource(source_name);
                        }
                    }

                    if (!this.skip_types) {
                        for (this.data.triple_slash_directives) |d| {
                            this.print("/// <reference ");
                            const attr = switch (d.kind) {
                                .lib => "lib=\"",
                                .path => "path=\"",
                                .types => "types=\"",
                                else => return error.TODO_triple_slash,
                            };

                            this.print(attr);
                            this.print(d.value);
                            this.print("\" />");
                            this.needs_newline = true;
                        }
                    }

                    var ref = maybeUnwrapRef(n) orelse 0;
                    while (ref != 0) {
                        const n2 = this.getNode(ref);
                        if (this.skip_types and hasFlag(n2, .declare)) {
                            ref = n2.next;
                            continue;
                        }

                        const needs_using_scope = this.scope == .none and (n2.kind == .variable_statement and hasFlag(n2, .using) or n2.kind == .defer_statement);
                        if (needs_using_scope) {
                            defer this.printHelpers();
                            if (comptime enable_conditional_bindings) {
                                const has_binding = n2.kind == .defer_statement and isBoundControlFlow(&this.data.nodes, this.getNode(unwrapRef(n2)));
                                if (has_binding) {
                                    const n3 = try this.data.nodes.push(.{
                                        .kind = .defer_statement,
                                        .data = @ptrFromInt(try this.getTransformer().transformIfOrWhile(this.getNode(unwrapRef(n2)))),
                                        .next = n2.next,
                                    });
                                    return this.visitUsingOrDeferScope(n3);
                                }
                            }
                            return this.visitUsingOrDeferScope(ref);
                        }

                        if (comptime enable_conditional_bindings) {
                            if ((n2.kind == .if_statement or n2.kind == .while_statement) and this.getNode(getPackedData(n2).left).kind == .variable_statement) {
                                const transformed = try this.getTransformer().transformIfOrWhile(n2);
                                try this.visitRef(transformed);
                                ref = this.getNode(transformed).next;
                                this.needs_newline = true;
                                continue;
                            }
                        }

                        try this.visit(n2);
                        ref = n2.next;
                        this.needs_newline = true;
                    }

                    this.printHelpers();
                },
                .external_node => {
                    const data: *const AstData = @alignCast(@ptrCast(n.data orelse return error.NoData));
                    const start = n.len;

                    const old_data: AstData = this.data;
                    this.data = data.*;
                    defer this.data = old_data;

                    const old_transformer = this.transformer;
                    this.transformer = null;
                    defer this.transformer = old_transformer;

                    // TODO: we don't need this if the start node is a source file
                    if (comptime print_source_map) {
                        if (this.data.source_name) |source_name| {
                            try this.source_map.setSource(source_name);
                        }
                    }

                    defer if (comptime print_source_map) {
                        if (old_data.source_name) |source_name| {
                            this.source_map.setSource(source_name) catch unreachable;
                        }
                    };

                    const old_replacements = this.replacements;
                    if (comptime use_replacements) {
                        this.replacements = null;
                    }

                    defer if (comptime use_replacements) {
                        this.replacements = old_replacements;
                    };

                    try this.visitRef(start);
                },
                .verbatim_node => {
                    this.print(getSlice(n, u8));
                    if (n.extra_data == 0) return;

                    var lines = n.extra_data;
                    while (lines > 1) : (lines -= 1) {
                        this.printLine();
                    }
                    this.needs_newline = true;
                },
                .with_statement => {
                    const d = getPackedData(n);
                    this.print("with (");
                    try this.visitRef(d.left);
                    this.print(") ");
                    try this.visitRef(d.right);
                },
                else => {
                    if (@intFromEnum(n.kind) >= @intFromEnum(SyntaxKind.break_keyword) and @intFromEnum(n.kind) <= @intFromEnum(SyntaxKind.of_keyword)) {
                        return this.print(syntaxKindToString(n.kind));
                    }
                    if (comptime is_debug) {
                        std.debug.print("MISSING {any}\n", .{n.kind});
                    }
                    return error.MISSING;
                },
            }
        }
    };
}

pub const PrintResult = struct {
    contents: []const u8,
    source_map: ?[]const u8,
};

pub fn printWithOptions(data: AstData, options: PrinterOptions) !PrintResult {
    var writer = try Writer.init(data.source.len);
    var source_map: ?[]const u8 = null;

    if (options.transform_to_cjs) {
        var r = try printToCjs(data, options.replacements);

        return .{
            .contents = r.contents,
            .source_map = try r.source_map.toJson(getAllocator()),
        };
    }

    if (options.emit_source_map) {
        var printer = Printer(Writer, .{ .print_source_map = true }).init(data, &writer);
        printer.skip_types = options.skip_types;

        defer printer.source_map.deinit();

        try printer.visit(data.nodes.at(data.start));
        if (options.inline_source_map) {
            if (printer.needs_newline) {
                writer.write("\n");
            }
            writer.write(try printer.source_map.toInlineComment(getAllocator()));
        } else {
            source_map = try printer.source_map.toJson(getAllocator());
        }
    } else {
        var printer = Printer(Writer, .{}).init(data, &writer);
        printer.skip_types = options.skip_types;

        try printer.visit(data.nodes.at(data.start));
    }

    // TODO: check perf impact
    // writer.buf.shrinkAndFree(writer.buf.items.len);

    return .{
        .contents = writer.buf.items,
        .source_map = source_map,
    };
}

pub fn print(data: AstData, node: AstNode) !void {
    const DebugOutput = struct {
        len: usize = 0,

        pub fn write(this: *@This(), text: []const u8) void {
            this.len += text.len;
            std.debug.print("{s}", .{text});
        }

        pub inline fn pos(this: *const @This()) usize {
            return this.len;
        }
    };

    var out = DebugOutput{};
    var printer = Printer(DebugOutput, .{}).init(data, &out);
    printer.skip_types = false;
    try printer.visit(&node);
}

pub const Writer = struct {
    buf: std.ArrayList(u8),

    pub fn init(capacity: usize) !@This() {
        return .{
            .buf = try std.ArrayList(u8).initCapacity(getAllocator(), capacity),
        };
    }

    pub fn write(this: *@This(), text: []const u8) void {
        this.buf.appendSlice(text) catch @panic("Out of memory");
    }

    pub inline fn pos(this: *const @This()) usize {
        return this.buf.items.len;
    }
};

pub fn printInMemory(data: AstData, node: AstNode) ![]const u8 {
    var writer = try Writer.init(data.source.len);
    var printer = Printer(Writer, .{}).init(data, &writer);
    printer.skip_types = true;
    try printer.visit(&node);

    return writer.buf.items;
}

pub fn printInMemoryWithTypes(data: AstData, node: AstNode) ![]const u8 {
    var writer = try Writer.init(data.source.len);
    var printer = Printer(Writer, .{}).init(data, &writer);
    printer.skip_types = false;
    try printer.visit(&node);

    return writer.buf.items;
}

pub fn printWithSourceMap(data: AstData, node: AstNode) !struct { contents: []const u8, source_map: SourceMap } {
    var writer = try Writer.init(data.source.len);
    var printer = Printer(Writer, .{ .print_source_map = true }).init(data, &writer);
    printer.skip_types = true;
    try printer.visit(&node);

    return .{
        .contents = writer.buf.items,
        .source_map = printer.source_map,
    };
}

pub fn printToCjs(_data: AstData, _replacements: ?*anyopaque) !struct { contents: []const u8, source_map: SourceMap } {
    var data = try _data.clone();

    var replacements = std.AutoArrayHashMap(NodeRef, NodeRef).init(getAllocator());
    defer replacements.deinit();

    if (_replacements) |r| {
        const z: *std.AutoArrayHashMap(NodeRef, NodeRef) = @alignCast(@ptrCast(r));
        var iter = z.iterator();
        try replacements.ensureUnusedCapacity(iter.len);
        while (iter.next()) |e| {
            replacements.putAssumeCapacity(e.key_ptr.*, e.value_ptr.*);
        }
    }

    var statements = NodeList.init(&data.nodes);
    var late_exports = std.ArrayList(NodeRef).init(getAllocator());
    defer late_exports.deinit();

    const copyStr = struct {
        pub fn f(comptime s: []const u8) ![]const u8 {
            const p = try getAllocator().alloc(u8, s.len);
            @memcpy(p, s);
            return p;
        }
    }.f;

    const x3 = try copyStr(__esm_exports_helper);

    try statements.append(.{
        .kind = .verbatim_node,
        .data = x3.ptr,
        .len = @intCast(x3.len),
        .extra_data = 1, // 1 new line
    });

    const x = try copyStr("require");

    const req = try data.nodes.push(.{
        .kind = .identifier,
        .data = x.ptr,
        .len = @intCast(x.len),
    });

    const default_ident = try data.nodes.push(.{
        .kind = .default_keyword,
    });

    const x2 = try copyStr("exports");

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
        pub fn f(_exports_ident: NodeRef, nodes: *BumpAllocator(AstNode), ident: NodeRef, init: NodeRef) !NodeRef {
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
    }.f;

    const first_statement = maybeUnwrapRef(data.nodes.at(data.start)) orelse 0;
    var iter = NodeIterator.init(&data.nodes, first_statement);
    while (iter.nextPair()) |pair| {
        switch (pair[0].kind) {
            .import_declaration => {
                const d = getPackedData(pair[0]);
                const spec = d.right;
                if (d.left == 0) {
                    // Side-effect import
                    const r = try data.nodes.push(.{
                        .kind = .call_expression,
                        .data = toBinaryDataPtrRefs(req, spec),
                    });
                    try replacements.put(pair[1], r);
                    continue;
                }

                const r = try data.nodes.push(.{
                    .kind = .call_expression,
                    .data = toBinaryDataPtrRefs(req, spec),
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

    var writer = try Writer.init(data.source.len);
    var printer = Printer(Writer, .{ .print_source_map = true, .use_replacements = true }).init(data, &writer);
    printer.skip_types = true;
    printer.replacements = &replacements;

    const sf = AstNode{
        .kind = .source_file,
        .data = @ptrFromInt(statements.head),
    };

    try printer.visit(&sf);

    return .{
        .contents = writer.buf.items,
        .source_map = printer.source_map,
    };
}

const SourceMap = struct {
    const base64_alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

    sources: std.ArrayList([]const u8),
    names: std.ArrayList([]const u8),
    mappings: std.ArrayList(u8),

    prev_col: u32 = 0,
    prev_source_line: u32 = 0,
    prev_source_col: u32 = 0,

    line: u32 = 0,
    last_line_pos: usize = 0,

    current_source_index: u32 = 0,
    source_index_delta: i32 = 0, // Always resets to 0 after a write

    needs_comma: bool = false,

    pub fn init(allocator: std.mem.Allocator) @This() {
        return .{
            .sources = std.ArrayList([]const u8).init(allocator),
            .names = std.ArrayList([]const u8).init(allocator),
            .mappings = std.ArrayList(u8).init(allocator),
        };
    }

    pub fn deinit(this: *@This()) void {
        this.sources.deinit();
        this.names.deinit();
        this.mappings.deinit();
    }

    inline fn encodeBase64(n: u32) u8 {
        return base64_alphabet[n];
    }

    fn encodeVlq(this: *@This(), n: i32) !void {
        if (n == 0) {
            return this.mappings.append(encodeBase64(0));
        }

        var x: u32 = 0;
        var val: u32 = @abs(n);
        x |= (val & 0xF) << 1;
        if (n < 0) {
            x |= 1;
        }

        val >>= 4;
        if (val > 0) {
            try this.mappings.append(encodeBase64(x | 0x20));
        } else {
            return this.mappings.append(encodeBase64(x));
        }

        while (true) {
            x = val & 0x1F;
            val >>= 5;

            if (val > 0) {
                try this.mappings.append(encodeBase64(x | 0x20));
            } else {
                return this.mappings.append(encodeBase64(x));
            }
        }
    }

    pub fn setSource(this: *@This(), name: []const u8) !void {
        // Linear scan is fine for small # of sources
        // An optimization is to compute a hash ahead of time
        const index = blk: {
            for (this.sources.items, 0..) |s, i| {
                if (strings.eql(name, s)) {
                    break :blk i;
                }
            }

            try this.sources.append(name);

            break :blk this.sources.items.len - 1;
        };

        const new_delta = @as(i32, @intCast(index)) - (@as(i32, @intCast(this.current_source_index)) + this.source_index_delta);
        this.current_source_index = @intCast(index);
        this.source_index_delta = new_delta;
    }

    pub fn encodeSegment(this: *@This(), pos: usize, source_line: u32, source_col: u32) !void {
        const col: u32 = @intCast(pos - this.last_line_pos);
        if (col == source_col and this.line == source_line and this.source_index_delta == 0) {
            return;
        }

        if (this.needs_comma) {
            try this.mappings.append(',');
        }

        try this.encodeVlq(@as(i32, @intCast(col)) - @as(i32, @intCast(this.prev_col)));
        try this.encodeVlq(this.source_index_delta);
        try this.encodeVlq(@as(i32, @intCast(source_line)) - @as(i32, @intCast(this.prev_source_line)));
        try this.encodeVlq(@as(i32, @intCast(source_col)) - @as(i32, @intCast(this.prev_source_col)));

        this.prev_col = col;
        this.prev_source_line = source_line;
        this.prev_source_col = source_col;
        this.source_index_delta = 0;
        this.needs_comma = true;
    }

    // inline fn writeSeparators(this: *@This()) !void {
    //     if (this.deferred_lines == 0) {
    //         return;
    //     }

    //     this.prev_col = 0;
    //     this.needs_comma = false;

    //     while (this.deferred_lines >= 4) {

    //     }
    // }

    pub fn endLine(this: *@This(), pos: usize) !void {
        this.prev_col = 0;
        this.line += 1;
        this.last_line_pos = pos;
        this.needs_comma = false;
        try this.mappings.append(';');
    }

    fn write(allocator: std.mem.Allocator, val: anytype, comptime is_base64: bool) ![]const u8 {
        // const NoOpEncoder = struct {
        //     pub fn calcSize(encoder: *const @This(), source_len: usize) usize {
        //         return source_len;
        //     }

        //     pub fn encode(encoder: *const @This(), dest: []u8, source: []const u8) []const u8 {

        //     }
        // };

        // const encoder = std.base64.standard_no_pad.Encoder;
        _ = is_base64;

        const s = switch (@typeInfo(@TypeOf(val))) {
            .Struct => |s| s,
            else => unreachable,
        };

        var size: usize = 0;
        size += 2; // {}
        size += s.fields.len - 1; // commas

        inline for (s.fields) |f| {
            const v = @field(val, f.name);
            const T = @TypeOf(v);

            switch (T) {
                comptime_int, u32 => {
                    size += (v / 10) + 1;
                },
                []u8, []const u8 => {
                    size += v.len + 2;
                },
                [][]const u8 => {
                    for (v) |str| {
                        size += str.len + 2;
                    }
                    if (v.len > 0) size += v.len - 1; // commas
                    size += 2; // []
                },
                else => {
                    std.debug.print("{any}", .{T});
                    return error.TODO;
                },
            }

            size += f.name.len + 3; // "":
        }

        const buf = try allocator.alloc(u8, size);
        var fbs = std.io.fixedBufferStream(buf);
        _ = try fbs.write("{");

        inline for (s.fields, 0..) |f, i| {
            try std.fmt.format(fbs.writer().any(), "\"{s}\":", .{f.name});

            const v = @field(val, f.name);
            const T = @TypeOf(v);

            switch (T) {
                comptime_int, u32 => {
                    try std.fmt.format(fbs.writer().any(), "{}", .{v});
                },
                []u8, []const u8 => {
                    try std.fmt.format(fbs.writer().any(), "\"{s}\"", .{v});
                },
                [][]const u8 => {
                    _ = try fbs.write("[");
                    for (v, 0..) |str, j| {
                        try std.fmt.format(fbs.writer().any(), "\"{s}\"", .{str});
                        if (j < v.len - 1) {
                            _ = try fbs.write(",");
                        }
                    }
                    _ = try fbs.write("]");
                },
                else => return error.TODO,
            }

            if (i < s.fields.len - 1) {
                _ = try fbs.write(",");
            }
        }

        _ = try fbs.write("}");

        return buf;
    }

    fn _toJson(this: *@This(), allocator: std.mem.Allocator, comptime is_base64: bool) ![]const u8 {
        // {
        // "version" : 3,
        // "file": "out.js",
        // "sourceRoot": "",
        // "sources": ["foo.js", "bar.js"],
        // "sourcesContent": [null, null],
        // "names": ["src", "maps", "are", "fun"],
        // "mappings": "A,AAAB;;ABCDE;"
        // }

        return write(allocator, .{
            .version = 3,
            .sources = this.sources.items,
            .mappings = this.mappings.items,
        }, is_base64);
    }

    pub fn toJson(this: *@This(), allocator: std.mem.Allocator) ![]const u8 {
        return this._toJson(allocator, false);
    }

    pub fn toInlineComment(this: *@This(), allocator: std.mem.Allocator) ![]const u8 {
        const prefix = "//# sourceMappingURL=data:application/json;base64,";
        const json_str = try this.toJson(allocator);
        defer allocator.free(json_str);

        const buf = try allocator.alloc(u8, prefix.len + std.base64.standard.Encoder.calcSize(json_str.len));
        @memcpy(buf[0..prefix.len], prefix);
        _ = std.base64.standard.Encoder.encode(buf[prefix.len..], json_str);

        return buf;
    }
};
