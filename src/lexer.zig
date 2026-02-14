const std = @import("std");

const tables = @import("lexer_tables.zig");

const string = []const u8;
const strings = @import("string_immutable.zig");
const CodePoint = i32;
const JavascriptString = []const u16;
const Indentation = struct {};
const ComptimeStringMap = @import("comptime_string_map.zig").ComptimeStringMap;

const unicode = std.unicode;

pub const T = tables.T;
pub const DefaultKeywords = tables.Keywords;
pub const ExtraKeyword = tables.ExtraKeyword;
pub const ClassScopeKeywords = tables.ClassScopeKeywords;
pub const ObjectLiteralKeywords = tables.ObjectLiteralKeywords;
pub const ConstructorParameterKeywords = tables.ConstructorParameterKeywords;
pub const InterfaceScopeKeywords = tables.InterfaceScopeKeywords;

pub const ChildlessJSXTags = tables.ChildlessJSXTags;

pub const Source = struct {
    contents: []const u8,
    name: ?[]const u8 = null,
};

const record_lines = true;

pub var emptyJavaScriptString = ([_]u16{0});

const is_debug = @import("builtin").mode == .Debug;

pub const JSONOptions = struct {
    /// Enable JSON-specific warnings/errors
    is_json: bool = false,

    /// tsconfig.json supports comments & trailing comments
    allow_comments: bool = false,
    allow_trailing_commas: bool = false,

    /// Loading JSON-in-JSON may start like \\""\\"
    /// This is technically invalid, since we parse from the first value of the string
    ignore_leading_escape_sequences: bool = false,
    ignore_trailing_escape_sequences: bool = false,

    json_warn_duplicate_keys: bool = true,

    always_decode_escape_sequences: bool = false,
};

pub fn NewLexer(
    comptime enable_simd: bool,
    comptime json_options: JSONOptions,
) type {
    return NewLexer_(
        enable_simd,
        json_options.is_json,
        json_options.allow_comments,
        json_options.allow_trailing_commas,
        json_options.ignore_leading_escape_sequences,
        json_options.ignore_trailing_escape_sequences,
        json_options.json_warn_duplicate_keys,
        json_options.always_decode_escape_sequences,
    );
}

fn NewLexer_(
    comptime enable_simd: bool,
    comptime json_options_is_json: bool,
    comptime json_options_allow_comments: bool,
    comptime json_options_allow_trailing_commas: bool,
    comptime json_options_ignore_leading_escape_sequences: bool,
    comptime json_options_ignore_trailing_escape_sequences: bool,
    comptime json_options_json_warn_duplicate_keys: bool,
    comptime json_options_always_decode_escape_sequences: bool,
) type {
    const json_options = JSONOptions{
        .is_json = json_options_is_json,
        .allow_comments = json_options_allow_comments,
        .allow_trailing_commas = json_options_allow_trailing_commas,
        .ignore_leading_escape_sequences = json_options_ignore_leading_escape_sequences,
        .ignore_trailing_escape_sequences = json_options_ignore_trailing_escape_sequences,
        .json_warn_duplicate_keys = json_options_json_warn_duplicate_keys,
        .always_decode_escape_sequences = json_options_always_decode_escape_sequences,
    };
    return struct {
        const LexerType = @This();
        const is_json = json_options.is_json;
        const json = json_options;
        const JSONBool = if (is_json) bool else void;
        const JSONBoolDefault: JSONBool = if (is_json) true else {};
        const LineMapType = if (record_lines) LineMap else void;

        pub const Error = error{
            UTF8Fail,
            OutOfMemory,
            SyntaxError,
            UnexpectedSyntax,
            JSONStringsMustUseDoubleQuotes,
            ParserError,
        };

        source: Source,
        current: usize = 0,
        start: usize = 0,
        end: usize = 0,
        // previous_backslash_quote_in_jsx: logger.Range = logger.Range.None,
        token: T = T.t_end_of_file,
        has_newline_before: bool = false,
        is_legacy_octal_literal: bool = false,
        is_log_disabled: bool = true,
        print_expect: bool = true,
        code_point: CodePoint = -1,
        identifier: []const u8 = "",
        // jsx_pragma: JSXPragma = .{},
        // source_mapping_url: ?js_ast.Span = null,
        number: f64 = 0.0,
        rescan_close_brace_as_template_token: bool = false,
        regex_flags_start: ?u16 = null,
        allocator: std.mem.Allocator,
        /// In JavaScript, strings are stored as UTF-16, but nearly every string is ascii.
        /// This means, usually, we can skip UTF8 -> UTF16 conversions.
        string_literal_buffer: std.ArrayList(u16),
        string_literal_slice: []const u8 = "",
        string_literal: []const u16,
        string_literal_is_ascii: bool = false,

        /// Only used for JSON stringification when bundling
        /// This is a zero-bit type unless we're parsing JSON.
        is_ascii_only: JSONBool = JSONBoolDefault,

        last_line: usize = 0,
        full_start: u32 = 0,

        pause_on_comments: bool = true,

        line_map: LineMapType = if (record_lines) undefined else {},

        pub fn clone(self: *const LexerType) LexerType {
            return LexerType{
                .source = self.source,
                .current = self.current,
                .start = self.start,
                .end = self.end,
                .token = self.token,
                .has_newline_before = self.has_newline_before,
                .is_legacy_octal_literal = self.is_legacy_octal_literal,
                .is_log_disabled = self.is_log_disabled,
                .code_point = self.code_point,
                .identifier = self.identifier,
                .regex_flags_start = self.regex_flags_start,
                .number = self.number,
                .rescan_close_brace_as_template_token = self.rescan_close_brace_as_template_token,
                .allocator = self.allocator,
                .string_literal_buffer = self.string_literal_buffer,
                .string_literal_slice = self.string_literal_slice,
                .string_literal = self.string_literal,
                .string_literal_is_ascii = self.string_literal_is_ascii,
                .is_ascii_only = self.is_ascii_only,
                .line_map = self.line_map,
            };
        }

        pub fn syntaxError(self: *LexerType) !void {
            @setCold(true);
            // @branchHint(.cold);

            self.addError(self.start, "Syntax Error!!", .{}, true);
            return Error.SyntaxError;
        }

        pub fn addDefaultError(self: *LexerType, msg: []const u8) !void {
            @setCold(true);

            self.addError(self.start, "{s}", .{msg}, true);
            return Error.SyntaxError;
        }

        pub fn addSyntaxError(self: *LexerType, _loc: usize, comptime fmt: []const u8, args: anytype) !void {
            @setCold(true);
            self.addError(_loc, fmt, args, false);
            return Error.SyntaxError;
        }

        pub fn addError(self: *LexerType, _loc: usize, comptime format: []const u8, args: anytype, _: bool) void {
            @setCold(true);

            if (self.is_log_disabled) return;

            _ = _loc;
            _ = format;
            _ = args;
            //self.log.addErrorFmt(&self.source, __loc, self.allocator, format, args) catch unreachable;
        }

        pub fn addRangeError(self: *LexerType, start: u32, end: u32, comptime format: []const u8, args: anytype) !void {
            @setCold(true);
            _ = self;
            _ = start;
            _ = end;
            _ = format;
            _ = args;
        }

        pub fn addCurrentRangeError(self: *LexerType, comptime format: []const u8, args: anytype) !void {
            @setCold(true);
            return self.addRangeError(@intCast(self.start), @intCast(self.end), format, args);
        }

        pub inline fn isIdentifierOrKeyword(lexer: LexerType) bool {
            return @intFromEnum(lexer.token) >= @intFromEnum(T.t_identifier);
        }

        pub fn deinit(this: *LexerType) void {
            if (comptime record_lines) {
                this.line_map.deinit();
            }
        }

        pub fn preAllocate(this: *LexerType) !void {
            if (comptime record_lines) {
                try this.line_map.approximateCapacityForSize(this.source.contents.len);
            }
        }

        fn decodeEscapeSequences(lexer: *LexerType, start: usize, text: []const u8, comptime BufType: type, buf_: *BufType) !void {
            var buf = buf_.*;
            defer buf_.* = buf;
            if (comptime is_json) lexer.is_ascii_only = false;

            const iterator = strings.CodepointIterator{ .bytes = text, .i = 0 };
            var iter = strings.CodepointIterator.Cursor{};
            while (iterator.next(&iter)) {
                const width = iter.width;
                switch (iter.c) {
                    '\r' => {
                        // From the specification:
                        //
                        // 11.8.6.1 Static Semantics: TV and TRV
                        //
                        // TV excludes the code units of LineContinuation while TRV includes
                        // them. <CR><LF> and <CR> LineTerminatorSequences are normalized to
                        // <LF> for both TV and TRV. An explicit EscapeSequence is needed to
                        // include a <CR> or <CR><LF> sequence.

                        // Convert '\r\n' into '\n'
                        const next_i: usize = iter.i + 1;
                        iter.i += @as(u32, @intFromBool(next_i < text.len and text[next_i] == '\n'));

                        // Convert '\r' into '\n'
                        buf.append('\n') catch unreachable;
                        continue;
                    },

                    '\\' => {
                        _ = iterator.next(&iter) or return;

                        const c2 = iter.c;

                        const width2 = iter.width;
                        switch (c2) {
                            // https://mathiasbynens.be/notes/javascript-escapes#single
                            'b' => {
                                buf.append(0x08) catch unreachable;
                                continue;
                            },
                            'f' => {
                                buf.append(0x0C) catch unreachable;
                                continue;
                            },
                            'n' => {
                                buf.append(0x0A) catch unreachable;
                                continue;
                            },
                            'v' => {
                                buf.append(0x0B) catch unreachable;
                                continue;
                            },
                            't' => {
                                buf.append(0x09) catch unreachable;
                                continue;
                            },
                            'r' => {
                                buf.append(0x0D) catch unreachable;
                                continue;
                            },

                            // legacy octal literals
                            '0'...'7' => {
                                const octal_start = (iter.i + width2) - 2;
                                if (comptime is_json) {
                                    lexer.end = start + iter.i - width2;
                                    try lexer.syntaxError();
                                }

                                // 1-3 digit octal
                                var is_bad = false;
                                var value: i64 = c2 - '0';
                                var restore = iter;

                                _ = iterator.next(&iter) or {
                                    if (value == 0) {
                                        try buf.append(0);
                                        return;
                                    }

                                    try lexer.syntaxError();
                                    return;
                                };

                                const c3: CodePoint = iter.c;

                                switch (c3) {
                                    '0'...'7' => {
                                        value = value * 8 + c3 - '0';
                                        restore = iter;
                                        _ = iterator.next(&iter) or return lexer.syntaxError();

                                        const c4 = iter.c;
                                        switch (c4) {
                                            '0'...'7' => {
                                                const temp = value * 8 + c4 - '0';
                                                if (temp < 256) {
                                                    value = temp;
                                                } else {
                                                    iter = restore;
                                                }
                                            },
                                            '8', '9' => {
                                                is_bad = true;
                                            },
                                            else => {
                                                iter = restore;
                                            },
                                        }
                                    },
                                    '8', '9' => {
                                        is_bad = true;
                                    },
                                    else => {
                                        iter = restore;
                                    },
                                }

                                iter.c = @as(i32, @intCast(value));
                                if (is_bad) {
                                    lexer.addRangeError(
                                        octal_start,
                                        iter.i,
                                        "Invalid legacy octal literal",
                                        .{},
                                    ) catch unreachable;
                                }
                            },
                            '8', '9' => {
                                iter.c = c2;
                            },
                            // 2-digit hexadecimal
                            'x' => {
                                var value: CodePoint = 0;
                                var c3: CodePoint = 0;
                                var width3: u3 = 0;

                                _ = iterator.next(&iter) or return lexer.syntaxError();
                                c3 = iter.c;
                                width3 = iter.width;
                                switch (c3) {
                                    '0'...'9' => {
                                        value = value * 16 | (c3 - '0');
                                    },
                                    'a'...'f' => {
                                        value = value * 16 | (c3 + 10 - 'a');
                                    },
                                    'A'...'F' => {
                                        value = value * 16 | (c3 + 10 - 'A');
                                    },
                                    else => {
                                        lexer.end = start + iter.i - width3;
                                        return lexer.syntaxError();
                                    },
                                }

                                _ = iterator.next(&iter) or return lexer.syntaxError();
                                c3 = iter.c;
                                width3 = iter.width;
                                switch (c3) {
                                    '0'...'9' => {
                                        value = value * 16 | (c3 - '0');
                                    },
                                    'a'...'f' => {
                                        value = value * 16 | (c3 + 10 - 'a');
                                    },
                                    'A'...'F' => {
                                        value = value * 16 | (c3 + 10 - 'A');
                                    },
                                    else => {
                                        lexer.end = start + iter.i - width3;
                                        return lexer.syntaxError();
                                    },
                                }

                                iter.c = value;
                            },
                            'u' => {
                                // We're going to make this an i64 so we don't risk integer overflows
                                // when people do weird things
                                var value: i64 = 0;

                                _ = iterator.next(&iter) or return lexer.syntaxError();
                                var c3 = iter.c;
                                var width3 = iter.width;

                                // variable-length
                                if (c3 == '{') {
                                    if (comptime is_json) {
                                        lexer.end = start + iter.i - width2;
                                        try lexer.syntaxError();
                                    }

                                    const hex_start = (iter.i + start) - width - width2 - width3;
                                    var is_first = true;
                                    var is_out_of_range = false;
                                    variableLength: while (true) {
                                        _ = iterator.next(&iter) or break :variableLength;
                                        c3 = iter.c;

                                        switch (c3) {
                                            '0'...'9' => {
                                                value = value * 16 | (c3 - '0');
                                            },
                                            'a'...'f' => {
                                                value = value * 16 | (c3 + 10 - 'a');
                                            },
                                            'A'...'F' => {
                                                value = value * 16 | (c3 + 10 - 'A');
                                            },
                                            '}' => {
                                                if (is_first) {
                                                    lexer.end = (start + iter.i) -| width3;
                                                    return lexer.syntaxError();
                                                }
                                                break :variableLength;
                                            },
                                            else => {
                                                lexer.end = (start + iter.i) -| width3;
                                                return lexer.syntaxError();
                                            },
                                        }

                                        // '\U0010FFFF
                                        // copied from golang utf8.MaxRune
                                        if (value > 1114111) {
                                            is_out_of_range = true;
                                        }
                                        is_first = false;
                                    }

                                    if (is_out_of_range) {
                                        try lexer.addRangeError(
                                            @intCast(start + hex_start),
                                            @intCast(iter.i + start),
                                            "Unicode escape sequence is out of range",
                                            .{},
                                        );

                                        return;
                                    }

                                    // fixed-length
                                } else {
                                    // Fixed-length
                                    // comptime var j: usize = 0;
                                    var j: usize = 0;
                                    while (j < 4) : (j += 1) {
                                        switch (c3) {
                                            '0'...'9' => {
                                                value = value * 16 | (c3 - '0');
                                            },
                                            'a'...'f' => {
                                                value = value * 16 | (c3 + 10 - 'a');
                                            },
                                            'A'...'F' => {
                                                value = value * 16 | (c3 + 10 - 'A');
                                            },
                                            else => {
                                                lexer.end = start + iter.i - width3;
                                                return lexer.syntaxError();
                                            },
                                        }

                                        if (j < 3) {
                                            _ = iterator.next(&iter) or return lexer.syntaxError();
                                            c3 = iter.c;

                                            width3 = iter.width;
                                        }
                                    }
                                }

                                iter.c = @as(CodePoint, @truncate(value));
                            },
                            '\r' => {
                                if (comptime is_json) {
                                    lexer.end = start + iter.i - width2;
                                    try lexer.syntaxError();
                                }

                                // Make sure Windows CRLF counts as a single newline
                                const next_i: usize = iter.i + 1;
                                iter.i += @as(u32, @intFromBool(next_i < text.len and text[next_i] == '\n'));

                                // Ignore line continuations. A line continuation is not an escaped newline.
                                continue;
                            },
                            '\n', 0x2028, 0x2029 => {
                                if (comptime is_json) {
                                    lexer.end = start + iter.i - width2;
                                    try lexer.syntaxError();
                                }

                                // Ignore line continuations. A line continuation is not an escaped newline.
                                continue;
                            },
                            else => {
                                if (comptime is_json) {
                                    switch (c2) {
                                        '"', '\\', '/' => {},
                                        else => {
                                            lexer.end = start + iter.i - width2;
                                            try lexer.syntaxError();
                                        },
                                    }
                                }
                                iter.c = c2;
                            },
                        }
                    },
                    else => {},
                }

                switch (iter.c) {
                    -1 => return try lexer.addDefaultError("Unexpected end of file"),
                    0...0xFFFF => {
                        buf.append(@as(u16, @intCast(iter.c))) catch unreachable;
                    },
                    else => {
                        iter.c -= 0x10000;
                        buf.ensureUnusedCapacity(2) catch unreachable;
                        buf.appendAssumeCapacity(@as(u16, @intCast(0xD800 + ((iter.c >> 10) & 0x3FF))));
                        buf.appendAssumeCapacity(@as(u16, @intCast(0xDC00 + (iter.c & 0x3FF))));
                    },
                }
            }
        }

        pub const InnerStringLiteral = packed struct { suffix_len: u3, needs_slow_path: bool };

        fn parseStringLiteralInner(lexer: *LexerType, comptime quote: CodePoint) !InnerStringLiteral {
            const check_for_backslash = comptime (is_json and json_options.always_decode_escape_sequences);
            var needs_slow_path = false;
            var suffix_len: u3 = if (comptime quote == 0) 0 else 1;
            var has_backslash: if (check_for_backslash) bool else void = if (comptime check_for_backslash) false else {};
            stringLiteral: while (true) {
                switch (lexer.code_point) {
                    '\\' => {
                        if (comptime check_for_backslash) {
                            has_backslash = true;
                        }

                        lexer.step();

                        // Handle Windows CRLF
                        if (lexer.code_point == '\r' and comptime !is_json) {
                            lexer.step();
                            if (lexer.code_point == '\n') {
                                lexer.step();
                            }
                            continue :stringLiteral;
                        }

                        if (comptime (is_json and json_options.ignore_trailing_escape_sequences)) {
                            if (lexer.code_point == quote and lexer.current >= lexer.source.contents.len) {
                                lexer.step();

                                break;
                            }
                        }

                        switch (lexer.code_point) {
                            // 0 cannot be in this list because it may be a legacy octal literal
                            'v', 'f', 't', 'r', 'n', '`', '\'', '"', '\\', 0x2028, 0x2029 => {
                                lexer.step();

                                continue :stringLiteral;
                            },
                            else => {
                                needs_slow_path = true;
                            },
                        }
                    },
                    // This indicates the end of the file

                    -1 => {
                        if (comptime quote != 0) {
                            try lexer.addDefaultError("Unterminated string literal");
                        }

                        break :stringLiteral;
                    },

                    '\r' => {
                        if (comptime quote != '`') {
                            try lexer.addDefaultError("Unterminated string literal");
                        }

                        // Template literals require newline normalization
                        needs_slow_path = true;
                    },

                    '\n' => {
                        switch (comptime quote) {
                            0 => {
                                break :stringLiteral;
                            },
                            '`' => {
                                try lexer.recordNewLine();
                            },
                            else => {
                                try lexer.addDefaultError("Unterminated string literal");
                            },
                        }
                    },

                    '$' => {
                        if (comptime quote == '`') {
                            lexer.step();
                            if (lexer.code_point == '{') {
                                suffix_len = 2;
                                lexer.step();
                                lexer.token = if (lexer.rescan_close_brace_as_template_token)
                                    T.t_template_middle
                                else
                                    T.t_template_head;

                                break :stringLiteral;
                            }
                            continue :stringLiteral;
                        }
                    },
                    // exit condition
                    quote => {
                        lexer.step();

                        break;
                    },

                    else => {

                        // Non-ASCII strings need the slow path
                        if (lexer.code_point >= 0x80) {
                            needs_slow_path = true;
                        } else if ((comptime is_json) and lexer.code_point < 0x20) {
                            try lexer.syntaxError();
                        } else if (comptime (quote == '"' or quote == '\'')) {
                            const remainder = lexer.source.contents[lexer.current..];
                            if (remainder.len >= 4096) {
                                lexer.current += indexOfInterestingCharacterInStringLiteral(remainder, quote) orelse {
                                    lexer.step();
                                    continue;
                                };
                                lexer.end = lexer.current -| 1;
                                lexer.step();
                                continue;
                            }
                        }
                    },
                }

                lexer.step();
            }

            if (comptime check_for_backslash) needs_slow_path = needs_slow_path or has_backslash;

            return InnerStringLiteral{ .needs_slow_path = needs_slow_path, .suffix_len = suffix_len };
        }

        pub fn parseStringLiteral(lexer: *LexerType, comptime quote: CodePoint) !void {
            if (comptime quote != '`') {
                lexer.token = T.t_string_literal;
            } else if (lexer.rescan_close_brace_as_template_token) {
                lexer.token = T.t_template_tail;
            } else {
                lexer.token = T.t_no_substitution_template_literal;
            }

            lexer.step();

            const string_literal_details = try lexer.parseStringLiteralInner(quote);

            // Reset string literal
            const base = if (comptime quote == 0) lexer.start else lexer.start + 1;
            lexer.string_literal_slice = lexer.source.contents[base..@min(lexer.source.contents.len, lexer.end - @as(usize, string_literal_details.suffix_len))];
            lexer.string_literal_is_ascii = !string_literal_details.needs_slow_path;
            if (string_literal_details.needs_slow_path) {
                lexer.string_literal_buffer.shrinkRetainingCapacity(0);
                lexer.string_literal_buffer.ensureUnusedCapacity(lexer.string_literal_slice.len) catch unreachable;
                try lexer.decodeEscapeSequences(lexer.start, lexer.string_literal_slice, @TypeOf(lexer.string_literal_buffer), &lexer.string_literal_buffer);
                lexer.string_literal = lexer.string_literal_buffer.items;
            }
            if (comptime is_json) lexer.is_ascii_only = lexer.is_ascii_only and lexer.string_literal_is_ascii;

            // if (comptime !FeatureFlags.allow_json_single_quotes) {
            //     if (quote == '\'' and is_json) {
            //         try lexer.addRangeError(lexer.range(), "JSON strings must use double quotes", .{}, true);
            //     }
            // }

            // for (text)
            // // if (needs_slow_path) {
            // //     // Slow path

            // //     // lexer.string_literal = lexer.(lexer.start + 1, text);
            // // } else {
            // //     // Fast path

            // // }
        }

        inline fn nextCodepointSlice(it: *LexerType) []const u8 {
            const cp_len = strings.wtf8ByteSequenceLengthWithInvalid(it.source.contents.ptr[it.current]);
            return if (!(cp_len + it.current > it.source.contents.len)) it.source.contents[it.current .. cp_len + it.current] else "";
        }

        inline fn nextCodepoint(it: *LexerType) CodePoint {
            it.end = it.current;

            const cp_len = strings.wtf8ByteSequenceLengthWithInvalid(it.source.contents.ptr[it.current]);
            if (cp_len + it.current > it.source.contents.len) {
                it.current += 1;
                return -1;
            }

            const slice = it.source.contents[it.current .. cp_len + it.current];
            if (cp_len == 1) {
                const code_point = @as(CodePoint, slice[0]);
                it.current += 1;
                return code_point;
            }

            const code_point = strings.decodeWTF8RuneTMultibyte(slice.ptr[0..4], @as(u3, @intCast(slice.len)), CodePoint, strings.unicode_replacement);

            it.current += if (code_point != strings.unicode_replacement)
                cp_len
            else
                1;

            return code_point;
        }

        // Inlining adds ~10% to binary size for ~5-10% speed improvement
        inline fn step(lexer: *LexerType) void {
            lexer.code_point = lexer.nextCodepoint();
        }

        pub fn getContext(self: *LexerType, amount: usize) []const u8 {
            const left = if (amount > self.start) 0 else self.start - amount;
            const right = @min(self.start + amount, self.source.contents.len);

            return self.source.contents[left..right];
        }

        pub inline fn expect(self: *LexerType, comptime token: T) !void {
            if (self.token != token) try self.expected(token);

            try self.next();
        }

        pub inline fn expectInClassScope(self: *LexerType, comptime token: T) !void {
            if (self.token != token) try self.expected(token);

            try self.nextInClassScope();
        }

        pub inline fn expectInInterfaceScope(self: *LexerType, comptime token: T) !void {
            if (self.token != token) try self.expected(token);

            // FIXME: remove the "newline" hack
            if (comptime token == .t_open_brace) {
                return self.nextInInterfaceScopeModifier();
            }

            try self.nextInInterfaceScope();
        }

        pub inline fn expectInConstructorParameterList(self: *LexerType, comptime token: T) !void {
            if (self.token != token) try self.expected(token);

            try self.nextInConstructorParameterList();
        }

        pub fn addUnsupportedSyntaxError(self: *LexerType, msg: []const u8) !void {
            self.addError(self.end, "Unsupported syntax: {s}", .{msg}, true);
            return Error.SyntaxError;
        }

        pub const IdentifierKind = enum { normal, private };
        pub const ScanResult = struct { token: T, contents: string };
        threadlocal var small_escape_sequence_buffer: [4096]u16 = undefined;
        const FakeArrayList16 = struct {
            items: []u16,
            i: usize = 0,

            pub fn append(fake: *FakeArrayList16, value: u16) !void {
                fake.items[fake.i] = value;
                fake.i += 1;
            }

            pub fn appendAssumeCapacity(fake: *FakeArrayList16, value: u16) void {
                fake.items[fake.i] = value;
                fake.i += 1;
            }
            pub fn ensureUnusedCapacity(fake: *FakeArrayList16, int: anytype) !void {
                _ = fake;
                _ = int;
            }
        };
        threadlocal var large_escape_sequence_list: std.ArrayList(u16) = undefined;
        threadlocal var large_escape_sequence_list_loaded: bool = false;

        // This is an edge case that doesn't really exist in the wild, so it doesn't
        // need to be as fast as possible.
        pub fn scanIdentifierWithEscapes(lexer: *LexerType, kind: IdentifierKind) anyerror!ScanResult {
            @setCold(true);

            var result = ScanResult{ .token = .t_end_of_file, .contents = "" };
            // First pass: scan over the identifier to see how long it is
            while (true) {
                // Scan a unicode escape sequence. There is at least one because that's
                // what caused us to get on this slow path in the first place.
                if (lexer.code_point == '\\') {
                    lexer.step();

                    if (lexer.code_point != 'u') {
                        try lexer.syntaxError();
                    }
                    lexer.step();
                    if (lexer.code_point == '{') {
                        // Variable-length
                        lexer.step();
                        while (lexer.code_point != '}') {
                            switch (lexer.code_point) {
                                '0'...'9', 'a'...'f', 'A'...'F' => {
                                    lexer.step();
                                },
                                else => try lexer.syntaxError(),
                            }
                        }

                        lexer.step();
                    } else {
                        // Fixed-length
                        // comptime var j: usize = 0;

                        switch (lexer.code_point) {
                            '0'...'9', 'a'...'f', 'A'...'F' => {
                                lexer.step();
                            },
                            else => try lexer.syntaxError(),
                        }
                        switch (lexer.code_point) {
                            '0'...'9', 'a'...'f', 'A'...'F' => {
                                lexer.step();
                            },
                            else => try lexer.syntaxError(),
                        }
                        switch (lexer.code_point) {
                            '0'...'9', 'a'...'f', 'A'...'F' => {
                                lexer.step();
                            },
                            else => try lexer.syntaxError(),
                        }
                        switch (lexer.code_point) {
                            '0'...'9', 'a'...'f', 'A'...'F' => {
                                lexer.step();
                            },
                            else => try lexer.syntaxError(),
                        }
                    }
                    continue;
                }

                if (!isIdentifierContinue(lexer.code_point)) {
                    break;
                }
                lexer.step();
            }

            // Second pass: re-use our existing escape sequence parser
            const original_text = lexer.raw();
            if (original_text.len < 1024) {
                var buf = FakeArrayList16{ .items = &small_escape_sequence_buffer, .i = 0 };
                try lexer.decodeEscapeSequences(lexer.start, original_text, FakeArrayList16, &buf);
                result.contents = lexer.utf16ToString(buf.items[0..buf.i]);
            } else {
                if (!large_escape_sequence_list_loaded) {
                    large_escape_sequence_list = try std.ArrayList(u16).initCapacity(lexer.allocator, original_text.len);
                    large_escape_sequence_list_loaded = true;
                }

                large_escape_sequence_list.shrinkRetainingCapacity(0);
                try lexer.decodeEscapeSequences(lexer.start, original_text, std.ArrayList(u16), &large_escape_sequence_list);
                result.contents = lexer.utf16ToString(large_escape_sequence_list.items);
            }

            const identifier = if (kind != .private)
                result.contents
            else
                result.contents[1..];

            if (!isIdentifier(identifier)) {
                try lexer.addCurrentRangeError("Invalid identifier: \"{s}\"", .{result.contents});
            }

            result.contents = result.contents;

            // Escaped keywords are not allowed to work as actual keywords, but they are
            // allowed wherever we allow identifiers or keywords. For example:
            //
            //   // This is an error (equivalent to "var var;")
            //   var \u0076\u0061\u0072;
            //
            //   // This is an error (equivalent to "var foo;" except for this rule)
            //   \u0076\u0061\u0072 foo;
            //
            //   // This is an fine (equivalent to "foo.var;")
            //   foo.\u0076\u0061\u0072;
            //
            result.token = if (DefaultKeywords.has(result.contents)) .t_escaped_keyword else .t_identifier;

            return result;
        }

        pub fn expectContextualKeyword(self: *LexerType, comptime keyword: string) !void {
            if (!self.isContextualKeyword(keyword)) {
                if (comptime is_debug) {
                    self.addError(self.start, "Expected \"{s}\" but found \"{s}\" (token: {s})", .{
                        keyword,
                        self.raw(),
                        @tagName(self.token),
                    }, true);
                } else {
                    std.debug.print("{s} {}\n", .{self.source.name orelse "", self.last_line});
                    self.addError(self.start, "Expected \"{s}\" but found \"{s}\"", .{ keyword, self.raw() }, true);
                }
                return Error.UnexpectedSyntax;
            }
            try self.next();
        }

        pub fn maybeExpandEquals(lexer: *LexerType) !void {
            switch (lexer.code_point) {
                '>' => {
                    // "=" + ">" = "=>"
                    lexer.token = .t_equals_greater_than;
                    lexer.step();
                },
                '=' => {
                    // "=" + "=" = "=="
                    lexer.token = .t_equals_equals;
                    lexer.step();

                    if (lexer.code_point == '=') {
                        // "=" + "==" = "==="
                        lexer.token = .t_equals_equals_equals;
                        lexer.step();
                    }
                },
                else => {},
            }
        }

        pub fn expectLessThan(lexer: *LexerType, comptime is_inside_jsx_element: bool) !void {
            switch (lexer.token) {
                .t_less_than => {
                    if (is_inside_jsx_element) {
                        try lexer.nextInsideJSXElement();
                    } else {
                        try lexer.next();
                    }
                },
                .t_less_than_equals => {
                    lexer.token = .t_equals;
                    lexer.start += 1;
                    try lexer.maybeExpandEquals();
                },
                .t_less_than_less_than => {
                    lexer.token = .t_less_than;
                    lexer.start += 1;
                },
                .t_less_than_less_than_equals => {
                    lexer.token = .t_less_than_equals;
                    lexer.start += 1;
                },
                else => {
                    try lexer.expected(.t_less_than);
                },
            }
        }

        pub fn expectGreaterThan(lexer: *LexerType, comptime is_inside_jsx_element: bool) !void {
            switch (lexer.token) {
                .t_greater_than => {
                    if (is_inside_jsx_element) {
                        try lexer.nextInsideJSXElement();
                    } else {
                        try lexer.next();
                    }
                },

                .t_greater_than_equals => {
                    lexer.token = .t_equals;
                    lexer.start += 1;
                    try lexer.maybeExpandEquals();
                },

                .t_greater_than_greater_than_equals => {
                    lexer.token = .t_greater_than_equals;
                    lexer.start += 1;
                },

                .t_greater_than_greater_than_greater_than_equals => {
                    lexer.token = .t_greater_than_greater_than_equals;
                    lexer.start += 1;
                },

                .t_greater_than_greater_than => {
                    lexer.token = .t_greater_than;
                    lexer.start += 1;
                },

                .t_greater_than_greater_than_greater_than => {
                    lexer.token = .t_greater_than_greater_than;
                    lexer.start += 1;
                },

                else => {
                    try lexer.expected(.t_greater_than);
                },
            }
        }

        inline fn recordNewLine(lexer: *@This()) !void {
            if (comptime record_lines) {
                try lexer.line_map.append(lexer.current, lexer.last_line);
                lexer.last_line = lexer.current;
            }
        }

        fn _next(lexer: *LexerType, comptime keywords: anytype, comptime use_default_keywords_if_same_line: bool) !void {
            lexer.has_newline_before = lexer.end == 0;
            lexer.full_start = @intCast(lexer.end);

            while (true) {
                lexer.start = lexer.end;
                // lexer.token = T.t_end_of_file;

                switch (lexer.code_point) {
                    -1 => {
                        lexer.token = T.t_end_of_file;
                    },

                    '#' => {
                        if (comptime is_json) {
                            return lexer.addUnsupportedSyntaxError("Private identifiers are not allowed in JSON");
                        }
                        if (lexer.start == 0 and lexer.source.contents[1] == '!') {
                            // "#!/usr/bin/env node"
                            lexer.token = .t_hashbang;
                            hashbang: while (true) {
                                lexer.step();
                                switch (lexer.code_point) {
                                    '\r', '\n', 0x2028, 0x2029 => {
                                        break :hashbang;
                                    },
                                    -1 => {
                                        break :hashbang;
                                    },
                                    else => {},
                                }
                            }
                            lexer.identifier = lexer.raw();
                        } else {
                            // "#foo"
                            lexer.step();
                            if (lexer.code_point == '\\') {
                                lexer.identifier = (try lexer.scanIdentifierWithEscapes(.private)).contents;
                            } else {
                                if (!isIdentifierStart(lexer.code_point)) {
                                    try lexer.syntaxError();
                                }

                                lexer.step();
                                while (isIdentifierContinue(lexer.code_point)) {
                                    lexer.step();
                                }
                                if (lexer.code_point == '\\') {
                                    lexer.identifier = (try lexer.scanIdentifierWithEscapes(.private)).contents;
                                } else {
                                    lexer.identifier = lexer.raw();
                                }
                            }
                            lexer.token = T.t_private_identifier;
                            break;
                        }
                    },
                    0x2028, 0x2029 => {
                        try lexer.recordNewLine();
                        lexer.has_newline_before = true;
                        lexer.step();
                        continue;
                    },
                    '\r', '\n' => {
                        try lexer.recordNewLine();
                        lexer.has_newline_before = true;
                        lexer.step();

                        if (lexer.code_point != -1) {
                            lexer.current += skipToInterestingCharacterNextLine(lexer.source.contents[lexer.current..]);
                            lexer.end = lexer.current - 1;
                        }

                        continue;
                    },
                    '\t', ' ' => {
                        lexer.step();
                        continue;
                    },
                    '(' => {
                        lexer.step();
                        lexer.token = T.t_open_paren;
                    },
                    ')' => {
                        lexer.step();
                        lexer.token = T.t_close_paren;
                    },
                    '[' => {
                        lexer.step();
                        lexer.token = T.t_open_bracket;
                    },
                    ']' => {
                        lexer.step();
                        lexer.token = T.t_close_bracket;
                    },
                    '{' => {
                        lexer.step();
                        lexer.token = T.t_open_brace;
                    },
                    '}' => {
                        lexer.step();
                        lexer.token = T.t_close_brace;
                    },
                    ',' => {
                        lexer.step();
                        lexer.token = T.t_comma;
                    },
                    ':' => {
                        lexer.step();
                        lexer.token = T.t_colon;
                    },
                    ';' => {
                        if (comptime is_json) {
                            return lexer.addUnsupportedSyntaxError("Semicolons are not allowed in JSON");
                        }

                        lexer.step();
                        lexer.token = T.t_semicolon;
                    },
                    '@' => {
                        if (comptime is_json) {
                            return lexer.addUnsupportedSyntaxError("Decorators are not allowed in JSON");
                        }

                        lexer.step();
                        lexer.token = T.t_at;
                    },
                    '~' => {
                        if (comptime is_json) {
                            return lexer.addUnsupportedSyntaxError("~ is not allowed in JSON");
                        }

                        lexer.step();
                        lexer.token = T.t_tilde;
                    },
                    '?' => {
                        // '?' or '?.' or '??' or '??='
                        lexer.step();
                        switch (lexer.code_point) {
                            '?' => {
                                lexer.step();
                                switch (lexer.code_point) {
                                    '=' => {
                                        lexer.step();
                                        lexer.token = T.t_question_question_equals;
                                    },
                                    else => {
                                        lexer.token = T.t_question_question;
                                    },
                                }
                            },

                            '.' => {
                                lexer.token = T.t_question;
                                const current = lexer.current;
                                const contents = lexer.source.contents;

                                // Lookahead to disambiguate with 'a?.1:b'
                                if (current < contents.len) {
                                    const c = contents[current];
                                    if (c < '0' or c > '9') {
                                        lexer.step();
                                        lexer.token = T.t_question_dot;
                                    }
                                }
                            },
                            else => {
                                lexer.token = T.t_question;
                            },
                        }
                    },
                    '%' => {
                        if (comptime is_json) {
                            return lexer.addUnsupportedSyntaxError("Operators are not allowed in JSON");
                        }

                        // '%' or '%='
                        lexer.step();
                        switch (lexer.code_point) {
                            '=' => {
                                lexer.step();
                                lexer.token = T.t_percent_equals;
                            },

                            else => {
                                lexer.token = T.t_percent;
                            },
                        }
                    },

                    '&' => {
                        if (comptime is_json) {
                            return lexer.addUnsupportedSyntaxError("Operators are not allowed in JSON");
                        }

                        // '&' or '&=' or '&&' or '&&='
                        lexer.step();
                        switch (lexer.code_point) {
                            '=' => {
                                lexer.step();
                                lexer.token = T.t_ampersand_equals;
                            },

                            '&' => {
                                lexer.step();
                                switch (lexer.code_point) {
                                    '=' => {
                                        lexer.step();
                                        lexer.token = T.t_ampersand_ampersand_equals;
                                    },

                                    else => {
                                        lexer.token = T.t_ampersand_ampersand;
                                    },
                                }
                            },
                            else => {
                                lexer.token = T.t_ampersand;
                            },
                        }
                    },

                    '|' => {
                        if (comptime is_json) {
                            return lexer.addUnsupportedSyntaxError("Operators are not allowed in JSON");
                        }

                        // '|' or '|=' or '||' or '||='
                        lexer.step();
                        switch (lexer.code_point) {
                            '=' => {
                                lexer.step();
                                lexer.token = T.t_bar_equals;
                            },
                            '|' => {
                                lexer.step();
                                switch (lexer.code_point) {
                                    '=' => {
                                        lexer.step();
                                        lexer.token = T.t_bar_bar_equals;
                                    },

                                    else => {
                                        lexer.token = T.t_bar_bar;
                                    },
                                }
                            },
                            else => {
                                lexer.token = T.t_bar;
                            },
                        }
                    },

                    '^' => {
                        if (comptime is_json) {
                            return lexer.addUnsupportedSyntaxError("Operators are not allowed in JSON");
                        }

                        // '^' or '^='
                        lexer.step();
                        switch (lexer.code_point) {
                            '=' => {
                                lexer.step();
                                lexer.token = T.t_caret_equals;
                            },

                            else => {
                                lexer.token = T.t_caret;
                            },
                        }
                    },

                    '+' => {
                        if (comptime is_json) {
                            return lexer.addUnsupportedSyntaxError("Operators are not allowed in JSON");
                        }

                        // '+' or '+=' or '++'
                        lexer.step();
                        switch (lexer.code_point) {
                            '=' => {
                                lexer.step();
                                lexer.token = T.t_plus_equals;
                            },

                            '+' => {
                                lexer.step();
                                lexer.token = T.t_plus_plus;
                            },

                            else => {
                                lexer.token = T.t_plus;
                            },
                        }
                    },

                    '-' => {

                        // '+' or '+=' or '++'
                        lexer.step();
                        switch (lexer.code_point) {
                            '=' => {
                                if (comptime is_json) {
                                    return lexer.addUnsupportedSyntaxError("Operators are not allowed in JSON");
                                }
                                lexer.step();
                                lexer.token = T.t_minus_equals;
                            },

                            '-' => {
                                if (comptime is_json) {
                                    return lexer.addUnsupportedSyntaxError("Operators are not allowed in JSON");
                                }
                                lexer.step();
                                lexer.token = T.t_minus_minus;
                            },

                            else => {
                                lexer.token = T.t_minus;
                            },
                        }
                    },

                    '*' => {
                        // '*' or '*=' or '**' or '**='

                        lexer.step();
                        switch (lexer.code_point) {
                            '=' => {
                                lexer.step();
                                lexer.token = .t_asterisk_equals;
                            },
                            '*' => {
                                lexer.step();
                                switch (lexer.code_point) {
                                    '=' => {
                                        lexer.step();
                                        lexer.token = .t_asterisk_asterisk_equals;
                                    },
                                    else => {
                                        lexer.token = .t_asterisk_asterisk;
                                    },
                                }
                            },
                            else => {
                                lexer.token = .t_asterisk;
                            },
                        }
                    },
                    '/' => {

                        // '/' or '/=' or '//' or '/* ... */'
                        lexer.step();

                        switch (lexer.code_point) {
                            '=' => {
                                lexer.step();
                                lexer.token = .t_slash_equals;
                            },
                            '/' => {
                                singleLineComment: while (true) {
                                    lexer.step();
                                    switch (lexer.code_point) {
                                        '\r', '\n', 0x2028, 0x2029 => {
                                            break :singleLineComment;
                                        },
                                        -1 => {
                                            break :singleLineComment;
                                        },
                                        else => {},
                                    }
                                }

                                if (comptime is_json) {
                                    if (!json.allow_comments) {
                                        try lexer.addCurrentRangeError("JSON does not support comments", .{});
                                        return;
                                    }
                                }

                                if (!lexer.pause_on_comments) continue;
                                lexer.token = .t_single_line_comment;
                                // lexer.scanCommentText();
                                // continue;
                            },
                            '*' => {
                                lexer.step();

                                multiLineComment: while (true) {
                                    switch (lexer.code_point) {
                                        '*' => {
                                            lexer.step();
                                            if (lexer.code_point == '/') {
                                                lexer.step();
                                                break :multiLineComment;
                                            }
                                        },
                                        '\r', '\n', 0x2028, 0x2029 => {
                                            try lexer.recordNewLine();
                                            lexer.has_newline_before = true;
                                            lexer.step();
                                        },
                                        -1 => {
                                            lexer.start = lexer.end;
                                            try lexer.addSyntaxError(
                                                lexer.start,
                                                "Expected \"*/\" to terminate multi-line comment",
                                                .{},
                                            );
                                        },
                                        else => {
                                            // if (comptime Environment.enableSIMD) {
                                            // TODO: this seems to work, but we shouldn't enable this until after improving test coverage
                                            // if (lexer.code_point < 128) {
                                            //     const remainder = lexer.source.contents[lexer.current..];
                                            //     if (remainder.len >= 4096) {
                                            //         lexer.current += skipToInterestingCharacterInMultilineComment(remainder) orelse {
                                            //             lexer.step();
                                            //             continue;
                                            //         };
                                            //         lexer.end = lexer.current -| 1;
                                            //         lexer.step();
                                            //         continue;
                                            //     }
                                            // }
                                            // }

                                            lexer.step();
                                        },
                                    }
                                }
                                if (comptime is_json) {
                                    if (!json.allow_comments) {
                                        try lexer.addCurrentRangeError("JSON does not support comments", .{});
                                        return;
                                    }
                                }

                                if (!lexer.pause_on_comments) continue;
                                lexer.token = .t_multi_line_comment;
                            },
                            else => {
                                lexer.token = .t_slash;
                            },
                        }
                    },

                    '=' => {
                        if (comptime is_json) {
                            return lexer.addUnsupportedSyntaxError("Operators are not allowed in JSON");
                        }

                        // '=' or '=>' or '==' or '==='
                        lexer.step();
                        switch (lexer.code_point) {
                            '>' => {
                                lexer.step();
                                lexer.token = T.t_equals_greater_than;
                            },

                            '=' => {
                                lexer.step();
                                switch (lexer.code_point) {
                                    '=' => {
                                        lexer.step();
                                        lexer.token = T.t_equals_equals_equals;
                                    },

                                    else => {
                                        lexer.token = T.t_equals_equals;
                                    },
                                }
                            },

                            else => {
                                lexer.token = T.t_equals;
                            },
                        }
                    },

                    '<' => {
                        if (comptime is_json) {
                            return lexer.addUnsupportedSyntaxError("Operators are not allowed in JSON");
                        }

                        // '<' or '<<' or '<=' or '<<='
                        lexer.step();
                        switch (lexer.code_point) {
                            '=' => {
                                lexer.step();
                                lexer.token = T.t_less_than_equals;
                            },

                            '<' => {
                                lexer.step();
                                switch (lexer.code_point) {
                                    '=' => {
                                        lexer.step();
                                        lexer.token = T.t_less_than_less_than_equals;
                                    },

                                    else => {
                                        lexer.token = T.t_less_than_less_than;
                                    },
                                }
                            },
                            else => {
                                lexer.token = T.t_less_than;
                            },
                        }
                    },

                    '>' => {
                        if (comptime is_json) {
                            return lexer.addUnsupportedSyntaxError("Operators are not allowed in JSON");
                        }

                        // '>' or '>>' or '>>>' or '>=' or '>>=' or '>>>='
                        lexer.step();

                        switch (lexer.code_point) {
                            '=' => {
                                lexer.step();
                                lexer.token = T.t_greater_than_equals;
                            },
                            '>' => {
                                lexer.step();
                                switch (lexer.code_point) {
                                    '=' => {
                                        lexer.step();
                                        lexer.token = T.t_greater_than_greater_than_equals;
                                    },
                                    '>' => {
                                        lexer.step();
                                        switch (lexer.code_point) {
                                            '=' => {
                                                lexer.step();
                                                lexer.token = T.t_greater_than_greater_than_greater_than_equals;
                                            },
                                            else => {
                                                lexer.token = T.t_greater_than_greater_than_greater_than;
                                            },
                                        }
                                    },
                                    else => {
                                        lexer.token = T.t_greater_than_greater_than;
                                    },
                                }
                            },
                            else => {
                                lexer.token = T.t_greater_than;
                            },
                        }
                    },

                    '!' => {
                        if (comptime is_json) {
                            return lexer.addUnsupportedSyntaxError("Operators are not allowed in JSON");
                        }

                        // '!' or '!=' or '!=='
                        lexer.step();
                        switch (lexer.code_point) {
                            '=' => {
                                lexer.step();
                                switch (lexer.code_point) {
                                    '=' => {
                                        lexer.step();
                                        lexer.token = T.t_exclamation_equals_equals;
                                    },

                                    else => {
                                        lexer.token = T.t_exclamation_equals;
                                    },
                                }
                            },
                            else => {
                                lexer.token = T.t_exclamation;
                            },
                        }
                    },

                    '\'' => {
                        try lexer.parseStringLiteral('\'');
                    },
                    '"' => {
                        try lexer.parseStringLiteral('"');
                    },
                    '`' => {
                        try lexer.parseStringLiteral('`');
                    },

                    '_', '$', 'a'...'z', 'A'...'Z' => {
                        const advance = latin1IdentifierContinueLength(lexer.source.contents[lexer.current..], enable_simd);

                        lexer.current += advance;

                        lexer.step();

                        if (lexer.code_point >= 0x80) {
                            while (isIdentifierContinue(lexer.code_point)) {
                                lexer.step();
                            }
                        }

                        if (lexer.code_point != '\\') {
                            lexer.identifier = lexer.raw();
                            if (comptime @TypeOf(keywords) == @TypeOf(null)) {
                                lexer.token = T.t_identifier;
                            } else {
                                if (use_default_keywords_if_same_line and !lexer.has_newline_before) {
                                    lexer.token = DefaultKeywords.get(lexer.identifier) orelse T.t_identifier;
                                } else {
                                    lexer.token = keywords.get(lexer.identifier) orelse T.t_identifier;
                                }
                            }
                        } else {
                            const scan_result = try lexer.scanIdentifierWithEscapes(.normal);
                            lexer.identifier = scan_result.contents;
                            lexer.token = scan_result.token;
                        }
                    },

                    '\\' => {
                        if (comptime is_json and json_options.ignore_leading_escape_sequences) {
                            if (lexer.start == 0 or lexer.current == lexer.source.contents.len - 1) {
                                lexer.step();
                                continue;
                            }
                        }

                        const scan_result = try lexer.scanIdentifierWithEscapes(.normal);
                        lexer.identifier = scan_result.contents;
                        lexer.token = scan_result.token;
                    },

                    '.' => {
                        try lexer.parseNumericLiteralOrDot('.');
                    },
                    '0' => {
                        try lexer.parseNumericLiteralOrDot('0');
                    },
                    '1'...'9' => {
                        try lexer.parseNumericLiteralOrDot(0);
                    },
                    else => {
                        // Check for unusual whitespace characters
                        if (isWhitespace(lexer.code_point)) {
                            lexer.step();
                            continue;
                        }

                        if (isIdentifierStart(lexer.code_point)) {
                            lexer.step();
                            while (isIdentifierContinue(lexer.code_point)) {
                                lexer.step();
                            }
                            if (lexer.code_point == '\\') {
                                const scan_result = try lexer.scanIdentifierWithEscapes(.normal);
                                lexer.identifier = scan_result.contents;
                                lexer.token = scan_result.token;
                            } else {
                                lexer.token = T.t_identifier;
                                lexer.identifier = lexer.raw();
                            }
                            break;
                        }

                        lexer.end = lexer.current;
                        lexer.token = T.t_syntax_error;
                    },
                }

                return;
            }
        }

        pub inline fn nextNoKeywords(lexer: *LexerType) !void {
            return _next(lexer, null, false);
        }

        pub inline fn nextInClassScope(lexer: *LexerType) !void {
            return _next(lexer, ClassScopeKeywords, true);
        }

        // We don't want to check for new lines on modifier keywords
        pub inline fn nextInClassScopeFlag(lexer: *LexerType) !void {
            return _next(lexer, ClassScopeKeywords, false);
        }

        pub inline fn nextInConstructorParameterList(lexer: *LexerType) !void {
            return _next(lexer, ConstructorParameterKeywords, false);
        }

        pub inline fn nextInInterfaceScope(lexer: *LexerType) !void {
            return _next(lexer, InterfaceScopeKeywords, true);
        }

        pub inline fn nextInInterfaceScopeModifier(lexer: *LexerType) !void {
            return _next(lexer, InterfaceScopeKeywords, false);
        }

        pub inline fn next(lexer: *LexerType) !void {
            return _next(lexer, DefaultKeywords, false);
        }

        pub inline fn nextInTypeLookahead(lexer: *LexerType) !void {
            return _next(lexer, @import("./lexer_tables.zig").FollowTypeLookaheadKeywords, false);
        }

        pub fn expected(self: *LexerType, token: T) !void {
            if (comptime is_debug) {
                if (self.print_expect)
                    std.debug.print("{any} != {any} source [line: {}, pos {d}]:\n   {s}\n", .{ self.token, token, self.line_map.count+1, self.start, self.getContext(25) });
            }
            // std.debug.print("{s} {}\n", .{self.source.name orelse "", self.line_count});

            return error.UnexpectedSyntax;
        }

        pub fn unexpected(lexer: *LexerType) !void {
            const found = finder: {
                lexer.start = @min(lexer.start, lexer.end);

                if (lexer.start == lexer.source.contents.len) {
                    break :finder "end of file";
                } else {
                    break :finder lexer.raw();
                }
            };

            try lexer.addCurrentRangeError("Unexpected {s}", .{found});
        }

        pub inline fn raw(self: *LexerType) []const u8 {
            return self.source.contents[self.start..self.end];
        }

        pub inline fn isContextualKeyword(self: *LexerType, comptime keyword: string) bool {
            return self.token == .t_identifier and strings.eqlComptime(self.raw(), keyword);
        }

        pub fn expectedString(self: *LexerType, text: string) !void {
            if (self.source.contents.len != self.start) {
                try self.addCurrentRangeError(
                    "Expected {s} but found \"{s}\"",
                    .{ text, self.raw() },
                );
            } else {
                try self.addCurrentRangeError(
                    "Expected {s} but found end of file",
                    .{text},
                );
            }
        }

        fn scanCommentText(lexer: *LexerType) void {
            const text = lexer.source.contents[lexer.start..lexer.end];
            const has_legal_annotation = text.len > 2 and text[2] == '!';
            const is_multiline_comment = text.len > 1 and text[1] == '*';

            // Omit the trailing "*/" from the checks below
            const end_comment_text =
                if (is_multiline_comment)
                text.len - 2
            else
                text.len;

            if (has_legal_annotation) {
                // lexer.comments_to_preserve_before.append(js_ast.G.Comment{
                //     .text = text,
                //     .loc = lexer.loc(),
                // }) catch unreachable;
            }

            const rest = text[0..end_comment_text];
            const end = rest.ptr + rest.len;
            _ = end;

            // if (comptime enable_simd) {
            //     const wrapped_len = rest.len - (rest.len % strings.ascii_vector_size);
            //     const comment_end = rest.ptr + wrapped_len;
            //     while (rest.ptr != comment_end) {
            //         const vec: strings.AsciiVector = rest.ptr[0..strings.ascii_vector_size].*;

            //         // lookahead for any # or @ characters
            //         const hashtag = @as(strings.AsciiVectorU1, @bitCast(vec == @as(strings.AsciiVector, @splat(@as(u8, '#')))));
            //         const at = @as(strings.AsciiVectorU1, @bitCast(vec == @as(strings.AsciiVector, @splat(@as(u8, '@')))));

            //         if (@reduce(.Max, hashtag + at) == 1) {
            //             rest.len = @intFromPtr(end) - @intFromPtr(rest.ptr);

            //             for (@as([strings.ascii_vector_size]u8, vec), 0..) |c, i| {
            //                 switch (c) {
            //                     '@', '#' => {
            //                         const chunk = rest[i + 1 ..];
            //                         if (!lexer.has_pure_comment_before) {
            //                             if (strings.hasPrefixWithWordBoundary(chunk, "__PURE__")) {
            //                                 lexer.has_pure_comment_before = true;
            //                                 continue;
            //                             }
            //                             // TODO: implement NO_SIDE_EFFECTS
            //                             // else if (strings.hasPrefixWithWordBoundary(chunk, "__NO_SIDE_EFFECTS__")) {
            //                             //     lexer.has_no_side_effect_comment_before = true;
            //                             //     continue;
            //                             // }
            //                         }

            //                         if (strings.hasPrefixWithWordBoundary(chunk, "jsx")) {
            //                             if (PragmaArg.scan(.skip_space_first, lexer.start + i + 1, "jsx", chunk)) |span| {
            //                                 lexer.jsx_pragma._jsx = span;
            //                             }
            //                         } else if (strings.hasPrefixWithWordBoundary(chunk, "jsxFrag")) {
            //                             if (PragmaArg.scan(.skip_space_first, lexer.start + i + 1, "jsxFrag", chunk)) |span| {
            //                                 lexer.jsx_pragma._jsxFrag = span;
            //                             }
            //                         } else if (strings.hasPrefixWithWordBoundary(chunk, "jsxRuntime")) {
            //                             if (PragmaArg.scan(.skip_space_first, lexer.start + i + 1, "jsxRuntime", chunk)) |span| {
            //                                 lexer.jsx_pragma._jsxRuntime = span;
            //                             }
            //                         } else if (strings.hasPrefixWithWordBoundary(chunk, "jsxImportSource")) {
            //                             if (PragmaArg.scan(.skip_space_first, lexer.start + i + 1, "jsxImportSource", chunk)) |span| {
            //                                 lexer.jsx_pragma._jsxImportSource = span;
            //                             }
            //                         } else if (i == 2 and strings.hasPrefixComptime(chunk, " sourceMappingURL=")) {
            //                             if (PragmaArg.scan(.no_space_first, lexer.start + i + 1, " sourceMappingURL=", chunk)) |span| {
            //                                 lexer.source_mapping_url = span;
            //                             }
            //                         }
            //                     },
            //                     else => {},
            //                 }
            //             }
            //         }

            //         rest.ptr += strings.ascii_vector_size;
            //     }
            //     rest.len = @intFromPtr(end) - @intFromPtr(rest.ptr);
            // }

            // while (rest.len > 0) {
            //     const c = rest[0];
            //     rest = rest[1..];
            //     switch (c) {
            //         '@', '#' => {
            //             const chunk = rest;
            //             const i = @intFromPtr(chunk.ptr) - @intFromPtr(text.ptr);
            //             if (!lexer.has_pure_comment_before) {
            //                 if (strings.hasPrefixWithWordBoundary(chunk, "__PURE__")) {
            //                     lexer.has_pure_comment_before = true;
            //                     continue;
            //                 }
            //             }

            //             if (strings.hasPrefixWithWordBoundary(chunk, "jsx")) {
            //                 if (PragmaArg.scan(.skip_space_first, lexer.start + i + 1, "jsx", chunk)) |span| {
            //                     lexer.jsx_pragma._jsx = span;
            //                 }
            //             } else if (strings.hasPrefixWithWordBoundary(chunk, "jsxFrag")) {
            //                 if (PragmaArg.scan(.skip_space_first, lexer.start + i + 1, "jsxFrag", chunk)) |span| {
            //                     lexer.jsx_pragma._jsxFrag = span;
            //                 }
            //             } else if (strings.hasPrefixWithWordBoundary(chunk, "jsxRuntime")) {
            //                 if (PragmaArg.scan(.skip_space_first, lexer.start + i + 1, "jsxRuntime", chunk)) |span| {
            //                     lexer.jsx_pragma._jsxRuntime = span;
            //                 }
            //             } else if (strings.hasPrefixWithWordBoundary(chunk, "jsxImportSource")) {
            //                 if (PragmaArg.scan(.skip_space_first, lexer.start + i + 1, "jsxImportSource", chunk)) |span| {
            //                     lexer.jsx_pragma._jsxImportSource = span;
            //                 }
            //             } else if (i == 2 and strings.hasPrefixComptime(chunk, " sourceMappingURL=")) {
            //                 if (PragmaArg.scan(.no_space_first, lexer.start + i + 1, " sourceMappingURL=", chunk)) |span| {
            //                     lexer.source_mapping_url = span;
            //                 }
            //             }
            //         },
            //         else => {},
            //     }
            // }
        }

        pub fn initWithoutReading(source: Source, allocator: std.mem.Allocator) LexerType {
            const empty_string_literal: JavascriptString = &emptyJavaScriptString;
            return LexerType{
                .source = source,
                .string_literal = empty_string_literal,
                .string_literal_buffer = std.ArrayList(u16).init(allocator),
                .allocator = allocator,
                .line_map = if (comptime record_lines) LineMap.init() else {},
            };
        }

        pub fn init(source: Source, allocator: std.mem.Allocator) !LexerType {
            var lex = initWithoutReading(source, allocator);
            lex.step();
            try lex.next();

            return lex;
        }

        pub fn scanFirst(lex: *LexerType) !void {
            std.debug.assert(lex.start == 0);
            lex.step();
            try lex.next();
        }

        inline fn assertNotJSON(_: *const LexerType) void {
            if (comptime is_json) @compileError("JSON should not reach this point");
            if (comptime is_json) unreachable;
        }

        pub fn scanRegExp(lexer: *LexerType) !void {
            lexer.assertNotJSON();
            lexer.regex_flags_start = null;
            while (true) {
                switch (lexer.code_point) {
                    '/' => {
                        lexer.step();

                        var has_set_flags_start = false;
                        const flag_characters = "dgimsuvy";
                        const min_flag = comptime std.mem.min(u8, flag_characters);
                        const max_flag = comptime std.mem.max(u8, flag_characters);
                        const RegexpFlags = std.bit_set.IntegerBitSet((max_flag - min_flag) + 1);
                        var flags = RegexpFlags.initEmpty();
                        while (isIdentifierContinue(lexer.code_point)) {
                            switch (lexer.code_point) {
                                'd', 'g', 'i', 'm', 's', 'u', 'y', 'v' => {
                                    if (!has_set_flags_start) {
                                        lexer.regex_flags_start = @as(u16, @truncate(lexer.end - lexer.start));
                                        has_set_flags_start = true;
                                    }
                                    const flag = max_flag - @as(u8, @intCast(lexer.code_point));
                                    if (flags.isSet(flag)) {
                                        lexer.addError(
                                            lexer.current,
                                            "Duplicate flag \"{u}\" in regular expression",
                                            .{@as(u21, @intCast(lexer.code_point))},
                                            false,
                                        );
                                    }
                                    flags.set(flag);

                                    lexer.step();
                                },
                                else => {
                                    lexer.addError(
                                        lexer.current,
                                        "Invalid flag \"{u}\" in regular expression",
                                        .{@as(u21, @intCast(lexer.code_point))},
                                        false,
                                    );

                                    lexer.step();
                                },
                            }
                        }
                        return;
                    },
                    '[' => {
                        lexer.step();
                        while (lexer.code_point != ']') {
                            try lexer.scanRegExpValidateAndStep();
                        }
                        lexer.step();
                    },
                    else => {
                        try lexer.scanRegExpValidateAndStep();
                    },
                }
            }
        }

        pub fn utf16ToString(lexer: *LexerType, js: []const u16) string {
            var temp: [4]u8 = undefined;
            var list = std.ArrayList(u8).initCapacity(lexer.allocator, js.len) catch unreachable;
            var i: usize = 0;
            while (i < js.len) : (i += 1) {
                var r1 = @as(i32, @intCast(js[i]));
                if (r1 >= 0xD800 and r1 <= 0xDBFF and i + 1 < js.len) {
                    const r2 = @as(i32, @intCast(js[i] + 1));
                    if (r2 >= 0xDC00 and r2 <= 0xDFFF) {
                        r1 = (r1 - 0xD800) << 10 | (r2 - 0xDC00) + 0x10000;
                        i += 1;
                    }
                }
                const width = strings.encodeWTF8Rune(&temp, r1);
                list.appendSlice(temp[0..width]) catch unreachable;
            }
            return list.items;
        }

        pub fn nextInsideJSXElement(lexer: *LexerType) !void {
            lexer.assertNotJSON();

            lexer.has_newline_before = false;

            while (true) {
                lexer.start = lexer.end;
                lexer.token = .t_end_of_file;

                switch (lexer.code_point) {
                    -1 => {
                        lexer.token = .t_end_of_file;
                    },
                    '\r', '\n', 0x2028, 0x2029 => {
                        try lexer.recordNewLine();
                        lexer.has_newline_before = true;
                        lexer.step();
                        continue;
                    },
                    '\t', ' ' => {
                        lexer.step();
                        continue;
                    },
                    '.' => {
                        lexer.step();
                        lexer.token = .t_dot;
                    },
                    '=' => {
                        lexer.step();
                        lexer.token = .t_equals;
                    },
                    '{' => {
                        lexer.step();
                        lexer.token = .t_open_brace;
                    },
                    '}' => {
                        lexer.step();
                        lexer.token = .t_close_brace;
                    },
                    '<' => {
                        lexer.step();
                        lexer.token = .t_less_than;
                    },
                    '>' => {
                        lexer.step();
                        lexer.token = .t_greater_than;
                    },
                    '/' => {
                        // '/' or '//' or '/* ... */'

                        lexer.step();
                        switch (lexer.code_point) {
                            '/' => {
                                single_line_comment: {
                                    while (true) {
                                        lexer.step();
                                        switch (lexer.code_point) {
                                            '\r', '\n', 0x2028, 0x2029 => {
                                                break :single_line_comment;
                                            },
                                            -1 => {
                                                break :single_line_comment;
                                            },
                                            else => {},
                                        }
                                    }
                                }
                                continue;
                            },
                            '*' => {
                                lexer.step();
                                multi_line_comment: {
                                    while (true) {
                                        switch (lexer.code_point) {
                                            '*' => {
                                                lexer.step();
                                                if (lexer.code_point == '/') {
                                                    lexer.step();
                                                    break :multi_line_comment;
                                                }
                                            },
                                            '\r', '\n', 0x2028, 0x2029 => {
                                                try lexer.recordNewLine();
                                                lexer.has_newline_before = true;
                                                lexer.step();
                                            },
                                            -1 => {
                                                lexer.start = lexer.end;
                                                try lexer.addSyntaxError(lexer.start, "Expected \"*/\" to terminate multi-line comment", .{});
                                            },
                                            else => {
                                                lexer.step();
                                            },
                                        }
                                    }
                                }
                                continue;
                            },
                            else => {
                                lexer.token = .t_slash;
                            },
                        }
                    },
                    '\'' => {
                        lexer.step();
                        try lexer.parseJSXStringLiteral('\'');
                    },
                    '"' => {
                        lexer.step();
                        try lexer.parseJSXStringLiteral('"');
                    },
                    else => {
                        if (isWhitespace(lexer.code_point)) {
                            lexer.step();
                            continue;
                        }

                        if (isIdentifierStart(lexer.code_point)) {
                            lexer.step();
                            while (isIdentifierContinue(lexer.code_point) or lexer.code_point == '-') {
                                lexer.step();
                            }

                            // Parse JSX namespaces. These are not supported by React or TypeScript
                            // but someone using JSX syntax in more obscure ways may find a use for
                            // them. A namespaced name is just always turned into a string so you
                            // can't use this feature to reference JavaScript identifiers.
                            if (lexer.code_point == ':') {
                                lexer.step();

                                if (isIdentifierStart(lexer.code_point)) {
                                    while (isIdentifierStart(lexer.code_point) or lexer.code_point == '-') {
                                        lexer.step();
                                    }
                                } else {
                                    try lexer.addSyntaxError(lexer.end, "Expected identifier after \"{s}\" in namespaced JSX name", .{lexer.raw()});
                                }
                            }

                            lexer.identifier = lexer.raw();
                            lexer.token = .t_identifier;
                            break;
                        }

                        lexer.end = lexer.current;
                        lexer.token = .t_syntax_error;
                    },
                }

                return;
            }
        }
        pub fn parseJSXStringLiteral(lexer: *LexerType, comptime quote: u8) !void {
            lexer.assertNotJSON();

            //var backslash = logger.Range.None;
            var needs_decode = false;

            string_literal: while (true) {
                switch (lexer.code_point) {
                    -1 => {
                        try lexer.syntaxError();
                    },
                    '&' => {
                        needs_decode = true;
                        lexer.step();
                    },

                    '\\' => {
                        // backslash = logger.Range{ .loc = logger.Loc{
                        //     .start = @as(i32, @intCast(lexer.end)),
                        // }, .len = 1 };
                        lexer.step();

                        // JSX string literals do not support escaping
                        // They're "pre" escaped
                        switch (lexer.code_point) {
                            'u', 0x0C, 0, '\t', std.ascii.control_code.vt, 0x08 => {
                                needs_decode = true;
                            },
                            else => {},
                        }

                        continue;
                    },
                    quote => {
                        // if (backslash.len > 0) {
                        //     backslash.len += 1;
                        //     lexer.previous_backslash_quote_in_jsx = backslash;
                        // }
                        lexer.step();
                        break :string_literal;
                    },

                    else => {
                        // Non-ASCII strings need the slow path
                        if (lexer.code_point >= 0x80) {
                            needs_decode = true;
                        } else if ((comptime is_json) and lexer.code_point < 0x20) {
                            try lexer.syntaxError();
                        }
                        lexer.step();
                    },
                }
                //backslash = logger.Range.None;
            }

            lexer.token = .t_string_literal;
            lexer.string_literal_slice = lexer.source.contents[lexer.start + 1 .. lexer.end - 1];
            lexer.string_literal_is_ascii = !needs_decode;
            lexer.string_literal_buffer.clearRetainingCapacity();
            if (needs_decode) {
                lexer.string_literal_buffer.ensureTotalCapacity(lexer.string_literal_slice.len) catch unreachable;
                try lexer.decodeJSXEntities(lexer.string_literal_slice, &lexer.string_literal_buffer);
                lexer.string_literal = lexer.string_literal_buffer.items;
            }
        }

        pub fn expectJSXElementChild(lexer: *LexerType, token: T) !void {
            lexer.assertNotJSON();

            if (lexer.token != token) {
                try lexer.expected(token);
            }

            try lexer.nextJSXElementChild();
        }

        pub fn nextJSXElementChild(lexer: *LexerType) !void {
            lexer.assertNotJSON();

            lexer.has_newline_before = false;
            const original_start = lexer.end;

            while (true) {
                lexer.start = lexer.end;
                lexer.token = T.t_end_of_file;

                switch (lexer.code_point) {
                    -1 => {
                        lexer.token = .t_end_of_file;
                    },
                    '{' => {
                        lexer.step();
                        lexer.token = .t_open_brace;
                    },
                    '<' => {
                        lexer.step();
                        lexer.token = .t_less_than;
                    },
                    else => {
                        var needs_fixing = false;

                        string_literal: while (true) {
                            switch (lexer.code_point) {
                                -1 => {
                                    try lexer.syntaxError();
                                },
                                '\r', '\n', 0x2028, 0x2029 => {
                                    try lexer.recordNewLine();
                                    needs_fixing = true;
                                    lexer.step();
                                },
                                '&' => {
                                    needs_fixing = true;
                                    lexer.step();
                                },
                                '{', '<' => {
                                    break :string_literal;
                                },
                                else => {
                                    // Non-ASCII strings need the slow path
                                    needs_fixing = needs_fixing or lexer.code_point >= 0x80;
                                    lexer.step();
                                },
                            }
                        }

                        lexer.token = .t_string_literal;
                        lexer.string_literal_slice = lexer.source.contents[original_start..lexer.end];
                        lexer.string_literal_is_ascii = !needs_fixing;
                        if (needs_fixing) {
                            // slow path
                            lexer.string_literal = try fixWhitespaceAndDecodeJSXEntities(lexer, lexer.string_literal_slice);

                            if (lexer.string_literal.len == 0) {
                                lexer.has_newline_before = true;
                                continue;
                            }
                        } else {
                            lexer.string_literal = &([_]u16{});
                        }
                    },
                }

                break;
            }
        }

        threadlocal var jsx_decode_buf: std.ArrayList(u16) = undefined;
        threadlocal var jsx_decode_init = false;
        pub fn fixWhitespaceAndDecodeJSXEntities(lexer: *LexerType, text: []const u8) !JavascriptString {
            lexer.assertNotJSON();

            if (!jsx_decode_init) {
                jsx_decode_init = true;
                jsx_decode_buf = std.ArrayList(u16).init(lexer.allocator);
            }
            jsx_decode_buf.clearRetainingCapacity();

            var decoded = jsx_decode_buf;
            defer jsx_decode_buf = decoded;
            const decoded_ptr = &decoded;

            var after_last_non_whitespace: ?u32 = null;

            // Trim whitespace off the end of the first line
            var first_non_whitespace: ?u32 = 0;

            const iterator = strings.CodepointIterator.init(text);
            var cursor = strings.CodepointIterator.Cursor{};

            while (iterator.next(&cursor)) {
                switch (cursor.c) {
                    '\r', '\n', 0x2028, 0x2029 => {
                        if (first_non_whitespace != null and after_last_non_whitespace != null) {
                            // Newline
                            if (decoded.items.len > 0) {
                                try decoded.append(' ');
                            }

                            // Trim whitespace off the start and end of lines in the middle
                            try lexer.decodeJSXEntities(text[first_non_whitespace.?..after_last_non_whitespace.?], &decoded);
                        }

                        // Reset for the next line
                        first_non_whitespace = null;
                    },
                    '\t', ' ' => {},
                    else => {
                        // Check for unusual whitespace characters
                        if (!isWhitespace(cursor.c)) {
                            after_last_non_whitespace = cursor.i + @as(u32, cursor.width);
                            if (first_non_whitespace == null) {
                                first_non_whitespace = cursor.i;
                            }
                        }
                    },
                }
            }

            if (first_non_whitespace) |start| {
                if (decoded.items.len > 0) {
                    try decoded.append(' ');
                }

                try decodeJSXEntities(lexer, text[start..text.len], decoded_ptr);
            }

            return decoded.items;
        }

        fn maybeDecodeJSXEntity(lexer: *LexerType, text: string, cursor: *strings.CodepointIterator.Cursor) void {
            lexer.assertNotJSON();

            if (strings.indexOfChar(text[cursor.width + cursor.i ..], ';')) |length| {
                const end = cursor.width + cursor.i;
                const entity = text[end .. end + length];
                if (entity[0] == '#') {
                    var number = entity[1..entity.len];
                    var base: u8 = 10;
                    if (number.len > 1 and number[0] == 'x') {
                        number = number[1..number.len];
                        base = 16;
                    }

                    cursor.c = std.fmt.parseInt(i32, number, base) catch |err| brk: {
                        switch (err) {
                            error.InvalidCharacter => {
                                lexer.addError(lexer.start, "Invalid JSX entity escape: {s}", .{entity}, false);
                            },
                            error.Overflow => {
                                lexer.addError(lexer.start, "JSX entity escape is too big: {s}", .{entity}, false);
                            },
                        }

                        break :brk strings.unicode_replacement;
                    };

                    cursor.i += @as(u32, @intCast(length)) + 1;
                    cursor.width = 1;
                } else if (tables.jsxEntity.get(entity)) |ent| {
                    cursor.c = ent;
                    cursor.i += @as(u32, @intCast(length)) + 1;
                }
            }
        }

        pub fn decodeJSXEntities(lexer: *LexerType, text: string, out: *std.ArrayList(u16)) !void {
            lexer.assertNotJSON();

            const iterator = strings.CodepointIterator.init(text);
            var cursor = strings.CodepointIterator.Cursor{};

            while (iterator.next(&cursor)) {
                if (cursor.c == '&') lexer.maybeDecodeJSXEntity(text, &cursor);

                if (cursor.c <= 0xFFFF) {
                    try out.append(@as(u16, @intCast(cursor.c)));
                } else {
                    cursor.c -= 0x10000;
                    try out.ensureUnusedCapacity(2);
                    (out.items.ptr + out.items.len)[0..2].* = [_]u16{
                        @as(
                            u16,
                            @truncate(@as(u32, @bitCast(@as(i32, 0xD800) + ((cursor.c >> 10) & 0x3FF)))),
                        ),
                        @as(
                            u16,
                            @truncate(@as(u32, @bitCast(@as(i32, 0xDC00) + (cursor.c & 0x3FF)))),
                        ),
                    };
                    out.items = out.items.ptr[0 .. out.items.len + 2];
                }
            }
        }
        pub fn expectInsideJSXElement(lexer: *LexerType, token: T) !void {
            lexer.assertNotJSON();

            if (lexer.token != token) {
                try lexer.expected(token);
            }

            try lexer.nextInsideJSXElement();
        }

        fn scanRegExpValidateAndStep(lexer: *LexerType) !void {
            lexer.assertNotJSON();

            if (lexer.code_point == '\\') {
                lexer.step();
            }

            switch (lexer.code_point) {
                '\r', '\n', 0x2028, 0x2029 => {
                    // Newlines aren't allowed in regular expressions
                    try lexer.syntaxError();
                },
                -1 => { // EOF
                    try lexer.syntaxError();
                },
                else => {
                    lexer.step();
                },
            }
        }

        pub fn rescanGreaterThanGreaterThan(lexer: *LexerType) !void {
            lexer.code_point = '>';
            lexer.current = lexer.end;
            lexer.end -= 1;
            try lexer.next();
        }

        pub fn rescanGreaterThanGreaterThanGreaterThan(lexer: *LexerType) !void {
            lexer.code_point = '>';
            lexer.current = lexer.end - 1;
            lexer.end -= 2;
            try lexer.next();
        }

        pub fn rescanCloseBraceAsTemplateToken(lexer: *LexerType) !void {
            lexer.assertNotJSON();

            if (comptime is_debug) {
                if (lexer.token != .t_close_brace) {
                    try lexer.expected(.t_close_brace);
                }
            }

            lexer.rescan_close_brace_as_template_token = true;
            lexer.code_point = '`';
            lexer.current = lexer.end;
            lexer.end -= 1;
            try lexer.next();
            lexer.rescan_close_brace_as_template_token = false;
        }

        fn parseNumericLiteralOrDot(lexer: *LexerType, comptime first: u8) !void {
            lexer.step();

            // Dot without a digit after it;
            if (first == '.' and (lexer.code_point < '0' or lexer.code_point > '9')) {
                // "..."
                if ((lexer.code_point == '.' and
                    lexer.current < lexer.source.contents.len) and
                    lexer.source.contents[lexer.current] == '.')
                {
                    lexer.end = lexer.current;
                    lexer.current += 1;
                    // lexer.step();
                    lexer.step();
                    lexer.token = T.t_dot_dot_dot;
                    return;
                }

                // "."
                lexer.token = T.t_dot;
                return;
            }

            var underscoreCount: usize = 0;
            var lastUnderscoreEnd: usize = 0;
            var hasDotOrExponent = first == '.';
            var base: f32 = 0.0;
            lexer.is_legacy_octal_literal = false;

            // Assume this is a number, but potentially change to a bigint later;
            lexer.token = T.t_numeric_literal;

            // Check for binary, octal, or hexadecimal literal;
            if (comptime first == '0') {
                switch (lexer.code_point) {
                    'b', 'B' => {
                        base = 2;
                    },

                    'o', 'O' => {
                        base = 8;
                    },

                    'x', 'X' => {
                        base = 16;
                    },

                    '0'...'7', '_' => {
                        base = 8;
                        lexer.is_legacy_octal_literal = true;
                    },
                    else => {},
                }
            }

            if (base != 0) {
                // Integer literal;
                var isFirst = true;
                var isInvalidLegacyOctalLiteral = false;
                lexer.number = 0;
                if (!lexer.is_legacy_octal_literal) {
                    lexer.step();
                }

                integerLiteral: while (true) {
                    switch (lexer.code_point) {
                        '_' => {
                            // Cannot have multiple underscores in a row;
                            if (lastUnderscoreEnd > 0 and lexer.end == lastUnderscoreEnd + 1) {
                                try lexer.syntaxError();
                            }

                            // The first digit must exist;
                            if (isFirst or lexer.is_legacy_octal_literal) {
                                try lexer.syntaxError();
                            }

                            lastUnderscoreEnd = lexer.end;
                            underscoreCount += 1;
                        },

                        '0', '1' => {
                            lexer.number = lexer.number * base + float64(lexer.code_point - '0');
                        },

                        '2', '3', '4', '5', '6', '7' => {
                            if (base == 2) {
                                try lexer.syntaxError();
                            }
                            lexer.number = lexer.number * base + float64(lexer.code_point - '0');
                        },
                        '8', '9' => {
                            if (lexer.is_legacy_octal_literal) {
                                isInvalidLegacyOctalLiteral = true;
                            } else if (base < 10) {
                                try lexer.syntaxError();
                            }
                            lexer.number = lexer.number * base + float64(lexer.code_point - '0');
                        },
                        'A', 'B', 'C', 'D', 'E', 'F' => {
                            if (base != 16) {
                                try lexer.syntaxError();
                            }
                            lexer.number = lexer.number * base + float64(lexer.code_point + 10 - 'A');
                        },

                        'a', 'b', 'c', 'd', 'e', 'f' => {
                            if (base != 16) {
                                try lexer.syntaxError();
                            }
                            lexer.number = lexer.number * base + float64(lexer.code_point + 10 - 'a');
                        },
                        else => {
                            // The first digit must exist;
                            if (isFirst) {
                                try lexer.syntaxError();
                            }

                            break :integerLiteral;
                        },
                    }

                    lexer.step();
                    isFirst = false;
                }

                const isBigIntegerLiteral = lexer.code_point == 'n' and !hasDotOrExponent;

                // Slow path: do we need to re-scan the input as text?
                if (isBigIntegerLiteral or isInvalidLegacyOctalLiteral) {
                    const text = lexer.raw();

                    // Can't use a leading zero for bigint literals;
                    if (isBigIntegerLiteral and lexer.is_legacy_octal_literal) {
                        try lexer.syntaxError();
                    }

                    // Filter out underscores;
                    if (underscoreCount > 0) {
                        var bytes = lexer.allocator.alloc(u8, text.len - underscoreCount) catch unreachable;
                        var i: usize = 0;
                        for (text) |char| {
                            if (char != '_') {
                                bytes[i] = char;
                                i += 1;
                            }
                        }
                    }

                    // Store bigints as text to avoid precision loss;
                    if (isBigIntegerLiteral) {
                        lexer.identifier = text;
                    } else if (isInvalidLegacyOctalLiteral) {
                        if (std.fmt.parseFloat(f64, text)) |num| {
                            lexer.number = num;
                        } else |_| {
                            try lexer.addSyntaxError(lexer.start, "Invalid number {s}", .{text});
                        }
                    }
                }
            } else {
                // Floating-point literal;
                const isInvalidLegacyOctalLiteral = first == '0' and (lexer.code_point == '8' or lexer.code_point == '9');

                // Initial digits;
                while (true) {
                    if (lexer.code_point < '0' or lexer.code_point > '9') {
                        if (lexer.code_point != '_') {
                            break;
                        }

                        // Cannot have multiple underscores in a row;
                        if (lastUnderscoreEnd > 0 and lexer.end == lastUnderscoreEnd + 1) {
                            try lexer.syntaxError();
                        }

                        // The specification forbids underscores in this case;
                        if (isInvalidLegacyOctalLiteral) {
                            try lexer.syntaxError();
                        }

                        lastUnderscoreEnd = lexer.end;
                        underscoreCount += 1;
                    }
                    lexer.step();
                }

                // Fractional digits;
                if (first != '.' and lexer.code_point == '.') {
                    // An underscore must not come last;
                    if (lastUnderscoreEnd > 0 and lexer.end == lastUnderscoreEnd + 1) {
                        lexer.end -= 1;
                        try lexer.syntaxError();
                    }

                    hasDotOrExponent = true;
                    lexer.step();
                    if (lexer.code_point == '_') {
                        try lexer.syntaxError();
                    }
                    while (true) {
                        if (lexer.code_point < '0' or lexer.code_point > '9') {
                            if (lexer.code_point != '_') {
                                break;
                            }

                            // Cannot have multiple underscores in a row;
                            if (lastUnderscoreEnd > 0 and lexer.end == lastUnderscoreEnd + 1) {
                                try lexer.syntaxError();
                            }

                            lastUnderscoreEnd = lexer.end;
                            underscoreCount += 1;
                        }
                        lexer.step();
                    }
                }

                // Exponent;
                if (lexer.code_point == 'e' or lexer.code_point == 'E') {
                    // An underscore must not come last;
                    if (lastUnderscoreEnd > 0 and lexer.end == lastUnderscoreEnd + 1) {
                        lexer.end -= 1;
                        try lexer.syntaxError();
                    }

                    hasDotOrExponent = true;
                    lexer.step();
                    if (lexer.code_point == '+' or lexer.code_point == '-') {
                        lexer.step();
                    }
                    if (lexer.code_point < '0' or lexer.code_point > '9') {
                        try lexer.syntaxError();
                    }
                    while (true) {
                        if (lexer.code_point < '0' or lexer.code_point > '9') {
                            if (lexer.code_point != '_') {
                                break;
                            }

                            // Cannot have multiple underscores in a row;
                            if (lastUnderscoreEnd > 0 and lexer.end == lastUnderscoreEnd + 1) {
                                try lexer.syntaxError();
                            }

                            lastUnderscoreEnd = lexer.end;
                            underscoreCount += 1;
                        }
                        lexer.step();
                    }
                }

                // Take a slice of the text to parse;
                var text = lexer.raw();

                // Filter out underscores;
                if (underscoreCount > 0) {
                    var i: usize = 0;
                    if (lexer.allocator.alloc(u8, text.len - underscoreCount)) |bytes| {
                        for (text) |char| {
                            if (char != '_') {
                                bytes[i] = char;
                                i += 1;
                            }
                        }
                        text = bytes;
                    } else |_| {
                        try lexer.addSyntaxError(lexer.start, "Out of Memory", .{});
                        return;
                    }
                }

                if (!hasDotOrExponent and lexer.code_point == 'n') {
                    // The only bigint literal that can start with 0 is "0n"
                    if (text.len > 1 and first == '0') {
                        try lexer.syntaxError();
                    }

                    // Store bigints as text to avoid precision loss;
                    lexer.identifier = text;
                } else if (!hasDotOrExponent and lexer.end - lexer.start < 10) {
                    // Parse a 32-bit integer (very fast path);
                    var number: u32 = 0;
                    for (text) |c| {
                        number = number * 10 + @as(u32, @intCast(c - '0'));
                    }
                    lexer.number = @as(f64, @floatFromInt(number));
                } else {
                    // Parse a double-precision floating-point number
                    if (std.fmt.parseFloat(f64, text)) |num| {
                        lexer.number = num;
                    } else |_| {
                        try lexer.addSyntaxError(lexer.start, "Invalid number", .{});
                    }
                }
            }

            // An underscore must not come last;
            if (lastUnderscoreEnd > 0 and lexer.end == lastUnderscoreEnd + 1) {
                lexer.end -= 1;
                try lexer.syntaxError();
            }

            // Handle bigint literals after the underscore-at-end check above;
            if (!hasDotOrExponent and lexer.code_point == 'n') {
                lexer.token = T.t_big_integer_literal;
                lexer.step();
            }

            // Identifiers can't occur immediately after numbers;
            if (isIdentifierStart(lexer.code_point)) {
                try lexer.syntaxError();
            }
        }
    };
}

pub const Lexer = NewLexer(true, .{});

const JSIdentifier = @import("identifier.zig");
pub inline fn isIdentifierStart(codepoint: i32) bool {
    return JSIdentifier.Bitset.isIdentifierStart(codepoint);
}
pub inline fn isIdentifierContinue(codepoint: i32) bool {
    return JSIdentifier.Bitset.isIdentifierPart(codepoint);
}

pub fn isWhitespace(codepoint: CodePoint) bool {
    return switch (codepoint) {
        0x000B, // line tabulation
        0x0009, // character tabulation
        0x000C, // form feed
        0x0020, // space
        0x00A0, // no-break space
        // Unicode "Space_Separator" code points
        0x1680, // ogham space mark
        0x2000, // en quad
        0x2001, // em quad
        0x2002, // en space
        0x2003, // em space
        0x2004, // three-per-em space
        0x2005, // four-per-em space
        0x2006, // six-per-em space
        0x2007, // figure space
        0x2008, // punctuation space
        0x2009, // thin space
        0x200A, // hair space
        0x202F, // narrow no-break space
        0x205F, // medium mathematical space
        0x3000, // ideographic space
        0xFEFF, // zero width non-breaking space
        => true,
        else => false,
    };
}

pub fn isIdentifier(text: string) bool {
    if (text.len == 0) {
        return false;
    }

    const iter = strings.CodepointIterator{ .bytes = text, .i = 0 };
    var cursor = strings.CodepointIterator.Cursor{};
    if (!iter.next(&cursor)) return false;

    if (!isIdentifierStart(cursor.c)) {
        return false;
    }

    while (iter.next(&cursor)) {
        if (!isIdentifierContinue(cursor.c)) {
            return false;
        }
    }

    return true;
}

inline fn float64(num: anytype) f64 {
    return @as(f64, @floatFromInt(num));
}

pub fn isLatin1Identifier(comptime Buffer: type, name: Buffer) bool {
    if (name.len == 0) return false;

    switch (name[0]) {
        'a'...'z',
        'A'...'Z',
        '$',
        '_',
        => {},
        else => return false,
    }

    if (name.len > 1) {
        for (name[1..]) |c| {
            switch (c) {
                '0'...'9',
                'a'...'z',
                'A'...'Z',
                '$',
                '_',
                => {},
                else => return false,
            }
        }
    }

    return true;
}

inline fn latin1IdentifierContinueLength(name: []const u8, comptime enable_simd: bool) usize {
    var remaining = name;
    const wrap_len = 16;
    const len_wrapped: usize = if (comptime enable_simd) remaining.len - (remaining.len % wrap_len) else 0;
    var wrapped = name[0..len_wrapped];
    remaining = name[wrapped.len..];

    if (comptime enable_simd) {
        // This is not meaningfully faster on aarch64.
        // Earlier attempt: https://zig.godbolt.org/z/j5G8M9ooG
        // Later: https://zig.godbolt.org/z/7Yzh7df9v
        const Vec = @Vector(wrap_len, u8);

        while (wrapped.len > 0) : (wrapped = wrapped[wrap_len..]) {
            var other: [wrap_len]u8 = undefined;
            const vec: [wrap_len]u8 = wrapped[0..wrap_len].*;
            for (vec, &other) |c, *dest| {
                dest.* = switch (c) {
                    '0'...'9',
                    'a'...'z',
                    'A'...'Z',
                    '$',
                    '_',
                    => 0,
                    else => 1,
                };
            }

            if (std.simd.firstIndexOfValue(@as(Vec, @bitCast(other)), 1)) |first| {
                return @as(usize, first) +
                    @intFromPtr(wrapped.ptr) - @intFromPtr(name.ptr);
            }
        }
    }

    for (remaining, 0..) |c, len| {
        switch (c) {
            '0'...'9',
            'a'...'z',
            'A'...'Z',
            '$',
            '_',
            => {},
            else => return len + len_wrapped,
        }
    }

    return name.len;
}

inline fn skipToInterestingCharacterNextLine(text_: []const u8) u32 {
    var text = text_;
    const space: @Vector(strings.ascii_vector_size, u8) = @splat(@as(u8, ' '));
    const tab: @Vector(strings.ascii_vector_size, u8) = @splat(@as(u8, '\t'));
    const V1x16 = strings.AsciiVectorU1;

    const text_end_len = text.len & ~(@as(usize, strings.ascii_vector_size) - 1);

    const text_end_ptr = text.ptr + text_end_len;

    while (text_end_ptr != text.ptr) {
        const vec: strings.AsciiVector = text.ptr[0..strings.ascii_vector_size].*;

        const any_significant =
            @as(V1x16, @bitCast(space != vec)) &
            @as(V1x16, @bitCast(tab != vec));

        if (@reduce(.Max, any_significant) > 0) {
            const bitmask = @as(u16, @bitCast(any_significant));
            const first = @ctz(bitmask);
            return @as(u32, @truncate(first + (@intFromPtr(text.ptr) - @intFromPtr(text_.ptr))));
        }
        text.ptr += strings.ascii_vector_size;
    }

    return @as(u32, @truncate(@intFromPtr(text.ptr) - @intFromPtr(text_.ptr)));
}

fn skipToInterestingCharacterInMultilineComment(text_: []const u8) ?u32 {
    var text = text_;
    const star: @Vector(strings.ascii_vector_size, u8) = @splat(@as(u8, '*'));
    const carriage: @Vector(strings.ascii_vector_size, u8) = @splat(@as(u8, '\r'));
    const newline: @Vector(strings.ascii_vector_size, u8) = @splat(@as(u8, '\n'));
    const V1x16 = strings.AsciiVectorU1;

    const text_end_len = text.len & ~(@as(usize, strings.ascii_vector_size) - 1);

    const text_end_ptr = text.ptr + text_end_len;

    while (text_end_ptr != text.ptr) {
        const vec: strings.AsciiVector = text.ptr[0..strings.ascii_vector_size].*;

        const any_significant =
            @as(V1x16, @bitCast(vec > strings.max_16_ascii)) |
            @as(V1x16, @bitCast(star == vec)) |
            @as(V1x16, @bitCast(carriage == vec)) |
            @as(V1x16, @bitCast(newline == vec));

        if (@reduce(.Max, any_significant) > 0) {
            const bitmask = @as(u16, @bitCast(any_significant));
            const first = @ctz(bitmask);
            return @as(u32, @truncate(first + (@intFromPtr(text.ptr) - @intFromPtr(text_.ptr))));
        }
        text.ptr += strings.ascii_vector_size;
    }

    return @as(u32, @truncate(@intFromPtr(text.ptr) - @intFromPtr(text_.ptr)));
}

inline fn indexOfInterestingCharacterInStringLiteral(text_: []const u8, comptime quote: u8) ?usize {
    var text = text_;
    const quote_: @Vector(strings.ascii_vector_size, u8) = @splat(@as(u8, quote));
    const backslash: @Vector(strings.ascii_vector_size, u8) = @splat(@as(u8, '\\'));
    const V1x16 = strings.AsciiVectorU1;

    while (text.len >= strings.ascii_vector_size) {
        const vec: strings.AsciiVector = text[0..strings.ascii_vector_size].*;

        const any_significant =
            @as(V1x16, @bitCast(vec > strings.max_16_ascii)) |
            @as(V1x16, @bitCast(vec < strings.min_16_ascii)) |
            @as(V1x16, @bitCast(quote_ == vec)) |
            @as(V1x16, @bitCast(backslash == vec));

        if (@reduce(.Max, any_significant) > 0) {
            const bitmask = @as(u16, @bitCast(any_significant));
            const first = @ctz(bitmask);
            return first + (@intFromPtr(text.ptr) - @intFromPtr(text_.ptr));
        }
        text = text[strings.ascii_vector_size..];
    }

    return null;
}

inline fn encodeVlq(val: u32) []const u8 {
    if (val < 128) {
        return &.{@intCast(val)};
    } else if (val < 16384) {
        return &.{
            @intCast(128 | ((val >> 7) & 0x7F)),
            @intCast(val & 0x7F),
        };
    } else if (val < 2097152) {
        return &.{
            @intCast(128 | ((val >> 14) & 0x7F)),
            @intCast(128 | ((val >> 7) & 0x7F)),
            @intCast(val & 0x7F),
        };
    }

    std.debug.assert(val < 268435456);

    return &.{
        @intCast(128 | ((val >> 21) & 0x7F)),
        @intCast(128 | ((val >> 14) & 0x7F)),
        @intCast(128 | ((val >> 7) & 0x7F)),
        @intCast(val & 0x7F),
    };
}

pub const LineMap = struct {
    const BumpAllocator = SizedBumpAllocator(4096, u8);
    const max_offset = @sizeOf(u32);

    positions: BumpAllocator,

    count: u32 = 0,

    pub fn init() @This() {
        var positions = BumpAllocator.init(getAllocator());
        positions.warmup() catch unreachable;

        return .{ .positions = positions };
    }

    pub fn deinit(this: *@This()) void {
        this.positions.deinit();
    }

    // we assume 4096:1 ratio for bytes of source to number of pages
    pub fn approximateCapacityForSize(this: *@This(), size: usize) !void {
        const num_pages = size >> 12;
        try this.positions.pages.ensureTotalCapacity(this.positions.allocator, num_pages+1);
    }

    inline fn _appendSlice(this: *@This(), slice: []const u8) !void {
        std.debug.assert(slice.len > 0 and slice.len <= 4);

        for (slice) |v| try this.positions.append(v);
    }

    pub fn append(this: *@This(), start_: usize, prev_: usize) !void {
        const start: u32 = @intCast(start_);
        const delta = start - @as(u32, @intCast(prev_));
        this.count += 1;

        try this._appendSlice(encodeVlq(delta));
    }

    pub fn decode(this: *const @This()) ![]const u32 {
        return Decoder.decode(this.positions, this.count);
    }

    pub const Decoder = struct {
        positions: BumpAllocator,
        count: u32,
        cursor: u32 = 0,
        prev: u32 = 0,

        inline fn readByte(this: *@This()) u8 {
            const byte = this.positions.at(this.cursor).*;
            this.cursor += 1;
            return byte;
        }

        inline fn readVlq(this: *@This()) u32 {
            var val: u32 = 0;
            while (true) {
                const byte = this.readByte();
                const has_more = byte & 128 == 128;
                val |= byte & 0x7F;

                if (!has_more) break;

                val = val << 7;
            }

            return val;
        }

        pub fn next(this: *@This()) ?u32 {
            if (this.count == 0) {
                return null;
            }

            const start = this.prev + this.readVlq();
            this.prev = start;
            this.count -= 1;

            return start;
        }

        pub fn init(positions: BumpAllocator, count: u32) @This() {
            return .{
                .positions = positions,
                .count = count,
            };
        }

        pub fn decode(positions: BumpAllocator, count: u32) ![]const u32 {
            // We start the decoded map at 0 for better compat w/ `typescript`
            var i: u32 = 1;
            var list = try getAllocator().alloc(u32, count + 1);
            list[0] = 0;

            var iter = @This().init(positions, count);
            while (iter.next()) |t| {
                list[i] = t;
                i += 1;
            }
            return list;
        }
    };
};

fn SizedBumpAllocator(comptime items_per_page: u16, comptime U: type) type {
    return struct {
        local_count: u16 = 0,
        pages: std.ArrayListUnmanaged([]U),
        allocator: std.mem.Allocator,

        pub fn init(allocator: std.mem.Allocator) @This() {
            return .{
                .pages = .{},
                .allocator = allocator,
            };
        }

        pub fn deinit(this: *@This()) void {
            for (this.pages.items) |p| {
                this.allocator.free(p);
            }
            this.pages.clearAndFree(this.allocator);
        }

        inline fn addPage(this: *@This()) !void {
            std.debug.assert((this.count() / items_per_page) >= this.pages.items.len);
            const page = try this.allocator.alloc(U, items_per_page);
            try this.pages.append(this.allocator, page);
        }

        inline fn warmup(this: *@This()) !void {
            std.debug.assert(this.pages.items.len == 0);
            const page = try this.allocator.alloc(U, items_per_page);
            try this.pages.append(this.allocator, page);
        }

        pub inline fn at(this: *const @This(), index: usize) *U {
            std.debug.assert(index < this.count());

            const page = index / items_per_page;
            const offset = @rem(index, items_per_page);

            return &this.pages.items[page][offset];
        }

        pub inline fn append(this: *@This(), val: U) !void {
            this.pages.items[this.pages.items.len - 1][this.local_count] = val;
            this.local_count += 1;

            if (this.local_count == items_per_page) {
                try this.addPage();
                this.local_count = 0;
            }
        }

        pub inline fn count(this: *const @This()) u32 {
            if (this.pages.items.len == 0) return 0;
            return (@as(u32, @truncate(this.pages.items.len - 1)) * items_per_page) + this.local_count;
        }
    };
}

pub const PositionsWriter = struct {
    const BumpAllocator = SizedBumpAllocator(2048, Position);

    positions: BumpAllocator,

    pub fn init() @This() {
        var positions = BumpAllocator.init(getAllocator());
        positions.warmup() catch unreachable;

        return .{ .positions = positions };
    }

    pub inline fn append(this: *@This(), full_start: u32, width: u32) !void {
        try this.positions.append(.{
            .full_start = full_start,
            .width = width,
        });
    }

    const Position = struct { full_start: u32, width: u32 };

    pub fn decode(this: *const @This()) ![]const Position {
        return Decoder.decode(this.positions, this.positions.count());
    }

    pub const Decoder = struct {
        positions: BumpAllocator,
        total: u32,
        count: u32,
        cursor: u32 = 0,
        prev: u32 = 0,

        inline fn readByte(this: *@This()) u8 {
            const byte = this.positions.at(this.cursor).*;
            this.cursor += 1;
            return byte;
        }

        inline fn readVlq(this: *@This()) u32 {
            var val: u32 = 0;
            while (true) {
                const byte = this.readByte();
                const has_more = byte & 128 == 128;
                val |= byte & 0x7F;

                if (!has_more) break;

                val = val << 7;
            }

            return val;
        }

        pub fn next(this: *@This()) ?Position {
            if (this.count == 0) {
                return null;
            }

            const p = this.positions.at(this.total - this.count).*;
            this.count -= 1;

            return p;
        }

        pub fn init(positions: BumpAllocator, count: u32) @This() {
            return .{
                .positions = positions,
                .count = count,
                .total = count,
            };
        }

        pub fn decode(positions: BumpAllocator, count: u32) ![]const Position {
            var i: u32 = 0;
            var list = try getAllocator().alloc(Position, count);

            var iter = @This().init(positions, count);
            while (iter.next()) |t| {
                list[i] = t;
                i += 1;
            }
            return list;
        }
    };
};

const getAllocator = @import("./string_immutable.zig").getAllocator;
