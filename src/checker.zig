const std = @import("std");
const parser = @import("./parser.zig");
const program = @import("./program.zig");

const Kind = program.Analyzer.Kind;
const TypeRef = program.Analyzer.TypeRef;
const NodeRef = parser.NodeRef;

const getSlice2 = program.Analyzer.getSlice2;
const getPackedData = parser.getPackedData;
const getSlice = parser.getSlice;

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

    pub fn checkVariableDeclaration(this: *@This(), decl_ref: NodeRef, init_type: TypeRef) !void {
        const decl = this.file.ast.nodes.at(decl_ref);
        std.debug.assert(decl.kind == .variable_declaration);

        const annotation_ref = decl.len;
        if (annotation_ref == 0) return;

        // no init, TODO: emit error if binding is const, regardless of annotation (could do this in parser)
        const d = getPackedData(decl);
        if (d.right == 0) return;

        const declared_type = try this.analyzer.getType(this.file, annotation_ref);

        if (!try this.analyzer.isAssignableTo(init_type, declared_type)) {
            const init_str = try this.analyzer.printType(init_type);
            const decl_str = try this.analyzer.printType(declared_type);
            try this.file.emitErrorFmt(d.left, "Type '{s}' is not assignable to type '{s}'", .{ init_str, decl_str });
        }
    }

    pub fn checkReadonlyAssignment(this: *@This(), node_ref: NodeRef) !void {
        const node = this.file.ast.nodes.at(node_ref);

        const target_ref = switch (node.kind) {
            .postfix_unary_expression => getPackedData(node).left,
            .prefix_unary_expression => getPackedData(node).right,
            .binary_expression => blk: {
                const op = @as(parser.SyntaxKind, @enumFromInt(node.len));
                switch (op) {
                    .equals_token,
                    .plus_equals_token,
                    .minus_equals_token,
                    .asterisk_equals_token,
                    .slash_equals_token,
                    => break :blk getPackedData(node).left,
                    else => return,
                }
            },
            else => return,
        };

        const target = this.file.ast.nodes.at(target_ref);
        if (target.kind != .property_access_expression) return;

        const d = getPackedData(target);
        const object_type = try this.analyzer.getType(this.file, d.left);
        const resolved = try this.analyzer.evaluateType(object_type, 1 << 0);

        if (resolved >= @intFromEnum(Kind.false)) return;

        const obj = this.analyzer.types.at(resolved);
        if (obj.getKind() != .object_literal) return;

        const prop_name_type = try this.analyzer.propertyNameToType(this.file, d.right);

        const members = getSlice2(obj, program.Analyzer.ObjectLiteralMember);
        for (members) |m| {
            if (m.kind == .call_signature or m.kind == .index) continue;

            if (m.name == prop_name_type) {
                if (m.isReadonly()) {
                    const prop_name = getSlice( this.file.ast.nodes.at(d.right), u8);
                    try this.file.emitErrorFmt(d.right, "Cannot assign to '{s}' because it is a read-only property", .{prop_name});
                }
                return;
            }
        }

        // TODO: check indices
    }

    pub fn checkDuplicateDeclaration(this: *@This(), decl_ref: NodeRef) !void {
        const decl = this.file.ast.nodes.at(decl_ref);
        const d = getPackedData(decl);

        const sym_ref = this.file.binder.getSymbol(d.left) orelse return;
        const sym = this.file.binder.symbols.at(sym_ref);

        if (sym.next != 0) {
            const name_node = this.file.ast.nodes.at(d.left);
            const name = getSlice(name_node, u8);
            try this.file.emitErrorFmt(d.left, "Cannot redeclare block-scoped variable '{s}'", .{name});
        }
    }

    const ObjectLiteralMember = program.Analyzer.ObjectLiteralMember;

    pub fn checkCallExpression(this: *@This(), call_ref: NodeRef) !void {
        const d = getPackedData(this.file.ast.nodes.at(call_ref));
        const callee_type = try this.analyzer.getType(this.file, d.left);

        return this.checkCallExpressionWithCallee(call_ref, callee_type);
    }

    fn emitNotCallableError(this: *@This(), call_ref: NodeRef, callee_type: TypeRef) !void {
        try this.file.emitErrorFmt(call_ref,  "Type '{s}' is not callable", .{
            try this.analyzer.printType(callee_type),
        });
    }

    pub fn checkCallExpressionWithCallee(this: *@This(), call_ref: NodeRef, callee_type: TypeRef) !void {
        if (callee_type >= @intFromEnum(Kind.false)) {
            return this.emitNotCallableError(call_ref, callee_type);
        }

        const call = this.file.ast.nodes.at(call_ref);
        const d = getPackedData(call);

        const callee = this.analyzer.types.at(callee_type);

        var arg_refs: [32]NodeRef = undefined;
        var arg_count: usize = 0;
        {
            var iter = parser.NodeIterator.init(&this.file.ast.nodes, d.right);
            while (iter.nextRef()) |r| {
                if (arg_count >= arg_refs.len) return; // TODO: too many args, allocate
                arg_refs[arg_count] = r;
                arg_count += 1;
            }
        }
        const args = arg_refs[0..arg_count];

        switch (callee.getKind()) {
            .function_literal => {
                try this.checkArgsAgainstSignature(callee_type, args, call_ref);
            },
            .object_literal => {
                // maybe overloads or function with properties
                const members = getSlice2(callee, ObjectLiteralMember);

                var last_sig: ?TypeRef = null;
                for (members) |*m| {
                    if (m.kind != .call_signature) continue;

                    const fn_ty = try m.getType(this.analyzer);

                    if (fn_ty >= @intFromEnum(Kind.false)) continue;
                    const fn_node = this.analyzer.types.at(fn_ty);

                    if (fn_node.getKind() == .parameterized) continue;
                    if (fn_node.getKind() != .function_literal) continue;

                    if (try this.signatureMatches(fn_ty, args)) return;

                    last_sig = fn_ty;
                }

                // report errors against the last signature (TODO: show errors with all overloads)
                if (last_sig) |sig| {
                    try this.checkArgsAgainstSignature(sig, args, call_ref);
                } else {
                    return this.emitNotCallableError(call_ref, callee_type);
                }
            },
            else => {
                return this.emitNotCallableError(call_ref, callee_type);
            },
        }
    }

    fn signatureMatches(this: *@This(), fn_type: TypeRef, args: []const NodeRef) !bool {
        const fn_node = this.analyzer.types.at(fn_type);
        const params = getSlice2(fn_node, TypeRef);

        const min_params = this.countRequiredParams(params);
        if (args.len < min_params) return false;

        var has_spread = false;
        if (args.len > params.len) {
            if (params.len == 0) return false;
            if (!this.analyzer.isSpreadElement(params[params.len - 1])) return false;
            has_spread = true;
        }

        const check_count = @min(args.len, if (has_spread) params.len - 1 else params.len);
        for (0..check_count) |i| {
            const param_ref = params[i];
            const arg_type = try this.analyzer.getType(this.file, args[i]);
            if (!try this.analyzer.isAssignableTo(arg_type, param_ref)) return false;
        }

        if (has_spread) {
            if (check_count == args.len) return true;

            // check the remaining args by indexing into the rest type
            return error.TODO_check_spread;
        }

        return true;
    }

    pub fn checkArgsAgainstSignature(this: *@This(), fn_type: TypeRef, args: []const NodeRef, call_ref: NodeRef) !void {
        const fn_node = this.analyzer.types.at(fn_type);
        const params = getSlice2(fn_node, TypeRef);
        const min_params = this.countRequiredParams(params);
        if (args.len < min_params) {
            try this.file.emitErrorFmt(call_ref,  "Expected {} arguments, but got {}", .{ min_params, args.len });
            return;
        }

        const check_count = @min(args.len, params.len);
        for (0..check_count) |i| {
            const param_ref = params[i];
            const arg_type = try this.analyzer.getType(this.file, args[i]);

            if (!try this.analyzer.isAssignableTo(arg_type, param_ref)) {
                const param_type = this.analyzer.getTupleElementType(param_ref);

                const arg_str = try this.analyzer.printType(arg_type);
                const param_str = try this.analyzer.printType(param_type);
                try this.file.emitErrorFmt(args[i], "Argument of type '{s}' is not assignable to parameter of type '{s}'", .{ arg_str, param_str });
                return;
            }
        }
    }

    fn countRequiredParams(this: *@This(), params: []const TypeRef) usize {
        var count: usize = params.len;
        while (count > 0) {
            const p = params[count - 1];
            if (p >= @intFromEnum(Kind.false)) break;
            const t = this.analyzer.types.at(p);
            if (!t.hasFlag(.optional) and !t.hasFlag(.spread)) break;
            count -= 1;
        }
        return count;
    }

    pub fn checkPointlessTypeofComparison(this: *@This(), node_ref: NodeRef, rhs_type: TypeRef) !void {
        if (this.couldBeString(rhs_type)) return;

        const rhs_str = try this.analyzer.printType(rhs_type);
        try this.file.emitErrorFmt(node_ref, "This comparison appears to be unintentional because the types 'string' and '{s}' have no overlap", .{rhs_str});
    }

    fn couldBeString(this: *@This(), ty: TypeRef) bool {
        if (ty == @intFromEnum(Kind.string) or ty == @intFromEnum(Kind.empty_string)) return true;
        if (ty == @intFromEnum(Kind.any)) return true;
        if (ty >= @intFromEnum(Kind.false)) return false;

        const t = this.analyzer.types.at(ty);
        return switch (t.getKind()) {
            .string_literal, .template_literal => true,
            .@"union" => {
                for (getSlice2(t, TypeRef)) |el| {
                    if (this.couldBeString(el)) return true;
                }
                return false;
            },
            else => false,
        };
    }

    pub fn checkComparisonOverlap(this: *@This(), node_ref: NodeRef, left_type: TypeRef, right_type: TypeRef) !void {
        const has_overlap = try this.analyzer.isAssignableTo(left_type, right_type) or
            try this.analyzer.isAssignableTo(right_type, left_type);

        if (!has_overlap) {
            const left_str = try this.analyzer.printType(left_type);
            const right_str = try this.analyzer.printType(right_type);
            try this.file.emitErrorFmt(node_ref, "This comparison appears to be unintentional because the types '{s}' and '{s}' have no overlap", .{ left_str, right_str });
        }
    }

    pub fn checkAsyncCallInSyncContext(this: *@This(), call_ref: NodeRef, return_type: TypeRef) !void {
        if (try this.analyzer.maybeUnwrapPromise(return_type) != null) {
            try this.file.emitErrorFmt(call_ref, "Async call in sync context requires 'as async'", .{});
        }
    }

    pub fn checkAsAsyncOnNonAsyncCall(this: *@This(), node_ref: NodeRef, call_type: TypeRef) !void {
        if (try this.analyzer.maybeUnwrapPromise(call_type) == null) {
            try this.file.emitErrorFmt(node_ref, "Cannot use 'as async' on a non-async call signature", .{});
        }
    }

    pub fn checkAwaitInSyncContext(this: *@This(), node_ref: NodeRef) !void {
        try this.file.emitErrorFmt(node_ref, "'await' expressions are only allowed within async functions", .{});
    }

    pub fn checkAwaitOnNonPromise(this: *@This(), node_ref: NodeRef, inner_type: TypeRef) !void {
        if (try this.analyzer.maybeUnwrapPromise(inner_type) == null) {
            try this.file.emitErrorFmt(node_ref, "'await' has no effect on the type of this expression", .{});
        }
    }

    pub fn checkFunctionReturnType(this: *@This(), _: NodeRef, func: *const parser.AstNode) !void {
        const return_type_ref = func.extra_data2;
        if (return_type_ref == 0) return;

        const declared_return = try this.analyzer.getType(this.file, return_type_ref);

        if (declared_return == @intFromEnum(Kind.void) or declared_return == @intFromEnum(Kind.undefined)) return;

        if (try this.analyzer.isAssignableTo(@intFromEnum(Kind.undefined), declared_return)) return;

        // TODO: function may not have a name
        const d = getPackedData(func);
        try this.file.emitErrorFmt(d.left, "Function lacks ending return statement and return type does not include 'undefined'", .{});
    }

    pub fn checkUseBeforeInit(this: *@This(), node_ref: NodeRef, flow_type: TypeRef) !void {
        if (!try this.analyzer.isAssignableTo(@intFromEnum(Kind.undefined), flow_type)) return;

        const name_node = this.file.ast.nodes.at(node_ref);
        const name = getSlice(name_node, u8);
        try this.file.emitErrorFmt(node_ref, "Variable '{s}' is used before being assigned", .{name});
    }

    pub fn checkArithmeticAssignment(this: *@This(), expr_ref: NodeRef, _dest_type: TypeRef) !void {
        const dest_type = try this.analyzer.followAllAliases(_dest_type);

        if (this.analyzer.getKindOfRef(dest_type) != .@"union") return;

        var gen = try ArithmeticEnumerator.init(this, expr_ref) orelse return;

        while (try gen.current()) |result_type| {
            if (try this.analyzer.isAssignableTo(result_type, dest_type)) {
                gen.advance();
                continue;
            }

            if (result_type == @intFromEnum(Kind.never)) {
                var sub_buf: [256]u8 = undefined;
                const sub_str = gen.printSubstituted(&sub_buf);
                try this.file.emitErrorFmt(expr_ref, "Expression '{s}' produces NaN or Infinity", .{ sub_str });
                break;
            }

            const result_str = try this.analyzer.printType(result_type);
            const dest_str = try this.analyzer.printType(dest_type);
            var sub_buf: [256]u8 = undefined;
            const sub_str = gen.printSubstituted(&sub_buf);
            try this.file.emitErrorFmt(expr_ref, "Type '{s}' ({s}) is not assignable to type '{s}'", .{ result_str, sub_str, dest_str });
            break;
        }
    }

    const ArithmeticEnumerator = struct {
        const max_slots = 8;
        const max_values = 64;

        checker: *Checker,
        expr_ref: NodeRef,
        values: [max_values]f64 = undefined,
        slot_offsets: [max_slots + 1]u8 = .{0} ** (max_slots + 1),
        indices: [max_slots]u8 = .{0} ** max_slots,
        num_slots: u8 = 0,
        total_values: u8 = 0,
        exhausted: bool = false,
        is_number: bool = false,

        fn init(checker: *Checker, expr_ref: NodeRef) !?@This() {
            var self = @This(){
                .checker = checker,
                .expr_ref = expr_ref,
            };

            if (!try self.collectSlots(expr_ref)) return null;

            return self;
        }

        fn current(this: *@This()) !?TypeRef {
            if (this.exhausted) return null;

            if (this.is_number) {
                this.exhausted = true;
                return @intFromEnum(Kind.number);
            }

            var eval_idx: u8 = 0;
            const result = this.evaluate(this.expr_ref, &eval_idx);
            if (std.math.isNan(result)) {
                this.exhausted = true;
                return @intFromEnum(Kind.never);
            }

            return try this.checker.analyzer.numberToType(f64, result);
        }

        fn printSubstituted(this: *@This(), buf: []u8) []const u8 {
            var eval_idx: u8 = 0;
            var pos: usize = 0;
            this.printExpr(this.expr_ref, buf, &pos, &eval_idx);
            return buf[0..pos];
        }

        fn printExpr(this: *@This(), ref: NodeRef, buf: []u8, pos: *usize, eval_idx: *u8) void {
            const node = this.checker.file.ast.nodes.at(ref);

            if (node.kind == .binary_expression) {
                const op = @as(parser.SyntaxKind, @enumFromInt(node.len));
                switch (op) {
                    .plus_token, .minus_token, .asterisk_token, .slash_token => {
                        const d = getPackedData(node);
                        this.printExpr(d.left, buf, pos, eval_idx);
                        const op_str = switch (op) {
                            .plus_token => " + ",
                            .minus_token => " - ",
                            .asterisk_token => " * ",
                            .slash_token => " / ",
                            else => unreachable,
                        };
                        if (pos.* + op_str.len <= buf.len) {
                            @memcpy(buf[pos.*..][0..op_str.len], op_str);
                            pos.* += op_str.len;
                        }
                        this.printExpr(d.right, buf, pos, eval_idx);
                        return;
                    },
                    else => {},
                }
            } else if (node.kind == .parenthesized_expression) {                
                if (pos.* + 1 <= buf.len) {
                    @memcpy(buf[pos.*..][0..1], "(");
                    pos.* += 1;
                }
                this.printExpr(parser.unwrapRef(node), buf, pos, eval_idx);
                if (pos.* + 1 <= buf.len) {
                    @memcpy(buf[pos.*..][0..1], ")");
                    pos.* += 1;
                }
                return;
            }

            // Leaf: print the current slot value
            const slot = eval_idx.*;
            eval_idx.* += 1;
            const start = this.slot_offsets[slot];
            const val = this.values[start + this.indices[slot]];

            const int_val: i64 = @intFromFloat(val);
            if (@as(f64, @floatFromInt(int_val)) == val) {
                const s = std.fmt.bufPrint(buf[pos.*..], "{}", .{int_val}) catch return;
                pos.* += s.len;
            } else {
                const s = std.fmt.bufPrint(buf[pos.*..], "{d}", .{val}) catch return;
                pos.* += s.len;
            }
        }

        fn collectSlots(this: *@This(), ref: NodeRef) !bool {
            const node = this.checker.file.ast.nodes.at(ref);

            if (node.kind == .binary_expression) {
                const op = @as(parser.SyntaxKind, @enumFromInt(node.len));
                switch (op) {
                    .plus_token, .minus_token, .asterisk_token, .slash_token => {
                        const d = getPackedData(node);
                        return try this.collectSlots(d.left) and try this.collectSlots(d.right);
                    },
                    else => {},
                }
            } else if (node.kind == .parenthesized_expression) {
                return this.collectSlots(parser.unwrapRef(node));
            }

            const ty = try this.checker.analyzer.getTypeAsConst(this.checker.file, ref);
            return this.addSlotForType(ty);
        }

        fn addSlotForType(this: *@This(), ty: TypeRef) anyerror!bool {
            if (ty == @intFromEnum(Kind.number)) {
                this.is_number = true;
                return true;
            }

            if (this.num_slots >= max_slots) return false;
            this.slot_offsets[this.num_slots] = this.total_values;

            if (ty >= @intFromEnum(Kind.zero)) {
                if (this.total_values >= max_values) return false;
                this.values[this.total_values] = this.checker.analyzer.getDoubleFromType(ty);
                this.total_values += 1;
            } else if (ty >= @intFromEnum(Kind.false)) {
                return false;
            } else {
                const t = this.checker.analyzer.types.at(ty);
                switch (t.getKind()) {
                    .number_literal => {
                        if (this.total_values >= max_values) return false;
                        this.values[this.total_values] = this.checker.analyzer.getDoubleFromType(ty);
                        this.total_values += 1;
                    },
                    .@"union" => {
                        const members = getSlice2(t, TypeRef);
                        for (members) |m| {
                            if (m == @intFromEnum(Kind.number)) {
                                this.is_number = true;
                                return true;
                            }
                            if (!this.checker.analyzer.isNumericLiteral(m)) return false;
                            if (this.total_values >= max_values) return false;
                            this.values[this.total_values] = this.checker.analyzer.getDoubleFromType(m);
                            this.total_values += 1;
                        }
                    },
                    .alias => {
                        const followed = try this.checker.analyzer.followAllAliases(ty);
                        if (followed == ty) return false;
                        
                        return this.addSlotForType(followed);
                    },
                    else => return false,
                }
            }

            this.num_slots += 1;
            this.slot_offsets[this.num_slots] = this.total_values;
            return true;
        }

        fn evaluate(this: *@This(), ref: NodeRef, eval_idx: *u8) f64 {
            const node = this.checker.file.ast.nodes.at(ref);

            if (node.kind == .binary_expression) {
                const op = @as(parser.SyntaxKind, @enumFromInt(node.len));
                switch (op) {
                    .plus_token, .minus_token, .asterisk_token, .slash_token => {
                        const d = getPackedData(node);
                        const left = this.evaluate(d.left, eval_idx);
                        const right = this.evaluate(d.right, eval_idx);
                        return switch (op) {
                            .plus_token => left + right,
                            .minus_token => left - right,
                            .asterisk_token => left * right,
                            .slash_token => if (right != 0) left / right else std.math.nan(f64),
                            else => unreachable,
                        };
                    },
                    else => {},
                }
            } else if (node.kind == .parenthesized_expression) {
                return this.evaluate(parser.unwrapRef(node), eval_idx);
            }

            const slot = eval_idx.*;
            eval_idx.* += 1;
            const start = this.slot_offsets[slot];
            return this.values[start + this.indices[slot]];
        }

        fn advance(this: *@This()) void {
            var i: u8 = this.num_slots;
            while (i > 0) {
                i -= 1;
                this.indices[i] += 1;
                const slot_len = this.slot_offsets[i + 1] - this.slot_offsets[i];
                if (this.indices[i] < slot_len) return;
                this.indices[i] = 0;
            }
            this.exhausted = true;
        }
    };
};
