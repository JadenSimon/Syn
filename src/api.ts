import type ts from 'typescript'
import * as fs from 'node:fs'
import * as path from 'node:path'
import * as api from './api.zig'
import * as reifiedTypes from './reifiedTypes'

export * from './predicates'

export const enum SyntaxKind {
    Unknown = 0,
    EndOfFileToken = 1,
    SingleLineCommentTrivia = 2,
    MultiLineCommentTrivia = 3,
    NewLineTrivia = 4,
    WhitespaceTrivia = 5,
    ShebangTrivia = 6,
    ConflictMarkerTrivia = 7,
    NonTextFileMarkerTrivia = 8,
    NumericLiteral = 9,
    BigIntLiteral = 10,
    StringLiteral = 11,
    JsxText = 12,
    JsxTextAllWhiteSpaces = 13,
    RegularExpressionLiteral = 14,
    NoSubstitutionTemplateLiteral = 15,
    TemplateHead = 16,
    TemplateMiddle = 17,
    TemplateTail = 18,
    OpenBraceToken = 19,
    CloseBraceToken = 20,
    OpenParenToken = 21,
    CloseParenToken = 22,
    OpenBracketToken = 23,
    CloseBracketToken = 24,
    DotToken = 25,
    DotDotDotToken = 26,
    SemicolonToken = 27,
    CommaToken = 28,
    QuestionDotToken = 29,
    LessThanToken = 30,
    LessThanSlashToken = 31,
    GreaterThanToken = 32,
    LessThanEqualsToken = 33,
    GreaterThanEqualsToken = 34,
    EqualsEqualsToken = 35,
    ExclamationEqualsToken = 36,
    EqualsEqualsEqualsToken = 37,
    ExclamationEqualsEqualsToken = 38,
    EqualsGreaterThanToken = 39,
    PlusToken = 40,
    MinusToken = 41,
    AsteriskToken = 42,
    AsteriskAsteriskToken = 43,
    SlashToken = 44,
    PercentToken = 45,
    PlusPlusToken = 46,
    MinusMinusToken = 47,
    LessThanLessThanToken = 48,
    GreaterThanGreaterThanToken = 49,
    GreaterThanGreaterThanGreaterThanToken = 50,
    AmpersandToken = 51,
    BarToken = 52,
    CaretToken = 53,
    ExclamationToken = 54,
    TildeToken = 55,
    AmpersandAmpersandToken = 56,
    BarBarToken = 57,
    QuestionToken = 58,
    ColonToken = 59,
    AtToken = 60,
    QuestionQuestionToken = 61,
    BacktickToken = 62,
    HashToken = 63,
    EqualsToken = 64,
    PlusEqualsToken = 65,
    MinusEqualsToken = 66,
    AsteriskEqualsToken = 67,
    AsteriskAsteriskEqualsToken = 68,
    SlashEqualsToken = 69,
    PercentEqualsToken = 70,
    LessThanLessThanEqualsToken = 71,
    GreaterThanGreaterThanEqualsToken = 72,
    GreaterThanGreaterThanGreaterThanEqualsToken = 73,
    AmpersandEqualsToken = 74,
    BarEqualsToken = 75,
    BarBarEqualsToken = 76,
    AmpersandAmpersandEqualsToken = 77,
    QuestionQuestionEqualsToken = 78,
    CaretEqualsToken = 79,
    Identifier = 80,
    PrivateIdentifier = 81,
    BreakKeyword = 83,
    CaseKeyword = 84,
    CatchKeyword = 85,
    ClassKeyword = 86,
    ConstKeyword = 87,
    ContinueKeyword = 88,
    DebuggerKeyword = 89,
    DefaultKeyword = 90,
    DeleteKeyword = 91,
    DoKeyword = 92,
    ElseKeyword = 93,
    EnumKeyword = 94,
    ExportKeyword = 95,
    ExtendsKeyword = 96,
    FalseKeyword = 97,
    FinallyKeyword = 98,
    ForKeyword = 99,
    FunctionKeyword = 100,
    IfKeyword = 101,
    ImportKeyword = 102,
    InKeyword = 103,
    InstanceOfKeyword = 104,
    NewKeyword = 105,
    NullKeyword = 106,
    ReturnKeyword = 107,
    SuperKeyword = 108,
    SwitchKeyword = 109,
    ThisKeyword = 110,
    ThrowKeyword = 111,
    TrueKeyword = 112,
    TryKeyword = 113,
    TypeOfKeyword = 114,
    VarKeyword = 115,
    VoidKeyword = 116,
    WhileKeyword = 117,
    WithKeyword = 118,
    ImplementsKeyword = 119,
    InterfaceKeyword = 120,
    LetKeyword = 121,
    PackageKeyword = 122,
    PrivateKeyword = 123,
    ProtectedKeyword = 124,
    PublicKeyword = 125,
    StaticKeyword = 126,
    YieldKeyword = 127,
    AbstractKeyword = 128,
    AccessorKeyword = 129,
    AsKeyword = 130,
    AssertsKeyword = 131,
    AssertKeyword = 132,
    AnyKeyword = 133,
    AsyncKeyword = 134,
    AwaitKeyword = 135,
    BooleanKeyword = 136,
    ConstructorKeyword = 137,
    DeclareKeyword = 138,
    GetKeyword = 139,
    InferKeyword = 140,
    IntrinsicKeyword = 141,
    IsKeyword = 142,
    KeyOfKeyword = 143,
    ModuleKeyword = 144,
    NamespaceKeyword = 145,
    NeverKeyword = 146,
    OutKeyword = 147,
    ReadonlyKeyword = 148,
    RequireKeyword = 149,
    NumberKeyword = 150,
    ObjectKeyword = 151,
    SatisfiesKeyword = 152,
    SetKeyword = 153,
    StringKeyword = 154,
    SymbolKeyword = 155,
    TypeKeyword = 156,
    UndefinedKeyword = 157,
    UniqueKeyword = 158,
    UnknownKeyword = 159,
    UsingKeyword = 160,
    FromKeyword = 161,
    GlobalKeyword = 162,
    BigIntKeyword = 163,
    OverrideKeyword = 164,
    OfKeyword = 165,
    QualifiedName = 166,
    ComputedPropertyName = 167,
    TypeParameter = 168,
    Parameter = 169,
    Decorator = 170,
    PropertySignature = 171,
    PropertyDeclaration = 172,
    MethodSignature = 173,
    MethodDeclaration = 174,
    ClassStaticBlockDeclaration = 175,
    Constructor = 176,
    GetAccessor = 177,
    SetAccessor = 178,
    CallSignature = 179,
    ConstructSignature = 180,
    IndexSignature = 181,
    TypePredicate = 182,
    TypeReference = 183,
    FunctionType = 184,
    ConstructorType = 185,
    TypeQuery = 186,
    TypeLiteral = 187,
    ArrayType = 188,
    TupleType = 189,
    OptionalType = 190,
    RestType = 191,
    UnionType = 192,
    IntersectionType = 193,
    ConditionalType = 194,
    InferType = 195,
    ParenthesizedType = 196,
    ThisType = 197,
    TypeOperator = 198,
    IndexedAccessType = 199,
    MappedType = 200,
    LiteralType = 201,
    NamedTupleMember = 202,
    TemplateLiteralType = 203,
    TemplateLiteralTypeSpan = 204,
    ImportType = 205,
    ObjectBindingPattern = 206,
    ArrayBindingPattern = 207,
    BindingElement = 208,
    ArrayLiteralExpression = 209,
    ObjectLiteralExpression = 210,
    PropertyAccessExpression = 211,
    ElementAccessExpression = 212,
    CallExpression = 213,
    NewExpression = 214,
    TaggedTemplateExpression = 215,
    TypeAssertionExpression = 216,
    ParenthesizedExpression = 217,
    FunctionExpression = 218,
    ArrowFunction = 219,
    DeleteExpression = 220,
    TypeOfExpression = 221,
    VoidExpression = 222,
    AwaitExpression = 223,
    PrefixUnaryExpression = 224,
    PostfixUnaryExpression = 225,
    BinaryExpression = 226,
    ConditionalExpression = 227,
    TemplateExpression = 228,
    YieldExpression = 229,
    SpreadElement = 230,
    ClassExpression = 231,
    OmittedExpression = 232,
    ExpressionWithTypeArguments = 233,
    AsExpression = 234,
    NonNullExpression = 235,
    MetaProperty = 236,
    SyntheticExpression = 237,
    SatisfiesExpression = 238,
    TemplateSpan = 239,
    SemicolonClassElement = 240,
    Block = 241,
    EmptyStatement = 242,
    VariableStatement = 243,
    ExpressionStatement = 244,
    IfStatement = 245,
    DoStatement = 246,
    WhileStatement = 247,
    ForStatement = 248,
    ForInStatement = 249,
    ForOfStatement = 250,
    ContinueStatement = 251,
    BreakStatement = 252,
    ReturnStatement = 253,
    WithStatement = 254,
    SwitchStatement = 255,
    LabeledStatement = 256,
    ThrowStatement = 257,
    TryStatement = 258,
    DebuggerStatement = 259,
    VariableDeclaration = 260,
    VariableDeclarationList = 261,
    FunctionDeclaration = 262,
    ClassDeclaration = 263,
    InterfaceDeclaration = 264,
    TypeAliasDeclaration = 265,
    EnumDeclaration = 266,
    ModuleDeclaration = 267,
    ModuleBlock = 268,
    CaseBlock = 269,
    NamespaceExportDeclaration = 270,
    ImportEqualsDeclaration = 271,
    ImportDeclaration = 272,
    ImportClause = 273,
    NamespaceImport = 274,
    NamedImports = 275,
    ImportSpecifier = 276,
    ExportAssignment = 277,
    ExportDeclaration = 278,
    NamedExports = 279,
    NamespaceExport = 280,
    ExportSpecifier = 281,
    MissingDeclaration = 282,
    ExternalModuleReference = 283,
    JsxElement = 284,
    JsxSelfClosingElement = 285,
    JsxOpeningElement = 286,
    JsxClosingElement = 287,
    JsxFragment = 288,
    JsxOpeningFragment = 289,
    JsxClosingFragment = 290,
    JsxAttribute = 291,
    JsxAttributes = 292,
    JsxSpreadAttribute = 293,
    JsxExpression = 294,
    JsxNamespacedName = 295,
    CaseClause = 296,
    DefaultClause = 297,
    HeritageClause = 298,
    CatchClause = 299,
    ImportAttributes = 300,
    ImportAttribute = 301,
    ImportTypeAssertionContainer = 302,
    PropertyAssignment = 303,
    ShorthandPropertyAssignment = 304,
    SpreadAssignment = 305,
    EnumMember = 306,
    SourceFile = 307,
    Bundle = 308,
    JsDocTypeExpression = 309,
    JsDocNameReference = 310,
    JsDocMemberName = 311,
    JsDocAllType = 312,
    JsDocUnknownType = 313,
    JsDocNullableType = 314,
    JsDocNonNullableType = 315,
    JsDocOptionalType = 316,
    JsDocFunctionType = 317,
    JsDocVariadicType = 318,
    JsDocNamepathType = 319,
    JsDocComment = 320,
    JsDocText = 321,
    JsDocTypeLiteral = 322,
    JsDocSignature = 323,
    JsDocLink = 324,
    JsDocLinkCode = 325,
    JsDocLinkPlain = 326,
    JsDocTag = 327,
    JsDocAugmentsTag = 328,
    JsDocImplementsTag = 329,
    JsDocAuthorTag = 330,
    JsDocDeprecatedTag = 331,
    JsDocClassTag = 332,
    JsDocPublicTag = 333,
    JsDocPrivateTag = 334,
    JsDocProtectedTag = 335,
    JsDocReadonlyTag = 336,
    JsDocOverrideTag = 337,
    JsDocCallbackTag = 338,
    JsDocOverloadTag = 339,
    JsDocEnumTag = 340,
    JsDocParameterTag = 341,
    JsDocReturnTag = 342,
    JsDocThisTag = 343,
    JsDocTypeTag = 344,
    JsDocTemplateTag = 345,
    JsDocTypedefTag = 346,
    JsDocSeeTag = 347,
    JsDocPropertyTag = 348,
    JsDocThrowsTag = 349,
    JsDocSatisfiesTag = 350,
    JsDocImportTag = 351,
    SyntaxList = 352,
    NotEmittedStatement = 353,
    PartiallyEmittedExpression = 354,
    CommaListExpression = 355,
    SyntheticReferenceExpression = 356,
    ReifyExpression = 699,
    DeferStatement = 700,
    Start = 1022,
    ParseError = 1023,
}

function isTypeNodeKind(kind: SyntaxKind) {
    return (kind >= SyntaxKind.TypePredicate && kind <= SyntaxKind.ImportType)
        || kind === SyntaxKind.AnyKeyword
        || kind === SyntaxKind.UnknownKeyword
        || kind === SyntaxKind.NumberKeyword
        || kind === SyntaxKind.BigIntKeyword
        || kind === SyntaxKind.ObjectKeyword
        || kind === SyntaxKind.BooleanKeyword
        || kind === SyntaxKind.StringKeyword
        || kind === SyntaxKind.SymbolKeyword
        || kind === SyntaxKind.VoidKeyword
        || kind === SyntaxKind.UndefinedKeyword
        || kind === SyntaxKind.NeverKeyword
        || kind === SyntaxKind.IntrinsicKeyword
        || kind === SyntaxKind.ExpressionWithTypeArguments
}

export function isTypeNode(node: ts.Node) {
    return isTypeNodeKind(node.kind as any)
}

const enum InternalNodeFlags {
    Let = 1 << 0,
    Const = 1 << 1,
    Using = 1 << 2,
    Async = 1 << 3,
    Export = 1 << 7,
    Declare = 1 << 8,
    Static = 1 << 9,
    Public = 1 << 10,
    Protected = 1 << 11,
    Private = 1 << 12,
    Readonly = 1 << 13,
    Abstract = 1 << 14,
    Override = 1 << 15,
    Generator = 1 << 16,
    Optional = 1 << 17,
}

function linkedListToArray(ref: number, buf: Uint32Array, source: Uint8Array, parent?: AstNode) {
    let count = 0
    let tmp = ref
    while (tmp !== 0) {
        count += 1
        tmp = buf[(tmp * 8) + 1]
    }

    let i = 0
    const arr: AstNode[] = new Array(count)
    while (ref !== 0) {
        const node = new AstNode(ref, buf, source, parent)
        arr[i++] = node
        ref = node.next
    }
    return arr
}

// these are cached fields, kept separate from the class because we want them to not be on the class shape
interface AstNode {
    _left?: AstNode
    _right?: AstNode
    _lenNode?: AstNode
    _extraNode?: AstNode
    _extra2Node?: AstNode

    _text?: string
    _types?: AstNode[]
}

class AstNode {
    public readonly kind: SyntaxKind

    public constructor(
        public readonly ref: number, 
        public readonly buf: Uint32Array,
        public readonly source: Uint8Array,
        public readonly parent: AstNode | undefined = undefined
    ) {
        this.kind = (this.buf[this.ref * 8]) & 0x3FF
    }

    public get flags() {
        return (this.buf[this.ref * 8]) >> 12
    }

    public get next() {
        return this.buf[(this.ref * 8) + 1]
    }

    private get l() {
        return this.buf[(this.ref * 8) + 2]
    }

    private get r() {
        return this.buf[(this.ref * 8) + 3]
    }

    private get len() {
        return this.buf[(this.ref * 8) + 4]
    }

    public get extra() {
        return this.buf[(this.ref * 8) + 5]
    }

    public get extra2() {
        return this.buf[(this.ref * 8) + 6]
    }

    public get location() {
        return this.buf[(this.ref * 8) + 7]
    }

    // This is _slow_
    get text() {
        if (this._text !== undefined) {
            return this._text
        }

        switch (this.kind) {
            case SyntaxKind.NumericLiteral: {
                // `r` and `l` contain a f64
                const view = new Float64Array(this.buf.buffer, (this.ref * 32) + 8, 1)
                return this._text = String(view[0])
            }

            case SyntaxKind.RegularExpressionLiteral:
            case SyntaxKind.Identifier:
            case SyntaxKind.PrivateIdentifier:
            case SyntaxKind.StringLiteral:
            case SyntaxKind.TemplateHead:
            case SyntaxKind.TemplateMiddle:
            case SyntaxKind.TemplateTail:
            case SyntaxKind.NoSubstitutionTemplateLiteral: {
                const offset = api.ptrOffset(this.l, this.r, this.source)

                return this._text = (this.source.subarray(offset, offset + this.len) as Buffer).toString('utf-8')
            }
        }
    }

    getText(sourceFile?: ts.SourceFile) {
        const sf = getSourceFileInternal(sourceFile ?? this as any)!
        return sf.getFullText().slice(
            api.getStart(sf!.handle, this.ref),
            api.getEnd(sf!.handle, this.ref),
        )
    }

    // Use this for keys
    get hash() {
        switch (this.kind) {
            case SyntaxKind.Identifier:
                return this.extra2
        }
    }

    // _left: AstNode | undefined
    get left() {
        return this._left ??= new AstNode(this.l, this.buf, this.source, this)
    }

    // _right: AstNode | undefined
    get right() {
        return this._right ??= new AstNode(this.r, this.buf, this.source, this)
    }

    // _lenNode: AstNode | undefined
    get lenNode() {
        return this._lenNode ??= new AstNode(this.len, this.buf, this.source, this)
    }

    // _extraNode: AstNode | undefined
    get extraNode() {
        return this._extraNode ??= new AstNode(this.extra, this.buf, this.source, this)
    }

    get extra2Node() {
        return this._extra2Node ??= new AstNode(this.extra2, this.buf, this.source, this)
    }

    get expression(): AstNode | undefined {
        switch (this.kind) {
            case SyntaxKind.ExportAssignment:
            case SyntaxKind.JsxExpression: // may be empty
            case SyntaxKind.JsxSpreadAttribute:
            case SyntaxKind.CallExpression:
            case SyntaxKind.NewExpression:
            case SyntaxKind.PropertyAccessExpression:
            case SyntaxKind.ElementAccessExpression:
            case SyntaxKind.AwaitExpression:
            case SyntaxKind.VoidExpression:
            case SyntaxKind.TypeOfExpression:
            case SyntaxKind.DeleteExpression:
            case SyntaxKind.SpreadAssignment:
            case SyntaxKind.SpreadElement:
            case SyntaxKind.Decorator:
            case SyntaxKind.ThrowStatement:
            case SyntaxKind.ComputedPropertyName:
            case SyntaxKind.ExpressionStatement:
            case SyntaxKind.ExpressionWithTypeArguments:
            case SyntaxKind.AsExpression:
            case SyntaxKind.SatisfiesExpression:
            case SyntaxKind.IfStatement:
            case SyntaxKind.WhileStatement:
            case SyntaxKind.ParenthesizedExpression:
            case SyntaxKind.TemplateSpan:
            case SyntaxKind.CaseClause:
            case SyntaxKind.SwitchStatement:
                return this.left

            case SyntaxKind.YieldExpression:
            case SyntaxKind.ReturnStatement:
                return this.l ? this.left : undefined

            case SyntaxKind.DoStatement:
            case SyntaxKind.ForOfStatement:
            case SyntaxKind.ForInStatement:
                return this.right  
        }
    }

    get operand(): AstNode | undefined {
        switch (this.kind) {
            case SyntaxKind.PrefixUnaryExpression:
                return this.right

            case SyntaxKind.PostfixUnaryExpression:
                return this.left
        }
    }

    get name(): AstNode | undefined {
        switch (this.kind) {
            case SyntaxKind.ImportClause:
                return this.l ? this.left : undefined

            case SyntaxKind.BindingElement:
            case SyntaxKind.GetAccessor:
            case SyntaxKind.SetAccessor:
            case SyntaxKind.PropertySignature:
            case SyntaxKind.PropertyDeclaration:
            case SyntaxKind.PropertyAssignment:
            case SyntaxKind.MethodDeclaration:
            case SyntaxKind.MethodSignature:
            case SyntaxKind.ShorthandPropertyAssignment:
            case SyntaxKind.Parameter:
            case SyntaxKind.TypeParameter:
            case SyntaxKind.NamespaceExport:
            case SyntaxKind.NamespaceImport:
            case SyntaxKind.EnumMember:
            case SyntaxKind.EnumDeclaration:
            case SyntaxKind.NamedTupleMember:
            case SyntaxKind.InterfaceDeclaration:
            case SyntaxKind.TypeAliasDeclaration:
            case SyntaxKind.VariableDeclaration:
                return this.left

            case SyntaxKind.MetaProperty:
            case SyntaxKind.PropertyAccessExpression:
                return this.right

            case SyntaxKind.ClassExpression:
            case SyntaxKind.ClassDeclaration:
            case SyntaxKind.FunctionExpression:
            case SyntaxKind.FunctionDeclaration:
                return this.l ? this.left : undefined

            case SyntaxKind.ImportSpecifier:
            case SyntaxKind.ExportSpecifier:
                return this.r ? this.right : this.left
        }
    }

    get label(): AstNode | undefined {
        switch (this.kind) {
            case SyntaxKind.LabeledStatement:
                return this.left
        }
    }

    get propertyName(): AstNode | undefined {
        switch (this.kind) {
            case SyntaxKind.ImportSpecifier:
            case SyntaxKind.ExportSpecifier:
                return this.r ? this.left : undefined

            case SyntaxKind.BindingElement:
                return this.len ? this.lenNode : undefined
        }
    }

    get namedBindings(): AstNode | undefined {
        switch (this.kind) {
            case SyntaxKind.ImportClause:
                return this.r ? this.right : undefined
        }
    }

    // _types: AstNode[] | undefined
    get types() {
        switch (this.kind) {
            case SyntaxKind.UnionType:
            case SyntaxKind.IntersectionType: {
                if (this._types) return this._types

                const arr = this._types = [] as AstNode[]
                if (this.r) {
                    arr.push(this.right)
                }

                let next = this.l
                while (next) {
                    const n = new AstNode(next, this.buf, this.source, this)
                    if (n.kind === this.kind) {
                        arr.push(new AstNode(n.r, this.buf, this.source, this))
                        next = n.l
                    } else {
                        arr.push(n)
                        break
                    }
                }

                return arr
            }
        }
    }

    // _elements: AstNode[] | undefined
    get elements() {
        switch (this.kind) {
            case SyntaxKind.ArrayLiteralExpression:
            case SyntaxKind.ObjectBindingPattern:
            case SyntaxKind.ArrayBindingPattern:
            case SyntaxKind.NamedExports:
            case SyntaxKind.NamedImports:
                return this._elements ??= linkedListToArray(this.l, this.buf, this.source, this)
        }
    }

    get body(): AstNode | undefined {
        switch (this.kind) {
            case SyntaxKind.ArrowFunction:
                return this.right

            case SyntaxKind.Constructor:
            case SyntaxKind.ModuleDeclaration:
                return this.r ? this.right : undefined

            case SyntaxKind.GetAccessor:
            case SyntaxKind.SetAccessor:
            case SyntaxKind.FunctionExpression:
            case SyntaxKind.FunctionDeclaration:
            case SyntaxKind.MethodDeclaration:
                return this.len ? this.lenNode : undefined
        }
    }

    get block(): AstNode | undefined  {
        switch (this.kind) {
            case SyntaxKind.CatchClause:
                return this.right    
        }
    }

    get variableDeclaration(): AstNode | undefined {
        switch (this.kind) {
            case SyntaxKind.CatchClause:
                return this.l ? this.left : undefined    
        }
    }

    get importClause(): AstNode | undefined {
        switch (this.kind) {
            case SyntaxKind.ImportDeclaration:
                return this.l ? this.left : undefined       
        }
    }

    get moduleSpecifier(): AstNode | undefined {
        switch (this.kind) {
            case SyntaxKind.ImportDeclaration:
                return this.right     

            case SyntaxKind.ExportDeclaration:
                return this.r ? this.right : undefined
        }
    }

    get initializer(): AstNode | undefined {
        switch (this.kind) {
            case SyntaxKind.ForStatement:
                if (this.l && this.left.kind === SyntaxKind.VariableStatement) {
                    const n = this.left.declarationList as any
                    n.parent = this
                    return n
                }

                return this.l ? this.left : undefined

            case SyntaxKind.ForOfStatement:
            case SyntaxKind.ForInStatement:
                if (this.left.kind === SyntaxKind.VariableStatement) {
                    const n = this.left.declarationList as any
                    n.parent = this
                    return n
                }
                return this.left

            case SyntaxKind.Parameter:
            case SyntaxKind.EnumMember:
            case SyntaxKind.JsxAttribute:
            case SyntaxKind.BindingElement:
            case SyntaxKind.VariableDeclaration:
            case SyntaxKind.PropertyDeclaration:
                return this.r ? this.right : undefined

            case SyntaxKind.PropertyAssignment:
                return this.right
        }
    }

    get statement(): AstNode | undefined {
        switch (this.kind) {
            case SyntaxKind.DoStatement:
                return this.left

            case SyntaxKind.ForStatement:
                return this.extraNode
            
            case SyntaxKind.ForInStatement:
            case SyntaxKind.ForOfStatement:
                return this.lenNode

            case SyntaxKind.WhileStatement:
            case SyntaxKind.LabeledStatement:
                return this.right
        }
    }

    // _members: AstNode[] | undefined
    get members() {
        switch (this.kind) {
            case SyntaxKind.TypeLiteral:
                return this._members ??= linkedListToArray(this.l, this.buf, this.source, this)

            case SyntaxKind.InterfaceDeclaration:
            case SyntaxKind.EnumDeclaration:
            case SyntaxKind.ClassDeclaration:
            case SyntaxKind.ClassExpression:
                return this._members ??= linkedListToArray(this.r, this.buf, this.source, this)
        }
    }

    // _statements: AstNode[] | undefined
    get statements() {
        switch (this.kind) {
            case SyntaxKind.Block:
            case SyntaxKind.SourceFile:
            case SyntaxKind.ClassStaticBlockDeclaration:
                return this._statements ??= linkedListToArray(this.l, this.buf, this.source, this)

            case SyntaxKind.CaseClause:
            case SyntaxKind.DefaultClause:
                return this._statements ??= linkedListToArray(this.len, this.buf, this.source, this)
        }
    }

    get objectType() {
        switch (this.kind) {
            case SyntaxKind.IndexedAccessType:
                return this.left
        }
    }

    get indexType() {
        switch (this.kind) {
            case SyntaxKind.IndexedAccessType:
                return this.right
        }
    }

    get type(): AstNode | undefined {
        switch (this.kind) {
            case SyntaxKind.FunctionType:
            case SyntaxKind.PropertySignature:
                return this.r ? this.right : undefined

            case SyntaxKind.TypeAliasDeclaration:
                return this.lenNode

            case SyntaxKind.PropertyDeclaration:
            case SyntaxKind.VariableDeclaration:
                return this.len ? this.lenNode : undefined

            case SyntaxKind.FunctionDeclaration:
                return this.extra2 ? this.extra2Node : undefined
        }
    }

    // _typeParameters: AstNode[] | undefined
    get typeParameters() {
        switch (this.kind) {
            case SyntaxKind.ClassExpression:
            case SyntaxKind.ClassDeclaration:
            case SyntaxKind.MethodDeclaration:
            case SyntaxKind.FunctionExpression:
            case SyntaxKind.FunctionDeclaration:
                return this._typeParameters ??= linkedListToArray(this.extra, this.buf, this.source, this)

            case SyntaxKind.InterfaceDeclaration:
                return this._typeParameters ??= linkedListToArray(this.len, this.buf, this.source, this)

            case SyntaxKind.TypeAliasDeclaration:
                return this._typeParameters ??= linkedListToArray(this.r, this.buf, this.source, this)
        }
    }
    
    // _properties: AstNode[] | undefined
    get properties() {
        switch (this.kind) {
            case SyntaxKind.JsxAttributes:
            case SyntaxKind.ObjectLiteralExpression:
                return this._properties ??= linkedListToArray(this.l, this.buf, this.source, this)
        }
    }

    // _parameters: AstNode[] | undefined
    get parameters() {
        switch (this.kind) {
            case SyntaxKind.FunctionType:
            case SyntaxKind.Constructor:
            case SyntaxKind.ArrowFunction:
                return this._parameters ??= linkedListToArray(this.l, this.buf, this.source, this)

            case SyntaxKind.GetAccessor:
            case SyntaxKind.SetAccessor:
            case SyntaxKind.MethodSignature:
            case SyntaxKind.MethodDeclaration:
            case SyntaxKind.FunctionExpression:
            case SyntaxKind.FunctionDeclaration:
                return this._parameters ??= linkedListToArray(this.r, this.buf, this.source, this)
        }
    }

    // Synthethic node
    // _declarationList: ts.VariableDeclarationList | undefined
    get declarationList() {
        if (this._declarationList) return this._declarationList

        const n: ts.VariableDeclarationList = {
            kind: SyntaxKind.VariableDeclarationList,
            parent: this,
            flags: this.flags,
            forEachChild: (...args) => forEachChild(n, ...args),
        }

        ;(n as any).declarations = this._declarations ??= linkedListToArray(this.l, this.buf, this.source, n)

        return this._declarationList = n
    }

    // _declarations: AstNode[] | undefined
    get declarations() {
        switch (this.kind) {
            case SyntaxKind.VariableDeclarationList:
                return this.declarationList.declarations
        }
    }

    get condition(): AstNode | undefined {
        switch (this.kind) {
            case SyntaxKind.ConditionalExpression:
                return this.left

            case SyntaxKind.ForStatement:
                return this.right
        }
    }

    get incrementor(): AstNode | undefined {
        switch (this.kind) {
            case SyntaxKind.ForStatement:
                return this.lenNode
        }
    }

    get whenTrue(): AstNode | undefined {
        switch (this.kind) {
            case SyntaxKind.ConditionalExpression:
                return this.right
        }
    }

    get whenFalse(): AstNode | undefined {
        switch (this.kind) {
            case SyntaxKind.ConditionalExpression:
                return this.lenNode
        }
    }

    get argumentExpression(): AstNode | undefined {
        switch (this.kind) {
            case SyntaxKind.ElementAccessExpression:
                return this.right
        }
    }

    get asteriskToken() {
        switch (this.kind) {
            case SyntaxKind.MethodDeclaration:
            case SyntaxKind.FunctionDeclaration:
            case SyntaxKind.FunctionExpression:
                if ((this.flags & InternalNodeFlags.Generator) === InternalNodeFlags.Generator) {
                    return { kind: SyntaxKind.AsteriskToken }
                }
                break
        }
    }

    get questionToken() {
        switch (this.kind) {
            case SyntaxKind.Parameter:
            case SyntaxKind.PropertySignature:
            case SyntaxKind.PropertyDeclaration:
            case SyntaxKind.MethodSignature:
            case SyntaxKind.NamedTupleMember:
                if ((this.flags & InternalNodeFlags.Optional) === InternalNodeFlags.Optional) {
                    return { kind: SyntaxKind.DotDotDotToken }
                }
                break
        }
    }

    get dotDotDotToken() {
        switch (this.kind) {
            case SyntaxKind.Parameter:
                if ((this.flags & InternalNodeFlags.Generator) === InternalNodeFlags.Generator) {
                    return { kind: SyntaxKind.DotDotDotToken }
                }
                break
        }
    }

    get operator() {
        switch (this.kind) {
            case SyntaxKind.TypeOperator:
            case SyntaxKind.PrefixUnaryExpression:
                return this.l

            case SyntaxKind.PostfixUnaryExpression:
                return this.r

        }
    }

    get operatorToken() {
        return { kind: this.len }
    }

    get questionDotToken() {
        switch (this.kind) {
            case SyntaxKind.CallExpression:
            case SyntaxKind.ElementAccessExpression:
            case SyntaxKind.PropertyAccessExpression:
                if ((this.flags & InternalNodeFlags.Optional) === InternalNodeFlags.Optional) {
                    return { kind: SyntaxKind.QuestionDotToken }
                }
                break
        }
    }

    get awaitModifier() {
        switch (this.kind) {
            case SyntaxKind.ForOfStatement:
                if ((this.flags & InternalNodeFlags.Async) === InternalNodeFlags.Async) {
                    return { kind: SyntaxKind.AwaitKeyword }
                } 
                break
        }
    }

    get isTypeOnly() {
        return (this.flags & InternalNodeFlags.Declare) === InternalNodeFlags.Declare
    }

    get exprName() {
        switch (this.kind) {
            case SyntaxKind.TypeQuery:
                return this.left
        }
    }

    // _arguments: AstNode[] | undefined
    get arguments() {
        switch (this.kind) {
            case SyntaxKind.NewExpression:
            case SyntaxKind.CallExpression:
                return this._arguments ??= linkedListToArray(this.r, this.buf, this.source, this)
        }
    }

    get thenStatement(): AstNode | undefined {
        switch (this.kind) {
            case SyntaxKind.IfStatement:
                return this.right
        }
    }

    get elseStatement(): AstNode | undefined {
        switch (this.kind) {
            case SyntaxKind.IfStatement:
                return this.len ? this.lenNode : undefined
        }
    }

    // _caseBlock: ts.Node | undefined
    get caseBlock() {
        switch (this.kind) {
            case SyntaxKind.SwitchStatement:
                return this._caseBlock ??= { 
                    kind: SyntaxKind.CaseBlock,
                    parent: this,
                    clauses: linkedListToArray(this.r, this.buf, this.source, this) 
                }
        } 
    }

    get tryBlock(): AstNode | undefined {
        switch (this.kind) {
            case SyntaxKind.TryStatement:
                return this.left
        } 
    }

    get catchClause(): AstNode | undefined {
        switch (this.kind) {
            case SyntaxKind.TryStatement:
                return this.r ? this.right : undefined
        } 
    }

    get finallyBlock(): AstNode | undefined {
        switch (this.kind) {
            case SyntaxKind.TryStatement:
                return this.len ? this.lenNode : undefined
        } 
    }

    get head(): AstNode | undefined {
        switch (this.kind) {
            case SyntaxKind.TemplateExpression:
                return this.left
        }
    }

    get literal(): AstNode | undefined {
        switch (this.kind) {
            case SyntaxKind.LiteralType:
                return this.left

            case SyntaxKind.TemplateSpan:
                return this.right
        }
    }

    //_templateSpans: AstNode[] | undefined
    get templateSpans() {
        switch (this.kind) {
            case SyntaxKind.TemplateExpression:
                return this._templateSpans ??= linkedListToArray(this.head!.next, this.buf, this.source, this)
        }
    }
    
    //_extendsClause: ts.HeritageClause | undefined
    get extendsClause(): AstNode | undefined {
        switch (this.kind) {
            case SyntaxKind.ClassExpression:
            case SyntaxKind.ClassDeclaration:
                if (this._extendsClause || !this.len) return this._extendsClause as any

                const clause = this._extendsClause = {
                    kind: SyntaxKind.HeritageClause,
                    token: SyntaxKind.ExtendsKeyword,
                    types: [this.lenNode],
                    parent: this,
                    forEachChild: (...args) => forEachChild(clause, ...args),
                    getSourceFile: () => this.getSourceFile(),
                    getLeadingTriviaWidth: () => 0,
                    pos: 0,
                }

                this.lenNode.parent = clause

                return clause
        }
    }

    get implementsClauses() {
        switch (this.kind) {
            case SyntaxKind.ClassExpression:
            case SyntaxKind.ClassDeclaration:
                return []
                //return linkedListToArray(this.extra2, this.buf, this.source, this)
        }
    }

    get heritageClauses() {
        switch (this.kind) {
            case SyntaxKind.ClassExpression:
            case SyntaxKind.ClassDeclaration: {
                const extendsClause = this.extendsClause
                if (extendsClause) {
                    return [extendsClause, ...this.implementsClauses!]
                }

                return this.extra2 ? this.implementsClauses : undefined
            }
        }
    }

    get modifiers(): ReturnType<typeof getModifiers> {
        return getModifiers(this)
    }

    get pos() {
        const sf = (this.getSourceFile() as any)?._sourceFile as SourceFileInternal | undefined
        if (!sf) {
            throw new Error('Missing source file')
        }

        return api.getFullStart(sf.handle, this.ref)
    }

    public forEachChild<T>(
        cbNode: (node: ts.Node) => T | undefined, 
        cbNodeArray?: (nodes: ts.NodeArray<ts.Node>) => T | undefined
    ): T | undefined {
        return forEachChild(this as any as ts.Node, cbNode, cbNodeArray)
    }

    public getSourceFile(): ts.SourceFile | undefined {
        return getSourceFile(this as any) as ts.SourceFile | undefined
    }

    public getFullText(sourceFile?: ts.SourceFile) {
        const sf = getSourceFileInternal(this as any)!
        if (this.kind === SyntaxKind.SourceFile) {
            return sf.getFullText()
        }

        return sf.getFullText().slice(
            api.getFullStart(sf.handle, this.ref),
            api.getEnd(sf.handle, this.ref)
        )
    }

    public getLeadingTriviaWidth(sourceFile?: ts.SourceFile): number {
        const sf = getSourceFileInternal(sourceFile ?? this as any)
        if (sf === undefined) {
            throw new Error(`No source file found`)
        }
        return api.getLeadingTriviaWidth(sf.handle, this.ref)
    }

    // SLOW
    public getLineAndCharacterOfPosition(pos: number): { line: number; character: number } {
        const sf = (this.getSourceFile() as any)?._sourceFile as SourceFileInternal | undefined
        if (!sf) {
            throw new Error('Missing source file')
        }

        const lines = sf.getLineMap()

        let l = 0
        let r = lines.length
        
        while (l < r) {
            const m = Math.floor((r - l) / 2) + l
            if (lines[m] < pos) {
                l = m+1
            } else {
                r = m
            }
        }

        return {
            line: l-1,
            character: pos - lines[l-1],
        }
    }
}

export function forEachChild<T>(
    node: ts.Node,
    cbNode: (node: ts.Node) => T | undefined, 
    cbNodeArray?: (nodes: ts.NodeArray<ts.Node>) => T | undefined
): T | undefined {
    if (cbNodeArray) {
        throw new Error('TODO')
    }

    return visitEachChild(node, cbNode as any, undefined, true) as T | undefined
}


export function getModifiers(node: AstNode | ts.HasModifiers) {
    if (!(node instanceof AstNode)) {
        return node.modifiers
    }

    const flags = node.flags
    if (flags === 0) {
        return
    }

    const arr: any[] = []
    if ((flags & InternalNodeFlags.Export) === InternalNodeFlags.Export) {
        arr.push({ kind: SyntaxKind.ExportKeyword })
    }

    if ((flags & InternalNodeFlags.Async) === InternalNodeFlags.Async) {
        arr.push({ kind: SyntaxKind.AsyncKeyword })
    }

    if ((flags & InternalNodeFlags.Declare) === InternalNodeFlags.Declare) {
        arr.push({ kind: SyntaxKind.DeclareKeyword })
    }

    if ((flags & InternalNodeFlags.Static) === InternalNodeFlags.Static) {
        arr.push({ kind: SyntaxKind.StaticKeyword })
    }

    if (node.kind === SyntaxKind.VariableStatement || node.kind === SyntaxKind.VariableDeclarationList) {
        if ((flags & InternalNodeFlags.Let) === InternalNodeFlags.Let) {
            arr.push({ kind: SyntaxKind.LetKeyword })
        }
    
        if ((flags & InternalNodeFlags.Const) === InternalNodeFlags.Const) {
            arr.push({ kind: SyntaxKind.ConstKeyword })
        }
    }

    return arr
}

export function setSyntheticLeadingComments(node: ts.Node, comments: any[]) {
    return node // TODO
}

export function getSyntheticLeadingComments(node: ts.Node) {
    return // TODO
}

export function canHaveModifiers(node: ts.Node) {
    const kind = node.kind;
    return kind === 168 /* TypeParameter */ || kind === 169 /* Parameter */ || kind === 171 /* PropertySignature */ || kind === 172 /* PropertyDeclaration */ || kind === 173 /* MethodSignature */ || kind === 174 /* MethodDeclaration */ || kind === 176 /* Constructor */ || kind === 177 /* GetAccessor */ || kind === 178 /* SetAccessor */ || kind === 181 /* IndexSignature */ || kind === 185 /* ConstructorType */ || kind === 218 /* FunctionExpression */ || kind === 219 /* ArrowFunction */ || kind === 231 /* ClassExpression */ || kind === 243 /* VariableStatement */ || kind === 262 /* FunctionDeclaration */ || kind === 263 /* ClassDeclaration */ || kind === 264 /* InterfaceDeclaration */ || kind === 265 /* TypeAliasDeclaration */ || kind === 266 /* EnumDeclaration */ || kind === 267 /* ModuleDeclaration */ || kind === 271 /* ImportEqualsDeclaration */ || kind === 272 /* ImportDeclaration */ || kind === 277 /* ExportAssignment */ || kind === 278 /* ExportDeclaration */;
}

export function canHaveDecorators(node: ts.Node) {
    const kind = node.kind;
    return kind === 169 /* Parameter */ || kind === 172 /* PropertyDeclaration */ || kind === 174 /* MethodDeclaration */ || kind === 177 /* GetAccessor */ || kind === 178 /* SetAccessor */ || kind === 231 /* ClassExpression */ || kind === 263 /* ClassDeclaration */;
}

export function isClassLike(node: AstNode | undefined) {
    return node && (node.kind === SyntaxKind.ClassDeclaration || node.kind === SyntaxKind.ClassExpression)
}

export function isAccessor(node: AstNode | undefined) {
    return node && (node.kind === SyntaxKind.GetAccessor || node.kind === SyntaxKind.SetAccessor)
}

export function isExpression(node: AstNode) {
    return isExpressionKind(node.kind)
}

export function isBreakOrContinueStatement(node: TsNode) {
    return false // XXX: TODO
}

function hasFlag(node: TsNode | AstNode, flag: InternalNodeFlags) {
    if (node instanceof AstNode) {
        return (node.flags & flag) === flag
    }

    return false // XXX: TODO
}

function hasAnyFlag(node: TsNode | AstNode, flags: number) {
    if (node instanceof AstNode) {
        return (node.flags & flags) !== 0
    }

    return false // XXX: TODO
}

const parameterPropFlags = InternalNodeFlags.Readonly 
    | InternalNodeFlags.Override 
    | InternalNodeFlags.Public 
    | InternalNodeFlags.Private 
    | InternalNodeFlags.Private

export function isParameterPropertyDeclaration(node: TsNode, parent: TsNode) {
    return parent.kind === SyntaxKind.Constructor && node.kind === SyntaxKind.Parameter && hasAnyFlag(node, parameterPropFlags)
}

function decodeLocation(loc: number) {
    switch (loc >> 30) {
        case 0b00:
            return {
                line: loc & 0x3FFFFF,
                col: (loc >> 22) & 0xFF,
            }
        case 0b01:
            return {
                line: loc & 0x3FFFF,
                col: (loc >> 18) & 0x3FF,
            }
        case 0b10:
            return {
                line: loc & 0xFFF,
                col: (loc >> 12) & 0x3FFFF,
            }
        case 0b11:
            return {
                line: loc & 0xFF,
                col: (loc >> 8) & 0x3FFFFF,
            }
    }
}

function getLineAndColumn(node: AstNode) {
    const location = node.location
    if (location === 0) {
        return
    }

    return decodeLocation(location)
}

function resizeIfNeeded(buf: Buffer, offset: number, needed: number) {
    const avail = buf.byteLength - offset
    if (avail >= needed) {
        return buf
    }

    const newSize = Math.max(buf.byteLength * 2, buf.byteLength + ((needed - avail) * 2))
    const newBuf = Buffer.allocUnsafe(newSize)
    newBuf.set(buf.subarray(0, offset), 0)

    return newBuf
}

function createNodeSerializer(nodeCount: number, sourceLen: number, skipTypes = false) {
    let next = 0
    let index = 0

    let heapPointer = 0
    let heap = Buffer.allocUnsafe(4096)

    let nodeBuffer = Buffer.allocUnsafe(nodeCount > 10_000 ? 32768 : 4096)
    let buf = new Uint32Array(nodeBuffer.buffer, nodeBuffer.byteOffset)
    let allocsBeforeResize = nodeBuffer.byteLength / 32

    const slices = new Map<any, { pointer: number; len: number }>()

    const isSynthetic = nodeCount === 0 && sourceLen === 0

    function resizeNodeBufferIfNeeded() {
        if (allocsBeforeResize > 0) {
            return allocsBeforeResize -= 1
        }

        const addedBytes = nodeBuffer.byteLength
        const newBuf = Buffer.allocUnsafe(nodeBuffer.byteLength * 2)
        newBuf.set(nodeBuffer, 0)
        nodeBuffer = newBuf
        buf = new Uint32Array(nodeBuffer.buffer, nodeBuffer.byteOffset)
        allocsBeforeResize = (addedBytes / 32) - 1
        // if (buf.buffer !== nodeBuffer.buffer) {
        //     buf = new Uint32Array(nodeBuffer.buffer, nodeBuffer.byteOffset)
        //     allocsBeforeResize = (() / 32) - 1
        // }
    }

    function writeString(s: string) {
        const cached = slices.get(s)
        if (cached !== undefined) {
            return cached
        }

        const data = Buffer.from(s)
        heap = resizeIfNeeded(heap, heapPointer, data.byteLength)
        heap.set(data, heapPointer)
        // Our heap starts at the end of the source buffer
        // Otherwise we'd overlap references
        const pointer = heapPointer + sourceLen
        heapPointer += data.byteLength

        const slice = { pointer, len: data.byteLength }
        slices.set(s, slice)

        return slice
    }

    function updateNext(node: AstNode) {
        resizeNodeBufferIfNeeded()

        buf.set(node.buf.subarray(node.ref * 8, (node.ref * 8) + 8), index * 8)
        buf[(index * 8) + 1] = next

        const ref = index + nodeCount
        index += 1

        return ref
    }

    function serializeNode(node: ts.Node | AstNode | undefined): number {
        if (!node) {
            return 0
        }

        if (!isSynthetic && node instanceof AstNode) {
            if (node.next !== next) {
                return updateNext(node)
            }

            return node.ref
        }

        // All nodes w/ `isSynthetic`
        // 
        // Identifier: 72790
        // StringLiteral: 50570
        // PropertyAssignment: 39877
        // PropertyAccessExpression: 19280
        // NumericLiteral: 15133
        // ObjectLiteralExpression: 13737
        // CallExpression: 13427
        // ArrowFunction: 6040

        // All true synthetic nodes
        // 11: 46793
        // 303: 37954
        // 80: 27866
        // 9: 13928
        // 210: 12685
        // 211: 9551
        // 213: 7344
        // 219: 5570
        // 241: 3752
        //
        // Serialization time: 58.8976049999998

        if (skipTypes) {
            switch (node.kind) {
                // case SyntaxKind.InterfaceDeclaration:
                // case SyntaxKind.TypeAliasDeclaration:
                case SyntaxKind.InferType:
                case SyntaxKind.ConditionalType:
                case SyntaxKind.LiteralType:
                case SyntaxKind.FunctionType:
                case SyntaxKind.TupleType:
                case SyntaxKind.ArrayType:
                case SyntaxKind.TypeQuery:
                case SyntaxKind.TypeLiteral:
                case SyntaxKind.TypeReference:
                case SyntaxKind.TypePredicate:
                case SyntaxKind.UnionType:
                case SyntaxKind.ParenthesizedType:
                case SyntaxKind.IntersectionType:
                    return 0
            }
        }

        resizeNodeBufferIfNeeded()
        const ref = index + nodeCount
        const offset = index * 8
        index += 1

        buf[offset + 7] = (node as any).location ?? 0

        switch (node.kind as any as SyntaxKind) {
            case SyntaxKind.StringLiteral:
                serializeStringLiteral(node, offset)
                break

            case SyntaxKind.Identifier:
            case SyntaxKind.PrivateIdentifier:
            case SyntaxKind.TemplateHead:
            case SyntaxKind.TemplateMiddle:
            case SyntaxKind.TemplateTail:
            case SyntaxKind.NoSubstitutionTemplateLiteral:
            case SyntaxKind.RegularExpressionLiteral:
                serializeStringLike(node, offset)
                break

            case SyntaxKind.Block:
            case SyntaxKind.SourceFile:
                serializeBlockLike(node, offset)
                break

            case SyntaxKind.NewExpression:
                serializeNewExpression(node, offset)
                break

            case SyntaxKind.CallExpression:
                serializeCallExpression(node, offset)
                break

            case SyntaxKind.GetAccessor:
            case SyntaxKind.SetAccessor:
            case SyntaxKind.MethodDeclaration:
            case SyntaxKind.FunctionExpression:
            case SyntaxKind.FunctionDeclaration:
                serializeFunctionDeclLike(node, offset)
                break

            case SyntaxKind.VariableDeclaration:
                serializeVariableDecl(node, offset)
                break

            case SyntaxKind.VariableStatement:
                serializeVariableStatement(node, offset)
                break

            case SyntaxKind.TryStatement:
                serializeTryStatement(node, offset)
                break

            case SyntaxKind.CatchClause:
                serializeCatchClause(node, offset)
                break

            case SyntaxKind.ShorthandPropertyAssignment:
                serializeWrapperNode(node.kind, node.name, offset)
                break

            case SyntaxKind.Decorator:
            case SyntaxKind.ComputedPropertyName:
            case SyntaxKind.SpreadElement:
            case SyntaxKind.SpreadAssignment:
            case SyntaxKind.DeleteExpression:
            case SyntaxKind.TypeOfExpression:
            case SyntaxKind.VoidExpression:
            case SyntaxKind.YieldExpression:
            case SyntaxKind.AwaitExpression:
            case SyntaxKind.ExpressionStatement:
            case SyntaxKind.ReturnStatement:
            case SyntaxKind.ThrowStatement:
            case SyntaxKind.ParenthesizedExpression:
                serializeWrapperNode(node.kind, node.expression, offset)
                break

            case SyntaxKind.Parameter:
                serializeParameter(node, offset)
                break

            case SyntaxKind.ArrowFunction:
                serializeArrowFunction(node, offset)
                break

            case SyntaxKind.IfStatement:
                serializeIfStatement(node, offset)
                break

            case SyntaxKind.WhileStatement:
                serializeWhileStatement(node, offset)
                break

            case SyntaxKind.ConditionalExpression:
                serializeConditionalExpression(node, offset)
                break

            case SyntaxKind.SwitchStatement:
                serializeSwitchStatement(node, offset)
                break

            case SyntaxKind.CaseClause:
            case SyntaxKind.DefaultClause:
                serializeCaseClause(node, offset)
                break

            case SyntaxKind.ForStatement:
                serializeForStatement(node, offset)
                break

            case SyntaxKind.ForOfStatement:
            case SyntaxKind.ForInStatement:
                serializeForOfStatement(node, offset)
                break

            case SyntaxKind.ClassExpression:
            case SyntaxKind.ClassDeclaration:
                serializeClassLikeDeclaration(node, offset)
                break

            case SyntaxKind.PropertyAccessExpression:
                serializePropertyAccessExpression(node, offset)
                break

            case SyntaxKind.ElementAccessExpression:
                serializeElementAccessExpression(node, offset)
                break

            case SyntaxKind.ObjectLiteralExpression:
                serializeObjectLiteralExpression(node, offset)
                break

            case SyntaxKind.ArrayLiteralExpression:
                serializeArrayLiteralExpression(node, offset)
                break

            case SyntaxKind.PropertyAssignment:
                serializePropertyAssignment(node, offset)
                break

            case SyntaxKind.LabeledStatement:
                serializeLabeledStatement(node, offset)
                break

            case SyntaxKind.BreakStatement:
            case SyntaxKind.ContinueStatement:
                serializeBreakOrContinue(node, offset)
                break

            case SyntaxKind.BinaryExpression:
                serializeBinaryExpression(node, offset)
                break

            case SyntaxKind.NumericLiteral: {
                buf[offset + 0] = SyntaxKind.NumericLiteral
                buf[offset + 1] = next
                const v = new Float64Array(buf.buffer, (offset * 4) + 8, 1)
                v[0] = Number((node as ts.NumericLiteral).text)
                break
            }

            case SyntaxKind.ExportSpecifier:
            case SyntaxKind.ImportSpecifier: {
                const flags = (node as ts.ImportOrExportSpecifier).isTypeOnly ? InternalNodeFlags.Declare : 0
                if ((node as ts.ImportOrExportSpecifier).propertyName !== undefined) {
                    const name = serializeNode((node as ts.ImportOrExportSpecifier).name)
                    const propertyName = serializeNode((node as ts.ImportOrExportSpecifier).propertyName)
                    buf[offset + 0] = (flags << 12) | node.kind
                    buf[offset + 1] = next
                    buf[offset + 2] = propertyName
                    buf[offset + 3] = name
                } else {
                    const name = serializeNode((node as ts.ImportOrExportSpecifier).name)
                    buf[offset + 0] = (flags << 12) | node.kind
                    buf[offset + 1] = next
                    buf[offset + 2] = name
                    buf[offset + 3] = 0
                }
                break
            }

            case SyntaxKind.NamedImports: {
                const imports = serializeList((node as ts.NamedImports).elements)
                buf[offset + 0] = SyntaxKind.NamedImports
                buf[offset + 1] = next
                buf[offset + 2] = imports
                buf[offset + 3] = 0
                break
            }

            case SyntaxKind.NamespaceExport:
            case SyntaxKind.NamespaceImport: {
                // TODO: handle the extra stuff w/ namespace import
                const name = serializeNode((node as ts.NamespaceImport | ts.NamespaceExport).name)
                buf[offset + 0] = node.kind
                buf[offset + 1] = next
                buf[offset + 2] = name
                buf[offset + 3] = 0
                break
            }

            case SyntaxKind.ImportClause: {
                const name = serializeNode((node as ts.ImportClause).name)
                const namedBindings = serializeNode((node as ts.ImportClause).namedBindings)
                buf[offset + 0] = SyntaxKind.ImportClause
                buf[offset + 1] = next
                buf[offset + 2] = name
                buf[offset + 3] = namedBindings
                break
            }

            case SyntaxKind.NamedExports: {
                const elements = serializeList((node as ts.NamedExports).elements)
                buf[offset + 0] = SyntaxKind.NamedExports
                buf[offset + 1] = next
                buf[offset + 2] = elements
                buf[offset + 3] = 0
                break
            }

            case SyntaxKind.ExportDeclaration: {
                const clause = serializeNode((node as ts.ExportDeclaration).exportClause)
                const specifier = serializeNode((node as ts.ExportDeclaration).moduleSpecifier)
                buf[offset + 0] = SyntaxKind.ExportDeclaration
                buf[offset + 1] = next
                buf[offset + 2] = clause
                buf[offset + 3] = specifier
                buf[offset + 4] = 0 // attributes
                break
            }

            case SyntaxKind.ImportDeclaration: {
                const clause = serializeNode((node as ts.ImportDeclaration).importClause)
                const specifier = serializeNode((node as ts.ImportDeclaration).moduleSpecifier)
                buf[offset + 0] = SyntaxKind.ImportDeclaration
                buf[offset + 1] = next
                buf[offset + 2] = clause
                buf[offset + 3] = specifier
                buf[offset + 4] = 0 // attributes
                break
            }

            case SyntaxKind.PrefixUnaryExpression: {
                const operand = serializeNode(node.operand)
                buf[offset + 0] = SyntaxKind.PrefixUnaryExpression
                buf[offset + 1] = next
                buf[offset + 2] = (node as ts.PrefixUnaryExpression).operator
                buf[offset + 3] = operand
                break
            }

            case SyntaxKind.PostfixUnaryExpression: {
                const operand = serializeNode(node.operand)
                buf[offset + 0] = SyntaxKind.PostfixUnaryExpression
                buf[offset + 1] = next
                buf[offset + 2] = operand
                buf[offset + 3] = (node as ts.PostfixUnaryExpression).operator
                break
            }

            case SyntaxKind.Constructor: {
                const parameters = serializeList(node.parameters)
                const body = serializeNode(node.body)
                buf[offset + 0] = SyntaxKind.Constructor
                buf[offset + 1] = next
                buf[offset + 2] = parameters
                buf[offset + 3] = body
                break
            }

            case SyntaxKind.AsExpression:
            case SyntaxKind.SatisfiesExpression: {
                const expression = serializeNode(node.expression)
                const type = serializeNode(node.type)
                buf[offset + 0] = node.kind
                buf[offset + 1] = next
                buf[offset + 2] = expression
                buf[offset + 3] = type
                break
            }

            case SyntaxKind.TemplateExpression: {
                const spans = serializeList([node.head, ...node.templateSpans])
                buf[offset + 0] = SyntaxKind.TemplateExpression
                buf[offset + 1] = next
                buf[offset + 2] = spans
                buf[offset + 3] = 0
                break
            }

            case SyntaxKind.TemplateSpan: {
                const expression = serializeNode(node.expression)
                const literal = serializeNode(node.literal)
                buf[offset + 0] = SyntaxKind.TemplateSpan
                buf[offset + 1] = next
                buf[offset + 2] = expression
                buf[offset + 3] = literal
                break
            }

            case SyntaxKind.BindingElement: {
                // TODO: `dotDotDotToken`
                const name = serializeNode(node.name)
                const initializer = serializeNode(node.initializer)
                const propertyName = serializeNode(node.propertyName)
                buf[offset + 0] = SyntaxKind.BindingElement
                buf[offset + 1] = next
                buf[offset + 2] = name
                buf[offset + 3] = initializer
                buf[offset + 4] = propertyName
                break
            }

            case SyntaxKind.ArrayBindingPattern:
            case SyntaxKind.ObjectBindingPattern: {
                const elements = serializeList(node.elements)
                buf[offset + 0] = node.kind
                buf[offset + 1] = next
                buf[offset + 2] = elements
                buf[offset + 3] = 0
                break
            }

            case SyntaxKind.ExportAssignment: {
                const expression = serializeNode(node.expression)
                buf[offset + 0] = node.kind
                buf[offset + 1] = next
                buf[offset + 2] = expression
                buf[offset + 3] = 0
                buf[offset + 4] = node.isExportEquals ? 1 : 0
                break
            }

            case SyntaxKind.ExpressionWithTypeArguments: {
                const expression = serializeNode(node.expression)
                const args = serializeList(node.typeArguments)
                buf[offset + 0] = node.kind
                buf[offset + 1] = next
                buf[offset + 2] = expression
                buf[offset + 3] = args
                break
            }

            case SyntaxKind.VariableDeclarationList: {
                const declarations = serializeList((node as ts.VariableDeclarationList).declarations)
                buf[offset + 0] = (node.flags << 12) | SyntaxKind.VariableDeclarationList
                buf[offset + 1] = next
                buf[offset + 2] = declarations
                buf[offset + 3] = 0
                break
            }

            case SyntaxKind.PropertyDeclaration: {
                let flags = 0
                if ((node as ts.PropertyDeclaration).questionToken) {
                    flags |= InternalNodeFlags.Optional
                }

                const name = serializeNode((node as ts.PropertyDeclaration).name)
                const type = serializeNode((node as ts.PropertyDeclaration).type)
                const initializer = serializeNode((node as ts.PropertyDeclaration).initializer)
                buf[offset + 0] = (getFlags(node, flags) << 12) | SyntaxKind.PropertyDeclaration
                buf[offset + 1] = next
                buf[offset + 2] = name
                buf[offset + 3] = initializer
                buf[offset + 4] = type
                break
            }

            case SyntaxKind.TrueKeyword:
            case SyntaxKind.FalseKeyword:
            case SyntaxKind.NullKeyword:
            case SyntaxKind.UndefinedKeyword:

            case SyntaxKind.VoidKeyword:
            case SyntaxKind.AnyKeyword:
            case SyntaxKind.BooleanKeyword:
            case SyntaxKind.StringKeyword:
            case SyntaxKind.NumberKeyword:
            case SyntaxKind.SymbolKeyword:
            case SyntaxKind.ObjectKeyword:
            case SyntaxKind.NeverKeyword:
            case SyntaxKind.UnknownKeyword:
            case SyntaxKind.IntrinsicKeyword:

            case SyntaxKind.NewKeyword:
            case SyntaxKind.ThisKeyword:
            case SyntaxKind.SuperKeyword:
            case SyntaxKind.ImportKeyword:
            case SyntaxKind.OmittedExpression:
            case SyntaxKind.EmptyStatement:
                buf[offset + 0] = node.kind
                buf[offset + 1] = next
                break             

            // Typescript only
            case SyntaxKind.EnumDeclaration: {
                let flags = 0
                if (!!(node as ts.EnumDeclaration).modifiers?.find(x => x.kind === SyntaxKind.ConstKeyword)) {
                    flags |= InternalNodeFlags.Const
                }
                const name = serializeNode((node as ts.EnumDeclaration).name)
                const members = serializeList((node as ts.EnumDeclaration).members)
                buf[offset + 0] = (flags << 12) | SyntaxKind.EnumDeclaration
                buf[offset + 1] = next
                buf[offset + 2] = name
                buf[offset + 3] = members
                break
            }

            case SyntaxKind.EnumMember: {
                const name = serializeNode((node as ts.EnumMember).name)
                const initializer = serializeNode((node as ts.EnumMember).initializer)
                buf[offset + 0] = SyntaxKind.EnumMember
                buf[offset + 1] = next
                buf[offset + 2] = name
                buf[offset + 3] = initializer
                break
            }

            case SyntaxKind.ModuleBlock: {
                const statements = serializeList((node as ts.ModuleBlock).statements)
                buf[offset + 0] = SyntaxKind.Block
                buf[offset + 1] = next
                buf[offset + 2] = statements
                buf[offset + 3] = 0
                break
            }

            case SyntaxKind.ModuleDeclaration: {
                const isNamespace = (node.flags & NodeFlags.Namespace) === NodeFlags.Namespace
                const name = serializeNode((node as ts.ModuleDeclaration).name)
                const body = serializeNode((node as ts.ModuleDeclaration).body)
                buf[offset + 0] = (getFlags(node, isNamespace ? InternalNodeFlags.Let : 0) << 12) | SyntaxKind.ModuleDeclaration
                buf[offset + 1] = next
                buf[offset + 2] = name
                buf[offset + 3] = body
                break
            }

            // Types
            case SyntaxKind.LiteralType:
                serializeWrapperNode(node.kind, (node as ts.LiteralTypeNode).literal, offset)
                break

            case SyntaxKind.InferType:
                serializeWrapperNode(node.kind, (node as ts.InferTypeNode).typeParameter, offset)
                break

            case SyntaxKind.ArrayType:
                serializeWrapperNode(node.kind, (node as ts.ArrayTypeNode).elementType, offset)
                break    

            case SyntaxKind.ParenthesizedType:
                serializeWrapperNode(node.kind, (node as ts.ParenthesizedTypeNode).type, offset)
                break

            case SyntaxKind.TypeQuery: {
                const exprName = serializeNode((node as ts.TypeQueryNode).exprName)
                const typeArguments = serializeList((node as ts.TypeQueryNode).typeArguments)
                buf[offset + 0] = SyntaxKind.TypeQuery
                buf[offset + 1] = next
                buf[offset + 2] = exprName
                buf[offset + 3] = 0
                buf[offset + 4] = typeArguments
                break
            }

            case SyntaxKind.FunctionType: {
                const typeParameters = serializeList((node as ts.FunctionTypeNode).parameters)
                const parameters = serializeList((node as ts.FunctionTypeNode).parameters)
                const type = serializeNode((node as ts.FunctionTypeNode).type)
                buf[offset + 0] = SyntaxKind.FunctionType
                buf[offset + 1] = next
                buf[offset + 2] = parameters
                buf[offset + 3] = type
                buf[offset + 4] = typeParameters
                break
            }

            case SyntaxKind.IndexedAccessType: {
                const objectType = serializeNode((node as ts.IndexedAccessTypeNode).objectType)
                const indexType = serializeNode((node as ts.IndexedAccessTypeNode).indexType)
                buf[offset + 0] = SyntaxKind.IndexedAccessType
                buf[offset + 1] = next
                buf[offset + 2] = objectType
                buf[offset + 3] = indexType
                break
            }

            case SyntaxKind.QualifiedName: {
                const left = serializeNode((node as ts.QualifiedName).left)
                const right = serializeNode((node as ts.QualifiedName).right)
                buf[offset + 0] = SyntaxKind.QualifiedName
                buf[offset + 1] = next
                buf[offset + 2] = left
                buf[offset + 3] = right
                break
            }

            case SyntaxKind.TypeReference: {
                const name = serializeNode((node as ts.TypeReferenceNode).typeName)
                const typeArguments = serializeList((node as ts.TypeReferenceNode).typeArguments)
                buf[offset + 0] = SyntaxKind.TypeReference
                buf[offset + 1] = next
                buf[offset + 2] = name
                buf[offset + 3] = typeArguments
                break
            }    

            case SyntaxKind.TypePredicate: {
                const parameterName = serializeNode((node as ts.TypePredicateNode).parameterName)
                const type = serializeNode((node as ts.TypePredicateNode).type)
                buf[offset + 0] = SyntaxKind.TypePredicate
                buf[offset + 1] = next
                buf[offset + 2] = parameterName
                buf[offset + 3] = type
                buf[offset + 4] = (node as ts.TypePredicateNode).assertsModifier ? 1 : 0 
                break
            }

            case SyntaxKind.InterfaceDeclaration: {
                const name = serializeNode((node as ts.InterfaceDeclaration).name)
                const members = serializeList((node as ts.InterfaceDeclaration).members)
                const heritageClauses = serializeList((node as ts.InterfaceDeclaration).heritageClauses?.[0]?.types)
                const typeParameters = serializeList((node as ts.InterfaceDeclaration).typeParameters)
                buf[offset + 0] = (getFlags(node) << 12) | SyntaxKind.InterfaceDeclaration
                buf[offset + 1] = next
                buf[offset + 2] = name
                buf[offset + 3] = members
                buf[offset + 4] = typeParameters
                buf[offset + 5] = heritageClauses
                break
            }

            case SyntaxKind.TypeAliasDeclaration: {
                const name = serializeNode((node as ts.TypeAliasDeclaration).name)
                const type = serializeNode((node as ts.TypeAliasDeclaration).type)
                const typeParameters = serializeList((node as ts.TypeAliasDeclaration).typeParameters)
                buf[offset + 0] = (getFlags(node) << 12) | SyntaxKind.TypeAliasDeclaration
                buf[offset + 1] = next
                buf[offset + 2] = name
                buf[offset + 3] = typeParameters
                buf[offset + 4] = type
                break
            }

            case SyntaxKind.UnionType:
            case SyntaxKind.IntersectionType: {
                const types = serializeList((node as ts.UnionTypeNode | ts.IntersectionTypeNode).types)
                buf[offset + 0] = (1 << 12) | node.kind // flags signals this is a list
                buf[offset + 1] = next
                buf[offset + 2] = types
                buf[offset + 3] = 0
                break
            }

            case SyntaxKind.TypeLiteral: {
                const members = serializeList((node as ts.TypeLiteralNode).members)
                buf[offset + 0] = SyntaxKind.TypeLiteral
                buf[offset + 1] = next
                buf[offset + 2] = members
                buf[offset + 3] = 0
                break
            }

            case SyntaxKind.TypeOperator: {
                const type = serializeNode((node as ts.TypeOperatorNode).type)
                buf[offset + 0] = SyntaxKind.TypeOperator
                buf[offset + 1] = next
                buf[offset + 2] = (node as ts.TypeOperatorNode).operator
                buf[offset + 3] = type
                break
            }

            case SyntaxKind.TypeParameter: {
                const name = serializeNode((node as ts.TypeParameterDeclaration).name)
                const constraint = serializeNode((node as ts.TypeParameterDeclaration).constraint)
                const defaultType = serializeNode((node as ts.TypeParameterDeclaration).default)
                buf[offset + 0] = SyntaxKind.TypeParameter
                buf[offset + 1] = next
                buf[offset + 2] = name
                buf[offset + 3] = constraint
                buf[offset + 4] = defaultType
                break
            }

            case SyntaxKind.ConditionalType: {
                const checkType = serializeNode((node as ts.ConditionalTypeNode).checkType)
                const extendsType = serializeNode((node as ts.ConditionalTypeNode).extendsType)
                const trueType = serializeNode((node as ts.ConditionalTypeNode).trueType)
                const falseType = serializeNode((node as ts.ConditionalTypeNode).falseType)
                buf[offset + 0] = SyntaxKind.ConditionalType
                buf[offset + 1] = next
                buf[offset + 2] = checkType
                buf[offset + 3] = extendsType
                buf[offset + 4] = trueType
                buf[offset + 5] = falseType
                break
            }

            case SyntaxKind.PropertySignature: {
                let flags = 0
                if ((node as ts.PropertySignature).questionToken) {
                    flags |= InternalNodeFlags.Optional
                }

                const name = serializeNode((node as ts.PropertySignature).name)
                const type = serializeNode((node as ts.PropertySignature).type)
                buf[offset + 0] = (getFlags(node, flags) << 12) | SyntaxKind.PropertySignature
                buf[offset + 1] = next
                buf[offset + 2] = name
                buf[offset + 3] = type
                break
            }
            
            case SyntaxKind.MethodSignature: {
                let flags = 0
                if ((node as ts.MethodSignature).questionToken) {
                    flags |= InternalNodeFlags.Optional
                }

                const name = serializeNode((node as ts.MethodSignature).name)
                const type = serializeNode((node as ts.MethodSignature).type)
                const parameters = serializeList((node as ts.MethodSignature).parameters)
                const typeParameters = serializeList((node as ts.MethodSignature).typeParameters)
                buf[offset + 0] = (getFlags(node, flags) << 12) | SyntaxKind.MethodSignature
                buf[offset + 1] = next
                buf[offset + 2] = name
                buf[offset + 3] = parameters
                buf[offset + 4] = type
                buf[offset + 5] = typeParameters
                break
            }

            case SyntaxKind.ClassStaticBlockDeclaration: {
                const body = serializeList((node as ts.ClassStaticBlockDeclaration).body.statements)
                buf[offset + 0] = SyntaxKind.ClassStaticBlockDeclaration
                buf[offset + 1] = next
                buf[offset + 2] = body
                buf[offset + 3] = 0
                break
            }

            default:
                if (node.kind === SyntaxKind.DeferStatement) {
                    // we need to ensure a clean `next` for the inner statement
                    const n = next
                    next = 0
                    const statement = serializeNode(node.statement ?? node.left)
                    buf[offset + 0] = SyntaxKind.DeferStatement
                    buf[offset + 1] = n
                    buf[offset + 2] = statement
                    buf[offset + 3] = 0
                    break
                }

                if (node.kind === SyntaxKind.ReifyExpression) {
                    const statement = serializeNode(node.type ?? node.left)
                    buf[offset + 0] = SyntaxKind.ReifyExpression
                    buf[offset + 1] = next
                    buf[offset + 2] = statement
                    buf[offset + 3] = 0
                    buf[offset + 3] = (node as any).len ?? 0
                    break
                }

                if (node.kind === SyntaxKind.CallSignature || node.kind === SyntaxKind.ConstructSignature) {
                    const type = serializeNode((node as any as ts.CallSignatureDeclaration).type)
                    const parameters = serializeList((node as any as ts.CallSignatureDeclaration).parameters)
                    const typeParameters = serializeList((node as any as ts.CallSignatureDeclaration).typeParameters)
                    buf[offset + 0] = node.kind
                    buf[offset + 1] = next
                    buf[offset + 2] = parameters
                    buf[offset + 3] = type
                    buf[offset + 4] = typeParameters
                    break
                }

                throw new Error(`Not handled: ${node.kind} [isSynthetic: ${isSynthetic}]`)
                //throw new Error(`Not handled: ${node.kind} (${SyntaxKind[node.kind]})`)
        }

        return ref
    }

    function getFlags(node: ts.Node, extraFlags = 0) {
        let flags = (node.flags ?? 0) as number
        flags |= extraFlags

        if (isSynthetic && node instanceof AstNode) return flags

        const modifiers = (node as any).modifiers
        if (!modifiers) return flags

        for (let i = 0; i < modifiers.length; i++) {
            switch (modifiers[i].kind as any as SyntaxKind) {
                case SyntaxKind.ConstKeyword:
                    flags |= InternalNodeFlags.Const
                    break
                case SyntaxKind.LetKeyword:
                    flags |= InternalNodeFlags.Let
                    break
                case SyntaxKind.ExportKeyword:
                    flags |= InternalNodeFlags.Export
                    break
                case SyntaxKind.AsyncKeyword:
                    flags |= InternalNodeFlags.Async
                    break
                case SyntaxKind.StaticKeyword:
                    flags |= InternalNodeFlags.Static
                    break
                case SyntaxKind.ReadonlyKeyword:
                    flags |= InternalNodeFlags.Readonly
                    break
                case SyntaxKind.DeclareKeyword:
                    flags |= InternalNodeFlags.Declare
                    break
            }
        }

        return flags
    }

    function serializeList(list: readonly ts.Node[] | undefined) {
        if (!list || list.length === 0) {
            return 0
        }

        const prev = next
        next = 0

        // Start from the tail and work backwards
        for (let i = list.length - 1; i >= 0; i--) {
            next = serializeNode(list[i])
        }

        const head = next
        next = prev

        return head
    }

    function serializeBinaryExpression(node: ts.BinaryExpression, offset: number) {
        const left = serializeNode(node.left)
        const right = serializeNode(node.right)
        buf[offset + 0] = SyntaxKind.BinaryExpression
        buf[offset + 1] = next
        buf[offset + 2] = left
        buf[offset + 3] = right
        buf[offset + 4] = node.operatorToken.kind
    }

    function serializeCatchClause(node: ts.CatchClause, offset: number) {
        const variableDeclaration = serializeNode(node.variableDeclaration)
        const block = serializeNode(node.block)
        buf[offset + 0] = SyntaxKind.CatchClause
        buf[offset + 1] = next
        buf[offset + 2] = variableDeclaration
        buf[offset + 3] = block
    }

    function serializeTryStatement(node: ts.TryStatement, offset: number) {
        const tryBlock = serializeNode(node.tryBlock)
        const catchClause = serializeNode(node.catchClause)
        const finallyBlock = serializeNode(node.finallyBlock)
        buf[offset + 0] = SyntaxKind.TryStatement
        buf[offset + 1] = next
        buf[offset + 2] = tryBlock
        buf[offset + 3] = catchClause
        buf[offset + 4] = finallyBlock
    }

    function serializePropertyAccessExpression(node: ts.PropertyAccessExpression, offset: number) {
        let flags = 0
        if (node.questionDotToken) {
            flags |= InternalNodeFlags.Optional
        }
        const expression = serializeNode(node.expression)
        const name = serializeNode(node.name)
        buf[offset + 0] = (flags << 12) | SyntaxKind.PropertyAccessExpression
        buf[offset + 1] = next
        buf[offset + 2] = expression
        buf[offset + 3] = name
    }

    function serializePropertyAssignment(node: ts.PropertyAssignment, offset: number) {
        const name = serializeNode(node.name)
        const initializer = serializeNode(node.initializer)
        buf[offset + 0] = SyntaxKind.PropertyAssignment
        buf[offset + 1] = next
        buf[offset + 2] = name
        buf[offset + 3] = initializer
    }

    function serializeObjectLiteralExpression(node: ts.ObjectLiteralExpression, offset: number) {
        const properties = serializeList(node.properties)
        buf[offset + 0] = SyntaxKind.ObjectLiteralExpression
        buf[offset + 1] = next
        buf[offset + 2] = properties
        buf[offset + 3] = 0
    }

    function serializeArrayLiteralExpression(node: ts.ArrayLiteralExpression, offset: number) {
        const elements = serializeList(node.elements)
        buf[offset + 0] = SyntaxKind.ArrayLiteralExpression
        buf[offset + 1] = next
        buf[offset + 2] = elements
        buf[offset + 3] = 0
    }

    function serializeElementAccessExpression(node: ts.ElementAccessExpression, offset: number) {
        let flags = 0
        if (node.questionDotToken) {
            flags |= InternalNodeFlags.Optional
        }
        const expression = serializeNode(node.expression)
        const argumentExpression = serializeNode(node.argumentExpression)
        buf[offset + 0] = (flags << 12) | SyntaxKind.ElementAccessExpression
        buf[offset + 1] = next
        buf[offset + 2] = expression
        buf[offset + 3] = argumentExpression
    }

    function serializeBreakOrContinue(node: ts.BreakStatement | ts.ContinueStatement, offset: number) {
        const label = serializeNode(node.label)
        buf[offset + 0] = node.kind
        buf[offset + 1] = next
        buf[offset + 2] = label
        buf[offset + 3] = 0
    }

    function serializeLabeledStatement(node: ts.LabeledStatement, offset: number) {
        const label = serializeNode(node.label)
        const statement = serializeNode(node.statement)
        buf[offset + 0] = SyntaxKind.LabeledStatement
        buf[offset + 1] = next
        buf[offset + 2] = label
        buf[offset + 3] = statement
    }

    function serializeWhileStatement(node: ts.WhileStatement, offset: number) {
        const expression = serializeNode(node.expression)
        const statement = serializeNode(node.statement)
        buf[offset + 0] = SyntaxKind.WhileStatement
        buf[offset + 1] = next
        buf[offset + 2] = expression
        buf[offset + 3] = statement
    } 

    function serializeIfStatement(node: ts.IfStatement, offset: number) {
        const expression = serializeNode(node.expression)
        const thenStatement = serializeNode(node.thenStatement)
        const elseStatement = serializeNode(node.elseStatement)
        buf[offset + 0] = SyntaxKind.IfStatement
        buf[offset + 1] = next
        buf[offset + 2] = expression
        buf[offset + 3] = thenStatement
        buf[offset + 4] = elseStatement
    } 

    function serializeArrowFunction(node: ts.ArrowFunction, offset: number) {
        const parameters = serializeList(node.parameters)
        const body = serializeNode(node.body)
        buf[offset + 0] = (getFlags(node) << 12) | SyntaxKind.ArrowFunction
        buf[offset + 1] = next
        buf[offset + 2] = parameters
        buf[offset + 3] = body
        buf[offset + 4] = 0
        buf[offset + 5] = 0
    }

    function serializeParameter(node: ts.ParameterDeclaration, offset: number) {
        let flags = node.flags
        if (node.dotDotDotToken) {
            flags |= InternalNodeFlags.Generator
        }
        if (node.questionToken) {
            flags |= InternalNodeFlags.Optional
        }

        // if (isConstructor && node.modifiers) {
        //     for (const m of node.modifiers) {
        //         switch ((m.kind as any) as SyntaxKind) {
        //             case SyntaxKind.PublicKeyword:
        //                 flags |= InternalNodeFlags.Public
        //                 break
        //             case SyntaxKind.PrivateKeyword:
        //                 flags |= InternalNodeFlags.Private
        //                 break
        //             case SyntaxKind.ProtectedKeyword:
        //                 flags |= InternalNodeFlags.Protected
        //                 break
        //             case SyntaxKind.ReadonlyKeyword:
        //                 flags |= InternalNodeFlags.Readonly
        //                 break
        //             case SyntaxKind.OverrideKeyword:
        //                 flags |= InternalNodeFlags.Override
        //                 break
        //         }
        //     }
        // }

        const name = serializeNode(node.name)
        const initializer = serializeNode(node.initializer)
        const type = serializeNode(node.type)
        buf[offset + 0] = (flags << 12) | SyntaxKind.Parameter
        buf[offset + 1] = next
        buf[offset + 2] = name
        buf[offset + 3] = initializer
        buf[offset + 4] = type
    }

    function serializeWrapperNode(kind: SyntaxKind, inner: ts.Node, offset: number) {
        const n = serializeNode(inner)
        buf[offset + 0] = kind
        buf[offset + 1] = next
        buf[offset + 2] = n
        buf[offset + 3] = 0
    }

    function serializeVariableStatement(node: ts.VariableStatement, offset: number) {
        const declarations = serializeList(node.declarationList.declarations)
        buf[offset + 0] = (getFlags(node) << 12) | SyntaxKind.VariableStatement
        buf[offset + 1] = next
        buf[offset + 2] = declarations
        buf[offset + 3] = 0
    }

    function serializeVariableDecl(node: ts.VariableDeclaration, offset: number) {
        const name = serializeNode(node.name)
        const initializer = serializeNode(node.initializer)
        buf[offset + 0] = SyntaxKind.VariableDeclaration
        buf[offset + 1] = next
        buf[offset + 2] = name
        buf[offset + 3] = initializer
        buf[offset + 4] = 0 // type
    }

    function serializeStringLiteral(node: ts.StringLiteral, offset: number) {
        let flags = 0
        if (isSynthetic && (node instanceof AstNode)) {
            flags = node.flags
        } else {
            flags |= node.isSingleQuote ? (1 << 0) : (1 << 1)
            flags |= (1 << 6) // synthetic
        }
        const r = writeString(node.text)
        buf[offset + 0] = (flags << 12) | node.kind
        buf[offset + 1] = next
        buf[offset + 2] = r.pointer
        buf[offset + 3] = 0xFF // tagged pointer
        buf[offset + 4] = r.len
        // buf[offset + 7] = 0 // location
    }

    function serializeStringLike(node: ts.Identifier | ts.StringLiteral | ts.PrivateIdentifier, offset: number) {
        const r = writeString(node.text)
        buf[offset + 0] = node.kind
        buf[offset + 1] = next
        buf[offset + 2] = r.pointer
        buf[offset + 3] = 0xFF // tagged pointer
        buf[offset + 4] = r.len
        // buf[offset + 7] = 0 // location
    }

    function serializeNewExpression(node: ts.NewExpression, offset: number) {
        const expression = serializeNode(node.expression)
        const argumentsArray = serializeList(node.arguments)
        buf[offset + 0] = SyntaxKind.NewExpression
        buf[offset + 1] = next
        buf[offset + 2] = expression
        buf[offset + 3] = argumentsArray
        buf[offset + 4] = 0 // TODO: type args
    }

    function serializeCallExpression(node: ts.CallExpression, offset: number) {
        let flags = 0
        if (node.questionDotToken) {
            flags |= InternalNodeFlags.Optional
        }
        const expression = serializeNode(node.expression)
        const argumentsArray = serializeList(node.arguments)
        buf[offset + 0] = (flags << 12) | SyntaxKind.CallExpression
        buf[offset + 1] = next
        buf[offset + 2] = expression
        buf[offset + 3] = argumentsArray
        buf[offset + 4] = 0 // TODO: type args
    }

    function serializeBlockLike(node: ts.SourceFile | ts.Block, offset: number) {
        const statements = serializeList(node.statements)
        buf[offset + 0] = node.kind
        buf[offset + 1] = next
        buf[offset + 2] = statements
        buf[offset + 3] = 0
    }

    function serializeFunctionDeclLike(node: ts.FunctionExpression | ts.FunctionDeclaration | ts.MethodDeclaration, offset: number) {
        let flags = 0

        if (node.asteriskToken) {
            flags |= InternalNodeFlags.Generator
        }

        const name = serializeNode(node.name)
        const parameters = serializeList(node.parameters)
        const body = serializeNode(node.body)
        const type = serializeNode(node.type)
        buf[offset + 0] = ((getFlags(node, flags)) << 12) | node.kind
        buf[offset + 1] = next
        buf[offset + 2] = name
        buf[offset + 3] = parameters
        buf[offset + 4] = body
        buf[offset + 5] = 0 // type params
        buf[offset + 6] = type
    }

    function serializeConditionalExpression(node: ts.ConditionalExpression, offset: number) {
        const condition = serializeNode(node.condition)
        const whenTrue = serializeNode(node.whenTrue)
        const whenFalse = serializeNode(node.whenFalse)
        buf[offset + 0] = SyntaxKind.ConditionalExpression
        buf[offset + 1] = next
        buf[offset + 2] = condition
        buf[offset + 3] = whenTrue
        buf[offset + 4] = whenFalse
    }

    function serializeSwitchStatement(node: ts.SwitchStatement, offset: number) {
        const expression = serializeNode(node.expression)
        const caseBlock = serializeList(node.caseBlock.clauses)
        buf[offset + 0] = SyntaxKind.SwitchStatement
        buf[offset + 1] = next
        buf[offset + 2] = expression
        buf[offset + 3] = caseBlock
    }

    function serializeCaseClause(node: ts.CaseClause | ts.DefaultClause, offset: number) {
        const expression = serializeNode((node as ts.CaseClause).expression as ts.Expression | undefined)
        const statements = serializeList(node.statements)
        buf[offset + 0] = node.kind
        buf[offset + 1] = next
        buf[offset + 2] = expression
        buf[offset + 3] = 0
        buf[offset + 4] = statements
    }

    function serializeForStatement(node: ts.ForStatement, offset: number) {
        const initializer = serializeNode(node.initializer)
        const condition = serializeNode(node.condition)
        const incrementor = serializeNode(node.incrementor)
        const statement = serializeNode(node.statement)
        buf[offset + 0] = node.kind
        buf[offset + 1] = next
        buf[offset + 2] = initializer
        buf[offset + 3] = condition
        buf[offset + 4] = incrementor
        buf[offset + 5] = statement
    }

    function serializeForOfStatement(node: ts.ForInStatement | ts.ForOfStatement, offset: number) {
        let flags = 0
        if ((node as ts.ForOfStatement).awaitModifier) {
            flags |= InternalNodeFlags.Async
        }

        const initializer = serializeNode(node.initializer)
        const expression = serializeNode(node.expression)
        const statement = serializeNode(node.statement)
        buf[offset + 0] = (flags << 12) | node.kind
        buf[offset + 1] = next
        buf[offset + 2] = initializer
        buf[offset + 3] = expression
        buf[offset + 4] = statement
    }

    function serializeClassLikeDeclaration(node: ts.ClassDeclaration | ts.ClassExpression, offset: number) {
        const name = serializeNode(node.name)
        const members = serializeList(node.members)
        const typeParameters = serializeList(node.typeParameters)
        const extendsClause = serializeNode(node.heritageClauses?.filter(n => n.token !== SyntaxKind.ImplementsKeyword)?.[0]?.types[0])
        const implementsClauses = serializeList(node.implementsClauses ?? node.heritageClauses?.filter(n => n.token === SyntaxKind.ImplementsKeyword)?.[0]?.types)
        buf[offset + 0] = (getFlags(node) << 12) | node.kind
        buf[offset + 1] = next
        buf[offset + 2] = name
        buf[offset + 3] = members
        buf[offset + 4] = extendsClause
        buf[offset + 5] = typeParameters
        buf[offset + 6] = implementsClauses
    }

    return {
        serializeNode,
        get buffer() {
            return nodeBuffer.subarray(0, index * 32)
        },
        get heap() {
            return heap.subarray(0, heapPointer)
        },
    }
}

export enum EmitHint {
    SourceFile = 0,
    Expression = 1,
    IdentifierName = 2,
    MappedTypeParameter = 3,
    Unspecified = 4,
    EmbeddedStatement = 5,
    JsxAttributeValue = 6,
    ImportTypeNodeAttributes = 7,
}

class FactoryNode {
    private _flags: number | undefined

    public constructor(
        public readonly kind: SyntaxKind,
        public readonly original?: ts.Node | AstNode
    ) {}

    set flags(val) {
        this._flags = val
    }

    get flags() {
        return this._flags ?? 0
    }

    public getSourceFile() {
        return undefined
    }

    public forEachChild(visitor: any) {
        return forEachChild(this as any as ts.Node, visitor)
    }
}

function createFactory(): ts.NodeFactory {
    function createNode<T extends ts.Node = ts.Node>(kind: SyntaxKind, original?: ts.Node | AstNode): { -readonly [P in keyof T]: T[P] } {
        return new FactoryNode(kind, original) as any
    }

    function createToken<T>(kind: SyntaxKind) {
        return new FactoryNode(kind) as T
    }

    function createEmptyStatement(original?: ts.Node) {
        return createNode(SyntaxKind.EmptyStatement, original)
    }

    function createIdentifier(text: string) {
        const node = createNode<ts.Identifier>(SyntaxKind.Identifier)
        node.text = text
        
        return node
    }

    function createBlock(statements: ts.Node[]) {
        return {
            kind: SyntaxKind.Block,
            statements,
        }
    }

    function createUnaryExpressionKind(kind: SyntaxKind, expression: ts.Expression) {
        const node = createNode<ts.Node & { expression: ts.Expression }>(kind)
        node.expression = expression
        
        return node
    }

    function createBinaryExpression(left: ts.Expression, operatorToken: SyntaxKind | ts.BinaryOperatorToken, right: ts.Expression) {
        const node = createNode<ts.BinaryExpression>(SyntaxKind.BinaryExpression)
        node.left = left
        node.operatorToken = typeof operatorToken === 'number' ? createToken(operatorToken) : operatorToken
        node.right = right

        return node
    }

    function createSourceFile(statements: ts.Statement[], original?: ts.Node) {
        const node = createNode<ts.SourceFile>(SyntaxKind.SourceFile, original)
        node.statements = statements as any
        node.fileName = (original as ts.SourceFile)?.fileName ?? ''

        return node
    }

    function createCallExpression(expression, typeArguments, argumentsArray = [], questionDotToken?: ts.Token<ts.SyntaxKind.QuestionDotToken>, original?: ts.Node) {
        const node = createNode<ts.CallExpression>(SyntaxKind.CallExpression, original)
        node.expression =  expression
        node.arguments = argumentsArray
        node.typeArguments = typeArguments
        node.questionDotToken = questionDotToken

        return node
    }

    function createMethodDeclaration(modifiers: readonly ts.ModifierLike[] | undefined, asteriskToken: ts.AsteriskToken | undefined, name: string | ts.PropertyName, questionToken: ts.QuestionToken | undefined, typeParameters: readonly ts.TypeParameterDeclaration[] | undefined, parameters: readonly ts.ParameterDeclaration[], type: ts.TypeNode | undefined, body: ts.Block | undefined, original?: ts.Node) {
        const node = createNode<ts.MethodDeclaration>(SyntaxKind.MethodDeclaration, original)
        node.modifiers =  modifiers
        node.asteriskToken = asteriskToken
        node.name = typeof name === 'string' ? createIdentifier(name) : name
        node.questionToken = questionToken
        node.parameters = parameters
        node.body = body

        return node
    }

    function createPropertyAccessExpression(expression: ts.LeftHandSideExpression, name: string | ts.Identifier | ts.PrivateIdentifier, original?: ts.Node) {
        const node = createNode<ts.PropertyAccessExpression>(SyntaxKind.PropertyAccessExpression, original)
        node.expression = expression
        node.name = typeof name === 'string' ? createIdentifier(name) : name
        
        return node
    }

    function createShorthandPropertyAssignment(name: string | ts.Identifier) {
        const node = createNode<ts.ShorthandPropertyAssignment>(SyntaxKind.ShorthandPropertyAssignment)
        node.name = typeof name === 'string' ? createIdentifier(name) : name

        return node
    }

    function createObjectLiteralExpression(properties: ts.Node[] = [], multiLine, original?: ts.Node) {
        const node = createNode<ts.ObjectLiteralExpression>(SyntaxKind.ObjectLiteralExpression, original)
        node.properties = properties
        
        return node
    }
   
    function createPrefixUnaryExpression(operator: ts.PrefixUnaryOperator, operand: ts.UnaryExpression, original?: ts.Node) {
        const node = createNode<ts.PrefixUnaryExpression>(SyntaxKind.PrefixUnaryExpression, original)
        node.operator = operator
        node.operand = operand
        
        return node
    }

    function cloneNode(node: ts.Node) {
        switch (node.kind) {
            case SyntaxKind.Identifier:
                return createIdentifier(node.text)
            case SyntaxKind.PropertyAccessExpression:
                return createPropertyAccessExpression(node.expression, node.name)
        }
        throw new Error(`${node.kind}`)
    }

    function createNumericLiteral(value, flags): ts.NumericLiteral {
        return { 
            kind: SyntaxKind.NumericLiteral,
            text: value,
            flags,
        }
    }

    function createClassExpression(modifiers: readonly ts.ModifierLike[] | undefined, name: string | ts.Identifier | undefined, typeParameters: readonly ts.TypeParameterDeclaration[] | undefined, heritageClauses: readonly ts.HeritageClause[] | undefined, members: readonly ts.ClassElement[], original?: ts.Node): ts.ClassExpression {
        const node = createNode<ts.ClassExpression>(SyntaxKind.ClassExpression, original)
        node.modifiers = modifiers
        node.name = typeof name === 'string' ? createIdentifier(name) : name
        node.heritageClauses = heritageClauses
        node.members = members
        node.typeParameters = typeParameters
        
        return node
    }

    function createNamespaceImport(name: ts.Identifier, original?: ts.Node): ts.NamespaceImport {
        const node = createNode<ts.NamespaceImport>(SyntaxKind.NamespaceImport, original)
        node.name = name
        
        return node
    }

    function createTypeReferenceNode(typeName: string | ts.EntityName, typeArguments?: readonly ts.TypeNode[], original?: ts.Node): ts.TypeReferenceNode {
        const node = createNode<ts.TypeReferenceNode>(SyntaxKind.TypeReference, original)
        node.typeName = typeof typeName === 'string' ? createIdentifier(typeName) : typeName
        node.typeArguments = typeArguments
        
        return node
    }

    function createExpressionWithTypeArguments(expression: ts.Expression, typeArguments: readonly ts.TypeNode[] | undefined, original?: ts.Node): ts.ExpressionWithTypeArguments {
        const node = createNode<ts.ExpressionWithTypeArguments>(SyntaxKind.ExpressionWithTypeArguments, original)
        node.expression = expression
        node.typeArguments = typeArguments
        
        return node
    }

    function createModuleDeclaration(...args: Parameters<ts.NodeFactory['createModuleDeclaration']>) {
        const node = createNode<ts.ModuleDeclaration>(SyntaxKind.ModuleDeclaration)
        node.modifiers = args[0]
        node.name = args[1]
        node.body = args[2]
        node.flags = args[3]
        
        return node
    }

    function createModuleBlock(statements: ts.Statement[]) {
        const node = createNode<ts.ModuleBlock>(SyntaxKind.ModuleBlock)
        node.statements = statements
        
        return node
    }

    function createExportDeclaration(modifiers: readonly ts.ModifierLike[] | undefined, isTypeOnly: boolean, exportClause: ts.NamedExportBindings | undefined, moduleSpecifier?: ts.Expression, attributes?: ImportAttributes, original?: ts.Node) {
        const node = createNode<ts.ExportDeclaration>(SyntaxKind.ExportDeclaration, original)
        node.modifiers = modifiers
        node.exportClause = exportClause
        node.moduleSpecifier = moduleSpecifier
        node.attributes = attributes
        node.isTypeOnly = isTypeOnly
        
        return node 
    }

    function createNamespaceExport(name: ts.ModuleExportName, original?: ts.Node) {
        const node = createNode<ts.NamespaceExport>(SyntaxKind.NamespaceExport, original)
        node.name = name
        
        return node 
    }

    function createNamedExports(elements: readonly ts.ExportSpecifier[], original?: ts.Node) {
        const node = createNode<ts.NamedExports>(SyntaxKind.NamedExports, original)
        node.elements = elements
        
        return node    
    }

    function createExportSpecifier(isTypeOnly: boolean, propertyName: string | ts.ModuleExportName | undefined, name: string | ts.ModuleExportName, original?: ts.Node) {
        const node = createNode<ts.ExportSpecifier>(SyntaxKind.ExportSpecifier, original)
        node.isTypeOnly = isTypeOnly
        node.propertyName = typeof propertyName === 'string' ? createIdentifier(propertyName) : propertyName
        node.name = typeof name === 'string' ? createIdentifier(name) : name
        
        return node
    }

    function createRegularExpressionLiteral(text: string, original?: ts.Node) {
        const node = createNode<ts.RegularExpressionLiteral>(SyntaxKind.RegularExpressionLiteral, original)
        node.text = text
        
        return node
    }

    function createArrayLiteralExpression(elements: ts.Expression[] = [], multiLine: boolean, original?: ts.Node) {
        const node = createNode<ts.ArrayLiteralExpression>(SyntaxKind.ArrayLiteralExpression, original)
        node.elements = elements
        
        return node
    }

    function createPropertySignature(modifiers: readonly ts.Modifier[] | undefined, name: ts.PropertyName | string, questionToken: ts.QuestionToken | undefined, type: ts.TypeNode | undefined, original?: ts.Node) {
        const node = createNode<ts.PropertySignature>(SyntaxKind.PropertySignature, original)
        node.modifiers = modifiers
        node.name = typeof name === 'string' ? createIdentifier(name) : name
        node.questionToken = questionToken
        node.type = type

        return node
    }

    function createInterfaceDeclaration(modifiers: readonly ts.ModifierLike[] | undefined, name: string | ts.Identifier, typeParameters: readonly ts.TypeParameterDeclaration[] | undefined, heritageClauses: readonly ts.HeritageClause[] | undefined, members: readonly ts.TypeElement[], original?: ts.Node) {
        const node = createNode<ts.InterfaceDeclaration>(SyntaxKind.InterfaceDeclaration, original)
        node.modifiers = modifiers
        node.name = typeof name === 'string' ? createIdentifier(name) : name
        node.typeParameters = typeParameters
        node.heritageClauses = heritageClauses
        node.members = members

        return node
    }

    function createArrayTypeNode(elementType: ts.TypeNode, original?: ts.Node) {
        const node = createNode<ts.ArrayTypeNode>(SyntaxKind.ArrayType, original)
        node.elementType = elementType

        return node
    }

    function createConstructorDeclaration(modifiers: readonly ts.ModifierLike[] | undefined, parameters: readonly ts.ParameterDeclaration[], body: ts.Block | undefined, original?: ts.Node) {
        const node = createNode<ts.ConstructorDeclaration>(SyntaxKind.Constructor, original)
        node.modifiers = modifiers
        node.parameters = parameters
        node.body = body

        return node
    }

    function createPropertyDeclaration(modifiers: readonly ts.ModifierLike[] | undefined, name: string | ts.PropertyName, questionOrExclamationToken: ts.QuestionToken | ts.ExclamationToken | undefined, type: ts.TypeNode | undefined, initializer: ts.Expression | undefined, original?: ts.Node) {
        const node = createNode<ts.PropertyDeclaration>(SyntaxKind.PropertyDeclaration, original)
        node.modifiers = modifiers
        node.name = typeof name === 'string' ? createIdentifier(name) : name
        node.exclamationToken = questionOrExclamationToken?.kind !== SyntaxKind.QuestionToken ? questionOrExclamationToken : undefined
        node.questionToken = questionOrExclamationToken?.kind === SyntaxKind.QuestionToken ? questionOrExclamationToken : undefined
        node.type = type
        node.initializer = initializer

        return node
    }

    function createClassDeclaration(modifiers: readonly ts.ModifierLike[] | undefined, name: string | ts.Identifier | undefined, typeParameters: readonly ts.TypeParameterDeclaration[] | undefined, heritageClauses: readonly ts.HeritageClause[] | undefined, members: readonly ts.ClassElement[], original?: ts.Node) {
        const node = createNode<ts.ClassDeclaration>(SyntaxKind.ClassDeclaration, original)
        node.modifiers = modifiers
        node.name = typeof name === 'string' ? createIdentifier(name) : name
        node.typeParameters = typeParameters
        node.heritageClauses = heritageClauses
        node.members = members

        return node
    }

    function createPropertyAssignment(name: string | ts.Identifier, initializer?: ts.Expression, original?: ts.Node) {
        const node = createNode<ts.PropertyAssignment>(SyntaxKind.PropertyAssignment, original)
        node.name = typeof name === 'string' ? createIdentifier(name) : name
        node.initializer = initializer

        return node
    }

    function createSpreadElement(expression: ts.Expression, original?: ts.Node) {
        const node = createNode<ts.SpreadElement>(SyntaxKind.SpreadElement, original)
        node.expression = expression

        return node
    }

    function createTemplateExpression(head: ts.TemplateHead, templateSpans: ts.TemplateExpression['templateSpans'], original?: ts.Node) {
        const node = createNode<ts.TemplateExpression>(SyntaxKind.TemplateExpression, original)
        node.head = head
        node.templateSpans = templateSpans

        return node
    }

    function createTemplateSpan(expression: ts.Expression, literal: ts.TemplateSpan['literal'], original?: ts.Node) {
        const node = createNode<ts.TemplateSpan>(SyntaxKind.TemplateSpan, original)
        node.expression = expression
        node.literal = literal

        return node
    }

    function updateWrapperNode(expression: ts.Expression, original: ts.Node) {
        const node = createNode<ts.Node>(original.kind, original)
        node.expression = expression

        return node
    }

    function createStringLiteral(text: string, isSingleQuote?: boolean) {
        const node = createNode<ts.StringLiteral>(SyntaxKind.StringLiteral)
        node.text = text.toString()
        node.isSingleQuote = isSingleQuote ?? !text.includes('\'')

        return node
    }

    function createEnumMember(name: string | ts.PropertyName, initializer?: ts.Expression, original?: ts.Node) {
        const node = createNode<ts.EnumMember>(SyntaxKind.EnumMember, original)
        node.name = typeof name === 'string' ? createIdentifier(name) : name
        node.initializer = initializer

        return node
    }

    function createEnumDeclaration(modifiers: ts.ModifierLike[] | undefined, name: string | ts.Identifier, members: readonly ts.EnumMember[], original?: ts.Node) {
        const node = createNode<ts.EnumDeclaration>(SyntaxKind.EnumDeclaration, original)
        node.name = typeof name === 'string' ? createIdentifier(name) : name
        node.modifiers = modifiers
        node.members = members

        return node
    }

    function createTypeLiteralNode(members: ts.TypeElement[] | undefined, original?: ts.Node) {
        const node = createNode<ts.TypeLiteralNode>(SyntaxKind.TypeLiteral, original)
        node.members = members ?? []

        return node
    }

    function createUnionTypeNode(types: readonly ts.TypeNode[], original?: ts.Node) {
        const node = createNode<ts.UnionTypeNode>(SyntaxKind.UnionType, original)
        node.types = types

        return node
    }

    function createTypeAliasDeclaration(modifiers: readonly ts.ModifierLike[] | undefined, name: string | ts.Identifier, typeParameters: readonly ts.TypeParameterDeclaration[] | undefined, type: ts.TypeNode, original?: ts.Node) {
        const node = createNode<ts.TypeAliasDeclaration>(SyntaxKind.TypeAliasDeclaration, original)
        node.name = typeof name === 'string' ? createIdentifier(name) : name
        node.modifiers = modifiers
        node.typeParameters = typeParameters
        node.type = type

        return node
    }

    function createClassStaticBlockDeclaration(body: ts.Block) {
        const node = createNode<ts.ClassStaticBlockDeclaration>(SyntaxKind.ClassStaticBlockDeclaration)
        node.body = body

        return node
    }

    function updateClassStaticBlockDeclaration(original: ts.ClassStaticBlockDeclaration, body: ts.Block) {
        const node = createNode<ts.ClassStaticBlockDeclaration>(SyntaxKind.ClassStaticBlockDeclaration, original)
        node.body = body

        return node
    }

    function createElementAccessExpression(expression: ts.LeftHandSideExpression, argumentExpression: ts.Expression | number, original?: ts.ElementAccessExpression) {
        const node = createNode<ts.ElementAccessExpression>(SyntaxKind.ElementAccessExpression, original)
        node.expression = expression
        node.argumentExpression = typeof argumentExpression === 'number' ? createNumericLiteral(argumentExpression, 0) : argumentExpression

        return node
    }

    function createVariableDeclaration(name, exclamationToken, type, initializer) {
        const ident = typeof name === 'string' ? createIdentifier(name) : name
        return {
            kind: SyntaxKind.VariableDeclaration,
            name: ident,
            exclamationToken,
            type, 
            initializer,
        }
    }

    function createCatchClause(variableDeclaration: string | ts.VariableDeclaration | ts.BindingName | undefined, block: ts.Block, original?: ts.CatchClause) {
        const node = createNode<ts.CatchClause>(SyntaxKind.CatchClause, original)
        node.variableDeclaration = typeof variableDeclaration === 'string' || (variableDeclaration && variableDeclaration.kind !== SyntaxKind.VariableDeclaration) 
            ? createVariableDeclaration(variableDeclaration) 
            : variableDeclaration
        node.block = block
        return node
    }

    function createTryStatement(tryBlock: ts.Block, catchClause: ts.CatchClause | undefined, finallyBlock: ts.Block | undefined, original?: ts.TryStatement) {
        const node = createNode<ts.TryStatement>(SyntaxKind.TryStatement, original)
        node.tryBlock = tryBlock
        node.catchClause = catchClause
        node.finallyBlock = finallyBlock
        return node
    }

    function createArrowFunction(modifiers, typeParameters, parameters, type, _, body) {
        const n = createNode<ts.ArrowFunction>(SyntaxKind.ArrowFunction)
        n.body = body.kind === SyntaxKind.ObjectLiteralExpression ? factory.createParenthesizedExpression(body) : body
        n.modifiers = modifiers
        n.parameters = parameters
        n.typeParameters = typeParameters
        n.type = type
        return n
    }

    function createImmediatelyInvokedArrowFunction(statements: readonly ts.Statement[]) {
        const body = factory.createBlock(statements, true)
        const fn = createArrowFunction(undefined, undefined, undefined, undefined, undefined, body)

        return createCallExpression(
            { kind: SyntaxKind.ParenthesizedExpression, expression: fn } as any as ts.ParenthesizedExpression,
            undefined,
            []
        )
    }

    return {
        cloneNode,
        createToken,
        createTrue: () => createToken(SyntaxKind.TrueKeyword),
        createFalse: () => createToken(SyntaxKind.FalseKeyword),
        createThis: () => createToken(SyntaxKind.ThisKeyword),
        createSuper: () => createToken(SyntaxKind.SuperKeyword),
        createNull: () => createToken(SyntaxKind.NullKeyword),
        createModifier: createToken,
        createEmptyStatement,
        createNotEmittedStatement: (node: ts.Statement) => createEmptyStatement(node),
        // createOmittedExpression: (node) => createEmptyStatement(node),
        createPrefixUnaryExpression,
        createIdentifier,
        createNumericLiteral,
        createBlock,
        createShorthandPropertyAssignment,
        createSourceFile: (statements: ts.Statement[]) => {
            return createSourceFile(statements)
        },
        updateSourceFile: (node: ts.Node, statements: ts.Statement[]) => {
            return createSourceFile(statements, node)
        },
        createSpreadElement,
        updateSpreadElement: (node: ts.Node, expression: ts.Expression) => {
            return createSpreadElement(expression, node)
        },
        createTemplateExpression,
        updateTemplateExpression: (node: ts.Node, head: ts.TemplateHead, templateSpans: ts.TemplateExpression['templateSpans']) => {
            return createTemplateExpression(head, templateSpans, node)
        },
        createTemplateSpan,
        updateTemplateSpan: (node: ts.Node, expression: ts.Expression, literal: ts.TemplateSpan['literal']) => {
            return createTemplateSpan(expression, literal, node)
        },
        createParenthesizedExpression: (expression: ts.Expression) => createUnaryExpressionKind(SyntaxKind.ParenthesizedExpression, expression),
        createComputedPropertyName: (expression: ts.Expression) => createUnaryExpressionKind(SyntaxKind.ComputedPropertyName, expression),
        createAwaitExpression: (expression: ts.Expression) => createUnaryExpressionKind(SyntaxKind.AwaitExpression, expression),
        createSpreadAssignment: (expression: ts.Expression) => createUnaryExpressionKind(SyntaxKind.SpreadAssignment, expression),
        createVoidZero: () => createUnaryExpressionKind(SyntaxKind.VoidExpression, createNumericLiteral(0, 0)),
        createIfStatement: (expression, thenStatement, elseStatement) => {
            const node = createNode<ts.IfStatement>(SyntaxKind.IfStatement)
            node.expression = expression
            node.thenStatement = thenStatement
            node.elseStatement = elseStatement
            return node
        },
        createExportDeclaration,
        createNamespaceExport,
        createNamedExports,
        createExportSpecifier,
        createRegularExpressionLiteral,
        createMethodDeclaration,
        createClassExpression,
        createNamespaceImport,
        createTypeReferenceNode,
        createExpressionWithTypeArguments,
        createModuleDeclaration,
        createModuleBlock,
        createPropertySignature,
        createInterfaceDeclaration,
        createArrayTypeNode,
        createConstructorDeclaration,
        createPropertyDeclaration,
        createClassDeclaration,
        createEnumDeclaration,
        createEnumMember,
        createTypeLiteralNode,
        createUnionTypeNode,
        createTypeAliasDeclaration,
        updateExpressionWithTypeArguments(node, expression, typeArguments) {
            return createExpressionWithTypeArguments(expression, typeArguments, node)
        },
        updateWrapperNode,
        updateHeritageClause(node: ts.Node, types: readonly ts.ExpressionWithTypeArguments[]) {
            const n = createNode<ts.HeritageClause>(SyntaxKind.HeritageClause, node)
            n.types = types
            n.token = node.token
            
            return n
        },
        createVariableDeclarationList: (declarations, flags) => {
            const node = createNode<ts.VariableDeclarationList>(SyntaxKind.VariableDeclarationList)
            node.declarations = declarations
            node.flags = flags
            return node
        },
        updateVariableDeclarationList: (node, declarations) => {
            const n = createNode<ts.VariableDeclarationList>(SyntaxKind.VariableDeclarationList, node)
            n.declarations = declarations
            n.flags = node.flags
            return n
        },
        createReturnStatement: (expression) => {
            return {
                kind: SyntaxKind.ReturnStatement,
                expression,
            }
        },
        createThrowStatement: (expression) => {
            return {
                kind: SyntaxKind.ThrowStatement,
                expression,
            }
        },
        createFunctionExpression: (modifiers, asteriskToken, name, typeParameters, parameters, type, body) => {
            const ident = typeof name === 'string' ? createIdentifier(name) : name
            return {
                kind: SyntaxKind.FunctionExpression,
                name: ident,
                type,
                parameters, 
                body,
                modifiers,
                flags: 0, // XXX
            }
        },
        createExportAssignment: (modifiers: readonly ts.ModifierLike[] | undefined, isExportEquals: boolean | undefined, expression: ts.Expression) => {
            return {
                kind: SyntaxKind.ExportAssignment,
                isExportEquals,
                expression,
                modifiers,
            }
        },
        createStrictEquality: (left: ts.Expression, right: ts.Expression) => {
            return createBinaryExpression(left, SyntaxKind.EqualsEqualsEqualsToken, right)
        },
        createAssignment: (left: ts.Expression, right: ts.Expression) => {
            return createBinaryExpression(left, SyntaxKind.EqualsToken, right)
        },
        createExpressionStatement: (expression: ts.Expression) => {
            return {
                kind: SyntaxKind.ExpressionStatement,
                expression,
            }
        },
        createStringLiteral,
        createImportClause: (isTypeOnly: boolean, name: ts.Identifier | undefined, namedBindings: ts.NamedImportBindings | undefined) => {
            return {
                kind: SyntaxKind.ImportClause,
                name,
                namedBindings,
                isTypeOnly,
            }
        },
        createImportDeclaration: (modifiers: readonly ts.ModifierLike[] | undefined, importClause: ts.ImportClause | undefined, moduleSpecifier: ts.Expression, attributes?: ts.ImportAttributes) => {
            const node = createNode<ts.ImportDeclaration>(SyntaxKind.ImportDeclaration)
            node.modifiers =  modifiers ?? []
            node.importClause = importClause
            node.moduleSpecifier = moduleSpecifier
            node.attributes = attributes
            
            return node
        },
        createExportDefault: (expression: ts.Expression) => {
            const node = createNode<ts.ExportAssignment>(SyntaxKind.ExportAssignment)
            node.expression = expression
            return node
        },
        createFunctionDeclaration: (modifiers, asteriskToken, name, typeParameters, parameters, type, body) => {
            const node = createNode<ts.FunctionDeclaration>(SyntaxKind.FunctionDeclaration)
            node.name = typeof name === 'string' ? createIdentifier(name) : name
            node.type = type
            node.parameters = parameters
            node.body = body
            node.modifiers = modifiers
            return node
        },
        updateFunctionDeclaration: (node, modifiers, asteriskToken, name, typeParameters, parameters, type, body) => {
            const ident = typeof name === 'string' ? createIdentifier(name) : name
            return {
                kind: SyntaxKind.FunctionDeclaration,
                original: node,
                name: ident,
                type,
                parameters, 
                body,
                modifiers,
            }
        },
        createVariableDeclaration,
        updateVariableDeclaration: (node, name, exclamationToken, type, initializer) => {
            const ident = typeof name === 'string' ? createIdentifier(name) : name
            return {
                kind: SyntaxKind.VariableDeclaration,
                original: node,
                name: ident,
                exclamationToken,
                type,
                initializer,
            }
        },
        createVariableStatement: (modifiers, declarationList) => {
            if ((declarationList.flags & InternalNodeFlags.Const) === InternalNodeFlags.Const) {
                modifiers = [...(modifiers ?? []), createToken(SyntaxKind.ConstKeyword)]
            } else if ((declarationList.flags & InternalNodeFlags.Let) === InternalNodeFlags.Let) {
                modifiers = [...(modifiers ?? []), createToken(SyntaxKind.LetKeyword)]
            }
            const node = createNode<ts.VariableStatement>(SyntaxKind.VariableStatement)
            node.modifiers = modifiers
            node.declarationList = declarationList
            node.flags = declarationList.flags
            return node
        },
        updateVariableStatement: (node, modifiers, declarationList) => {
            const s = {
                kind: SyntaxKind.VariableStatement,
                original: node,
                modifiers,
                declarationList,
                flags: node.flags,
            }

            declarationList.parent = s

            return s
        },
        createParameterDeclaration: (modifiers, dotDotDotToken, name, questionToken, type, initializer) => {
            const ident = typeof name === 'string' ? createIdentifier(name) : name
            return {
                kind: SyntaxKind.Parameter,
                modifiers,
                name: ident,
                questionToken,
                dotDotDotToken,
                type,
                initializer,
                flags: 0, // XXX
            }
        },
        updateParameterDeclaration: (node, modifiers, dotDotDotToken, name, questionToken, type, initializer) => {
            const ident = typeof name === 'string' ? createIdentifier(name) : name
            return {
                kind: SyntaxKind.Parameter,
                original: node,
                modifiers,
                name: ident,
                questionToken,
                dotDotDotToken,
                type,
                initializer,
            }
        },
        createCallChain: (expression, questionDotToken, typeArguments, argumentsArray = []) => {
            return createCallExpression(expression, typeArguments, argumentsArray, questionDotToken)
        },
        createCallExpression: (expression, typeArguments, argumentsArray = []) => {            
            return createCallExpression(expression, typeArguments, argumentsArray)
        },
        updateCallExpression: (node, expression, typeArguments, argumentsArray = []) => {
            return createCallExpression(expression, typeArguments, argumentsArray, node.questionDotToken, node)
        },
        createNewExpression: (expression, typeArguments, argumentsArray = []) => {
            return {
                kind: SyntaxKind.NewExpression,
                expression,
                arguments: argumentsArray,
                typeArguments,
            }
        },
        updateNewExpression: (node, expression, typeArguments, argumentsArray = []) => {
            return {
                kind: SyntaxKind.NewExpression,
                original: node,
                expression,
                arguments: argumentsArray,
                typeArguments,
                // location: node.location,
            }
        },
        createArrowFunction,
        updateArrowFunction: (node, modifiers, typeParameters, parameters, type, _, body) => {
            const n = createNode<ts.ArrowFunction>(SyntaxKind.ArrowFunction, node)
            n.body = body.kind === SyntaxKind.ObjectLiteralExpression ? factory.createParenthesizedExpression(body) : body
            n.modifiers = modifiers
            n.parameters = parameters
            n.typeParameters = typeParameters
            n.type = type
            return n
        },
        createArrayLiteralExpression,
        updateArrayLiteralExpression: (node, elements) => {
            return createArrayLiteralExpression(elements, true, node)
        },
        createObjectLiteralExpression,
        updateObjectLiteralExpression(node, props, multiLine) {
            return createObjectLiteralExpression(props, multiLine, node)
        },
        createPropertyAccessExpression,
        createElementAccessExpression,
        updateElementAccessExpression: (node, expression, argumentExpression) => {
            return createElementAccessExpression(expression, argumentExpression, node)
        },
        updatePropertyAssignment: (node, name, initializer) => {
            return createPropertyAssignment(name, initializer, node)
        },
        createPropertyAssignment,
        createBinaryExpression,
        updateBinaryExpression: (node, left, operatorToken, right) => {
            return {
                kind: SyntaxKind.BinaryExpression,
                original: node,
                left,
                operatorToken,
                right,
            }
        },
        updateSourceFile: (node, statements) => {
            return {
                kind: SyntaxKind.SourceFile,
                original: node,
                statements,
                fileName: node.fileName,
            }
        },
        updateBlock: (node, statements) => {
            return {
                kind: SyntaxKind.Block,
                original: node,
                statements,
            }
        },
        updateClassExpression: (node, modifiers, name, typeParameters, heritageClauses, members) => {
            return {
                kind: SyntaxKind.ClassExpression,
                original: node,
                name,
                members,
                heritageClauses,
                typeParameters,
            }
        },
        updateClassDeclaration: (node, modifiers, name, typeParameters, heritageClauses, members) => {
            return {
                kind: SyntaxKind.ClassDeclaration,
                original: node,
                modifiers,
                name,
                members,
                heritageClauses,
                typeParameters,
            }
        },
        updatePropertyAccessExpression: (node, expression, name) => {
            return createPropertyAccessExpression(expression, name, node)
        },
        createClassStaticBlockDeclaration,
        updateClassStaticBlockDeclaration,
        createCatchClause,
        createTryStatement,
        createImmediatelyInvokedArrowFunction,
    } as any
}

export const factory = createFactory()

function getListStart(node: AstNode, field: string) {
    switch (node.kind) {
        case SyntaxKind.Block:
        case SyntaxKind.SourceFile:
            if (field === 'statements') {
                return node.l
            }
            break

        case SyntaxKind.NewExpression:
        case SyntaxKind.CallExpression:
            if (field === 'arguments') {
                return node.r
            }
            break

        case SyntaxKind.Constructor:
            if (field === 'parameters') {
                return node.l
            }
            break

        case SyntaxKind.GetAccessor:
        case SyntaxKind.SetAccessor:
        case SyntaxKind.MethodDeclaration:
        case SyntaxKind.FunctionExpression:
        case SyntaxKind.FunctionDeclaration:
            if (field === 'parameters') {
                return node.r
            }
            break

        case SyntaxKind.ArrowFunction:
            if (field === 'parameters') {
                return node.l
            }
            break

        case SyntaxKind.SwitchStatement:
            if (field === 'caseBlock') {
                return node.r
            }
            break

        case SyntaxKind.DefaultClause:
        case SyntaxKind.CaseClause:
            if (field === 'statements') {
                return node.len
            }
            break

        case SyntaxKind.VariableDeclarationList:
            if (field === 'declarations') {
                return node.l
            }
            break

        case SyntaxKind.ObjectLiteralExpression:
            if (field === 'properties') {
                return node.l
            }
            break

        case SyntaxKind.ArrayBindingPattern:
        case SyntaxKind.ObjectBindingPattern:
        case SyntaxKind.ArrayLiteralExpression:
            if (field === 'elements') {
                return node.l
            }
            break

        case SyntaxKind.ClassExpression:
        case SyntaxKind.ClassDeclaration:
            if (field === 'members') {
                return node.r
            } else if (field === 'typeParameters') {
                return node.extra
            } else if (field === 'implementsClauses') {
                return node.extra2
            }
            break     

        case SyntaxKind.TemplateExpression:
            if (field === 'templateSpans') {
                return node.head!.next
            }
            break   
    }

    throw new Error(`Not implemented: kind: ${node.kind}, field: ${field}`)
}

type TsNode = Omit<ts.Node, 'kind'> & { kind: SyntaxKind }

function visitArray(
    arr: readonly ts.Node[] | undefined,
    visitor: (node: ts.Node) => ts.Node | undefined,
    isForEachChild: boolean
) {
    if (arr === undefined || arr.length === 0) {
        return
    }

    if (isForEachChild) {
        for (let i = 0; i < arr.length; i++) {
            const r = visitor(arr[i])
            if (r) {
                return r
            }
        }
        return
    }

    let didChange = false
    const statements: ts.Node[] = new Array(arr.length)
    for (let i = 0; i < arr.length; i++) {
        const r = visitor(arr[i])
        if (r === undefined || r === arr[i]) {
            statements[i] = arr[i]
            continue
        }

        didChange = true
        statements[i] = r
    }

    return didChange ? statements : undefined
}

// comptime=isForEachChild 
export function visitEachChild(node: ts.Node, visitor: (node: ts.Node) => ts.Node | undefined, _?: any, isForEachChild = false): ts.Node | undefined {
    function visitNode(node: ts.Node | undefined) {
        if (node === undefined) {
            return
        }

        const r = visitor(node)
        if (isForEachChild) return r

        return r === node ? undefined : r
    }

    type ArrayFields<T> = { [P in keyof T]: T[P] extends any[] | undefined ? P : never }[keyof T]

    function visitArrayField<K extends ArrayFields<AstNode>>(node: ts.Node | AstNode, field: K) {
        return visitArray((node as any)[field], visitor, isForEachChild)
    }

    switch (node.kind as any as SyntaxKind) {
        case SyntaxKind.SourceFile: {
            const statements = visitArrayField(node, 'statements')
            if (isForEachChild) return statements

            if (statements) {
                return factory.updateSourceFile(node, statements)
            }

            break
        }

        case SyntaxKind.Block: {
            const statements = visitArrayField(node, 'statements')
            if (isForEachChild) return statements

            if (statements !== undefined) {
                return {
                    kind: SyntaxKind.Block,
                    original: node,
                    statements,
                }
            }

            break
        }

        case SyntaxKind.CallExpression:
        case SyntaxKind.NewExpression: {
            const expression = visitNode(node.expression)
            if (isForEachChild && expression) return expression

            const argumentsArray = visitArrayField(node, 'arguments')
            if (isForEachChild) return argumentsArray

            if (expression !== undefined || argumentsArray !== undefined) {
                if (node.kind == SyntaxKind.CallExpression) {
                    return factory.updateCallExpression(node, expression ?? node.expression, undefined, argumentsArray ?? node.arguments)
                }
                return factory.updateNewExpression(node, expression ?? node.expression, undefined, argumentsArray ?? node.arguments)
            }

            break
        }

        case SyntaxKind.CatchClause: {
            const variableDeclaration = visitNode(node.variableDeclaration)
            if (isForEachChild && variableDeclaration) return variableDeclaration

            const block = visitNode(node.block)
            if (isForEachChild) return block

            if (variableDeclaration || block) {
                return {
                    kind: SyntaxKind.CatchClause,
                    original: node,
                    variableDeclaration: variableDeclaration ?? node.variableDeclaration,
                    block: block ?? node.block,
                }
            }

            break
        }

        case SyntaxKind.TryStatement: {
            const tryBlock = visitNode(node.tryBlock)
            if (isForEachChild && tryBlock) return tryBlock

            const catchClause = visitNode(node.catchClause)
            if (isForEachChild && catchClause) return catchClause

            const finallyBlock = visitNode(node.finallyBlock)
            if (isForEachChild) return finallyBlock

            if (tryBlock || catchClause || finallyBlock) {
                return {
                    kind: SyntaxKind.TryStatement,
                    original: node,
                    tryBlock: tryBlock ?? node.tryBlock,
                    catchClause: catchClause ?? node.catchClause,
                    finallyBlock: finallyBlock ?? node.finallyBlock,
                }
            }

            break
        }

        case SyntaxKind.ComputedPropertyName:
        case SyntaxKind.SpreadElement:
        case SyntaxKind.SpreadAssignment:
        case SyntaxKind.DeleteExpression:
        case SyntaxKind.VoidExpression:
        case SyntaxKind.TypeOfExpression:
        case SyntaxKind.ParenthesizedExpression:
        case SyntaxKind.YieldExpression:
        case SyntaxKind.AwaitExpression:
        case SyntaxKind.ThrowStatement:
        case SyntaxKind.ReturnStatement:
        case SyntaxKind.ExpressionStatement: {
            const expression = visitNode(node.expression)
            if (isForEachChild) return expression

            if (expression !== undefined) {
                return factory.updateWrapperNode(expression, node)
            }

            break
        }

        case SyntaxKind.ConditionalExpression: {
            const condition = visitNode(node.condition)
            if (isForEachChild && condition) return condition

            const whenTrue = visitNode(node.whenTrue)
            if (isForEachChild && whenTrue) return whenTrue

            const whenFalse = visitNode(node.whenFalse)
            if (isForEachChild) return whenFalse

            if (condition || whenFalse || whenFalse) {
                return {
                    kind: node.kind,
                    original: node,
                    condition: condition ?? node.condition,
                    whenTrue: whenTrue ?? node.whenTrue,
                    whenFalse: whenFalse ?? node.whenFalse,
                }
            }

            break
        }

        case SyntaxKind.IfStatement: {
            const expression = visitNode(node.expression)
            if (isForEachChild && expression) return expression

            const thenStatement = visitNode(node.thenStatement)
            if (isForEachChild && thenStatement) return thenStatement

            const elseStatement = visitNode(node.elseStatement)
            if (isForEachChild) return elseStatement

            if (expression || thenStatement || elseStatement) {
                return {
                    kind: SyntaxKind.IfStatement,
                    original: node,
                    expression: expression ?? node.expression,
                    thenStatement: thenStatement ?? node.thenStatement,
                    elseStatement: elseStatement ?? node.elseStatement,
                }
            }

            break
        }

        case SyntaxKind.DoStatement: {
            const statement = visitNode(node.statement)
            if (isForEachChild && statement) return statement

            const expression = visitNode(node.expression)
            if (isForEachChild) return expression

            if (expression || statement) {
                return {
                    kind: SyntaxKind.DoStatement,
                    original: node,
                    expression: expression ?? node.expression,
                    statement: statement ?? node.statement,
                }
            }

            break
        }

        case SyntaxKind.WhileStatement: {
            const expression = visitNode(node.expression)
            if (isForEachChild && expression) return expression

            const statement = visitNode(node.statement)
            if (isForEachChild) return statement

            if (expression || statement) {
                return {
                    kind: SyntaxKind.WhileStatement,
                    original: node,
                    expression: expression ?? node.expression,
                    statement: statement ?? node.statement,
                }
            }

            break
        }

        case SyntaxKind.Parameter: {
            const name = visitNode(node.name)
            if (isForEachChild && name) return name

            const initializer = visitNode(node.initializer)
            if (isForEachChild) return initializer

            if (name || initializer) {
                return {
                    kind: node.kind,
                    original: node,
                    name: name ?? node.name,
                    initializer: initializer ?? node.initializer,
                    dotDotDotToken: node.dotDotDotToken,
                    questionToken: node.questionToken,
                    flags: node.flags,
                }
            }

            break
        }

        case SyntaxKind.GetAccessor:
        case SyntaxKind.SetAccessor: {
            const name = visitNode(node.name)
            if (isForEachChild && name) return name

            const body = visitNode(node.body)
            if (isForEachChild && body) return body

            const parameters = visitArrayField(node, 'parameters')
            if (isForEachChild) return parameters

            if (name || body || parameters) {
                return {
                    kind: node.kind,
                    original: node,
                    name: name ?? node.name,
                    body: body ?? node.body,
                    parameters: parameters ?? node.parameters,
                    modifiers: node.modifiers,
                }
            }

            break
        }

        case SyntaxKind.PropertyDeclaration: {
            const name = visitNode((node as ts.PropertyDeclaration).name)
            if (isForEachChild && name) return name

            const initializer = visitNode((node as ts.PropertyDeclaration).initializer)
            if (isForEachChild) return initializer

            if (name || initializer) {
                return {
                    kind: node.kind,
                    original: node,
                    modifiers: node.modifiers,
                    name: name ?? node.name,
                    initializer: initializer ?? node.initializer,
                }
            }
            break
        }

        case SyntaxKind.MethodDeclaration:
        case SyntaxKind.FunctionExpression:
        case SyntaxKind.FunctionDeclaration: {
            const name = visitNode(node.name)
            if (isForEachChild && name) return name

            const parameters = visitArrayField(node, 'parameters')
            if (isForEachChild && parameters) return parameters

            const body = visitNode(node.body)
            if (isForEachChild) return body

            if (name || body || parameters) {
                return {
                    kind: node.kind,
                    original: node,
                    name: name ?? node.name,
                    body: body ?? node.body,
                    parameters: parameters ?? node.parameters,
                    asteriskToken: node.asteriskToken,
                    modifiers: node.modifiers,
                }
            }

            break
        }

        case SyntaxKind.Constructor: {
            const parameters = visitArrayField(node, 'parameters')
            if (isForEachChild && parameters) return parameters

            const body = visitNode(node.body)
            if (isForEachChild) return body

            if (body || parameters) {
                return {
                    kind: SyntaxKind.Constructor,
                    original: node,
                    body: body ?? node.body,
                    parameters: parameters ?? node.parameters,
                }
            }

            break
        }

        case SyntaxKind.ArrowFunction: {
            const parameters = visitArrayField(node, 'parameters')
            if (isForEachChild && parameters) return parameters

            const body = visitNode(node.body)
            if (isForEachChild) return body

            if (body || parameters) {
                return factory.updateArrowFunction(node, node.modifiers, undefined, parameters ?? node.parameters, undefined, undefined, body ?? node.body)
            }

            break
        }

        case SyntaxKind.DefaultClause: {
            const statements = visitArrayField(node, 'statements')
            if (isForEachChild) return statements

            if (statements) {
                return {
                    kind: SyntaxKind.DefaultClause,
                    original: node,
                    statements,
                }
            }
            break
        }

        case SyntaxKind.CaseClause: {
            const expression = visitNode(node.expression)
            if (isForEachChild && expression) return expression

            const statements = visitArrayField(node, 'statements')
            if (isForEachChild) return statements

            if (expression || statements) {
                return {
                    kind: SyntaxKind.CaseClause,
                    original: node,
                    expression: expression ?? node.expression,
                    statements: statements ?? node.statements,
                }
            }
            break
        }

        case SyntaxKind.SwitchStatement: {
            const expression = visitNode(node.expression)
            if (isForEachChild && expression) return expression

            const caseBlock = visitArray((node as any)['caseBlock'].clauses, visitor, isForEachChild)
            if (isForEachChild) return caseBlock

            if (expression || caseBlock) {
                return {
                    kind: node.kind,
                    original: node,
                    expression: expression ?? node.expression,
                    caseBlock: caseBlock ? { kind: SyntaxKind.CaseBlock, clauses: caseBlock, original: node.caseBlock } : node.caseBlock,
                }
            }
            break
        }

        case SyntaxKind.BinaryExpression: {
            const left = visitNode(node.left)
            if (isForEachChild && left) return left

            const right = visitNode(node.right)
            if (isForEachChild) return right

            if (left || right) {
                return {
                    kind: SyntaxKind.BinaryExpression,
                    original: node,
                    operatorToken: node.operatorToken,
                    left: left ?? node.left,
                    right: right ?? node.right,
                }
            }
            break
        }

        case SyntaxKind.ForStatement: {
            const initializer = visitNode(node.initializer)
            if (isForEachChild && initializer) return initializer

            const condition = visitNode(node.condition)
            if (isForEachChild && condition) return condition

            const incrementor = visitNode(node.incrementor)
            if (isForEachChild && incrementor) return incrementor

            const statement = visitNode(node.statement)
            if (isForEachChild) return statement

            if (initializer || condition || incrementor || statement) {
                return {
                    kind: SyntaxKind.ForStatement,
                    original: node,
                    initializer: initializer ?? node.initializer,
                    condition: condition ?? node.condition,
                    incrementor: incrementor ?? node.incrementor,
                    statement: statement ?? node.statement,
                }
            }
            break
        }

        case SyntaxKind.ForOfStatement:
        case SyntaxKind.ForInStatement: {
            const initializer = visitNode(node.initializer)
            if (isForEachChild && initializer) return initializer

            const expression = visitNode(node.expression)
            if (isForEachChild && expression) return expression

            const statement = visitNode(node.statement)
            if (isForEachChild) return statement
 
            if (initializer || expression || statement) {
                return {
                    kind: node.kind,
                    original: node,
                    initializer: initializer ?? node.initializer,
                    expression: expression ?? node.expression,
                    statement: statement ?? node.statement,
                }
            }
            break
        }

        case SyntaxKind.VariableDeclarationList: {
            const declarations = visitArrayField(node, 'declarations')
            if (isForEachChild) return declarations
  
            if (declarations !== undefined) {
                return factory.updateVariableDeclarationList(node, declarations) 
            }
            break
        }

        case SyntaxKind.VariableStatement: {
            const declarationList = visitNode(node.declarationList)
            if (isForEachChild) return declarationList

            if (declarationList !== undefined) {
                return factory.updateVariableStatement(node, node.modifiers, declarationList)
            }
            break
        }

        case SyntaxKind.VariableDeclaration: {
            const name = visitNode(node.name)
            if (isForEachChild && name) return name

            const initializer = visitNode(node.initializer)
            if (isForEachChild) return initializer

            if (name || initializer) {
                return factory.updateVariableDeclaration(
                    node,
                    name ?? node.name,
                    undefined,
                    undefined, // type
                    initializer ?? node.initializer,
                )
            }
            break
        }

        case SyntaxKind.ObjectLiteralExpression: {
            const properties = visitArrayField(node, 'properties')
            if (isForEachChild) return properties

            if (properties !== undefined) {
                return factory.updateObjectLiteralExpression(node, properties)
            }
            break
        }

        case SyntaxKind.ArrayLiteralExpression: {
            const elements = visitArrayField(node, 'elements')
            if (isForEachChild) return elements

            if (elements !== undefined) {
                return factory.updateArrayLiteralExpression(node, elements)
            }
            break
        }

        case SyntaxKind.PropertyAssignment: {
            const name = visitNode(node.name)
            if (isForEachChild && name) return name
 
            const initializer = visitNode(node.initializer)
            if (isForEachChild) return initializer

            if (name || initializer) {
                return factory.updatePropertyAssignment(node, name ?? node.name, initializer ?? node.initializer)
            }
            break
        }

        case SyntaxKind.PropertyAccessExpression: {
            const expression = visitNode(node.expression)
            if (isForEachChild && expression) return expression
            
            const name = visitNode(node.name)
            if (isForEachChild) return name

            if (expression !== undefined || name !== undefined) {
                return factory.updatePropertyAccessExpression(
                    node,
                    expression ?? node.expression,
                    name ?? node.name,
                )
            }
            break
        }

        case SyntaxKind.ElementAccessExpression: {
            const expression = visitNode(node.expression)
            if (isForEachChild && expression) return expression
            
            const argumentExpression = visitNode(node.argumentExpression)
            if (isForEachChild) return argumentExpression

            if (expression || argumentExpression) {
                return factory.updateElementAccessExpression(
                    node,
                    expression ?? node.expression,
                    argumentExpression ?? node.argumentExpression,
                )
            }
            break
        }

        case SyntaxKind.ClassExpression:
        case SyntaxKind.ClassDeclaration: {
            const name = visitNode(node.name)
            if (isForEachChild && name) return name

            const members = visitArrayField(node, 'members')
            if (isForEachChild && members) return members

            const extendsClause = visitNode(node.extendsClause)
            if (isForEachChild) return extendsClause

            // const implementsClauses = visitArrayField(node, 'implementsClauses')

            if (name || members || extendsClause) {
                return {
                    kind: node.kind,
                    original: node,
                    modifiers: node.modifiers,
                    name: name ?? node.name,
                    members: members ?? node.members,
                    heritageClauses: extendsClause ? [extendsClause] : node.heritageClauses,
                }
            }
            break
        }

        case SyntaxKind.PrefixUnaryExpression: {
            const operand = visitNode(node.operand)
            if (isForEachChild) return operand

            if (operand) {
                return {
                    kind: SyntaxKind.PrefixUnaryExpression,
                    original: node,
                    operator: node.operator,
                    operand,
                }
            }
            break
        }

        case SyntaxKind.PostfixUnaryExpression: {
            const operand = visitNode(node.operand)
            if (isForEachChild) return operand

            if (operand) {
                return {
                    kind: SyntaxKind.PostfixUnaryExpression,
                    original: node,
                    operator: node.operator,
                    operand,
                }
            }
            break
        }

        case SyntaxKind.SatisfiesExpression:
        case SyntaxKind.AsExpression: {
            const expression = visitNode(node.expression)
            if (isForEachChild) return expression

            if (expression) {
                return {
                    kind: node.kind, // SyntaxKind.AsExpression,
                    original: node,
                    expression,
                    type: node.type, // TODO: visit type
                }
            }
            break
        }

        case SyntaxKind.TemplateSpan: {
            const expression = visitNode(node.expression)
            if (isForEachChild) return expression

            // TODO: visit literal (?)
            if (expression) {
                return factory.updateTemplateSpan(node, expression, node.literal)
            }
            break
        }

        case SyntaxKind.TemplateExpression: {
            const templateSpans = visitArrayField(node, 'templateSpans')
            if (isForEachChild) return templateSpans

            if (templateSpans) {
                return factory.updateTemplateExpression(node, node.head, templateSpans)
            }
            break
        }

        case SyntaxKind.BindingElement: {
            const name = visitNode(node.name)
            if (isForEachChild && name) return name

            const initializer = visitNode(node.initializer)
            if (isForEachChild && initializer) return initializer

            const propertyName = visitNode(node.propertyName)
            if (isForEachChild) return propertyName

            if (name || initializer || propertyName) {
                return {
                    kind: SyntaxKind.BindingElement,
                    original: node,
                    name: name ?? node.name,
                    initializer: initializer ?? node.initializer,
                    propertyName: propertyName ?? node.propertyName,   
                    dotDotDotToken: node.dotDotDotToken,
                }
            }
            break
        }

        case SyntaxKind.ObjectBindingPattern: {
            const elements = visitArrayField(node, 'elements')
            if (isForEachChild) return elements

            if (elements) {
                return {
                    kind: SyntaxKind.ObjectBindingPattern,
                    original: node,
                    elements,
                }
            }
            break
        }

        case SyntaxKind.ArrayBindingPattern: {
            const elements = visitArrayField(node, 'elements')
            if (isForEachChild) return elements

            if (elements) {
                return {
                    kind: SyntaxKind.ArrayBindingPattern,
                    original: node,
                    elements,
                }
            }
            break
        }

        case SyntaxKind.ExpressionWithTypeArguments: {
            const expression = visitNode((node as ts.ExpressionWithTypeArguments).expression)
            if (isForEachChild) return expression

            if (expression) {
                return {
                    kind: SyntaxKind.ExpressionWithTypeArguments,
                    original: node,
                    expression,
                    typeArguments: node.typeArguments,
                }
            }
            break
        }

        case SyntaxKind.ShorthandPropertyAssignment: {
            const name = visitNode((node as ts.ShorthandPropertyAssignment).name)
            if (isForEachChild) return name

            // TODO: objectAssignmentInitializer?: Expression;

            if (name) {
                return {
                    kind: SyntaxKind.ShorthandPropertyAssignment,
                    original: node,
                    name,
                }
            }
            break
        }

        case SyntaxKind.HeritageClause: {
            const types = visitArrayField(node, 'types')
            if (isForEachChild) return types

            if (types) {
                return {
                    kind: SyntaxKind.HeritageClause,
                    original: node,
                    token: node.token,
                    types,
                }
            }
            break
        }

        case SyntaxKind.EnumDeclaration: {
            const name = visitNode((node as ts.EnumDeclaration).name)
            if (isForEachChild && name) return name

            const members = visitArrayField(node, 'members')
            if (isForEachChild) return members

            if (name || members) {
                return {
                    kind: SyntaxKind.EnumDeclaration,
                    original: node,
                    name: name ?? node.name,
                    members: members ?? node.members,
                    modifiers: node.modifiers,
                }
            }
            break
        }

        case SyntaxKind.EnumMember: {
            const name = visitNode((node as ts.EnumMember).name)
            if (isForEachChild && name) return name

            const initializer = visitNode((node as ts.EnumMember).initializer)
            if (isForEachChild) return initializer

            if (name || initializer) {
                return {
                    kind: SyntaxKind.EnumMember,
                    original: node,
                    name: name ?? node.name,
                    initializer: initializer ?? node.initializer,
                }
            }
            break
        }

        case SyntaxKind.LabeledStatement: {
            const statement = visitNode((node as ts.LabeledStatement).statement)
            if (isForEachChild) return statement

            if (statement) {
                return {
                    kind: SyntaxKind.LabeledStatement,
                    original: node,
                    label: node.label,
                    statement,
                }
            }
            break
        }

        case SyntaxKind.ClassStaticBlockDeclaration: {
            const body = visitNode((node as ts.ClassStaticBlockDeclaration).body)
            if (isForEachChild) return body

            if (body) {
                return factory.updateClassStaticBlockDeclaration(node, body)
            }
            break
        }

        case SyntaxKind.ExportDeclaration:
        case SyntaxKind.ImportDeclaration:
            break // TOOD?

        // TODO: (optionally) visit type nodes
        // the keyword nodes probably don't need to be visited yet
        case SyntaxKind.ImportKeyword:
        case SyntaxKind.ThisKeyword:
        case SyntaxKind.SuperKeyword:
        case SyntaxKind.NullKeyword:
        case SyntaxKind.UndefinedKeyword:
        case SyntaxKind.TemplateHead:
        case SyntaxKind.NoSubstitutionTemplateLiteral:
        case SyntaxKind.TypeLiteral:
        case SyntaxKind.UnionType:
        case SyntaxKind.TypeReference:
        case SyntaxKind.StringKeyword:
        case SyntaxKind.FalseKeyword:
        case SyntaxKind.TrueKeyword:
        case SyntaxKind.ContinueStatement:
        case SyntaxKind.BreakStatement:
        case SyntaxKind.EmptyStatement:
        case SyntaxKind.Identifier:
        case SyntaxKind.PrivateIdentifier:
        case SyntaxKind.NumericLiteral:
        case SyntaxKind.StringLiteral:
        case SyntaxKind.RegularExpressionLiteral:
        case SyntaxKind.InterfaceDeclaration:
        case SyntaxKind.TypeAliasDeclaration:
        case SyntaxKind.ModuleDeclaration: // FIXME: This should be visited esp. if it's a namespace decl
        case SyntaxKind.OmittedExpression:
            break

        default:
            if (node.kind === SyntaxKind.DeferStatement) {
                const statement = visitNode(node.statement ?? node.left)
                if (isForEachChild) return statement

                if (statement) {
                    return {
                        kind: SyntaxKind.DeferStatement,
                        original: node,
                        statement: statement,
                    }
                }

                break
            }

            if (node.kind === SyntaxKind.ReifyExpression) {
                break
            }

            if (node.kind === SyntaxKind.ExportAssignment) {
                const expression = visitNode((node as ts.ExportAssignment).expression)
                if (isForEachChild) return expression

                if (expression) {
                    const n = factory.createExportAssignment((node as ts.ExportAssignment).modifiers, (node as ts.ExportAssignment).isExportEquals, expression as ts.Expression)
                    ;(n as any).original = node
                    return n
                }

                break
            }

            if (!node.kind) {
                console.log(node)
                throw new Error(`unknown node`)
            }

            throw new Error(`Missing kind: ${node.kind}`)
    }

    return isForEachChild ? undefined : node
}

export function setSourceMapRange(node: ts.Node, range: ts.SourceMapRange | AstNode | undefined) {
    if (!range) {
        throw new Error('TODO: unset source map range')
    }
    if (node instanceof AstNode) {
        return node // TODO
    }
    if (range instanceof AstNode) {
        // if (range.location === 0) {
        //     console.log('MISSING LOCATION', range.kind)
        // }
        // 219
        // 263
        // 211
        ;(node as any).location = range.location
        // TODO: we don't handle `end`
        return node
    }
    ;(node as any).location = range.location
    return node
}

export function setTextRange(node: ts.Node, range: ts.SourceMapRange) {
    return node
}

export function getOriginalNode(node: ts.Node): ts.Node {
    while ((node as any).original) {
        node = (node as any).original
    }
    return node
}

class SourceFileInternal {
    public readonly kind: SyntaxKind

    private _view?: Uint32Array
    private _node?: AstNode
    private _lineMap?: number[] | Uint32Array
    private _fullText?: string

    constructor(
        public readonly handle: any, 
        public readonly source: Uint8Array,
        public readonly ref: number,
        public readonly fileName: string
    ) {
        this.kind = SyntaxKind.SourceFile
    }

    get view(): Uint32Array {
        return this._view ??= new Uint32Array(api.serialize(this.handle))
    }

    get node() {
        if (this._node) return this._node

        const n = new AstNode(this.ref, this.view, this.source)
        ;(n as any)._sourceFile = this
        ;(n as any).fileName = this.fileName
        ;(n as any).isDeclarationFile = this.isDeclarationFile

        return this._node = n
    }
    
    get statements() {
        return this.node.statements
    }

    get text() {
        return this.getFullText()
    }

    get isDeclarationFile() {
        return !!this.fileName.match(/\.d(?:\..+)?\.ts$/)
    }

    public forEachChild<T>(
        cbNode: (node: ts.Node) => T | undefined, 
        cbNodeArray?: (nodes: ts.NodeArray<ts.Node>) => T | undefined
    ): T | undefined {
        return forEachChild(this as any as ts.Node, cbNode, cbNodeArray)
    }

    getSourceFile() {
        return this
    }

    getFullText() {
        if (this._fullText !== undefined) {
            return this._fullText
        }

        const b = this.source instanceof Buffer ? this.source : Buffer.from(this.source.buffer, this.source.byteOffset, this.source.byteLength)

        return this._fullText = b.toString('utf-8') // XXX
    }

    getLineMap() {
        if (this._lineMap) return this._lineMap

        return this._lineMap = getLineMap(this as any)

        // const m = this._lineMap = [0]
        // const text = this.getFullText()

        // for (let i = 0; i < text.length; i++) {
        //     if (text[i] === '\n') {
        //         m.push(i + 1)
        //     }
        // }

        // return m
    }
}

export enum NodeFlags {
    None = 0,
    Let = 1,
    Const = 2,
    Using = 4,
    AwaitUsing = 6,
    NestedNamespace = 8,
    Synthesized = 16,
    Namespace = 32,
    OptionalChain = 64,
    ExportContext = 128,
    ContainsThis = 256,
    HasImplicitReturn = 512,
    HasExplicitReturn = 1024,
    GlobalAugmentation = 2048,
    HasAsyncFunctions = 4096,
    DisallowInContext = 8192,
    YieldContext = 16384,
    DecoratorContext = 32768,
    AwaitContext = 65536,
    DisallowConditionalTypesContext = 131072,
    ThisNodeHasError = 262144,
    JavaScriptFile = 524288,
    ThisNodeOrAnySubNodesHasError = 1048576,
    HasAggregatedChildData = 2097152,
    JSDoc = 16777216,
    JsonFile = 134217728,
    BlockScoped = 7,
    Constant = 6,
    ReachabilityCheckFlags = 1536,
    ReachabilityAndEmitFlags = 5632,
    ContextFlags = 101441536,
    TypeExcludesFlags = 81920,
}

export enum ScriptTarget {
    /** @deprecated */
    ES3 = 0,
    ES5 = 1,
    ES2015 = 2,
    ES2016 = 3,
    ES2017 = 4,
    ES2018 = 5,
    ES2019 = 6,
    ES2020 = 7,
    ES2021 = 8,
    ES2022 = 9,
    ES2023 = 10,
    ESNext = 99,
    JSON = 100,
    Latest = 99,
}

export enum ModuleKind {
    None = 0,
    CommonJS = 1,
    AMD = 2,
    UMD = 3,
    System = 4,
    ES2015 = 5,
    ES2020 = 6,
    ES2022 = 7,
    ESNext = 99,
    Node16 = 100,
    NodeNext = 199,
    Preserve = 200,
}

export function createSourceFile(fileName: string, sourceText: string | Uint8Array | undefined, languageVersionOrOptions: ts.ScriptTarget | ts.CreateSourceFileOptions, setParentNodes?: boolean, scriptKind?: ts.ScriptKind) {
    const data = typeof sourceText === 'string' ? Buffer.from(sourceText, 'utf-8') : (sourceText ?? fs.readFileSync(fileName))
    const handle = api.createSourceFile(fileName.toString(), data, false)

    return new SourceFileInternal(handle, data, api.getStartRef(handle), fileName).node as any as ts.SourceFile
}

function getRootTypesFile(dir: string, host: Pick<ts.System, 'readFile'>): string {
    const pkgJsonText = host.readFile(path.resolve(dir, 'package.json'), 'utf-8')
    if (!pkgJsonText) {
        throw new Error(`Missing package.json: ${dir}`)
    }

    const pkgJson = JSON.parse(pkgJsonText)
    if (!pkgJson.types) {
        throw new Error(`Missing "types" field in package.json: ${dir}`)
    }

    return path.resolve(dir, pkgJson.types)
}

export function createCompilerHost(options: ts.CompilerOptions, setParentNodes?: boolean): ts.CompilerHost {
    const typesDirCache = new Map<string, string>()
    const moduleResolveCache = new Map<string, string>()
    const fileExistsCache = new Map<string, string>()

    function fileExists(filename: string) {
        const cached = fileExistsCache.get(filename)
        if (cached !== undefined) {
            return cached
        }

        const result = sys.fileExists(filename)
        fileExistsCache.set(filename, result)
        return result
    }

    function resolveTypesDirective(name: string, dir: string): string | undefined {
        const d = path.resolve(dir, 'node_modules', '@types', name)
        try {
            return getRootTypesFile(d, sys)
        } catch (e) {
            if (path.dirname(dir) === dir) {
                return
            }

            return resolveTypesDirective(name, path.dirname(dir))
        }
    }

    process.on('unhandledRejection', console.log)

    async function createSourceFileAsync(fileName: string, isLib: boolean) {
        // Node's `fs/promises` impl. is poorly designed for smaller files 
        // due to running every syscall as a microtask
        //
        // It seems doing this from the JS side is faster
        const data = sys.readFile(fileName, 'bytes')
        if (data === undefined) {
            throw new Error(`file not found: ${fileName}`)
        }
        const sf = await api.createSourceFileAsync(fileName, data, isLib)
        sf._source = data
        sf._fileName = fileName

        return sf
    }

    async function parseBatch(files: string[]) {
        const results = files.map(f => requestSourceFile(f))

        return await Promise.all(results)
    }

    const fileHandles = new Map<string, any>() // FIXME: don't expose this
    const requestSourceFile = async (fileName: string, isLib: boolean = false) => {
        if (fileHandles.has(fileName)) {
            return fileHandles.get(fileName)!
        }

        const sf = createSourceFileAsync(fileName, isLib).then(r => {
            fileHandles.set(fileName, r)
            return r
        })
        fileHandles.set(fileName, sf)

        return sf
    }

    const enum ResolveMode { Lib, Types, Path, Module } 

    const getLibDir = () => path.dirname(sys.getExecutingFilePath())

    const resolveFilePath = (mode: ResolveMode, value: string, origin?: string) => {
        if (path.isAbsolute(value)) {
            return value
        }

        switch (mode) {
            case ResolveMode.Lib:
                if (!value || value === 'lib.d.ts') {
                    return path.resolve(getLibDir(), 'lib.d.ts')
                }

                // Normalization
                value = value.replace(/^lib\.(.+)\.d\.ts$/, '$1')

                if (value == 'esnext.asynciterable') {
                    value = 'es2018.asynciterable'
                }
                if (value == 'esnext.symbol') {
                    value = 'es2019.symbol'
                }
                if (value == 'esnext.bigint') {
                    value = 'es2020.bigint'
                }

                // xxx
                if (value === 'es2022' && !origin) {
                    value = 'es2022.full'
                }

                return path.resolve(getLibDir(), `lib.${value}.d.ts`)

            case ResolveMode.Module: {
                if (!origin) {
                    throw new Error(`Missing origin file`)
                }

                // Bare specifier
                if (!value.startsWith('.')) {
                    if (value.startsWith('node:') || value.startsWith('synapse:')) return

                    function getPkgOrigin() {
                        // brittle
                        const prefix = 'node_modules'
                        const keyIndex = origin!.lastIndexOf(prefix)
                        if (keyIndex === -1) return sys.getCurrentDirectory()

                        const nextSep = origin!.indexOf('/', keyIndex+prefix.length+1)

                        return origin!.slice(0, nextSep)
                    }

                    const pkgOrigin = getPkgOrigin()
                    const key = `${pkgOrigin}::${value}`
                    const cached = moduleResolveCache.get(key)
                    if (cached) {
                        return cached
                    }

                    const m = resolveModuleName(value, origin, options, sys)
                    const result = m.resolvedModule?.resolvedFileName
                    moduleResolveCache.set(key, result)

                    return result
                }

                const base = path.resolve(path.dirname(origin), value === '..' ? 'index.ts' : value)
                if (base.endsWith('.ts')) {
                    return base
                }

                const extname = path.extname(origin)
                if (extname === '.ts') {
                    if (fileExists(`${base}.ts`)) {
                        return `${base}.ts`
                    }
                    if (fileExists(`${base}/index.ts`)) {
                        return `${base}/index.ts`
                    }
                } else if (extname === '.syn') {
                    if (fileExists(`${base}.syn`)) {
                        return `${base}.syn`
                    }
                }

                if (fileExists(`${base}.d.ts`)) {
                    return `${base}.d.ts`
                }

                const extname2 = path.extname(base)
                if (extname2) {
                    const base2 = base.slice(0, -extname2.length)
                    if (fileExists(`${base2}.d${extname2}.ts`)) {
                        return `${base2}.d${extname2}.ts`
                    }
                }

                if (extname !== '.ts') {
                    if (fileExists(`${base}.ts`)) {
                        return `${base}.ts`
                    }
                    if (fileExists(`${base}/index.ts`)) {
                        return `${base}/index.ts`
                    }
                }

                return    
            }

            case ResolveMode.Path: {
                if (!origin) {
                    throw new Error(`Missing origin file`)
                }

                const base = path.resolve(path.dirname(origin), value)
                if (base.endsWith('.ts')) {
                    return base
                }

                if (fileExists(`${base}.d.ts`)) {
                    return `${base}.d.ts`
                }

                return
            }

            case ResolveMode.Types: {
                const dir = origin ? path.dirname(origin) : sys.getCurrentDirectory()
                const key = `${dir}::${value}`
                const cached = typesDirCache.get(key)
                if (cached) {
                    return cached
                }

                const result = resolveTypesDirective(value, dir)
                if (!result) {
                    throw new Error(`Failed to find types for "${value}" from ${dir}`)
                }
                typesDirCache.set(key, result)

                return result
            }
        }
    }

    const handle = api.createCompilerHost({ requestSourceFile, resolveFilePath })

    return {
        ...sys,
        _handle: handle,
        fileHandles,
        parseBatch,
        getSourceFile: (fileName, versionOrOpt, onError, _) => {
            try {
                const h = waitForPromise(requestSourceFile(fileName))
                if (!h._sourceFile) {
                    h._sourceFile = new SourceFileInternal(h, h._source, api.getStartRef(h), h._fileName)
                }
    
                return h._sourceFile
            } catch (e) {
                throw new Error(`Failed to get source file: ${fileName}`, { cause: e })
            }
        },
    } as any as ts.CompilerHost
}

function waitForPromise(o: any) {
    if (!(o instanceof Promise)) {
        return o
    }
    setTimeout(() => {}, 0) // XXX: hack for a bug, already resolved promises can cause stalls...
    return api.waitForPromise(o)
}

interface InternalApiSourceFileHandle {
    _source: Uint8Array
    _fileName: string
    _sourceFile?: SourceFileInternal
}

export function resolveModuleName(
    moduleName: string, 
    containingFile: string, 
    compilerOptions: ts.CompilerOptions, 
    host: ts.ModuleResolutionHost, 
    cache?: ts.ModuleResolutionCache, 
    redirectedReference?: ts.ResolvedProjectReference, 
    resolutionMode?: ts.ResolutionMode
): ts.ResolvedModuleWithFailedLookupLocations {
    function tryGetDeclaration(base: string) {
        const ext = path.extname(base)
        if (ext === '.ts') {
            return base
        }

        const withoutExt = !ext ? base : base.slice(0, -ext.length)

        if (!ext && host.fileExists(path.join(`${withoutExt}`, 'index.ts'))) {
            return path.join(`${withoutExt}`, 'index.ts')
        }

        if (host.fileExists(`${withoutExt}.d.ts`)) {
            return `${withoutExt}.d.ts`
        }

        if (host.fileExists(`${withoutExt}.ts`)) {
            return `${withoutExt}.ts`
        }
    }

    if (moduleName.startsWith('.')) {
        const base = path.resolve(path.dirname(containingFile), moduleName)
        const resolved = tryGetDeclaration(base)

        if (!resolved) {
            return { resolvedModule: undefined }
        }

        return {
            resolvedModule: {
                resolvedFileName: resolved,
                get extension() {
                    return path.extname(resolved)
                },
            },
        }
    }

    function maybeGetSubpath() {
        const ind = moduleName.indexOf('/')
        if (ind === -1) return ''

        if (!moduleName.startsWith('@')) {
            return moduleName.slice(ind)
        }

        const ind2 = moduleName.indexOf('/', ind+1)
        if (ind2 === -1) return ''

        return moduleName.slice(ind2)
    }

    if (compilerOptions.paths) {
        // TODO: wildcard
        const arr = compilerOptions.paths[moduleName]
        if (arr?.length) {
            const resolvedFileName = arr[0]
            return {
                resolvedModule: {
                    isExternalLibraryImport: false,
                    resolvedFileName: resolvedFileName,
                    extension: path.extname(resolvedFileName),
                }
            }
        }
    }

    const subpath = moduleName.startsWith('@') 
        ? moduleName.split('/').slice(2).join('/') 
        : moduleName.split('/').slice(1).join('/')

    const trueModuleName = subpath ? moduleName.slice(0, -(subpath.length + 1)) : moduleName
    const rootDir = host.getCurrentDirectory?.() ?? process.cwd()

    function searchForPackage(dir: string) {
        const p = path.resolve(dir, 'node_modules', trueModuleName, 'package.json')
    
        const d = host.readFile(p)
        if (d !== undefined) {
            const pkgJson = JSON.parse(d)

            return {
                filePath: p,
                pkgJson,
            }
        }

        if (dir === rootDir) return

        const next = path.dirname(dir)
        if (next !== dir) {
            return searchForPackage(next)
        }
    }

    const pkg = searchForPackage(host.getCurrentDirectory?.() ?? process.cwd())
    if (!pkg) return { resolvedModule: undefined }

    const exports = pkg.pkgJson.exports
    if (typeof exports === 'string') {
        const resolvedFileName = tryGetDeclaration(path.resolve(path.dirname(pkg.filePath), exports))
        if (resolvedFileName) {
            return {
                resolvedModule: {
                    isExternalLibraryImport: true,
                    resolvedFileName: resolvedFileName,
                    extension: path.extname(resolvedFileName),
                }
            }
        }
    } else if (typeof exports === 'object') {    
        let resolved: string
        if (!subpath) {
            resolved = exports['.']
        } else {
            resolved = exports['./' + subpath]
        }

        if (typeof resolved === 'string') {
            const resolvedFileName = tryGetDeclaration(path.resolve(path.dirname(pkg.filePath), resolved))
            if (resolvedFileName) {
                return {
                    resolvedModule: {
                        isExternalLibraryImport: true,
                        resolvedFileName: resolvedFileName,
                        extension: path.extname(resolvedFileName),
                    }
                }
            }
        }
    }

    const typings = pkg.pkgJson.types ?? pkg.pkgJson.typings
    if (typeof typings === 'string') {
        const resolvedFileName = tryGetDeclaration(path.resolve(path.dirname(pkg.filePath), typings))
        if (resolvedFileName) {
            return {
                resolvedModule: {
                    isExternalLibraryImport: true,
                    resolvedFileName: resolvedFileName,
                    extension: path.extname(resolvedFileName),
                }
            }
        }
    }

    return {
        resolvedModule: undefined,
    }
}

export function isStatement(node: AstNode) {
    switch (node.kind) {
        case SyntaxKind.FunctionDeclaration:
        case SyntaxKind.ClassDeclaration:
        case SyntaxKind.VariableStatement:
        case SyntaxKind.ExpressionStatement:
        case SyntaxKind.ExportDeclaration:
        case SyntaxKind.ImportDeclaration:
        case SyntaxKind.IfStatement:
        case SyntaxKind.DoStatement:
        case SyntaxKind.WhileStatement:
        case SyntaxKind.ForStatement:
        case SyntaxKind.ForOfStatement:
        case SyntaxKind.ForInStatement:
        case SyntaxKind.SwitchStatement:
        case SyntaxKind.ReturnStatement:
        case SyntaxKind.BreakStatement:
        case SyntaxKind.ContinueStatement:
        case SyntaxKind.LabeledStatement:
        case SyntaxKind.ThrowStatement:
            return true
    }

    return false
}
export function isFunctionLike(node: ts.Node) {
    function isFunctionLikeDeclarationKind(kind) {
        switch (kind) {
          case 262 /* FunctionDeclaration */:
          case 174 /* MethodDeclaration */:
          case 176 /* Constructor */:
          case 177 /* GetAccessor */:
          case 178 /* SetAccessor */:
          case 218 /* FunctionExpression */:
          case 219 /* ArrowFunction */:
            return true;
          default:
            return false;
        }
      }
      function isFunctionLikeKind(kind) {
        switch (kind) {
          case 173 /* MethodSignature */:
          case 179 /* CallSignature */:
          case 323 /* JSDocSignature */:
          case 180 /* ConstructSignature */:
          case 181 /* IndexSignature */:
          case 184 /* FunctionType */:
          case 317 /* JSDocFunctionType */:
          case 185 /* ConstructorType */:
            return true;
          default:
            return isFunctionLikeDeclarationKind(kind);
        }
      }

    return !!node && isFunctionLikeKind(node.kind)
}

export function isPropertyName(node: ts.Node) {
    const kind = node.kind;
    return kind === 80 /* Identifier */ || kind === 81 /* PrivateIdentifier */ || kind === 11 /* StringLiteral */ || kind === 9 /* NumericLiteral */ || kind === 167 /* ComputedPropertyName */;
}

export function isBindingName(node: ts.Node) {
    const kind = node.kind;
    return kind === 80 /* Identifier */ || kind === 206 /* ObjectBindingPattern */ || kind === 207 /* ArrayBindingPattern */;
}

export function isClassElement(node: ts.Node) {
    const kind = node.kind;
    return kind === 176 /* Constructor */ || kind === 172 /* PropertyDeclaration */ || kind === 174 /* MethodDeclaration */ || kind === 177 /* GetAccessor */ || kind === 178 /* SetAccessor */ || kind === 181 /* IndexSignature */ || kind === 175 /* ClassStaticBlockDeclaration */ || kind === 240 /* SemicolonClassElement */;
}


function isLeftHandSideExpressionKind(kind: SyntaxKind) {
    switch (kind) {
        case 211 /* PropertyAccessExpression */:
        case 212 /* ElementAccessExpression */:
        case 214 /* NewExpression */:
        case 213 /* CallExpression */:
        case 284 /* JsxElement */:
        case 285 /* JsxSelfClosingElement */:
        case 288 /* JsxFragment */:
        case 215 /* TaggedTemplateExpression */:
        case 209 /* ArrayLiteralExpression */:
        case 217 /* ParenthesizedExpression */:
        case 210 /* ObjectLiteralExpression */:
        case 231 /* ClassExpression */:
        case 218 /* FunctionExpression */:
        case 80 /* Identifier */:
        case 81 /* PrivateIdentifier */:
        case 14 /* RegularExpressionLiteral */:
        case 9 /* NumericLiteral */:
        case 10 /* BigIntLiteral */:
        case 11 /* StringLiteral */:
        case 15 /* NoSubstitutionTemplateLiteral */:
        case 228 /* TemplateExpression */:
        case 97 /* FalseKeyword */:
        case 106 /* NullKeyword */:
        case 110 /* ThisKeyword */:
        case 112 /* TrueKeyword */:
        case 108 /* SuperKeyword */:
        case 235 /* NonNullExpression */:
        case 233 /* ExpressionWithTypeArguments */:
        case 236 /* MetaProperty */:
        case 102 /* ImportKeyword */:
        case 282 /* MissingDeclaration */:
        case SyntaxKind.UndefinedKeyword:
            return true;
        default:
            return false;
    }
}

function isUnaryExpressionKind(kind: SyntaxKind) {
    switch (kind) {
        case 224 /* PrefixUnaryExpression */:
        case 225 /* PostfixUnaryExpression */:
        case 220 /* DeleteExpression */:
        case 221 /* TypeOfExpression */:
        case 222 /* VoidExpression */:
        case 223 /* AwaitExpression */:
        case 216 /* TypeAssertionExpression */:
            return true;
        default:
            return isLeftHandSideExpressionKind(kind);
    }
}

function isExpressionKind(kind: SyntaxKind) {
    switch (kind) {
        case 227 /* ConditionalExpression */:
        case 229 /* YieldExpression */:
        case 219 /* ArrowFunction */:
        case 226 /* BinaryExpression */:
        case 230 /* SpreadElement */:
        case 234 /* AsExpression */:
        case 232 /* OmittedExpression */:
        case 355 /* CommaListExpression */:
        case 354 /* PartiallyEmittedExpression */:
        case 238 /* SatisfiesExpression */:
            return true;
        default:
            return isUnaryExpressionKind(kind);
    }
}

export function createProgram(rootNames: readonly string[], options: ts.CompilerOptions & { _emitBuffers?: boolean }, host?: ts.CompilerHost, oldProgram?: ts.Program, configFileParsingDiagnostics?: readonly ts.Diagnostic[]): ts.Program {
    const h = host ?? createCompilerHost(options)
    const hostHandle = (h as any)._handle
    if (!hostHandle) {
        throw new Error('Missing API handle')
    }

    const willEmitSourceMap = options.sourceMap && !options.inlineSourceMap
    const willEmitDeclarationSourceMap = options.declarationMap && !options.inlineSourceMap

    // tsc seems to wait to discover the extra types directory
    if (!options.types) {
        const types = findTypePackages(h.getCurrentDirectory(), h, false)
        options.types = types.map(t => t[0])
    }

    const opt = { 
        types: options.types, 
        lib: options.noLib ? [] : options.lib,
    }

    const printerOpt = { 
        emit_source_map: options.sourceMap,
        inline_source_map: options.inlineSourceMap,
        transform_to_cjs: options.module === ModuleKind.NodeNext,
    }

    const promise = new Promise<any>((resolve, reject) => {
        if (rootNames.length === 0) {
            resolve(api.createProgram(rootNames, hostHandle, () => {}, opt))
        } else {
            const handle = api.createProgram(rootNames, hostHandle, () => resolve(handle), opt)
        }
    })

    const programHandle = waitForPromise(promise)

    function getSourceFileFromHandle(handle: InternalApiSourceFileHandle) {
        if (!handle._sourceFile) {
            handle._sourceFile = new SourceFileInternal(handle, handle._source, api.getStartRef(handle), handle._fileName)
        }

        return handle._sourceFile
    }

    function getSourceFile(fileName: string) {
        if (h.fileHandles.has(fileName)) {
            return getSourceFileFromHandle(h.fileHandles.get(fileName))
        }

        const fileHandle = api.getSourceFileByPath(programHandle, fileName)
        if (!fileHandle) return

        return getSourceFileFromHandle(fileHandle) as any as ts.SourceFile
    }

    function getSourceFiles(): readonly ts.SourceFile[] {
        const handles = Array.from(h.fileHandles.values()) as InternalApiSourceFileHandle[]

        return handles.map((h) => getSourceFileFromHandle(h) as any as ts.SourceFile)
    }

    function getEmitData(d: Uint8Array | ArrayBuffer) {
        if (options._emitBuffers) {
            return d as string // XXX
        }
        return Buffer.from(d).toString('utf-8')
    }

    function emitSourceFile(sf: ts.SourceFile, writeFile?: ts.WriteFileCallback, cancellationToken?: ts.CancellationToken, declarationOnly?: boolean, emittedFiles: string[] = []) {
        const internal = getSourceFileInternal(sf)
        if (!internal) {
            throw new Error('Missing internal source file')
        }

        const fileName = internal.fileName
        const baseName = fileName.replace(/\.[jt]sx?|\.syn$/, '')

        if (!declarationOnly) {
            emittedFiles.push(baseName + '.js')
            if (willEmitSourceMap) {
                emittedFiles.push(baseName + '.js.map')
            }
        }

        if (options.declaration) {
            emittedFiles.push(baseName + '.d.ts')
            if (willEmitDeclarationSourceMap) {
                emittedFiles.push(baseName + '.d.ts.map')
            }
        }

        function emitFile(name: string, result: Uint8Array) {
            writeFile?.(name, getEmitData(result), false)
        }

        function emitFiles(extname: '.js' | '.d.ts', result: { contents: Uint8Array, mappings?: Uint8Array }) {
            if (extname === '.js' && willEmitSourceMap || extname === '.d.ts' && willEmitDeclarationSourceMap) {
                if (!result.mappings) {
                    throw new Error('Missing source mappings')
                }

                writeFile?.(baseName + `${extname}.map`, getEmitData(result.mappings), false)
            }

            writeFile?.(baseName + extname, getEmitData(result.contents), false)
        }
    
        if (!declarationOnly) {
            if (fileName.endsWith('.syn')) {
                const result = api.printSynFile(programHandle, internal.handle, printerOpt)
                emitFiles('.js', result)
            } else {
                const result: Promise<any> = api.printNodeAsync(internal.handle, internal.ref, undefined, undefined, printerOpt)
                const p = result.then(
                    r => emitFiles('.js', r),
                    err => console.log('emit failed', internal.fileName, err)
                )

                // XXX: needed for synthetic .d.ts files
                if (options.declaration) {
                    waitForPromise(p)
                } else {
                    waitForPromise(p)
                }
            }
        }

        if (options.declaration) {
            const decl = api.printDeclarationFile(programHandle, internal.handle, printerOpt)
            emitFiles('.d.ts', decl)
        }

        return emittedFiles
    }

    function emit(targetSourceFile?: ts.SourceFile, writeFile?: ts.WriteFileCallback, cancellationToken?: ts.CancellationToken, emitOnlyDtsFiles?: boolean, customTransformers?: ts.CustomTransformers): ts.EmitResult {
        if (targetSourceFile) {
            const emittedFiles = emitSourceFile(targetSourceFile, writeFile, cancellationToken, emitOnlyDtsFiles)

            return {
                emitSkipped: false,
                diagnostics: [],
                emittedFiles,
            }
        }

        const emittedFiles: string[] = []

        for (const f of getSourceFiles()) {
            if (!isSourceFileFromExternalLibrary(f)) {
                emitSourceFile(f, writeFile, cancellationToken, emitOnlyDtsFiles, emittedFiles)
            }
        }

        return {
            emitSkipped: false,
            diagnostics: [],
            emittedFiles,
        }
    }

    function isSourceFileFromExternalLibrary(sf: ts.SourceFile) {
        return sf.fileName.includes('node_modules') || path.basename(sf.fileName).startsWith('lib.') // XXX: TODO
    }

    let reifier: any
    function getReifier() {
        if (reifier) return reifier

        const types = reifiedTypes.createTypeNamespace()
        const reifierHandle = api.createReifier(programHandle, types)

        function __reify(fileName: string, nodeRef: number, typeParams = 0) {
            const sf = getSourceFile(fileName)
            if (!sf) {
                throw new Error(`missing sf: ${fileName}`)
            }
            const h = getSourceFileInternal(sf)
            if (!h) {
                throw new Error(`missing sf handle: ${fileName}`)
            }
            return api.getReifiedType(reifierHandle, h.handle, nodeRef, typeParams)
        }

        function __callTypeFunction(typeRef: number, args: number[]) {
            return api.callTypeFunction(reifierHandle, typeRef, args)
        }

        types.__callTypeFunction = __callTypeFunction

        return reifier = {
            types,
            __reify,
        }
    }

    function getTypeChecker() {
        function getSignatureFromDeclaration(declaration: ts.SignatureDeclaration): ts.Signature | undefined {
            return undefined
        }

        return {
            getSignatureFromDeclaration,
        }
    }

    function getSourceFileByPath(fileName: string) {
        return getSourceFile(fileName)
    }

    // FIXME: should be an array w/ separate API call to format
    function getSemanticDiagnostics(sourceFile?: ts.SourceFile, cancellationToken?: ts.CancellationToken) {
        if (!sourceFile) return '' // TODO

        const internal = getSourceFileInternal(sourceFile)
        if (!internal) return ''

        return api.getFormattedDiagnostics(programHandle, internal.handle)
    }

    return {
        _handle: programHandle,
        emit,
        getSourceFile,
        getSourceFiles,
        getTypeChecker,
        isSourceFileFromExternalLibrary,
        getCompilerOptions: () => options,
        getRootFileNames: () => rootNames,
        getSourceFileByPath,
        getReifier,
        getSemanticDiagnostics,
    } as any
}

export function findAncestor(node: ts.Node, test: (n: ts.Node) => boolean | undefined | 'quit'): ts.Node | undefined {
    let p = node
    while (p) {
        const t = test(p)
        if (t === 'quit') return
        if (t) return p
        p = p.parent
    }
}

function parseComments(text: string, pos: number) {
    // From `typescript`
    function isWhiteSpaceLike(ch) {
        return isWhiteSpaceSingleLine(ch) || isLineBreak(ch);
    }

    function isWhiteSpaceSingleLine(ch) {
        return ch === 32 /* space */ || ch === 9 /* tab */ || ch === 11 /* verticalTab */ || ch === 12 /* formFeed */ || ch === 160 /* nonBreakingSpace */ || ch === 133 /* nextLine */ || ch === 5760 /* ogham */ || ch >= 8192 /* enQuad */ && ch <= 8203 /* zeroWidthSpace */ || ch === 8239 /* narrowNoBreakSpace */ || ch === 8287 /* mathematicalSpace */ || ch === 12288 /* ideographicSpace */ || ch === 65279 /* byteOrderMark */;
    }

    function isLineBreak(ch) {
        return ch === 10 /* lineFeed */ || ch === 13 /* carriageReturn */ || ch === 8232 /* lineSeparator */ || ch === 8233 /* paragraphSeparator */;
    }

    const comments: ts.CommentRange[] = []
    while (pos < text.length) {
      const ch = text.charCodeAt(pos);
      switch (ch) {
        case 13 /* carriageReturn */:
          if (text.charCodeAt(pos + 1) === 10 /* lineFeed */) {
            pos++;
          }
        case 10 /* lineFeed */:
          pos++;
          continue;
        case 9 /* tab */:
        case 11 /* verticalTab */:
        case 12 /* formFeed */:
        case 32 /* space */:
          pos++;
          continue;
        case 47 /* slash */:
          if (text.charCodeAt(pos + 1) === 47 /* slash */) {
            const start = pos
            let hasTrailingNewLine = false
            pos += 2;
            while (pos < text.length) {
              if (isLineBreak(text.charCodeAt(pos))) {
                hasTrailingNewLine = true
                break;
              }
              pos++;
            }
            comments.push({
                pos: start,
                end: pos,
                kind: SyntaxKind.SingleLineCommentTrivia,
                hasTrailingNewLine,
            })
            continue;
          }
          if (text.charCodeAt(pos + 1) === 42 /* asterisk */) {
            const start = pos
            let hasTrailingNewLine = false
            pos += 2;
            while (pos < text.length) {
              if (text.charCodeAt(pos) === 42 /* asterisk */ && text.charCodeAt(pos + 1) === 47 /* slash */) {
                pos += 2;
                break;
              }
              pos++;
            }
            comments.push({
                pos: start,
                end: pos,
                kind: SyntaxKind.MultiLineCommentTrivia,
                hasTrailingNewLine,
            })
            continue;
          }
          break;
        default:
          if (ch > 127 /* maxAsciiCharacter */ && isWhiteSpaceLike(ch)) {
            pos++;
            continue;
          }
          break;
      }
      return comments.length > 0 ? comments : undefined
    }
}

export function getLeadingCommentRanges(text: string, pos: number): ts.CommentRange[] | undefined {
    return parseComments(text, pos)
}

function getSourceFile(node: ts.Node) {
    if (node.kind === SyntaxKind.SourceFile) {
        return node
    }
    return findAncestor(node, n => n.kind === SyntaxKind.SourceFile)
}

function getSourceFileInternal(node: ts.Node): SourceFileInternal | undefined {
    if (node instanceof SourceFileInternal) return node
    return (getSourceFile(node) as any)?._sourceFile
}

function getLineMap(sf: ts.SourceFile) {
    const internal = getSourceFileInternal(sf)
    if (!internal) {
        throw new Error('TODO')
    }

    const buf = api.getLineMap(internal.handle)
    if (!buf) {
        throw new Error('TODO: missing line map')
    }

    return new Uint32Array(buf)
}

// Includes options not in the public API
type PrinterOptions = ts.PrinterOptions & {
    emitSourceMap?: boolean
    sourceMapRootDir?: string
    inlineSourceMap?: boolean
    handlers?: ts.PrintHandlers
    _stripTypes?: boolean
}

function printNodeInternalBuffer(n: ts.Node, options?: PrinterOptions): Buffer {
    if (n instanceof SourceFileInternal) {
        const sf = n as SourceFileInternal
        const serializer = createNodeSerializer(sf.ref + 1, sf.source.byteLength, options?.skip_types)
        const r = serializer.serializeNode(sf.node)
        const result = api.printNode(sf.handle, r, serializer.buffer, serializer.heap, options)
    
        return Buffer.from(result.contents)
    }

    const sf = getSourceFileInternal(getOriginalNode(n))
    if (!sf) {
        const serializer = createNodeSerializer(0, 0, options?.skip_types)
        const r = serializer.serializeNode(n)

        const result = api.printSyntheticNode(r, serializer.buffer, serializer.heap, options)
    
        return Buffer.from(result.contents)
    }

    const serializer = createNodeSerializer(sf.ref + 1, sf.source.byteLength, options?.skip_types)
    const r = serializer.serializeNode(n)

    const result = api.printNode(sf.handle, r, serializer.buffer, serializer.heap, options)

    return Buffer.from(result.contents)
}

function printNodeInternal(n: ts.Node, options?: PrinterOptions): string {
    return printNodeInternalBuffer(n, options).toString('utf-8')
}

export function createPrinter(printerOptions?: PrinterOptions, handlers?: ts.PrintHandlers): ts.Printer {
    function printNode(hint: ts.EmitHint, node: ts.Node, sourceFile: ts.SourceFile): string {
        return printNodeInternal(node, {
            skip_types: sourceFile.fileName?.endsWith('.js') ?? false,
            emit_source_map: printerOptions?.emitSourceMap,
            inline_source_map: printerOptions?.inlineSourceMap,
        })
    }

    function writeFile(file: ts.SourceFile, writer: any, sourceMapGenerator: ReturnType<typeof createSourceMapGenerator>) {
        const skipTypes = printerOptions?._stripTypes ?? !file.fileName.match(/\.d(?:\..+)?\.ts$/)
        const internalFile = getSourceFileInternal(file)

        const printerOpt = {
            skip_types: skipTypes,
            emit_source_map: printerOptions?.emitSourceMap,
            inline_source_map: printerOptions?.inlineSourceMap,
            file_name: sourceMapGenerator?.file ?? file.fileName,
        }

        if (!internalFile) {
            const serializer = createNodeSerializer(0, 0, skipTypes)
            const r = serializer.serializeNode(file)
            const result = api.printSyntheticNode(r, serializer.buffer, serializer.heap, printerOpt)

            writer.getText = () => Buffer.from(result.contents).toString('utf-8')
            writer._result = result

            if (sourceMapGenerator && !printerOptions?.inlineSourceMap) {
                if (!result.mappings) {
                    throw new Error(`Missing source map: ${file.fileName}`)
                }
                sourceMapGenerator.toJSON = () => {
                    const r = JSON.parse(Buffer.from(result.mappings).toString('utf-8'))
                    r.sourceRoot = sourceMapGenerator.sourceRoot
                    r.file = sourceMapGenerator.file
                    r.sources[0] = file.fileName
                    return r
                }
            }

            return
        }

        const sf = internalFile
        const serializer = createNodeSerializer(sf.ref + 1, sf.source.byteLength, skipTypes)
        const r = serializer.serializeNode(sf.node)
        const result = api.printNode(sf.handle, r, serializer.buffer, serializer.heap, printerOpt)

        writer.getText = () => Buffer.from(result.contents).toString('utf-8')
        writer._result = result

        if (sourceMapGenerator && !printerOptions?.inlineSourceMap) {
            if (!result.mappings) {
                throw new Error(`Missing source map: ${file.fileName}`)
            }
            sourceMapGenerator.toJSON = () => {
                const r = JSON.parse(Buffer.from(result.mappings).toString('utf-8'))
                r.sourceRoot = sourceMapGenerator.sourceRoot
                r.file = sourceMapGenerator.file
                r.sources[0] = file.fileName
                return r
            }
        }
    }

    return { printNode, writeFile } as any
}

export const isSyn = true
export const nullTransformationContext = { factory }

export function createTextWriter() {
    return {}
}

export function createSourceMapGenerator(host: any /**SourceMapHost*/, file: string, sourceRoot: string | undefined, sourcesDirectoryPath: string, opt: any) {
    return { file, sourceRoot, sourcesDirectoryPath, toJSON: () => {} }
}

export function _printSourceFile(sf: ts.SourceFile, printerOptions?: PrinterOptions) {
    const writer = createTextWriter()
    const printer = createPrinter(printerOptions)
    printer.writeFile(sf, writer)

    return writer.getText()
}

export enum JsxEmit {
    None = 0,
    Preserve = 1,
    React = 2,
    ReactNative = 3,
    ReactJSX = 4,
    ReactJSXDev = 5,
}

export function parseConfigFileTextToJson(fileName: string, text: string): { config?: any; error?: ts.Diagnostic } {
    try {
        return { config: api.parseJson5(text) }
    } catch (e) {
        return { error: createErrorDiagnostic((e as any).message, fileName) }
    }
}

enum DiagnosticCategory {
    Warning = 0,
    Error = 1,
    Suggestion = 2,
    Message = 3,
}

function createErrorDiagnostic(msg: string, source?: string): ts.Diagnostic {
    return {
        file: undefined,
        code: 0, // TODO
        length: undefined,
        start: 0, // TODO
        source: source,
        category: DiagnosticCategory.Error,
        messageText: msg,
    }
}

function findTypePackages(workingDir: string, host: ts.ParseConfigHost, recursive = false) {
    function tryReadTypesDir(dir: string) {
        const result: [name: string, file: string][] = []
        const p = path.resolve(dir, 'node_modules', '@types')
        const exists = host.directoryExists?.(p) ?? true
        if (!exists) {
            return result
        }

        const dirs = host.getDirectories?.(p) ?? []
        for (const d of dirs) {
            result.push([path.basename(d), getRootTypesFile(d, host)])
        }

        return result
    }

    const result: [name: string, file: string][] = []
    let currentDir = workingDir
    do {
        result.push(...tryReadTypesDir(currentDir))
        if (path.dirname(currentDir) === currentDir) break
        currentDir = path.dirname(currentDir)
    } while (recursive)

    return result
}

export enum ModuleResolutionKind {
    Classic = 1,
    Node10 = 2,
    Node16 = 3,
    NodeNext = 99,
    Bundler = 100,
}

export function parseJsonConfigFileContent(json: any, host: ts.ParseConfigHost, basePath: string, existingOptions?: ts.CompilerOptions): ts.ParsedCommandLine {
    if (typeof json !== 'object' || json === null) {
        return {
            fileNames: [],
            options: {},
            errors: [createErrorDiagnostic('Not an object')],
        }
    }

    if (basePath.endsWith('.json')) {
        basePath = path.dirname(basePath)
    }

    const errors: ts.Diagnostic[] = []
    const options: ts.CompilerOptions = {}
    const fileNames: string[] = []

    const compilerOptions = json.compilerOptions ?? {}

    if (compilerOptions.outDir) {
        options.outDir = path.resolve(basePath, compilerOptions.outDir)
    }

    if (compilerOptions.rootDir) {
        options.rootDir = path.resolve(basePath, compilerOptions.rootDir)
    }

    if (compilerOptions.paths) {
        const paths = options.paths = {}
        for (const [k, v] of Object.entries(compilerOptions.paths)) {
            paths[k] = v.map(x => path.resolve(basePath, x))
        }
    }

    options.declaration = compilerOptions.declaration
    options.sourceMap = compilerOptions.sourceMap
    options.declarationMap = compilerOptions.declarationMap
    options.allowArbitraryExtensions = compilerOptions.allowArbitraryExtensions

    if (typeof compilerOptions.target === 'string') {
        switch (compilerOptions.target.toLowerCase()) {
            case 'esnext':
                options.target = ScriptTarget.ESNext
                break

            default:
                options.target = ScriptTarget[compilerOptions.target.toUpperCase() as keyof typeof ScriptTarget]
                if (options.target === undefined) {
                    throw new Error(`Invalid target: ${compilerOptions.target}`)
                }
                break
        }
    }

    if (typeof compilerOptions.module === 'string') {
        switch (compilerOptions.module.toLowerCase()) {
            case 'esnext':
                options.module = ModuleKind.ESNext
                break
            case 'node16':
                options.module = ModuleKind.Node16
                break
            case 'nodenext':
                options.module = ModuleKind.NodeNext
                break
            case 'commonjs':
                options.module = ModuleKind.CommonJS
                break

            default:
                options.module = ModuleKind[compilerOptions.module.toUpperCase() as keyof typeof ModuleKind]
                if (options.module === undefined) {
                    throw new Error(`Invalid module mode: ${compilerOptions.module}`)
                }
                break
        }
    }

    if (typeof compilerOptions.moduleResolution === 'string') {
        switch (compilerOptions.moduleResolution.toLowerCase()) {
            case 'classic':
                options.moduleResolution = ModuleResolutionKind.Classic
                break
            case 'bundler':
                options.moduleResolution = ModuleResolutionKind.Bundler
                break
            case 'nodenext':
                options.moduleResolution = ModuleResolutionKind.NodeNext
                break

            default:
                options.moduleResolution = ModuleResolutionKind[compilerOptions.moduleResolution.toUpperCase() as keyof typeof ModuleResolutionKind]
                if (options.moduleResolution === undefined) {
                    throw new Error(`Invalid module resolutin mode: ${compilerOptions.moduleResolution}`)
                }
                break
        }
    }

    if (!compilerOptions.noLib) {
        options.lib = compilerOptions.lib
        if (!options.lib) {
            switch (options.target ?? ScriptTarget.ES2022) {
                case ScriptTarget.ES2015:
                    options.lib = ['es2015']
                    break
                case ScriptTarget.ES2016:
                    options.lib = ['es2016']
                    break
                case ScriptTarget.ES2017:
                    options.lib = ['es2017']
                    break
                case ScriptTarget.ES2018:
                    options.lib = ['es2018']
                    break
                case ScriptTarget.ES2019:
                    options.lib = ['es2019']
                    break
                case ScriptTarget.ES2020:
                    options.lib = ['es2020']
                    break
                case ScriptTarget.ES2021:
                    options.lib = ['es2021']
                    break
                case ScriptTarget.ES2022:
                    options.lib = ['es2022']
                    break         
                case ScriptTarget.ES2023:
                    options.lib = ['es2023']
                    break
                case ScriptTarget.Latest:
                    options.lib = ['esnext']
                    break

                default:
                    throw new Error(`TODO: ${ScriptTarget[options.target!]}`)
            }
        }
    }

    const defaultFileExtensions = ['.ts', '.syn']

    function includeFileWorker(f: string) {
        if (f === '**') {
            const files = host.readDirectory(basePath, defaultFileExtensions, json.exclude, [], undefined)
            fileNames.push(...files)
        } else if (f === '*') {
            const files = host.readDirectory(basePath, defaultFileExtensions, json.exclude, [], 1)
            fileNames.push(...files)
            return
        } else if (f === '*.ts') {
            const files = host.readDirectory(basePath, ['.ts'], json.exclude, [], 1)
            fileNames.push(...files)
            return 
        }

        const resolved = path.resolve(basePath, f)
        if (path.extname(resolved) !== '') {
            fileNames.push(resolved)
            return
        }

        // XXX: just try to read as dir...
        try {
            const files = host.readDirectory(resolved, defaultFileExtensions, json.exclude, [], undefined)
            fileNames.push(...files)
        } catch (e) {
            if ((e as any).code === 'ENOENT') {
                return
            }
            if ((e as any).code !== 'ENOTDIR') {
                throw e
            }
            // FIXME: it could be something more esoteric (like a device)
            fileNames.push(resolved)
        }
    }

    const include = json.include ?? ['**']
    // FIXME: validate that this is an array
    for (const f of include as string[]) {
        includeFileWorker(f)
    }

    return {
        fileNames,
        options,
        errors,
        raw: json,
    }
}

export function createGetCanonicalFileName(useCaseSensitiveFileNames?: boolean) {
    return (f: string) => f // TODO
}

function createSystem(): ts.System {
    return {
        newLine: '\n',
        args: [],
        useCaseSensitiveFileNames: false, // TODO
        readFile(path: string, encoding?: string): string | undefined {
            try {
                return fs.readFileSync(path, encoding === 'bytes' ? undefined : (encoding as BufferEncoding) ?? 'utf-8')
            } catch (e) {
                if ((e as any).code !== 'ENOENT') {
                    throw e
                }
            }
        },
        writeFile(path: string, data: string, writeByteOrderMark?: boolean): void {
            fs.writeFileSync(path, data)
        },
        resolvePath(p: string): string {
            return path.resolve(p)
        },
        fileExists(path: string): boolean {
            return fs.existsSync(path)
        },
        directoryExists(path: string): boolean {
            return fs.existsSync(path)
        },
        createDirectory(path: string): void {
            fs.mkdirSync(path, { recursive: true })
        },
        getExecutingFilePath(): string {
            return path.resolve(path.dirname(import.meta.filename || __filename), 'lib', 'tsc.js')
        },
        getCurrentDirectory(): string {
            return process.cwd()
        },
        getDirectories(dir: string): string[] {
            const files = fs.readdirSync(dir, { withFileTypes: true })
            return files.filter(f => f.isDirectory()).map(f => path.resolve(dir, f.name))
        },
        readDirectory(dir: string, extensions?: readonly string[], exclude?: readonly string[], include?: readonly string[], depth?: number): string[] {
            // TODO: incomplete

            const result: string[] = []
            function doRead(dir: string, depthThreshold?: number) {
                if ((depthThreshold !== undefined && depthThreshold === 0)) return

                const files = fs.readdirSync(dir, { withFileTypes: true })
                for (const f of files) {
                    if (f.isDirectory()) {
                        if (f.name.startsWith('.') || f.name === 'node_modules') continue
                        doRead(path.resolve(dir, f.name), depthThreshold !== undefined ? depthThreshold - 1 : undefined)
                    } else if (!extensions ? true : extensions.includes(path.extname(f.name))) {
                        result.push(path.resolve(dir, f.name))
                    }
                }
            }

            
            doRead(dir, depth)

            return result
        },
        exit(exitCode) {
            process.exit(exitCode)
        },
        write(s) {
            process.stdout.write(s)
        },
    }
}

export const sys = createSystem()
