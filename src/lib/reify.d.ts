/// <reference no-default-lib="true"/>

declare namespace Type {
    class Array<T extends Type = Type> extends globalThis.Array<T> {
        readonly type: T
        readonly maxLength: number
        readonly readonly?: boolean
    }

    interface TupleElement {
        readonly name?: string
        readonly type: Type
        readonly rest?: boolean
        readonly optional?: boolean
    }

    // TODO: this can be narrowed e.g. Type.Tuple & [reify 1, reify 2]
    // the finite case would need to re-use whatever the checker is doing for `[1,2,3] as const`
    class Tuple extends globalThis.Array<Type> {
        readonly maxLength: number
        readonly elements: TupleElement[]
        readonly readonly?: boolean
    }

    interface ObjectProperty<T extends Type = Type> {
        readonly type: T
        readonly docs?: string
        readonly readonly?: boolean
        readonly optional?: boolean
        readonly source?: Type.Object // only present if this property was inherited
    }

    interface ObjectSymbolProperty extends ObjectProperty {
        readonly name: symbol
    }

    interface ObjectIndexSignature {
        readonly name: string
        readonly index: Type
        readonly element: Type
        readonly readonly?: boolean
        readonly optional?: boolean
        readonly docs?: string
        readonly source?: Type.Object
    }

    interface ObjectCallSignature {
        readonly type: Type.Function | Type.TypeFunction
        readonly newable?: boolean
        readonly docs?: string
        readonly source?: Type.Object
    }

    type ObjectSignature = ObjectIndexSignature | ObjectCallSignature

    class Object {
        readonly [name: string]: Type
    }

    // Only relevant when a type has no object representation 
    //  -> Type.getBase(reify string & {}) === reify string
    function getBase(t: Type.Object): Type | undefined
    function getProperties<T extends Type.Object>(t: T): Record<keyof T, ObjectProperty>
    function getSymbolProperties(t: Type.Object): ObjectSymbolProperty[] | undefined
    function getCallSignatures(t: Type.Object): ObjectCallSignature[] | undefined
    function getIndexSignatures(t: Type.Object): ObjectIndexSignature[] | undefined


    function findClass(t: Type.Object): { name: string, type: Type, value?: any } | undefined

    class Function {
        readonly params: Tuple & { elements: (TupleElement & { name: string })[] }
        readonly returns?: Type // only present for non-void return types
        readonly newable?: boolean
        readonly this?: Type
        // if true, `returns` contains the awaited type
        // reify (() => Promise<number>)
        //  -> { params: [], returns: reify number, async: true }
        readonly async?: boolean
    }

    // Unions are readonly but we do not want to annotate it as such
    interface Union<T extends Type = Type> extends Set<T>, Omit<T[], 'keys' | 'entries' | 'forEach'> {}
    class Union<T extends Type = Type> extends Set<T> {}

    // `foo${string}bar` 
    //  -> { strings: ['foo', 'bar'], types: [Type.string] }
    class Template {
        readonly strings: string[]
        readonly types: Type[]
    }

    // "bare" parameterized types become type functions.
    //
    // ```syn
    // const f = reify <T>(x: T) => T
    // const f2 = f(reify number)
    // const f3 = reify (x: number) => number
    // f2 == f3 // true
    // ```
    interface TypeFunction<T extends Type[] = Type[], U extends Type = Type> {
        (...args: T): U
    }

    class TypeFunction {
        getParameters(): { name: string; extends?: Type; defaultValue?: Type }[]
    }

    // so you can do `if (reify string === Type.string) ...`
    const string: unique symbol
    const number: unique symbol
    const boolean: unique symbol
    const bigint: unique symbol
    const symbol: unique symbol
    const object: unique symbol
    const any: unique symbol
    const never: unique symbol
    const unknown: unique symbol

    type Intrinsic = 
        | typeof string
        | typeof number
        | typeof boolean
        | typeof bigint
        | typeof symbol
        | typeof object
        | typeof any
        | typeof never
        | typeof unknown

    function isArrayType(t: Type): t is Array
    function isTuple(t: Type): t is Tuple
    function isObject(t: Type): t is Object
    function isFunction(t: Type): t is Function
    function isUnion(t: Type): t is Union
    function isTemplate(t: Type): t is Template
    function isIntrinsic(t: Type): t is Intrinsic
    function isLiteral(t: Type): t is Literal
    function isTypeFunction(t: Type): t is TypeFunction

    type Literal = 
        | undefined
        | null
        | boolean
        | string
        | number
        | bigint

    // the checker uses this to narrow a type
    interface Kinds {
        array: Array
        tuple: Tuple
        object: Object
        function: Function
        union: Union
        template: Template
        intrinsic: Intrinsic
        literal: Literal
        typeFunction: TypeFunction
        // enum ?
    }

    function kind(t: Type): keyof Kinds
}

type Type =
    | Type.Array
    | Type.Tuple
    | Type.Object
    | Type.Function
    | Type.Union
    | Type.Template
    | Type.Intrinsic
    | Type.Literal
    | Type.TypeFunction

