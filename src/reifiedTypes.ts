import * as vm from 'node:vm'

const any = Symbol.for('type.any')
const never = Symbol.for('type.never')
const unknown = Symbol.for('type.unknown')
const Void = Symbol.for('type.void')

const number = Symbol.for('type.number')
const string = Symbol.for('type.string')
const object = Symbol.for('type.object')
const boolean = Symbol.for('type.boolean')
const symbol = Symbol.for('type.symbol')

type _type = any // placeholder

class Template {
    strings!: string[]
    types!: _type[]

    private _regexp: RegExp | undefined

    get regexp() {
        if (this._regexp) return this._regexp

        function escapeRegExp(pattern: string) {
            return pattern.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")
        }

        function templateToPattern(t: Template, useGroups: boolean) {
            const parts: string[] = []
            for (let i = 0; i < t.types.length; i++) {
                parts.push(escapeRegExp(t.strings[i]))
                parts.push(`(${useGroups ? '' : '?:'}${typeToPattern(t.types[i])})`)
            }

            const tail = t.strings[t.strings.length - 1]
            if (tail) {
                parts.push(tail)
            }

            return parts.join('')
        }

        function typeToPattern(t: _type): string {
            switch (t) {
                case boolean:
                    return 'true|false'
                case string:
                    return '.*'
                case number:
                    // does not handle all possible cases
                    return '(?:-?\\d+\\.\\d+)|(?:-?\\d+)'
            }

            switch (tag(t)) {
                case 'literal':
                    if (t === null || t === undefined) {
                        return String(t)
                    }
                    return escapeRegExp(String(t))
                case 'union':
                    return [...t].map(x => typeToPattern(x)).join('|')
                case 'template':
                    return templateToPattern(t as Template, false)
            }

            throw new Error(`unexpected type: ${t}`)
        }

        return this._regexp = new RegExp(templateToPattern(this, true))
    }
}

// TODO: all mutable methods on Set and Array should throw
class Union2 extends Set {
    constructor(private readonly elements: any[]) {
        super(elements)

        const proto = Object.getPrototypeOf(this)
        Object.setPrototypeOf(this, new Proxy(proto, {
            get: (t, p, r) => {
                if (p === originalProtoSym) {
                    return proto
                }
                if (p !== Symbol.iterator && (Reflect.has(t, p) || !r)) {
                    return Reflect.get(t, p, r)
                }

                const z = Reflect.get(elements, p, elements)
                if (typeof z === 'function') {
                    return z.bind(elements) // a tiny bit fragile, a slightly cleaner way is to merge proto methods onto `Union` that forward using `elements`
                }

                return z
            },
        }))
    }

    private toString() {
        const id = (this as any)[typeIdSym]
        if (id === undefined) {
            return super.toString()
        }
        return `[type Union ${id}]`
    }

    public includes(val: any) {
        return this.has(val)
    }

    static [Symbol.hasInstance](instance: any) {
        if (typeof instance !== 'object' || instance === null) {
            return false 
        }

        const original = Object.getPrototypeOf(instance)?.[originalProtoSym]

        return this.prototype === original || super[Symbol.hasInstance](instance)
    }
}

function maybeParseTypeId(prop: string | symbol) {
    if (typeof prop !== 'string') return

    const m = prop.match(/^\[type Union (\d+)\]$/)
    
    return m?.[1]
}

function defineNonEnumerable(o: any, p: PropertyKey, value: any) {
    return Object.defineProperty(o, p, { configurable: true, writable: true, enumerable: false, value })
}

export function createTypeNamespace() {
    class Union extends Set {}
    class ObjectType {}
    class ArrayType {}
    class FunctionType {}
    class Tuple {
        elements = [] as any[]
        add(t: any) {
            this.elements.push({ type: t })
        }
    }

    const cache = new Map<number, any>()
    function __hasCachedType(ty: number) {
        return cache.has(ty)
    }

    function __getCachedType(ty: number) {
        return cache.get(ty)
    }

    function __setCachedType(ty: number, v: any) {
        if (typeof v === 'function' || (typeof v === 'object' && v !== null)) {
            defineNonEnumerable(v, typeIdSym, ty)
        }
        cache.set(ty, v)
    }

    // helpers

    function kind(t: any) {
        if (typeof t !== 'object' || t === null) {
            if (typeof t === 'symbol') {
                switch (t) {
                    case any:
                    case never:
                    case unknown:
                    case Void:
                    case number:
                    case string:
                    case object:
                    case boolean:
                    case symbol:
                        return 'intrinsic'
                }
            }
            return 'literal'
        }

        if (t instanceof ObjectType) return 'object'
        if (t instanceof Union) return 'union'
        if (t instanceof ArrayType) return 'array'
        if (t instanceof Tuple) return 'tuple'

        return 'object'
    }

    function findDiscriminant(t: Union): PropertyKey | undefined {
        let min = -1
        const keys: PropertyKey[][] = []
        for (const u of t) {
            if (kind(u) !== 'object') return

            const s = Object.keys(u)
            keys.push(s)

            if (min === -1 || s.length < keys[min].length) {
                min = keys.length-1
            }
        }

        outer: for (const k of keys[min]) {
            const types = new Array(t.size)
            for (const u of t) {
                const t2 = u[k]

                // FIXME: this should check for overlapping types, not equivalence
                if (types.includes(t2)) continue outer
                types.push(t2)
            }

            return k
        }
    }

    // for the reifier
    function __Object() {
        return new ObjectType()
    }

    function __Tuple() {
        return new Tuple()
    }

    function __Union() {
        return new Union()
    }

    function __ArrayType() {
        return new ArrayType()
    }

    function typeToTypeId(t: _type): number {
        switch (kind(t)) {
            case 'array':
            case 'object':
            case 'tuple':
            case 'union': {
                const id = (t as any)[typeIdSym]
                if (id === undefined) {
                    throw new Error(`todo: synthetic types`)
                }
                return id
            }
        }
        throw new Error(`todo: ${kind(t)}`)
    }

    function __TypeFunction(target: number, arity: number = 0) {
        const f = (...args: any[]) => {
            if (args.length < arity) {
                throw new Error(`too few arguments: ${args.length} < ${arity}`)
            }
            const mapped = args.map(typeToTypeId)
            return (globalThis as any).type.__callTypeFunction(target, mapped)
        }

        return f
    }

    return {
        kind,
        findDiscriminant,

        any,
        never,
        unknown,
        Void,
        number,
        string,
        object,
        boolean,
        symbol,

        ArrayType,
        Union,
        Tuple,
        Object: ObjectType,

        __ArrayType,
        __Object,
        __Tuple,
        __Union,
        __TypeFunction,

        __getCachedType,
        __hasCachedType,
        __setCachedType,
    }
}

const typeIdSym = Symbol.for('_type.id')
const originalProtoSym = Symbol.for('_type.proto')

const arrayPrototypeKeys = new Set([
    ...Object.getOwnPropertyNames(Array.prototype),
    ...Object.getOwnPropertySymbols(Array.prototype),
])

interface TupleElementDescriptor {
    name?: string
    type: _type
    optional?: boolean
    rest?: boolean 
}

const tupleElementDescriptors = Symbol.for('_type.tuple.elements')
function getElementDescriptors(o: any) {
    if (typeof o !== 'object' || o === null) return

    return (o as _Tuple)[tupleElementDescriptors]
}

function getTypeFromElementDescriptors(index: number, descriptors: TupleElementDescriptor[]) {
    for (let i = 0; i < descriptors.length; i++) {
        const desc = descriptors[i]
        if (i === index) return desc.type

        // fields after a rest element are ignored during dynamic indexing
        if (desc.rest) return desc.type
    }

    return undefined
}

function getUnionTypeFromElementDescriptors(descriptors: TupleElementDescriptor[]) {
    if (descriptors.length === 0) return never

    const types = descriptors.map(x => x.type)

    return new Union2(types)
}

function proxyTuplePrototype(proto: any) {
    return new Proxy(proto, {
        get: (t, p, r) => {
            if (p === originalProtoSym) {
                return proto
            }
            if (p === number) {
                return getUnionTypeFromElementDescriptors(getElementDescriptors(r) ?? [])
            }
            if (typeof p === 'symbol' || arrayPrototypeKeys.has(p)) {
                return Reflect.get(t, p, r)
            }

            const index = Number(p)
            if (Number.isNaN(index) || !Number.isInteger(index) || index < 0) {
                return Reflect.get(t, p, r)
            }

            const descriptors = getElementDescriptors(r)
            if (!descriptors) {
                return Reflect.get(t, p, r)
            }
            
            return getTypeFromElementDescriptors(index, descriptors)
        },
    })
}

class _Tuple extends Array {
    public readonly maxLength: number
    private readonly [tupleElementDescriptors]!: TupleElementDescriptor[]

    constructor(descriptors: TupleElementDescriptor[] = []) {
        const restElement = descriptors.findIndex(d => d.rest)
        const optionalElement = descriptors.findIndex(d => d.optional)
        const length = restElement === -1
            ? optionalElement === -1 ? descriptors.length : optionalElement
            : optionalElement === -1 ? restElement : Math.min(optionalElement, restElement)

        super(length)

        if (restElement !== -1) {
            this.maxLength = Infinity
        } else if (optionalElement !== -1) {
            this.maxLength = optionalElement
        } else {
            this.maxLength = length
        }

        defineNonEnumerable(this, tupleElementDescriptors, descriptors)

        // implies rest or optional elements
        if (this.length !== this.maxLength) {
            const proto = Object.getPrototypeOf(this)
            Object.setPrototypeOf(this, proxyTuplePrototype(proto))
        }
    }

    // This is needed if we proxy our prototype
    static [Symbol.hasInstance](instance: any) {
        if (typeof instance !== 'object' || instance === null) {
            return false 
        }

        const original = Object.getPrototypeOf(instance)?.[originalProtoSym]

        return this.prototype === original || super[Symbol.hasInstance](instance)
    }
}

const objectPrototypeKeys = new Set([
    ...Object.getOwnPropertyNames(Object.prototype),
    ...Object.getOwnPropertySymbols(Object.prototype),
])

function tag(t: _type) {
    if (typeof t !== 'object' || t === null) {
        if (typeof t === 'symbol') {
            switch (t) {
                case any:
                case never:
                case unknown:
                case Void:
                case number:
                case string:
                case object:
                case boolean:
                case symbol:
                    return 'intrinsic'
            }
        }
        return 'literal'
    }

    if (t instanceof _Object) return 'object'
    if (t instanceof Set) return 'union'
    //if (t instanceof ArrayType) return 'array'
    if (t instanceof _Tuple) return 'tuple'
    if (t instanceof Template) return 'template'

    return 'object'
}

function getTypeFromIndexSignatures(index: string | symbol, signatures: IndexSignature[]) {
    if (signatures.length === 0) {
        return undefined
    }

    if (typeof index === 'symbol') {
        const s = signatures.find(x => x.key === symbol)

        return s?.type
    }

    const rem = signatures.filter(x => x.type !== symbol)

    for (const s of rem) {
        if (s.key === string) return s.type

        if (tag(s.key) === 'template') {
            const r = (s.key as Template).regexp
            if (r.test(index)) return s.type
        }
    }

    if (Number.isNaN(Number(index))) return undefined
    
    return rem.find(x => x.type === number)?.type
}

function proxyObjectTypePrototype(proto: any) {
    return new Proxy(proto, {
        get: (t, p, r) => {
            if (p === originalProtoSym) {
                return proto
            }

            const typeId = maybeParseTypeId(p)
            if (typeId !== undefined) {
                const ty = (globalThis as any).type.__getCachedType(typeId)
                if (ty instanceof Union2) {
                    const descriptor = getObjectTypeDescriptor(r)
                    if (!descriptor) return unknown

                    return new Union2((ty as any).map((x: any) => {
                        if (typeof x === 'string' && x !== p) {
                            return r[x]
                        }

                        return getTypeFromIndexSignatures(x, descriptor.indexSignatures)
                    }))
                }
                return unknown
            }

            if (objectPrototypeKeys.has(p) || Reflect.has(t, p)) {
                return Reflect.get(t, p, r)
            }

            const descriptor = getObjectTypeDescriptor(r)
            if (!descriptor) {
                return Reflect.get(t, p, r)
            }
            
            return getTypeFromIndexSignatures(p, descriptor.indexSignatures)
        },
    })
}

interface ObjectTypeProperty {
    readonly name: PropertyKey
    readonly type: any
    readonly readonly?: boolean
    readonly optional?: boolean
}

interface IndexSignature {
    readonly name: string
    readonly key: any
    readonly type: any
    readonly readonly?: boolean
    readonly optional?: boolean
}

interface ObjectTypeDescriptor {
    readonly properties: ObjectTypeProperty[]
    readonly indexSignatures: IndexSignature[]
    readonly callSignatures: any[] // (Function | TypeFunction)[]
}

const objectTypeDescriptor = Symbol.for('_type.object.typeDescriptor')
function getObjectTypeDescriptor(o: any) {
    if (typeof o !== 'object' || o === null) return

    return (o as _Object)[objectTypeDescriptor]
}

class _Object extends Object {
    private readonly [objectTypeDescriptor]!: ObjectTypeDescriptor

    constructor(descriptor: ObjectTypeDescriptor) {
        super()

        defineNonEnumerable(this, objectTypeDescriptor, descriptor)

        if (descriptor.indexSignatures.length) {
            const proto = Object.getPrototypeOf(this)
            Object.setPrototypeOf(this, proxyObjectTypePrototype(proto))
        }
    }

    static [Symbol.hasInstance](instance: any) {
        if (typeof instance !== 'object' || instance === null) {
            return false 
        }

        const original = Object.getPrototypeOf(instance)?.[originalProtoSym]

        return this.prototype === original || super[Symbol.hasInstance](instance)
    }
}

export function runSynModule(text: string, fileName: string, reifier: { types: any, __reify: any }) {
    ;(globalThis as any).type = reifier.types
    const ctx = vm.createContext({
        __filename: fileName,
        type: reifier.types,
        __reify: reifier.__reify,
        console: console,
        performance,
        Buffer,
        JSON,
    }, {
        origin: fileName,
    })
    return vm.runInContext('"use strict";\n' + text, ctx)
}