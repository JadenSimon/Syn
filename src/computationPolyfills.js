const bind = Function.prototype.bind
Function.prototype.bind = function(...args) {
    const f = bind.apply(this, args)
    if (!this[Symbol.for('toComputation')]) {
        return f        
    }
    return Object.assign(f, {
        [Symbol.for('toComputation')]: () => ['$0.bind(...$1)', [this, args]]
    })
}

Map.prototype[Symbol.for('toComputation')] = function() {
    const entries = [...this.entries()]
    return ['new Map(...$0)', [entries]]
}

Set.prototype[Symbol.for('toComputation')] = function() {
    const values = [...this.values()]
    return ['new Set(...$0)', [values]]
}

RegExp.prototype[Symbol.for('toComputation')] = function() {
    return ['new RegExp($0, $1)', [this.source, this.flags]]
}

// weak map/set are considered "emptied" as computations

WeakMap.prototype[Symbol.for('toComputation')] = function() {
    return ['new WeakMap()', []]
} 

WeakSet.prototype[Symbol.for('toComputation')] = function() {
    return ['new WeakSet()', []]
}

// weak refs are sort of "dead" after the underlying is collected, so, we will reject this
WeakRef.prototype[Symbol.for('toComputation')] = function() {
    throw new TypeError('WeakRef cannot become a computation')
}

Date.prototype[Symbol.for('toComputation')] = function() {
    return ['new Date($0)', [this.toISOString()]]
}

Symbol.prototype[Symbol.for('toComputation')] = function() {
    const key = Symbol.keyFor(this)
    if (!key) throw new TypeError(`Cannot use "${this}" as a computation because it is not in the global registry.`)
    return ['Symbol.for($0)', [key]]
}

Object.prototype[Symbol.for('toComputation')] = function() {
    let proto = Object.getPrototypeOf(this)
    const descriptors = Object.fromEntries(
        Object.entries(Object.getOwnPropertyDescriptors(this)).filter(([k]) => {
            if (typeof k === 'symbol') {
                if (!Symbol.keyFor(k)) return false // silently skip over
            }
            return true
        })
    )
    if (proto) {
        // we cannot directly instrument prototype, so we have to indirectly use the original ctor
        const ctor = this.constructor
        if (proto !== ctor) {
            throw new TypeError('Cannot coerce mismatched object prototypes')
        }
        proto = { [Symbol.for('toComputation')]: () => ['$0.prototype', [ctor]] }
    }
    return ['Object.create($0, $1)', [proto, descriptors]]
}

// array buffer and typed arrays _could_ be turned into computations, though their specific representation would be bloated
