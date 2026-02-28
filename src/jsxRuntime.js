"use strict";
(() => {
    function __setAttribute(el, key, v) {
        if (key === 'style' && typeof v === 'object') {
            if (!v) {
                el.style = ''
            } else {
                for (const k2 of keys(v)) {
                    el.style[k2] = v[k2]
                }
            }
        } else {
            if (typeof v === 'function') {
                el.addEventListener(key.slice(2).toLowerCase(), v)
                return
            }
            if (key === 'value') {
                el.value = v
                return
            }
            if (v == null) {
                el.removeAttribute(key)
            } else {
                el.setAttribute(key, v)
            }
        }
        return v
    }

    function __setSpreadAttribute(el, c, v, isComp) {
        if (c != null) {
            // todo: improve perf using a light diff
            for (const k of keys(c)) {
                if (isComp) {
                    delete el._p[k]
                } else {
                    __setAttribute(el, k, null)
                }
            }
        }
        if (v != null) {
            for (const k of keys(v)) {
                if (isComp) {
                    el._p[k] = v[k]
                } else {
                    __setAttribute(el, k, v[k])
                }
            }
        }
        return { ...v }
    }

    function updateComponent(o) {
        if (o._d) o._d = 0
        o._u?.()
        const t = o._r
        if (typeof t === 'function') {
            o._r = t(o._p)
            o._b?.replaceWith(o._r)
        } else {
            if (t.dispatchEvent(new CustomEvent('parentupdate', { cancelable: true })))
                __jsx_update(t)
        }
    }

    function __comp(wc) {
        return wc ? O.setPrototypeOf(doc.createElement(componentName), proxyProto) : {
            _p: {},
            [SYM_UPDATE]() { updateComponent(this) },
        }
    }

    const SYM_UPDATE = Symbol.for('update')
    Symbol.update = SYM_UPDATE

    class SynComponent extends HTMLElement {
        _p = {};    // props
        _u;         // update callback
        _r;         // root element
        _rp;        // root element prototype
        _d;         // children slot delta

        [SYM_UPDATE]() {
            updateComponent(this)
        }

        connectedCallback() {
            // this.replaceWith(this._r)
            synProto.replaceWith.call(this, this._r)
        }
    }

    let timer = 0

    // grouped variable statements to minify better
    const globals = globalThis
    const doc = document
    const O = Object
    const keys = O.keys
    const synProto = SynComponent.prototype
    const delegated = new Set()
    const cache = new Map()
    const setCacheTimer = () => setTimeout(clean, 1000)
    const __t = s => {
        timer ||= setCacheTimer()
        let info = cache.get(s)
        if (!info) cache.set(s, info = { c: 0, n: null })
        if (info.c < 5) info.c += 1
        if (info.c > 1) info.n ??= parse(s)
        // if (info.n) return info.n.cloneNode(true)
        return info.n?.cloneNode(true) ?? parse(s)
    }
    const componentName = 'syn-c'
    const R = Reflect
    const owned = R.ownKeys(synProto)
    const getRootElProto = r => r._rp ??= O.getPrototypeOf(r._r)
    const proxyProto = new Proxy(synProto, {
        get: (t, p, r) => {
            if (owned.includes(p)) return R.get(t, p, r)
            // note: we assume no getters returning a function
            const v = R.get(getRootElProto(r), p, r._r)
            return r._r && typeof v === 'function' ? v.bind(r._r) : v
        },
        set: (t, p, v, r) => R.set(getRootElProto(r), p, v, r._r),
        getPrototypeOf: t => t, 
    })

    // things the proxy breaks on:
    // * instanceof - it won't tell you what the underlying element is beyond HTMLElement (use .tagName)
    // * reference equality with the underlying (use .isSameNode or .isEqualNode)
    // * mutation beyond whatever the DOM API handles isn't shared between the nodes (TODO)
    // * unbound methods / using a different receiver 

    function clean() {
        for (let [k, v] of cache) if (--v.c === 0) cache.delete(k)
        timer = cache.size ? setCacheTimer() : 0
    }

    function parse(s) {
        // TODO: svg, mathml
        // const t = isMathML
        //   ? document.createElementNS("http://www.w3.org/1998/Math/MathML", "template")
        //   : document.createElement("template");
        // t.innerHTML = html;

        // return isSVG ? t.content.firstChild.firstChild : isMathML ? t.firstChild : t.content.firstChild;

        const t = doc.createElement('template')
        t.innerHTML = s
        return t.content.firstChild
    }

    function delegate(name) {
        if (!delegated.has(name)) {
            delegated.add(name)
            doc.addEventListener(name, handleEvent)
        }
    }

    function handleEvent(ev) {
        let node = ev.target
        let stopped
        let patched
        const patch = () => {
            if (!patched) {
                patched = 1
                O.defineProperty(ev, 'currentTarget', {
                    enumerable: true,
                    configurable: true,
                    get: () => node,
                })
                const k = 'stopPropagation'
                const originalStop = ev[k]
                ev[k] = function(...args) {
                    stopped = 1
                    return originalStop.call(this, ...args)
                }
            }
            return ev
        }
        const key = `__${ev.type}`
        while (node) {
            const handler = node[key]
            if (handler && !node.disabled) {
                handler.call(node, patch())
                if (stopped) break
            }
            node = node.parentElement
        }
    }

    function __jsx_update(el) {
        el?.[SYM_UPDATE]?.()
    }

    // l = len, s = start
    function setComponentSlot(p, c, v, parent, isSpread) {
        const children = parent._p.children
        if (p.s === undefined) {
            p.s = children.length
            p.l = 1
            parent._d = 0
        }
        const len = isSpread ? v.length : 1
        const s = p.s + parent._d
        if (!v || !len) {
            if (p.l > 0) {
                children.splice(s, p.l)
                parent._d -= p.l
                p.l = 0
            }
            return p
        }
        if (isSpread) {
            children.splice(s, p.l, ...v)
        } else {
            children.splice(s, p.l, v)
        }
        parent._d -= p.l - (p.l = len)
        return v
    }

    function unwrapComp(v) {
        return O.getPrototypeOf(v) === proxyProto ? v._r : v
    }

    function unwrapCompArr(v) {
        // bail out of the optimization if there's no component
        if (!v.length || unwrapComp(v[0]) === v[0]) return v
        return v.map(unwrapComp)
    }

    function __setSlot(p, c, v, parent) {
        if (p && !p.ELEMENT_NODE) {
            return setComponentSlot(p, c, v, parent, 0)
        }
        if (parent) {
            if (v == null) {
                parent.textContent = ''
            } else if (typeof v !== 'object') {
                const fc = c != null ? parent.firstChild : c
                if (fc?.nodeType === 3) {
                    fc.nodeValue = v
                } else {
                    parent.textContent = v
                }
            } else {
                parent.replaceChildren(unwrapComp(v))
            }
            return v
        }
        if (v == null) {
            if (p !== c) c.replaceWith(p)
            return p
        }
        if (typeof v !== 'object') {
            const t = doc.createTextNode(v) // throws on symbols
            c.replaceWith(t)
            return t
        }
        v = unwrapComp(v)
        c.replaceWith(v)
        return v
    }

    function clearRange(p) {
        const r = new Range()
        r.setStartAfter(p)
        r.setEndBefore(p.__e)
        r.deleteContents()
    }

    function __setSpreadSlot(p, c, v, parent) {
        if (p && !p.ELEMENT_NODE) {
            return setComponentSlot(p, c, v, parent, 1)
        }
        if (parent) {
            if (v == null) {
                parent.replaceChildren()
            } else {
                parent.replaceChildren(...unwrapCompArr(v))
            }
            return v
        }
        // TODO: compiler should emit range markers instead so we can better handle spreads on the starts/ends
        if (!p.__e) {
            p.__e = p.cloneNode()
            p.after(p.__e)
        }
        if (v == null) {
            if (p !== c) clearRange(p)
            return p
        }
        clearRange(p)
        p.after(...unwrapCompArr(v))
        return v
    }

    if (!globals.__t) {
        customElements.define(componentName, SynComponent)
        delegate('click')
        O.assign(globals, {
            __t,
            __jsx_update,
            __setSlot,
            __setSpreadSlot,
            __setAttribute,
            __setSpreadAttribute,
            __comp,
        })
    }
})()