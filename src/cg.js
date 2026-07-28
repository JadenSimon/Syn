// chandelier graph

function condense(supercell) {
    let index = 0
    const stack = []
    const components = []

    for (const x of supercell.cells) {
        if (x.index === undefined) {
            strongconnect(x)
        }
    }

    for (const c of components) {
        for (const x of c) {
            x.onstack = false
            x.index = undefined
            x.lowlink = undefined
        }
    }

    function strongconnect(v) {
        v.index = index
        v.lowlink = index
        index += 1
        stack.push(v)
        v.onstack = true
        for (const w of v.refs) {
            if (w.index === undefined) {
                strongconnect(w)
                v.lowlink = Math.min(v.lowlink, w.lowlink)
            } else if (w.onstack) {
                v.lowlink = Math.min(v.lowlink, w.index)
            }
        }
        if (v.lowlink === v.index) {
            const scc = []
            while (true) {
                const w = stack.pop()
                w.onstack = false
                scc.push(w)
                if (w === v) break
            }
            components.push(scc)
        }
    }

    return components
}


const pages = []
function allocPage() {
    const b = new ArrayBuffer(4 * 1024 * 1024)
    pages.push(b)
    return [pages.length-1, b]
}

// out verticals are always destination leaf edges
// they are used as an optimization to coalesce many outbound refs
// these are still recorded at the peer-to-peer edge, you can think of it as being staggered a layer 
function createScView(pageBuf, offset) {
    const maxCells = 64
    const vertSize = 2 // not vertices, but "verticals"
    const numVerticals = 8
    const v = new DataView(pageBuf, offset)
    function getParentRef() {
        return v.getUint32(0, true)
    }
    function setParentRef(val) {
        return v.setUint32(0, val, true)
    }
    function getCellCount() {
        return v.getUint8(4)
    }
    function setCellCount(val) {
        return v.setUint8(4, val)
    }
    function getDepth() {
        return v.getUint8(5)
    }
    function setDepth(val) {
        return v.setUint8(5, val)
    }
    function setFlags() {
        return v.getUint8(6)
    }
    function getFlags(val) {
        return v.setUint8(6, val)
    }
    function getLocalRc(id) {
        return v.getUint8(7+id)
    }
    function setLocalRc(id, val) {
        return v.setUint8(7+id, val)
    }
    function incLocalRc(id) {
        const nv = getLocalRc(id) + 1
        setLocalRc(id, nv)
        return nv
    }
    function decLocalRc(id) {
        const nv = getLocalRc(id) - 1
        setLocalRc(id, nv)
        return nv
    }
    // in/out verts are 16 bytes each inline. the last word is an allocated extension
    function getInVert(idx) {
        return [v.getUint8(7+maxCells+(idx*2)), v.getUint8(7+maxCells+(idx*2)+1)]
    }
    // id should be biased +1
    function setInVert(idx, id, val) {
        v.setUint8(7+maxCells+(idx*2), id)
        v.setUint8(7+maxCells+(idx*2)+1, val)
    }
    const outVerticalsTotalSize = numVerticals*5
    const outVertStart = 7+maxCells+16
    function getOutVert(idx) {
        return [v.getUint32(outVertStart+(idx*5), true), v.getUint8(outVertStart+(idx*5)+4)]
    }
    function setOutVert(idx, id, val) {
        v.setUint32(outVertStart+(idx*5), id, true)
        v.setUint8(outVertStart+(idx*5)+4, val)
    }
    // peer outbound edges, 16 bytes inline w/ allocated extension
    const peerEdgeStart = outVertStart+outVerticalsTotalSize
    function getPeerEdge(idx) {
        return [v.getUint8(peerEdgeStart+(idx*2)), v.getUint8(peerEdgeStart+(idx*2)+1)]
    }
    function setPeerEdge(idx, id, val) {
        v.setUint8(peerEdgeStart+(idx*2), id)
        v.setUint8(peerEdgeStart+(idx*2)+1, val)
    }
    const heapCellStart = peerEdgeStart+16
    function findInVert(id) {
        for (let i = 0; i < 6; i++) {
            if (v.getUint8(7+maxCells+(i*2)) === id) {
                return v.getUint8(7+maxCells+(i*2)+1)
            }
        }
    }
    function findOutVert(id) {
        for (let i = 0; i < 6; i++) {
            if (v.getUint32(outVertStart+(i*5)) === id) {
                return v.getUint8(outVertStart+(i*5)+4)
            }
        }
    }
    function findPeerEdge(id) {
        for (let i = 0; i < 6; i++) {
            if (v.getUint8(peerEdgeStart+(i*2)) === id) {
                return v.getUint8(peerEdgeStart+(i*2)+1)
            }
        }
    }
    function putInVert(id, val) {
        for (let i = 0; i < 6; i++) {
            const k = v.getUint8(7+maxCells+(i*2))
            if (k === id) {
                if (val === 0) {
                    let j = i+1
                    for (; j < 6; j++) {
                        if (v.getUint8(7+maxCells+(j*2)) === 0) {
                            break
                        }
                    }
                    if (i+1 === j) {
                        v.setUint8(7+maxCells+(i*2), 0)
                    } else {
                        const [id2, val2] = getInVert(j)
                        v.setUint8(7+maxCells+(j*2), 0)
                        v.setUint8(7+maxCells+(i*2), id2)
                        v.setUint8(7+maxCells+(i*2)+1, val2)
                    }
                } else {
                    v.setUint8(7+maxCells+(i*2)+1, val)
                }
                return
            }
            if (k === 0) {
                if (val === 0) throw new Error('what')
                v.setUint8(7+maxCells+(i*2), id)
                v.setUint8(7+maxCells+(i*2)+1, val)
                return
            }
        }
        throw new Error('out of room!')
    }
    function putOutVert(id, val) {
        const base = outVertStart
        const verticalSize = 5
        for (let i = 0; i < 6; i++) {
            const k = v.getUint32(base+(i*verticalSize), true)
            if (k === id) {
                if (val === 0) {
                    let j = i+1
                    for (; j < 6; j++) {
                        if (v.getUint32(base+(j*verticalSize), true) === 0) {
                            break
                        }
                    }
                    if (i+1 === j) {
                        v.setUint32(base+(i*verticalSize), 0, true)
                    } else {
                        const [id2, val2] = getOutVert(j)
                        v.setUint32(base+(j*verticalSize), 0, true)
                        v.setUint32(base+(i*verticalSize), id2, true)
                        v.setUint8(base+(i*verticalSize)+4, val2)
                    }
                } else {
                    v.setUint8(base+(i*verticalSize)+4, val)
                }
                return
            }
            if (k === 0) {
                if (val === 0) throw new Error('what')
                v.setUint32(base+(i*verticalSize), id, true)
                v.setUint8(base+(i*verticalSize)+4, val)
                return
            }
        }
        throw new Error('out of room!')
    }
    function collectOutVerticals() {
        const arr = []
        for (let i = 0; i < 6; i++) {
            const k = v.getUint32(base+(i*verticalSize), true)
            if (k === 0) break
            arr.push(k)
        }
        return arr
    }
    // returns true when a vertical edge was added
    function incOutVertical(id) {
        const base = outVertStart
        const verticalSize = 5
        for (let i = 0; i < 6; i++) {
            const k = v.getUint32(base+(i*verticalSize), true)
            if (k === id) {
                v.setUint8(base+(i*verticalSize)+4, v.setUint8(base+(i*verticalSize)+4) + 1)
                return false
            }
            if (k === 0) {
                v.setUint32(base+(i*verticalSize), id, true)
                v.setUint8(base+(i*verticalSize)+4, 1)
                return true
            }
        }
        throw new Error('out of room!')
    }
    // returns true when a vertical edge was removed
    function decOutVertical(id) {
        const base = outVertStart
        const verticalSize = 5
        for (let i = 0; i < 6; i++) {
            const k = v.getUint32(base+(i*verticalSize), true)
            if (k !== id) continue
            const val = v.getUint8(base+(i*verticalSize)+4)
            if (val === 1) {
                let j = i+1
                for (; j < 6; j++) {
                    if (v.getUint32(base+(j*verticalSize), true) === 0) {
                        break
                    }
                }
                if (i+1 === j) {
                    v.setUint32(base+(i*verticalSize), 0, true)
                } else {
                    const [id2, val2] = getOutVert(j)
                    v.setUint32(base+(j*verticalSize), 0, true)
                    v.setUint32(base+(i*verticalSize), id2, true)
                    v.setUint8(base+(i*verticalSize)+4, val2)
                }
                return true
            }
            v.setUint8(base+(i*verticalSize)+4, val - 1)
            return false
        }
        throw new Error('not found!')
    }
    function putPeerEdge(id, val) {
        const base = peerEdgeStart
        for (let i = 0; i < 6; i++) {
            const k = v.getUint8(base+(i*2))
            if (k === id) {
                if (val === 0) {
                    let j = i+1
                    for (; j < 6; j++) {
                        if (v.getUint8(base+(j*2)) === 0) {
                            break
                        }
                    }
                    if (i+1 === j) {
                        v.setUint8(base+(i*2), 0)
                    } else {
                        const [id2, val2] = getPeerEdge(j)
                        v.setUint8(base+(j*2), 0)
                        v.setUint8(base+(i*2), id2)
                        v.setUint8(base+(i*2)+1, val2)
                    }
                } else {
                    v.setUint8(base+(i*2)+1, val)
                }
                return
            }
            if (k === 0) {
                if (val === 0) throw new Error('what')
                v.setUint8(base+(i*2), id)
                v.setUint8(base+(i*2)+1, val)
                return
            }
        }
        throw new Error('out of room!')
    }
    // these assumed unbiased
    function getHeapCellId(idx) {
        return v.getUint32(heapCellStart+idx*4, true)
    }
    function setHeapCellId(idx, val) {
        return v.setUint32(heapCellStart+idx*4, val, true)
    }
    function pushHeapCell(val) {
        const id = getCellCount()
        setCellCount(id + 1)
        setHeapCellId(id, val)
        return id + 1
    }
    function removeHeapCell(relId) {
        const count = getCellCount()
        setHeapCellId(relId-1, 0)
        if (count === relId) {
            setCellCount(count - 1)
        } else {
            // free list
        }
        return
    }
    // XXX
    function getSelfLocalId() {
        return v.getUint8(heapCellStart+(64*4))
    }
    function setSelfLocalId(val) {
        return v.setUint8(heapCellStart+(64*4), val)
    }
    const eternalBit = 0x01
    const dirtyBit = 0x02

    function hasFlag(bit) {
        return (getFlags() & bit) === bit
    }
    function setFlagBit(bit) {
        setFlags(val ? (flags & ~bit) : (flags | bit))
    }

    function isEternal() {
        return (getFlags() & eternalBit) === eternalBit
    }
    function setEternal(val) {
        const flags = getFlags()
        setFlags(!val ? (flags & ~eternalBit) : (flags | eternalBit))
    }
    function isDirty() {
        return (getFlags() & dirtyBit) === dirtyBit
    }
    function setDirty(val) {
        const flags = getFlags()
        setFlags(!val ? (flags & ~dirtyBit) : (flags | dirtyBit))
    }

    return {
        get depth() {
            return getDepth()
        },
        getParentRef,
        setParentRef,
        getCellCount,
        setCellCount,
        getDepth,
        setDepth,
        setFlags,
        getFlags,
        getLocalRc,
        setLocalRc,
        getInVert,
        setInVert,
        getOutVert,
        setOutVert,
        getPeerEdge,
        setPeerEdge,
        findInVert,
        findOutVert,
        findPeerEdge,
        putInVert,
        putOutVert,
        putPeerEdge,
        incLocalRc,
        decLocalRc,
        incOutVertical,
        decOutVertical,
        collectOutVerticals,
        getHeapCellId,
        setHeapCellId,
        getSelfLocalId,
        getSelfLocalId,
        isEternal,
        setEternal,
        isDirty,
        setDirty,
        pushHeapCell,
        removeHeapCell,
        getSelfLocalId,
        setSelfLocalId,
    }
}

function getScId(handle) {
    return (handle >> 8) & 0xFFFFFF
}

function getRelId(handle) {
    return handle & 0xFF
}

function toAbsoluteHandle(scId, relId) {
    return (scId << 8) | relId
}

function isSc(handle) {
    return handle >> 30 === 1
}

const scMap = new Map()
function createSc(parent, isRoot = false) {
    parent = typeof parent === 'number' ? parent : parent.id
    const id = isRoot ? 0 : scMap.size+1
    const [_, page] = allocPage()
    const view = createScView(page, 0)
    const self = { 
        id,
        parent,
        ...view,
    }
    view.setParentRef(id)
    if (!isRoot) {
        self.setSelfLocalId(scMap.get(parent).pushHeapCell(self.id))
    }
    const parentDepth = scMap.get(parent)?.getDepth()
    view.setDepth((parentDepth !== undefined) ? parentDepth + 1 : 0)
    scMap.set(id, self)
    return self
}

function addEdge(from, to) {
    const sc1 = getScId(from)
    const sc2 = getScId(to)
    if (sc1 === sc2) {
        const sc = scMap.get(sc1)
        sc.incLocalRc(getRelId(to))
        return
    }
    let fromSc = scMap.get(sc1)
    if (!fromSc.incOutVertical(to)) return
    let toSc = scMap.get(sc2)
    addInboundVertical(toSc, to)
    while (fromSc.parent !== toSc.parent) {
        const d1 = fromSc.depth
        const d2 = toSc.depth 
        if (d1 >= d2) {
            fromSc = scMap.get(fromSc.parent)
            if (!fromSc.incOutVertical(to)) return
        }
        if (d1 <= d2) {
            const next = scMap.get(toSc.parent)
            addInboundVertical(next, (toSc.id | (1 << 30)))
            toSc = next
        }
    }
    const toId = toSc.getSelfLocalId()
    const val = fromSc.findPeerEdge(toId) ?? 0
    fromSc.putPeerEdge(toId, val + 1)
    scMap.get(fromSc.parent).incLocalRc(toId)
}

function addInboundVertical(fromSc, toHandle) {
    const relId = isSc(toHandle) ? scMap.get(toHandle & ~(1 << 30)).getSelfLocalId() : getRelId(toHandle)
    const invc = fromSc.findInVert(relId) ?? 0
    fromSc.putInVert(relId, invc + 1)
}

function removeInboundVertical(fromSc, toHandle) {
    const relId = isSc(toHandle) ? scMap.get(toHandle & ~(1 << 30)).getSelfLocalId() : getRelId(toHandle)
    const invc = fromSc.findInVert(relId)
    if (!invc) throw new Error('??? no: ' + relId + ' : ' + fromSc + ' : ' + toHandle)
    fromSc.putInVert(relId, invc - 1)
    if (invc === 1) {
        if (fromSc.getLocalRc(relId) === 0) {
            free(toHandle)
        }
    }
}

function free(handle) {
    if (isSc(handle)) {
        const scid = handle & ~(1 << 30)
        const sc = scMap.get(scid)
        const parentScid = sc.parent
        const parent = scMap.get(parentScid)
        const idx = sc.getSelfLocalId()
        parent.removeHeapCell(idx)
        console.log('freed SC', scid)
    } else {
        const scid = getScId(handle)
        const sc = scMap.get(scid)
        const idx = getRelId(handle)
        sc.removeHeapCell(idx)
        clearLeafEdges(handle)
        console.log('freed', scid, idx)
    }
}

function removeEdge(from, to) {
    const sc1 = getScId(from)
    const sc2 = getScId(to)
    if (sc1 === sc2) {
        const sc = scMap.get(sc1)
        const relId = getRelId(to)
        if (sc.decLocalRc(relId) === 0) {
            if (!sc.findInVert(relId)) {
                free(to)
            }
        }
        return
    }
    removeScEdge(sc1, sc2, to)
}

function removeScEdge(scid1, scid2, to) {
    let fromSc = scMap.get(scid1)
    if (!fromSc.decOutVertical(to)) return
    let toSc = scMap.get(scid2)
    removeInboundVertical(toSc, to)
    while (fromSc.parent !== toSc.parent) {
        const d1 = fromSc.depth
        const d2 = toSc.depth 
        if (d1 >= d2) {
            fromSc = scMap.get(fromSc.parent)
            if (!fromSc.decOutVertical(to)) return
        }
        if (d1 <= d2) {
            const next = scMap.get(toSc.parent)
            removeInboundVertical(next, (toSc.id | (1 << 30)))
            toSc = next
        }
    }
    const toId = toSc.getSelfLocalId()
    const val = fromSc.findPeerEdge(toId) ?? 0
    fromSc.putPeerEdge(toId, val - 1)
    const parent = scMap.get(fromSc.parent)
    parent.decLocalRc(toId)
    if (val === 1) {
        if (!parent.findInVert(toId)) {
            free((toSc.id | (1 << 30)))
        }
    }
}

function createNestedTree(depth, parent = root) {
    let t = createSc(parent)
    while (depth > 0) {
        t = createSc(t)
        depth -= 1
    }
    return t
}

function clearScEdges(sc) {

}

function clearLeafEdges(handle) {
    const m = edgeMap.get(handle)
    if (!m) return
    for (const [k, v] of m) {
        removeEdge(handle, k)
    }
}

const edgeMap = new Map() // temporary, cells need to have their own data structures
function addTestEdge(from, to) {
    let m = edgeMap.get(from)
    if (!m) {
        edgeMap.set(from, m = new Map())
    }
    m.set(to, (m.get(to) ?? 0) + 1)
    addEdge(from, to)
}
function removeTestEdge(from, to) {
    let m = edgeMap.get(from)
    if (!m) {
        edgeMap.set(from, m = new Map())
    }
    const nv = (m.get(to) ?? 0) - 1
    if (nv === 0) {
        m.delete(to)
    } else {
        m.set(to, nv)
    }
    removeEdge(from, to)
}

const root = createSc(0, true)
const t1 = createSc(root)
const t2 = createSc(root)
const t3 = createSc(t2)
const t4 = createSc(t3)

t1.putInVert(1, 1)
console.log(t1.findInVert(1))
t1.incLocalRc(1)
t1.incLocalRc(1)
t1.incLocalRc(1)
console.log(t1.getLocalRc(1))
console.log(t1.decLocalRc(1), t1.getLocalRc(1))
const testId1 = toAbsoluteHandle(t1.id, 1)
const testId2 = toAbsoluteHandle(t2.id, 2)
const testId3 = toAbsoluteHandle(t2.id, 4)
const testId4 = toAbsoluteHandle(t3.id, 5)
const testId5 = toAbsoluteHandle(t3.id, 6)
const deepTestId1 = toAbsoluteHandle(t4.id, 7)

addEdge(testId1, testId3) // anchor
addEdge(testId1, testId4) // anchor

// addEdge(testId1, testId2)
// removeEdge(testId1, testId2)

addEdge(testId1, testId5)
removeEdge(testId1, testId5)


addEdge(testId1, deepTestId1)
removeEdge(testId1, deepTestId1)


function testRun() {
    const ops = 100_000
    const p = performance.now()
    // addEdge(testId1, testId2)
    addEdge(testId1, testId5)
    for (let i = 0; i < ops; i++) {
        addEdge(testId1, testId5)
        removeEdge(testId1, testId5)
    }
    console.log(((performance.now()-p)/ops)*1e6, 'ns/op')
}

testRun()