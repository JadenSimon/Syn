// chandelier graph

// unused tarjan's
// not all that useful for a minimal CG. only needed when you're desparate for mem.
//
// function condense(supercell) {
//     let index = 0
//     const stack = []
//     const components = []

//     for (const x of supercell.cells) {
//         if (x.index === undefined) {
//             strongconnect(x)
//         }
//     }

//     for (const c of components) {
//         for (const x of c) {
//             x.onstack = false
//             x.index = undefined
//             x.lowlink = undefined
//         }
//     }

//     function strongconnect(v) {
//         v.index = index
//         v.lowlink = index
//         index += 1
//         stack.push(v)
//         v.onstack = true
//         for (const w of v.refs) {
//             if (w.index === undefined) {
//                 strongconnect(w)
//                 v.lowlink = Math.min(v.lowlink, w.lowlink)
//             } else if (w.onstack) {
//                 v.lowlink = Math.min(v.lowlink, w.index)
//             }
//         }
//         if (v.lowlink === v.index) {
//             const scc = []
//             while (true) {
//                 const w = stack.pop()
//                 w.onstack = false
//                 scc.push(w)
//                 if (w === v) break
//             }
//             components.push(scc)
//         }
//     }

//     return components
// }


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
    const inVerticalsStart = 7
    // in/out verts are 16 bytes each inline. the last word is an allocated extension
    function getInVert(idx) {
        return [v.getUint8(inVerticalsStart+(idx*2)), v.getUint8(inVerticalsStart+(idx*2)+1)]
    }
    // id should be biased +1
    function setInVert(idx, id, val) {
        v.setUint8(inVerticalsStart+(idx*2), id)
        v.setUint8(inVerticalsStart+(idx*2)+1, val)
    }
    const outVerticalsTotalSize = numVerticals*5
    const outVertStart = inVerticalsStart+16
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
            if (v.getUint8(inVerticalsStart+(i*2)) === id) {
                return v.getUint8(inVerticalsStart+(i*2)+1)
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
            const k = v.getUint8(inVerticalsStart+(i*2))
            if (k === id) {
                if (val === 0) {
                    let j = i+1
                    for (; j < 6; j++) {
                        if (v.getUint8(inVerticalsStart+(j*2)) === 0) {
                            break
                        }
                    }
                    if (i+1 === j) {
                        v.setUint8(inVerticalsStart+(i*2), 0)
                    } else {
                        const [id2, val2] = getInVert(j)
                        v.setUint8(inVerticalsStart+(j*2), 0)
                        v.setUint8(inVerticalsStart+(i*2), id2)
                        v.setUint8(inVerticalsStart+(i*2)+1, val2)
                    }
                } else {
                    v.setUint8(inVerticalsStart+(i*2)+1, val)
                }
                return
            }
            if (k === 0) {
                if (val === 0) throw new Error('what')
                v.setUint8(inVerticalsStart+(i*2), id)
                v.setUint8(inVerticalsStart+(i*2)+1, val)
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
        const base = outVertStart
        const verticalSize = 5
        const arr = []
        for (let i = 0; i < 6; i++) {
            const k = v.getUint32(base+(i*verticalSize), true)
            if (k === 0) break
            // arr.push([k, v.getUint8(base+(i*verticalSize)+4)])
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
    // we want to pack ref counts right beside the cells
    // the layout is ugly
    function getLocalRc(id) {
        return v.getUint8(heapCellStart+(id-1)*5)
    }
    function setLocalRc(id, val) {
        return v.setUint8(heapCellStart+(id-1)*5, val)
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
    // these assumed unbiased
    function getHeapCellId(idx) {
        return v.getUint32(heapCellStart+idx*5+1, true)
    }
    function setHeapCellId(idx, val) {
        return v.setUint32(heapCellStart+idx*5+1, val, true)
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
    function freeHeapCell(relId, expected) {
        const addr = getHeapCellId(relId-1)
        if (addr === 0) return false
        // we aren't using "real" refs
        // if (addr !== expected) throw new Error(`bad free: ${addr} !== ${expected} [idx: ${relId}]`)
        removeHeapCell(relId)
        return true
    }
    function collectHeapCells() {
        const arr = []
        const c = getCellCount()
        for (let i = 0; i < c; i++) {
            const k = getHeapCellId(i)
            if (k === 0) break
            arr.push([i+1, k])
        }
        return arr
    }
    const heapCellEnd = heapCellStart+(maxCells*5)
    // XXX
    function getSelfLocalId() {
        return v.getUint8(heapCellEnd)
    }
    function setSelfLocalId(val) {
        return v.setUint8(heapCellEnd, val)
    }
    const eternalBit = 0x01
    const dirtyBit = 0x02
    const cutawayBit = 0x04
    const freedBit = 0x08

    function hasFlag(bit) {
        return (getFlags() & bit) === bit
    }
    function setFlagBit(bit, val) {
        const flags = getFlags()
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
    function isCutaway() {
        return hasFlag(cutawayBit)
    }
    function setCutaway(val) {
        setFlagBit(cutawayBit, val)
    }
    function isFreed() {
        return hasFlag(freedBit)
    }
    function setFreed(val) {
        setFlagBit(freedBit, val)
    }
    function printInfo() {
        console.log(`--- ${getCellCount()} cells ---`)
        for (const x of collectHeapCells()) {
            const name = isSc(x[1]) ? `${x[1] & ~(1 << 30)} (sc)` : x[1]
            console.log(`  ${x[0]}: ${name} (${getLocalRc(x[0])})`)
        }
        console.log('--- out verticals ---')
        for (const x of collectOutVerticals()) {
            console.log(`  ${x}: ${findOutVert(x)}`)
        }
        console.log('--- in verticals ---')
        {
            for (let i = 0; i < 6; i++) {
                const c = getInVert(i)
                if (c[0] === 0) break
                console.log(`  ${c[0]}: ${c[1]}`)
            }
        }
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
        collectHeapCells,
        getSelfLocalId,
        getSelfLocalId,
        isEternal,
        setEternal,
        isDirty,
        setDirty,
        isCutaway,
        setCutaway,
        isFreed,
        setFreed,
        pushHeapCell,
        removeHeapCell,
        freeHeapCell,
        getSelfLocalId,
        setSelfLocalId,
        printInfo,
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

const scTable = []
function getSc(id) {
    return scTable[id]
}
function createSc(parent, isRoot = false) {
    parent = typeof parent === 'number' ? parent : parent.id
    const id = isRoot ? 0 : scTable.length+1
    const [_, page] = allocPage()
    const view = createScView(page, 0)
    const self = { 
        id,
        parent,
        ...view,
    }
    view.setParentRef(id)
    if (!isRoot) {
        self.setSelfLocalId(getSc(parent).pushHeapCell(self.id | (1 << 30)))
    }
    const parentDepth = getSc(parent)?.getDepth()
    view.setDepth((parentDepth !== undefined) ? parentDepth + 1 : 0)
    scTable[id] = self
    return self
}

function addEdge(from, to) {
    const sc1 = getScId(from)
    const sc2 = getScId(to)
    if (sc1 === sc2) {
        const sc = getSc(sc1)
        sc.incLocalRc(getRelId(to))
        return
    }
    let fromSc = getSc(sc1)
    if (!fromSc.incOutVertical(to)) return
    let toSc = getSc(sc2)
    addInboundVertical(toSc, to)
    const arr = []
    while (fromSc.parent !== toSc.parent) {
        const d1 = fromSc.depth
        const d2 = toSc.depth 
        if (d1 >= d2) {
            fromSc = getSc(fromSc.parent)
            if (!fromSc.incOutVertical(to)) return
        }
        if (d1 <= d2) {
            const next = getSc(toSc.parent)
            // addInboundVertical(next, (toSc.id | (1 << 30)))
            arr.push(next, (toSc.id | (1 << 30)))
            toSc = next
        }
    }
    const toId = toSc.getSelfLocalId()
    const val = fromSc.findPeerEdge(toId) ?? 0
    fromSc.putPeerEdge(toId, val + 1)
    getSc(fromSc.parent).incLocalRc(toId)
    for (let i = 0; i < arr.length; i += 2) {
        addInboundVertical(arr[i], arr[i+1])
    }
}

function addInboundVertical(fromSc, toHandle) {
    const relId = isSc(toHandle) ? getSc(toHandle & ~(1 << 30)).getSelfLocalId() : getRelId(toHandle)
    const invc = fromSc.findInVert(relId) ?? 0
    fromSc.putInVert(relId, invc + 1)
}

function removeInboundVertical(fromSc, toHandle) {
    const relId = isSc(toHandle) ? getSc(toHandle & ~(1 << 30)).getSelfLocalId() : getRelId(toHandle)
    const invc = fromSc.findInVert(relId)
    if (!invc) throw new Error('??? no: ' + relId + ' : ' + fromSc + ' : ' + toHandle)
    fromSc.putInVert(relId, invc - 1)
    if (invc === 1) {
        if (fromSc.getLocalRc(relId) === 0) {
            free(toHandle)
        }
    }
}

function freeCutawaySc(scid, sc = getSc(scid)) {
    if (sc.isFreed()) return
    sc.setFreed(true)
    sc.setCutaway(true)
    freeScCells(sc)
    const parentScid = sc.parent
    const parent = getSc(parentScid)
    const idx = sc.getSelfLocalId()
    parent.removeHeapCell(idx)
    console.log('freed SC', scid)
}

function free(handle) {
    if (isSc(handle)) {
        const scid = handle & ~(1 << 30)
        const sc = getSc(scid)
        if (!sc.isCutaway()) {
            sc.setCutaway(true)
            clearScEdges(sc)
        }
        freeCutawaySc(scid, sc)
    } else {
        const scid = getScId(handle)
        const sc = getSc(scid)
        const idx = getRelId(handle)
        if (!sc.freeHeapCell(idx, handle)) return
        if (!sc.isCutaway()) {
            clearLeafEdges(handle)
        }
        console.log('freed', 'rel id', idx, 'scid', scid)
    }
}

function removeEdge(from, to) {
    const sc1 = getScId(from)
    const sc2 = getScId(to)
    if (sc1 === sc2) {
        const sc = getSc(sc1)
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
    let fromSc = getSc(scid1)
    if (!fromSc.decOutVertical(to)) return
    let toSc = getSc(scid2)
    removeInboundVertical(toSc, to)
    const arr = []
    while (fromSc.parent !== toSc.parent) {
        const d1 = fromSc.depth
        const d2 = toSc.depth 
        if (d1 >= d2) {
            fromSc = getSc(fromSc.parent)
            if (!fromSc.decOutVertical(to)) return
        }
        if (d1 <= d2) {
            const next = getSc(toSc.parent)
            arr.push(next, (toSc.id | (1 << 30)))
            // removeInboundVertical(next, (toSc.id | (1 << 30)))
            toSc = next
        }
    }
    if (removeScPeerEdge(fromSc, toSc)) return
    for (let i = 0; i < arr.length; i += 2) {
        removeInboundVertical(arr[i], arr[i+1])
    }
}

function removeScPeerEdge(fromSc, toSc) {
    const toId = toSc.getSelfLocalId()
    const val = fromSc.findPeerEdge(toId) ?? 0
    fromSc.putPeerEdge(toId, val - 1)
    const parent = getSc(fromSc.parent)
    parent.decLocalRc(toId)
    if (val === 1) {
        if (!parent.findInVert(toId)) {
            free((toSc.id | (1 << 30)))
            return true
        }
    }
    return false
}

function createNestedTree(depth, parent = root) {
    let t = createSc(parent)
    while (depth > 0) {
        t = createSc(t)
        depth -= 1
    }
    return t
}

function findScDepthPeerId(sc, leafHandle) {
    const scid2 = getScId(leafHandle)
    if (scid2 === sc.id) return scid2
    let sc2 = getSc(scid2)
    const td = sc.depth
    while (td !== sc2.depth) {
        sc2 = getSc(sc2.parent)
    }
    return sc2.id
}

function clearScEdges(sc) {
    for (const leafHandle of sc.collectOutVerticals()) {
        const scid2 = getScId(leafHandle)
        if (sc.id === scid2) {
            const toSc = getSc(scid2)
            if (!removeScPeerEdge(sc, toSc)) {
                removeInboundVertical(toSc, leafHandle)
            }
        } else {
            removeScEdge(sc.parent, scid2, leafHandle)
        }
    }
}

function freeScCells(sc) {
    for (const cell of sc.collectHeapCells()) {
        if (isSc(cell[1])) {
            freeCutawaySc(cell[1] & ~(1 << 30))
            continue
        }
        free(toAbsoluteHandle(sc.id, cell[0]))
    }
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

function createTestCell(sc) {
    const id = sc.pushHeapCell(0xFF00 + sc.getCellCount())
    return toAbsoluteHandle(sc.id, id)
}

const root = createSc(0, true)

function testScenario1() {
    const t1 = createSc(root)
    const t2_1 = createSc(root)
    const t2_2 = createSc(root)
    const top = createTestCell(t1)
    const a = createTestCell(t2_1)
    const b = createTestCell(t2_2)
    const b2 = createTestCell(t2_2)
    addTestEdge(b, b2)
    addTestEdge(b2, b)
    addTestEdge(top, a)
    addTestEdge(a, b)
    removeEdge(top, a)
}

testScenario1()

function benchEdgeToggle() {
    const t1 = createSc(root)
    const t2_1 = createSc(t1)
    const t2_2 = createSc(t1)
    const a = createTestCell(t2_1)
    const b = createTestCell(t2_2)

    const ops = 100_000
    const p = performance.now()
    addEdge(a, b)
    for (let i = 0; i < ops; i++) {
        addEdge(a, b)
        removeEdge(a, b)
    }
    console.log(((performance.now()-p)/ops)*1e6, 'ns/op')
}

 benchEdgeToggle()