// chandelier graph

const putInVertTrace = []
function traceInVert(scId, relIdx, val) {
    const stack = new Error().stack.split('\n').slice(2, 6).join(' | ')
    putInVertTrace.push({ scId, relIdx, val, stack })
    if (putInVertTrace.length > 200000) putInVertTrace.shift()
}

const pages = []
function allocPage() {
    const b = new ArrayBuffer(4 * 1024)
    pages.push(b)
    return [pages.length-1, b]
}

// out verticals are always destination leaf edges
// they are used as an optimization to coalesce many outbound refs
// these are still recorded at the peer-to-peer edge, you can think of it as being staggered a layer 
function createScView(pageBuf, offset, debugScId) {
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
    function getDepthExtent() {
        return v.getUint8(6)
    }
    function setDepthExtent(val) {
        return v.setUint8(6, val)
    }
    function getFlags() {
        return v.getUint8(7)
    }
    function setFlags(val) {
        return v.setUint8(7, val)
    }
    const inVerticalsStart = 8
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
    let allocatedInVerticals
    let allocatedOutVerticals
    let allocatedPeerEdges
    let allocatedLocalRc
    // localRc is directly indexed by relId (not a scanned small table), so
    // a plain array (id-1 -> count) is the natural overflow backing.
    function allocLocalRc() {
        allocatedLocalRc = []
        for (let id = 1; id <= maxCells; id++) {
            allocatedLocalRc[id-1] = v.getUint8(heapCellStart+(id-1)*5)
        }
    }
    // these may now be triggered *before* all 6 inline slots are filled
    // (proactively, to avoid a u8 counter overflowing) -- stop at the
    // first empty slot instead of assuming all 6 are occupied.
    function allocInVerts() {
        allocatedInVerticals = new Map()
        for (let i = 0; i < 6; i++) {
            const k = v.getUint8(inVerticalsStart+(i*2))
            if (k === 0) break
            const cur = v.getUint8(inVerticalsStart+(i*2)+1)
            if (cur === 0) throw new Error(`allocInVerts: stale inline entry k=${k} cur=0 at slot ${i}`)
            allocatedInVerticals.set(k, cur)
        }
    }
    function allocOutVerts() {
        const base = outVertStart
        const verticalSize = 5
        allocatedOutVerticals = new Map()
        for (let i = 0; i < 6; i++) {
            const k = v.getUint32(base+(i*verticalSize), true)
            if (k === 0) break
            const cur = v.getUint8(base+(i*verticalSize)+4)
            allocatedOutVerticals.set(k, cur)
        }
    }
    function allocPeerEdges() {
        allocatedPeerEdges = new Map()
        for (let i = 0; i < 6; i++) {
            const k = v.getUint8(peerEdgeStart+(i*2))
            if (k === 0) break
            const cur = v.getUint8(peerEdgeStart+(i*2)+1)
            allocatedPeerEdges.set(k, cur)
        }
    }

    const heapCellStart = peerEdgeStart+16
    function findInVert(relIdx) {
        if (allocatedInVerticals) {
            return allocatedInVerticals.get(relIdx)
        }
        for (let i = 0; i < 6; i++) {
            if (v.getUint8(inVerticalsStart+(i*2)) === relIdx) {
                return v.getUint8(inVerticalsStart+(i*2)+1)
            }
        }
    }
    function findOutVert(absHandle) {
        if (allocatedOutVerticals) {
            return allocatedOutVerticals.get(absHandle)
        }
        for (let i = 0; i < 6; i++) {
            if (v.getUint32(outVertStart+(i*5)) === absHandle) {
                return v.getUint8(outVertStart+(i*5)+4)
            }
        }
    }
    function checkOneByteOverflow(v) {
        if (v >= 256) throw new Error(`u8 overflow: ${v}`)
    }
    function findPeerEdge(id) {
        if (allocatedPeerEdges) {
            return allocatedPeerEdges.get(id)
        }
        for (let i = 0; i < 6; i++) {
            if (v.getUint8(peerEdgeStart+(i*2)) === id) {
                return v.getUint8(peerEdgeStart+(i*2)+1)
            }
        }
    }
    function putInVert(relIdx, val) {
        if (Number.isNaN(val) || Number.isNaN(relIdx)) throw new Error(`putInVert NaN: relIdx=${relIdx} val=${val}`)
        traceInVert(debugScId, 'inVert:' + relIdx, val)
        if (!allocatedInVerticals && val > 255) allocInVerts() // would overflow the u8 slot -- bail to the map early
        if (allocatedInVerticals) {
            if (val === 0) {
                allocatedInVerticals.delete(relIdx)
            } else {
                allocatedInVerticals.set(relIdx, val)
            }
            return
        }
        for (let i = 0; i < 6; i++) {
            const k = v.getUint8(inVerticalsStart+(i*2))
            if (k === relIdx) {
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
                        const [id2, val2] = getInVert(j-1)
                        v.setUint8(inVerticalsStart+(j-1)*2, 0)
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
                v.setUint8(inVerticalsStart+(i*2), relIdx)
                v.setUint8(inVerticalsStart+(i*2)+1, val)
                return
            }
        }
        allocInVerts()
        allocatedInVerticals.set(relIdx, val)
        // throw new Error('out of room!')
    }
    function putOutVert(absId, val) {
        traceInVert(debugScId, 'outVert:' + absId, val)
        if (!allocatedOutVerticals && val > 255) allocOutVerts() // would overflow the u8 slot -- bail to the map early
        if (allocatedOutVerticals) {
            if (val === 0) {
                allocatedOutVerticals.delete(absId)
            } else {
                allocatedOutVerticals.set(absId, val)
            }
            return
        }
        const base = outVertStart
        const verticalSize = 5
        for (let i = 0; i < 6; i++) {
            const k = v.getUint32(base+(i*verticalSize), true)
            if (k === absId) {
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
                        const [id2, val2] = getOutVert(j-1)
                        v.setUint32(base+((j-1)*verticalSize), 0, true)
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
                v.setUint32(base+(i*verticalSize), absId, true)
                v.setUint8(base+(i*verticalSize)+4, val)
                return
            }
        }
        allocOutVerts()
        allocatedOutVerticals.set(absId, val)
        // throw new Error('out of room!')
    }
    function collectOutVerticals() {
        if (allocatedOutVerticals) {
            return [...allocatedOutVerticals.keys()]
        }
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
    function incOutVertical(relIdx) {
        if (allocatedOutVerticals) {
            const v = allocatedOutVerticals.get(relIdx)
            allocatedOutVerticals.set(relIdx, (v ?? 0) + 1)
            return !v
        }
        const base = outVertStart
        const verticalSize = 5
        for (let i = 0; i < 6; i++) {
            const k = v.getUint32(base+(i*verticalSize), true)
            if (k === relIdx) {
                const cur = v.getUint8(base+(i*verticalSize)+4)
                if (cur === 255) { // would overflow the u8 slot -- bail to the map early
                    allocOutVerts()
                    allocatedOutVerticals.set(relIdx, cur + 1)
                    return false
                }
                v.setUint8(base+(i*verticalSize)+4, cur + 1)
                return false
            }
            if (k === 0) {
                v.setUint32(base+(i*verticalSize), relIdx, true)
                v.setUint8(base+(i*verticalSize)+4, 1)
                return true
            }
        }
        allocOutVerts()
        allocatedOutVerticals.set(relIdx, 1)
        return true
        // throw new Error('out of room!')
    }
    // returns true when a vertical edge was removed
    function decOutVertical(relIdx) {
        if (allocatedOutVerticals) {
            const v = allocatedOutVerticals.get(relIdx)
            if (!v) throw new Error('womp ?')
            if (v === 1) {
                allocatedOutVerticals.delete(relIdx)
            } else {
                allocatedOutVerticals.set(relIdx, v - 1)
            }
            return v === 1
        }
        const base = outVertStart
        const verticalSize = 5
        for (let i = 0; i < 6; i++) {
            const k = v.getUint32(base+(i*verticalSize), true)
            if (k !== relIdx) continue
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
                    const [id2, val2] = getOutVert(j-1)
                    v.setUint32(base+((j-1)*verticalSize), 0, true)
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
    function putPeerEdge(relIdx, val) {
        traceInVert(debugScId, 'peerEdge:' + relIdx, val)
        if (!allocatedPeerEdges && val > 255) allocPeerEdges() // would overflow the u8 slot -- bail to the map early
        if (allocatedPeerEdges) {
            if (val === 0) {
                allocatedPeerEdges.delete(relIdx)
            } else {
                allocatedPeerEdges.set(relIdx, val)
            }
            return
        }
        const base = peerEdgeStart
        for (let i = 0; i < 6; i++) {
            const k = v.getUint8(base+(i*2))
            if (k === relIdx) {
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
                        const [id2, val2] = getPeerEdge(j-1)
                        v.setUint8(base+(j-1)*2, 0)
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
                v.setUint8(base+(i*2), relIdx)
                v.setUint8(base+(i*2)+1, val)
                return
            }
        }
        allocPeerEdges()
        allocatedPeerEdges.set(relIdx, val)
        // throw new Error('out of room!')
    }
    function collectPeerEdges() {
        if (allocatedPeerEdges) {
            return [...allocatedPeerEdges.entries()]
        }
        const base = peerEdgeStart
        const arr = []
        for (let i = 0; i < 6; i++) {
            const k = v.getUint8(base+(i*2))
            if (k === 0) break
            arr.push([k, v.getUint8(base+(i*2)+1)])
        }
        return arr
    }
    // we want to pack ref counts right beside the cells
    // the layout is ugly
    function getLocalRc(id) {
        if (allocatedLocalRc) return allocatedLocalRc[id-1] ?? 0
        return v.getUint8(heapCellStart+(id-1)*5)
    }
    function setLocalRc(id, val) {
        if (!allocatedLocalRc && val > 255) allocLocalRc() // would overflow the u8 slot -- bail to the array early
        if (allocatedLocalRc) {
            allocatedLocalRc[id-1] = val
            return
        }
        checkOneByteOverflow(val)
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
    const heapCellFreeList = []
    function pushHeapCell(val) {
        if (heapCellFreeList.length) {
            const id = heapCellFreeList.pop()
            setHeapCellId(id - 1, val)
            // DEBUG CHECKS
            if (getLocalRc(id) !== 0) throw new Error(`Allocated with non-zero RC: ${id} -> ${getLocalRc(id)}`)
            if (findInVert(id)) throw new Error('Allocated with non-zero in vertical')
            if (findOutVert(id)) throw new Error('Allocated with non-zero out vertical')
            return id
        }
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
            heapCellFreeList.push(relId)
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
            if (k === 0) continue // hole left by a freed-list removal, not the end
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
    const leafBit = 0x10

    function hasFlag(bit) {
        return (getFlags() & bit) === bit
    }
    function setFlagBit(bit, val) {
        const flags = getFlags()
        setFlags(val ? (flags | bit) : (flags & ~bit))
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
    // unused for now
    function isLeafOnly() {
        return hasFlag(leafBit)
    }
    function setLeafOnly(val) {
        setFlagBit(leafBit, val)
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
            if (allocatedInVerticals) {
                for (const [k, v] of allocatedInVerticals) {
                    console.log(`  ${k}: ${v}`)
                }
            } else {
                for (let i = 0; i < 6; i++) {
                    const c = getInVert(i)
                    if (c[0] === 0) break
                    console.log(`  ${c[0]}: ${c[1]}`)
                }
            }
        }
    }
    // scans in/out/peer tables for any (nonzero key, zero value) pair --
    // an entry that should have been fully compacted away but wasn't.
    // returns a list of problem descriptions, empty if clean.
    function checkIntegrity() {
        const problems = []
        if (allocatedInVerticals) {
            for (const [k, val] of allocatedInVerticals) {
                if (k !== 0 && val === 0) problems.push(`inVert(map) k=${k} val=0`)
            }
        } else {
            for (let i = 0; i < 6; i++) {
                const [k, val] = getInVert(i)
                if (k !== 0 && val === 0) problems.push(`inVert(inline) k=${k} val=0 slot=${i}`)
            }
        }
        if (allocatedOutVerticals) {
            for (const [k, val] of allocatedOutVerticals) {
                if (k !== 0 && val === 0) problems.push(`outVert(map) k=${k} val=0`)
            }
        } else {
            const base = outVertStart
            const verticalSize = 5
            for (let i = 0; i < 6; i++) {
                const k = v.getUint32(base+(i*verticalSize), true)
                const val = v.getUint8(base+(i*verticalSize)+4)
                if (k !== 0 && val === 0) problems.push(`outVert(inline) k=${k} val=0 slot=${i}`)
            }
        }
        if (allocatedPeerEdges) {
            for (const [k, val] of allocatedPeerEdges) {
                if (k !== 0 && val === 0) problems.push(`peerEdge(map) k=${k} val=0`)
            }
        } else {
            for (let i = 0; i < 6; i++) {
                const [k, val] = getPeerEdge(i)
                if (k !== 0 && val === 0) problems.push(`peerEdge(inline) k=${k} val=0 slot=${i}`)
            }
        }
        return problems
    }
    return {
        checkIntegrity,
        get depth() {
            return getDepth()
        },
        getParentRef,
        setParentRef,
        getCellCount,
        setCellCount,
        getDepth,
        setDepth,
        getDepthExtent,
        setDepthExtent,
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
        collectPeerEdges,
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
        isLeafOnly,
        setLeafOnly,
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
    const view = createScView(page, 0, id)
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
    const arr = [toSc, to]
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
    if (!invc) throw new Error('??? no: ' + relId + ' : ' + fromSc.printInfo() + ' : ' + toHandle)
    fromSc.putInVert(relId, invc - 1)
    if (invc === 1) {
        if (fromSc.getLocalRc(relId) === 0) {
            free(toHandle)
        }
    }
}

const freedHandles = new Set() // test instrumentation

function freeCutawaySc(scid, sc = getSc(scid)) {
    if (sc.isFreed()) return
    sc.setFreed(true)
    sc.setCutaway(true)
    freeScCells(sc)
    const parentScid = sc.parent
    const parent = getSc(parentScid)
    const idx = sc.getSelfLocalId()
    parent.removeHeapCell(idx)
    freedHandles.add(sc.id | (1 << 30))
    // console.log('freed SC', scid)
}

function free(handle) {
    if (isSc(handle)) {
        const scid = handle & ~(1 << 30)
        const sc = getSc(scid)
        if (sc.isEternal()) return
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
        freedHandles.add(handle)
        // console.log('freed', 'rel id', idx, 'scid', scid)
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
    const arr = [toSc, to]
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
    if (parent.decLocalRc(toId) === 0) {
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
            removeScEdge(sc.id, scid2, leafHandle)
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
        for (let i = 0; i < v; i++) {
            removeEdge(handle, k)
        }
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

// Groups `psc`'s SC-children (all children of `psc` are SCs, never leaves)
// by "depth extent" -- a generation counter that starts at 0 and is bumped
// by +1 each time a batch gets wrapped into a fresh parent by this pass.
// We pick the most populous extent (ties -> lowest extent), run Tarjan's
// SCC over just that bucket (edges = peer edges between bucket members),
// and collapse it: every real cycle (SCC with >1 member) becomes its own
// new wrapper SC one extent higher; everything left over (singleton SCCs,
// i.e. not part of any cycle) is bundled into exactly one more wrapper SC
// -- they are NOT treated as individual SCCs, per spec.
function condense(psc) {
    const buckets = new Map() // depthExtent -> [sc, ...]
    for (const x of psc.collectHeapCells()) {
        if (!isSc(x[1])) throw new Error('leaves? hmm')
        const child = getSc(x[1] & ~(1 << 30))
        const arr = buckets.get(child.getDepthExtent()) ?? []
        arr.push(child)
        buckets.set(child.getDepthExtent(), arr)
    }

    let bestDepth = null
    let bestList = null
    for (const [depth, list] of buckets) {
        if (bestList === null || list.length > bestList.length || (list.length === bestList.length && depth < bestDepth)) {
            bestDepth = depth
            bestList = list
        }
    }
    if (!bestList || bestList.length < 2) return // nothing worth condensing

    const bucketIds = new Set(bestList.map((s) => s.id))

    // --- Tarjan's SCC over bestList, edges = peer edges staying within the bucket
    let index = 0
    const stack = []
    const indices = new Map()
    const lowlinks = new Map()
    const onStack = new Set()
    const sccs = []

    function neighborsOf(sc) {
        const out = []
        for (const [toId, val] of sc.collectPeerEdges()) {
            if (!val) continue
            const stored = psc.getHeapCellId(toId - 1) // relIds are 1-based
            if (!stored || !isSc(stored)) continue
            const targetId = stored & ~(1 << 30)
            if (bucketIds.has(targetId)) out.push(getSc(targetId))
        }
        return out
    }

    function strongConnect(v) {
        indices.set(v.id, index)
        lowlinks.set(v.id, index)
        index++
        stack.push(v)
        onStack.add(v.id)
        for (const w of neighborsOf(v)) {
            if (!indices.has(w.id)) {
                strongConnect(w)
                lowlinks.set(v.id, Math.min(lowlinks.get(v.id), lowlinks.get(w.id)))
            } else if (onStack.has(w.id)) {
                lowlinks.set(v.id, Math.min(lowlinks.get(v.id), indices.get(w.id)))
            }
        }
        if (lowlinks.get(v.id) === indices.get(v.id)) {
            const scc = []
            let w
            do {
                w = stack.pop()
                onStack.delete(w.id)
                scc.push(w)
            } while (w !== v)
            sccs.push(scc)
        }
    }

    for (const sc of bestList) {
        if (!indices.has(sc.id)) strongConnect(sc)
    }

    const groups = sccs.filter((s) => s.length > 1)
    const remainder = sccs.filter((s) => s.length === 1).flat()
    if (remainder.length) groups.push(remainder)

    const newWrappers = []
    for (const group of groups) {
        newWrappers.push(mergeGroup(psc, group, bestDepth + 1))
    }

    // condensation can *reveal* garbage: a real cycle (or a bundle of
    // otherwise-unreferenced remainder singletons) that turns out to have
    // zero external support once merged. Only check this once *all*
    // merges for this condense() are done -- checking mid-loop lets an
    // early free mutate psc's children out from under later mergeGroup
    // calls that are still iterating them.
    for (const newSc of newWrappers) {
        const relId = newSc.getSelfLocalId()
        if (psc.getLocalRc(relId) === 0 && !psc.findInVert(relId)) {
            free(newSc.id | (1 << 30))
        }
    }
}

// Wraps `group` (a set of `psc`'s direct SC-children) into one new SC,
// itself a fresh direct child of `psc` at `newDepthExtent`. Every bit of
// support that used to point at a group member -- psc's own localRc/inVert
// for it, and each *other* psc-sibling's individual peer edge into it --
// gets consolidated onto the new wrapper first. Only once that's in place
// do we detach the group members from psc and drop their old entries, so
// there's never a window where something looks unsupported and gets
// incorrectly collected mid-move. Peer edges *between* group members are
// rekeyed (not dropped) since both endpoints now share the wrapper as
// their immediate parent, which is exactly what a peer edge encodes.
function mergeGroup(psc, group, newDepthExtent) {
    const newSc = createSc(psc)
    newSc.setDepthExtent(newDepthExtent)

    const oldRelId = new Map(group.map((s) => [s.id, s.getSelfLocalId()]))
    const groupIds = new Set(group.map((s) => s.id))

    // 1) consolidate psc-level support (localRc + inVert) onto newSc
    let totalRc = 0
    let totalInVert = 0
    const perChildInVert = new Map() // old.id -> its own individual psc-level inVert value
    for (const old of group) {
        const v = psc.findInVert(old.getSelfLocalId()) ?? 0
        if (v) perChildInVert.set(old.id, v)
        totalInVert += v
    }
    if (totalInVert > 0) {
        psc.putInVert(newSc.getSelfLocalId(), totalInVert)
    }

    // 2) redirect every other psc-sibling's individual peer edge(s) into
    // the group so they point at newSc instead
    for (const x of psc.collectHeapCells()) {
        if (!isSc(x[1])) continue
        const sibId = x[1] & ~(1 << 30)
        if (sibId === newSc.id || groupIds.has(sibId)) continue
        const sib = getSc(sibId)
        let redirected = 0
        for (const old of group) {
            const v = sib.findPeerEdge(old.getSelfLocalId())
            if (v) redirected += v
        }
        if (redirected > 0) {
            const existing = sib.findPeerEdge(newSc.getSelfLocalId()) ?? 0
            sib.putPeerEdge(newSc.getSelfLocalId(), existing + redirected)
            totalRc += redirected;
        }
    }
    if (totalRc > 0) {
        psc.setLocalRc(newSc.getSelfLocalId(), totalRc)
    }
    // 2.5) redirect each group member's OWN outgoing peer edges to psc
    // siblings OUTSIDE the group -- old's parent is about to change, so a
    // peer edge keyed by "relId within psc" would go stale otherwise
    // (internal group<->group peer edges are handled separately in step 4)
    for (const old of group) {
        for (const [relId, val] of old.collectPeerEdges()) {
            if (!val) continue
            const stored = psc.getHeapCellId(relId - 1)
            if (!stored) continue
            const targetId = stored & ~(1 << 30)
            if (groupIds.has(targetId)) continue
            old.putPeerEdge(relId, 0)
            const target = getSc(targetId)
            const existing = newSc.findPeerEdge(target.getSelfLocalId()) ?? 0
            newSc.putPeerEdge(target.getSelfLocalId(), existing + val)
        }
    }

    // 2.6) mirror every one of a group member's OWN out-vertical entries
    // onto newSc too. clearScEdges() discovers what to tear down purely by
    // walking an SC's own out-vertical table (not its peer-edge table), so
    // even the "simple" entries backing a peer edge that just moved to
    // newSc in step 2.5 need a matching raw entry here, or a future bulk
    // free of newSc would never find them.
    for (const old of group) {
        for (const targetHandle of old.collectOutVerticals()) {
            const val = old.findOutVert(targetHandle)
            if (!val) continue
            for (let i = 0; i < val; i++) newSc.incOutVertical(targetHandle)
            newSc.incOutVertical(targetHandle)
        }
    }

    // 3) detach group members from psc and reattach under newSc
    for (const old of group) {
        psc.setLocalRc(old.getSelfLocalId(), 0) // clean up before the relId is free-listed
        if (psc.findInVert(old.getSelfLocalId())) psc.putInVert(old.getSelfLocalId(), 0)
        psc.removeHeapCell(old.getSelfLocalId())
        const individualInVert = perChildInVert.get(old.id)
        old.parent = newSc.id
        old.setSelfLocalId(newSc.pushHeapCell(old.id | (1 << 30)))
        bumpDepth(old, newSc.depth + 1 - old.depth)
        // 3.5) mirror this child's own (individual, not aggregated) psc-level
        // in-vertical support onto newSc's own in-vertical table for it --
        // a future removal, recomputed against the post-merge tree, walks
        // down INTO newSc looking for exactly this per-child entry
        if (individualInVert) newSc.putInVert(old.getSelfLocalId(), individualInVert)
    }

    // 4) rekey peer edges between group members: same relationship, just
    // scoped one level deeper now that both endpoints share newSc as parent
    for (const a of group) {
        for (const b of group) {
            if (a === b) continue
            const staleRelId = oldRelId.get(b.id)
            const v = a.findPeerEdge(staleRelId)
            if (!v) continue
            a.putPeerEdge(staleRelId, 0)
            const freshRelId = b.getSelfLocalId()
            const existing = a.findPeerEdge(freshRelId) ?? 0
            a.putPeerEdge(freshRelId, existing + v)
            newSc.setLocalRc(freshRelId, newSc.getLocalRc(freshRelId) + v)
        }
    }

    // 5) only now remove the old (now-stale) sibling -> group peer edges --
    // everything they were protecting has an equal-or-greater replacement
    // on newSc already in place from step 2
    for (const x of psc.collectHeapCells()) {
        if (!isSc(x[1])) continue
        const sibId = x[1] & ~(1 << 30)
        if (sibId === newSc.id) continue
        const sib = getSc(sibId)
        for (const old of group) {
            const staleRelId = oldRelId.get(old.id)
            if (sib.findPeerEdge(staleRelId)) sib.putPeerEdge(staleRelId, 0)
        }
    }

    return newSc
}

// keeps the depth invariant (child.depth === parent.depth + 1) intact for
// an SC's whole subtree after it gets moved to sit under a new parent
function bumpDepth(sc, delta) {
    if (!delta) return
    sc.setDepth(sc.depth + delta)
    for (const x of sc.collectHeapCells()) {
        if (isSc(x[1])) bumpDepth(getSc(x[1] & ~(1 << 30)), delta)
    }
}

// balanced allocation
// this assumes `sc` isn't the root
function allocInto(sc) {
    const cur = sc
    if (cur.activeSc?.isFreed()) cur.activeSc = undefined
    if (!cur.activeSc) {
        cur.activeSc = createSc(cur)
    }
    sc = cur.activeSc ?? cur // XXX: close enough

    const count = sc.getCellCount()
    if (count < 50) {
        // if (!sc.isLeafOnly()) throw new Error('but why?')
        const fakeAddr = sc.pushHeapCell(0xFF00 + sc.getCellCount())
        return toAbsoluteHandle(sc.id, fakeAddr)
    }

    const psc = getSc(sc.parent)

    const count2 = psc.getCellCount()
    if (count2 < 50) {
        cur.activeSc = createSc(cur)
        return createTestCell(cur.activeSc)
    }
    condense(psc)
    cur.activeSc = createSc(psc)
    return createTestCell(cur.activeSc)
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

const shouldFuzz = false

// --- fuzz harness: random add/remove edges, cross-checked against a shadow
// adjacency graph (edgeMap) for reachability from a pinned root. Any handle
// reachable from the root must never appear in freedHandles -- that's the
// one hard soundness invariant (freeing something still referenced). We do
// NOT flag the inverse (something unreachable that never gets freed) as a
// bug on its own, since same-SC cycles are expected to leak per the design.
//
// The anchor cell lives in its own dedicated SC, isolated from the churn
// pool -- nothing else in the churn pool ever shares an SC with it, so it
// can never get swept as a side effect of a *sibling* SC's peer-edge
// cascade (which is legitimate per-design and not something to pin
// against, since `isEternal`/`setEternal` exists as API surface but isn't
// consulted by `free()` yet -- an unwired stub, not a bug).
function fuzzTest(seed, steps, numLeafScs = 3, cellsPerLeaf = 2) {
    if (!shouldFuzz) return
    let s = seed >>> 0
    function rnd() {
        s = (s * 1664525 + 1013904223) >>> 0
        return s / 0xFFFFFFFF
    }
    function rndInt(n) { return Math.floor(rnd() * n) }

    const mid = createSc(root)
    const leafScs = []
    const cells = []
    for (let i = 0; i < numLeafScs; i++) {
        const leafSc = createSc(mid)
        leafScs.push(leafSc)
        for (let j = 0; j < cellsPerLeaf; j++) {
            cells.push(createTestCell(leafSc))
        }
    }

    const anchorSc = createSc(mid)
    const rootCell = createTestCell(anchorSc)
    const roots = new Set([rootCell])

    // an SC that's simply never been referenced yet ("hasn't been found",
    // per design) isn't a leak -- only an SC that HAD vertical support and
    // then lost it entirely, while surviving, is
    const everHadSupport = new Set()

    const shadow = new Map()
    function shadowAdd(from, to) {
        if (!shadow.has(from)) shadow.set(from, new Set())
        shadow.get(from).add(to)
    }

    function reachableFromRoots() {
        const seen = new Set()
        const stack = [...roots]
        while (stack.length) {
            const h = stack.pop()
            if (seen.has(h)) continue
            seen.add(h)
            for (const to of shadow.get(h) ?? []) stack.push(to)
        }
        return seen
    }

    // pin one churn cell alive via a permanent edge from the isolated anchor,
    // so reachability actually propagates into the churn pool
    addTestEdge(rootCell, cells[0])
    shadowAdd(rootCell, cells[0])

    const pairs = []
    for (let i = 0; i < cells.length; i++) {
        for (let j = 0; j < cells.length; j++) {
            if (i !== j) pairs.push([cells[i], cells[j]])
        }
    }

    let violations = 0
    let capacitySkips = 0
    const opLog = []
    for (let step = 0; step < steps; step++) {
        const [from, to] = pairs[rndInt(pairs.length)]
        if (freedHandles.has(from) || freedHandles.has(to)) continue

        const m = edgeMap.get(from)
        const existingCount = m?.get(to) ?? 0
        const isAdd = existingCount === 0 || rnd() < 0.55
        opLog.push([isAdd ? 'add' : 'remove', from, to])
        try {
            if (isAdd) {
                addTestEdge(from, to)
                shadowAdd(from, to)
            } else {
                removeTestEdge(from, to)
                const m2 = edgeMap.get(from)
                if (!m2 || !m2.has(to)) {
                    shadow.get(from)?.delete(to)
                }
            }
        } catch (e) {
            if (e.message === 'out of room!') { capacitySkips++; continue } // known fixed-size-region stub, not an algo bug
            console.log('FAILED at step', step, 'op', opLog[opLog.length-1])
            console.log('full op log:', JSON.stringify(opLog))
            throw e
        }

        const live = reachableFromRoots()
        for (const h of live) {
            if (freedHandles.has(h)) {
                violations++
                console.log('BUG: reachable handle was freed!', h, 'at step', step, 'op:', isAdd ? 'add' : 'remove', from, '->', to)
            }
        }

        // an SC that HAD vertical support (something in its parent's
        // localRc, or an in-vertical from further up) and then lost it
        // entirely, while surviving unfreed, is a real leak -- distinct
        // from an SC that simply hasn't been referenced yet ("hasn't been
        // found", per design), and distinct from a cycle among cells
        // *inside* an otherwise-still-supported SC.
        for (const leafSc of leafScs) {
            if (leafSc.isFreed()) continue
            const relId = leafSc.getSelfLocalId()
            const hasSupport = mid.getLocalRc(relId) > 0 || !!mid.findInVert(relId)
            if (hasSupport) {
                everHadSupport.add(leafSc.id)
            } else if (everHadSupport.has(leafSc.id)) {
                violations++
                console.log('LEAK BUG: SC', leafSc.id, 'lost all vertical support but was not freed, at step', step, 'op:', isAdd ? 'add' : 'remove', from, '->', to)
            }
        }
    }
    console.log(`fuzz done: ${steps} steps, ${violations} soundness violations, ${capacitySkips} capacity skips, ${freedHandles.size} handles freed total`)
}

fuzzTest(12345, 5000)
fuzzTest(999, 5000)
fuzzTest(777777, 5000)
fuzzTest(2468, 5000)
fuzzTest(13579, 5000)

fuzzTest(1111, 8000, 8, 8)
fuzzTest(2222, 8000, 8, 8)
fuzzTest(3333, 8000, 8, 8)
fuzzTest(4444, 8000, 8, 8)
fuzzTest(5555, 8000, 8, 8)

// all 64 cells as siblings in a single SC (pure local-rc/in-vertical churn,
// no cross-SC peer/out-vertical mechanics at all -- but with rootCell's own
// isolated anchor SC still in the mix, so mid still has 2 children total)
fuzzTest(6001, 8000, 1, 64)
fuzzTest(6002, 8000, 1, 64)
fuzzTest(6003, 8000, 1, 64)

// two full 64-cell SCs as siblings -- forces heavy cross-SC out-vertical/
// in-vertical/peer-edge overflow between two maxed-out SCs
fuzzTest(7001, 8000, 2, 64)
fuzzTest(7002, 8000, 2, 64)
fuzzTest(7003, 8000, 2, 64)

// --- heap cell churn: creates and directly frees cells within an SC over
// time (not just toggling edges among a fixed pool), stressing the new
// free-list reuse path in pushHeapCell/removeHeapCell -- specifically
// whether a relId handed back out after being freed starts from clean
// state (localRc, inVert) and whether collectHeapCells still enumerates
// everything correctly once holes exist.
function fuzzTestCellChurn(seed, steps, numLeafScs = 4, initialCellsPerLeaf = 4) {
    if (!shouldFuzz) return
    let s = seed >>> 0
    function rnd() {
        s = (s * 1664525 + 1013904223) >>> 0
        return s / 0xFFFFFFFF
    }
    function rndInt(n) { return Math.floor(rnd() * n) }

    const mid = createSc(root)
    const leafScs = []
    for (let i = 0; i < numLeafScs; i++) leafScs.push(createSc(mid))

    const anchorSc = createSc(mid)
    const rootCell = createTestCell(anchorSc)
    const roots = new Set([rootCell])

    let cells = []
    for (let i = 0; i < numLeafScs; i++) {
        for (let j = 0; j < initialCellsPerLeaf; j++) {
            cells.push(createTestCell(leafScs[i]))
        }
    }

    const shadow = new Map()
    function shadowAdd(from, to) {
        if (!shadow.has(from)) shadow.set(from, new Set())
        shadow.get(from).add(to)
    }
    function reachableFromRoots() {
        const seen = new Set()
        const stack = [...roots]
        while (stack.length) {
            const h = stack.pop()
            if (seen.has(h)) continue
            seen.add(h)
            for (const to of shadow.get(h) ?? []) stack.push(to)
        }
        return seen
    }
    function hasAnyInboundEdge(h) {
        for (const m of edgeMap.values()) {
            if (m.get(h) > 0) return true
        }
        return false
    }

    addTestEdge(rootCell, cells[0])
    shadowAdd(rootCell, cells[0])

    let violations = 0
    let created = 0
    let directlyFreed = 0
    const opLog = []
    for (let step = 0; step < steps; step++) {
        const roll = rnd()
        try {
            if (roll < 0.15 && cells.length < numLeafScs * 40) {
                const liveLeafScs = leafScs.filter((s) => !s.isFreed())
                if (liveLeafScs.length) {
                    const sc = liveLeafScs[rndInt(liveLeafScs.length)]
                    const h = createTestCell(sc)
                    cells.push(h)
                    created++
                    opLog.push(['create', leafScs.indexOf(sc)])
                }
                continue
            }
            if (roll < 0.25) {
                const candidates = cells.filter((h) => h !== cells[0] && !freedHandles.has(h) && !hasAnyInboundEdge(h))
                if (candidates.length) {
                    const h = candidates[rndInt(candidates.length)]
                    opLog.push(['free', cells.indexOf(h)])
                    free(h)
                    directlyFreed++
                    if (freedHandles.has(h) && reachableFromRoots().has(h)) {
                        violations++
                        console.log('BUG: directly freed a still-reachable cell!', h, 'at step', step)
                    }
                }
                continue
            }

            const live = cells.filter((h) => !freedHandles.has(h))
            if (live.length < 2) continue
            const from = live[rndInt(live.length)]
            let to = live[rndInt(live.length)]
            if (to === from) continue

            const m = edgeMap.get(from)
            const existingCount = m?.get(to) ?? 0
            const isAdd = existingCount === 0 || rnd() < 0.55
            opLog.push([isAdd ? 'add' : 'remove', cells.indexOf(from), cells.indexOf(to)])
            if (isAdd) {
                addTestEdge(from, to)
                shadowAdd(from, to)
            } else {
                removeTestEdge(from, to)
                const m2 = edgeMap.get(from)
                if (!m2 || !m2.has(to)) shadow.get(from)?.delete(to)
            }

            const liveSet = reachableFromRoots()
            for (const h of liveSet) {
                if (freedHandles.has(h)) {
                    violations++
                    console.log('BUG: reachable handle was freed!', h, 'at step', step)
                }
            }
        } catch (e) {
            console.log('FAILED at step', step, ':', e.message)
            console.log('full op log:', JSON.stringify(opLog))
            throw e
        }
    }
    console.log(`cell churn fuzz done: ${steps} steps, ${created} created, ${directlyFreed} directly freed, ${violations} violations, ${cells.length} total cells ever created`)
}

fuzzTestCellChurn(8001, 8000)
fuzzTestCellChurn(8002, 8000)
fuzzTestCellChurn(8003, 8000)
fuzzTestCellChurn(8004, 8000, 8, 8)
fuzzTestCellChurn(8005, 8000, 8, 8)

// --- condense() scenario test: A<->B<->C form a cycle, D and E don't
// participate in any cycle, X is an outside sibling with a peer edge into
// the cycle (A specifically). After condense(psc): A,B,C should be
// wrapped into one new SC (a real SCC), D,E into another (remainder, not
// treated as their own SCCs), and X's peer edge should be redirected to
// point at the cycle's new wrapper instead of A directly.
function testCondense() {
    const psc = createSc(root)
    const A = createSc(psc)
    const B = createSc(psc)
    const C = createSc(psc)
    const D = createSc(psc)
    const E = createSc(psc)
    const X = createSc(psc)

    const aCell = createTestCell(A)
    const bCell = createTestCell(B)
    const cCell = createTestCell(C)
    createTestCell(D)
    createTestCell(E)
    const xCell = createTestCell(X)
    X.setDepthExtent(5) // a different (already-condensed) generation -- not part of this bucket

    addTestEdge(aCell, bCell)
    addTestEdge(bCell, cCell)
    addTestEdge(cCell, aCell)
    addTestEdge(xCell, aCell)

    console.log('--- condense test: before ---')
    console.log('psc children:', psc.collectHeapCells().map((x) => x[1] & ~(1 << 30)))
    console.log('X peer edges before:', X.collectPeerEdges())

    condense(psc)

    console.log('--- condense test: after ---')
    const after = psc.collectHeapCells().map((x) => getSc(x[1] & ~(1 << 30)))
    for (const child of after) {
        console.log(' child', child.id, 'depthExtent', child.getDepthExtent(), 'members', child.collectHeapCells().map((c) => c[1] & ~(1 << 30)))
    }
    console.log('X peer edges after:', X.collectPeerEdges())

    const cycleWrapper = after.find((s) => s.collectHeapCells().some((c) => (c[1] & ~(1 << 30)) === A.id))
    const remainderWrapper = after.find((s) => s.collectHeapCells().some((c) => (c[1] & ~(1 << 30)) === D.id))
    console.log('sanity: psc child count === 3 (X + 2 wrappers)?', after.length === 3)
    console.log('sanity: cycle wrapper has exactly A,B,C?', cycleWrapper && cycleWrapper.getCellCount() === 3)
    console.log('sanity: remainder wrapper has exactly D,E?', remainderWrapper && remainderWrapper.getCellCount() === 2)
    console.log('sanity: X now points at cycle wrapper?', X.findPeerEdge(cycleWrapper.getSelfLocalId()) === 1)
    console.log('sanity: A no longer directly a psc child?', !after.includes(A))
}
testCondense()

// --- condense() fuzzing: build a psc with many child SCs holding cells,
// churn random leaf-level edges among those cells (organically creating
// real SC-level peer edges and, sometimes, real cycles), run condense one
// or more times interleaved with more churn, and check the one hard
// invariant: condense is a pure reorganization, never a collection pass --
// anything reachable before a condense() call must still be reachable
// (never end up in freedHandles) after it, and that must keep holding
// under further churn against the now-deeper structure.
function fuzzTestCondense(seed, numChildSCs, cellsPerChild, rounds, stepsPerRound) {
    //if (!shouldFuzz) return
    let s = seed >>> 0
    function rnd() {
        s = (s * 1664525 + 1013904223) >>> 0
        return s / 0xFFFFFFFF
    }
    function rndInt(n) { return Math.floor(rnd() * n) }

    const psc = createSc(root)
    const children = []
    for (let i = 0; i < numChildSCs; i++) children.push(createSc(psc))

    const cells = []
    for (let i = 0; i < numChildSCs; i++) {
        for (let j = 0; j < cellsPerChild; j++) cells.push(createTestCell(children[i]))
    }

    // anchor lives under `root` directly, siblings with `psc` itself -- never
    // touched by condense(psc), which only ever reorganizes psc's own children
    const anchorSc = createSc(root)
    const rootCell = createTestCell(anchorSc)
    const roots = new Set([rootCell])

    const shadow = new Map()
    function shadowAdd(f, t) {
        if (!shadow.has(f)) shadow.set(f, new Set())
        shadow.get(f).add(t)
    }
    function reachable() {
        const seen = new Set()
        const stack = [...roots]
        while (stack.length) {
            const h = stack.pop()
            if (seen.has(h)) continue
            seen.add(h)
            for (const t of shadow.get(h) ?? []) stack.push(t)
        }
        return seen
    }

    addTestEdge(rootCell, cells[0])
    shadowAdd(rootCell, cells[0])

    function churnStep() {
        const live = cells.filter((h) => !freedHandles.has(h))
        if (live.length < 2) return
        const from = live[rndInt(live.length)]
        const to = live[rndInt(live.length)]
        if (to === from) return
        const m = edgeMap.get(from)
        const existing = m?.get(to) ?? 0
        const isAdd = existing === 0 || rnd() < 0.6
        if (isAdd) {
            addTestEdge(from, to)
            shadowAdd(from, to)
        } else {
            removeTestEdge(from, to)
            const m2 = edgeMap.get(from)
            if (!m2 || !m2.has(to)) shadow.get(from)?.delete(to)
        }
    }

    function checkSoundness(label) {
        let bad = 0
        for (const h of reachable()) {
            if (freedHandles.has(h)) {
                bad++
                console.log('BUG:', label, 'freed a still-reachable cell!', h, 'seed', seed)
            }
        }
        return bad
    }

    let violations = 0
    let condenseErrors = 0
    for (let round = 0; round < rounds; round++) {
        for (let i = 0; i < stepsPerRound; i++) churnStep()
        violations += checkSoundness('pre-condense churn')

        try {
            condense(psc)
        } catch (e) {
            condenseErrors++
            console.log('CONDENSE CRASHED:', e.message, 'round', round, 'seed', seed)
            break
        }
        violations += checkSoundness('condense')
    }
    console.log(`condense fuzz seed=${seed} n=${numChildSCs}x${cellsPerChild} done: ${violations} violations, ${condenseErrors} condense errors, ${freedHandles.size} freed total`)
}

// --- minimal-repro harness for fuzzTestCondense failures: records every
// op (churn + condense-round markers) against a fixed-size cell pool
// (indices stable, cells array never grows), replays them against a fresh
// but structurally identical setup, and delta-debugs down to the smallest
// failing sequence.
function replayCondenseOps(ops, numChildSCs, cellsPerChild, quiet) {
    const psc = createSc(root)
    const children = []
    for (let i = 0; i < numChildSCs; i++) children.push(createSc(psc))
    const cells = []
    for (let i = 0; i < numChildSCs; i++) {
        for (let j = 0; j < cellsPerChild; j++) cells.push(createTestCell(children[i]))
    }
    const anchorSc = createSc(root)
    const rootCell = createTestCell(anchorSc)
    const roots = new Set([rootCell])
    const shadow = new Map()
    function shadowAdd(f, t) {
        if (!shadow.has(f)) shadow.set(f, new Set())
        shadow.get(f).add(t)
    }
    function reachable() {
        const seen = new Set()
        const stack = [...roots]
        while (stack.length) {
            const h = stack.pop()
            if (seen.has(h)) continue
            seen.add(h)
            for (const t of shadow.get(h) ?? []) stack.push(t)
        }
        return seen
    }
    addTestEdge(rootCell, cells[0])
    shadowAdd(rootCell, cells[0])

    const origLog = console.log
    if (quiet) console.log = () => {}
    try {
        for (const op of ops) {
            if (op[0] === 'add') {
                addTestEdge(cells[op[1]], cells[op[2]])
                shadowAdd(cells[op[1]], cells[op[2]])
            } else if (op[0] === 'remove') {
                removeTestEdge(cells[op[1]], cells[op[2]])
                const m2 = edgeMap.get(cells[op[1]])
                if (!m2 || !m2.has(cells[op[2]])) shadow.get(cells[op[1]])?.delete(cells[op[2]])
            } else if (op[0] === 'condense') {
                condense(psc)
            }
            for (const h of reachable()) {
                if (freedHandles.has(h)) return 'SOUNDNESS: ' + h
            }
        }
        return null
    } catch (e) {
        return e.message
    } finally {
        if (quiet) console.log = origLog
    }
}

function isValidCondenseSequence(ops) {
    const counts = new Map()
    for (const op of ops) {
        if (op[0] === 'condense') continue
        const key = op[1] + ':' + op[2]
        const c = counts.get(key) ?? 0
        if (op[0] === 'add') counts.set(key, c + 1)
        else {
            if (c <= 0) return false
            counts.set(key, c - 1)
        }
    }
    return true
}

function ddminCondense(ops, numChildSCs, cellsPerChild, matchesErr) {
    let current = ops.slice()
    let changed = true
    while (changed) {
        changed = false
        for (let i = 0; i < current.length; i++) {
            const candidate = current.slice(0, i).concat(current.slice(i + 1))
            if (!isValidCondenseSequence(candidate)) continue
            if (matchesErr(replayCondenseOps(candidate, numChildSCs, cellsPerChild, true))) {
                current = candidate
                changed = true
                break
            }
        }
    }
    return current
}

function findCondenseMinimalRepro(seed, numChildSCs, cellsPerChild, rounds, stepsPerRound) {
    let s = seed >>> 0
    function rnd() {
        s = (s * 1664525 + 1013904223) >>> 0
        return s / 0xFFFFFFFF
    }
    function rndInt(n) { return Math.floor(rnd() * n) }

    const psc = createSc(root)
    const children = []
    for (let i = 0; i < numChildSCs; i++) children.push(createSc(psc))
    const cells = []
    const cellIndex = new Map()
    for (let i = 0; i < numChildSCs; i++) {
        for (let j = 0; j < cellsPerChild; j++) {
            const h = createTestCell(children[i])
            cellIndex.set(h, cells.length)
            cells.push(h)
        }
    }
    const anchorSc = createSc(root)
    const rootCell = createTestCell(anchorSc)
    const roots = new Set([rootCell])
    const shadow = new Map()
    function shadowAdd(f, t) {
        if (!shadow.has(f)) shadow.set(f, new Set())
        shadow.get(f).add(t)
    }
    function reachable() {
        const seen = new Set()
        const stack = [...roots]
        while (stack.length) {
            const h = stack.pop()
            if (seen.has(h)) continue
            seen.add(h)
            for (const t of shadow.get(h) ?? []) stack.push(t)
        }
        return seen
    }
    addTestEdge(rootCell, cells[0])
    shadowAdd(rootCell, cells[0])

    const opLog = []
    let foundErr = null
    outer: for (let round = 0; round < rounds; round++) {
        for (let i = 0; i < stepsPerRound; i++) {
            const live = cells.filter((h) => !freedHandles.has(h))
            if (live.length < 2) continue
            const fromH = live[rndInt(live.length)]
            const toH = live[rndInt(live.length)]
            if (toH === fromH) continue
            const from = cellIndex.get(fromH)
            const to = cellIndex.get(toH)
            const m = edgeMap.get(cells[from])
            const existing = m?.get(cells[to]) ?? 0
            const isAdd = existing === 0 || rnd() < 0.6
            opLog.push([isAdd ? 'add' : 'remove', from, to])
            if (isAdd) {
                addTestEdge(cells[from], cells[to])
                shadowAdd(cells[from], cells[to])
            } else {
                removeTestEdge(cells[from], cells[to])
                const m2 = edgeMap.get(cells[from])
                if (!m2 || !m2.has(cells[to])) shadow.get(cells[from])?.delete(cells[to])
            }
            for (const h of reachable()) {
                if (freedHandles.has(h)) {
                    foundErr = 'SOUNDNESS: ' + h
                    break outer
                }
            }
        }
        opLog.push(['condense'])
        try {
            condense(psc)
        } catch (e) {
            foundErr = e.message
            break outer
        }
        for (const h of reachable()) {
            if (freedHandles.has(h)) {
                foundErr = 'SOUNDNESS: ' + h
                break outer
            }
        }
    }

    console.log('found error:', foundErr, 'after', opLog.length, 'ops')
    if (!foundErr) return
    const errPrefix = foundErr.startsWith('SOUNDNESS') ? 'SOUNDNESS' : foundErr
    const minimal = ddminCondense(opLog, numChildSCs, cellsPerChild, (e) => e && (e.startsWith(errPrefix) || (errPrefix === 'SOUNDNESS' && e.startsWith('SOUNDNESS'))))
    console.log('minimal repro (', minimal.length, 'ops ):', JSON.stringify(minimal))
    console.log('minimal replay error:', replayCondenseOps(minimal, numChildSCs, cellsPerChild, true))
}

fuzzTestCondense(9001, 12, 4, 4, 300)
fuzzTestCondense(9002, 12, 4, 4, 300)
fuzzTestCondense(9003, 20, 3, 5, 400)
fuzzTestCondense(9004, 20, 3, 5, 400)
fuzzTestCondense(9005, 8, 8, 6, 500)
fuzzTestCondense(9006, 16, 16, 10, 1000)
fuzzTestCondense(9007, 32, 32, 20, 2000)

// findCondenseMinimalRepro(9007, 32, 32, 20, 2000)

// --- chaos/soak test: continuously allocInto() a single logical SC
// (self-balancing via activeSc + condense as it grows), while applying
// random mutations to whatever cells we currently believe are alive,
// proportional to how many of those there are. Not checking a specific
// invariant here -- just observing, over wall-clock time, whether/why
// memory (page count is the real cost: 4MB per SC ever created) keeps
// growing even as cells get collected.
function chaosTest(seed, durationMs, mutationRate = 0.02, verify = false) {
    // if (!shouldFuzz) return
    let s = seed >>> 0
    function rnd() {
        s = (s * 1664525 + 1013904223) >>> 0
        return s / 0xFFFFFFFF
    }
    function rndInt(n) { return Math.floor(rnd() * n) }

    // optional soundness verification (off by default -- adds real overhead:
    // a shadow adjacency graph plus a reachability walk after every
    // mutation). Anchored the same way as the other fuzzers: an isolated
    // SC, sibling to `top`, never itself subject to condense(top)'s
    // reorganization, pinned to the very first allocated cell.
    let roots, shadow, shadowAdd, reachable
    if (verify) {
        const anchorSc = createSc(root)
        const rootCell = createTestCell(anchorSc)
        roots = new Set([rootCell])
        shadow = new Map()
        shadowAdd = (f, t) => {
            if (!shadow.has(f)) shadow.set(f, new Set())
            shadow.get(f).add(t)
        }
        reachable = () => {
            const seen = new Set()
            const stack = [...roots]
            while (stack.length) {
                const h = stack.pop()
                if (seen.has(h)) continue
                seen.add(h)
                for (const t of shadow.get(h) ?? []) stack.push(t)
            }
            return seen
        }
        // pinned once the first real cell exists, below
    }
    let pinnedRoot = false
    let violations = 0
    function checkSoundness(label) {
        if (!verify) return
        for (const h of reachable()) {
            if (freedHandles.has(h)) {
                violations++
                console.log('BUG:', label, 'freed a still-reachable cell!', h)
            }
        }
    }

    function scanAllIntegrity(label) {
        for (let id = 0; id < scTable.length; id++) {
            const sc = scTable[id]
            if (!sc) continue
            const problems = sc.checkIntegrity()
            if (problems.length) {
                console.log(`INTEGRITY VIOLATION at ${label}: SC ${id} ->`, problems)
                const relevant = putInVertTrace.filter((t) => t.scId === id)
                console.log(`--- putInVert trace for SC ${id} (${relevant.length} calls) ---`)
                for (const t of relevant) {
                    console.log(`  relIdx=${t.relIdx} val=${t.val}\n    ${t.stack}`)
                }
                return true
            }
        }
        return false
    }

    const top = createSc(root)
    let alive = []

    const startTime = Date.now()
    let iterations = 0
    let allocCount = 0
    let mutationCount = 0
    const startPages = pages.length
    const startScCount = scTable.length

    while (Date.now() - startTime < durationMs) {
        iterations++

        const numAlloc = 1 + rndInt(3)
        for (let i = 0; i < numAlloc; i++) {
            const h = allocInto(top)
            alive.push(h)
            allocCount++
        }

        if (iterations % 200 === 0) {
            alive = alive.filter((h) => !freedHandles.has(h)) // just our own bookkeeping, not cg.js state
        }

        const liveNow = alive.filter((h) => !freedHandles.has(h))
        const numMutations = Math.max(1, Math.floor(liveNow.length * mutationRate))
        for (let i = 0; i < numMutations; i++) {
            if (liveNow.length < 2) break
            const from = liveNow[rndInt(liveNow.length)]
            const to = liveNow[rndInt(liveNow.length)]
            if (from === to) continue
            const m = edgeMap.get(from)
            const existing = m?.get(to) ?? 0
            const isAdd = existing === 0 || rnd() < 0.5
            if (isAdd) {
                addTestEdge(from, to)
                if (verify) shadowAdd(from, to)
            } else {
                removeTestEdge(from, to)
                if (verify) {
                    const m2 = edgeMap.get(from)
                    if (!m2 || !m2.has(to)) shadow.get(from)?.delete(to)
                }
            }
            mutationCount++
        }

        checkSoundness(`iter=${iterations}`)
        if (scanAllIntegrity(`iter=${iterations}`)) return

        if (iterations % 2000 === 0) {
            const elapsed = Date.now() - startTime
            console.log(
                `[t=${elapsed}ms] iter=${iterations} allocated=${allocCount} freed=${freedHandles.size}` +
                ` aliveTracked=${liveNow.length} scCount=${scTable.length} pages=${pages.length}` +
                ` (${((pages.length * 4) / 1024).toFixed(1)}MB)`
            )
        }
    }

    console.log(`chaos test seed=${seed} done: ${iterations} iterations in ${Date.now() - startTime}ms`)
    if (verify) console.log(`  soundness violations: ${violations}`)
    console.log(`  allocated=${allocCount} freed=${freedHandles.size} mutations=${mutationCount}`)
    console.log(`  scCount: ${startScCount} -> ${scTable.length} (+${scTable.length - startScCount})`)
    console.log(`  pages: ${startPages} -> ${pages.length} (+${pages.length - startPages}, ${(((pages.length - startPages) * 4) / 1024).toFixed(1)}MB)`)
    console.log(`  live-cell-to-page ratio: ${(alive.filter((h) => !freedHandles.has(h)).length / pages.length).toFixed(2)}`)
}

chaosTest(42424, 16000, 0.02, true)

// --- minimal-repro harness for chaosTest soundness violations: records
// every individual action (alloc / add / remove, using stable indices into
// the append-only `alive` array) and stops at the first violation, then
// delta-debugs down to the smallest failing sequence.
function chaosFindRepro(seed, maxActions, mutationRate) {
    let s = seed >>> 0
    function rnd() {
        s = (s * 1664525 + 1013904223) >>> 0
        return s / 0xFFFFFFFF
    }
    function rndInt(n) { return Math.floor(rnd() * n) }

    const anchorSc = createSc(root)
    const rootCell = createTestCell(anchorSc)
    const roots = new Set([rootCell])
    const shadow = new Map()
    function shadowAdd(f, t) {
        if (!shadow.has(f)) shadow.set(f, new Set())
        shadow.get(f).add(t)
    }
    function reachable() {
        const seen = new Set()
        const stack = [...roots]
        while (stack.length) {
            const h = stack.pop()
            if (seen.has(h)) continue
            seen.add(h)
            for (const t of shadow.get(h) ?? []) stack.push(t)
        }
        return seen
    }

    const real_top = createSc(root)
    const top = createSc(real_top)
    let alive = []
    let pinnedRoot = false
    const opLog = []
    let foundErr = null

    for (let action = 0; action < maxActions; action++) {
        const doAlloc = alive.length === 0 || rnd() < 0.3
        if (doAlloc) {
            opLog.push(['alloc'])
            let h
            try {
                h = allocInto(top)
            } catch (e) {
                foundErr = e.message
                break
            }
            alive.push(h)
            if (!pinnedRoot) {
                addTestEdge(rootCell, h)
                shadowAdd(rootCell, h)
                pinnedRoot = true
            }
        } else {
            const liveNow = alive.filter((h) => !freedHandles.has(h))
            if (liveNow.length < 2) continue
            const fromH = liveNow[rndInt(liveNow.length)]
            const toH = liveNow[rndInt(liveNow.length)]
            if (fromH === toH) continue
            const from = alive.indexOf(fromH)
            const to = alive.indexOf(toH)
            const m = edgeMap.get(fromH)
            const existing = m?.get(toH) ?? 0
            const isAdd = existing === 0 || rnd() < 0.5
            opLog.push([isAdd ? 'add' : 'remove', from, to])
            try {
                if (isAdd) {
                    addTestEdge(fromH, toH)
                    shadowAdd(fromH, toH)
                } else {
                    removeTestEdge(fromH, toH)
                    const m2 = edgeMap.get(fromH)
                    if (!m2 || !m2.has(toH)) shadow.get(fromH)?.delete(toH)
                }
            } catch (e) {
                foundErr = e.message
                break
            }
        }

        for (const h of reachable()) {
            if (freedHandles.has(h)) {
                foundErr = 'SOUNDNESS: ' + h
                break
            }
        }
        if (foundErr) break
    }

    console.log('found error:', foundErr, 'after', opLog.length, 'ops')
    return { opLog, foundErr }
}

function replayChaosOps(ops, quiet) {
    const anchorSc = createSc(root)
    const rootCell = createTestCell(anchorSc)
    const roots = new Set([rootCell])
    const shadow = new Map()
    function shadowAdd(f, t) {
        if (!shadow.has(f)) shadow.set(f, new Set())
        shadow.get(f).add(t)
    }
    function reachable() {
        const seen = new Set()
        const stack = [...roots]
        while (stack.length) {
            const h = stack.pop()
            if (seen.has(h)) continue
            seen.add(h)
            for (const t of shadow.get(h) ?? []) stack.push(t)
        }
        return seen
    }
    const top = createSc(root)
    let alive = []
    let pinnedRoot = false

    const origLog = console.log
    if (quiet) console.log = () => {}
    try {
        for (const op of ops) {
            if (op[0] === 'alloc') {
                const h = allocInto(top)
                alive.push(h)
                if (!pinnedRoot) {
                    addTestEdge(rootCell, h)
                    shadowAdd(rootCell, h)
                    pinnedRoot = true
                }
            } else if (op[0] === 'add') {
                const fromH = alive[op[1]]
                const toH = alive[op[2]]
                if (fromH === undefined || toH === undefined) continue
                if (freedHandles.has(fromH) || freedHandles.has(toH)) continue
                addTestEdge(fromH, toH)
                shadowAdd(fromH, toH)
            } else if (op[0] === 'remove') {
                const fromH = alive[op[1]]
                const toH = alive[op[2]]
                if (fromH === undefined || toH === undefined) continue
                if (freedHandles.has(fromH) || freedHandles.has(toH)) continue
                if (!(edgeMap.get(fromH)?.get(toH) > 0)) continue
                removeTestEdge(fromH, toH)
                const m2 = edgeMap.get(fromH)
                if (!m2 || !m2.has(toH)) shadow.get(fromH)?.delete(toH)
            }
            for (const h of reachable()) {
                if (freedHandles.has(h)) return 'SOUNDNESS: ' + h
            }
        }
        return null
    } catch (e) {
        return e.message
    } finally {
        if (quiet) console.log = origLog
    }
}

function ddminChaos(ops, matchesErr) {
    let current = ops.slice()
    let changed = true
    while (changed) {
        changed = false
        for (let i = 0; i < current.length; i++) {
            const candidate = current.slice(0, i).concat(current.slice(i + 1))
            if (matchesErr(replayChaosOps(candidate, true))) {
                current = candidate
                changed = true
                break
            }
        }
    }
    return current
}

// const { opLog: chaosOpLog, foundErr: chaosFoundErr } = chaosFindRepro(42424, 20000, 0.02)
// if (chaosFoundErr) {
//     const errPrefix = chaosFoundErr.startsWith('SOUNDNESS') ? 'SOUNDNESS' : chaosFoundErr
//     const minimalChaos = ddminChaos(chaosOpLog, (e) => e && (e.startsWith(errPrefix)))
//     console.log('minimal chaos repro (', minimalChaos.length, 'ops):', JSON.stringify(minimalChaos))
//     console.log('minimal replay error:', replayChaosOps(minimalChaos, false))
// }

function replayCellChurnOps(ops, numLeafScs, initialCellsPerLeaf, quiet) {
    const mid = createSc(root)
    const leafScs = []
    for (let i = 0; i < numLeafScs; i++) leafScs.push(createSc(mid))
    const anchorSc = createSc(mid)
    const rootCell = createTestCell(anchorSc)
    let cells = []
    for (let i = 0; i < numLeafScs; i++) {
        for (let j = 0; j < initialCellsPerLeaf; j++) cells.push(createTestCell(leafScs[i]))
    }
    addTestEdge(rootCell, cells[0])

    const origLog = console.log
    if (quiet) console.log = () => {}
    try {
        for (const op of ops) {
            if (op[0] === 'create') {
                if (leafScs[op[1]].isFreed()) continue // matches liveLeafScs filtering: never target an already-freed SC
                cells.push(createTestCell(leafScs[op[1]]))
            } else if (op[0] === 'free') {
                if (freedHandles.has(cells[op[1]])) continue // matches live-filtering: never target an already-freed cell
                free(cells[op[1]])
            } else if (op[0] === 'add') {
                if (freedHandles.has(cells[op[1]]) || freedHandles.has(cells[op[2]])) continue
                addTestEdge(cells[op[1]], cells[op[2]])
            } else if (op[0] === 'remove') {
                if (freedHandles.has(cells[op[1]]) || freedHandles.has(cells[op[2]])) continue
                if (!(edgeMap.get(cells[op[1]])?.get(cells[op[2]]) > 0)) continue // no such edge to remove
                removeTestEdge(cells[op[1]], cells[op[2]])
            }
        }
        return null
    } catch (e) {
        return e.message
    } finally {
        if (quiet) console.log = origLog
    }
}

function isValidCellChurnSequence(ops) {
    const created = new Set([0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]) // initial 4x4 pool indices
    let nextIdx = 16
    const freed = new Set()
    const edgeCounts = new Map()
    for (const op of ops) {
        if (op[0] === 'create') {
            created.add(nextIdx)
            nextIdx++
        } else if (op[0] === 'free') {
            if (!created.has(op[1]) || freed.has(op[1])) return false
            freed.add(op[1])
        } else if (op[0] === 'add') {
            if (!created.has(op[1]) || !created.has(op[2])) return false
            if (freed.has(op[1]) || freed.has(op[2])) return false
            const key = op[1] + ':' + op[2]
            edgeCounts.set(key, (edgeCounts.get(key) ?? 0) + 1)
        } else if (op[0] === 'remove') {
            if (!created.has(op[1]) || !created.has(op[2])) return false
            const key = op[1] + ':' + op[2]
            const c = edgeCounts.get(key) ?? 0
            if (c <= 0) return false
            edgeCounts.set(key, c - 1)
        }
    }
    return true
}

function ddminCellChurn(ops, numLeafScs, initialCellsPerLeaf, matchesErr) {
    let current = ops.slice()
    let changed = true
    while (changed) {
        changed = false
        for (let i = 0; i < current.length; i++) {
            const candidate = current.slice(0, i).concat(current.slice(i + 1))
            if (matchesErr(replayCellChurnOps(candidate, numLeafScs, initialCellsPerLeaf, true))) {
                current = candidate
                changed = true
                break
            }
        }
    }
    return current
}

