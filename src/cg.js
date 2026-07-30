// chandelier graph

const putInVertTrace = []
function traceInVert(scId, relIdx, val) {
    return
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

const pageFreeList = []

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
        if (!val) throw new Error('invalid depth. never set depth to zero')
        checkOneByteOverflow(val)
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
        assertRelIdx(relIdx)
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
            if (v.getUint32(outVertStart+(i*5), true) === absHandle) {
                return v.getUint8(outVertStart+(i*5)+4)
            }
        }
    }
    function checkOneByteOverflow(v) {
        if (v < 0) throw new Error(`u8 underflow: ${v}`)
        if (v >= 256) throw new Error(`u8 overflow: ${v}`)
        if (!Number.isInteger(v)) throw new Error(`got garbage: ${v}`)
    }
    function assertRelIdx(v) {
        if (!(v >= 1 && v <= 64) || isNaN(v) || !v) throw new Error(`invalid relative index: ${v}`)
    }
    function findPeerEdge(relIdx) {
        if (allocatedPeerEdges) {
            return allocatedPeerEdges.get(relIdx)
        }
        for (let i = 0; i < 6; i++) {
            if (v.getUint8(peerEdgeStart+(i*2)) === relIdx) {
                return v.getUint8(peerEdgeStart+(i*2)+1)
            }
        }
    }
    function putInVert(relIdx, val) {
        assertRelIdx(relIdx)
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
    function collectInVerticals() {
        if (allocatedInVerticals) {
            return [...allocatedInVerticals.keys()]
        }
        const base = inVerticalsStart
        const verticalSize = 2
        const arr = []
        for (let i = 0; i < 6; i++) {
            const k = v.getUint8(base+(i*verticalSize))
            if (k === 0) break
            // arr.push([k, v.getUint8(base+(i*verticalSize)+4)])
            arr.push(k)
        }
        return arr
    }
    // returns true when a vertical edge was added
    function incOutVertical(absHandle) {
        if (allocatedOutVerticals) {
            const v = allocatedOutVerticals.get(absHandle)
            allocatedOutVerticals.set(absHandle, (v ?? 0) + 1)
            return !v
        }
        const base = outVertStart
        const verticalSize = 5
        for (let i = 0; i < 6; i++) {
            const k = v.getUint32(base+(i*verticalSize), true)
            if (k === absHandle) {
                const cur = v.getUint8(base+(i*verticalSize)+4)
                if (cur === 255) { // would overflow the u8 slot -- bail to the map early
                    allocOutVerts()
                    allocatedOutVerticals.set(absHandle, cur + 1)
                    return false
                }
                v.setUint8(base+(i*verticalSize)+4, cur + 1)
                return false
            }
            if (k === 0) {
                v.setUint32(base+(i*verticalSize), absHandle, true)
                v.setUint8(base+(i*verticalSize)+4, 1)
                return true
            }
        }
        allocOutVerts()
        allocatedOutVerticals.set(absHandle, 1)
        return true
    }
    // returns true when a vertical edge was removed
    function decOutVertical(absHandle) {
        if (allocatedOutVerticals) {
            const v = allocatedOutVerticals.get(absHandle)
            if (!v || v < 0) throw new Error('womp ?')
            if (v === 1) {
                allocatedOutVerticals.delete(absHandle)
            } else {
                allocatedOutVerticals.set(absHandle, v - 1)
            }
            return v === 1
        }
        const base = outVertStart
        const verticalSize = 5
        for (let i = 0; i < 6; i++) {
            const k = v.getUint32(base+(i*verticalSize), true)
            if (k !== absHandle) continue
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
        printInfo()
        throw new Error(`not found! handle: ${absHandle}`)
    }
    function putPeerEdge(relIdx, val) {
        assertRelIdx(relIdx)
        if (val < 0) throw new Error(`peer edge underflow: ${relIdx} -> ${val}`)
        if (relIdx === getSelfLocalId() && val !== 0) {
            printInfo()
            throw new Error(`peer edge circularity: ${relIdx}`)
        }
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
    function getLocalRc(relIdx) {
        assertRelIdx(relIdx)
        if (allocatedLocalRc) return allocatedLocalRc[relIdx-1] ?? 0
        return v.getUint8(heapCellStart+(relIdx-1)*5)
    }
    function setLocalRc(relIdx, val) {
        assertRelIdx(relIdx)
        if (!allocatedLocalRc && val > 255) allocLocalRc() // would overflow the u8 slot -- bail to the array early
        if (allocatedLocalRc) {
            allocatedLocalRc[relIdx-1] = val
            return
        }
        checkOneByteOverflow(val)
        return v.setUint8(heapCellStart+(relIdx-1)*5, val)
    }
    function incLocalRc(relIdx) {
        assertRelIdx(relIdx)
        const nv = getLocalRc(relIdx) + 1
        setLocalRc(relIdx, nv)
        return nv
    }
    function decLocalRc(relIdx) {
        assertRelIdx(relIdx)
        const nv = getLocalRc(relIdx) - 1
        setLocalRc(relIdx, nv)
        return nv
    }
    // these assumed unbiased
    function getHeapCellId(idx) {
        return v.getUint32(heapCellStart+idx*5+1, true)
    }
    function setHeapCellId(idx, val) {
        assertRelIdx(idx+1)
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
        if (id === 64) throw new Error('SC cell overflow')
        setCellCount(id + 1)
        setHeapCellId(id, val)
        return id + 1
    }
    function removeHeapCell(relIdx) {
        assertRelIdx(relIdx)
        const count = getCellCount()
        setHeapCellId(relIdx-1, 0)
        if (count === relIdx) {
            setCellCount(count - 1)
        } else {
            heapCellFreeList.push(relIdx)
            if (findInVert(relIdx)) putInVert(relIdx, 0)
            if (getLocalRc(relIdx)) setLocalRc(relIdx, 0)
        }
    }
    function freeHeapCell(relIdx, expected) {
        const addr = getHeapCellId(relIdx-1)
        if (addr === 0) return false
        // we aren't using "real" refs
        // if (addr !== expected) throw new Error(`bad free: ${addr} !== ${expected} [idx: ${relId}]`)
        removeHeapCell(relIdx)
        return true
    }
    function collectHeapCells() {
        const arr = []
        const c = getCellCount()
        for (let i = 0; i < c; i++) {
            const k = getHeapCellId(i)
            if (k === 0) continue
            arr.push([i+1, k])
        }
        return arr
    }
    const heapCellEnd = heapCellStart+(maxCells*5)
    // XXX
    function getSelfLocalId() {
        const val = v.getUint8(heapCellEnd)
        if (val === 0) throw new Error(`corrupted SC: ${debugScId}`)
        assertRelIdx(val)
        return val
    }
    function setSelfLocalId(val) {
        assertRelIdx(val)
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
        console.log('depth: ' + getDepth())
        console.log(`--- ${getLiveCellCount()} cells ---`)
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
    function getLiveCellCount() {
        return getCellCount() - heapCellFreeList.length
    }
    function clearPeerEdges() {
        allocatedPeerEdges = undefined
        for (let i = 0; i < 6; i++) {
            v.setUint8(peerEdgeStart+(i*2), 0)
            v.setUint8(peerEdgeStart+(i*2)+1, 0)
        }
    }
    function clearOutVerts() {
        allocatedOutVerticals = undefined
        const base = outVertStart
        const verticalSize = 5
        for (let i = 0; i < 6; i++) {
            v.setUint32(base+(i*verticalSize), 0, true)
            v.setUint8(base+(i*verticalSize)+4, 0)
        }
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
        getLiveCellCount,
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
        collectInVerticals,
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
        clearPeerEdges,
        clearOutVerts,
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

function scanScTable() {
    for (const sc of scTable) {
        if (sc.parent === 0 && sc.depth === 0) {
            throw new Error(`corrupted sc: ${sc.id}`)
        }
    }
}

const scIds = []
const scTable = []
function getSc(id) {
    return scTable[id]
}
function createSc(parent, isRoot = false) {
    parent = typeof parent === 'number' ? parent : parent.id
    const id = scIds.length ? scIds.pop() : scTable.length
    let page
    if (!isRoot && pageFreeList.length) {
        const [oldId, p] = pageFreeList.pop()
        page = p
        scTable[oldId] = undefined
        scIds.push(oldId)
        for (let relId = 1; relId <= 64; relId++) {
            freedHandles.delete(toAbsoluteHandle(oldId, relId))
        }
        freedHandles.delete(oldId | (1 << 30))
    } else {
        const [_, p] = allocPage()
        page = p
    }
    const view = createScView(page, 0, id)
    const self = {
        id,
        parent: isRoot ? undefined : parent,
        page,
        ...view,
        get depth() {
            return view.getDepth()
        },
    }
    view.setParentRef(id)
    if (!isRoot) {
        self.setSelfLocalId(getSc(parent).pushHeapCell(self.id | (1 << 30)))
    }
    const parentDepth = getSc(parent)?.getDepth()
    if (parentDepth === undefined && !isRoot) throw new Error(`failed to find root`)
    if (!isRoot) {
        view.setDepth(parentDepth + 1)
    }
    scTable[id] = self
    view.setFreed(false)
    return self
}

function addEdge(from, to) {
    if (isSc(from) || isSc(to)) throw new Error(`addEdge is only for leaf handles: ${from} -> ${to}`)
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
        if (d1 >= d2 && d1 > 0) {
            fromSc = getSc(fromSc.parent)
            if (!fromSc.incOutVertical(to)) return
        }
        if (d1 <= d2 && d2 > 0) {
            const next = getSc(toSc.parent)
            // addInboundVertical(next, (toSc.id | (1 << 30)))
            arr.push(next, (toSc.id | (1 << 30)))
            toSc = next
        }
    }
    if (fromSc === toSc) {
        fromSc.printInfo()
        throw new Error('that is weird')
    }
    if (fromSc.depth !== toSc.depth) {
        throw new Error(`mismatched depths: ${fromSc.depth} !== ${toSc.depth}`)
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
    if (!invc) {
        if (isSc(toHandle)) {
            console.log('--- from info ---')
            fromSc.printInfo()
            console.log('-----------------')
            getSc(toHandle & ~(1 << 30)).printInfo()
            throw new Error('got sc')
        }
        const targetSc = getSc(getScId(toHandle))
        console.log(getScId(toHandle))
        console.log('--- target info ---')
        targetSc.printInfo()
        console.log('-------------------')
        throw new Error('??? no: ' + relId + ' : ' + fromSc.printInfo() + ' : ' + toHandle)
    }
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
    if (sc.allocParent) {
        sc.allocParent.activeSc = undefined
    }
    new Uint8Array(sc.page).fill(0)
    pageFreeList.push([sc.id, sc.page])
    sc.setFreed(true)
    for (let relId = 1; relId <= 64; relId++) {
        edgeMap.delete(toAbsoluteHandle(sc.id, relId))
    }
}

function free(handle) {
    if (isSc(handle)) {
        const scid = handle & ~(1 << 30)
        const sc = getSc(scid)
        if (sc.isEternal() || sc.isFreed()) return
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
        edgeMap.delete(handle)
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
        if (d1 >= d2 && d1 > 0) {
            fromSc = getSc(fromSc.parent)
            if (!fromSc.decOutVertical(to)) return
        }
        if (d1 <= d2 && d2 > 0) {
            const next = getSc(toSc.parent)
            arr.push(next, (toSc.id | (1 << 30)))
            // removeInboundVertical(next, (toSc.id | (1 << 30)))
            toSc = next
        }
    }
    if (removeScPeerEdge(fromSc, toSc)) {
        // the entire subgraph was just removed
        return
    }
    for (let i = 0; i < arr.length; i += 2) {
        if (arr[i].isFreed()) continue // this can happen when we have up verts holding up intermediate ancestors
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

function clearScEdges(sc) {
    for (const leafHandle of sc.collectOutVerticals()) {
        const scid2 = getScId(leafHandle)
        if (sc.id === scid2) {
            throw new Error('should never happen')
        }
        if (sc.isFreed()) throw new Error('unexpected free')
        if (getSc(scid2).isFreed()) continue //throw new Error('unexpected free')
        // FIXME: this should be bulk
        const count = sc.findOutVert(leafHandle)
        for (let i = 0; i < count; i++) {
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
    const parent = getSc(getScId(handle))
    edgeMap.delete(handle)
    for (const [k, v] of m) {
        if (getSc(getScId(k)).isFreed()) continue
        // our parent may get freed while we're removing edges!
        if (parent.isFreed()) break
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
    if (nv < 0) throw new Error('edge removal was called with no edges')
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

function condense(psc) {
    const buckets = new Map() // depthExtent -> [sc, ...]
    for (const x of psc.collectHeapCells()) {
        if (!isSc(x[1])) throw new Error('leaves? hmm')
        const child = getSc(x[1] & ~(1 << 30))
        const arr = buckets.get(child.getDepthExtent()) ?? []
        arr.push(child)
        buckets.set(child.getDepthExtent(), arr)
        // for tarjan's
        child._lowlink = undefined
        child._index = undefined
        child._onstack = false
    }

    let bestDepth = null
    let bestList = null
    for (const [depth, list] of buckets) {
        if (bestList === null || list.length > bestList.length || (list.length === bestList.length && depth < bestDepth)) {
            bestDepth = depth
            bestList = list
        }
    }
    if (!bestList || bestList.length < 2) return

    const bucketIds = new Set(bestList.map((s) => s.id))

    let index = 0
    const stack = []
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
        v._index = index
        v._lowlink = index
        index++
        stack.push(v)
        v._onstack = true
        for (const w of neighborsOf(v)) {
            if (w._index === undefined) {
                strongConnect(w)
                v._lowlink = Math.min(v._lowlink, w._lowlink)
            } else if (w._onstack) {
                v._lowlink = Math.min(v._lowlink, w._index)
            }
        }
        if (v._lowlink === v._index) {
            const scc = []
            let w
            do {
                w = stack.pop()
                w._onstack = false
                scc.push(w)
            } while (w !== v)
            sccs.push(scc)
        }
    }

    for (const sc of bestList) {
        if (sc._index === undefined) strongConnect(sc)
    }

    const groups = sccs.filter((s) => s.length > 1)
    const remainder = sccs.filter((s) => s.length === 1).flat()
    if (remainder.length > 1) groups.push(remainder)

    const newWrappers = []
    for (const group of groups) {
        newWrappers.push(mergeGroup(psc, group, bestDepth + 1))
    }

    for (const x of psc.collectHeapCells()) {
        if (!isSc(x[1])) continue
        const newSc = getSc(x[1] & ~(1 << 30))
        if (newSc.isFreed()) continue
        const relId = newSc.getSelfLocalId()
        if (psc.getLocalRc(relId) === 0 && !psc.findInVert(relId)) {
            free(newSc.id | (1 << 30))
        }
    }

    // for (const x of psc.collectHeapCells()) {
    //     if (!isSc(x[1])) continue
    //     simplifySc(getSc(x[1] & ~(1 << 30)))
    // }

    let depthExtent = 0
    for (const x of psc.collectHeapCells()) {
        if (!isSc(x[1])) continue
        const child = getSc(x[1] & ~(1 << 30))
        depthExtent = Math.max(child.getDepthExtent(), depthExtent)
    }
    psc.setDepthExtent(depthExtent + 1)
}

function simplifySc(sc) {
    if (sc.depth <= 2) return
    if (sc.getLiveCellCount() !== 1) return
    const child = sc.collectHeapCells()[0]
    if (!child[0]) return
    if (!isSc(child[1])) return
    const parent = getSc(sc.parent)
    parent.setHeapCellId(sc.getSelfLocalId()-1, child[1])
    const childSc = getSc(child[1] & ~(1 << 30))
    childSc.setSelfLocalId(sc.getSelfLocalId())
    // if (sc.getInVert(child[0])) sc.putInVert(child[0], 0)
    sc.removeHeapCell(child[0])
    childSc.parent = sc.parent
    childSc.setDepth(sc.depth)
    childSc.setDepthExtent(sc.getDepthExtent())
    sc.clearOutVerts()
    free(sc.id | (1 << 30))
    simplifySc(childSc)
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

    let totalRc = 0
    let totalInVert = 0
    const perChildInVert = new Map()
    for (const old of group) {
        const v = psc.findInVert(old.getSelfLocalId()) ?? 0
        if (v) perChildInVert.set(old.id, v)
        totalInVert += v
    }
    if (totalInVert > 0) {
        psc.putInVert(newSc.getSelfLocalId(), totalInVert)
    }

    for (const x of psc.collectHeapCells()) {
        if (!isSc(x[1])) continue
        const sibId = x[1] & ~(1 << 30)
        if (sibId === newSc.id || groupIds.has(sibId)) continue
        const sib = getSc(sibId)
        let redirected = 0
        for (const old of group) {
            const v = sib.findPeerEdge(old.getSelfLocalId())
            if (v) {
                redirected += v
                sib.putPeerEdge(old.getSelfLocalId(), 0)
                perChildInVert.set(old.id, (perChildInVert.get(old.id) ?? 0) + v)
            }
        }
        if (redirected > 0) {
            sib.putPeerEdge(newSc.getSelfLocalId(), redirected)
            totalRc += redirected;
        }
    }
    if (totalRc > 0) {
        psc.setLocalRc(newSc.getSelfLocalId(), totalRc)
    }

    const rcs = new Map()
    const needsFixup = new Map()
    for (const old of group) {
        for (const [relId, val] of old.collectPeerEdges()) {
            if (!val) throw new Error('missing val')
            const stored = psc.getHeapCellId(relId - 1)
            if (!stored) throw new Error('missing cell')
            const targetId = stored & ~(1 << 30)
            if (groupIds.has(targetId)) {
                rcs.set(targetId, (rcs.get(targetId) ?? 0) + val)
                let m = needsFixup.get(old)
                if (!m) needsFixup.set(old, m = new Map())
                m.set(getSc(targetId), [relId, val])
                continue
            }
            old.putPeerEdge(relId, 0)
            const target = getSc(targetId)
            const existing = newSc.findPeerEdge(target.getSelfLocalId()) ?? 0
            newSc.putPeerEdge(target.getSelfLocalId(), existing + val)
        }
    }

    for (const old of group) {
        for (const targetHandle of old.collectOutVerticals()) {
            const val = old.findOutVert(targetHandle)
            if (!val) continue
            let scId = isSc(targetHandle) ? (targetHandle & ~(1 << 30)) : getScId(targetHandle)
            let internal = false
            while (scId !== psc.id) {
                if (groupIds.has(scId)) { internal = true; break }
                if (scId === 0) throw new Error('should never happen')
                scId = getSc(scId).parent
            }
            if (internal) continue
            newSc.incOutVertical(targetHandle)
        }
    }

    for (const old of group) {
        psc.setLocalRc(old.getSelfLocalId(), 0)
        if (psc.findInVert(old.getSelfLocalId())) psc.putInVert(old.getSelfLocalId(), 0)
        psc.removeHeapCell(old.getSelfLocalId())
        const individualInVert = perChildInVert.get(old.id)
        const rc = rcs.get(old.id)
        old.parent = newSc.id
        const relIdx = newSc.pushHeapCell(old.id | (1 << 30))
        old.setSelfLocalId(relIdx)
        bumpDepth(old, newSc.depth + 1 - old.depth)
        if (individualInVert) newSc.putInVert(relIdx, individualInVert)
        if (rc) newSc.setLocalRc(relIdx, rc)
    }

    for (const [k, v] of needsFixup) {
        const m = new Map()
        for (const [other, [relIdx, val]] of v) {
            m.set(other.getSelfLocalId(), val)
            k.putPeerEdge(relIdx, 0)
        }
        for (const [k2, v2] of m) {
            k.putPeerEdge(k2, v2)
        }
    }

    return newSc
}

// sigh
function bumpDepth(sc, delta) {
    if (!delta) throw new Error('unexpected zero delta')
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
        cur.activeSc.allocParent = cur
    }

    const count = cur.activeSc.getCellCount()
    if (count < 50) {
        return createTestCell(cur.activeSc)
    }

    const count2 = cur.getCellCount()
    if (count2 < 50) {
        cur.activeSc = createSc(cur)
        cur.activeSc.allocParent = cur
        return createTestCell(cur.activeSc)
    }
    condense(cur)
    if (cur.getCellCount() >= 64) {
        throw new Error('sc overflow')
    }
    cur.activeSc = createSc(cur)
    cur.activeSc.allocParent = cur
    return createTestCell(cur.activeSc)
}

const root = createSc(0, true)
root.setEternal(true)

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

    // pin one churn cell alive via a permanent edge from the isolated anchor
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
            if (e.message === 'out of room!') { capacitySkips++; continue }
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

        // an SC that had vertical support and then lost it but wasn't freed = leak
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

fuzzTest(6001, 8000, 1, 64)
fuzzTest(6002, 8000, 1, 64)
fuzzTest(6003, 8000, 1, 64)

fuzzTest(7001, 8000, 2, 64)
fuzzTest(7002, 8000, 2, 64)
fuzzTest(7003, 8000, 2, 64)

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

function fuzzTestCondense(seed, numChildSCs, cellsPerChild, rounds, stepsPerRound) {
    if (!shouldFuzz) return
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
        const isAdd = rnd() < 0.05
        if (isAdd) {
            addTestEdge(from, to)
            shadowAdd(from, to)
        } else {
            if (!existing) return
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
        try {
            for (let i = 0; i < stepsPerRound; i++) churnStep()
            violations += checkSoundness('pre-condense churn')
        } catch (e) {
            console.log('CHURN ERROR:', e.message, 'round', round, 'seed', seed)
            throw e
        }

        try {
            condense(psc)
        } catch (e) {
            condenseErrors++
            console.log('CONDENSE ERROR:', e.message, 'round', round, 'seed', seed)
            throw e
        }
        violations += checkSoundness('condense')
    }
    console.log(`condense fuzz seed=${seed} n=${numChildSCs}x${cellsPerChild}: ${violations} violations, ${condenseErrors} condense errors, ${freedHandles.size} freed total`)
}

fuzzTestCondense(9001, 12, 4, 4, 200)
fuzzTestCondense(9002, 12, 4, 4, 300)
fuzzTestCondense(9003, 20, 3, 5, 400)
fuzzTestCondense(9004, 20, 3, 5, 400)
fuzzTestCondense(9005, 8, 8, 6, 500)
fuzzTestCondense(9006, 16, 16, 10, 500)
fuzzTestCondense(9007, 32, 32, 20, 500)
fuzzTestCondense(9008, 63, 63, 20, 500)

function chaosTest(seed, durationMs, mutationRate = 0.02, verify = false) {
    // if (!shouldFuzz) return
    let s = seed >>> 0
    function rnd() {
        s = (s * 1664525 + 1013904223) >>> 0
        return s / 0xFFFFFFFF
    }
    function rndInt(n) { return Math.floor(rnd() * n) }

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
    }
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

        const numAlloc = 1 + rndInt(4)
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
            if (freedHandles.has(from) || freedHandles.has(to)) continue
            const m = edgeMap.get(from)
            const existing = m?.get(to) ?? 0
            const isAdd = existing === 0 || rnd() < 0.7
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

  // chaosTest(42424, 30000, 0.5, true)

function chaosStackTest(seed, durationMs, opts = {}) {
    const {
        pushProb: basePushProb = 0.1,
        popProb: basePopProb = 0.08,
        allocProb = 0.5,
        crossEdgeProb = 0.15,
        maxGoalDepth = 500,
        verify = false,
    } = opts

    let s = seed >>> 0
    function rnd() {
        s = (s * 1664525 + 1013904223) >>> 0
        return s / 0xFFFFFFFF
    }
    function rndInt(n) { return Math.floor(rnd() * n) }

    const eternal = createSc(root)
    const eternalCell = createTestCell(eternal)
    let roots, shadow, shadowAdd, reachable
    let violations = 0
    if (verify) {
        roots = new Set([eternalCell])
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
    }
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
                return true
            }
        }
        return false
    }

    const framesParent = createSc(root)
    // frame descriptor: { sc, anchorCell, cells: [] }
    const frames = []

    function pushFrame() {
        if (framesParent.getCellCount() >= 50) condense(framesParent)
        const sc = createSc(framesParent)
        const leafSc = createSc(sc)
        const anchorCell = createTestCell(leafSc)
        const frame = { sc, anchorCell, cells: [] }
        if (frames.length) {
            const caller = frames[frames.length - 1]
            if (freedHandles.has(caller.anchorCell)) {
                console.log(reachable?.(caller.anchorCell))
                throw new Error('caller anchor was freed')
            }
            addTestEdge(caller.anchorCell, anchorCell) // caller -> callee frame link
            if (verify) shadowAdd(caller.anchorCell, anchorCell)
        } else {
            addTestEdge(eternalCell, anchorCell)
            if (verify) shadowAdd(eternalCell, anchorCell)
        }
        frames.push(frame)
        return frame
    }

    function popFrame() {
        if (frames.length < 2) return
        const callee = frames.pop()
        const caller = frames[frames.length - 1]
        removeTestEdge(caller.anchorCell, callee.anchorCell)
        if (verify) shadow.get(caller.anchorCell)?.delete(callee.anchorCell)
    }

    pushFrame()

    function cleanFreed() {
        for (let i = 0; i < frames.length; i++) {
            frames[i].cells = frames[i].cells.filter(x => !freedHandles.has(x))
        }
        freedHandles.clear()
    }

    let goalDepth = 1 + rndInt(maxGoalDepth)
    let goalsReached = 0

    const startTime = Date.now()
    let iterations = 0
    let allocCount = 0
    let pushCount = 0
    let popCount = 0
    let crossEdgeCount = 0
    let allocTime = 0
    let totalFreed = 0
    const startPages = pages.length
    const startScCount = scTable.length

    function iterate() {
        iterations++

        if (frames.length === goalDepth) {
            goalDepth = 1 + rndInt(maxGoalDepth)
            goalsReached++
        }
        const depthDelta = goalDepth - frames.length // >0 wants push, <0 wants pop
        const bias = Math.min(0.35, Math.abs(depthDelta) * 0.02)
        const pushProb = depthDelta > 0 ? basePushProb + bias : Math.max(0.01, basePushProb - bias)
        const popProb = depthDelta < 0 ? basePopProb + bias : Math.max(0.01, basePopProb - bias)

        const roll = rnd()

        if (roll < pushProb) {
            pushFrame()
            pushCount++
        } else if (roll < pushProb + popProb) {
            popFrame()
            popCount++
        } else if (roll < pushProb + popProb + allocProb && frames.length > 1) {
            const allocStart = performance.now()
            const active = frames[frames.length - 1]
            const batch = 1 + rndInt(16)
            for (let i = 0; i < batch; i++) {
                const h = allocInto(active.sc)
                if (freedHandles.has(active.anchorCell)) throw new Error('anchor was freed')
                addTestEdge(active.anchorCell, h) // reachable via the frame itself, like a real local slot
                if (verify) shadowAdd(active.anchorCell, h)
                active.cells.push(h)
                allocCount++
            }
            allocTime += performance.now() - allocStart
        } else if (roll < pushProb + popProb + allocProb + crossEdgeProb && frames.length > 1) {
            const active = frames[frames.length - 1]
            const otherIdx = rndInt(frames.length - 1)
            const other = frames[otherIdx]
            if (active.cells.length > 1 && other.cells.length > 1) {
                // index 0 is always the frame's anchor cell -- reserved for
                // the structural push/pop chain link, never a valid random
                // cross-edge target
                const activeCell = active.cells[1 + rndInt(active.cells.length - 1)]
                const otherCell = other.cells[1 + rndInt(other.cells.length - 1)]
                if (!freedHandles.has(activeCell) && !freedHandles.has(otherCell)) {
                    const forward = rnd() < 0.5
                    const from = forward ? activeCell : otherCell
                    const to = forward ? otherCell : activeCell
                    const m = edgeMap.get(from)
                    const existing = m?.get(to) ?? 0
                    if (existing === 0 || rnd() < 0.6) {
                        addTestEdge(from, to)
                        if (verify) shadowAdd(from, to)
                    } else {
                        removeTestEdge(from, to)
                        if (verify) {
                            const m2 = edgeMap.get(from)
                            if (!m2 || !m2.has(to)) shadow.get(from)?.delete(to)
                        }
                    }
                    crossEdgeCount++
                }
            }
        }

        checkSoundness(`iter=${iterations}`)
        // if (scanAllIntegrity(`iter=${iterations}`)) return

        if (iterations % 2000 === 0) {
            const elapsed = Date.now() - startTime
            // simplifySc(getSc(frames[0].sc.parent))
            frames[0].sc.printInfo()
            // let p = getSc(frames[0].sc.parent)
            // while (p !== framesParent) {
            //     p.printInfo()
            //     p = getSc(p.parent)
            // }
            totalFreed += freedHandles.size
            cleanFreed()
            console.log(
                `[t=${elapsed}ms] iter=${iterations} frames=${frames.length} goal=${goalDepth} allocated=${allocCount}` +
                ` pushed=${pushCount} popped=${popCount} crossEdges=${crossEdgeCount} freed=${totalFreed} allocTime=${Math.round(allocTime)}ms` +
                ` scCount=${scTable.length} pages=${pages.length} (${((pages.length * 4) / 1024).toFixed(1)}MB)`
            )
            allocTime = 0
            {
                const v8 = require("v8");
                v8.getHeapSpaceStatistics().filter(x => x.space_size > 0).forEach(s =>
                    console.log(
                        `${s.space_name.padEnd(22)} ` +
                        `${(s.space_used_size / 1048576).toFixed(1).padStart(6)} / ` +
                        `${(s.space_size / 1048576).toFixed(1).padStart(6)} MB` +
                        `  phys=${(s.physical_space_size / 1048576).toFixed(1).padStart(6)} MB`
                    ));
            }
        }
    }

    while (Date.now() - startTime < durationMs) {
        try {
            iterate()
        } catch (err) {
            console.log('failed at iter', iterations, 'frame count', frames.length)
            throw err
        }
    }

    console.log(`chaos stack test seed=${seed} done: ${iterations} iterations in ${Date.now() - startTime}ms`)
    if (verify) console.log(`  soundness violations: ${violations}`)
    console.log(`  frames alive=${frames.length}, pushed=${pushCount} popped=${popCount}, goal depths reached=${goalsReached}`)
    console.log(`  allocated=${allocCount} crossEdges=${crossEdgeCount} freed=${freedHandles.size}`)
    console.log(`  scCount: ${startScCount} -> ${scTable.length} (+${scTable.length - startScCount})`)
    console.log(`  pages: ${startPages} -> ${pages.length} (+${pages.length - startPages}, ${(((pages.length - startPages) * 4) / 1024).toFixed(1)}MB)`)
}

//chaosStackTest(24683, 16000, { verify: false })

 // chaosStackTest(24682, 100_000, { verify: false })
 chaosStackTest(24684, 200_000_000, { verify: false })

 // chaosStackTest(24681, 16000, { verify: true })

// --- minimal-repro harness for chaosStackTest: records a flat, fully
// resolved op log (push/pop/alloc/cross, with frames identified by a
// stable monotonic id -- never reused, unlike array position -- and cells
// identified by stable per-frame indices) and stops at the first crash or
// soundness violation. Then delta-debugs it down.
function chaosStackFindRepro(seed, maxActions, opts = {}) {
    const { pushProb: basePushProb = 0.1, popProb: basePopProb = 0.08, allocProb = 0.5, crossEdgeProb = 0.15, maxGoalDepth = 500 } = opts
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

    const framesParent = createSc(root)
    const frames = [] // { id, sc, anchorCell, cells: [] } -- stack, id is monotonic
    let nextFrameId = 0

    const opLog = []
    let foundErr = null

    function pushFrame() {
        opLog.push(['push'])
        if (framesParent.getCellCount() >= 50) condense(framesParent)
        const sc = createSc(framesParent)
        const leafSc = createSc(sc)
        const anchorCell = createTestCell(leafSc)
        const frame = { id: nextFrameId++, sc, anchorCell, cells: [anchorCell] }
        if (frames.length) {
            const caller = frames[frames.length - 1]
            addTestEdge(caller.anchorCell, anchorCell)
            shadowAdd(caller.anchorCell, anchorCell)
        } else {
            addTestEdge(rootCell, anchorCell)
            shadowAdd(rootCell, anchorCell)
        }
        frames.push(frame)
    }

    function popFrame() {
        if (frames.length < 2) return
        opLog.push(['pop'])
        const callee = frames.pop()
        const caller = frames[frames.length - 1]
        removeTestEdge(caller.anchorCell, callee.anchorCell)
        shadow.get(caller.anchorCell)?.delete(callee.anchorCell)
    }

    pushFrame()
    opLog.length = 0 // the initial frame is created unconditionally by both this fn and replayStackOps -- don't double-log it

    let goalDepth = 1 + rndInt(maxGoalDepth)
    let goalsReached = 0

    function checkErr() {
        for (const h of reachable()) {
            if (freedHandles.has(h)) return 'SOUNDNESS: ' + h
        }
        return null
    }

    for (let action = 0; action < maxActions; action++) {
        if (frames.length === goalDepth) {
            goalDepth = 1 + rndInt(maxGoalDepth)
            goalsReached++
        }
        const depthDelta = goalDepth - frames.length
        const bias = Math.min(0.35, Math.abs(depthDelta) * 0.02)
        const pushProb = depthDelta > 0 ? basePushProb + bias : Math.max(0.01, basePushProb - bias)
        const popProb = depthDelta < 0 ? basePopProb + bias : Math.max(0.01, basePopProb - bias)
        const roll = rnd()
        try {
            if (roll < pushProb) {
                pushFrame()
            } else if (roll < pushProb + popProb) {
                popFrame()
            } else if (roll < pushProb + popProb + allocProb) {
                const active = frames[frames.length - 1]
                opLog.push(['alloc'])
                const h = allocInto(active.sc)
                addTestEdge(active.anchorCell, h)
                shadowAdd(active.anchorCell, h)
                active.cells.push(h)
            } else if (roll < pushProb + popProb + allocProb + crossEdgeProb && frames.length > 1) {
                const active = frames[frames.length - 1]
                const otherIdx = rndInt(frames.length - 1)
                const other = frames[otherIdx]
                if (active.cells.length > 1 && other.cells.length > 1) {
                    const activeCellIdx = 1 + rndInt(active.cells.length - 1)
                    const otherCellIdx = 1 + rndInt(other.cells.length - 1)
                    const activeCell = active.cells[activeCellIdx]
                    const otherCell = other.cells[otherCellIdx]
                    if (!freedHandles.has(activeCell) && !freedHandles.has(otherCell)) {
                        const forward = rnd() < 0.5
                        const from = forward ? activeCell : otherCell
                        const to = forward ? otherCell : activeCell
                        const m = edgeMap.get(from)
                        const existing = m?.get(to) ?? 0
                        const isAdd = existing === 0 || rnd() < 0.5
                        opLog.push(['cross', other.id, activeCellIdx, otherCellIdx, forward, isAdd])
                        if (isAdd) {
                            addTestEdge(from, to)
                            shadowAdd(from, to)
                        } else {
                            removeTestEdge(from, to)
                            const m2 = edgeMap.get(from)
                            if (!m2 || !m2.has(to)) shadow.get(from)?.delete(to)
                        }
                    }
                }
            }
        } catch (e) {
            foundErr = e.message
            break
        }
        foundErr = checkErr()
        if (foundErr) break
    }

    console.log('found error:', foundErr, 'after', opLog.length, 'ops')
    return { opLog, foundErr }
}

function replayStackOps(ops, quiet, skipReachCheck) {
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

    const framesParent = createSc(root)
    const frames = []
    const framesById = new Map()
    let nextFrameId = 0

    function pushFrame() {
        if (framesParent.getCellCount() >= 50) condense(framesParent)
        const sc = createSc(framesParent)
        const leafSc = createSc(sc)
        const anchorCell = createTestCell(leafSc)
        const frame = { id: nextFrameId++, sc, anchorCell, cells: [anchorCell] }
        if (frames.length) {
            const caller = frames[frames.length - 1]
            addTestEdge(caller.anchorCell, anchorCell)
            shadowAdd(caller.anchorCell, anchorCell)
        } else {
            addTestEdge(rootCell, anchorCell)
            shadowAdd(rootCell, anchorCell)
        }
        frames.push(frame)
        framesById.set(frame.id, frame)
    }
    function popFrame() {
        if (frames.length < 2) return
        const callee = frames.pop()
        const caller = frames[frames.length - 1]
        removeTestEdge(caller.anchorCell, callee.anchorCell)
        shadow.get(caller.anchorCell)?.delete(callee.anchorCell)
    }

    pushFrame()

    const origLog = console.log
    if (quiet) console.log = () => {}
    try {
        for (const op of ops) {
            if (op[0] === 'push') {
                pushFrame()
            } else if (op[0] === 'pop') {
                popFrame()
            } else if (op[0] === 'alloc') {
                const active = frames[frames.length - 1]
                const h = allocInto(active.sc)
                addTestEdge(active.anchorCell, h)
                shadowAdd(active.anchorCell, h)
                active.cells.push(h)
            } else if (op[0] === 'cross') {
                const [, otherFrameId, activeCellIdx, otherCellIdx, forward, isAdd] = op
                const active = frames[frames.length - 1]
                const other = framesById.get(otherFrameId)
                if (!other || !active) continue
                const activeCell = active.cells[activeCellIdx]
                const otherCell = other.cells[otherCellIdx]
                if (activeCell === undefined || otherCell === undefined) continue
                const from = forward ? activeCell : otherCell
                const to = forward ? otherCell : activeCell
                if (freedHandles.has(from) || freedHandles.has(to)) continue
                if (isAdd) {
                    addTestEdge(from, to)
                    shadowAdd(from, to)
                } else {
                    if (!(edgeMap.get(from)?.get(to) > 0)) continue
                    removeTestEdge(from, to)
                    const m2 = edgeMap.get(from)
                    if (!m2 || !m2.has(to)) shadow.get(from)?.delete(to)
                }
            }
            if (!skipReachCheck) {
                for (const h of reachable()) {
                    if (freedHandles.has(h)) return 'SOUNDNESS: ' + h
                }
            }
        }
        return null
    } catch (e) {
        return e.message
    } finally {
        if (quiet) console.log = origLog
    }
}

function ddminStack(ops, matchesErr, skipReachCheck) {
    let current = ops.slice()
    let changed = true
    while (changed) {
        changed = false
        for (let i = 0; i < current.length; i++) {
            const candidate = current.slice(0, i).concat(current.slice(i + 1))
            if (matchesErr(replayStackOps(candidate, true, skipReachCheck))) {
                current = candidate
                changed = true
                break
            }
        }
    }
    return current
}

function testCutawayScenario() {
    const parent = createSc(root)
    const topSc = createSc(parent)
    const otherSc = createSc(parent)
    const a = createTestCell(topSc)
    const b = createTestCell(otherSc)
    addTestEdge(a, b)
    console.log('established a->b. topSc cutaway?', topSc.isCutaway(), 'otherSc cutaway?', otherSc.isCutaway())

    free(topSc.id | (1 << 30))
    console.log('after freeing topSc: topSc cutaway/freed =', topSc.isCutaway(), topSc.isFreed(),
        '| otherSc cutaway/freed =', otherSc.isCutaway(), otherSc.isFreed(),
        '| a freed?', freedHandles.has(a), '| b freed?', freedHandles.has(b))

    const c = createTestCell(topSc)
    console.log('created c in (already cutaway) topSc: c =', c)

    try {
        addTestEdge(c, a)
        console.log('addTestEdge(c, a) [same, dead SC] succeeded')
    } catch (e) {
        console.log('addTestEdge(c, a) threw:', e.message)
    }

    try {
        addTestEdge(c, b)
        console.log('addTestEdge(c, b) [cross into otherSc] succeeded')
        removeTestEdge(c, b)
        console.log('removeTestEdge(c, b) succeeded')
    } catch (e) {
        console.log('cross edge via stale topSc threw:', e.message)
    }
}
testCutawayScenario()

// minimal repro: A->B->C chain across 3 sibling SCs. Merging the SCs
// containing A and B (bypassing condense, calling mergeGroup directly)
// should leave the merged SC with NO out-vertical entry for the A->B edge,
// since B now lives inside the same merged SC as A -- that edge is now
// internal (rekeyed as a peer edge in step 4), not something "leaving" the
// merged SC. Step 2.6 currently mirrors it anyway, which is wrong.
function testMergeGroupInternalEdgeBug() {
    const scA = createSc(root)
    const scB = createSc(root)
    const scC = createSc(root)
    const a = createTestCell(scA)
    const b = createTestCell(scB)
    const c = createTestCell(scC)
    addTestEdge(a, b)
    addTestEdge(b, c)

    const newSc = mergeGroup(root, [scA, scB], 1)
    console.log('--- merged SC (should contain A and B, C should NOT appear as an out-vertical target) ---')
    newSc.printInfo()
    scA.printInfo()
    scB.printInfo()
}
testMergeGroupInternalEdgeBug()

