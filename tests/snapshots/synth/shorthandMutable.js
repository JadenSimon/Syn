// @filename: main.syn
function makeBitpack(types) {
  let bitsUsed = 0
  for (const x of types) {
    const next = 1
    if (bitsUsed + next > 16) break
    bitsUsed += next;
  }
  return {
    bitsUsed
  }
}
