// @filename: main.ts
export const nested = (x = function(y = (z) => z) {}) => x
export const templated = (a, b, c, k) => `${(a ?? `${k === "p" ? "" : `${k}-`}${b}`) || "d"}${c ? `${c}` : ""}`
export const templated2 = (a, b = `${''}()){}})\\`) => ``
export const templated3 = (a, b = `${''}()
){}})\\`) => ``
function t(...args) {}
export const taggedTemplate = (a, b = t`${''}`) => ``
export const regexp = (a = /\)/) => a
export const regexp2 = (a = /=\)/) => a
export const regexp3 = (a = /\/=\{\){}/) => a
export const typed = (a) => true
let y2
export const conditional = true ? y2 = () => true : false
export const conditional2 = true ? (y2 = () => true) : false
export const conditional3 = true ? (y2 = ({
  y2
})) : false
export const conditional4 = true ? (y2 = {
  y2
}) : false
export const conditional5 = true ? (y2 = () => ({
  y2
})) : false
export const parens1 = (() => ({
  y2
}))
