// tests for `isArrowFn`

export const nested = (x = function(y = (z) => z) {}) => x

export const templated = (a: any, b: any, c: any, k: any) => `${(a??`${k==="p"?"":`${k}-`}${b}`)||"d"}${c?`${c}`:""}`

export const templated2 = (a: any, b = `${''}()){}})\\`) => ``
export const templated3 = (
    a: any, 
    b = `${''}()
){}})\\`
) => ``

function t(...args: any[]) {}

export const taggedTemplate = (a: any, b = t`${''}`) => ``

export const regexp = (a = /\)/) => a
export const regexp2 = (a = /=\)/) => a
export const regexp3 = (a = /\/=\{\){}/) => a

// param is intentionally untyped 
export const typed = (a): boolean => true 

let y2: any
export const conditional = true ? y2 = (): true => true : false
export const conditional2 = true ? (y2 = (): true => true) : false
export const conditional3 = true ? (y2 = ({ y2 })) : false
export const conditional4 = true ? (y2 = { y2 }) : false
export const conditional5 = true ? (y2 = (): any => ({ y2 })) : false


export const parens1 = ((): any => ({ y2 }))


// types
export type Fn1 = () => true
export type Fn2 = (() => true)
export type Fn3<T> = T extends () => infer U ? (a: U) => void : false
export type Fn4<T> = T extends () => infer U ? ((a: U) => void) : false

