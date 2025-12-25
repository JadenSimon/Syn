export function doStuff(x: number): string
export function doStuff(x: string): number
export function doStuff<T>(x: T): T
export function doStuff(x: any) { return x }

export const x1 = doStuff(1)
export const x2 = doStuff('1')
export const x3 = doStuff(null)
