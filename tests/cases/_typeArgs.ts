function fn<T, U>(a: T, b: U): T & U {
    return Object.assign(a as any, b) as any
} 

// we're specifically testing semicolons in the interface types...
export const c = fn<{ a: string; }, { b: number; }>({ a: '' }, { b: 1 })