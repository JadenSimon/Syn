
type PathArgs<T extends string> = T extends `${infer P}{${infer U}}${infer S}` 
    ? [...PathArgs<P>, string, ...PathArgs<S>] 
    : []

export type X1 = PathArgs<'/hello'>
export type X2 = PathArgs<'/hello/{x}'>