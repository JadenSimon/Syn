interface Shape<T> {
    method<U = T, V = never>(
        cb1: (v: T) => U, 
        cb2?: () => V
    ): Shape<U | V>
}


const x = {} as Shape<number>

export const y = x.method((a) => 'no', () => 1)
export const y2 = x.method(() => 'yes')
export const y3 = x.method(a => a)

declare function method<U>(cb1: () => U): Shape<U>

export const y4 = method(() => 'no')

