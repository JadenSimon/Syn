type Renamed<T> = { [P in keyof T as `name_${P & string}`]: T[P] }
export type Renamed_1 = Renamed<{ x: string }>
export type Renamed_2 = Renamed_1 extends { name_x: string } ? true : false

type Prop<T> = T & string
export type Prop_1 = Prop<'foo'>

export function prop() {
    type y = Prop_1
    const x = <T extends y | number>(a: T) => typeof a === 'number' ? 1 : 0

    return x(2)
}

export function main(...args: string[]) {
    return args[0]
}

// `tsc` uses 12 types to type check these 4 lines
type Box<out T> = { value: T }
export type A = Box<string> | Box<number>
export type B = A extends { value: infer U } ? U : never
export const x: B = '1'