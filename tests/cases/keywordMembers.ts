const x = { true: false }
const y = x.true
const { true: blue } = x
export { blue }

export type Why = { if: 'yes', readonly readonly: number, default: 'default' }

export interface Foo {
    new(): Foo
    <T>(a: T): void
    (): number
    get [1 + 1](): 2
    readonly new: number; foo: string
    'aa'<U>(): U
}

export interface Foo2 {
    [keys: string]: string;
}
