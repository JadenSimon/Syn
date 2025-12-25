declare module "foo" {
    export const a = 1
    export const b = 2
}

declare module "foo2" {
    export const a = 3
    export const c = 4
}

declare module "foo3" {
    export * from 'foo'
    export * from 'foo2'
    export { a } from 'foo2'
}

declare module "foo4" {
    import * as x from 'foo3'
    export type Result = typeof x.a
}