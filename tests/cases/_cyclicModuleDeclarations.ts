declare module "A" {
    export const a = 1
    export { b } from 'B'
}

declare module "B" {
    export * from 'A'
    import { a } from 'A'
    export { a as b }
}

declare module "C" {
    import { b } from 'B'
    export type b = typeof b
}
