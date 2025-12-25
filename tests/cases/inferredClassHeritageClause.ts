// @filename: types.ts
/// <reference lib="es2015" />

interface ResourceDefinition<T extends object = object, U extends any[] = [
]> {
    read?(state: T): T | Promise<T>;
    create(...args: U): T | Promise<T>;
    update?(state: T, ...args: U): T | Promise<T>;
    delete?(state: T, ...args: U): void | Promise<void>;
    import?(id: string): T | Promise<T>;
}
type ResourceConstructor<T extends object = object, U extends any[] = [
]> = {
    new (...args: U): Readonly<T>;
};
export declare function defineResource<T extends object = object, U extends any[] = [
]>(definition: ResourceDefinition<T, U>): ResourceConstructor<T, U>;

// @filename: main.ts
import { defineResource } from './types'

export interface X {
    y?: number
}

function foo<T>(x: { x: T, y?: number }) {
    return x.x
}

export const x = foo({ x: 1 })

function foo2<T>(x: { x(...args: any[]): T }) {
    return x.x()
}

export const x2 = foo2({ x() { return 1 } })


export class ErrorResource extends defineResource({
    create: () => {
        return { stack: new Error().stack }
    }
}) {}

const err = new ErrorResource()
export const errStack = err.stack

export class BucketObject extends defineResource({
    create: async (key: string, data: string) => {
        return { key }
    },
    delete: async (state) => {
    },
}) {}
