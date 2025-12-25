// @filename: types.ts
interface ResourceDefinition<T extends object = object, U extends any[] = []> {
  read?(state: T): T | Promise<T>
  create(...args: U): T | Promise<T>
  update?(state: T, ...args: U): T | Promise<T>
  delete?(state: T, ...args: U): void | Promise<void>
  import?(id: string): T | Promise<T>
}
type ResourceConstructor<T extends object = object, U extends any[] = []> = { new (...args: U): Readonly<T> };
export declare function defineResource<T extends object, U extends any[]>(definition: ResourceDefinition<T, U>): ResourceConstructor<T, U>;

// @filename: main.ts
export interface X {
  y?: number
}
export declare const x: number
export declare const x2: number
export declare class ErrorResource extends ResourceConstructor<{ stack: string | undefined }, []> {}
export declare const errStack: string | undefined
export declare class BucketObject extends ResourceConstructor<{ key: string }, [key: string, data: string]> {}
