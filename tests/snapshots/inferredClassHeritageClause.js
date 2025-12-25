// @filename: types.ts


// @filename: main.ts
import { defineResource } from './types'
function foo(x) {
  return x.x
}
export const x = foo({
  x: 1
})
function foo2(x) {
  return x.x()
}
export const x2 = foo2({
  x() {
    return 1
  }
})
export class ErrorResource extends defineResource({
  create: () => {
    return {
      stack: new Error().stack
    }
  }
}) {}
const err = new ErrorResource()
export const errStack = err.stack
export class BucketObject extends defineResource({
  create: async (key, data) => {
    return {
      key
    }
  },
  delete: async (state) => {}
}) {}
