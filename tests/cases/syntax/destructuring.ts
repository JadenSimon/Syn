
const q = Symbol()
const { [q]: binding } = { [q]: 1 }

let { 1: z }: number[] = [1,2]

;({ 'zz': z } = { 'zz': 1 })

export const _z = z