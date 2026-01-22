export function f(x: string | 1 | 2 | 3) {
    if (typeof x === 'number') if (x === 1 || x === 3) return;
    if (x === 2) return x
    return x // string
}

export function f2(a: 1 | 2 | 3 | 4 | undefined) {
    if (a === 1) {
        const x = a
        const x2 = a
        const x3 = a
        const x4 = a
        return
    } else if (a === 2) {
     //   return
    } else if (a === 3) {
        return
    }

    return a
}

export function f3(a: 1 | 2 | 3 | 4 | undefined, cond1: boolean, cond2: boolean) {
    if ((cond1 && a === 1) || (cond2 && a === 2) || a === 3) {
        return cond2
    }

    return a
}


// export function f2() {
//     const a1 = true as boolean
//     const a2 = true as boolean
//     const a3 = true as boolean
//     if (a1) {
//         if (!a2) return
//     } else {
//         if (!a2) return
//     }
//     return
// }


// export function f4() {
//     const a1 = true as boolean
//     const a2 = true as boolean
//     const a3 = true as boolean
//     if (!a1) return
//     if (!a2) return
//     if (!a3) return
//     return [a1,a2,a3]
// }

// let x: string | number | undefined = undefined

// while (true) {
//     if (typeof x === 'string') {
//         x = 1
//         continue
//     }
//     if (typeof x === 'undefined') {
//         x = '1'
//         continue
//     }
//     x // number
// }