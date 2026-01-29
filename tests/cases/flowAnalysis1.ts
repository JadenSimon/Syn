// export function f(x: string | 1 | 2 | 3) {
//     if (typeof x === 'number') if (x === 1 || x === 3) return;
//     if (x === 2) return x
//     return x // string
// }


// 2 | 3 | 4 | 5 | -2 | -1
// export function stressReturn(
//   a: 0 | 1 | 2 | 3 | 4 | 5 | undefined,
//   b: 0 | 1 | 2 | 6 | undefined,
//   c: 0 | 1 | undefined,
//   f: boolean,
//   g: boolean,
//   h: boolean,
// ) {
//   let x: -2 | 0 | 1 | 2 | 3 | 4 | 5 | undefined = a
//   let y: 0 | 1 | 2 | 6 | 7 | undefined = b
//   let z: 0 | 1 | 5 | undefined = c

//   if (x === 5 && f) {
//     return x        // 5
//   }

//   if ((x === 3 && (y = 2)) || (g && (x = 1))) {
//     if (y === 2) {
//       return y      // 2
//     }
//     if (x === 1) throw ''
//     return x        // 3
//   }

//   if ((z === 1 && (x = 0)) || (h && (z = 0))) {
//     throw new Error('')
//   }

//   if (x === undefined || y === undefined || z === undefined) {
//     return -1
//   }

//   if (x === 0 && (x = -2)) {
//     return x
//   }

//   if (x === 1) throw ''

//   return x || y || z // `x` is never falsy
// }


export function f3(a: 1 | 2 | 3 | 4 | undefined, cond1: boolean, cond2: boolean) {
    if ((cond1 && a === 1) || (cond2 && a === 2) || a === 3) {
        return a
    }

}

export function f4(cond: boolean) {
    let x: 0 | 1 | 2 = 0
    if (cond || (x = 1)) {
        return x
    }
    return x
}

export function f5(cond: boolean) {
    let x: 0 | 1 | 2 = 0
    if (cond) {
        x = 2
    } else {
        x = 1
    }
    return x
}

export function f6() {
    const x = {} as { a: number | string }

    if (typeof x.a === 'number') {
        let y = { ...x }
        return y
    }

    return x.a
}

export function f7() {
    const x = {} as { a: 0 | 1 | 2 | string}

    if (typeof x.a === 'string' || x.a === 0) {
        if (x.a !== 0) return -1
        return x.a
    }
}

export function f8() {
    let x: number | undefined = undefined
    x ??= 1
    return x
}

export function f9() {
    const x = 1 as 1 | 2 | 3 | 4 | 5
    let y = 1 as 1 | 2
    switch (x) {
        case 3:
        case 2:
            if (x === 2) {
                y = 1
                break
            }
            throw x
        case 4:
        case 5:
        case 1:
            throw x        
    }
    return y
}

export function f10() {
    let x: 1 | 2 | 3 = 1 
    let y: 1 | 2 | 3 = 1
    while (x !== 3) {
        x = y * 4
    }
    return x
}

export function f11(x: { a: number } | undefined) {
    if (!x) return

    if (x.a === 2) {
        return x.a
    }
}

export function f12(x: { a: number } | undefined) {
    const y = x ? x.a : '1'

    return y
}

// type S1 = { input: { a: number } };
// type S2 = { input: { b: number } };
// type S3 = { input: { c: number } };
// type S4 = { input: { d: number } };
// type S5 = { input: { e: number } };
// type S6 = { input: { f: number } };
// type S7 = { input: { g: number } };
// type S8 = { input: { h: number } };
// type S9 = { input: { i: number } };
// type S10 = { input: { j: number } };
// type S11 = { input: { k?: string[] } };

// type Base = { type: string; settings: S1 | S2 | S3 | S4 | S5 | S6 | S7 | S8 | S9 | S10 | S11; };

// type Action =
//   | Base & { type: '1'; settings: S1 }
//   | Base & { type: '2'; settings: S2 }
//   | Base & { type: '3'; settings: S3 }
//   | Base & { type: '4'; settings: S4 }
//   | Base & { type: '5'; settings: S5 }
//   | Base & { type: '6'; settings: S6 }
//   | Base & { type: '7'; settings: S7 }
//   | Base & { type: '8'; settings: S8 }
//   | Base & { type: '9'; settings: S9 }
//   | Base & { type: '10'; settings: S10 }
//   | Base & { type: '11'; settings: S11 };

// declare function read<T>(arg: T): T;
// declare const step: Action;

// export const x = read({
//   ...step,
//   settings: { ...step.settings, input: { ...step.settings.input, k: ['5'] } },
// });

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