export function f(x: string | 1 | 2 | 3) {
    if (typeof x === 'number') if (x === 1) return;
    return x // string | 2 | 3
}