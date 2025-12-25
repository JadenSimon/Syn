enum X {
    a = 1,
    b,
    c = (a << 4) | 0x3,
}

// FIXME: should be treated as a "namespace access" type
export const x = X.c