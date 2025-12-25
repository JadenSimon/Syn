

export class Delete { delete() {}; 'ok'() {} 
    async get() {}
    async set() {}
    async *values() {}
}

export class Static {
    static {}
    static {
        this
    }
    static {
        { 2 }
        1
    }
}

class Passthrough {
    constructor(public readonly data: any) {}

    getData() {
        return this.data
    }
}

const v1 = new Passthrough('foo')
const v2 = new Passthrough('bar')

class SwitchedTestClassExternal {
    private readonly data: Passthrough

    constructor(kind: '1' | '2') {
        switch (kind) {
            case '1':
                this.data = v1
                break
            case '2':
                this.data = v2
                break
        }
    }

    callMe() {
        return this.data.getData()
    }
}

class TestClass {
    constructor(public readonly data: string) {}

    foo(str: string) {
        return this.bar() + str
    }

    foo2(str: string) {
        return this.bar() + str
    }

    bar() {
        return this.data
    }
}
