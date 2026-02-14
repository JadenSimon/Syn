# Syn

Syn is a programming language that preserves TypeScript muscle memory while enabling patterns TypeScript cannot express without extra tooling. Unlike TypeScript, Syn treats programs as inputs to a synthesis phase, not just a type-checking pass. 

Synthesis _executes_ programs at build time and serializes their resulting state as portable JavaScript without implicit runtime dependencies. Programs can use types as values, precompute expensive operations, embed build-time data, and create deployable infrastructure through synthesis. Deployment is treated as a first-class concern of the language ecosystem instead of a separate tool.

## Using Syn
You'll need [Synapse](https://github.com/Cohesible/synapse?tab=readme-ov-file#installation) to build Syn. Synapse is the underlying synthesis/deployment engine that Syn builds on top of and will eventually be folded into this repo. Currently, this repo contains only the language frontend.

### Building
1. Clone the Synapse repo: https://github.com/Cohesible/synapse
2. In the _Syn_ repo, create the language frontend package using `synapse run package`
3. Move `out/package.tgz` into the Synapse repo and change the `typescript` dependency version to `file:package.tgz`. 
4. Run `synapse install && synapse compile --no-synth && synapse build` to create `dist/bin/synapse`, which can parse and execute `.syn` files.

## Differences from TypeScript
* Intersection types `T & U` with objects behave like the runtime spread syntax (flat merge)
    - Use `Intersect<T, U>` for true type intersection 
* Conditional types only distribute unions over `infer`:
    - `A extends any ? [A] : never` does _not_ distribute
    - `A extends infer U ? [U] : never` does distribute
* `keyof` on non-objects, include arrays/tuples, no longer uses their prototypes
    - `keyof 'aa'` - 0 | 1
    - `keyof []` - never
    - `keyof string[]` - number
* `object` is stricter
    - no pure callable types
    - no array/tuple types
<!-- * `any` is not viral and does not turn off all type checking. `any` behaves more like `unknown`
    - `(x as any).y` checker error, `x` could be null or undefined
    - `(x as any)?.y` OK, produces the same `any`
    - `any` does _not_ skip assignment checks unless `as any` is used at that site -->
