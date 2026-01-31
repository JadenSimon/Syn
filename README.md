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
* Intersection types with objects behave more like the spread operator (flat merge)
* Reworked enums
    - 
    - enum(string) Foo { success, failure }