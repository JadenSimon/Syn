// @filename: main.syn
function mutateParamShorthand(x = 1) {
  const c__x = [x]
  c__x[0] = c__x[0] + 1;
  return {
    x: c__x[0]
  }
}
mutateParamShorthand[Symbol.for('toComputation')] = () => ['function mutateParamShorthand(x = 1) {\n  const c__x = [x]\n  c__x[0] = c__x[0] + 1;\n  return {\n    x: c__x[0]\n  }\n}', []];
