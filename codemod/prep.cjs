exports.prep = function (text) {
  return text
    .replace(/\b(if|while) \(const /g, (m, k) => `${k} (      `)
    .replace(/(^|[\s;{}])defer(\s)/g, (m, a, b) => `${a}     ${b}`)
}
