const input = await Deno.readTextFile('../input.txt')
const lines = input.split(/\r?\n/).map((num) => parseInt(num))
const reducePairs = (limit: number) =>
  lines.reduce(
    (acc, curr) =>
      curr < limit && lines.includes(limit - curr) ? acc * curr : acc,
    1
  )

const tripletResult = lines.reduce((acc, curr) => {
  if (curr < 2020) {
    const pairQuotient = reducePairs(2020 - curr)
    if (pairQuotient > 1) {
      return curr * pairQuotient
    }
  }
  return acc
})

console.log(`Pair result: ${reducePairs(2020)}`)
console.log(`Triplet result: ${tripletResult}`)
