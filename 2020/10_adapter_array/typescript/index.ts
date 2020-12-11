const input = await Deno.readTextFile('../input.txt')
const joltages: number[] = input
  .trim()
  .split(/\r?\n/)
  .map((x) => parseInt(x))
  .sort((a, b) => a - b)

const highest = Math.max(...joltages)
joltages.push(highest + 3)

const getRatings = (rating: number) =>
  rating === highest
    ? [rating + 3]
    : new Array(3).fill(rating + 1).map((x, i) => x + i)

const getPossibleNext = (num: number) =>
  getRatings(num)
    .filter((x) => joltages.includes(x))
    .sort((a, b) => a - b)

const getLowestChain = (num: number): number[] => {
  const possibleNext: number[] = getPossibleNext(num)

  if (!possibleNext.length) {
    return []
  }
  const firstMatch = possibleNext[0]
  return [Math.abs(firstMatch - num), ...getLowestChain(firstMatch)]
}

const result = getLowestChain(0)
const oneVoltD = result.filter((x) => x === 1).length
const threeVoltD = result.filter((x) => x === 3).length

console.log(`Part 1: ${oneVoltD * threeVoltD}`)

const memo: { [k: number]: number } = {}
const getAllChains = (num: number): number => {
  if (num === highest + 3) {
    return 1
  }

  if (!memo[num]) {
    memo[num] = getPossibleNext(num).reduce(
      (acc, curr) => acc + getAllChains(curr),
      0
    )
  }

  return memo[num]
}

console.log(`Part 2: ${getAllChains(0)}`)
