const file = await Deno.readTextFile('../input.txt')
const numbers: BigInt[] = file.trim().split(/\r?\n/).map(BigInt)

//@ts-ignore
const add = (a: BigInt, b: BigInt) => a + b
//@ts-ignore
const sub = (a: BigInt, b: BigInt): BigInt => a - b

const sumInPrevious = (
  list: BigInt[],
  checkIdx: number,
  amount: number = 25
) => {
  const lowIdx = checkIdx - amount < 0 ? 0 : checkIdx - amount
  const checkNum = list[checkIdx]
  const previous = list.slice(lowIdx, checkIdx)

  return previous
    .map((x, i, arr) => {
      return arr.reduce((acc, curr, j) => {
        if (j == i) {
          return acc
        } else {
          return [...acc, add(curr, x)]
        }
      }, [] as BigInt[])
    })
    .some((x) => x.includes(checkNum))
}

const findWeakness = (list: BigInt[], check: BigInt) => {
  let start = 0
  let stop = 0
  let sum = list[0]

  while (sum !== check) {
    if (sum < check) {
      stop += 1
      sum = add(sum, list[stop])
    } else {
      sum = sub(sum, list[start])
      start += 1
    }
  }

  const correct = list.slice(start, stop)
  const result = correct
    .reduce(([min, max], e) => [e < min ? e : min, e > max ? e : max], [
      correct[0],
      correct[0],
    ])
    .reduce((a, c) => add(a, c))
  return result
}

const firstWrong = numbers.reduce(
  (acc, curr, i, arr) => (sumInPrevious(arr, i) ? acc : curr),
  BigInt(0)
)

console.log(`Part 1: ${firstWrong.toString()}`)
console.log(`Part 2: ${findWeakness(numbers, firstWrong)}`)
