const input = await Deno.readTextFile('../input.txt')
const seats: string[][] = input
  .trim()
  .split(/\r?\n/)
  .map((line) => line.split(''))

const getSurrounding = (list: string[][], row: number, col: number) => {
  const limits = [-1, list.length, list[0].length]
  const checks = [
    [row - 1, col - 1],
    [row - 1, col],
    [row - 1, col + 1],
    [row, col - 1],
    [row, col + 1],
    [row + 1, col - 1],
    [row + 1, col],
    [row + 1, col + 1],
  ]
  return checks.flatMap(([row, col]) => {
    return limits.includes(row) || limits.includes(col) ? [] : [list[row][col]]
  })
}
const gridCompare = (grid1: string[][], grid2: string[][]) => {
  if (grid1.length !== grid2.length) {
    return false
  }
  for (let i = 0; i < grid1.length; i++) {
    for (let j = 0; j < grid1[0].length; j++) {
      if (grid1[i][j] !== grid2[i][j]) {
        return false
      }
    }
  }
  return true
}

let result = seats
while (true) {
  const nextGrid = result.map((row, rowNum) => {
    return row.map((seat, colNum) => {
      const occupied = getSurrounding(result, rowNum, colNum).filter(
        (x) => x === '#'
      )
      switch (seat) {
        case 'L':
          return !occupied.length ? '#' : seat
        case '#':
          return occupied.length >= 4 ? 'L' : seat
        default:
          return seat
      }
    })
  })

  if (gridCompare(result, nextGrid)) {
    break
  } else {
    result = nextGrid
  }
}

const occupied = result.reduce(
  (acc, curr) => acc + curr.filter((x) => x === '#').length,
  0
)

console.log(`Part 1: ${occupied}`)
