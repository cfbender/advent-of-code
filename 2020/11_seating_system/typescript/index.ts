import isEqual from 'http://deno.land/x/lodash@4.17.11-es/isEqual.js'

const input = await Deno.readTextFile('../input.txt')
const seats: string[][] = input
  .trim()
  .split(/\n/)
  .map((line) => `*${line}*`.trim().split(''))

const padding = new Array(seats[0].length).fill('*')
seats.unshift(padding)
seats.push(padding)

const checks = [
  [-1, -1],
  [-1, 0],
  [-1, 1],
  [0, -1],
  [0, 1],
  [1, -1],
  [1, 0],
  [1, 1],
]

const checkAll = (list: string[][], checks: number[][]): string[] => {
  return checks.flatMap(([checkRow, checkCol]) => {
    if (checkRow === -1 || checkCol === -1) {
      return []
    }

    return checkRow >= list.length || checkCol >= list[0].length
      ? []
      : [list[checkRow][checkCol]]
  })
}
const getSurrounding = (list: string[][], row: number, col: number) => {
  const gridChecks = checks.map(([dx, dy]) => [dx + row, dy + col])
  return checkAll(list, gridChecks)
}

const getAllSeen = (list: string[][], initX: number, initY: number) => {
  const gridChecks = checks.flatMap(([dx, dy]) => {
    let x = initX + dx
    let y = initY + dy
    if (x === -1 || y === -1 || x >= list.length || y >= list[0].length) {
      return []
    }

    while (list[x][y] === '.') {
      if (list[x][y] === '*') {
        return []
      }

      x += dx
      y += dy
    }

    return [[x, y]]
  })

  return checkAll(list, gridChecks)
}
const get = (part: number) => {
  let result = seats
  while (true) {
    const nextGrid = result.map((row, rowNum, fullGrid) => {
      return row.map((seat, colNum) => {
        const occupied =
          part === 1
            ? getSurrounding(fullGrid, rowNum, colNum).filter((x) => x === '#')
                .length
            : getAllSeen(fullGrid, rowNum, colNum).filter((x) => x === '#')
                .length

        if (seat === 'L') {
          return !occupied ? '#' : seat
        }

        if (seat === '#') {
          return occupied >= (part === 1 ? 4 : 5) ? 'L' : seat
        }
        return seat
      })
    })

    if (isEqual(result, nextGrid)) {
      break
    } else {
      result = nextGrid
    }
  }

  return result.reduce(
    (acc, curr) => acc + curr.filter((x) => x === '#').length,
    0
  )
}

console.log(`Part 1: ${get(1)}`)
console.log(`Part 2: ${get(2)}`)
