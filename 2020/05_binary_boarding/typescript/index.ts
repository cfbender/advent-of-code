const input = await Deno.readTextFile('../input.txt')
const seats = input.split(/\r?\n/)

const getSeatID = (row: number, column: number) => row * 8 + column

const parseInstructions = ({
  ins,
  type,
}: {
  ins: string,
  type: 'column' | 'row',
}) => {
  const rows = 128
  const columns = 8

  const possibilities = Array.from(
    Array(type === 'column' ? columns : rows).keys()
  )
  return ins.split('').reduce((acc, letter) => {
    return letter === 'L' || letter === 'F'
      ? acc.slice(0, acc.length / 2)
      : acc.slice(acc.length / 2)
  }, possibilities)
}

const getSeat = (seat: string) => {
  const rowInstructions = seat.substring(0, 7)
  const columnInstructions = seat.substring(7)

  const [row] = parseInstructions({ ins: rowInstructions, type: 'row' })
  const [column] = parseInstructions({
    ins: columnInstructions,
    type: 'column',
  })

  return getSeatID(row, column)
}

const findMissingSeat = (seatIDs: number[], min: number, max: number) =>
  ((min + max) / 2) * (max - min + 1) -
  seatIDs.reduce((acc, curr) => acc + curr)

const max = seats.map(getSeat).reduce((a, b) => Math.max(a, b))
const min = seats
  .map(getSeat)
  .filter((x) => x !== 0)
  .reduce((a, b) => Math.min(a, b))
console.log(`Part 1: ${seats.map(getSeat).reduce((a, b) => Math.max(a, b))}`)
console.log(`Part 2: ${findMissingSeat(seats.map(getSeat), min, max)}`)
