const input = await Deno.readTextFile('../input.txt')
const rows = input.split(/\r?\n/)
const getEncounters = (dx: number, dy: number) => {
  return rows.filter((row, idx) =>
    // on stepped row, cycle the index back for how many times you've gone around and check if #
    idx % dy === 0 ? row[((dx * idx) / dy) % row.length] === '#' : false
  ).length
}

console.log(`Part 1: ${getEncounters(3, 1)}`)
console.log(
  `Part 2: ${[
    [1, 1],
    [3, 1],
    [5, 1],
    [7, 1],
    [1, 2],
  ]
    .map(([dx, dy]) => getEncounters(dx, dy))
    .reduce((product, x) => x * product)}`
)
