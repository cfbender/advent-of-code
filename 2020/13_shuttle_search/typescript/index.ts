const input = await Deno.readTextFile('../input.txt')
const [timeStr, idList] = input.split(/\r?\n/)

const time = parseInt(timeStr)
const ids = idList.split(',').map((x) => parseInt(x))

let curr = 0
let checkTime = time
while (!Number.isInteger(checkTime / ids[curr])) {
  curr++
  if (curr === ids.length) {
    curr %= ids.length
    checkTime++
  }
}

console.log(`Part 1: ${(checkTime - time) * ids[curr]}`)

const getEarliest = () => {
  let curr = 0
  const buses = ids.flatMap((id, idx) => (Number.isNaN(id) ? [] : [[id, idx]]))
  let step = 1

  buses.forEach(([id, idx]) => {
    while ((curr + idx) % id !== 0) {
      curr += step
    }
    step *= id
  })

  return curr
}

console.log(getEarliest())
