const input = await Deno.readTextFile('../input.txt')

type Direction = 'N' | 'E' | 'W' | 'S'
type Instruction = [string, number]

const nav: Instruction[] = input
  .trim()
  .split(/\r?\n/)
  .map((x) => [x[0], parseInt(x.substr(1))])

const testData: Instruction[] = `F10
N3
F7
R90
F11`
  .split('\n')
  .map((x) => [x[0], parseInt(x.substr(1))])

interface Position {
  long: number;
  lat: number;
  facing?: number;
}

const cardinalDegrees: Record<number, Direction> = {
  0: 'N',
  90: 'E',
  180: 'S',
  270: 'W',
}

const getManhattanDistance = (pos: Position) =>
  Math.abs(pos.long) + Math.abs(pos.lat)

const getMovement = ([type, amount]: Instruction, pos: Position): Position => {
  if (!pos.facing) {
    pos.facing = 0
  }
  if (type === 'F') {
    type = cardinalDegrees[pos.facing]
  }
  if ((type === 'L' || type === 'R') && ![0, 90, 180, 270].includes(amount)) {
    console.log(`Shit is fucked: ${type}, ${amount}`)
  }

  switch (type) {
    case 'N':
    case 'S':
      return { ...pos, lat: pos.lat + (type === 'N' ? amount : -amount) }

    case 'E':
    case 'W':
      return { ...pos, long: pos.long + (type === 'E' ? amount : -amount) }

    default: {
      let result = (pos.facing + (type === 'R' ? amount : 360 - amount)) % 360
      return {
        ...pos,
        facing: result,
      }
    }
  }
}

const getPos = (ins: Instruction[]) => {
  return ins.reduce((acc, curr) => getMovement(curr, acc), {
    long: 0,
    lat: 0,
    facing: 90,
  } as Position)
}

type ShipWithWaypoint = [Position, Position]

const transformPoint = (pos: Position, degrees: number) => {
    const radians = degrees * (Math.PI / 180)
    const long = Math.round((pos.long * Math.cos(radians)) + (pos.lat * Math.sin(radians)))
    const lat= Math.round((pos.lat * Math.cos(radians)) - (pos.long * Math.sin(radians)))
    return {long, lat}
}
const getWaypointMovement = (
  [type, amount]: Instruction,
  [shipPos, wayPos]: ShipWithWaypoint
): ShipWithWaypoint => {
  switch (type) {
    case 'F':
      return [{ lat: shipPos.lat + wayPos.lat * amount, long: shipPos.long + wayPos.long * amount }, wayPos]
    case 'R':
    case'L':
        return [shipPos, {...transformPoint(wayPos,type === 'R' ? amount : 360 - amount )} ]
    default:
      return [shipPos, getMovement([type, amount], wayPos)]
  }
}
const getPosWaypoint = (ins: Instruction[]) => {
  return ins.reduce(
    (acc, curr) => {
      return getWaypointMovement(curr, acc)
    },
    [
      { long: 0, lat: 0 },
      { long: 10, lat: 1 },
    ] as ShipWithWaypoint
  )
}

const result = getPos(nav)
console.log(`Part 1: ${getManhattanDistance(result)}`)

const result2= getPosWaypoint(nav)
console.log(`Part2: ${getManhattanDistance(result2[0])}`)
