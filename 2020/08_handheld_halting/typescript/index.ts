const file = await Deno.readTextFile('../input.txt')
const instructions: string[] = file.trim().split(/\r?\n/)

const parseInstruction = (x: string): [string, number] => [
  x.substring(0, 3),
  parseInt(x.substring(4)),
]

type Instruction = [string, number]

const parsedInstructions: Instruction[] = instructions.map((x: string) =>
  parseInstruction(x)
)

const processInstructions = (input: Instruction[]) => {
  let currentIdx = 0
  let acc = 0
  let looped = false
  let finished = false

  const checkedIndices: Record<number, boolean> = {}

  while (!looped && !finished) {
    const [type, amount] = input[currentIdx]
    checkedIndices[currentIdx] = true

    console.log(currentIdx)
    switch (type) {
      case 'nop':
        currentIdx++
        break
      case 'jmp':
        currentIdx += amount
        break
      case 'acc':
        acc += amount
        currentIdx++
        break
    }

    if (currentIdx >= input.length) {
      finished = true
    }
    if (checkedIndices[currentIdx]) {
      looped = true
    }
  }

  return [acc, finished]
}

const fixInput = (input: Instruction[]) => {
  let correctInstructions = [...input]
  let foundFix = false

  input.forEach(([type, amount], idx, arr) => {
    if (type === 'acc' || foundFix) {
      return
    }
    const newType = type === 'jmp' ? 'nop' : 'jmp'
    const arrCopy = [...arr]
    arrCopy.splice(idx, 1, [newType, amount])
    const [_, finished] = processInstructions(arrCopy)
    if (finished) {
      correctInstructions = arrCopy
      foundFix = true
    }
  })

  return correctInstructions
}

console.log(`Part 1: ${processInstructions(parsedInstructions)[0]}`)
