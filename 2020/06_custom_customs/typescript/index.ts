const input = await Deno.readTextFile('../input.txt')
const rows = input.split(/\r?\n\r?\n/).map((row) => row.replace(/\r?\n/g, ' '))
console.log(rows)

const getAnswers = (input: string): number => {
  return input.split('').reduce((acc: string[], curr: string) => {
    if (!acc.includes(curr) && curr !== ' ') {
      acc.push(curr)
    }
    return acc
  }, []).length
}
console.log(
  `Part 1: ${rows.map(getAnswers).reduce((acc, curr) => acc + curr, 0)}`
)

const getGroupAnswers = (answers: string) => {
  const [first, ...rest] = answers.split(' ')
  return first.split('').reduce((acc: string[], curr) => {
    const inOthers = rest.every((set) => set.includes(curr))
    if (!acc.includes(curr) && curr !== ' ' && inOthers) {
      acc.push(curr)
    }
    return acc
  }, []).length
}
console.log(
  `Part 2: ${rows.map(getGroupAnswers).reduce((acc, curr) => acc + curr, 0)}`
)
