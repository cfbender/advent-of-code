const input = await Deno.readTextFile('../input.txt')
const rules: string[] = input.split(/\r?\n/)

const getBagColor = (s: string) => {
  const matches = s.match(/\d (\w* \w*)/)
  if (matches) {
    return matches[1]
  } else return ''
}

const getParentBags = (endBag = 'shiny gold'): string[] => {
  const parents = rules
    .map((rule) =>
      rule.split('contain').map((x) => x.replace(/bag(s)?/, '').trim())
    )
    .filter((data) => data[1].includes(endBag))
    .map((data) => data[0])

  return [
    ...parents,
    ...parents.flatMap((parentBag) => getParentBags(parentBag)),
  ]
}

const getChildBagCount = (parentBag = 'shiny gold'): number => {
  const children = rules
    .filter((rule) => rule.startsWith(parentBag))[0]
    .split('contain')
    .map((x) => x.split(',').map((y) => y.trim()))[1]

  if (children[0] === 'no other bags.') {
    return 0
  }

  return children
    .map((child) => {
      const childCount = parseInt(child[0])
      const color = getBagColor(child)
      return childCount + childCount * getChildBagCount(color)
    })
    .reduce((acc, curr) => acc + curr)
}

console.log(`Part 1: ${Array.from(new Set(getParentBags())).length}`)
console.log(`Part 2: ${getChildBagCount()}`)
