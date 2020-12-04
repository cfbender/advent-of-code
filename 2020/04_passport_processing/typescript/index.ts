const input = await Deno.readTextFile('../input.txt')
const rows = input.split(/\r?\n\r?\n/).map((row) => row.replace(/\r?\n/g, ' '))
type StringPassport = {
  byr?: string,
  iyr?: string,
  eyr?: string,
  hgt?: string,
  hcl?: string,
  ecl?: string,
  pid?: string,
}
type Passport = {
  byr?: number,
  iyr?: number,
  eyr?: number,
  hgt?: string,
  hcl?: string,
  ecl?: string,
  pid?: string,
}

type PassportTest = (data: Passport) => boolean

const parsePassport = (input: StringPassport): Passport | undefined =>
  input.byr &&
  input.iyr &&
  input.eyr &&
  input.hgt &&
  input.hcl &&
  input.ecl &&
  input.pid
    ? {
        ...input,
        byr: parseInt(input.byr),
        iyr: parseInt(input.iyr),
        eyr: parseInt(input.eyr),
      }
    : undefined

const isValidPassport = (input: StringPassport) => {
  const parsed = parsePassport(input)
  if (!parsed) {
    return false
  }
  const tests: PassportTest[] = [
    ({ byr = 0 }) => byr >= 1920 && byr <= 2002,
    ({ iyr = 0 }) => iyr >= 2010 && iyr <= 2020,
    ({ eyr = 0 }) => eyr >= 2020 && eyr <= 2030,
    ({ hgt = '' }) => {
      const match = hgt.match(/(\d+)(cm)?(in)?/)
      if (match) {
        const number = parseInt(match[1])
        const unit = match[2] || match[3]
        return unit === 'in'
          ? number >= 59 && number <= 76
          : number >= 150 && number <= 193
      } else {
        return false
      }
    },
    ({ hcl = '' }) => Boolean(hcl.match(/^#([a-f0-9]{6})$/)),
    ({ ecl = '' }) =>
      ['amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth'].includes(ecl),
    ({ pid = '' }) => Boolean(pid.match(/^\d{9}$/)),
  ]
  return tests.map((test) => test(parsed)).every((result) => result)
}

const passports: StringPassport[] = rows.map((row) =>
  JSON.parse(
    `{${row
      .replace(/(\S+):(\S+)/g, '"$1":"$2"')
      .split(' ')
      .join(',')}}`
  )
)
console.log(`Part 1: ${passports.filter((item) => parsePassport(item)).length}`)
console.log(
  `Part 2: ${passports.filter((data) => isValidPassport(data)).length}`
)
