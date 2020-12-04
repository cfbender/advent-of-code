const input = await Deno.readTextFile('../input.txt')
const passwords = input.split(/\r?\n/)
const password_regex = /(\d+)-(\d+) (\w): (\w+)/

type Password = {
  low: number,
  high: number,
  letter: string,
  password: string,
}

const passwordInfo: Password[] = passwords
  .map((password) => password.match(password_regex))
  .map((data) =>
    data
      ? {
          low: parseInt(data[1]),
          high: parseInt(data[2]),
          letter: data[3],
          password: data[4],
        }
      : { low: Infinity, high: -Infinity, letter: ' ', password: '' }
  )

console.log(
  `Part 1: ${
    passwordInfo.filter(
      ({ low, high, letter, password }) =>
        low <= password.split(letter).length - 1 &&
        password.split(letter).length - 1 <= high
    ).length
  }`
)

console.log(
  `Part 2: ${
    passwordInfo.filter(
      ({ low, high, letter, password }) =>
        (password[low - 1] === letter && password[high - 1] !== letter) ||
        (password[low - 1] !== letter && password[high - 1] === letter)
    ).length
  }`
)
