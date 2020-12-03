use std::fs;

fn get_input() -> Vec<Vec<char>> {
    fs::read_to_string("input.txt")
        .expect("Something went wrong reading the input")
        .lines()
        .into_iter()
        .map(|line| line.chars().collect())
        .collect()
}

fn get_encounters(input: &Vec<Vec<char>>, right: usize, down: usize) -> usize {
    let mut x = 0;
    let mut y = 0;

    input.iter().fold(0, |acc, curr_row| {
        let result: usize;
        if y == 0 || y % down != 0 {
            result = acc
        } else {
            result = if curr_row[x] == '#' { acc + 1 } else { acc };
        }
        if y % down == 0 {
            x = (x + right) % curr_row.len();
        }
        y = y + 1;
        result
    })
}

fn main() {
    let map = get_input();

    let encounters = vec![
        get_encounters(&map, 1, 1),
        get_encounters(&map, 3, 1),
        get_encounters(&map, 5, 1),
        get_encounters(&map, 7, 1),
        get_encounters(&map, 1, 2),
    ]
    .iter()
    .fold(1, |acc, curr| acc * curr);

    println!("{}", encounters)
}
