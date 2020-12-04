use std::fs;

fn get_input() -> Vec<Vec<char>> {
    fs::read_to_string("../input.txt")
        .expect("Something went wrong reading the input")
        .lines()
        .into_iter()
        .map(|line| line.chars().collect())
        .collect()
}

fn get_encounters(input: &Vec<Vec<char>>, right: usize, down: usize) -> usize {
    input
        .iter()
        .step_by(down)
        .enumerate()
        .filter(|(y, line)| line.iter().cycle().nth(right * y) == Some(&'#'))
        .count()
}

fn main() {
    let map = get_input();

    let encounters: usize = vec![(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
        .iter()
        .map(|&(right, down)| get_encounters(&map, right, down))
        .product();

    println!("{}", encounters)
}
