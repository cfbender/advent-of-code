use std::fs;

fn get_input() -> Vec<usize> {
    fs::read_to_string("input.txt")
        .expect("Something went wrong reading the input")
        .lines()
        .into_iter()
        .map(|line| line.parse::<usize>().unwrap())
        .collect()
}

fn multiply_pairs(input: &Vec<usize>, limit: usize) -> usize {
    input.iter().fold(1, |acc, &curr| {
        if curr < limit && input.contains(&(limit - &curr)) {
            acc * curr
        } else {
            acc
        }
    })
}

fn main() {
    let input = get_input();
    let pair_result = multiply_pairs(&input, 2020);

    let triplet_result = input.iter().fold(1, |acc, &curr| {
        if curr < 2020 {
            let difference = 2020 - curr;

            let pair_quotient = multiply_pairs(&input, difference);
            if pair_quotient > 1 {
                curr * pair_quotient
            } else {
                acc
            }
        } else {
            acc
        }
    });

    println!("Pair result: {:?}", pair_result);
    println!("Triplet result: {:?}", triplet_result);
}
