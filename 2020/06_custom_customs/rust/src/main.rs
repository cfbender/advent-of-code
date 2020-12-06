use std::fs;

fn get_input() -> Vec<String> {
    fs::read_to_string("../input.txt")
        .expect("Something went wrong reading the input")
        .lines()
        .into_iter()
        .map(|line| {
            if line == "" {
                String::from("_$")
            } else {
                format!(" {}", String::from(line))
            }
        })
        .collect::<String>()
        .split("_$")
        .map(|s| String::from(s.trim()))
        .collect()
}

fn get_answers(input: &String) -> usize {
    input
        .chars()
        .fold(Vec::new(), |mut acc, curr| {
            if !acc.contains(&curr) && curr != ' ' {
                acc.push(curr)
            }
            acc
        })
        .len()
}

fn get_group_answers(answers: &String) -> usize {
    let individuals: Vec<&str> = answers.split(" ").collect();

    individuals[0]
        .chars()
        .fold(Vec::new(), |mut acc, curr| {
            let in_others = individuals.clone().drain(1..).all(|x| x.contains(curr));
            if !acc.contains(&curr) && curr != ' ' && in_others {
                acc.push(curr)
            }
            acc
        })
        .len()
}

fn main() {
    let input = get_input();
    println!(
        "Part 1: {:?}",
        input.iter().map(|x| get_answers(x)).sum::<usize>()
    );

    println!(
        "Part 2: {:?}",
        input.iter().map(|x| get_group_answers(x)).sum::<usize>()
    );
}
