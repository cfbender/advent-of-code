use std::fs;

fn get_input() -> Vec<String> {
    fs::read_to_string("../input.txt")
        .expect("Something went wrong reading the input")
        .lines()
        .map(|line| String::from(line))
        .collect()
}

fn get_seat_id(row: i32, column: i32) -> i32 {
    row * 8 + column
}

fn parse_instructions(instructions: &str, choices: i32) -> i32 {
    instructions
        .chars()
        .fold((0..choices).collect(), |acc: Vec<i32>, letter| {
            if letter == 'L' || letter == 'F' {
                acc.clone().drain(..(acc.len() / 2)).collect()
            } else {
                acc.clone().drain((acc.len() / 2)..).collect()
            }
        })
        .get(0)
        .unwrap()
        .clone()
}

fn get_seat(seat: String) -> i32 {
    get_seat_id(
        parse_instructions(&seat[..7], 128),
        parse_instructions(&seat[7..], 8),
    )
}

fn get_missing(ids: Vec<i32>, min: i32, max: i32) -> i32 {
    ((min + max) / 2) * (max - min + 1) - ids.iter().sum::<i32>()
    
}

fn main() {
    let lines = get_input();
    let max= lines
            .iter()
            .map(|seat| get_seat(seat.clone()))
            .max()
            .unwrap();

    let min= lines
            .iter()
            .map(|seat| get_seat(seat.clone()))
            .min()
            .unwrap();
            
    println!(
        "Part 1: {}",
        max
    );
    println!(
        "Part 2: {:?}",
        get_missing(lines.iter().map(|seat| get_seat(seat.clone())).collect(), min, max)
    )
}
