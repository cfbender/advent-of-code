use regex::Regex;
use std::fs;

fn get_input() -> Vec<String> {
    fs::read_to_string("input.txt")
        .expect("Something went wrong reading the input")
        .lines()
        .into_iter()
        .map(|line| String::from(line))
        .collect()
}

#[derive(Debug)]
struct Password<'a> {
    low: usize,
    high: usize,
    letter: &'a str,
    password: &'a str,
}

fn main() {
    let passwords = get_input();

    let password_regex = Regex::new(r"(\d+)-(\d+) (\w): (\w+)").unwrap();
    let password_info: Vec<Password> = passwords
        .iter()
        .map(|info| {
            let captures = password_regex.captures(&info).unwrap();

            let low: usize = captures.get(1).unwrap().as_str().parse().unwrap();
            let high: usize = captures.get(2).unwrap().as_str().parse().unwrap();
            let letter: &str = captures.get(3).unwrap().as_str();
            let password: &str = captures.get(4).unwrap().as_str();

            Password {
                low,
                high,
                letter,
                password,
            }
        })
        .collect();

    let old_valid_passwords = password_info
        .iter()
        .filter(
            |Password {
                 low,
                 high,
                 letter,
                 password,
             }| {
                let count: usize = password.matches(letter).count();

                low <= &count && &count <= high
            },
        )
        .count();

    println!("Old System Valid passwords: {:?}", old_valid_passwords);

    let valid_passwords = password_info
        .iter()
        .filter(
            |Password {
                 low,
                 high,
                 letter,
                 password,
             }| {
                let low_char = password.chars().nth(*low - 1);
                let high_char = password.chars().nth(*high - 1);

                if low_char.is_some() && high_char.is_some() {
                    (low_char.unwrap().to_string() == *letter
                        && high_char.unwrap().to_string() != *letter) || 
                    (low_char.unwrap().to_string() != *letter
                        && high_char.unwrap().to_string() == *letter) 
                } else {
                    false
                }
            },
        )
        .count();

    println!("Valid passwords: {:?}", valid_passwords);
}
