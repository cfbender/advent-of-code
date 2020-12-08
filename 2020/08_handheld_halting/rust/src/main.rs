use std::{collections::HashSet, convert::TryInto, fs};

#[derive(Debug, Clone)]
struct Instruction {
    ins_type: String,
    amount: i32,
}

fn get_input() -> Vec<Instruction> {
    fs::read_to_string("../input.txt")
        .expect("Something went wrong reading the input")
        .lines()
        .map(|line| Instruction {
            ins_type: String::from(&line[..3]),
            amount: line[4..].parse().unwrap(),
        })
        .collect::<Vec<Instruction>>()
}

fn add(u: usize, i: i32) -> usize {
    if i.is_negative() {
        u.wrapping_sub(i.wrapping_abs() as usize)
    } else {
        u.wrapping_add(i as usize)
    }
}

fn fix_program(input: &Vec<Instruction>) -> Vec<Instruction> {
    let mut idx = 0;
    input.iter().fold(Vec::new(), |acc, curr| {
        if idx >= input.len() {
            return acc;
        }
        let mut clone = input.clone();
        let new_type: String = match curr.ins_type.as_str() {
            "jmp" => String::from("nop"),
            "nop" => String::from("jmp"),
            _ => curr.ins_type.clone(),
        };

        clone.splice(
            idx..idx + 1,
            vec![Instruction {
                ins_type: new_type,
                amount: curr.amount,
            }],
        );
        let result = run_program(&clone);

        idx = idx + 1;
        if let Ok(_x) = result {
            clone
        } else {
            acc
        }
    })
}

fn run_program(input: &Vec<Instruction>) -> Result<usize, usize> {
    let mut checked: HashSet<usize> = HashSet::new();
    let mut i = 0;
    let mut acc: i32 = 0;
    let mut terminated = false;

    input.iter().try_for_each(|_x| -> Option<()> {
        checked.insert(i);

        if i >= input.len() {
            return None;
        }

        let current = input.get(i).unwrap();

        match current.ins_type.as_str() {
            "jmp" => {
                i = add(i, current.amount);
            }
            "acc" => {
                acc = acc + current.amount;
                i = i + 1;
            }
             _ => {
                i = i + 1;
            }
        }

        if checked.contains(&i) {
            terminated = true;
            None
        } else {
            Some(())
        }
    });

    match terminated {
        false => Ok(acc.try_into().unwrap()),
        true => Err(acc.try_into().unwrap()),
    }
}

fn main() {
    let input = get_input();

    let result = run_program(&input);
    println!("Part 1: {:?}", result);
    let fix = fix_program(&input);
    println!("Part 2: {:?}", run_program(&fix));
}
