//THIS CODE IS NOT WORKING. See print statements on run for negative number behavior that I
//couldn't fix. the jmp/-93 instruction should subtract 93 from the index, but instead it adds 1.
//It logs out as being negative at runtime, but doesn't pass that "if" branch when put into the
//"add" function
use std::{collections::HashSet, convert::TryInto, fs};

#[derive(Debug)]
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
    if i < 0 {
        let result = u.overflowing_sub(i.wrapping_abs() as u32 as usize);
        if result.1 == true {
            0
        } else {
            result.0
        }
    } else {
        u.overflowing_add(i as usize).0
    }
}

fn run_program(input: &Vec<Instruction>) -> Result<usize, usize> {
    let mut checked: HashSet<usize> = HashSet::new();
    let mut i = 0;
    let mut acc = 0;
    let mut terminated = false;
    input.iter().try_for_each(|curr| -> Option<()> {
        checked.insert(i);

        println!("{}", i);
        if i >= input.len() {
            terminated = true;
            return None;
        }

        let current = input.iter().nth(i).unwrap();

        println!("{:?}", current);
        match current.ins_type.as_str() {
            "jmp" => {
                i = add(i, curr.amount);
            }
            "acc" => {
                acc = acc + curr.amount;
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
    println!("{:?}", result)
}
