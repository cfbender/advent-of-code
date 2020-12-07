use std::fs;
// this solution is really slow for some reason
fn get_input() -> Vec<String> {
    fs::read_to_string("../input.txt")
        .expect("Something went wrong reading the input")
        .lines()
        .map(|line| String::from(line))
        .collect()
}

fn get_bag_color(bag: &str) -> String {
    let trimmed = bag
        .replace(" bags", "")
        .replace(" bag", "")
        .replace(".", "");

    String::from(&trimmed[2..])
}

fn get_parent_bags(input: &Vec<String>, child: &str) -> Vec<String> {
    let mut parents: Vec<String> = input
        .iter()
        .map(|rule| {
            rule.split("contain")
                .map(|x| String::from(x.replace("bags", "").trim()))
        })
        .filter(|data| data.clone().nth(1).unwrap().contains(&child))
        .map(|data| String::from(data.into_iter().nth(0).unwrap()))
        .collect();
    let mut results: Vec<String> = parents
        .iter()
        .flat_map(|bag| get_parent_bags(input, bag))
        .collect();
    parents.append(&mut results);
    parents.sort_unstable();
    parents.dedup();
    parents
}

fn get_child_bag_count(input: &Vec<String>, parent: &str) -> usize {
    let children: Vec<&str> = input
        .iter()
        .filter(|rule| rule.starts_with(parent))
        .nth(0)
        .unwrap()
        .split("contain")
        .map(|x| x.split(",").map(|y| y.trim()))
        .nth(1)
        .unwrap()
        .collect();

    if children.get(0).unwrap() == &"no other bags." {
        return 0;
    } else {
        return children
            .iter()
            .map(|child| {
                let count: usize = child.chars().nth(0).unwrap().to_digit(10).unwrap() as usize;
                let color = get_bag_color(child);
                count + count * get_child_bag_count(input, &color)
            })
            .sum();
    }
}

fn main() {
    let input = get_input();
    println!("Part 1:{:?}", get_parent_bags(&input, "shiny gold").len());
    println!("Part 2:{:?}", get_child_bag_count(&input, "shiny gold"));
}
