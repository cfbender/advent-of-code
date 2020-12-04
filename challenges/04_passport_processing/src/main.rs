use std::fs;

fn get_input() -> Vec<String> {
    fs::read_to_string("input.txt")
        .expect("Something went wrong reading the input")
        .lines()
        .into_iter()
        .map(|line| {
            if line == "" {
                String::from("_$")
            } else {
                line.split(" ").collect::<Vec<&str>>().join(",") + ","
            }
        })
        .collect::<String>()
        .split("_$")
        .map(|data| String::from(data))
        .collect()
}

#[derive(Debug)]
struct Passport {
    byr: Option<String>,
    iyr: Option<String>,
    eyr: Option<String>,
    hgt: Option<String>,
    hcl: Option<String>,
    ecl: Option<String>,
    pid: Option<String>,
    cid: Option<String>,
}

impl Default for Passport {
    fn default() -> Passport {
        Passport {
            byr: None,
            iyr: None,
            eyr: None,
            hgt: None,
            hcl: None,
            ecl: None,
            pid: None,
            cid: None,
        }
    }
}

impl Passport {
    fn is_valid(&self) -> bool {
        vec![
            self.byr.clone().map_or(false, |byr| {
                (1920..2003).contains(&byr.parse::<i32>().unwrap_or(0))
            }),
            self.iyr.clone().map_or(false, |iyr| {
                (2010..2021).contains(&iyr.parse::<i32>().unwrap_or(0))
            }),
            self.eyr.clone().map_or(false, |eyr| {
                (2020..2031).contains(&eyr.parse::<i32>().unwrap_or(0))
            }),
            self.hgt.clone().map_or(false, |hgt| {
                let unit = &hgt[hgt.len() - 2..];
                match unit {
                    "cm" => (150..194).contains(&hgt[..3].parse::<i32>().unwrap_or(0)),
                    "in" => (59..77).contains(&hgt[..2].parse::<i32>().unwrap_or(0)),
                    _ => false,
                }
            }),
            self.hcl.clone().map_or(false, |hcl| {
                hcl.starts_with('#')
                    && hcl.len() == 7
                    && hcl[1..].chars().all(|x| x.is_alphanumeric())
            }),
            self.ecl.clone().map_or(false, |ecl| {
                vec![
                    String::from("amb"),
                    String::from("blu"),
                    String::from("brn"),
                    String::from("gry"),
                    String::from("grn"),
                    String::from("hzl"),
                    String::from("oth"),
                ]
                .contains(&ecl)
            }),
            self.pid.clone().map_or(false, |pid| {
                pid.len() == 9 && pid.parse::<i32>().unwrap_or(0) != 0
            }),
        ]
        .iter()
        .all(|result| *result)
    }
}

fn main() {
    let input = get_input();
    let get_pasport_prop = |prop: &str| Some(prop[4..].parse().unwrap());
    let valid_passports: Vec<Passport> = input
        .iter()
        .map(|data| {
            let mut passport = Passport::default();
            data.split(",").for_each(|prop| {
                if prop.len() > 0 {
                    let prop_value = get_pasport_prop(prop);
                    match prop.get(..3) {
                        Some("byr") => passport.byr = prop_value,
                        Some("iyr") => passport.iyr = prop_value,
                        Some("eyr") => passport.eyr = prop_value,
                        Some("hgt") => passport.hgt = prop_value,
                        Some("hcl") => passport.hcl = prop_value,
                        Some("ecl") => passport.ecl = prop_value,
                        Some("pid") => passport.pid = prop_value,
                        Some("cid") => passport.cid = prop_value,
                        _ => {}
                    }
                }
            });
            passport
        })
        .filter(|passport| passport.is_valid())
        .collect();
    println!("Valid passports: {:?}", valid_passports.len());
}
