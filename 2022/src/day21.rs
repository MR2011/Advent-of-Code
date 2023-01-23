use regex::Regex;
use std::collections::HashMap;

#[derive(Debug)]
enum Operation {
    Value(i64),
    Add(String, String),
    Subtract(String, String),
    Multiply(String, String),
    Divide(String, String),
    Equal(String, String),
}

impl Operation {
    fn from_str(s: &str) -> Operation {
        let tokens: Vec<&str> = s.split_whitespace().collect();
        if tokens.len() == 1 {
            let value = tokens[0].parse::<i64>().unwrap();
            Operation::Value(value)
        } else {
            let left = tokens[0].to_string();
            let right = tokens[2].to_string();
            match tokens[1] {
                "+" => Operation::Add(left, right),
                "-" => Operation::Subtract(left, right),
                "*" => Operation::Multiply(left, right),
                "/" => Operation::Divide(left, right),
                "=" => Operation::Equal(left, right),
                _ => panic!("invalid operation"),
            }
        }
    }
}

fn parse_line(line: &str, equal: bool) -> (String, Operation) {
    let mut tokens: Vec<&str> = line.split(": ").collect();
    let name = tokens[0].to_string();
    let operation;
    if name == "root" && equal {
        let re = Regex::new(r"\+|-|/|\*").unwrap();
        operation = re.replace_all(tokens[1], "=").to_string();
    } else {
        operation = tokens[1].to_string();
    }
    (name, Operation::from_str(&operation))
}

fn parse_input(input: &str, equal: bool) -> HashMap<String, Operation> {
    input.lines().map(|l| parse_line(l, equal)).collect()
}

fn resolve(monkey: &String, monkeys: &HashMap<String, Operation>) -> i64 {
    match monkeys.get(monkey).unwrap() {
        Operation::Value(v) => *v,
        Operation::Add(l, r) => resolve(l, monkeys) + resolve(r, monkeys),
        Operation::Subtract(l, r) => resolve(l, monkeys) - resolve(r, monkeys),
        Operation::Multiply(l, r) => resolve(l, monkeys) * resolve(r, monkeys),
        Operation::Divide(l, r) => resolve(l, monkeys) / resolve(r, monkeys),
        Operation::Equal(l, r) => {
            if resolve(l, monkeys) == resolve(r, monkeys) {
                1
            } else {
                0
            }
        }
    }
}

fn depends_on_humn(monkey: &String, monkeys: &HashMap<String, Operation>) -> bool {
    if monkey == "humn" {
        return true;
    }

    match monkeys.get(monkey).unwrap() {
        Operation::Value(v) => false,
        Operation::Add(l, r)
        | Operation::Subtract(l, r)
        | Operation::Multiply(l, r)
        | Operation::Divide(l, r)
        | Operation::Equal(l, r) => depends_on_humn(l, monkeys) || depends_on_humn(r, monkeys),
    }
}

fn resolve_humn(monkey: &String, monkeys: &HashMap<String, Operation>, target: i64) -> i64 {
    if monkey == "humn" {
        return target;
    }
    match monkeys.get(monkey).unwrap() {
        Operation::Value(v) => *v,
        Operation::Add(l, r) => {
            if depends_on_humn(l, &monkeys) {
                let value = resolve(r, monkeys);
                let next_target = target - value;
                return resolve_humn(l, &monkeys, next_target);
            } else {
                let value = resolve(l, monkeys);
                let next_target = target - value;
                return resolve_humn(r, &monkeys, next_target);
            }
        }
        Operation::Subtract(l, r) => {
            if depends_on_humn(l, &monkeys) {
                let value = resolve(r, monkeys);
                let next_target = target + value;
                return resolve_humn(l, &monkeys, next_target);
            } else {
                let value = resolve(l, monkeys);
                let next_target = (target - value) * -1;
                return resolve_humn(r, &monkeys, next_target);
            }
        }

        Operation::Multiply(l, r) => {
            if depends_on_humn(l, &monkeys) {
                let value = resolve(r, monkeys);
                let next_target = target / value;
                return resolve_humn(l, &monkeys, next_target);
            } else {
                let value = resolve(l, monkeys);
                let next_target = target / value;
                return resolve_humn(r, &monkeys, next_target);
            }
        }
        Operation::Divide(l, r) => {
            if depends_on_humn(l, &monkeys) {
                let value = resolve(r, monkeys);
                let next_target = target * value;
                return resolve_humn(l, &monkeys, next_target);
            } else {
                let value = resolve(l, monkeys);
                let next_target = target / value;
                return resolve_humn(r, &monkeys, next_target);
            }
        }
        Operation::Equal(l, r) => {
            if depends_on_humn(l, &monkeys) {
                let value = resolve(r, monkeys);
                return resolve_humn(l, &monkeys, value);
            } else {
                let value = resolve(l, monkeys);
                return resolve_humn(r, &monkeys, value);
            }
        }
    };
    0
}

#[aoc(day21, part1)]
pub fn solve_part1(input: &str) -> i64 {
    let monkeys = parse_input(input, false);
    resolve(&"root".to_string(), &monkeys)
}

#[aoc(day21, part2)]
pub fn solve_part2(input: &str) -> i64 {
    let mut monkeys = parse_input(input, true);
    resolve_humn(&"root".to_string(), &monkeys, 0)
}
