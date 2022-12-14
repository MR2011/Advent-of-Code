use regex::Regex;
use std::collections::VecDeque;

type Stack = VecDeque<char>;
type Stacks = Vec<Stack>;

fn init_stacks(input: &str) -> Stacks {
    let mut stacks = vec![Stack::new(); 9];
    input.lines().for_each(|l| {
        for (i, j) in (1..l.len()).step_by(4).enumerate() {
            let c = l.chars().nth(j).unwrap_or(' ');
            if c.is_alphabetic() {
                stacks[i].push_back(c);
            }
        }
    });
    stacks
}

fn run_instructions(instructions: &str, mut stacks: Stacks, multi: bool) -> Stacks {
    let re = Regex::new(r"move (\d+) from (\d+) to (\d+)").unwrap();
    instructions.lines().for_each(|l| {
        let caps = re.captures(l).unwrap();
        let num = caps.get(1).unwrap().as_str().parse::<i32>().unwrap_or(0);
        let src = caps.get(2).unwrap().as_str().parse::<usize>().unwrap_or(0);
        let dst = caps.get(3).unwrap().as_str().parse::<usize>().unwrap_or(0);
        let mut tmp = Stack::new();
        for _ in 0..num {
            let item = stacks[src - 1].pop_front().unwrap();
            if multi {
                tmp.push_back(item);
            } else {
                stacks[dst - 1].push_front(item);
            }
        }
        if multi {
            tmp.append(&mut stacks[dst - 1]);
            stacks[dst - 1] = tmp;
        }
    });
    stacks
}

#[aoc(day5, part1)]
pub fn solve_part1(input: &str) -> String {
    let (stack_input, instructions) = input.split_once("\n\n").unwrap();
    let s0 = init_stacks(stack_input);
    let mut sn = run_instructions(instructions, s0, false);
    let mut result = String::with_capacity(sn.len());
    for i in 0..9 {
        result.push(sn[i].pop_front().unwrap());
    }
    result
}

#[aoc(day5, part2)]
pub fn solve_part2(input: &str) -> String {
    let (stack_input, instructions) = input.split_once("\n\n").unwrap();
    let s0 = init_stacks(stack_input);
    let mut sn = run_instructions(instructions, s0, true);
    let mut result = String::with_capacity(sn.len());
    for i in 0..9 {
        result.push(sn[i].pop_front().unwrap());
    }
    result
}
