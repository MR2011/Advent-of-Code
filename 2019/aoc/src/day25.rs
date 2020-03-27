use crate::intcode::VM;
use crate::intcode::State;
use std::io;

fn generate_input(instruction: &str, vm: &mut VM) {
    for c in instruction.chars() {
        vm.feed_input(c as i64);
    }
    vm.feed_input(10); // Newline
}

#[aoc_generator(day25)]
pub fn prepare_input(input: &str) -> Vec<i64> {
    let tokens: Vec<i64> = input.trim().split(",")
        .map(|token| token.parse::<i64>().unwrap())
        .collect();
    tokens
}

#[aoc(day25, part1)]
pub fn solve_part1(input: &Vec<i64>) -> i64 {
    // Items: planetoid, pointer, wreath, sand
    let mut vm = VM::new(input.clone(), Vec::new());
    let stdin = io::stdin();
    loop {
        match vm.run_until_new_state() {
            State::NewOutput => {
                print!("{}", *vm.output.last().unwrap() as u8 as char);
            },
            State::NeedInput => {
                let mut cmd = String::new();
                io::stdin().read_line(&mut cmd);
                generate_input(cmd.trim(), &mut vm);
            },
            State::Halted => {
                break
            },
            _ => (),
        }
    }
    1
}

#[aoc(day25, part2)]
pub fn solve_part2(input: &Vec<i64>) -> i64 {
    // no part 2
    1
}


