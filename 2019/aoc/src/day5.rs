use crate::intcode::VM;

#[aoc_generator(day5)]
pub fn prepare_input(input: &str) -> Vec<i64> {
    let tokens: Vec<i64> = input.trim().split(",")
        .map(|token| token.parse::<i64>().unwrap())
        .collect();
    tokens
}

#[aoc(day5, part1)]
pub fn solve_part1(input: &Vec<i64>) -> i64 {
    let mut read_input = Vec::new();
    read_input.push(1);
    let mut vm = VM::new(input.clone(), read_input);
    vm.run();
    vm.output.pop().unwrap()
}

#[aoc(day5, part2)]
pub fn solve_part2(input: &Vec<i64>) -> i64 {
    let mut read_input = Vec::new();
    read_input.push(5);
    let mut vm = VM::new(input.clone(), read_input);
    vm.run();
    vm.output.pop().unwrap()
}
