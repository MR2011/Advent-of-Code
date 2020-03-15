use crate::intcode::VM;
use crate::intcode::State;

fn generate_input(x: char, y: char, instruction: &str, vm: &mut VM) {
    for c in instruction.chars() {
        vm.feed_input(c as i64);
    }
    if x != '\0' && y != '\0' {
        vm.feed_input(32); // Space
        vm.feed_input(x as i64);
        vm.feed_input(32); // Space
        vm.feed_input(y as i64);
    }
    vm.feed_input(10); // Newline
}

fn display(vm: &mut VM) {
    while vm.run_until_new_state() == State::NewOutput {
        print!("{}", *vm.output.last().unwrap() as u8 as char);
    }
}

#[aoc_generator(day21)]
pub fn prepare_input(input: &str) -> Vec<i64> {
    let tokens: Vec<i64> = input.trim().split(",")
        .map(|token| token.parse::<i64>().unwrap())
        .collect();
    tokens
}

#[aoc(day21, part1)]
pub fn solve_part1(input: &Vec<i64>) -> i64 {
    let mut vm = VM::new(input.clone(), vec![]);
    display(&mut vm);
    // (!A or !C) and D
    generate_input('A', 'T', "NOT", &mut vm);
    generate_input('C', 'J', "NOT", &mut vm);
    generate_input('T', 'J', "OR", &mut vm);
    generate_input('D', 'J', "AND", &mut vm);
    generate_input('\0', '\0', "WALK", &mut vm);
    display(&mut vm);
    display(&mut vm);
    vm.run();
    *vm.output.last().unwrap()
}

#[aoc(day21, part2)]
pub fn solve_part2(input: &Vec<i64>) -> i64 {
    let mut vm = VM::new(input.clone(), vec![]);
    display(&mut vm);
    // copied from reddit :)
    generate_input('B', 'J', "OR", &mut vm);
    generate_input('C', 'J', "AND", &mut vm);
    generate_input('J', 'J', "NOT", &mut vm);
    generate_input('D', 'J', "AND", &mut vm);
    generate_input('H', 'J', "AND", &mut vm);
    generate_input('A', 'T', "NOT", &mut vm);
    generate_input('T', 'J', "OR", &mut vm);
    generate_input('\0', '\0', "RUN", &mut vm);
    display(&mut vm);
    display(&mut vm);
    vm.run();
    *vm.output.last().unwrap()
}

