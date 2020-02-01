use crate::intcode::VM;
use crate::intcode::State;
use std::cmp::Ordering;

fn move_paddle(x1: i64, x2: i64) -> i64 {
    match x1.cmp(&x2) {
        Less => -1,
        Greater => 1,
        Equal => 0,
    }
}

#[aoc_generator(day13)]
pub fn prepare_input(input: &str) -> Vec<i64> {
    let tokens: Vec<i64> = input.trim().split(",")
        .map(|token| token.parse::<i64>().unwrap())
        .collect();
    tokens
}

#[aoc(day13, part1)]
pub fn solve_part1(input: &Vec<i64>) -> usize {
    let mut vm = VM::new(input.clone(), vec![]);
    let state = vm.run();
    let count: usize = vm.output.chunks(3)
                    .map(|c| (c[0], c[1], c[2]))
                    .filter(|t| t.2 == 2)
                    .count();
    count
}

#[aoc(day13, part2)]
pub fn solve_part2(input: &Vec<i64>) -> i64 {
    let mut vm = VM::new(input.clone(), vec![]);
    vm.set(0, 2);
    let mut output = Vec::new();
    let mut ball = (0, 0);
    let mut paddle = (0, 0);
    let mut score = 0;
    loop {
        match vm.run_until_new_state() {
            State::NewOutput => output.push(*vm.output.last().unwrap()), 
            State::NeedInput => vm.feed_input(move_paddle(ball.0, paddle.0)),
            State::Halted => break,
            _ => continue,
        }

        if output.len() == 3 {
            if output[0] == -1 && output[1] == 0 {
                score = output[2];
            }else if output[2] == 4 {
                ball = (output[0], output[1]);
            }else if output[2] == 3 {
                paddle = (output[0], output[1]);
            }
            output = Vec::new();
        }
    }
    score
}
