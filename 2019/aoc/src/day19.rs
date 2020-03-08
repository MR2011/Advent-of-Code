use crate::intcode::VM;
use crate::intcode::State;
use std::collections::HashSet;

fn is_affected(input: &Vec<i64>, x: i64, y: i64) -> bool {
    let mut vm = VM::new(input.clone(), vec![]);
    vm.feed_input(x);
    vm.feed_input(y);
    vm.run_until_new_state();
    vm.output.last() == Some(&1)
}

#[aoc_generator(day19)]
pub fn prepare_input(input: &str) -> Vec<i64> {
    let tokens: Vec<i64> = input.trim().split(",")
        .map(|token| token.parse::<i64>().unwrap())
        .collect();
    tokens
}

#[aoc(day19, part1)]
pub fn solve_part1(input: &Vec<i64>) -> i64 {
    let mut affected = 0;
    for y in 0..50 {
        for x in 0..50 {
            if is_affected(input, x, y) {
                affected += 1;
            }
        }
    }
    affected
}

#[aoc(day19, part2)]
pub fn solve_part2(input: &Vec<i64>) -> i64 {
    let mut x = 0;
    let mut y = 0;
    while !is_affected(input, x+99, y) {
       y += 1; 
       while !is_affected(input, x, y+99) {
           x += 1;
       }
    }
    x * 10000 + y
}

