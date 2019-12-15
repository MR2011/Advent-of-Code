use std::fs::read_to_string;
use std::iter::successors;

fn calc_total_fuel(mass: &u32) -> u32 {
    successors(Some(*mass), |m| (m/3).checked_sub(2)).skip(1).sum()
}

fn parse_line(line: &str) -> u32 {
    line.parse().unwrap()
}

fn main() {
    let input = read_to_string("input").expect("File not found!");
    let modules: Vec<u32> = input.lines().map(parse_line).collect();
    let fuel_requirements: u32 = modules.iter().map(|m| m/3 - 2).sum();
    let total_fuel_requirements: u32 = modules.iter().map(calc_total_fuel).sum();
    println!("Part 1: {}\nPart 2: {}", fuel_requirements, total_fuel_requirements);
}
