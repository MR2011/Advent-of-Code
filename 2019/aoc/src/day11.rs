use crate::intcode::VM;
use crate::intcode::State;
use std::collections::BTreeMap;

#[derive(Copy, Clone)]
enum Color {
    Black,
    White,
}
impl Color {
    fn from_int(input: i64) -> Color {
        match input {
            0 => Color::Black,
            1 => Color::White,
            _ => panic!("Unknown Color!"),
        }
    }

    fn to_int(input: Color) -> i64 {
        match input {
            Color::Black => 0,
            Color::White => 1,
            _ => panic!("Unknown Color!"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
struct Coordinate {
    x: i32,
    y: i32,
}

impl Coordinate {

    fn new(x: i32, y: i32) -> Coordinate {
        Coordinate {
            x: x,
            y: y,
        }
    }

    fn step(&mut self, direction: &Direction) {
        match direction {
            Direction::Up => self.y += 1,
            Direction::Down => self.y -= 1,
            Direction::Left => self.x -= 1,
            Direction::Right => self.x += 1,
        }
    }
}

enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl Direction {
    fn turn_left(&self) -> Direction {
        match &self {
            Direction::Up => Direction::Left,
            Direction::Down => Direction::Right,
            Direction::Left => Direction::Down,
            Direction::Right => Direction::Up,
        }
    }
    
    fn turn_right(&self) -> Direction {
        match &self {
            Direction::Up => Direction::Right,
            Direction::Down => Direction::Left,
            Direction::Left => Direction::Up,
            Direction::Right => Direction::Down,
        }
    }
}

pub struct Robot {
    brain: VM,
    location: Coordinate,
    direction: Direction,
    panels: BTreeMap<Coordinate, Vec<Color>>,
}

impl Robot {

    pub fn new(program: Vec<i64>, input: i64) -> Robot {
        Robot {
            brain: VM::new(program, vec![input]),
            location: Coordinate::new(0, 0),
            direction: Direction::Up,
            panels: BTreeMap::new(),
        }
    }

    fn step(&mut self, turn: i64) {
        match turn {
            0 => self.direction = self.direction.turn_left(),
            1 => self.direction = self.direction.turn_right(),
            _ => panic!("Undefined turn direction!"),
        }
        self.location.step(&self.direction);
    }

    pub fn paint(&mut self) {
        let mut output: Vec<i64> = Vec::new();
        loop {
            match self.brain.run_until_new_state() {
                State::NewOutput => output.push(*self.brain.output.last().unwrap()), 
                State::NeedInput => self.scan_panel(),
                State::Halted => break,
                _ => continue,
            }

            if output.len() == 2 {
                self.paint_panel(output[0]);
                self.step(output[1]);
                output = Vec::new();
            }
        }
    }

    fn scan_panel(&mut self) {
        if self.panels.contains_key(&self.location) {
            let color = Color::to_int(
                *self.panels.get(&self.location).unwrap().last().unwrap()
            );
            self.brain.feed_input(color);
        } else {
            self.panels.insert(self.location, vec![Color::Black]);
            self.brain.feed_input(0);
        }
    }

    fn paint_panel(&mut self, color_code: i64) {
        let color = Color::from_int(color_code);
        if self.panels.contains_key(&self.location) {
            self.panels.get_mut(&self.location).unwrap().push(color);
        } else {
            self.panels.insert(self.location, vec![color]);
        }
    }

    pub fn print_panels(&mut self) {
        // let min = self.panels.keys().min();
        let x_min = self.panels.keys().map(|c| c.x).min().unwrap();
        let x_max = self.panels.keys().map(|c| c.x).max().unwrap();
        let y_min = self.panels.keys().map(|c| c.y).min().unwrap();
        let y_max = self.panels.keys().map(|c| c.y).max().unwrap();
        for y in (y_min..=y_max).rev() {
            for x in x_min..=x_max {
                let key = Coordinate::new(x, y);
                if self.panels.contains_key(&key) {
                    match self.panels.get(&key).unwrap().last() {
                        Some(Color::Black) => print!(" "),
                        Some(Color::White) => print!("#"),
                        _ => print!(" "),
                    }
                } else {
                    print!(" ")
                }
            }
            println!("");
        }
    }
}

#[aoc_generator(day11)]
pub fn prepare_input(input: &str) -> Vec<i64> {
    let tokens: Vec<i64> = input.trim().split(",")
        .map(|token| token.parse::<i64>().unwrap())
        .collect();
    tokens
}

#[aoc(day11, part1)]
pub fn solve_part1(input: &Vec<i64>) -> usize {
    let mut robot = Robot::new(input.clone(), 0);
    robot.paint();
    robot.panels.len()
}

#[aoc(day11, part2)]
pub fn solve_part2(input: &Vec<i64>) -> i32 {
    let mut robot = Robot::new(input.clone(), 1);
    robot.paint();
    robot.print_panels();
    1
}

#[cfg(test)]
mod tests {
    use super::*;
    
    //Part 1
    #[test]
    fn test1() {
    }
}
