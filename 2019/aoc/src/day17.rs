use crate::intcode::VM;
use crate::intcode::State;

struct Map {
    grid: Vec<Vec<char>>,
}

impl Map {
    pub fn new() -> Map {
        let mut grid = Vec::new();
        grid.push(Vec::new());
        Map {
            grid: grid,
        }
    }

    pub fn insert(&mut self, v: i64) {
        if v == 10 {
            self.grid.push(Vec::new());
        } else {
            let mut row = self.grid.last_mut().unwrap();
            row.push(std::char::from_u32(v as u32).unwrap());
        }
    }

    pub fn get(&self, x: usize, y: usize) -> Option<char> {
        if y < self.grid.len() && x < self.grid[y].len() {
            return Some(self.grid[y][x]);
        } else {
            return None; 
        }
    }

    fn is_intersection(&self, x: usize, y: usize) -> bool {
        self.get(x-1, y) == Some('#') &&
        self.get(x+1, y) == Some('#') && 
        self.get(x, y+1) == Some('#') && 
        self.get(x, y-1) == Some('#')
    }

    pub fn alignment_parameters(self) -> usize {
        let mut sum = 0;
        for (ri, row) in self.grid.iter().enumerate() {
            for (ci, col) in row.iter().enumerate() {
                if *col == '#' {
                    if self.is_intersection(ci, ri) {
                        sum += (ri * ci);
                    }
                }
            }
        }
        sum
    }

    pub fn print(&self) {
        for row in self.grid.iter() {
            for col in row.iter() {
                print!("{}", col);
            }
            println!("");
        }
    }
}

fn feed_input(vm: &mut VM, input: &str) {
    for c in input.chars() {
        vm.feed_input(c as i64);
    }
}

#[aoc_generator(day17)]
pub fn prepare_input(input: &str) -> Vec<i64> {
    let tokens: Vec<i64> = input.trim().split(",")
        .map(|token| token.parse::<i64>().unwrap())
        .collect();
    tokens
}

#[aoc(day17, part1)]
pub fn solve_part1(input: &Vec<i64>) -> usize {
    let mut vm = VM::new(input.clone(), vec![]);
    let mut map = Map::new();
    while vm.run_until_new_state() != State::Halted {;
        let output = *vm.output.last().unwrap();
        map.insert(output);
    }
    map.alignment_parameters()
}

#[aoc(day17, part2)]
pub fn solve_part2(input: &Vec<i64>) -> i64 {
    let mut vm = VM::new(input.clone(), vec![]);
    vm.set(0, 2);
    // Solved by hand
    let main = "A,B,A,B,C,C,B,A,C,A\n";
    let a = "L,10,R,8,R,6,R,10\n";
    let b = "L,12,R,8,L,12\n";
    let c = "L,10,R,8,R,8\n";
    let video_feed = "n\n";
    feed_input(&mut vm, main);
    feed_input(&mut vm, a);
    feed_input(&mut vm, b);
    feed_input(&mut vm, c);
    feed_input(&mut vm, video_feed);

    vm.run();
    *vm.output.last().unwrap()
}
