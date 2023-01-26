use regex::Regex;
use std::collections::HashMap;

type Grid = HashMap<Coord, Tile>;
type Coord = (usize, usize);

#[derive(Eq, PartialEq)]
enum Tile {
    Empty,
    Wall,
}

impl Tile {
    fn from_chr(s: char) -> Tile {
        match s {
            '.' => Tile::Empty,
            '#' => Tile::Wall,
            _ => panic!("invalid tile"),
        }
    }
}

enum Instruction {
    Move(usize),
    TurnLeft,
    TurnRight,
}

#[derive(Debug, Copy, Clone)]
enum Direction {
    Up,
    Right,
    Down,
    Left,
}

impl Direction {
    fn to_int(&self) -> usize {
        match self {
            Direction::Up => 0,
            Direction::Right => 1,
            Direction::Down => 2,
            Direction::Left => 3,
        }
    }

    fn from_int(i: usize) -> Direction {
        match i {
            0 => Direction::Up,
            1 => Direction::Right,
            2 => Direction::Down,
            3 => Direction::Left,
            _ => panic!("invalid int value"),
        }
    }

    fn turn_right(&self) -> Direction {
        let id = self.to_int();
        Direction::from_int((id + 1) % 4)
    }

    fn turn_left(&self) -> Direction {
        let id = self.to_int();
        Direction::from_int((id + 4 - 1) % 4)
    }
}

struct Map {
    grid: Grid,
    start: Coord,
}

impl Map {
    fn solve(&self, instructions: Vec<Instruction>, cube: bool) -> usize {
        let mut current = self.start;
        let mut dir = Direction::Right;
        for instruction in instructions {
            match instruction {
                Instruction::TurnLeft => {
                    dir = dir.turn_left();
                }
                Instruction::TurnRight => {
                    dir = dir.turn_right();
                }
                Instruction::Move(n) => {
                    if cube {
                        (current, dir) = self.step_cube(current, n, dir);
                    } else {
                        current = self.step(current, n, &dir);
                    }
                }
            }
        }
        self.compute_password(current, dir)
    }

    fn compute_password(&self, c: Coord, dir: Direction) -> usize {
        let d = match dir {
            Direction::Right => 0,
            Direction::Down => 1,
            Direction::Left => 2,
            Direction::Up => 3,
        };
        1000 * c.1 + 4 * c.0 + d
    }

    fn step(&self, mut current: Coord, steps: usize, dir: &Direction) -> Coord {
        for _ in 0..steps {
            let mut next = current;
            match dir {
                Direction::Up => {
                    next.1 -= 1;
                    if !self.grid.contains_key(&next) {
                        next.1 = self.col_end(next.0);
                    }
                }
                Direction::Down => {
                    next.1 += 1;
                    if !self.grid.contains_key(&next) {
                        next.1 = self.col_start(next.0);
                    }
                }
                Direction::Left => {
                    next.0 -= 1;
                    if !self.grid.contains_key(&next) {
                        next.0 = self.row_end(next.1);
                    }
                }
                Direction::Right => {
                    next.0 += 1;
                    if !self.grid.contains_key(&next) {
                        next.0 = self.row_start(next.1);
                    }
                }
            }
            if *self.grid.get(&next).unwrap() == Tile::Empty {
                current = next;
            } else if *self.grid.get(&next).unwrap() == Tile::Wall {
                break;
            }
        }
        current
    }

    fn col_start(&self, x: usize) -> usize {
        self.grid
            .keys()
            .filter(|(xi, _)| *xi == x)
            .map(|(_, yi)| *yi)
            .min()
            .unwrap()
    }

    fn col_end(&self, x: usize) -> usize {
        self.grid
            .keys()
            .filter(|(xi, _)| *xi == x)
            .map(|(_, yi)| *yi)
            .max()
            .unwrap()
    }

    fn row_start(&self, y: usize) -> usize {
        self.grid
            .keys()
            .filter(|(_, yi)| *yi == y)
            .map(|(xi, _)| *xi)
            .min()
            .unwrap()
    }

    fn row_end(&self, y: usize) -> usize {
        self.grid
            .keys()
            .filter(|(_, yi)| *yi == y)
            .map(|(xi, _)| *xi)
            .max()
            .unwrap()
    }
    
    // hardcoded transitions for my input
    fn step_cube(
        &self,
        mut current: Coord,
        steps: usize,
        mut dir: Direction,
    ) -> (Coord, Direction) {
        for _ in 0..steps {
            let mut next = match dir {
                Direction::Up => (current.0, current.1 - 1),
                Direction::Down => (current.0, current.1 + 1),
                Direction::Left => (current.0 - 1, current.1),
                Direction::Right => (current.0 + 1, current.1),
            };
            let mut next_dir = dir;
            if !self.grid.contains_key(&next) {
                match dir {
                    Direction::Up => match next.0 {
                        1..=50 => {
                            next = (51, next.0 + 50);
                            next_dir = Direction::Right;
                        }
                        51..=100 => {
                            next = (1, next.0 + 100);
                            next_dir = Direction::Right;
                        }
                        101..=150 => {
                            next = (next.0 - 100, 200);
                        }
                        _ => panic!("invalid position"),
                    },
                    Direction::Down => match next.0 {
                        1..=50 => next = (next.0 + 100, 1),
                        51..=100 => {
                            next = (50, next.0 + 100);
                            next_dir = Direction::Left;
                        }
                        101..=150 => {
                            next = (100, next.0 - 50);
                            next_dir = Direction::Left;
                        }
                        _ => panic!("invalid position"),
                    },
                    Direction::Right => match next.1 {
                        1..=50 => {
                            next = (100, 151 - next.1);
                            next_dir = Direction::Left;
                        }
                        51..=100 => {
                            next = (next.1 + 50, 50);
                            next_dir = Direction::Up;
                        }
                        101..=150 => {
                            next = (150, 151 - next.1);
                            next_dir = Direction::Left;
                        }
                        151..=200 => {
                            next = (next.1 - 100, 150);
                            next_dir = Direction::Up;
                        }
                        _ => panic!("invalid position"),
                    },
                    Direction::Left => match next.1 {
                        1..=50 => {
                            next = (1, 151 - next.1);
                            next_dir = Direction::Right;
                        }
                        51..=100 => {
                            next = (next.1 - 50, 101);
                            next_dir = Direction::Down;
                        }
                        101..=150 => {
                            next = (51, 151 - next.1);
                            next_dir = Direction::Right;
                        }
                        151..=200 => {
                            next = (next.1 - 100, 1);
                            next_dir = Direction::Down;
                        }
                        _ => panic!("invalid position"),
                    },
                }
            }
            if *self.grid.get(&next).unwrap() == Tile::Empty {
                current = next;
                dir = next_dir;
            } else if *self.grid.get(&next).unwrap() == Tile::Wall {
                break;
            }
        }
        (current, dir)
    }
}

fn create_map(input: &str) -> Map {
    let mut grid: Grid = HashMap::new();
    for (y, line) in input.lines().enumerate() {
        for (x, c) in line.chars().enumerate() {
            if c != ' ' {
                let tile = Tile::from_chr(c);
                grid.insert((x + 1, y + 1), tile);
            }
        }
    }
    let mut m = Map {
        grid: grid,
        start: (1, 1),
    };
    m.start.0 = m.row_start(1);
    m
}

fn create_instructions(input: &str) -> Vec<Instruction> {
    let mut instructions: Vec<Instruction> = Vec::new();
    let re = Regex::new(r"(L|R|\d+)").unwrap();
    for cap in re.captures_iter(input) {
        match &cap[1] {
            "L" => instructions.push(Instruction::TurnLeft),
            "R" => instructions.push(Instruction::TurnRight),
            n => {
                let num = n.parse::<usize>().unwrap();
                instructions.push(Instruction::Move(num));
            }
        }
    }
    instructions
}

#[aoc(day22, part1)]
pub fn solve_part1(input: &str) -> usize {
    let (first, last) = input.split_once("\n\n").unwrap();
    let map = create_map(first);
    let instructions = create_instructions(last);
    map.solve(instructions, false)
}

#[aoc(day22, part2)]
pub fn solve_part2(input: &str) -> usize {
    let (first, last) = input.split_once("\n\n").unwrap();
    let map = create_map(first);
    let instructions = create_instructions(last);
    map.solve(instructions, true)
}
