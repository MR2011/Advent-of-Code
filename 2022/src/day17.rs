use std::collections::HashMap;
use std::iter::{Cycle, Enumerate};
use std::vec::IntoIter;

type JetCycle = Enumerate<Cycle<IntoIter<Jet>>>;
type RockCycle = Enumerate<Cycle<IntoIter<RockType>>>;

const LAST_ROWS: usize = 20;

#[derive(Copy, Clone, Debug)]
enum Jet {
    Left,
    Right,
}

#[derive(Eq, PartialEq, Hash, Debug)]
struct State {
    jet: usize,
    rock: usize,
    rows: Vec<u8>,
}

#[derive(Copy, Clone, Debug)]
enum RockType {
    Horizontal,
    Vertical,
    Plus,
    LShape,
    Square,
}

impl RockType {
    fn order() -> Vec<RockType> {
        vec![
            RockType::Horizontal,
            RockType::Plus,
            RockType::LShape,
            RockType::Vertical,
            RockType::Square,
        ]
    }
    fn len() -> usize {
        RockType::order().len()
    }
}

struct Rock {
    data: Vec<u8>,
}

impl Rock {
    fn new(rt: RockType) -> Rock {
        let data = match rt {
            RockType::Horizontal => {
                vec![0b0011110]
            }
            RockType::Plus => {
                vec![0b0001000, 0b0011100, 0b0001000]
            }
            RockType::LShape => {
                vec![0b0011100, 0b0000100, 0b0000100]
            }
            RockType::Vertical => {
                vec![0b0010000, 0b0010000, 0b0010000, 0b0010000]
            }
            RockType::Square => {
                vec![0b0011000, 0b0011000]
            }
        };
        Rock { data: data }
    }

    fn shift(&mut self, jet: Jet) {
        match jet {
            Jet::Left => {
                self.data = self.data.iter().map(|n| n << 1).collect();
            }
            Jet::Right => {
                self.data = self.data.iter().map(|n| n >> 1).collect();
            }
        }
    }

    fn undo_shift(&mut self, jet: Jet) {
        match jet {
            Jet::Left => {
                self.shift(Jet::Right);
            }
            Jet::Right => {
                self.shift(Jet::Left);
            }
        }
    }

    fn can_shift(&self, jet: Jet) -> bool {
        let left = 0b1000000;
        let right = 0b0000001;
        match jet {
            Jet::Left => self.data.iter().all(|n| (n & left) == 0),
            Jet::Right => self.data.iter().all(|n| (n & right) == 0),
        }
    }
}

struct Tower {
    data: Vec<u8>,
    jets: JetCycle,
    rocks: RockCycle,
    jet_len: usize,
}

impl Tower {
    fn init(jets: Vec<Jet>) -> Tower {
        let l = jets.len();
        Tower {
            data: Vec::new(),
            jets: jets.into_iter().cycle().enumerate(),
            rocks: RockType::order().into_iter().cycle().enumerate(),
            jet_len: l,
        }
    }

    fn simulate(&mut self, total: usize) -> usize {
        let mut states: HashMap<State, (usize, usize)> = HashMap::new();
        let mut remaining_rocks = total;
        let mut height_per_cycle = 0;
        let mut rocks_per_cycle = 0;
        let jet_idx = 0;
        for rock_num in 0..total {
            let (rock_idx, rock_type) = self.rocks.next().unwrap();
            let mut rock = Rock::new(rock_type);
            // bottom edge is three units above the highest rock in the room
            for _ in 0..3 {
                let (_, jet) = self.jets.next().unwrap();
                if rock.can_shift(jet) {
                    rock.shift(jet);
                }
            }
            let mut y = self.data.len();
            loop {
                let (_, jet) = self.jets.next().unwrap();
                if rock.can_shift(jet) {
                    rock.shift(jet);
                    // shift colliding with existing rocks?
                    if self.collision(&rock, y) {
                        rock.undo_shift(jet);
                    }
                }
                if (y == 0) || self.settled(&rock, y) {
                    break;
                }
                y -= 1;
            }
            // add rock to tower
            for i in 0..rock.data.len() {
                if (y + i) >= self.data.len() {
                    self.data.push(rock.data[i]);
                } else {
                    self.data[y + i] |= rock.data[i];
                }
            }
            // cycle detection
            if self.data.len() > LAST_ROWS {
                let state = State {
                    jet: jet_idx % self.jet_len,
                    rock: rock_idx % RockType::len(),
                    rows: self.data[self.data.len() - LAST_ROWS..].to_vec(),
                };
                match states.get(&state) {
                    Some((height, rocks)) => {
                        height_per_cycle = self.data.len() - height;
                        rocks_per_cycle = rock_num - rocks;
                        remaining_rocks = (total - rock_num) % rocks_per_cycle;
                    }
                    None => {
                        states.insert(state, (self.data.len(), rock_num));
                    }
                }
            }
            remaining_rocks -= 1;
            if remaining_rocks == 0 {
                let num_cycles = (total / rocks_per_cycle) - 1;
                return self.data.len() + num_cycles * height_per_cycle;
            }
        }
        0
    }

    fn collision(&self, rock: &Rock, y: usize) -> bool {
        let mut found = false;
        for i in 0..rock.data.len() {
            if (y + i) < self.data.len() {
                if (self.data[y + i] & rock.data[i]) != 0 {
                    found = true;
                }
            }
        }
        found
    }

    fn settled(&self, rock: &Rock, y: usize) -> bool {
        let mut done = false;
        for i in 0..rock.data.len() {
            if (y + i - 1) < self.data.len() {
                if (self.data[y + i - 1] & rock.data[i]) != 0 {
                    done = true;
                }
            }
        }
        done
    }

    fn print(&self) {
        for r in self.data.iter().rev() {
            println!("{:#09b}", r)
        }
    }
}

fn parse_input(input: &str) -> Vec<Jet> {
    input
        .chars()
        .map(|c| match c {
            '<' => Jet::Left,
            '>' => Jet::Right,
            _ => panic!("invalid input"),
        })
        .collect()
}

#[aoc(day17, part1)]
pub fn solve_part1(input: &str) -> usize {
    let jets = parse_input(input);
    let mut tower = Tower::init(jets);
    tower.simulate(2022)
}

#[aoc(day17, part2)]
pub fn solve_part2(input: &str) -> usize {
    let jets = parse_input(input);
    let mut tower = Tower::init(jets);
    tower.simulate(1000000000000)
}
