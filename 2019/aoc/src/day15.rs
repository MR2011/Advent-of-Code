use crate::intcode::VM;
use std::cmp::min;
use std::collections::VecDeque;
use std::collections::HashSet;
use std::collections::HashMap;

enum Direction {
    North,
    South,
    West,
    East
}

impl Direction {
    fn to_int(&self) -> i64 {
        match self {
            Direction::North => 1,
            Direction::South => 2,
            Direction::West => 3,
            Direction::East => 4,
        }
    }

    fn neighbor(x: i32, y: i32, d: &Direction) -> (i32, i32) {
        match d {
            Direction::North => (x, y+1),
            Direction::South => (x, y-1),
            Direction::West => (x-1, y),
            Direction::East => (x+1, y),
        }
    }

    fn to_list() -> Vec<Direction> {
        vec![
            Direction::North,
            Direction::South,
            Direction::West,
            Direction::East,
        ]
    }
}
#[derive(Clone, Copy, PartialEq, Eq)]
enum Tile {
    Wall,
    Empty,
    Oxygen,
}

impl Tile {
    fn from(n: &i64) -> Tile {
        match n {
            0 => Tile::Wall,
            1 => Tile::Empty,
            2 => Tile::Oxygen,
            _ => panic!("Unknown Tile"),
        }
    }
}
#[derive(Clone, Eq, PartialEq, Hash)]
struct Robot {
    x: i32,
    y: i32,
    steps: i64,
    vm: VM,
}

impl Robot {
    pub fn move_to(&self, direction: &Direction) -> Robot {
        let coords = Direction::neighbor(self.x, self.y, direction);
        let mut r = Robot {
            vm: self.vm.clone(),
            x: coords.0,
            y: coords.1,
            steps: self.steps + 1,
        };
        r.vm.feed_input(direction.to_int());
        r.vm.run_until_new_state();
        r
    }
}

struct Map {
    map: HashMap<(i32, i32), Tile>,
    min_steps: i64,
    oxygen: (i32, i32),
}

fn explore(vm: VM) -> Map {
    let robot = Robot{
        x: 0,
        y: 0,
        steps: 0,
        vm: vm,
    };
    let mut map = HashMap::new();
    let mut robots: VecDeque<Robot> = VecDeque::new();
    let mut oxygen = (0, 0);
    let directions = Direction::to_list();

    let mut min_steps = std::i64::MAX;
    robots.push_back(robot);

    while let Some(robot) = robots.pop_front() {
        for direction in directions.iter() {
            let new_robot = robot.move_to(direction);
            if !map.contains_key(&(new_robot.x, new_robot.y)) {
                let tile = Tile::from(new_robot.vm.output.last().unwrap());
                map.insert((new_robot.x, new_robot.y), tile);
                match tile { 
                    Tile::Wall => continue,
                    Tile::Empty => robots.push_back(new_robot),
                    Tile::Oxygen => {
                        min_steps = min(min_steps, new_robot.steps);
                        oxygen = (new_robot.x, new_robot.y)
                    },
                }
            }
        }
    }
    Map{ map, min_steps, oxygen}
}

fn release_oxygen(mut map: Map) -> i32 {
    let mut coords: VecDeque<(i32, i32)> = VecDeque::new();
    coords.push_back(map.oxygen);
    let directions = Direction::to_list();
    let mut minutes = 0;
    while !coords.is_empty() {
        let mut neighbors = HashSet::new();
        let mut updated = false;
        while let Some(point) = coords.pop_front() {
            for direction in directions.iter() {
                let neighbor = Direction::neighbor(point.0, point.1, direction);
                if *map.map.get(&neighbor).unwrap() == Tile::Empty {
                    map.map.insert(neighbor, Tile::Oxygen);
                    neighbors.insert(neighbor);
                    updated = true;
                }
            }
        }
        coords.extend(neighbors.into_iter());
        minutes += 1;
    }
    minutes - 1 // -1 because the last tile has no free neighbors but minutes is inc
}


#[aoc_generator(day15)]
pub fn prepare_input(input: &str) -> Vec<i64> {
    let tokens: Vec<i64> = input.trim().split(",")
        .map(|token| token.parse::<i64>().unwrap())
        .collect();
    tokens
}

#[aoc(day15, part1)]
pub fn solve_part1(input: &Vec<i64>) -> i64 {
    let vm = VM::new(input.clone(), vec![]);
    let map = explore(vm);
    map.min_steps
}

#[aoc(day15, part2)]
pub fn solve_part2(input: &Vec<i64>) -> i32 {
    let vm = VM::new(input.clone(), vec![]);
    let map = explore(vm);
    release_oxygen(map)
}
