use std::collections::HashSet;

#[derive(Clone, Copy)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

#[derive(Clone, Copy)]
struct Instruction {
    direction: Direction,
    steps: i32,
}

#[derive(Clone, Copy, Eq, Hash, PartialEq)]
struct Point {
    x: i32,
    y: i32,
}

struct Rope {
    knots: Vec<Point>,
    history: HashSet<Point>,
}

impl Rope {
    fn move_to(&mut self, instruction: Instruction) {
        for _ in 0..instruction.steps {
            match instruction.direction {
                Direction::Up => self.knots[0].y += 1,
                Direction::Down => self.knots[0].y -= 1,
                Direction::Left => self.knots[0].x += 1,
                Direction::Right => self.knots[0].x -= 1,
            }
            for i in 1..self.knots.len() {
                self.follow(i - 1, i);
            }
            self.history.insert(self.knots.last().unwrap().clone());
        }
    }

    fn follow(&mut self, head: usize, tail: usize) {
        let dx = self.knots[head].x - self.knots[tail].x;
        let dy = self.knots[head].y - self.knots[tail].y;
        if dx.abs() == 2 || dy.abs() == 2 {
            self.knots[tail].x += dx.signum();
            self.knots[tail].y += dy.signum();
        }
    }
}

fn create_instructions(input: &str) -> Vec<Instruction> {
    input
        .lines()
        .map(|l| {
            let (first, last) = l.split_once(" ").unwrap();
            let steps = last.parse::<i32>().unwrap();
            let direction = match first {
                "U" => Direction::Up,
                "D" => Direction::Down,
                "L" => Direction::Left,
                "R" => Direction::Right,
                _ => panic!("invalid direction"),
            };
            Instruction {
                direction: direction,
                steps: steps,
            }
        })
        .collect()
}

#[aoc(day9, part1)]
pub fn solve_part1(input: &str) -> usize {
    let instructions = create_instructions(input);
    let mut rope = Rope {
        knots: vec![Point { x: 0, y: 0 }; 2],
        history: HashSet::new(),
    };
    instructions.iter().for_each(|&i| rope.move_to(i));
    rope.history.len()
}

#[aoc(day9, part2)]
pub fn solve_part2(input: &str) -> usize {
    let instructions = create_instructions(input);
    let mut rope = Rope {
        knots: vec![Point { x: 0, y: 0 }; 10],
        history: HashSet::new(),
    };
    instructions.iter().for_each(|&i| rope.move_to(i));
    rope.history.len()
}
