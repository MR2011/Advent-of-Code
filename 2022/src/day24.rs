use std::collections::HashMap;
use std::collections::HashSet;

#[derive(Eq, PartialEq, Debug)]
enum Tile {
    Left,
    Right,
    Up,
    Down,
}

type Coord = (isize, isize);

struct Grid {
    data: HashMap<Coord, Tile>,
    max_x: isize,
    max_y: isize,
}

impl Grid {
    fn shortest_path(&mut self, start: Coord, end: Coord, mut minutes: isize) -> isize {
        let mut queue: HashSet<Coord> = HashSet::new();
        queue.insert(start);

        loop {
            let mut candidates: HashSet<Coord> = HashSet::new();
            for (cx, cy) in queue.into_iter() {
                for (nx, ny) in self.neighbors((cx, cy)).into_iter() {
                    if (nx, ny) == end {
                        return minutes;
                    }
                    if self.in_boundaries((nx, ny)) && self.empty_tile((nx, ny), minutes) {
                        candidates.insert((nx, ny));
                    }
                }
            }
            queue = candidates;
            if queue.is_empty() {
                queue.insert(start);
            }
            minutes += 1;
        }
    }

    fn empty_tile(&self, (x, y): Coord, m: isize) -> bool {
        self.data.get(&((x - m).rem_euclid(self.max_x), y)) != Some(&Tile::Right)
            && self.data.get(&((x + m).rem_euclid(self.max_x), y)) != Some(&Tile::Left)
            && self.data.get(&((x, (y - m).rem_euclid(self.max_y)))) != Some(&Tile::Down)
            && self.data.get(&((x, (y + m).rem_euclid(self.max_y)))) != Some(&Tile::Up)
    }

    fn in_boundaries(&self, (x, y): Coord) -> bool {
        (x >= 0) && (y >= 0) && (y < self.max_y) && (x < self.max_x)
    }

    fn neighbors(&self, (x, y): Coord) -> Vec<Coord> {
        vec![(x, y), (x - 1, y), (x + 1, y), (x, y + 1), (x, y - 1)]
    }
}

fn parse_input(input: &str) -> Grid {
    let mut grid: HashMap<Coord, Tile> = HashMap::new();
    for (y, line) in input.lines().enumerate() {
        for (x, c) in line.chars().enumerate() {
            if c == '.' || c == '#' {
                continue;
            }
            let tile = match c {
                '>' => Tile::Right,
                '<' => Tile::Left,
                'v' => Tile::Down,
                '^' => Tile::Up,
                _ => panic!("invalid tile"),
            };
            grid.insert(((x - 1) as isize, (y - 1) as isize), tile);
        }
    }
    let max_y = input.lines().count() - 2;
    let max_x = input.lines().next().unwrap().len() - 2;
    Grid {
        data: grid,
        max_y: max_y as isize,
        max_x: max_x as isize,
    }
}

#[aoc(day24, part1)]
pub fn solve_part1(input: &str) -> isize {
    let mut grid = parse_input(input);
    let end = ((grid.max_x - 1) as isize, grid.max_y as isize);
    grid.shortest_path((0, -1), end, 0)
}

#[aoc(day24, part2)]
pub fn solve_part2(input: &str) -> isize {
    let mut grid = parse_input(input);
    let end = ((grid.max_x - 1) as isize, grid.max_y as isize);
    let start = (0 as isize, -1 as isize);
    let m1 = grid.shortest_path(start, end, 0);
    let m2 = grid.shortest_path(end, start, m1);
    grid.shortest_path(start, end, m2)
}
