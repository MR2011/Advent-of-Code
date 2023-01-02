use std::cmp::Ordering;
use std::collections::BinaryHeap;
use std::collections::HashMap;
use std::collections::HashSet;

#[derive(Copy, Clone, Eq, PartialEq)]
struct Node {
    cost: usize,
    coord: Coord,
}

impl Ord for Node {
    fn cmp(&self, other: &Self) -> Ordering {
        other.cost.cmp(&self.cost)
    }
}

impl PartialOrd for Node {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Eq, Hash, PartialEq, Copy, Clone, Debug)]
struct Coord {
    x: usize,
    y: usize,
}

struct Grid {
    grid: HashMap<Coord, usize>,
    start: Coord,
    end: Coord,
    bounds: Coord,
}

impl Grid {
    fn neighbors(&self, node: Coord) -> Vec<Coord> {
        let mut neighbors: Vec<Coord> = Vec::new();
        for offset in vec![(0, 1), (0, -1), (1, 0), (-1, 0)].iter() {
            let x = node.x as isize + offset.0;
            let y = node.y as isize + offset.1;
            if x >= 0 && x < self.bounds.x as isize && y >= 0 && y < self.bounds.y as isize {
                neighbors.push(Coord {
                    x: x as usize,
                    y: y as usize,
                });
            }
        }
        neighbors
    }

    fn from(input: &str) -> Grid {
        let mut grid: HashMap<Coord, usize> = HashMap::new();
        let mut start = Coord { x: 0, y: 0 };
        let mut end = Coord { x: 0, y: 0 };
        for (y, line) in input.lines().enumerate() {
            for (x, c) in line.chars().enumerate() {
                let coord = Coord { x: x, y: y };
                let elevation;
                if c == 'S' {
                    start = coord;
                    elevation = 'a' as usize;
                } else if c == 'E' {
                    end = coord;
                    elevation = 'z' as usize;
                } else {
                    elevation = c as usize;
                }
                grid.insert(coord, elevation);
            }
        }
        let max_x = grid.keys().map(|k| k.x).max().unwrap();
        let max_y = grid.keys().map(|k| k.y).max().unwrap();
        Grid {
            grid: grid,
            start: start,
            end: end,
            bounds: Coord { x: max_x, y: max_y },
        }
    }

    fn shortest_path(&self, src: Coord, dst: Coord) -> Option<usize> {
        let mut pq = BinaryHeap::new();

        pq.push(Node {
            cost: 0,
            coord: src,
        });

        let mut visited = HashSet::new();
        visited.insert(self.start);

        while let Some(Node { cost, coord }) = pq.pop() {
            if coord == dst {
                return Some(cost);
            }

            let elevation = self.grid.get(&coord).unwrap();
            let neighbors = self.neighbors(coord);

            let candidates = neighbors
                .iter()
                .filter(|c| {
                    let e = self.grid.get(c).unwrap();
                    e <= elevation || *e == elevation + 1
                })
                .collect::<Vec<&Coord>>();

            for candidate in candidates {
                if visited.insert(*candidate) {
                    pq.push(Node {
                        cost: cost + 1,
                        coord: *candidate,
                    });
                }
            }
        }
        None
    }
}

#[aoc(day12, part1)]
pub fn solve_part1(input: &str) -> usize {
    let grid = Grid::from(input);
    match grid.shortest_path(grid.start, grid.end) {
        Some(p) => p,
        None => 0,
    }
}

#[aoc(day12, part2)]
pub fn solve_part2(input: &str) -> usize {
    let grid = Grid::from(input);
    grid.grid
        .iter()
        .filter(|&(_, v)| *v == ('a' as usize))
        .map(
            |(coord, _)| match grid.shortest_path(coord.clone(), grid.end) {
                Some(p) => p,
                None => usize::MAX,
            },
        )
        .min()
        .unwrap()
}
