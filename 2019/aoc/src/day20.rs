use std::cmp::Ordering;
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;

#[derive(Ord, PartialOrd, Copy, Clone, Hash, PartialEq, Eq)]
struct Coord {
    x: usize,
    y: usize,
}

impl Coord {
    pub fn neighbors(&self) -> Vec<Coord> {
        let mut neighbors = Vec::new();
        for (dx, dy) in [(0, 1), (1, 0), (-1, 0), (0, -1)].iter() {
            let x = self.x as isize + dx;
            let y = self.y as isize + dy;
            neighbors.push(Coord {
                x: x as usize,
                y: y as usize,
            });
        }
        neighbors
    }
}

#[derive(PartialEq, Eq)]
pub enum Tile {
    Free,
    Wall,
    Portal(String),
}

pub struct Map {
    inner_portals: HashMap<Coord, Coord>,
    outer_portals: HashMap<Coord, Coord>,
    start: Coord,
    end: Coord,
    grid: HashMap<Coord, Tile>,
}

impl Map {
    pub fn shortest_path(&self) -> usize {
        let mut q: VecDeque<(Coord, usize)> = VecDeque::new();
        let mut visited: HashSet<Coord> = HashSet::new();

        q.push_back((self.start, 0));
        visited.insert(self.start);

        while let Some(s) = q.pop_front() {
            let p = s.0;
            let steps = s.1;
            if p == self.end {
                return steps;
            }
            for neighbor in p.neighbors().iter() {
                if !visited.contains(&neighbor) {
                    match self.grid.get(&neighbor) {
                        Some(Tile::Free) => {
                            q.push_back((*neighbor, steps + 1));
                        }
                        Some(Tile::Portal(s)) => {
                            if let Some(port) = self.inner_portals.get(&p) {
                                q.push_back((*port, steps + 1));
                            } else if let Some(port) = self.outer_portals.get(&p) {
                                q.push_back((*port, steps + 1));
                            }
                        }
                        _ => (),
                    }
                    visited.insert(*neighbor);
                }
            }
        }
        std::usize::MAX
    }

    pub fn shortest_path2(&self) -> usize {
        let mut q: VecDeque<(Coord, usize, usize)> = VecDeque::new();
        let mut visited: HashSet<(Coord, usize)> = HashSet::new();

        q.push_back((self.start, 0, 0));
        visited.insert((self.start, 0));

        while let Some(s) = q.pop_front() {
            let p = s.0;
            let steps = s.1;
            let level = s.2;
            if p == self.end && level == 0 {
                return steps;
            }
            for neighbor in p.neighbors().iter() {
                if !visited.contains(&(*neighbor, level)) {
                    match self.grid.get(&neighbor) {
                        Some(Tile::Free) => {
                            q.push_back((*neighbor, steps + 1, level));
                        }
                        Some(Tile::Portal(s)) => {
                            if let Some(inner) = self.inner_portals.get(&p)  {
                                q.push_back((*inner, steps + 1, level + 1));
                            }else if let Some(outer) = self.outer_portals.get(&p) {
                                if level > 0 {
                                        q.push_back((*outer, steps + 1, level - 1));
                                }
                            }
                        }
                        // Some(Tile::Portal(s)) => match self.inner_portals.get(&p) {
                        //     Some(inner) => {
                        //         q.push_back((*inner, steps + 1, level + 1));
                        //     }
                        //     None => match self.outer_portals.get(&p) {
                        //         Some(outer) => {
                        //             if level > 0 {
                        //                 q.push_back((*outer, steps + 1, level - 1));
                        //             }
                        //         }
                        //         None => (),
                        //     },
                        // },
                        _ => (),
                    }
                    visited.insert((*neighbor, level));
                }
            }
        }
        std::usize::MAX
    }
}
#[aoc_generator(day20)]
pub fn prepare_input(input: &str) -> Map {
    let mut inner_portals = HashMap::new();
    let mut outer_portals = HashMap::new();
    let mut grid = HashMap::new();
    let mut portals = Vec::new();

    for (y, line) in input.lines().enumerate() {
        for (x, tile) in line.chars().enumerate() {
            let c = Coord { x: x, y: y };
            match tile {
                '#' => {
                    grid.insert(c, Tile::Wall);
                }
                '.' => {
                    grid.insert(c, Tile::Free);
                }
                'A'..='Z' => {
                    grid.insert(c, Tile::Portal(tile.to_string()));
                    if let Some(Tile::Portal(s)) = grid.get(&Coord { x: x - 1, y: y }) {
                        let name = format!("{}{}", s, tile);
                        let mut x2 = x + 1;
                        if grid.get(&Coord { x: x - 2, y: y }) == Some(&Tile::Free) {
                            x2 = x - 2;
                        }
                        portals.push((name.clone(), Coord { x: x2, y: y }));
                    } else {
                        if let Some(Tile::Portal(s)) = grid.get(&Coord { x: x, y: y - 1 }) {
                            let name = format!("{}{}", s, tile);
                            let mut y2 = y + 1;
                            if grid.get(&Coord { x: x, y: y - 2 }) == Some(&Tile::Free) {
                                y2 = y - 2;
                            }
                            portals.push((name.clone(), Coord { x: x, y: y2 }));
                        }
                    }
                }
                _ => {
                    grid.insert(c, Tile::Wall);
                }
            }
        }
    }

    let width = grid.keys().max_by_key(|k| k.x).unwrap().x;
    let height = grid.keys().max_by_key(|k| k.y).unwrap().y;

    portals.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap());
    let start = portals.remove(0).1;
    let end = portals.remove(portals.len() - 1).1;

    portals.chunks(2).for_each(|item| {
        if (item[0].1).x < 3
            || (item[0].1).x > width - 3
            || (item[0].1).y < 3
            || (item[0].1).y > height - 3
        {
            outer_portals.insert(item[0].1, item[1].1);
            inner_portals.insert(item[1].1, item[0].1);
        } else {
            inner_portals.insert(item[0].1, item[1].1);
            outer_portals.insert(item[1].1, item[0].1);
        }
    });

    Map {
        start: start,
        end: end,
        grid: grid,
        inner_portals: inner_portals,
        outer_portals: outer_portals,
    }
}

#[aoc(day20, part1)]
pub fn solve_part1(map: &Map) -> usize {
    map.shortest_path()
}

#[aoc(day20, part2)]
pub fn solve_part2(map: &Map) -> usize {
    map.shortest_path2()
}
