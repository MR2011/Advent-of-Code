use std::collections::HashMap;

const WIDTH: isize = 5;
const HEIGHT: isize = 5;

type Point = (isize, isize);
type Grid = HashMap<Point, Tile>;

fn direct_neighbors(x: isize, y: isize, grid: &Grid) -> Vec<Option<&Tile>> {
    let mut neighbors = Vec::new();
    for (dx, dy) in &[(0, 1), (1, 0), (-1, 0), (0, -1)] {
        neighbors.push(grid.get(&(x + dx, y + dy)));
    }
    neighbors
}

fn outer_neighbors(x: isize, y: isize, grid: &Grid) -> Vec<Option<&Tile>> {
    let mut neighbors = Vec::new();
    if y == 0 {
        // get outer top tile
        neighbors.push(grid.get(&(2, 1)));
    }
    if x == 0 {
        // get outer left tile
        neighbors.push(grid.get(&(1, 2)));
    }
    if x == 4 {
        // get outer right tile
        neighbors.push(grid.get(&(3, 2)));
    }
    if y == 4 {
        // get outer bottom tile
        neighbors.push(grid.get(&(2, 3)));
    }
    neighbors
}

fn inner_neighbors(x: isize, y: isize, grid: &Grid) -> Vec<Option<&Tile>> {
    let mut neighbors = Vec::new();
    if x == 1 && y == 2 {
        // get inner left tiles
        for i in 0..HEIGHT {
            neighbors.push(grid.get(&(0, i)));
        }
    }
    if x == 2 && y == 1 {
        // get inner top tiles
        for i in 0..WIDTH {
            neighbors.push(grid.get(&(i, 0)));
        }
    }
    if x == 3 && y == 2 {
        // get inner right tiles
        for i in 0..HEIGHT {
            neighbors.push(grid.get(&(4, i)));
        }
    }
    if x == 2 && y == 3 {
        // get inner bottom tiles
        for i in 0..WIDTH {
            neighbors.push(grid.get(&(i, 4)));
        }
    }
    neighbors
}

fn tick(grid: &Grid) -> Grid {
    let mut new_state = grid.clone();
    for y in 0..HEIGHT {
        for x in 0..WIDTH {
            let bugs = direct_neighbors(x, y, grid)
                .iter()
                .filter(|&n| *n == Some(&Tile::Bug))
                .count();
            match grid.get(&(x, y)) {
                Some(Tile::Bug) => {
                    if bugs != 1 {
                        new_state.insert((x, y), Tile::Empty);
                    }
                }
                Some(Tile::Empty) => {
                    if bugs == 1 || bugs == 2 {
                        new_state.insert((x, y), Tile::Bug);
                    }
                }
                _ => (),
            }
        }
    }
    new_state.clone()
}

fn biodiversity(grid: &Grid) -> usize {
    let mut diversity = 0;
    let mut pow = 1;
    for y in 0..HEIGHT {
        for x in 0..HEIGHT {
            if grid.get(&(x, y)) == Some(&Tile::Bug) {
                diversity += pow;
            }
            pow = pow << 1;
        }
    }
    diversity
}

fn empty_grid() -> Grid {
    let mut grid = HashMap::new();
    for y in 0..HEIGHT {
        for x in 0..WIDTH {
            grid.insert((x, y), Tile::Empty);
        }
    }
    grid
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum Tile {
    Bug,
    Empty,
}

#[aoc_generator(day24)]
pub fn prepare_input(input: &str) -> Grid {
    let mut grid = HashMap::new();
    for (y, line) in input.lines().enumerate() {
        for (x, tile) in line.chars().enumerate() {
            match tile {
                '#' => {
                    grid.insert((x as isize, y as isize), Tile::Bug);
                }
                '.' => {
                    grid.insert((x as isize, y as isize), Tile::Empty);
                }
                _ => (),
            }
        }
    }
    grid
}

#[aoc(day24, part1)]
pub fn solve_part1(input: &Grid) -> usize {
    let mut new_state = input.clone();
    let mut ratings = Vec::new();
    loop {
        new_state = tick(&new_state);
        let rating = biodiversity(&new_state);
        if ratings.contains(&rating) {
            return rating;
        }
        ratings.push(rating);
    }
}

#[aoc(day24, part2)]
pub fn solve_part2(input: &Grid) -> i64 {
    let mut grids: HashMap<isize, Grid> = HashMap::new();
    let mut grid = input.clone();
    grids.insert(0, grid);
    let empty = empty_grid();
    for minute in 0..200 {
        let min = (minute + 1) * -1;
        let max = (minute + 1);
        grids.insert(min, empty.clone());
        grids.insert(max, empty.clone());
        let mut new_grids = grids.clone();
        for lvl in min..=max {
            let grid = grids.get(&lvl).unwrap();
            let mut new_grid = grid.clone();
            for y in 0..HEIGHT {
                for x in 0..WIDTH {
                    if !(y == 2 && x == 2) {
                        let mut neighbors: Vec<Option<&Tile>> = Vec::new();
                        // Outer
                        if lvl > min {
                            let outer = &grids.get(&(lvl - 1)).unwrap();
                            neighbors.extend(outer_neighbors(x, y, outer).iter().cloned());
                        }
                        // Inner
                        if lvl < max {
                            let inner = &grids.get(&(lvl + 1)).unwrap();
                            neighbors.extend(inner_neighbors(x, y, inner).iter().cloned());
                        }
                        neighbors.extend(direct_neighbors(x, y, grid).iter().cloned());
                        let bugs = neighbors.iter().filter(|&n| *n == Some(&Tile::Bug)).count();
                        match grid.get(&(x, y)) {
                            Some(Tile::Bug) => {
                                if bugs != 1 {
                                    new_grid.insert((x, y), Tile::Empty);
                                }
                            }
                            Some(Tile::Empty) => {
                                if bugs == 1 || bugs == 2 {
                                    new_grid.insert((x, y), Tile::Bug);
                                }
                            }
                            _ => (),
                        }
                    }
                }
            }
            new_grids.insert(lvl, new_grid.clone());
        }
        grids = new_grids.clone();
    }
    let mut bugs = 0;
    for (lvl, grid) in grids.into_iter() {
        for (p, tile) in grid.into_iter() {
            if tile == Tile::Bug {
                bugs += 1;
            }
        }
    }
    bugs
}
