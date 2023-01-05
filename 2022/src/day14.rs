use std::cmp::{max, min};

type Coord = (usize, usize);
type Grid = Vec<Vec<char>>;

fn create_path(line: &str) -> Vec<Coord> {
    line.split(" -> ")
        .map(|p| {
            let (first, last) = p.split_once(",").unwrap();
            let x = first.parse::<usize>().unwrap();
            let y = last.parse::<usize>().unwrap();
            (x, y)
        })
        .collect()
}

fn create_grid(input: &str) -> Grid {
    let paths: Vec<Vec<Coord>> = input.lines().map(|l| create_path(l)).collect();
    let max_y = paths
        .iter()
        .map(|p| p.iter().map(|(x, y)| y).max().unwrap())
        .max()
        .unwrap()
        + 2;
    let mut grid = vec![vec!['.'; 1000]; max_y + 1];
    grid[max_y] = vec!['#'; 1000];
    for path in paths {
        path.windows(2).for_each(|w| {
            let (x1, y1) = w[0];
            let (x2, y2) = w[1];
            for i in min(y1, y2)..=max(y1, y2) {
                grid[i][x1] = '#';
            }
            for i in min(x1, x2)..=max(x1, x2) {
                grid[y1][i] = '#';
            }
        });
    }
    grid
}

fn solve(mut grid: Grid) -> (usize, usize) {
    let mut units_p1 = 0;
    for units in 1.. {
        let (mut x, mut y) = (500, 0);
        loop {
            if y >= grid.len() - 2 && units_p1 == 0 {
                units_p1 = units - 1;
            }
            let mut done = false;
            for dx in [0, -1, 1] {
                let a = x + dx;
                if grid[y + 1][a as usize] == '.' {
                    (x, y) = (a, y + 1);
                    done = true;
                    break;
                }
            }
            if !done {
                grid[y][x as usize] = 'o';
                break;
            }
        }
        if (x, y) == (500, 0) {
            return (units_p1, units);
        }
    }
    (0, 0)
}

#[aoc(day14, part1)]
pub fn solve_part1(input: &str) -> usize {
    let grid = create_grid(input);
    let (units, _) = solve(grid);
    units
}

#[aoc(day14, part2)]
pub fn solve_part2(input: &str) -> usize {
    let grid = create_grid(input);
    let (_, units) = solve(grid);
    units
}
