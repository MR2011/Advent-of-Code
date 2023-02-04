use std::collections::HashMap;
use std::collections::HashSet;

type Elf = (isize, isize);

enum Direction {
    North,
    East,
    South,
    West,
}

struct Grid {
    elves: HashSet<Elf>,
    offset: usize,
    dirs: Vec<Direction>,
}

impl Grid {
    fn round(&mut self) -> usize {
        let mut next_elves: HashSet<Elf> = HashSet::new();
        let mut moves: HashMap<Elf, Vec<Elf>> = HashMap::new();

        let mut moved = 0;
        for current in self.elves.iter() {
            let next = self.propose(*current);
            if *current != next {
                moved += 1;
            }
            moves.entry(next).or_insert(Vec::new()).push(*current);
        }

        for (next, elves) in moves.iter() {
            if elves.len() > 1 {
                next_elves.extend(elves);
            } else {
                next_elves.insert(*next);
            }
        }

        self.elves = next_elves;
        self.offset += 1;
        moved
    }

    fn propose(&self, (x, y): Elf) -> Elf {
        if self.do_nothing((x, y)) {
            return (x, y);
        }
        for i in 0..4 {
            let index = (i + self.offset) % 4;
            match self.dirs[index] {
                Direction::North => {
                    if self.can_move_north((x, y)) {
                        return (x, y - 1);
                    }
                }
                Direction::South => {
                    if self.can_move_south((x, y)) {
                        return (x, y + 1);
                    }
                }
                Direction::West => {
                    if self.can_move_west((x, y)) {
                        return (x - 1, y);
                    }
                }
                Direction::East => {
                    if self.can_move_east((x, y)) {
                        return (x + 1, y);
                    }
                }
            }
        }
        (x, y)
    }

    fn do_nothing(&self, elf: Elf) -> bool {
        self.can_move_north(elf)
            && self.can_move_south(elf)
            && self.can_move_west(elf)
            && self.can_move_east(elf)
    }

    fn can_move_north(&self, (x, y): Elf) -> bool {
        let nw = self.elves.contains(&(x - 1, y - 1));
        let n = self.elves.contains(&(x, y - 1));
        let ne = self.elves.contains(&(x + 1, y - 1));
        !(nw || n || ne)
    }

    fn can_move_south(&self, (x, y): Elf) -> bool {
        let sw = self.elves.contains(&(x - 1, y + 1));
        let s = self.elves.contains(&(x, y + 1));
        let se = self.elves.contains(&(x + 1, y + 1));
        !(sw || s || se)
    }

    fn can_move_west(&self, (x, y): Elf) -> bool {
        let sw = self.elves.contains(&(x - 1, y + 1));
        let nw = self.elves.contains(&(x - 1, y - 1));
        let w = self.elves.contains(&(x - 1, y));
        !(sw || w || nw)
    }

    fn can_move_east(&self, (x, y): Elf) -> bool {
        let se = self.elves.contains(&(x + 1, y + 1));
        let ne = self.elves.contains(&(x + 1, y - 1));
        let e = self.elves.contains(&(x + 1, y));
        !(se || e || ne)
    }

    fn empty_tiles(&self) -> usize {
        let (min_x, max_x, min_y, max_y) = self.boundary();
        let mut count = 0;
        for y in min_y..=max_y {
            for x in min_x..=max_x {
                let e = (x as isize, y as isize);
                if !self.elves.contains(&e) {
                    count += 1;
                }
            }
        }
        count
    }

    fn boundary(&self) -> (isize, isize, isize, isize) {
        let min_x = self.elves.iter().map(|(x, _)| *x).min().unwrap();
        let max_x = self.elves.iter().map(|(x, _)| *x).max().unwrap();
        let min_y = self.elves.iter().map(|(_, y)| *y).min().unwrap();
        let max_y = self.elves.iter().map(|(_, y)| *y).max().unwrap();
        (min_x, max_x, min_y, max_y)
    }

    fn print(&self) {
        let (min_x, max_x, min_y, max_y) = self.boundary();
        for y in min_y..=max_y {
            for x in min_x..=max_x {
                let e = (x as isize, y as isize);
                if self.elves.contains(&e) {
                    print!("#");
                } else {
                    print!(".");
                }
            }
            print!("\n");
        }
    }
}

fn create_grid(input: &str) -> Grid {
    let mut elves: HashSet<Elf> = HashSet::new();
    for (y, line) in input.lines().enumerate() {
        for (x, c) in line.chars().enumerate() {
            if c == '#' {
                elves.insert((x as isize, y as isize));
            }
        }
    }
    Grid {
        elves: elves,
        offset: 0,
        dirs: vec![
            Direction::North,
            Direction::South,
            Direction::West,
            Direction::East,
        ],
    }
}

#[aoc(day23, part1)]
pub fn solve_part1(input: &str) -> usize {
    let mut grid = create_grid(input);

    for _ in 0..10 {
        grid.round();
    }

    grid.empty_tiles()
}

#[aoc(day23, part2)]
pub fn solve_part2(input: &str) -> usize {
    let mut grid = create_grid(input);

    for i in 1.. {
        if grid.round() == 0 {
            return i;
        }
    }
    0
}
