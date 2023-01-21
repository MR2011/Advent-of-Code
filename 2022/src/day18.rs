use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;

#[derive(Hash, Eq, PartialEq, Copy, Clone, Debug)]
struct Point {
    x: i32,
    y: i32,
    z: i32,
}

impl Point {
    fn neighbors(&self) -> Vec<Point> {
        let mut n: Vec<Point> = Vec::new();
        let offsets = vec![
            (0, 0, 1),
            (0, 0, -1),
            (1, 0, 0),
            (-1, 0, 0),
            (0, 1, 0),
            (0, -1, 0),
        ];
        for o in offsets {
            let p = Point {
                x: self.x + o.0,
                y: self.y + o.1,
                z: self.z + o.2,
            };
            n.push(p);
        }
        n
    }
}

fn calc_surface_area(points: &HashSet<Point>) -> usize {
    let mut sa = 0;
    for p in points.iter() {
        let mut connected = 0;
        for n in p.neighbors() {
            if points.contains(&n) {
                connected += 1;
            }
        }
        sa += 6 - connected;
    }
    sa
}

fn parse_point(s: &str) -> Point {
    let tokens: Vec<i32> = s.split(",").map(|n| n.parse::<i32>().unwrap()).collect();
    Point {
        x: tokens[0],
        y: tokens[1],
        z: tokens[2],
    }
}

fn parse_input(input: &str) -> HashSet<Point> {
    input.lines().map(|l| parse_point(l)).collect()
}

#[aoc(day18, part1)]
pub fn solve_part1(input: &str) -> usize {
    let points = parse_input(input);
    calc_surface_area(&points)
}

#[aoc(day18, part2)]
pub fn solve_part2(input: &str) -> usize {
    let points = parse_input(input);
    let sa = calc_surface_area(&points);

    let min_x = points.iter().map(|p| p.x).min().unwrap();
    let min_y = points.iter().map(|p| p.y).min().unwrap();
    let min_z = points.iter().map(|p| p.z).min().unwrap();
    let max_x = points.iter().map(|p| p.x).max().unwrap();
    let max_y = points.iter().map(|p| p.y).max().unwrap();
    let max_z = points.iter().map(|p| p.z).max().unwrap();

    let mut air: HashMap<Point, bool> = HashMap::new();
    for x in (min_x - 1)..=(max_x + 1) {
        for y in (min_y - 1)..=(max_y + 1) {
            for z in (min_z - 1)..=(max_z + 1) {
                let p = Point { x: x, y: y, z: z };
                if !points.contains(&p) {
                    air.insert(p, false);
                }
            }
        }
    }

    let mut q: VecDeque<Point> = VecDeque::new();
    q.push_back(Point {
        x: min_x - 1,
        y: min_y - 1,
        z: min_z - 1,
    });

    while !q.is_empty() {
        let current = q.pop_front().unwrap();
        air.insert(current, true);
        for n in current.neighbors() {
            if air.contains_key(&n) && !air[&n] && !q.contains(&n) {
                q.push_back(n);
            }
        }
    }

    let air_filtered: HashSet<Point> = air
        .keys()
        .cloned()
        .filter(|c| !air[c])
        .collect();

    let sa_air = calc_surface_area(&air_filtered);
    sa - sa_air
}
