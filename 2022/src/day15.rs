use regex::Regex;
use std::cmp::max;

#[derive(Copy, Clone)]
struct Coord {
    x: isize,
    y: isize,
}

#[derive(Copy, Clone)]
struct Range {
    start: isize,
    end: isize,
}

struct Ranges {
    data: Vec<Range>,
}

impl Ranges {
    fn add(&mut self, range: Range) {
        self.data.push(range);
        self.merge();
    }

    fn merge(&mut self) {
        self.data.sort_by(|a, b| a.start.cmp(&b.start));
        let mut merged: Vec<Range> = Vec::new();
        merged.push(self.data[0].clone());

        for i in 1..self.data.len() {
            let current: Range = self.data[i].clone();
            let j: usize = merged.len() - 1;

            if current.start >= merged[j].start && current.start <= merged[j].end {
                merged[j].end = max(current.end, merged[j].end);
            } else {
                merged.push(current);
            }
        }
        self.data = merged;
    }
}

fn distance(c1: &Coord, c2: &Coord) -> isize {
    (c1.x - c2.x).abs() + (c1.y - c2.y).abs()
}

fn parse_line(line: &str) -> (Coord, Coord) {
    let re: Regex = Regex::new(r"x=(-?\d+), y=(-?\d+).*x=(-?\d+), y=(-?\d+)").unwrap();
    let cap = re.captures(line).unwrap();
    let sensor = Coord {
        x: cap[1].parse::<isize>().unwrap(),
        y: cap[2].parse::<isize>().unwrap(),
    };
    let beacon = Coord {
        x: cap[3].parse::<isize>().unwrap(),
        y: cap[4].parse::<isize>().unwrap(),
    };
    (sensor, beacon)
}

fn parse_input(input: &str) -> Vec<(Coord, Coord)> {
    input.lines().map(|l| parse_line(l)).collect()
}

fn solve(row: isize, pairs: Vec<(Coord, Coord)>) -> Ranges {
    let mut ranges = Ranges { data: Vec::new() };
    for pair in pairs {
        let sensor = pair.0;
        let beacon = pair.1;
        let dist_to_beacon = distance(&sensor, &beacon);
        let y_dist = (row - sensor.y).abs();
        let d = dist_to_beacon - y_dist;
        if d > 0 {
            let range = Range {
                start: sensor.x - d,
                end: sensor.x + d,
            };
            ranges.add(range);
        }
    }
    ranges
}

#[aoc(day15, part1)]
pub fn solve_part1(input: &str) -> isize {
    let pairs = parse_input(input);
    let ranges = solve(2_000_000, pairs);
    ranges.data.iter().map(|r| r.end - r.start).sum()
}

#[aoc(day15, part2)]
pub fn solve_part2(input: &str) -> u64 {
    let pairs = parse_input(input);
    for row in 0..=4_000_000 {
        let ranges = solve(row, pairs.clone());
        for w in ranges.data.windows(2) {
            if (w[1].start - w[0].end + 1) > 0 {
                return (w[0].end + 2) as u64 * 4000000 + row as u64;
            }
        }
    }
    0
}
