use std::collections::HashSet;
use std::iter::FromIterator;

fn decode(c: &char) -> i32 {
    match c.is_uppercase() {
        true => *c as i32 - 38,
        false => *c as i32 - 96,
    }
}

fn find_common(s: &str) -> i32 {
    let (s1, s2) = s.split_at(s.len() / 2);
    let first: HashSet<char> = HashSet::from_iter(s1.chars());
    let last: HashSet<char> = HashSet::from_iter(s2.chars());
    let common = first.intersection(&last).last().unwrap_or(&'0');
    decode(common)
}

fn find_common_groups(a: &str, b: &str, c: &str) -> i32 {
    let s1: HashSet<char> = HashSet::from_iter(a.chars());
    let s2: HashSet<char> = HashSet::from_iter(b.chars());
    let s3: HashSet<char> = HashSet::from_iter(c.chars());
    let tmp: HashSet<char> = s1.intersection(&s2).cloned().collect();
    let common = tmp.intersection(&s3).last().unwrap_or(&'0');
    decode(common)
}

#[aoc(day3, part1)]
pub fn solve_part1(input: &str) -> i32 {
    input.lines().map(|l| find_common(l)).sum()
}

#[aoc(day3, part2)]
pub fn solve_part2(input: &str) -> i32 {
    input
        .lines()
        .collect::<Vec<&str>>()
        .chunks(3)
        .map(|c| find_common_groups(c[0], c[1], c[2]))
        .sum()
}
