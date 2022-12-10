#[derive(Copy, Clone)]
pub struct Section {
    min: i32,
    max: i32,
}

fn fully_contains(s1: Section, s2: Section) -> bool {
    s1.min >= s2.min && s1.max <= s2.max || s2.min >= s1.min && s2.max <= s1.max
}

fn overlap(s1: Section, s2: Section) -> bool {
    !(s1.max < s2.min || s1.min > s2.max) 
}

pub fn create_sections(s: &str) -> (Section, Section) {
    let (first, last) = s.split_once(",").unwrap();
    (create_section(first), create_section(last))
}

pub fn create_section(s: &str) -> Section {
    let (min, max) = s.split_once("-").unwrap();
    Section {
        min: min.parse().unwrap_or(0),
        max: max.parse().unwrap_or(0),
    }
}

#[aoc(day4, part1)]
pub fn solve_part1(input: &str) -> usize {
    input
        .lines()
        .filter(|l| {
            let (s1, s2) = create_sections(l);
            fully_contains(s1, s2)
        })
        .count()
}

#[aoc(day4, part2)]
pub fn solve_part2(input: &str) -> usize {
    input
        .lines()
        .filter(|l| {
            let (s1, s2) = create_sections(l);
            overlap(s1, s2)
        })
        .count()
}
