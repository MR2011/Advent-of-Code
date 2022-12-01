use itertools::Itertools;

#[aoc(day1, part1)]
pub fn solve_part1(input: &str) -> i32 {
    input
        .split("\n\n")
        .map(|e| e.lines().filter_map(|f| f.parse::<i32>().ok()).sum::<i32>())
        .max()
        .unwrap_or(0)
}

#[aoc(day1, part2)]
pub fn solve_part2(input: &str) -> i32 {
    input
        .split("\n\n")
        .map(|e| e.lines().filter_map(|f| f.parse::<i32>().ok()).sum::<i32>())
        .sorted_by(|a,b| b.cmp(a))
        .take(3)
        .sum::<i32>()
}
