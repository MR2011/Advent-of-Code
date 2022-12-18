use std::collections::HashSet;

fn find_start(input: &str, size: usize) -> usize {
    let l = input.chars().collect::<Vec<char>>();
    for (i, w) in l.windows(size).enumerate() {
        let s: HashSet<&char> = HashSet::from_iter(w.iter());
        if s.len() == size {
            return i + size 
        }
    }
    0
}

#[aoc(day6, part1)]
pub fn solve_part1(input: &str) -> usize {
    find_start(input, 4)
}

#[aoc(day6, part2)]
pub fn solve_part2(input: &str) -> usize {
    find_start(input, 14)
}
