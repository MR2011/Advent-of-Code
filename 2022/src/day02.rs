fn value(s: &str) -> Option<i32> {
    match s {
        "A" | "X" => Some(1),
        "B" | "Y" => Some(2),
        "C" | "Z" => Some(3),
        _ => None,
    }
}

// If both numbers are the same, no one wins
// If both numbers are consecutive, the bigger one wins
// If both numbers aren’t consecutive, the smaller one wins
// https://eduherminio.github.io/blog/rock-paper-scissors/
fn score(p1: i32, p2: i32) -> i32 {
    // add 3 to avoid negative number
    match (3 + p1 - p2) % 3 {
        0 => p2 + 3,
        1 => p2,
        _ => p2 + 6,
    }
}

fn choose(p1: i32, outcome: i32) -> Option<i32> {
    match outcome {
        // add 3 to avoid negative number and substract to start at 1
        1 => Some((3 + p1 - 2) % 3 + 1),
        2 => Some(p1),
        3 => Some(p1 % 3 + 1),
        _ => None,
    }
}

fn play(line: &str) -> i32 {
    let tokens: Vec<&str> = line.split(" ").collect();
    let p1 = value(tokens[0]).unwrap_or(0);
    let p2 = value(tokens[1]).unwrap_or(0);
    score(p1, p2)
}

fn choose_and_play(line: &str) -> i32 {
    let tokens: Vec<&str> = line.split(" ").collect();
    let p1 = value(tokens[0]).unwrap_or(0);
    let outcome = value(tokens[1]).unwrap_or(0);
    let p2 = choose(p1, outcome).unwrap_or(0);
    score(p1, p2)
}

#[aoc(day2, part1)]
pub fn solve_part1(input: &str) -> i32 {
    input.lines().map(|line| play(line)).sum()
}

#[aoc(day2, part2)]
pub fn solve_part2(input: &str) -> i32 {
    input.lines().map(|line| choose_and_play(line)).sum()
}
