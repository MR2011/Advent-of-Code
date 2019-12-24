use std::iter::successors;

fn calc_total_fuel(mass: &u32) -> u32 {
    successors(Some(*mass), |m| (m/3).checked_sub(2)).skip(1).sum()
}

fn parse_line(line: &str) -> u32 {
    line.parse().unwrap()
}

#[aoc(day1, part1)]
pub fn solve_part1(input: &str) -> u32 {
    let modules: Vec<u32> = input.lines().map(parse_line).collect();
    modules.iter().map(|m| m/3 - 2).sum()
}

#[aoc(day1, part2)]
pub fn solve_part2(input: &str) -> u32 {
    let modules: Vec<u32> = input.lines().map(parse_line).collect();
    modules.iter().map(calc_total_fuel).sum()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test1() {
        assert_eq!(solve_part1("12"), 2)
    }
    
    #[test]
    fn test2() {
        assert_eq!(solve_part1("14"), 2)
    }
    
    #[test]
    fn test3() {
        assert_eq!(solve_part1("1969"), 654)
    }
    
    #[test]
    fn test4() {
        assert_eq!(solve_part1("100756"), 33583)
    }
    
    #[test]
    fn test5() {
        assert_eq!(solve_part2("14"), 2)
    }
    #[test]
    fn test6() {
        assert_eq!(solve_part2("1969"), 966)
    }
    
    #[test]
    fn test7() {
        assert_eq!(solve_part2("100756"), 50346)
    }
}
