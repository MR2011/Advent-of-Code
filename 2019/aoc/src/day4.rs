use std::ops::Range;

#[derive(Debug)]
struct Password (usize);


impl Password {
    fn valid_p1(&self) -> bool {
        self.contains_six_digits() 
            && self.contains_pair() 
            && self.is_increasing()
    }
    
    fn valid_p2(&self) -> bool {
        self.contains_six_digits() 
            && self.contains_strict_pair() 
            && self.is_increasing()
    }
    
    fn contains_six_digits(&self) -> bool {
        (self.0 >= 100000) && (self.0 <= 999999)
    }
    
    fn contains_pair(&self) -> bool {
        let chars: Vec<char> = self.0.to_string().chars().collect();
        chars.windows(2).any(|w| w[0] == w[1])
    }
    
    fn contains_strict_pair(&self) -> bool {
        let chars: Vec<char> = self.0.to_string().chars().collect();
        chars.iter().any(|c| chars.iter().filter(|d| c == *d).count() == 2)
    }

    fn is_increasing(&self) -> bool {
        let chars: Vec<char> = self.0.to_string().chars().collect();
        chars.windows(2).all(|w| w[0] <= w[1])
    }
}

#[aoc_generator(day4)]
pub fn prepare_input(input: &str) -> Range<usize> {
    let r: Vec<usize> = input.split("-")
        .map(|i| i.parse::<usize>().unwrap())
        .collect();
    Range {
        start: r[0],
        end: r[1],
    }
}

#[aoc(day4, part1)]
pub fn solve_part1(range: &Range<usize>) -> i32 {
    let mut valid_pws = 0;
    for i in range.start .. range.end {
        if Password(i).valid_p1() {
            valid_pws += 1;
        }
    }
    return valid_pws
}
#[aoc(day4, part2)]
pub fn solve_part2(range: &Range<usize>) -> i32 {
    let mut valid_pws = 0;
    for i in range.start .. range.end {
        if Password(i).valid_p2() {
            valid_pws += 1;
        }
    }
    return valid_pws
}

#[cfg(test)]
mod tests {
    use super::*;

    // Part 1
    #[test]
    fn test1() {
        let pw = Password(111111);
        assert_eq!(pw.valid_p1(), true);
    }
    
    #[test]
    fn test2() {
        let pw = Password(223450);
        assert_eq!(pw.valid_p1(), false);
    }
    
    #[test]
    fn test3() {
        let pw = Password(123789);
        assert_eq!(pw.valid_p1(), false);
    }
    // Part 2 
    #[test]
    fn test4() {
        let pw = Password(112233);
        assert_eq!(pw.valid_p2(), true);
    }
    #[test]
    fn test5() {
        let pw = Password(123444);
        assert_eq!(pw.valid_p2(), false);
    }
    #[test]
    fn test6() {
        let pw = Password(111122);
        assert_eq!(pw.valid_p2(), true);
    }
}
