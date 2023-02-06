fn to_decimal(s: &str) -> i64 {
    let mut number = 0;
    let len = s.len();
    for (i, c) in s.char_indices().rev() {
        let digit = match c {
            '0' => 0,
            '1' => 1, 
            '2' => 2,
            '-' => -1, 
            '=' => -2,
            _ => panic!("invalid digit"),
        };
        number += (5_i64.pow((len - i -1) as u32) * digit);
    }
    number
}

fn to_snafu(mut n: i64) -> String {
    let mut number = String::new();
    while n != 0 {
        let rem = n % 5;
        let digit = match rem {
            0 => '0',
            1 => '1',
            2 => '2',
            3 => '=',
            4 => '-',
            _ => panic!("invalid digit"),
        };
        number.push(digit);
        n = (n+2)/5;
    }
    number.chars().rev().collect()

}

#[aoc(day25, part1)]
pub fn solve_part1(input: &str) -> String {
    let sum_dec = input.lines().map(|l| to_decimal(l)).sum();
    to_snafu(sum_dec)
}

#[aoc(day25, part2)]
pub fn solve_part2(input: &str) -> isize {
    2
}

