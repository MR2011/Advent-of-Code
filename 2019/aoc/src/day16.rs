struct Pattern {
    index: usize,
    repeat: usize,
}

impl Iterator for Pattern {
    type Item = i32;

    fn next(&mut self) -> Option<i32> {
        self.index = (self.index + 1) % (self.repeat * 4);
        Some(
            match self.index / self.repeat {
                0 => 0,
                1 => 1,
                2 => 0,
                3 => -1,
                _ => panic!(""),
            }
        )
    }
}

fn new_element(input: Vec<i32>, pos: usize) -> i32 {
    let mut pattern = Pattern{ index: pos, repeat: pos+1 };
    let sum: i32 = input.iter()
        .skip(pos)
        .map(|i| i * pattern.next().unwrap())
        .sum();
    (sum % 10).abs()
}

fn fft_phase(input: Vec<i32>) -> Vec<i32> {
    let mut output = Vec::new();
    for i in 0..input.len() {
        output.push(new_element(input.clone(), i));
    }
    output
}

fn fft(mut list: Vec<i32>, n: i32, offset: usize, part: i32) -> String {
    for i in 0..n {
        match part {
            1 => list = fft_phase(list.clone()),
            2 => list = fft_phase2(list.clone()),
            _ => panic!(""),
        }
    }
    let mut output: String = list.iter()
                             .map(|i| i.to_string())
                             .collect::<Vec<String>>()
                             .join("");
    output[offset..offset+8].to_string()
}


fn fft_phase2(input: Vec<i32>) -> Vec<i32> {
    let mut output = input.clone();
    let mut sum = 0;
    for i in (0..input.len()).rev() {
        sum = (sum + input[i]) % 10;
        output[i] = sum;
    }
    output
}

#[aoc_generator(day16)]
pub fn prepare_input(input: &str) -> Vec<i32> {
    input.chars()
        .map(|c| c.to_digit(10).unwrap() as i32 )
        .collect()
}

#[aoc(day16, part1)]
pub fn solve_part1(input: &Vec<i32>) -> String {
    fft(input.clone(), 100, 0, 1)
}

#[aoc(day16, part2)]
pub fn solve_part2(input: &Vec<i32>) -> String {
    let mut long_input = Vec::new();
    for i in 0..10000 {
        long_input.append(&mut input.clone());
    }
    let offset: usize = input[0..7].iter()
                                  .map(|x| x.to_string())
                                  .collect::<String>()
                                  .parse()
                                  .unwrap();
    long_input.drain(0..offset);
    fft(long_input, 100, 0, 2)
}

mod tests {
    use super::*;

    #[test]
    fn test1() {
        let input = prepare_input("12345678"); 
        assert_eq!(fft(input, 1, 0, 1), "48226158")
    }
    
    #[test]
    fn test2() {
        let input = prepare_input("12345678"); 
        assert_eq!(fft(input, 2, 0, 1), "34040438")
    }
    
    #[test]
    fn test3() {
        let input = prepare_input("12345678"); 
        assert_eq!(fft(input, 3, 0, 1), "03415518")
    }
    
    #[test]
    fn test4() {
        let input = prepare_input("12345678"); 
        assert_eq!(fft(input, 4, 0, 1), "01029498")
    }
    
    #[test]
    fn test5() {
        let input = prepare_input("80871224585914546619083218645595"); 
        assert_eq!(fft(input, 100, 0, 1), "24176176")
    }
    
    #[test]
    fn test6() {
        let input = prepare_input("19617804207202209144916044189917"); 
        assert_eq!(fft(input, 100, 0, 1), "73745418")
    }
    
    #[test]
    fn test7() {
        let input = prepare_input("69317163492948606335995924319873"); 
        assert_eq!(fft(input, 100, 0, 1), "52432133")
    }
    
    #[test]
    fn test8() {
        let input = prepare_input("03036732577212944063491565474664"); 
        assert_eq!(solve_part2(&input), "84462026")
    }
    
    #[test]
    fn test9() {
        let input = prepare_input("02935109699940807407585447034323"); 
        assert_eq!(solve_part2(&input), "78725270")
    }
   
    #[test]
    fn test10() {
        let input = prepare_input("03081770884921959731165446850517"); 
        assert_eq!(solve_part2(&input), "53553731")
    }
}
