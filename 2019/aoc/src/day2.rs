use crate::intcode::VM;

#[aoc_generator(day2)]
pub fn prepare_input(input: &str) -> Vec<i64> {
    let mut tokens = prepare_test_input(input);
    tokens[1] = 12; //noun
    tokens[2] = 2; //verb
    tokens

}
pub fn prepare_test_input(input: &str) -> Vec<i64> {
    let tokens: Vec<i64> = input.trim().split(",")
        .map(|token| token.parse::<i64>().unwrap())
        .collect();
    tokens
}

#[aoc(day2, part1)]
pub fn solve_part1(input: &Vec<i64>) -> i64 {
    let mut vm = VM::new(input.clone(), Vec::new());
    vm.run();
    vm.get(0)
}

#[aoc(day2, part2)]
pub fn solve_part2(input: &Vec<i64>) -> i64 {
    for noun in 0..100 {
        for verb in 0..100 {
            let mut input_clone = input.clone();
            input_clone[1] = noun;
            input_clone[2] = verb;
            let mut vm = VM::new(input_clone, Vec::new());
            vm.run();
            if vm.get(0) == 19690720 {
                return 100 * noun + verb;
            }
        }
    }
    return -1;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test1() {
        let input = prepare_test_input("1,9,10,3,2,3,11,0,99,30,40,50");
        assert_eq!(solve_part1(&input), 3500);
    }
    
    #[test]
    fn test2() {
        let input = prepare_test_input("1,0,0,0,99");
        assert_eq!(solve_part1(&input), 2);
    }

    #[test]
    fn test3() {
        let input = prepare_test_input("2,3,0,3,99"); 
        assert_eq!(solve_part1(&input), 2);
    }
    
    #[test]
    fn test4() {
        let input = prepare_test_input("2,4,4,5,99,0");
        assert_eq!(solve_part1(&input), 2);
    }

    #[test]
    fn test5() {
        let input = prepare_test_input("1,1,1,4,99,5,6,0,99"); 
        assert_eq!(solve_part1(&input), 30);
    }
}

