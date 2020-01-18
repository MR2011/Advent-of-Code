use crate::intcode::VM;

#[aoc_generator(day9)]
pub fn prepare_input(input: &str) -> Vec<i64> {
    let tokens: Vec<i64> = input.trim().split(",")
        .map(|token| token.parse::<i64>().unwrap())
        .collect();
    tokens
}

#[aoc(day9, part1)]
pub fn solve_part1(input: &Vec<i64>) -> i64 {
    let mut read_input = Vec::new();
    read_input.push(1);
    let mut vm = VM::new(input.clone(), read_input);
    let state = vm.run();
    *vm.output.last().unwrap()
}

#[aoc(day9, part2)]
pub fn solve_part2(input: &Vec<i64>) -> i64 {
    let mut read_input = Vec::new();
    read_input.push(2);
    let mut vm = VM::new(input.clone(), read_input);
    vm.run();
    *vm.output.last().unwrap()
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test1() {
        let input = "109,-1,4,1,99";
        let tokens = prepare_input(input);
        assert_eq!(solve_part1(&tokens), -1)
    }

    #[test]
    fn test2() {
        let input = "109,-1,104,1,99";
        let tokens = prepare_input(input);
        assert_eq!(solve_part1(&tokens), 1)
    }
    
    #[test]
    fn test3() {
        let input = "109,-1,204,1,99";
        let tokens = prepare_input(input);
        assert_eq!(solve_part1(&tokens), 109)
    }

    #[test]
    fn test4() {
        let input = "109,1,9,2,204,-6,99";
        let tokens = prepare_input(input);
        assert_eq!(solve_part1(&tokens), 204)
    }

    #[test]
    fn test5() {
        let input = "109,1,109,9,204,-6,99";
        let tokens = prepare_input(input);
        assert_eq!(solve_part1(&tokens), 204)
    }

    #[test]
    fn test6() {
        let input = "109,1,209,-1,204,-106,99";
        let tokens = prepare_input(input);
        assert_eq!(solve_part1(&tokens), 204)
    }

    #[test]
    fn test7() {
        let input = "109,1,3,3,204,2,99";
        let tokens = prepare_input(input);
        assert_eq!(solve_part1(&tokens), 1)
    }

    #[test]
    fn test8() {
        let input = "109,1,203,2,204,2,99";
        let tokens = prepare_input(input);
        assert_eq!(solve_part1(&tokens), 1)
    }
}
