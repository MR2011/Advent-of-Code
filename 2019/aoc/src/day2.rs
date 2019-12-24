enum Opcode {
    ADD,
    MULTIPLY,
    HALT,
}

impl Opcode {
    fn new(from: i32) -> Option<Opcode> {
        match from {
            1 => Some(Opcode::ADD),
            2 => Some(Opcode::MULTIPLY),
            99 => Some(Opcode::HALT),
            _ => None,
        }
    }
}
struct VM {
    ip: usize,
    memory: Vec<i32>,
}

impl VM {
    pub fn new(input: Vec<i32>) -> VM {
        VM {
            ip: 0,
            memory: input,
        }
    }

    fn run(&mut self) {
        loop {
            match Opcode::new(self.memory[self.ip]) {
                Some(Opcode::ADD) => self.add(),
                Some(Opcode::MULTIPLY) => self.multiply(),
                Some(Opcode::HALT) => break,
                _ => break,
            }
        }
    }

    fn add(&mut self) {
        let (dst, src2, src1) = self.get_operators();
        self.memory[dst] = self.memory[src1] + self.memory[src2];
        self.ip += 4;
    }

    fn multiply(&mut self) {
        let (dst, src2, src1) = self.get_operators();
        self.memory[dst] = self.memory[src1] * self.memory[src2];
        self.ip += 4;
    }

    fn get_operators(&mut self) -> (usize, usize, usize) {
        (self.memory[self.ip+3] as usize,
         self.memory[self.ip+2] as usize,
         self.memory[self.ip+1] as usize)
    }
}

#[aoc_generator(day2)]
pub fn prepare_input(input: &str) -> Vec<i32> {
    let mut tokens = prepare_test_input(input);
    tokens[1] = 12; //noun
    tokens[2] = 2; //verb
    tokens

}
pub fn prepare_test_input(input: &str) -> Vec<i32> {
    let tokens: Vec<i32> = input.trim().split(",")
        .map(|token| token.parse::<i32>().unwrap())
        .collect();
    tokens
}

#[aoc(day2, part1)]
pub fn solve_part1(input: &Vec<i32>) -> i32 {
    let mut vm = VM::new(input.clone());
    vm.run();
    vm.memory[0]
}

#[aoc(day2, part2)]
pub fn solve_part2(input: &Vec<i32>) -> i32 {
    for noun in 0..100 {
        for verb in 0..100 {
            let mut input_clone = input.clone();
            input_clone[1] = noun;
            input_clone[2] = verb;
            let mut vm = VM::new(input_clone);
            vm.run();
            if vm.memory[0] == 19690720 {
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

