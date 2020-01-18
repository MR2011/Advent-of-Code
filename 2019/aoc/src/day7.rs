use crate::intcode::VM;
use crate::intcode::State;
use permute;
use std::cmp::max;

pub struct AmplifierCircuit {
    amps: Vec<VM>,
}

impl AmplifierCircuit{
    fn new(program: &Vec<i64>, phases: Vec<i64>) -> AmplifierCircuit {
        AmplifierCircuit {
            amps: phases
                    .iter()
                    .map(|&p| VM::new(program.clone(), vec![p]))
                    .collect()
        }
    }


    fn run(&mut self, init: i64) -> i64 {
        let mut output = init;
        for amp in &mut self.amps {
            amp.feed_input(output);
            amp.run();
            output = amp.output.pop().unwrap();
        }
        output
    }

    fn run_feedback(&mut self) -> i64 {
        let mut output = 0;
        let mut halted = false;
        while !halted {
            for (i, amp) in self.amps.iter_mut().enumerate() {
                amp.feed_input(output);
                match amp.run_until_new_state() {
                    State::NewOutput => output = amp.output.pop().unwrap(),
                    State::Halted => halted = i == 4,
                    State::NeedInput => break,
                    State::OK => continue,
                }

            }
        }
        output
    }
}

#[aoc_generator(day7)]
pub fn prepare_input(input: &str) -> Vec<i64> {
    let tokens: Vec<i64> = input.trim().split(",")
        .map(|token| token.parse::<i64>().unwrap())
        .collect();
    tokens
}

#[aoc(day7, part1)]
pub fn solve_part1(input: &Vec<i64>) -> i64 {
    let mut max_signal = 0;
    for phases in permute::permute(vec![0, 1, 2, 3, 4]) {
        let mut circuit = AmplifierCircuit::new(input, phases);
        max_signal = max(max_signal, circuit.run(0));
    }
    max_signal
}

#[aoc(day7, part2)]
pub fn solve_part2(input: &Vec<i64>) -> i64 {
    let mut max_signal = 0;
    for phases in permute::permute(vec![5, 6, 7, 8, 9]) {
        let mut circuit = AmplifierCircuit::new(input, phases);
        max_signal = max(max_signal, circuit.run_feedback());
    }
    max_signal
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test1() {
        let input = "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0";
        let tokens = prepare_input(input);
        assert_eq!(solve_part1(&tokens), 43210);
    }

    #[test]
    fn test2() {
        let input = "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0";
        let tokens = prepare_input(input);
        assert_eq!(solve_part1(&tokens), 54321);
    }

    #[test]
    fn test3() {
        let input = "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0";
        let tokens = prepare_input(input);
        assert_eq!(solve_part1(&tokens), 65210);
    }

    #[test]
    fn test4() {
        let input ="3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5";
        let tokens = prepare_input(input);
        assert_eq!(solve_part2(&tokens), 139629729);
    }

    #[test]
    fn test5() {
        let input = "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10";
        let tokens = prepare_input(input);
        assert_eq!(solve_part2(&tokens), 18216);
    }
}

