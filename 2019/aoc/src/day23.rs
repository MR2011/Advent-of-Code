use crate::intcode::State;
use crate::intcode::VM;
use std::collections::VecDeque;

#[derive(Copy, Clone)]
struct Message {
    address: i64,
    x: i64,
    y: i64,
}

struct Computer {
    address: i64,
    vm: VM,
    message_queue: VecDeque<Message>,
    needsInput: bool,
}

impl Computer {
    pub fn new(address: usize, program: Vec<i64>) -> Computer {
        Computer {
            address: address as i64,
            vm: VM::new(program, vec![address as i64]),
            message_queue: VecDeque::new(),
            needsInput: false,
        }
    }

    pub fn step(&mut self) -> Option<Message> {
        match self.vm.run_until_new_state() {
            State::NeedInput => {
                match self.message_queue.pop_front() {
                    Some(msg) => {
                        self.vm.feed_input(msg.x);
                        self.vm.feed_input(msg.y);
                    }
                    None => {
                        self.vm.feed_input(-1);
                        self.needsInput = true;
                    }
                }
                None
            }
            State::NewOutput => {
                let address = *self.vm.output.last().unwrap();
                self.vm.run_until_new_state();
                let x = *self.vm.output.last().unwrap();
                self.vm.run_until_new_state();
                let y = *self.vm.output.last().unwrap();
                Some(Message {
                    address: address,
                    x: x,
                    y: y,
                })
            }
            State::Halted => None,
            State::OK => None,
        }
    }

    pub fn push_msg(&mut self, msg: Message) {
        self.message_queue.push_back(msg);
        self.needsInput = false;
    }

    pub fn idle(&self) -> bool {
        self.needsInput && self.message_queue.is_empty()
    }
}

#[aoc_generator(day23)]
pub fn prepare_input(input: &str) -> Vec<i64> {
    let tokens: Vec<i64> = input
        .trim()
        .split(",")
        .map(|token| token.parse::<i64>().unwrap())
        .collect();
    tokens
}

#[aoc(day23, part1)]
pub fn solve_part1(input: &Vec<i64>) -> i64 {
    let mut computers = Vec::new();
    for i in (0..50) {
        computers.push(Computer::new(i, input.clone()));
    }
    loop {
        for i in (0..50) {
            match computers[i].step() {
                Some(msg) => {
                    if msg.address == 255 {
                        return msg.y;
                    } else {
                        computers[msg.address as usize].push_msg(msg);
                    }
                }
                None => (),
            }
        }
    }
}

#[aoc(day23, part2)]
pub fn solve_part2(input: &Vec<i64>) -> i64 {
    let mut computers = Vec::new();
    let mut nat: Option<Message> = None;
    let mut last_nat_y: Option<i64> = None;
    for i in (0..50) {
        computers.push(Computer::new(i, input.clone()));
    }
    loop {
        for i in (0..50) {
            match computers[i].step() {
                Some(msg) => {
                    if msg.address == 255 {
                        nat = Some(msg);
                    } else {
                        computers[msg.address as usize].push_msg(msg);
                    }
                }
                None => (),
            }
        }
        // all idle?
        if computers.iter().all(|c| c.idle()) {
            if let Some(msg) = nat.take() {
                if let Some(y) = last_nat_y.take() {
                    if msg.y == y {
                        return y;
                    }
                }
                last_nat_y = Some(msg.y);
                computers[0].push_msg(msg);
            }
        }
    }
}
