
#[derive(Debug)]
enum Opcode {
    Add,
    Multiply,
    Read,
    Write,
    JumpTrue,
    JumpFalse,
    Less,
    Equal,
    Halt,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum State {
    Halted,
    NeedInput,
    NewOutput,
    OK,
}

#[derive(Debug)]
pub enum Mode {
    Position,
    Immediate,
}

impl Mode {
    fn new(from: i32) -> Option<Mode> {
        match from {
            0 => Some(Mode::Position),
            1 => Some(Mode::Immediate),
            _ => None,
        }
    }
}

type ModeSet = (Mode, Mode, Mode);

impl Opcode {
    fn new(from: i32) -> Option<Opcode> {
        match from {
            1 => Some(Opcode::Add),
            2 => Some(Opcode::Multiply),
            3 => Some(Opcode::Read),
            4 => Some(Opcode::Write),
            5 => Some(Opcode::JumpTrue),
            6 => Some(Opcode::JumpFalse),
            7 => Some(Opcode::Less),
            8 => Some(Opcode::Equal),
            99 => Some(Opcode::Halt),
            _ => None,
        }
    }
}

#[derive(Debug)]
struct Instruction {
    opcode: Opcode,
    modes: (Mode, Mode, Mode),
}

impl Instruction {
    fn new(from: i32) -> Instruction {
        Instruction {
            opcode: Opcode::new(from % 100).unwrap(),
            modes: (Mode::new((from/100) % 10).unwrap(), 
                    Mode::new((from/1000) % 10).unwrap(), 
                    Mode::new((from/10000) % 10).unwrap()),
        }
    }
}

pub struct VM {
    ip: usize,
    memory: Vec<i32>,
    input: Vec<i32>,
    pub output: Vec<i32>,
    state: State,
}

impl VM {
    pub fn new(memory: Vec<i32>, input: Vec<i32>) -> VM {
        VM {
            ip: 0,
            memory: memory,
            input: input,
            output: Vec::new(),
            state: State::OK,
        }
    }

    pub fn run(&mut self) -> State {
        self.state = State::OK;
        while self.state != State::NeedInput && self.state != State::Halted {
            self.tick();
        }
        self.state
    }

    pub fn run_until_new_state(&mut self) -> State {
        self.state = State::OK;
        while self.state == State::OK {
            self.tick();
        }
        self.state
    }

    pub fn tick(&mut self) -> State {
        let instruction = Instruction::new(self.memory[self.ip]);
        match instruction.opcode {
            Opcode::Add => self.add(instruction.modes),
            Opcode::Multiply => self.multiply(instruction.modes),
            Opcode::Read => self.read(),
            Opcode::Write => self.write(instruction.modes),
            Opcode::JumpTrue => self.jump_if_true(instruction.modes),
            Opcode::JumpFalse => self.jump_if_false(instruction.modes),
            Opcode::Less => self.less(instruction.modes),
            Opcode::Equal => self.equals(instruction.modes),
            Opcode::Halt => self.state = State::Halted,
        }
        return self.state;
    }

    fn add(&mut self, modes: ModeSet) {
        let (src1, src2, dst) = self.get_addresses(modes);
        self.memory[dst] = self.memory[src1] + self.memory[src2];
        self.ip += 4;
        self.state = State::OK;
    }

    fn multiply(&mut self, modes: ModeSet) {
        let (src1, src2, dst) = self.get_addresses(modes);
        self.memory[dst] = self.memory[src1] * self.memory[src2];
        self.ip += 4;
        self.state = State::OK;
    }

    fn read(&mut self) {
        if self.input.is_empty() {
            self.state = State::NeedInput;
        }else {
            let dst = self.get_address(1, Mode::Position);
            self.memory[dst] = self.input.remove(0);
            self.ip += 2;
            self.state = State::OK;
        }
    }

    fn write(&mut self, modes: ModeSet) {
        let src = self.get_address(1, modes.0);
        self.output.push(self.memory[src]);
        self.ip += 2;
        self.state = State::NewOutput;
    }

    fn jump_if_true(&mut self, modes: ModeSet) {
        let (src1, src2, _) = self.get_addresses(modes);
        if self.memory[src1] != 0 {
            self.ip = self.memory[src2] as usize;
        }else {
            self.ip += 3;
        }
        self.state = State::OK;
    }

    fn jump_if_false(&mut self, modes: ModeSet) {
        let (src1, src2, _) = self.get_addresses(modes);
        if self.memory[src1] == 0 {
            self.ip = self.memory[src2] as usize;
        }else {
            self.ip += 3;
        }
        self.state = State::OK;
    }

    fn less(&mut self, modes: ModeSet) {
        let (src1, src2, dst) = self.get_addresses(modes);
        if self.memory[src1] < self.memory[src2] {
            self.memory[dst] = 1;
        }else {
            self.memory[dst] = 0;
        }
        self.ip += 4;
        self.state = State::OK;
    }

    fn equals(&mut self, modes: ModeSet) {
        let (src1, src2, dst) = self.get_addresses(modes);
        if self.memory[src1] == self.memory[src2] {
            self.memory[dst] = 1;
        }else {
            self.memory[dst] = 0;
        }
        self.ip += 4;
        self.state = State::OK;
    }

    fn get_addresses(&mut self, modes: ModeSet) -> (usize, usize, usize) {
        (self.get_address(1, modes.0),
         self.get_address(2, modes.1),
         self.get_address(3, modes.2))
    }

    fn get_address(&mut self, offset: usize, mode: Mode) -> usize {
        match mode {
            Mode::Position => self.memory[self.ip + offset] as usize,
            Mode::Immediate => self.ip + offset,
        }
    }

    pub fn get(self, index: usize) -> i32 {
        self.memory[index]
    }

    pub fn feed_input(&mut self, item: i32) {
        self.input.push(item);
    }

}
