
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
    AdjustRelativeBase,
    Halt,
}

#[derive(Debug, PartialEq, Copy, Clone, Hash, Eq)]
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
    Relative,
}

impl Mode {
    fn new(from: i64) -> Option<Mode> {
        match from {
            0 => Some(Mode::Position),
            1 => Some(Mode::Immediate),
            2 => Some(Mode::Relative),
            _ => None,
        }
    }
}

type ModeSet = (Mode, Mode, Mode);

impl Opcode {
    fn new(from: i64) -> Option<Opcode> {
        match from {
            1 => Some(Opcode::Add),
            2 => Some(Opcode::Multiply),
            3 => Some(Opcode::Read),
            4 => Some(Opcode::Write),
            5 => Some(Opcode::JumpTrue),
            6 => Some(Opcode::JumpFalse),
            7 => Some(Opcode::Less),
            8 => Some(Opcode::Equal),
            9 => Some(Opcode::AdjustRelativeBase),
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
    fn new(from: i64) -> Instruction {
        Instruction {
            opcode: Opcode::new(from % 100).unwrap(),
            modes: (Mode::new((from/100) % 10).unwrap(), 
                    Mode::new((from/1000) % 10).unwrap(), 
                    Mode::new((from/10000) % 10).unwrap()),
        }
    }
}

#[derive(Clone, Hash, Eq, PartialEq)]
pub struct VM {
    ip: usize,
    memory: Vec<i64>,
    input: Vec<i64>,
    pub output: Vec<i64>,
    state: State,
    base: isize,
}

impl VM {
    pub fn new(memory: Vec<i64>, input: Vec<i64>) -> VM {
        VM {
            ip: 0,
            memory: memory,
            input: input,
            output: Vec::new(),
            state: State::OK,
            base: 0,
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
        let instruction = Instruction::new(self.get(self.ip));
        match instruction.opcode {
            Opcode::Add => self.add(instruction.modes),
            Opcode::Multiply => self.multiply(instruction.modes),
            Opcode::Read => self.read(instruction.modes),
            Opcode::Write => self.write(instruction.modes),
            Opcode::JumpTrue => self.jump_if_true(instruction.modes),
            Opcode::JumpFalse => self.jump_if_false(instruction.modes),
            Opcode::Less => self.less(instruction.modes),
            Opcode::Equal => self.equals(instruction.modes),
            Opcode::AdjustRelativeBase => self.adjust_relative_base(instruction.modes),
            Opcode::Halt => self.state = State::Halted,
        }
        return self.state;
    }

    fn add(&mut self, modes: ModeSet) {
        let (src1, src2, dst) = self.get_addresses(modes);
        let result = self.get(src1) + self.get(src2);
        self.set(dst, result);
        self.ip += 4;
        self.state = State::OK;
    }

    fn multiply(&mut self, modes: ModeSet) {
        let (src1, src2, dst) = self.get_addresses(modes);
        let result = self.get(src1) * self.get(src2);
        self.set(dst, result);
        self.ip += 4;
        self.state = State::OK;
    }

    fn read(&mut self, modes: ModeSet) {
        if self.input.is_empty() {
            self.state = State::NeedInput;
        }else {
            let (dst, _, _) = self.get_addresses(modes);
            let value = self.input.remove(0);
            self.set(dst, value);
            self.ip += 2;
            self.state = State::OK;
        }
    }

    fn write(&mut self, modes: ModeSet) {
        let (src, _, _) = self.get_addresses(modes);
        let value = self.get(src);
        self.output.push(value);
        self.ip += 2;
        self.state = State::NewOutput;
    }

    fn jump_if_true(&mut self, modes: ModeSet) {
        let (src1, src2, _) = self.get_addresses(modes);
        if self.get(src1) != 0 {
            self.ip = self.get(src2) as usize;
        }else {
            self.ip += 3;
        }
        self.state = State::OK;
    }

    fn jump_if_false(&mut self, modes: ModeSet) {
        let (src1, src2, _) = self.get_addresses(modes);
        if self.get(src1) == 0 {
            self.ip = self.get(src2) as usize;
        }else {
            self.ip += 3;
        }
        self.state = State::OK;
    }

    fn less(&mut self, modes: ModeSet) {
        let (src1, src2, dst) = self.get_addresses(modes);
        if self.get(src1) < self.get(src2) {
            self.set(dst, 1);
        }else {
            self.set(dst, 0);
        }
        self.ip += 4;
        self.state = State::OK;
    }

    fn equals(&mut self, modes: ModeSet) {
        let (src1, src2, dst) = self.get_addresses(modes);
        if self.get(src1) == self.get(src2) {
            self.set(dst, 1);
        }else {
            self.set(dst, 0);
        }
        self.ip += 4;
        self.state = State::OK;
    }

    fn adjust_relative_base(&mut self, modes: ModeSet) {
        let (src, _, _) = self.get_addresses(modes);
        self.base += self.get(src) as isize ;
        self.ip += 2;
        self.state = State::OK;
    }

    fn get_addresses(&mut self, modes: ModeSet) -> (usize, usize, usize) {
        (self.get_address(1, modes.0),
         self.get_address(2, modes.1),
         self.get_address(3, modes.2))
    }

    fn get_address(&mut self, offset: usize, mode: Mode) -> usize {
        let address = self.ip + offset;
        match mode {
            Mode::Position => self.get(address) as usize,
            Mode::Immediate => address,
            Mode::Relative => (self.get(address) as isize + self.base) as usize,
        }
    }

    // resize if index out of bounds would happen
    fn prevent_oob(&mut self, dst: usize) {
        if dst >= self.memory.len() {
            self.memory.resize(dst + 1, 0);
        }
    }

    pub fn get(&mut self, index: usize) -> i64 {
        self.prevent_oob(index);
        self.memory[index]
    }

    pub fn  set(&mut self, dst: usize, value: i64) {
        self.prevent_oob(dst);
        self.memory[dst] = value;
    }

    pub fn feed_input(&mut self, item: i64) {
        self.input.push(item);
    }

}
