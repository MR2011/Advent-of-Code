
#[derive(Debug)]
enum Opcode {
    ADD,
    MULTIPLY,
    READ,
    WRITE,
    JUMP_TRUE,
    JUMP_FALSE,
    LESS,
    EQUAL,
    HALT,
}

#[derive(Debug)]
pub enum Mode {
    POSITION,
    IMMEDIATE,
}

impl Mode {
    fn new(from: i32) -> Option<Mode> {
        match from {
            0 => Some(Mode::POSITION),
            1 => Some(Mode::IMMEDIATE),
            _ => None,
        }
    }
}

type ModeSet = (Mode, Mode, Mode);

impl Opcode {
    fn new(from: i32) -> Option<Opcode> {
        match from {
            1 => Some(Opcode::ADD),
            2 => Some(Opcode::MULTIPLY),
            3 => Some(Opcode::READ),
            4 => Some(Opcode::WRITE),
            5 => Some(Opcode::JUMP_TRUE),
            6 => Some(Opcode::JUMP_FALSE),
            7 => Some(Opcode::LESS),
            8 => Some(Opcode::EQUAL),
            99 => Some(Opcode::HALT),
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
}

impl VM {
    pub fn new(memory: Vec<i32>, input: Vec<i32>) -> VM {
        VM {
            ip: 0,
            memory: memory,
            input: input,
            output: Vec::new(),
        }
    }

    pub fn run(&mut self) {
        loop {
            let instruction = Instruction::new(self.memory[self.ip]);
            match instruction.opcode {
                Opcode::ADD => self.add(instruction.modes),
                Opcode::MULTIPLY => self.multiply(instruction.modes),
                Opcode::READ => self.read(),
                Opcode::WRITE => self.write(instruction.modes),
                Opcode::JUMP_TRUE => self.jump_if_true(instruction.modes),
                Opcode::JUMP_FALSE => self.jump_if_false(instruction.modes),
                Opcode::LESS => self.less(instruction.modes),
                Opcode::EQUAL => self.equals(instruction.modes),
                Opcode::HALT => break,
            }
        }
    }

    fn add(&mut self, modes: ModeSet) {
        let (src1, src2, dst) = self.get_addresses(modes);
        self.memory[dst] = self.memory[src1] + self.memory[src2];
        self.ip += 4;
    }

    fn multiply(&mut self, modes: ModeSet) {
        let (src1, src2, dst) = self.get_addresses(modes);
        self.memory[dst] = self.memory[src1] * self.memory[src2];
        self.ip += 4;
    }

    fn read(&mut self) {
        let dst = self.get_address(1, Mode::POSITION);
        self.memory[dst] = self.input.remove(0);
        self.ip += 2;
    }

    fn write(&mut self, modes: ModeSet) {
        let src = self.get_address(1, modes.0);
        self.output.push(self.memory[src]);
        self.ip += 2;
    }

    fn jump_if_true(&mut self, modes: ModeSet) {
        let (src1, src2, _) = self.get_addresses(modes);
        if self.memory[src1] != 0 {
            self.ip = self.memory[src2] as usize;
        }else {
            self.ip += 3;
        }
    }

    fn jump_if_false(&mut self, modes: ModeSet) {
        let (src1, src2, _) = self.get_addresses(modes);
        if self.memory[src1] == 0 {
            self.ip = self.memory[src2] as usize;
        }else {
            self.ip += 3;
        }
    }

    fn less(&mut self, modes: ModeSet) {
        let (src1, src2, dst) = self.get_addresses(modes);
        if self.memory[src1] < self.memory[src2] {
            self.memory[dst] = 1;
        }else {
            self.memory[dst] = 0;
        }
        self.ip += 4;
    }

    fn equals(&mut self, modes: ModeSet) {
        let (src1, src2, dst) = self.get_addresses(modes);
        if self.memory[src1] == self.memory[src2] {
            self.memory[dst] = 1;
        }else {
            self.memory[dst] = 0;
        }
        self.ip += 4;
    }

    fn get_addresses(&mut self, modes: ModeSet) -> (usize, usize, usize) {
        (self.get_address(1, modes.0),
         self.get_address(2, modes.1),
         self.get_address(3, modes.2))
    }

    fn get_address(&mut self, offset: usize, mode: Mode) -> usize {
        match mode {
            Mode::POSITION => self.memory[self.ip + offset] as usize,
            Mode::IMMEDIATE => self.ip + offset,
        }
    }

    pub fn get(self, index: usize) -> i32 {
        self.memory[index]
    }
}
