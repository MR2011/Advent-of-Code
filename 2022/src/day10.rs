enum Command {
    Addx(i32),
    Noop,
}

struct CPU {
    x: i32,
    cycle: usize,
    pixels: Vec<char>,
}

impl CPU {
    fn init() -> Self {
        CPU {
            x: 1,
            cycle: 1,
            pixels: vec![],
        }
    }

    fn run(&mut self, program: Vec<Command>) -> i32 {
        let mut next_cycle = 20;
        let mut signal = 0;
        self.calculate_pixel();
        for cmd in program {
            match cmd {
                Command::Addx(i) => {
                    self.cycle += 1;
                    self.calculate_signal(&mut signal, &mut next_cycle);
                    self.calculate_pixel();

                    self.cycle += 1;
                    self.x += i;
                    self.calculate_signal(&mut signal, &mut next_cycle);
                    self.calculate_pixel();
                }
                Command::Noop => {
                    self.cycle += 1;
                    self.calculate_signal(&mut signal, &mut next_cycle);
                    self.calculate_pixel();
                }
            }
        }
        signal
    }

    fn calculate_signal(&self, signal: &mut i32, next_cycle: &mut usize) {
        if self.cycle == *next_cycle {
            *signal += self.x * self.cycle as i32;
            *next_cycle += 40;
        }
    }

    fn calculate_pixel(&mut self) {
        let sprite = vec![self.x - 1, self.x, self.x + 1];
        let pixel_pos = ((self.cycle  - 1) % 40) as i32;

        if sprite.contains(&pixel_pos) {
            self.pixels.push('#');
        } else {
            self.pixels.push('.');
        }
    }

    fn draw(&self) {
        self.pixels.chunks(40).for_each(|c| {
            println!("{}", c.iter().collect::<String>());
        });
    }
}

fn parse_commands(input: &str) -> Vec<Command> {
    input
        .lines()
        .map(|l| match &l[..4] {
            "addx" => {
                let (_, last) = l.split_once(" ").unwrap();
                let num = last.parse::<i32>().unwrap();
                Command::Addx(num)
            }
            "noop" => Command::Noop,
            _ => {
                panic!("Unknown cmd")
            }
        })
        .collect()
}

#[aoc(day10, part1)]
pub fn solve_part1(input: &str) -> i32 {
    let cmds = parse_commands(input);
    let mut cpu = CPU::init();
    cpu.run(cmds)
}

#[aoc(day10, part2)]
pub fn solve_part2(input: &str) -> i32 {
    let cmds = parse_commands(input);
    let mut cpu = CPU::init();
    cpu.run(cmds);
    cpu.draw();
    2
}
