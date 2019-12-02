use std::fs::read_to_string;

fn run(mut tokens: Vec<usize>, noun: usize, verb: usize) -> usize {
    tokens[1] = noun;
    tokens[2] = verb;
    let mut i = 0;
    loop {
        match tokens[i] {
            1 => {
                let a = tokens[i+1];
                let b = tokens[i+2];
                let c = tokens[i+3];
                tokens[c] = tokens[a] + tokens[b];
            }
            2 => {
                let a = tokens[i+1];
                let b = tokens[i+2];
                let c = tokens[i+3];
                tokens[c] = tokens[a] * tokens[b];
            }
            99 => {
                break
            }
            _ => {
            }
        }
        i += 4;
    }
    tokens[0]
}

fn main() {
    let input = read_to_string("input").expect("File not found!");
    let tokens: Vec<usize> = input.trim().split(",")
        .map(|token| token.parse::<usize>().unwrap())
        .collect();
    let x = run(tokens.clone(), 12, 2);
    println!("Part 1: {:?}", x);

    for noun in 0..100 {
        for verb in 0..100 {
            if run(tokens.clone(), noun, verb) == 19690720 {
                println!("Part 2: {}", 100 * noun + verb);
            }
        }
    }

}
