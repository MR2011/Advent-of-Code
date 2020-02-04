use std::collections::HashMap;
use std::cmp::max;
use std::cmp::min;
use std::cmp::Ordering;

#[derive(Debug)]
pub struct Reaction {
    input: Vec<Chemical>,
    output: Chemical,
}

#[derive(Debug)]
pub struct Chemical {
    name: String,
    amount: i64
}

fn calculate_ore(chemical: &String, amount: i64, reactions: &HashMap<String, Reaction>,
                 supply: &mut HashMap<String, i64>) -> i64 {
    let reaction = reactions.get(chemical).unwrap();
    let available = match supply.get(chemical){
        Some(n) => *n,
        None => 0,
    };
    let pending = (max(0, amount - available) as f64 
                   / reaction.output.amount as f64).ceil();
    let surplus = (pending as i64 * reaction.output.amount) - (amount - available);
    if chemical != "ORE" {
        supply.insert(chemical.to_string(), surplus);
    }
    let mut ore = 0;
    for c in reaction.input.iter() {
        if c.name == "ORE" {
            ore += pending as i64 * c.amount;
        }else {
            ore += calculate_ore(&c.name, c.amount * pending as i64, reactions, supply);
        }
    }
    ore
}

fn parse_chemical(input: &str) -> Chemical {
    let tokens: Vec<&str> = input.trim().split(" ").collect();
    let chemical = tokens[1];
    let amount: i64 = tokens[0].parse().unwrap();
    Chemical{name: chemical.to_string(), amount: amount}
}

#[aoc_generator(day14)]
pub fn prepare_input(input: &str) -> HashMap<String, Reaction> {
    let mut reactions = HashMap::new();
    for reaction in input.lines() {
        let tokens: Vec<&str> = reaction.split("=>").collect();
        let input_chemicals: Vec<Chemical> = tokens[0].split(",")
                                                      .map(parse_chemical)
                                                      .collect();
        let output_chemical = parse_chemical(tokens[1]);
        reactions.insert(output_chemical.name.clone(), 
                         Reaction{input: input_chemicals, output: output_chemical} );
    }
    reactions
}

#[aoc(day14, part1)]
pub fn solve_part1(input: &HashMap<String, Reaction>) -> i64 {
    calculate_ore(&"FUEL".to_string(), 1, input, &mut HashMap::new())
}

#[aoc(day14, part2)]
pub fn solve_part2(input: &HashMap<String, Reaction>) -> i64 {
    let available: i64 = 1000000000000;
    let mut fuel = calculate_ore(&"FUEL".to_string(), 1, input, &mut HashMap::new());
    let mut low = available / fuel;
    let mut high = low * 2;
    while low < high {
        fuel = (low + high) / 2; 
        let ore = calculate_ore(&"FUEL".to_string(), fuel, input, &mut HashMap::new());
        match ore.cmp(&available) {
            Ordering::Less => low = max(fuel, low +1),
            Ordering::Greater => high = min(fuel, high-1),
            Ordering::Equal => break,
        };
    }
    fuel
}
