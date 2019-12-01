use std::fs::read_to_string;

fn main() {
    let input = read_to_string("input").expect("File not found!");
    let module_masses: Vec<i32> = input.lines()
        .map(|line| line.parse::<i32>().unwrap())
        .collect();
    //Part 1
    let fuel_modules: i32 = module_masses.iter()
        .map(|mass| mass / 3 - 2)
        .sum();
    println!("{}", fuel_modules);

    //Part 2
    let total_fuel_modules: i32 = module_masses.iter()
        .map(|mass| total_fuel(*mass) - mass)
        .sum();
    println!("{}", total_fuel_modules);
}

fn total_fuel(mass: i32) -> i32 {
    if mass <= 0 {
        return 0;
    }else{
        return mass + total_fuel(mass / 3 - 2);
    }
}
