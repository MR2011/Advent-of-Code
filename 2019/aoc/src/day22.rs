use mod_exp::mod_exp;
use std::cmp::Ordering;

pub enum Shuffle {
    Stack,
    Deal(usize),
    Cut(isize),
}

#[aoc_generator(day22)]
pub fn prepare_input(input: &str) -> Vec<Shuffle> {
    let mut shuffles = Vec::new();
    input.lines().for_each(|line| {
        if line.contains("stack") {
            shuffles.push(Shuffle::Stack);
        } else if line.contains("deal") {
            let n: usize = line.split(" ").last().unwrap().parse().unwrap();
            shuffles.push(Shuffle::Deal(n));
        } else if line.contains("cut") {
            let n: isize = line.split(" ").last().unwrap().parse().unwrap();
            shuffles.push(Shuffle::Cut(n));
        }
    });
    shuffles
}

#[aoc(day22, part1)]
pub fn solve_part1(input: &Vec<Shuffle>) -> usize {
    let mut deck: Vec<i32> = (0..10007i32).collect();
    for shuffle in input.iter() {
        match shuffle {
            Shuffle::Stack => {
                deck.reverse();
            }
            Shuffle::Cut(n) => match n.cmp(&0) {
                Ordering::Less => {
                    let i = n.abs() as usize % deck.len();
                    deck.rotate_right(i)
                }
                Ordering::Greater => {
                    let i = n.abs() as usize % deck.len();
                    deck.rotate_left(i)
                }
                Ordering::Equal => (),
            },
            Shuffle::Deal(n) => {
                let mut cut = deck.clone();
                let mut index = 0;
                for (i, val) in deck.iter().enumerate() {
                    cut[index] = *val;
                    index = (index + n) % deck.len();
                }
                deck = cut.clone();
            }
            _ => (),
        }
    }
    deck.iter().position(|&s| s == 2019).unwrap()
}

#[aoc(day22, part2)]
pub fn solve_part2(input: &Vec<Shuffle>) -> i128 {
    // Source:
    // https://github.com/zedrdave/advent_of_code/blob/master/2019/22/__main__.py
    let m: i128 = 119315717514047;
    let n: i128 = 101741582076661;
    let mut coef = (1, 0);
    let deal = |x: i128, a: i128, b: i128| (a * x % m, b * x % m);
    let stack = |a: i128, b: i128| (-a % m, (m - 1 - b) % m);
    let cut = |x: i128, a: i128, b: i128| (a, (b - x) % m);
    let pos = 2020;
    for shuffle in input.iter() {
        match shuffle {
            Shuffle::Stack => {
                coef = stack(coef.0, coef.1);
            }
            Shuffle::Cut(n) => {
                coef = cut(*n as i128, coef.0, coef.1);
            }
            Shuffle::Deal(n) => {
                coef = deal(*n as i128, coef.0, coef.1);
            }
            _ => (),
        }
    }
    let a = coef.0;
    let b = coef.1;
    let r = (b * mod_exp(1 - a, (m - 2), m)) % m + m;
    ((pos - r) * mod_exp(a, n * (m - 2) , m) + r) % m + m
}
