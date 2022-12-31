use itertools::Itertools;
use std::collections::VecDeque;

#[derive(Debug)]
enum Operation {
    Add(usize),
    Mul(usize),
    Square,
}

#[derive(Debug)]
struct Monkey {
    items: VecDeque<usize>,
    operation: Operation,
    test: usize,
    if_true: usize,
    if_false: usize,
    inspected: usize,
}

impl Monkey {
    fn inspect(&mut self) {
        for item in self.items.iter_mut() {
            *item = match self.operation {
                Operation::Add(n) => *item + n,
                Operation::Mul(n) => *item * n,
                Operation::Square => *item * *item,
            };
            self.inspected += 1;
        }
    }

    fn throw<R>(&mut self, reduction: R) -> Vec<(usize, usize)>
    where
        R: Fn(usize) -> usize,
    {
        let mut items: Vec<(usize, usize)> = Vec::new();
        while !self.items.is_empty() {
            let item = reduction(self.items.pop_front().unwrap());
            if (item % self.test) == 0 {
                items.push((self.if_true, item));
            } else {
                items.push((self.if_false, item));
            }
        }
        items
    }
}

fn parse_monkey(input: &str) -> Monkey {
    let (l1, l2, l3, l4, l5) = input.lines().skip(1).collect_tuple().unwrap();

    let items: Vec<usize> = l1
        .strip_prefix("  Starting items:")
        .unwrap()
        .split(", ")
        .map(|n| n.trim().parse::<usize>().unwrap())
        .collect();

    let op;
    if l2.contains("old * old") {
        op = Operation::Square;
    } else {
        let last = l2.split(" ").last().unwrap();
        let num = last.parse::<usize>().unwrap();
        op = match l2.contains("+") {
            true => Operation::Add(num),
            false => Operation::Mul(num),
        }
    }

    let test = l3
        .strip_prefix("  Test: divisible by ")
        .unwrap()
        .parse::<usize>()
        .unwrap();

    let if_true = l4
        .strip_prefix("    If true: throw to monkey ")
        .unwrap()
        .parse::<usize>()
        .unwrap();
    let if_false = l5
        .strip_prefix("    If false: throw to monkey ")
        .unwrap()
        .parse::<usize>()
        .unwrap();

    Monkey {
        items: VecDeque::from(items),
        operation: op,
        test: test,
        if_true: if_true,
        if_false: if_false,
        inspected: 0,
    }
}

fn parse_input(input: &str) -> Vec<Monkey> {
    input.split("\n\n").map(|m| parse_monkey(m)).collect()
}

#[aoc(day11, part1)]
pub fn solve_part1(input: &str) -> usize {
    let mut monkeys = parse_input(input);
    for _ in 0..20 {
        for m in 0..monkeys.len() {
            monkeys[m].inspect();
            for (target, item) in monkeys[m].throw(|x| x / 3) {
                monkeys[target].items.push_back(item);
            }
        }
    }
    let mut monkey_business = monkeys
        .iter()
        .map(|m| m.inspected)
        .collect::<Vec<usize>>();
    monkey_business.sort_by(|a, b| b.cmp(a));
    monkey_business[0] * monkey_business[1]
}

#[aoc(day11, part2)]
pub fn solve_part2(input: &str) -> usize {
    let mut monkeys = parse_input(input);
    let lcd: usize = monkeys.iter().map(|m| m.test).product();
    for _ in 0..10_000 {
        for m in 0..monkeys.len() {
            monkeys[m].inspect();
            for (target, item) in monkeys[m].throw(|x| x % lcd) {
                monkeys[target].items.push_back(item);
            }
        }
    }
    let mut monkey_business = monkeys.iter()
        .map(|m| m.inspected)
        .collect::<Vec<usize>>();
    monkey_business.sort_by(|a, b| b.cmp(a));
    monkey_business[0] * monkey_business[1]
}
