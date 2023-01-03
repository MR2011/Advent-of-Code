use serde_json::{json, Value};
use std::cmp::Ordering;

fn compare(a: &Value, b: &Value) -> Ordering {
    match (a, b) {
        (Value::Number(n), Value::Number(m)) => {
            let u = n.as_i64().unwrap_or(0);
            let v = m.as_i64().unwrap_or(0);
            u.cmp(&v)
        }
        (Value::Array(n), Value::Array(m)) => match (n.len(), m.len()) {
            (0, 0) => Ordering::Equal,
            (0, _) => Ordering::Less,
            (_, 0) => Ordering::Greater,
            (_, _) => match compare(&a[0], &b[0]) {
                Ordering::Equal => {
                    let ai = json!(n[1..]);
                    let bi = json!(m[1..]);
                    compare(&ai, &bi)
                }
                order => order,
            },
        },
        (Value::Array(n), Value::Number(m)) => compare(a, &json!(vec![m])),
        (Value::Number(n), Value::Array(m)) => compare(&json!(vec![n]), b),
        _ => {
            panic!("invalid ordering")
        }
    }
}

fn parse_input(input: &str) -> Vec<Value> {
    input
        .lines()
        .filter(|l| !l.is_empty())
        .map(|l| serde_json::from_str(l).unwrap())
        .collect()
}

#[aoc(day13, part1)]
pub fn solve_part1(input: &str) -> usize {
    let packets = parse_input(input);
    let pairs: Vec<_> = packets.chunks(2).map(|c| c).collect();
    let mut sum = 0;
    for (i, pair) in pairs.iter().enumerate() {
        if compare(&pair[0], &pair[1]) == Ordering::Less {
            sum += i + 1;
        }
    }
    sum
}

#[aoc(day13, part2)]
pub fn solve_part2(mut input: &str) -> usize {
    let mut packets = parse_input(input);
    let div1 = json!([[2]]);
    let div2 = json!([[6]]);
    packets.push(div1.clone());
    packets.push(div2.clone());
    packets.sort_by(|a, b| compare(a, b));

    let pos1 = packets.iter().position(|p| p == &div1).unwrap() + 1;
    let pos2 = packets.iter().position(|p| p == &div2).unwrap() + 1;
    pos1 * pos2
}
