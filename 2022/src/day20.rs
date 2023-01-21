#[derive(Copy, Clone)]
struct Number {
    value: i64,
    index: usize,
}

fn parse_input(input: &str, key: i64) -> Vec<Number> {
    input
        .lines()
        .enumerate()
        .map(|(i, l)| {
            let value = l.parse::<i64>().unwrap();
            Number {
                value: value * key,
                index: i,
            }
        })
        .collect()
}

fn decrypt(mut cipher: Vec<Number>, rounds: usize) -> i64 {
    for _ in 0..rounds {
        for i in 0..cipher.len() {
            // find current index for original index i
            let idx = cipher.iter().position(|n| n.index == i).unwrap();
            let current = cipher[idx];
            let new_idx = (idx as i64 + current.value).rem_euclid((cipher.len() - 1) as i64);
            cipher.remove(idx);
            cipher.insert(new_idx as usize, current);
        }
    }

    let zero = cipher.iter().position(|&n| n.value == 0).unwrap();
    let n1 = cipher[(zero + 1000) % cipher.len()].value;
    let n2 = cipher[(zero + 2000) % cipher.len()].value;
    let n3 = cipher[(zero + 3000) % cipher.len()].value;
    n1 + n2 + n3
}

#[aoc(day20, part1)]
pub fn solve_part1(input: &str) -> i64 {
    let cipher = parse_input(input, 1);
    decrypt(cipher, 1)
}

#[aoc(day20, part2)]
pub fn solve_part2(input: &str) -> i64 {
    let cipher = parse_input(input, 811_589_153);
    decrypt(cipher, 10)
}
