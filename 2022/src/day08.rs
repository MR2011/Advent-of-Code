type Map = Vec<Vec<u32>>;
type Coord = (usize, usize);

fn create_map(input: &str) -> Map {
    input.lines()
        .map(|l| l.chars().map(|c| c.to_digit(10).unwrap()).collect())
        .collect()
}

fn is_visible((x, y): Coord, map: Map) -> bool {
    let tree = map[y][x];
    let max_x = map[0].len();
    let max_y = map.len();
    if x == 0 || x == (max_x-1) || y == 0 || y == (max_y-1) {
        return true;
    }
    let left = map[y][0..x].iter().all(|&t| t < tree);
    let right = map[y][x+1..max_x].iter().all(|&t| t < tree);
    let top = map[0..y].iter().all(|r| r[x] < tree);
    let bottom = map[y+1..max_y].iter().all(|r| r[x] < tree);
    left || right || top || bottom
}

fn scenic_score((x, y): Coord, map: Map) -> usize {
    let tree = map[y][x];
    let max_x = map[0].len();
    let max_y = map.len();
    // if on the edge, multiplication with 0 results in 0 score
    if x == 0 || x == (max_x-1) || y == 0 || y == (max_y-1) {
        return 0;
    }
    // we stop once we encounter a tree which is >= tree so we have to add + 1
    // to also consider this tree. always adding +1 allows us to skip checking
    // the edges!
    let left = map[y][1..x].iter().rev().take_while(|&t| t < &tree).count() + 1;
    let right = map[y][x+1..max_x-1].iter().take_while(|&t| t < &tree).count() + 1;
    let top = map[1..y].iter().rev().take_while(|r| r[x] < tree).count() + 1;
    let bottom = map[y+1..max_y-1].iter().take_while(|r| r[x] < tree).count() + 1;
    left * right * top * bottom
}


#[aoc(day8, part1)]
pub fn solve_part1(input: &str) -> usize {
    let map = create_map(input);
    let max_x = map[0].len();
    let max_y = map.len();
    let mut visible = 0;
    for i in 0..max_x {
        for j in 0..max_y {
            if is_visible((i, j), map.clone()) {
                visible += 1;
            }
        }
    }
    visible
}

#[aoc(day8, part2)]
pub fn solve_part2(input: &str) -> usize {
    let map = create_map(input);
    let max_x = map[0].len();
    let max_y = map.len();
    let mut max_score = 0;
    for i in 0..max_x {
        for j in 0..max_y {
            let s = scenic_score((i, j), map.clone());
            if s > max_score {
                max_score = s;
            }
        }
    }
    max_score
}
