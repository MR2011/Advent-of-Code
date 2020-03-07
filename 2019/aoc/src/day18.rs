use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;

#[derive(Debug, Copy, Clone, Eq, Hash, PartialEq)]
struct Point(i32, i32);

impl Point {
    fn neighbors(&self) -> Vec<Point> {
        let offsets = [(0, 1), (1, 0), (-1, 0), (0, -1)];
        let mut neighbors = Vec::new();
        for (dx, dy) in offsets.iter() {
            neighbors.push(Point(self.0 + dx, self.1 + dy));
        }
        neighbors
    }
}

#[derive(Hash, Eq, PartialEq, Copy, Clone, Debug)]
enum Tile {
    Free,
    Key(u32),
    Door(u32),
}

#[derive(Hash, Eq, PartialEq, Copy, Clone, Debug)]
struct State {
    missing_keys: KeyMap,
    pos: Point,
}

// Bitmap: each Pointition represents a key
#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
struct KeyMap{
    keys: u32,
}

impl KeyMap {
    fn contains(&self, key: u32) -> bool {
        match self.keys >> key & 1 {
            1 => true,
            _ => false,
        }
    }

    fn set(&mut self, key: u32) {
        self.keys = self.keys | (1 << key);
    }

    fn unset(&mut self, key: u32) {
        self.keys = self.keys ^ (1 << key) as u32;
    }

    fn empty(&self) -> bool {
        self.keys == 0
    }
}

pub struct Map {
    tiles: HashMap<Point, Tile>,
    start: Point,
    keys: KeyMap,
}

impl Map {
    pub fn shortest_path(&self) -> usize {
        let mut states: HashMap<State, usize> = HashMap::new();
        let mut q: VecDeque<State> = VecDeque::new();
        let start = State { missing_keys: self.keys, pos: self.start };
        let mut steps_current: usize = 0;

        states.insert(start, 0);
        q.push_back(start);

        while let Some(state) = q.pop_front() {
            match states.get(&state) {
                Some(steps) => {
                    steps_current = *steps;

                    if state.missing_keys.empty() {
                        return steps_current;
                    }

                    for neighbor in self.neighbors(state) {
                        if !states.contains_key(&neighbor) {
                            states.insert(neighbor, steps_current + 1);
                            q.push_back(neighbor);
                        }
                    }
                },
                _ => break,
            }
        }
        std::usize::MAX
    }

    fn neighbors(&self, state: State) -> Vec<State> {
        let mut neighbors = Vec::new();
        for neighbor in state.pos.neighbors().iter() {
            let mut new_state = State{ missing_keys: state.missing_keys, pos: *neighbor };
            match self.tiles.get(neighbor) {
                Some(Tile::Door(key)) => {
                    // do we have the key?
                    if !state.missing_keys.contains(*key) {
                        neighbors.push(new_state);
                    }
                },
                Some(Tile::Key(key)) => {
                    // do we have the key already?
                    if new_state.missing_keys.contains(*key) {
                        new_state.missing_keys.unset(*key);
                    }
                    neighbors.push(new_state);
                },
                Some(Tile::Free) => {
                    neighbors.push(new_state);
                },
                _ => (),
            }
        }
        return neighbors
    }

    pub fn partition(&self) -> Vec<Map> {
        let mut tiles = self.tiles.clone();
        let mut maps = Vec::new();

        for neighbor in self.start.neighbors().iter() {
            tiles.remove(&neighbor);
        }
        tiles.remove(&self.start);

        for (dx, dy) in &[(-1, -1), (1, 1), (1, -1), (-1, 1)] {
            let start = Point(self.start.0 + dx, self.start.1 + dy);
            tiles.insert(start, Tile::Free);
            let mut map = Map{ tiles: tiles.clone(),
                               keys: KeyMap{ keys: 0 },
                               start: start};
            map.find_keys();
            maps.push(map);
        }

        maps

    }

    fn find_keys(&mut self) {
        let mut q: VecDeque<Point> = VecDeque::new();
        let mut visited = HashSet::new();
        let mut keys = KeyMap{ keys: 0 };

        q.push_back(self.start);
        visited.insert(self.start);

        while let Some(p) = q.pop_front() {
            visited.insert(p);
            match self.tiles.get(&p) {
                Some(v) => {
                    match v {
                        Tile::Key(k) => keys.set(*k),
                        _ => (),
                    }
                    for neighbor in p.neighbors() {
                       if !visited.contains(&neighbor) {
                           q.push_back(neighbor);
                       }
                    }
                },
                _ => (),
            };
        }
        self.keys = keys;
    }
}

#[aoc_generator(day18)]
pub fn prepare_input(input: &str) -> Map {
    let mut tiles = HashMap::new();
    let mut keys = KeyMap{ keys: 0 };
    let mut point = Point(0, 0);

    for (y, line) in input.lines().enumerate() {
        for (x, tile) in line.chars().enumerate() {
            let p = Point(x as i32, y as i32);
            match tile {
                'a'..='z' => {
                    let key = tile as u32 - 'a' as u32;
                    tiles.insert(p, Tile::Key(key));
                    keys.set(key as u32);
                },
                'A'..='Z' => {
                    tiles.insert(p, Tile::Door(tile as u32 - 'A' as u32));
                },
                '.' => {
                    tiles.insert(p, Tile::Free);
                },
                '@' => {
                    tiles.insert(p, Tile::Free);
                    point = p;
                },
                _ => (),
            }
        }
    }
    Map { tiles: tiles, keys: keys, start: point }
}

#[aoc(day18, part1)]
pub fn solve_part1(input: &Map) -> usize {
    input.shortest_path()
}

#[aoc(day18, part2)]
pub fn solve_part2(input: &Map) -> usize {
    let mut steps = 0;
    for partition in input.partition() {
        steps += partition.shortest_path();
    }
    steps
}
