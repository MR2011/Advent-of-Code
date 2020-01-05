use std::collections::HashMap;
use std::collections::HashSet;
use std::iter::FromIterator;

pub struct OrbitMap{
    map: HashMap<String, String>,
}

impl OrbitMap {

    fn new(input: &str) -> OrbitMap {
        OrbitMap {
            map : input.trim()
                .lines()
                .map(|line| {
                    let tokens: Vec<&str> = line.split(")").collect();
                    (tokens[1].to_string(), tokens[0].to_string())
                })
                .collect()
        }
    }
    
    fn orbit_checksum(&self) -> usize {
        self.map.keys()
            .map(|k| {
                let path = self.path_from_root(k.to_string(), Vec::new());
                path.iter().count() - 1
            })
            .sum()
    }

    fn path_from_root(&self, object: String, mut visited: Vec<String>) -> Vec<String> {
        visited.push(object.clone());
        match self.map.get(&object) {
            None => visited,
            Some(predecessor) => self.path_from_root(predecessor.to_string(), visited),
        }
    }

    fn min_transfers(&self, from: String, to: String) -> usize {
        let from_set: HashSet<String> = HashSet::from_iter(self.path_from_root(from, Vec::new()));
        let to_set: HashSet<String> = HashSet::from_iter(self.path_from_root(to, Vec::new()));
        return from_set.symmetric_difference(&to_set).count() - 2 // - SAN and YOU
    }
} 


#[aoc_generator(day6)]
pub fn prepare_input(input: &str) -> OrbitMap {
    OrbitMap::new(input)
}

#[aoc(day6, part1)]
pub fn solve_part1(map: &OrbitMap) -> usize {
    map.orbit_checksum()
}

#[aoc(day6, part2)]
pub fn solve_part2(map: &OrbitMap) -> usize {
    map.min_transfers("YOU".to_string(), "SAN".to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test1() {
        let raw_input = "\
COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L"; 
        let input = prepare_input(raw_input);
        assert_eq!(solve_part1(&input), 42);
    }

    #[test]
    fn test2() {
        let raw_input = "\
COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
K)YOU
I)SAN";
        let input = prepare_input(raw_input);
        assert_eq!(solve_part2(&input), 4);
    }
}

