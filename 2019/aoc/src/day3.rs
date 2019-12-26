use std::collections::HashSet;
use std::iter::FromIterator;
use std::cmp;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Point(i32, i32);

impl Point {
    fn move_in_direction(&mut self, direction: char) {
        match direction {
            'U' => self.1 += 1,
            'D' => self.1 -= 1,
            'L' => self.0 -= 1,
            'R' => self.0 += 1,
            _ => (), 
        };
    }
    // Manhattan distance from origin (0,0)
    fn dist(&self) -> i32 {
        self.0.abs() + self.1.abs()
    }
}

pub struct Step {
    direction: char,
    size: i32,
}

impl Step {
    pub fn new(step: &str) -> Step {
        Step {
            direction: step.chars().next().unwrap(), 
            size: step[1..].parse().unwrap(),
        }
    }
}

struct Wire {
    path: Vec<Point>,
}

impl Wire {
    pub fn new(steps: &Vec<Step>) -> Wire {
        let mut wire = Wire { path: Vec::new() };
        wire.process_path(steps);
        return wire
    }

    fn process_path(&mut self, steps: &Vec<Step>) {
        let mut position = Point(0, 0);
        for step in steps {
            for i in 0..step.size {
                position.move_in_direction(step.direction);
                self.path.push(position.clone());
            }
        }
    }

    fn path_as_set(&self) -> HashSet<Point>{
        HashSet::from_iter(self.path.iter().cloned())
    }
}

#[aoc_generator(day3)]
pub fn prepare_input(input: &str) -> Vec<Vec<Step>> {
    input.trim().lines()
        .map(|line| {
            line.split(",")
                .map(|item| Step::new(item))
                .collect()
        })
        .collect()
}

#[aoc(day3, part1)]
pub fn solve_part1(wires_input: &Vec<Vec<Step>>) -> i32 {
    let first_wire = Wire::new(&wires_input[0]).path_as_set();
    let second_wire = Wire::new(&wires_input[1]).path_as_set();
    
    let min = first_wire.intersection(&second_wire)
        .into_iter()
        .min_by(
            |p, q| p.dist().cmp(&q.dist())
            )
        .unwrap();
    return min.dist() 
}

#[aoc(day3, part2)]
pub fn solve_part2(wires_input: &Vec<Vec<Step>>) -> usize {
    let first_wire = Wire::new(&wires_input[0]);
    let second_wire = Wire::new(&wires_input[1]);
    
    let mut min_dist = std::usize::MAX;
    for p in first_wire.path_as_set().intersection(&second_wire.path_as_set()) {
        let first_index = first_wire.path.iter().position(|&x| x == *p).unwrap() + 1;
        let second_index = second_wire.path.iter().position(|&x| x == *p).unwrap() + 1;
		min_dist = cmp::min(first_index + second_index, min_dist);
    }
    return min_dist
}

#[cfg(test)]
mod tests {
    use super::*;
    
    //Part 1
    #[test]
    fn test1() {
        let input = prepare_input("R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83");
        assert_eq!(solve_part1(&input), 159);
    }
    #[test]
    fn test2() {
        let input = prepare_input("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7");
        assert_eq!(solve_part1(&input), 135);
    }
    //Part 2
    #[test]
    fn test3() {
        let input = prepare_input("R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83");
        assert_eq!(solve_part2(&input), 610);
    }
    #[test]
    fn test4() {
        let input = prepare_input("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7");
        assert_eq!(solve_part2(&input), 410);
    }
}
