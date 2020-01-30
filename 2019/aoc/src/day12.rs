use regex::Regex;
use std::cmp::Ordering;
extern crate num;
use num::integer::lcm;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Vec3d {
    x: i32,
    y: i32,
    z: i32,
}
#[derive(PartialEq, Eq, Clone, Copy)]
struct Moon {
    pos: Vec3d,
    vel: Vec3d,
}

impl Moon {
    pub fn new(pos: Vec3d) -> Moon {
        Moon {
            pos: pos,
            vel: Vec3d{x: 0, y: 0, z: 0},
        }
    }

    pub fn apply_gravity(&mut self, moon: Moon) {
        self.vel.x += self.compare_gravity_axis(self.pos.x, moon.pos.x);
        self.vel.y += self.compare_gravity_axis(self.pos.y, moon.pos.y);
        self.vel.z += self.compare_gravity_axis(self.pos.z, moon.pos.z);
    }

    pub fn apply_velocity(&mut self) {
        self.pos.x += self.vel.x;
        self.pos.y += self.vel.y;
        self.pos.z += self.vel.z;
    }

    pub fn total_energy(&self) -> i32 {
        self.potential_energy() * self.kinetic_energy()
    }

    fn potential_energy(&self) -> i32 {
        self.pos.x.abs() + self.pos.y.abs() + self.pos.z.abs()
    }

    fn kinetic_energy(&self) -> i32 {
        self.vel.x.abs() + self.vel.y.abs() + self.vel.z.abs()
    }

    fn compare_gravity_axis(&self, x1: i32, x2: i32) -> i32 {
        match x1.cmp(&x2) {
            Ordering::Less => 1,
            Ordering::Equal => 0,
            Ordering::Greater => -1,
        }
    }
}

fn step(moons: &mut Vec<Moon>) {
    for i in 0..moons.len() {
        for j in i+1..moons.len() {
            let m1 = moons[i];
            let m2 = moons[j];
            moons[i].apply_gravity(m2);
            moons[j].apply_gravity(m1);
        }
    }

    for moon in moons.iter_mut() {
        moon.apply_velocity();
    }
}

#[aoc_generator(day12)]
pub fn prepare_input(input: &str) -> Vec<Vec3d> {
    let re = Regex::new(r"<x=(.*), y=(.*), z=(.*)>").unwrap();
    input.trim().lines()
        .map(|line| {
            let matches = re.captures(line).unwrap();
            Vec3d{ x: matches.get(1).unwrap().as_str().to_string().parse().unwrap(), 
                   y: matches.get(2).unwrap().as_str().to_string().parse().unwrap(), 
                   z: matches.get(3).unwrap().as_str().to_string().parse().unwrap()}
        })
        .collect()
}

#[aoc(day12, part1)]
pub fn solve_part1(input: &Vec<Vec3d>) -> i32 {
    let mut moons: Vec<Moon> = input.iter().map(|pos| Moon::new(*pos)).collect();
    for _ in 0..1000 {
        step(&mut moons);
    }
    moons.iter().map(|m| m.total_energy()).sum()
}

#[aoc(day12, part2)]
pub fn solve_part2(input: &Vec<Vec3d>) -> i64 {
    let mut moons: Vec<Moon> = input.iter().map(|pos| Moon::new(*pos)).collect();
    let mut cycles = [0, 0, 0];
    let init_x: Vec<(i32, i32)>= moons.iter().map(|m| (m.pos.x, m.vel.x)).collect();
    let init_y: Vec<(i32, i32)>= moons.iter().map(|m| (m.pos.y, m.vel.y)).collect();
    let init_z: Vec<(i32, i32)>= moons.iter().map(|m| (m.pos.z, m.vel.z)).collect();
    let mut i: i64 = 0;
    while cycles[0] == 0 || cycles[1] == 0 || cycles[2] == 0 {
        step(&mut moons);
        i += 1;
        if cycles[0] == 0 {
            let x: Vec<(i32, i32)>= moons.iter().map(|m| (m.pos.x, m.vel.x)).collect();
            cycles[0] = if x ==  init_x { i } else { 0 };
        }
        if cycles[1] == 0 {
            let y: Vec<(i32, i32)>= moons.iter().map(|m| (m.pos.y, m.vel.y)).collect();
            cycles[1] = if y ==  init_y { i } else { 0 };
        }
        if cycles[2] == 0 {
            let z: Vec<(i32, i32)>= moons.iter().map(|m| (m.pos.z, m.vel.z)).collect();
            cycles[2] = if z ==  init_z { i } else { 0 };
        }
    }
    lcm(cycles[0], lcm(cycles[1], cycles[2]))
}

