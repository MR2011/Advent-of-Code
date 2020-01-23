use std::collections::HashSet;
use std::cmp;
use std::f32;

#[derive(Debug, Eq, Hash, PartialEq, Copy, Clone)]
pub struct Asteroid {
    x: i32,
    y: i32,
}

impl Asteroid {
    pub fn new(x: i32, y: i32) -> Asteroid {
        Asteroid {
            x: x,
            y: y,
        }
    }

    pub fn angle(&self, asteroid: Asteroid) -> i32 {
        let dx = asteroid.x as f32 - self.x as f32;
        let dy = self.y as f32 - asteroid.y as f32;
        let angle = dy.atan2(dx) * 180.0 / f32::consts::PI;
        // work with integer instead of floating points
        let mut angle_i = (angle * 10.0).round() as i32;
        // Convert to 0-360° or 0-3600 since it is multiplied by 10
        // to consider the first decimal place
        angle_i = (-1 * angle_i + 900) % 3600;
        if angle_i < 0 {
            angle_i += 3600;
        }
        angle_i
    }

    pub fn distance(&self, asteroid: Asteroid) -> i32 {
        // manhattan distance
        (self.x - asteroid.x).abs() + (self.y - asteroid.y).abs()
    }
}

pub struct AsteroidMap {
    asteroids: HashSet<Asteroid>,
}

impl AsteroidMap {

    pub fn new(input: &str) -> AsteroidMap {
        let mut asteroids: HashSet<Asteroid> = HashSet::new();
        for (y, line) in input.lines().enumerate() {
            for (x, c) in line.chars().enumerate() {
                if c == '#' {
                    asteroids.insert(Asteroid::new(x as i32, y as i32));
                }
            }
        }
        AsteroidMap {
            asteroids: asteroids,
        }
    }

    pub fn find_station_location(&self) -> usize {
        let mut max = 0;
        for candidate in self.asteroids.iter() {
            let mut angles: HashSet<i32> = HashSet::new();
            for asteroid in self.asteroids.iter() {
                if candidate != asteroid {
                    let angle = candidate.angle(*asteroid);
                    angles.insert(angle);
                }
            }
            max = cmp::max(max, angles.len());
        }
        max
    }
    
    pub fn vaporize(&self, station: Asteroid) -> Asteroid {
        let mut asteroids = Vec::new();
        for asteroid in self.asteroids.iter() {
            let angle = station.angle(*asteroid);
            let distance = station.distance(*asteroid);
            asteroids.push((angle, distance, asteroid.clone(), false))
        }
        
        //sort by angles and distances
        asteroids.sort_by(|a,b| {
            a.0.partial_cmp(&b.0).unwrap().then(a.1.cmp(&b.1))
        });

        let mut i = 0;
        let mut last = (-1, 0, Asteroid::new(0,0), false);
        for v in asteroids.iter_mut() {
            if !v.3 { // if not vaporized
                if last.0 != v.0 { // different angle = next asteroid
                    if i == 199 {
                        last = *v;
                        break
                    }
                    v.3 = true; // vaporize
                    i += 1;
                }
                last = *v;
            }
        }
        last.2
    }

}
#[aoc_generator(day10)]
pub fn prepare_input(input: &str) -> AsteroidMap {
    AsteroidMap::new(input)
}

#[aoc(day10, part1)]
pub fn solve_part1(map: &AsteroidMap) -> usize {
    map.find_station_location()
}

#[aoc(day10, part2)]
pub fn solve_part2(map: &AsteroidMap) -> i32 {
    let station = Asteroid::new(19,14);
    let asteroid = map.vaporize(station);
    asteroid.x * 100 + asteroid.y
}
