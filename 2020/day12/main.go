package main

import (
    "bufio"
    "fmt"
    "os"
)

const NORTH = 'N'
const SOUTH = 'S'
const EAST = 'E'
const WEST = 'W'
const FORWARD = 'F'
const LEFT = 'L'
const RIGHT = 'R'

type Instruction struct {
    action rune
    value int
}

type Ship struct {
    direction rune
    x int
    y int
    wpX int
    wpY int
}

func (s *Ship) move(direction rune, value int) {
    switch direction {
    case NORTH:
	s.y += value
    case SOUTH:
	s.y -= value
    case WEST:
	s.x -= value
    case EAST:
	s.x += value
    }
}

func (s *Ship) moveWP(direction rune, value int) {
    switch direction {
    case NORTH:
	s.wpY += value
    case SOUTH:
	s.wpY -= value
    case WEST:
	s.wpX -= value
    case EAST:
	s.wpX += value
    }
}

func (s *Ship) turn(direction rune, value int) {
    turns := int(value / 90)
    if direction == LEFT {
	turns *= -1
    }
    directions := []rune{NORTH, EAST, SOUTH, WEST}
    var start int
    for i, d := range directions {
	if d == s.direction {
	    start = i
	}
    }
    index := (start + turns) % 4
    if index < 0 {
	index += 4
    }
    s.direction = directions[index]
}

func (s *Ship) turnWP(direction rune, value int) {
    turns := int(value / 90)
    for turns > 0 {
	switch direction {
	case LEFT:
	    y := s.wpX
	    s.wpX = -s.wpY
	    s.wpY = y
	case RIGHT:
	    x := s.wpY
	    s.wpY = -s.wpX
	    s.wpX = x
	}
	turns--
    }
}

func abs(d int) int {
    if d < 0{
	d *= -1
    }
    return d
}


func parseInput() []Instruction {
    file, _ := os.Open("input")
    scanner := bufio.NewScanner(file)
    defer file.Close()
    instructions := []Instruction{}
    for scanner.Scan() {
	action, value := ' ', 0
	fmt.Sscanf(scanner.Text(), "%c%d", &action, &value)
	instructions = append(instructions, Instruction{action, value})
    }
    return instructions
}


func part1() int {
    instructions := parseInput()
    ship := Ship{EAST, 0, 0, 0, 0}
    for _, i := range instructions {
	switch i.action {
	case NORTH, SOUTH, EAST, WEST:
	    ship.move(i.action, i.value)
	case LEFT, RIGHT:
	    ship.turn(i.action, i.value)
	case FORWARD:
	    ship.move(ship.direction, i.value)
	}

    }
    return abs(ship.x) + abs(ship.y)
}

func part2() int {
    instructions := parseInput()
    ship := Ship{EAST, 0, 0, 10, 1}
    for _, i := range instructions {
	switch i.action {
	case NORTH, SOUTH, EAST, WEST:
	    ship.moveWP(i.action, i.value)
	case LEFT, RIGHT:
	    ship.turnWP(i.action, i.value)
	case FORWARD:
	    ship.x += ship.wpX * i.value
	    ship.y += ship.wpY * i.value
	}

    }
    return abs(ship.x) + abs(ship.y)
}

func main() {
    fmt.Printf("Part 1: %d\n", part1())
    fmt.Printf("Part 2: %d\n", part2())
}
