package main

import (
    "bufio"
    "fmt"
    "os"
)

const FLOOR = '.'
const EMPTY = 'L'
const OCCUPIED = '#'

var OFFSETS = []Coord{
    {0, 1}, {0, -1}, {1, 0}, {-1, 0}, {1, 1}, {1, -1}, {-1, 1}, {-1, -1},
}

type Coord struct {
    x int
    y int
}

type Grid map[Coord]rune


func parseInput() Grid {
    file, _ := os.Open("input")
    scanner := bufio.NewScanner(file)
    defer file.Close()
    seats := Grid{}
    y := 0
    for scanner.Scan() {
	for x, s := range scanner.Text() {
	    coord := Coord{x, y}
	    seats[coord] = s
	}
	y++
    }
    return seats
}

func occupiedSeats(seats Grid, coord Coord) int {
    occupied := 0
    for _, offset := range OFFSETS {
	neighbor := Coord{
	    coord.x + offset.x,
	    coord.y + offset.y,
	}
	if seats[neighbor] == OCCUPIED {
	    occupied++
	}

    }
    return occupied
}

func neighborsInView(seats Grid, coord Coord) int {
    occupied := 0
    for _, offset := range OFFSETS {
	neighbor := Coord{
	    coord.x + offset.x,
	    coord.y + offset.y,
	}
	for seats[neighbor] != 0 {
	    if seats[neighbor] == OCCUPIED {
		occupied++
		break
	    } else if seats[neighbor] == EMPTY {
		break
	    }
	    neighbor.x += offset.x
	    neighbor.y += offset.y
	}

    }
    return occupied

}

func simulate(grid Grid, maxNeighbors int, directNeighbors bool) (Grid, bool, int) {
    changed := false
    newGrid := Grid{}
    occupied := 0
    for coord, seat := range grid {
	occupiedNeighbors := 0
	if directNeighbors {
	    occupiedNeighbors = occupiedSeats(grid, coord)
	} else {
	    occupiedNeighbors = neighborsInView(grid, coord)
	}
	if seat == EMPTY && occupiedNeighbors == 0 {
	    newGrid[coord] = OCCUPIED
	    changed = true
	    occupied++
	}else if seat == OCCUPIED {
	    if occupiedNeighbors >= maxNeighbors {
		newGrid[coord] = EMPTY
		changed = true
	    } else {
		occupied++
		newGrid[coord] = seat
	    }
	}else {
	    newGrid[coord] = seat
	}
    }
    return newGrid, changed, occupied
}

func part1() int {
    grid := parseInput()
    changed := true
    occupied := 0
    for changed {
	grid, changed, occupied = simulate(grid, 4, true)
    }
    return occupied
}

func part2() int {
    grid := parseInput()
    changed := true
    occupied := 0
    for changed {
	grid, changed, occupied = simulate(grid, 5, false)
    }
    return occupied
}

func main() {
    fmt.Printf("Part 1: %d\n", part1())
    fmt.Printf("Part 2: %d\n", part2())
}
