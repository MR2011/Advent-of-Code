package main

import (
    "bufio"
    "fmt"
    "os"
)

type Cube struct {
    x int
    y int
    z int
    w int
}

func (c Cube) getNeighbors(dimensions int) []Cube {
    neighbors := []Cube{}
    wStart := 0
    wEnd := 1
    if dimensions == 4{
	wStart = c.w - 1
	wEnd = c.w + 2
    }
    for w:=wStart; w<wEnd; w++ {
	for z:=c.z-1; z<c.z+2; z++ {
	    for y:=c.y-1; y<c.y+2; y++ {
		for x:=c.x-1; x<c.x+2; x++ {
		    if x==c.x && y==c.y && z==c.z && w==c.w {
			continue
		    }
		    neighbors = append(neighbors, Cube{x, y, z, w})
		}
	    }
	}
    }
    return neighbors
}

func (c Cube) countActiveNeighbors(grid map[Cube]bool, dimensions int) int {
    active := 0
    for _, neighbor := range c.getNeighbors(dimensions) {
	if grid[neighbor] {
	    active++
	}
    }
    return active
}

func parseInput() map[Cube]bool {
    file, _ := os.Open("input")
    scanner := bufio.NewScanner(file)
    defer file.Close()
    grid := map[Cube]bool{}
    y := 0
    for scanner.Scan() {
	for x, c := range scanner.Text() {
	    cube := Cube{x, y, 0, 0}
	    if c == '#' {
		grid[cube] = true
	    }else if c == '.' {
		grid[cube] = false
	    }
	}
	y++
    }
    return grid
}

func simulate(grid map[Cube]bool, dimensions int) map[Cube]bool {
    nextCycle := map[Cube]bool{}
    for cube, active := range grid {
	nextCycle[cube] = active
	for _, neighbor := range cube.getNeighbors(dimensions) {
	    if !nextCycle[neighbor] {
		nextCycle[neighbor] = false
	    }
	}
    }
    for cube, active := range nextCycle {
	if dimensions == 3 && cube.w != 0 {
	    continue
	}
	activeNeighbors := cube.countActiveNeighbors(grid, dimensions)
	if active {
	    if (activeNeighbors == 2 || activeNeighbors == 3) {
		nextCycle[cube] = true
	    } else {
		nextCycle[cube] = false
	    }
	} else {
	    if activeNeighbors == 3 {
		nextCycle[cube] = true
	    } else {
		nextCycle[cube] = false
	    }
	}
    }
    return nextCycle
}

func countActiveCubes(grid map[Cube]bool) int {
    activeCubes := 0
    for _, active := range grid {
	if active {
	    activeCubes++
	}
    }
    return activeCubes
}


func part1() int {
    grid := parseInput()
    for i:=0; i<6; i++ {
	grid = simulate(grid, 3)
    }
    return countActiveCubes(grid)
}

func part2() int {
    grid := parseInput()
    for i:=0; i<6; i++ {
	grid = simulate(grid, 4)
    }
    return countActiveCubes(grid)
}

func main() {
    fmt.Printf("Part 1: %d\n", part1())
    fmt.Printf("Part 2: %d\n", part2())
}
