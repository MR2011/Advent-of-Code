package main

import (
    "bufio"
    "fmt"
    "os"
)


const WHITE = false
const BLACK = true

// Used cube coordinates according to:
// https://www.redblobgames.com/grids/hexagons/#coordinates-cube
type Tile struct {
    x int
    y int
    z int
}

func (t *Tile) move(direction string) {
    switch direction {
    case "ne":
	t.x++
	t.z--
    case "e":
	t.x++
	t.y--
    case "se":
	t.y--
	t.z++
    case "sw":
	t.x--
	t.z++
    case "w":
	t.x--
	t.y++
    case "nw":
	t.y++
	t.z--
    }
}

// https://www.redblobgames.com/grids/hexagons/#neighbors-cube
func (t *Tile) neighbors() []Tile {
    return []Tile{
	Tile{t.x+1, t.y, t.z-1}, //ne
	Tile{t.x+1, t.y-1, t.z}, //e
	Tile{t.x, t.y-1, t.z+1}, //se
	Tile{t.x-1, t.y, t.z+1}, //sw
	Tile{t.x-1, t.y+1, t.z}, //w
	Tile{t.x, t.y+1, t.z-1}, //nw
    }

}

func parseInput() map[Tile]bool {
    file, _ := os.Open("input")
    scanner := bufio.NewScanner(file)
    defer file.Close()
    tiles := map[Tile]bool{}
    for scanner.Scan() {
	direction := ""
	tile := Tile{0, 0, 0}
	for _, c := range scanner.Text() {
	    direction += string(c)
	    if c == 'e' || c == 'w' {
		tile.move(direction)
		direction = ""
	    }
	}
	tiles[tile] = !tiles[tile]
    }
    return tiles
}

func countBlackTiles(tiles map[Tile]bool) int {
    black := 0
    for _, isBlack := range tiles {
	if isBlack {
	    black++ 
	}
    }
    return black
}

func part1() int {
    tiles := parseInput()
    return countBlackTiles(tiles)
}

func part2() int {
    tiles := parseInput()
    newTiles := map[Tile]bool{}
    for day:=0; day<100; day++ {
	newTiles = map[Tile]bool{}
	// add the outer neighbors
	for tile, _ := range tiles {
	    for _, neighbor := range tile.neighbors() {
		if !tiles[neighbor] {
		    tiles[neighbor] = WHITE
		}
	    }
	}
	for tile, isBlack := range tiles {
	    blackNeighbors := 0
	    for _, neighbor := range tile.neighbors() {
		if tiles[neighbor] {
		    blackNeighbors++
		}
	    }
	    if isBlack {
		if blackNeighbors == 0 || blackNeighbors > 2 {
		    newTiles[tile] = WHITE
		} else {
		    newTiles[tile] = tiles[tile]
		}
	    } else {
		if blackNeighbors == 2 {
		    newTiles[tile] = BLACK
		} else {
		    newTiles[tile] = tiles[tile]
		}
	    }
	}
	tiles = newTiles
    }
    return countBlackTiles(tiles)
}

func main() {
    fmt.Printf("Part 1: %d\n", part1())
    fmt.Printf("Part 2: %d\n", part2())
}
