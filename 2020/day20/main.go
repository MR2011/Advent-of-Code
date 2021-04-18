package main

import (
    "bufio"
    "fmt"
    "os"
    "strings"
    "strconv"
)

const SIZE int = 10

type Tile struct {
    id int
    grid [][]rune
    neighbors []*Tile
    size int
}

func (t *Tile) top() string {
    return string(t.grid[0])
}

func (t *Tile) bottom() string {
    return string(t.grid[t.size-1])
}

func (t *Tile) left() string {
    var l strings.Builder
    for i:=0; i<t.size; i++ {
	l.WriteRune(t.grid[i][0])
    }
    return l.String()
}

func (t *Tile) right() string {
    var r strings.Builder
    for i:=0; i<t.size; i++ {
	r.WriteRune(t.grid[i][t.size-1])
    }
    return r.String()
}

func (t *Tile) edges() []string{
    return []string{
	t.top(),
	t.bottom(),
	t.left(),
	t.right(),
    }
}

func (t *Tile) allPossibleEdges() [] string{
    edges := t.edges()
    horizontal := t.flipHorizontal()
    vertical := t.flipVertical()
    edges = append(edges, horizontal.top())
    edges = append(edges, horizontal.bottom())
    edges = append(edges, vertical.left())
    edges = append(edges, vertical.right())
    return edges
}

func (t *Tile) flipVertical() Tile {
    grid := [][]rune{}
    for i:=t.size-1; i>=0; i-- {
	grid = append(grid, t.grid[i])
    }
    return Tile {
	t.id,
	grid,
	[]*Tile{},
	t.size,
    }
}

func (t *Tile) flipHorizontal() Tile {
    grid := [][]rune{}
    for i:=0; i<t.size; i++ {
	row := []rune{}
	for j:=t.size-1; j>=0; j-- {
	    row = append(row, t.grid[i][j])
	}
	grid = append(grid, row)
    }
    return Tile {
	t.id,
	grid,
	[]*Tile{},
	t.size,
    }
}

func (t *Tile) removeBorder() Tile {
    grid := [][]rune{}
    for i, row := range t.grid {
	if i > 0 && i < t.size-1 {
	    grid = append(grid, row[1:len(row)-1])
	}
    }
    return Tile {
	t.id,
	grid,
	t.neighbors,
	t.size,
    }
}

// func (t *Tile) rotateRight() Tile {
//     grid := [][]rune{}

//     for i := range t.grid {
// 	for j := range t.grid[i] {
// 	    grid[i][t.size-j-1] = t.grid[i][j]
// 	}
//     }
//     return Tile {
// 	t.id,
// 	grid,
// 	[]*Tile{},
// 	t.size,
//     }
// }

func (t *Tile) isPossibleNeighbor(neighbor *Tile) bool {
    edges := t.allPossibleEdges()
    neighborEdges := neighbor.allPossibleEdges()
    for _, edge := range edges {
	for _, neighborEdge := range neighborEdges {
	    if edge == neighborEdge {
		return true
	    }
	}
    }
    return false
}

func parseInput() map[int]Tile {
    file, _ := os.Open("input")
    scanner := bufio.NewScanner(file)
    defer file.Close()
    tiles := map[int]Tile{}
    id := 0
    grid := [][]rune{}
    neighbors := []*Tile{}
    for scanner.Scan() {
	if len(scanner.Text()) == 0 {
	    t := Tile{id, grid, neighbors, SIZE}
	    tiles[id] = t
	    grid = [][]rune{}
	} else if strings.HasPrefix(scanner.Text(), "Tile") {
	    tokens := strings.Split(scanner.Text(), " ")
	    id, _ = strconv.Atoi(tokens[1][:len(tokens[1])-1])
	} else {
	    row := []rune{}
	    for _, c := range scanner.Text() {
		row = append(row, c)
	    }
	    grid = append(grid, row)
	}
    }
    return tiles
}

func part1() int {
    tiles := parseInput()
    result := 1
    for id, tile := range tiles {
	for idNeighbor, neighbor := range tiles {
	    if id == idNeighbor {
		continue
	    }
	    if tile.isPossibleNeighbor(&neighbor) {
		tile.neighbors = append(tile.neighbors, &neighbor)
	    }
	}
	if len(tile.neighbors) == 2 {
	    result *= tile.id
	}

    }
    return result
}

func part2() int {
    return 2
}

func main() {
    fmt.Printf("Part 1: %d\n", part1())
    fmt.Printf("Part 2: %d\n", part2())
}
