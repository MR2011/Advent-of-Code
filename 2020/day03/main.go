package main

import (
    "bufio"
    "fmt"
    "os"
)

func parseInput() []string {
    file, _ := os.Open("input")
    scanner := bufio.NewScanner(file)
    defer file.Close()
    var forest []string
    for scanner.Scan() {
	forest = append(forest, scanner.Text())
    }
    return forest
}

func countTrees(dx int, dy int) int {
    forest := parseInput()
    x, y, trees := 0, 0, 0
    width := len(forest[0])
    for y < len(forest) {
	if forest[y][x] == '#' {
	    trees++
	}
	y = y + dy
	x = (x + dx) % width
    }
    return trees
}

func part1() int {
    return countTrees(3, 1)
}

func part2() int {
    x := []int{1, 3, 5, 7, 1}
    y := []int{1, 1, 1, 1, 2}
    trees := 1
    for i := range x {
	trees *= countTrees(x[i], y[i])
    }
    return trees
}

func main() {
    fmt.Printf("Part 1: %d\n", part1())
    fmt.Printf("Part 2: %d\n", part2())
}
