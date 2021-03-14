package main

import (
    "bufio"
    "fmt"
    "os"
)

func parseInput() [][]string {
    file, _ := os.Open("input")
    scanner := bufio.NewScanner(file)
    defer file.Close()
    groups := [][]string{}
    people := []string{}
    for scanner.Scan() {
	if scanner.Text() == "" {
	    groups = append(groups, people)
	    people = []string{}
	} else {
	    people = append(people, scanner.Text())
	}
    }
    return groups
}

func parts() (int, int) {
    groups := parseInput()
    sumPart1 := 0
    sumPart2 := 0
    for _, group := range groups {
	yes := make(map[rune]int)
	for _, person := range group {
	    for _, answer := range person {
		yes[answer]++
	    }
	}
	for _, value := range yes {
	    if value > 0 {
		sumPart1++
	    }
	    if value == len(group) {
		sumPart2++
	    }
	}
    }
    return sumPart1, sumPart2
}

func main() {
    part1, part2 := parts()
    fmt.Printf("Part 1: %d\n", part1)
    fmt.Printf("Part 2: %d\n", part2)
}
