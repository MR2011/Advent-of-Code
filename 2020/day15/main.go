package main

import (
    "fmt"
)

var STARTING_NUMS = []int{6, 3, 15, 13, 1, 0}

func initMemory() map[int]int {
    memory := map[int]int{}
    for i, num := range STARTING_NUMS {
	memory[num] = (i+1)
    }
    return memory
}

func play(maxTurns int) int {
    memory := initMemory()
    turn := len(STARTING_NUMS) + 1
    lastSpoken := 0
    for turn < maxTurns{
	if memory[lastSpoken] == 0 {
	    memory[lastSpoken] = turn
	    lastSpoken = 0
	} else {
	    age := turn - memory[lastSpoken]
	    memory[lastSpoken] = turn
	    lastSpoken = age
	}
	turn++
    }
    return lastSpoken
}

func part1() int {
    return play(2020)
}

func part2() int {
    return play(30000000)
}

func main() {
    fmt.Printf("Part 1: %d\n", part1())
    fmt.Printf("Part 2: %d\n", part2())
}
