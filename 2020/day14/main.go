package main

import (
    "fmt"
    "os"
    "bufio"
    "strings"
)

func parseMask(mask string) (int, int, int, []int) {
    setMask := 0
    clrMask := 0
    xIndices := []int{}
    floatMask := 0
    for i := range mask {
	c := rune(mask[len(mask)-1-i])
	if c == 'X' {
	    xIndices = append(xIndices, 1 << i)
	    floatMask |= (1 << i)
	} else if c == '1' {
	    setMask |= (1 << i)
	} else if c == '0' {
	    clrMask |= (1 << i)
	}
    }
    floatingPermutations := []int{0}
    for _, index := range xIndices {
	for _, permutation := range floatingPermutations {
	    floatingPermutations = append(floatingPermutations, index|permutation)
	}
    }
    return setMask, clrMask, floatMask, floatingPermutations
}

func parseInput(floatingMode bool) map[int]int {
    file, _ := os.Open("input")
    scanner := bufio.NewScanner(file)
    defer file.Close()
    memory := map[int]int{}
    mask := ""
    setMask := 0
    clrMask := 0
    floatMask := 0
    floatingPermutations := []int{}
    index := 0
    value := 0
    for scanner.Scan() {
	if strings.HasPrefix(scanner.Text(), "mask") {
	    fmt.Sscanf(scanner.Text(), "mask = %s", &mask)
	    setMask, clrMask, floatMask, floatingPermutations = parseMask(mask)
	} else if strings.HasPrefix(scanner.Text(), "mem") {
	    fmt.Sscanf(scanner.Text(), "mem[%d] = %d", &index, &value)
	    if floatingMode {
		for _, permutation := range floatingPermutations {
		    // Unset X bits in index with floatMask
		    memory[(index & ^floatMask)|setMask|permutation] = value
		}
	    } else {
		value |= setMask
		value &= ^clrMask
		memory[index] = value
	    }
	}

    }
    return memory
}

func calcSum(memory map[int]int) int {
    sum := int(0)
    for _, value := range memory {
	sum += value
    }
    return sum
}

func part1() int {
    memory := parseInput(false)
    return calcSum(memory)
}

func part2() int {
    memory := parseInput(true)
    return calcSum(memory)
}

func main() {
    fmt.Printf("Part 1: %d\n", part1())
    fmt.Printf("Part 2: %d\n", part2())
}
