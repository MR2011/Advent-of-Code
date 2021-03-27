
package main

import (
    "bufio"
    "fmt"
    "os"
    "strconv"
    "sort"
)

func parseInput() []int {
    file, _ := os.Open("input")
    scanner := bufio.NewScanner(file)
    defer file.Close()
    adapters := []int{}
    for scanner.Scan() {
	number, _ := strconv.Atoi(scanner.Text())
	adapters = append(adapters, number)
    }
    adapters = append(adapters, 0)
    sort.Ints(adapters)
    adapters = append(adapters, adapters[len(adapters)-1] + 3)
    return adapters
}


func part1() int {
    adapters := parseInput()
    ones, threes := 0, 0
    for i:=0; i < len(adapters) - 1; i++ {
	if adapters[i+1] - adapters[i] == 1 {
	    ones++
	} else if adapters[i+1] - adapters[i] == 3 {
	    threes++
	}
    }
    return ones * threes 
}

func part2() int {
    adapters := parseInput()
    diffs := []int{}
    for i:=1; i < len(adapters); i++ {
	diffs = append(diffs, adapters[i] - adapters[i-1])
    }
    variants := 1
    ones := 0
    for _, d := range diffs {
	if d == 1 {
	    ones++
	} else {
	    switch ones {
	    case 2:
		// for example 10, 11, 12, 15
		// 1: 10, 11, 12, 15
		// 2: 10, 12, 15
		variants *= 2
	    case 3:
		// for example 4, 5, 6, 7
		// 1: 4, 5, 6, 7
		// 2: 4, 6, 7
		// 3: 4, 5, 7
		// 4: 4, 7
		variants *= 4
	    case 4:
		// for example 4, 5, 6, 7, 8
		// 1: 4, 5, 6, 7, 8
		// 2: 4, 6, 7, 8
		// 3: 4, 5, 7, 8
		// 4: 4, 5, 6, 8
		// 5: 4, 7, 8
		// 6: 4, 5, 8
		// 7: 4, 6, 8
		variants *= 7
	    }
	    ones = 0
	}
    }

    return variants 
}

func main() {
    fmt.Printf("Part 1: %d\n", part1())
    fmt.Printf("Part 2: %d\n", part2())
}
