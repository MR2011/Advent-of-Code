package main

import (
    "fmt"
    "os"
    "bufio"
    "strconv"
    "strings"
)

func parseInput() (int, map[int]int) {
    file, _ := os.Open("input")
    scanner := bufio.NewScanner(file)
    defer file.Close()
    timestamp := 0
    busSchedule := map[int]int{}
    for scanner.Scan() {
	if timestamp == 0 {
	    timestamp, _ = strconv.Atoi(scanner.Text())
	} else {
	    for i, token := range strings.Split(scanner.Text(), ",") {
		if token == "x" {
		    busSchedule[i] = -1
		} else {
		    busSchedule[i], _ = strconv.Atoi(token)
		}

	    }
	}
    }
    return timestamp, busSchedule
}

// https://en.wikipedia.org/wiki/Euclidean_algorithm#Implementations
func gcd(a int, b int) int {
    for b != 0 {
	t := b
	b = a % b
	a = t
    }
    return a
}

// https://en.wikipedia.org/wiki/Least_common_multiple#Using_the_greatest_common_divisor
func lcm(a int, b int) int {
    return a * b / gcd(a, b)
}

func part1() int {
    timestamp, busSchedule := parseInput()
    t := timestamp
    busIds := []int{}
    for _, id := range busSchedule {
	if id != -1 {
	    busIds = append(busIds, id)
	}
    }
    for {
	for _, id := range busIds {
	    if t % id == 0 {
		return (t - timestamp) * id
	    }
	}
	t++
    }
    return -1
}

// https://en.wikipedia.org/wiki/Euclidean_algorithm#Chinese_remainder_theorem
func part2() int {
    _, busSchedule := parseInput()
    t := busSchedule[0]
    stepsize := busSchedule[0]
    for offset, id := range busSchedule {
	if id != -1 {
	    for {
		if (t+offset) % id == 0 {
		    break
		}
		t += stepsize
	    }
	    stepsize = lcm(stepsize, id)
	}
    }
    return t
}

func main() {
    fmt.Printf("Part 1: %d\n", part1())
    fmt.Printf("Part 2: %d\n", part2())
}
