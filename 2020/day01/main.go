package main

import (
    "bufio"
    "strconv"
    "fmt"
    "os"
)

func parse_input() []int {
    file, _ := os.Open("input")
    scanner := bufio.NewScanner(file)
    defer file.Close()
    var numbers []int
    for scanner.Scan() {
	number, _ := strconv.Atoi(scanner.Text())
	numbers = append(numbers, number)
    }
    return numbers
}

func part1() int {
    numbers := parse_input()
    for i, n1 := range(numbers) {
	for _, n2 := range(numbers[i+1:]) {
	    if n1 + n2 == 2020 {
		return n1 * n2
	    }
	}
    }
    return -1
}

func part2() int {
    numbers := parse_input()
    for i, n1 := range(numbers) {
	for j, n2 := range(numbers[i+1:]) {
	    for _, n3 := range(numbers[j+1:]) {
		if n1 + n2 + n3 == 2020 {
		    return n1 * n2 * n3
		}
	    }
	}
    }
    return -1
}

func main() {
    fmt.Printf("Part1: %d\n",  part1())
    fmt.Printf("Part2: %d\n",  part2())
}
