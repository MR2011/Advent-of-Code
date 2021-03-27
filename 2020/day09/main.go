package main

import (
    "bufio"
    "fmt"
    "os"
    "strconv"
)

func parseInput() []int {
    file, _ := os.Open("input")
    scanner := bufio.NewScanner(file)
    defer file.Close()
    cipher := []int{}
    for scanner.Scan() {
	number, _ := strconv.Atoi(scanner.Text())
	cipher = append(cipher, number)
    }
    return cipher
}


func part1() int {
    cipher := parseInput()
    for i := 25; i < len(cipher); i++ {
	n := cipher[i]
	preamble := map[int]bool{}
	for j := (i-25); j < i; j++ {
	    preamble[cipher[j]] = true
	}
	valid := false
	for j := (i-25); j < i; j++ {
	    if preamble[n - cipher[j]] {
		valid = true
	    }
	}
	if !valid {
	    return n
	}
    }
    return 1
}

func part2() int {
    cipher := parseInput()
    n := 23278925
    for i, _ := range cipher {
	m := 0
	min := n
	max := 0
	for j := i; j < len(cipher) && m < n; j++ {
	    m += cipher[j]
	    if cipher[j] < min {
		min = cipher[j]
	    } else if cipher[j] > max {
		max = cipher[j]
	    }
	}
	if m == n {
	    return min + max
	}
    }
    return -1
}

func main() {
    fmt.Printf("Part 1: %d\n", part1())
    fmt.Printf("Part 2: %d\n", part2())
}
