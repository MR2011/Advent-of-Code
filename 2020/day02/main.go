package main

import (
    "bufio"
    "fmt"
    "os"
    "strings"
    "strconv"
)

type Entry struct {
    min int
    max int
    char rune
    password string
}

func (e Entry) CharMatches(i int, char rune) bool {
    return rune(e.password[i-1]) == char
}

func parse_input() []Entry {
    file, _ := os.Open("input")
    scanner := bufio.NewScanner(file)
    defer file.Close()
    var entries []Entry
    for scanner.Scan() {
	tokens := strings.Fields(scanner.Text())
	frequency := strings.Split(tokens[0], "-")
	min, _ := strconv.Atoi(frequency[0])
	max, _ := strconv.Atoi(frequency[1])
	entries = append(entries, Entry{
	    min: min,
	    max: max,
	    char: rune(tokens[1][0]),
	    password: tokens[2],
	})
    }
    return entries
}

func part1() int {
    entries := parse_input()
    valid := 0
    for _, e := range entries {
	c := strings.Count(e.password, string(e.char))
	if c >= e.min && c <= e.max {
	    valid++
	}
    }
    return valid
}

func part2() int {
    entries := parse_input()
    valid := 0
    for _, e := range entries {
	if e.CharMatches(e.min, e.char) != e.CharMatches(e.max, e.char) {
	    valid++
	}
    }
    return valid
}

func main() {
    fmt.Printf("Part 1: %d\n", part1())
    fmt.Printf("Part 2: %d\n", part2())
}
