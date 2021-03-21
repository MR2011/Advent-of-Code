package main

import (
    "bufio"
    "fmt"
    "os"
    "regexp"
    "strings"
)

type Rules map[string]map[string]int

func parseInput() Rules {
    file, _ := os.Open("input")
    scanner := bufio.NewScanner(file)
    defer file.Close()
    rules := map[string]map[string]int{}
    for scanner.Scan() {
	tokens := strings.Split(scanner.Text(), " bags contain ")
	name := tokens[0]
        reg := regexp.MustCompile(`\d\s\w+\s\w+`)
        matches := reg.FindAllString(tokens[1], -1)
	rules[name] = make(map[string]int)
	for _, val := range matches {
            count, description, color := 0, "", ""
            fmt.Sscanf(val, "%d %s %s", &count, &description, &color)
	    rules[name][description + " " + color] = count
        }
    }
    return rules
}

func containsBag(parent string, bag string, rules Rules) bool {
    if rules[parent][bag] > 0 {
	return true
    }
    contains := false
    for c, _ := range rules[parent] {
	if containsBag(c, bag, rules) {
	    contains = true
	}
    }
    return contains
}

func countBags(name string, rules Rules) int {
    total := 0
    for child, num := range rules[name] {
	total += num + (num * countBags(child, rules))
    }
    return total
}

func part1() int {
    rules := parseInput()
    n := 0
    for bag, _ := range rules {
	if containsBag(bag, "shiny gold", rules) {
	    n++
	}
    }
    return n
}
func part2() int {
    rules := parseInput()
    return countBags("shiny gold", rules)
}

func main() {
    fmt.Printf("Part 1: %d\n", part1())
    fmt.Printf("Part 2: %d\n", part2())
}
