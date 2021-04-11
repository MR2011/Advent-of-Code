package main

import (
    "bufio"
    "fmt"
    "os"
    "strings"
    "strconv"
)

func isDigit(s string) bool {
    _, err := strconv.Atoi(s)
    return err == nil
}

func isOperator(s string) bool {
    return s == "*" || s == "+"
}

func compute(operator string, a int, b int) int {
    if operator == "+" {
	return a + b
    }else if operator == "*" {
	return a * b
    }
    return 0
}

func eval(tokens *[]string) int {
    accumulator := 0
    op := "+"
    for i, token := range *tokens {
	(*tokens)[i] = ""
	switch {
	case isDigit(token):
	    val, _ := strconv.Atoi(token)
	    accumulator = compute(op, val, accumulator)
	case isOperator(token):
	    op = token
	case token == "(":
	    val := eval(tokens)
	    accumulator = compute(op, val, accumulator)
	case token == ")":
	    return accumulator
	}
    }
    return accumulator
}

func eval2(tokens *[]string) int {
    multiplier := 1
    accumulator := 0
    for i, token := range *tokens {
	(*tokens)[i] = ""
	switch {
	case isDigit(token):
	    val, _ := strconv.Atoi(token)
	    accumulator += val * multiplier
	case token == "*":
	    multiplier = accumulator
	    accumulator = 0
	case token == "(":
	    val := eval2(tokens)
	    accumulator += val * multiplier
	case token == ")":
	    return accumulator
	}
    }
    return accumulator
}

func parseInput() []string {
    file, _ := os.Open("input")
    scanner := bufio.NewScanner(file)
    defer file.Close()
    equations := []string{}
    for scanner.Scan() {
	// Add whitespaces for tokenizing
	equation := strings.ReplaceAll(scanner.Text(), "(", "( ")
	equation = strings.ReplaceAll(equation, ")", " )")
	equations = append(equations, equation)
    }
    return equations
}

func part1() int {
    equations := parseInput()
    total := 0
    for _, equation := range equations {
	tokens := strings.Fields(equation)
	total += eval(&tokens)
    }
    return total
}

func part2() int {
    equations := parseInput()
    total := 0
    for _, equation := range equations {
	tokens := strings.Fields(equation)
	total += eval2(&tokens)
    }
    return total
}

// https://github.com/mebeim/aoc/blob/master/2020/README.md#day-18---operation-order
func main() {
    fmt.Printf("Part 1: %d\n", part1())
    fmt.Printf("Part 2: %d\n", part2())
}
