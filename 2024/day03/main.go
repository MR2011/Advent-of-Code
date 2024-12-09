package day03

import (
	"aoc-2024/common"
	"bufio"
	"fmt"
	"os"
	"regexp"
)

const (
	DO   = `do\(\)`
	DONT = `don't\(\)`
	MUL  = `mul\((\d+),(\d+)\)`
)

func Part1() int {
	file, err := os.Open("./day03/input")

	if err != nil {
		fmt.Printf("Error: %v\n", err)
		return 0
	}

	defer file.Close()
	scanner := bufio.NewScanner(file)

	result := 0

	for scanner.Scan() {
		r := regexp.MustCompile(MUL)
		for _, match := range r.FindAllStringSubmatch(scanner.Text(), -1) {
			result += (common.Atoi(match[1]) * common.Atoi(match[2]))
		}
	}

	return result
}

func Part2() int {
	file, err := os.Open("./day03/input")

	if err != nil {
		fmt.Printf("Error: %v\n", err)
		return 0
	}

	defer file.Close()
	scanner := bufio.NewScanner(file)

	result := 0
	active := true

	for scanner.Scan() {
		r := regexp.MustCompile(DO + "|" + DONT + "|" + MUL)
		for _, match := range r.FindAllStringSubmatch(scanner.Text(), -1) {
			if m, _ := regexp.MatchString(DO, match[0]); m {
				active = true
			} else if m, _ := regexp.MatchString(DONT, match[0]); m {
				active = false
			} else if active {
				result += (common.Atoi(match[1]) * common.Atoi(match[2]))
			}
		}
	}

	return result
}

func Run() {
	fmt.Printf("Day 01:\n\tPart 1: %d\n\tPart 2: %d\n", Part1(), Part2())
}
