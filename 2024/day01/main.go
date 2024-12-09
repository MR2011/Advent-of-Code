package day01

import (
	"aoc-2024/common"
	"bufio"
	"fmt"
	"os"
	"sort"
	"strings"

	"github.com/samber/lo"
)

type Lists struct {
	Left  []int
	Right []int
}

func parseInput(path string) (Lists, error) {
	file, err := os.Open(path)

	if err != nil {
		return Lists{}, err
	}

	scanner := bufio.NewScanner(file)
	defer file.Close()

	left := []int{}
	right := []int{}

	for scanner.Scan() {
		tokens := strings.Fields(scanner.Text())
		left = append(left, common.Atoi(tokens[0]))
		right = append(right, common.Atoi(tokens[1]))
	}

	l := Lists{
		Left:  left,
		Right: right,
	}

	return l, nil
}

func Part1() int {
	lists, err := parseInput("day01/input")
	if err != nil {
		fmt.Printf("Error: %v\n", err)
		return 0
	}

	sort.Ints(lists.Left)
	sort.Ints(lists.Right)

	sum := 0

	for i := range lists.Left {
		sum += common.AbsInt(lists.Left[i] - lists.Right[i])
	}

	return sum
}

func Part2() int {
	lists, err := parseInput("day01/input")
	if err != nil {
		fmt.Printf("Error: %v\n", err)
		return 0
	}
	counts := lo.CountValues(lists.Right)

	similarity := 0
	for _, l := range lists.Left {
		similarity += l * counts[l]
	}

	return similarity
}

func Run() {
	fmt.Printf("Day 01:\n\tPart 1: %d\n\tPart 2: %d\n", Part1(), Part2())
}
