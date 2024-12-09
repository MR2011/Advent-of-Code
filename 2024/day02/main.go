package day02

import (
	"aoc-2024/common"
	"fmt"

	"github.com/samber/lo"
)

func isSafe(l []int) bool {
	for i := range l {
		if i < len(l)-1 {
			diff := common.AbsInt(l[i] - l[i+1])
			if diff < 1 || diff > 3 {
				return false
			}
		}
	}
	return lo.IsSorted(l) || lo.IsSorted(lo.Reverse(l))
}

func Part1() int {
	matrix := common.ReadIntMatrix("day02/input")

	safe := 0
	for _, row := range matrix {
		if isSafe(row) {
			safe += 1
		}
	}
	return safe
}

func Part2() int {
	matrix := common.ReadIntMatrix("day02/input")

	safe := 0
	for _, row := range matrix {
		if isSafe(row) {
			safe += 1
		} else {
			for i := range row {
				if isSafe(lo.DropByIndex(row, i)) {
					safe += 1
					break
				}
			}
		}
	}
	return safe
}

func Run() {
	fmt.Printf("Day 02:\n\tPart 1: %d\n\tPart 2: %d\n", Part1(), Part2())
}
