package common

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"

	"github.com/samber/lo"
)

func Atoi(s string) int {
	i, err := strconv.Atoi(s)
	if err != nil {
		fmt.Printf("error: %v\n", err)
		return 0
	}
	return i
}

func AbsInt(i int) int {
	if i < 0 {
		return -i
	}
	return i
}

func ReadIntMatrix(path string) [][]int {
	file, err := os.Open(path)

	if err != nil {
		fmt.Printf("error: %v\n", err)
		return [][]int{}
	}

	scanner := bufio.NewScanner(file)
	defer file.Close()

	matrix := [][]int{}
	for scanner.Scan() {
		tokens := lo.Map(strings.Fields(scanner.Text()), func(s string, index int) int {
			return Atoi(s)
		})
		matrix = append(matrix, tokens)
	}

	return matrix
}
