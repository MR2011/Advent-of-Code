package main

import (
    "bufio"
    "fmt"
    "os"
    "sort"
    "strings"
    "strconv"
)

func parseInput() []string {
    file, _ := os.Open("input")
    scanner := bufio.NewScanner(file)
    defer file.Close()
    seats := []string{}
    for scanner.Scan() {
	seats = append(seats, scanner.Text())
    }
    return seats
}

func calcSeatID(seat string) int {
    minRowNo := 0
    maxRowNo := 127
    minColNo := 0
    maxColNo := 7
    for i, c := range seat {
	if i < 7 {
	    if c == 'F' {
		maxRowNo = (minRowNo + maxRowNo) / 2
	    }else if c == 'B' {
		minRowNo = (minRowNo + maxRowNo) / 2 + 1
	    }
	} else {
	    if c == 'L' {
		maxColNo = (minColNo + maxColNo) / 2
	    } else if c == 'R' {
		minColNo = (minColNo + maxColNo) / 2 + 1
	    }
	}
    }
    return minRowNo * 8 + minColNo
}

// After looking at some other solutions, I learned that the seat string can be
// converted to a binary string and then interpreted as a binary number.

func binarySolution() {
    seats := parseInput()
    ids := []int{}
    for _, seat := range seats {
	binString := strings.
	    NewReplacer("F", "0", "B", "1", "L", "0", "R", "1").
	    Replace(seat)
	id, _ := strconv.ParseUint(binString, 2, 10)
	ids = append(ids, int(id))

    }
    sort.Ints(ids)
}


func part1() int {
    seats := parseInput()
    maxID := 0
    for _, seat := range seats {
	seatID := calcSeatID(seat)
	if seatID > maxID {
	    maxID = seatID
	}
    }
    return maxID
}

func part2() int {
    seats := parseInput()
    ids := []int{}
    for _, seat := range seats {
	seatID := calcSeatID(seat)
	ids = append(ids, seatID)
    }
    sort.Ints(ids)
    for i, id := range ids {
	if i > 1 {
	    if id - ids[i-1] > 1 {
		return id - 1
	    }
	}
    }
    return -1
}

func main() {
    fmt.Printf("Part 1: %d\n", part1())
    fmt.Printf("Part 2: %d\n", part2())
}
