package main

import (
    "fmt"
)

func transform(subjectNum int, loopSize int) int {
    result := 1
    for i:=0; i<loopSize; i++ {
	result *= subjectNum
	result = result % 20201227
    }
    return result
}

func calcLoopSize(pubKey int) int {
    i := 1
    key := 1
    for {
	key *= 7
	key = key % 20201227
	if key == pubKey {
	    return i
	}
	i++
    }
}

func part1() int {
    cardPubKey := 2069194
    doorPubKey := 16426071
    cardLoopSize := calcLoopSize(cardPubKey)
    encKey := transform(doorPubKey, cardLoopSize)
    return encKey
}

func part2() int {
    return 2
}

func main() {
    fmt.Printf("Part 1: %d\n", part1())
    fmt.Printf("Part 2: %d\n", part2())
}
