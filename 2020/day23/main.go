package main

import (
    "fmt"
    "strings"
    "strconv"
    "container/ring"
)

const INPUT string = "916438275"
// const INPUT string = "389125467"

type Game struct {
    cups *ring.Ring
    current int
    max int
    startAt map[int]*ring.Ring
}

func (g *Game) round() {
    picks := g.cups.Unlink(3)
    dst := g.cups.Value.(int)
    for {
	dst--
	if dst < 1 {
	    dst = g.max
	}
	picked := false
	picks.Do(func(v interface{}) {
	    if v.(int) == dst {
		picked = true
	    }
	})

	if !picked {
	    break
	}
    }
    g.startAt[dst].Link(picks)
    g.cups = g.cups.Next()
}

func createGame(size int) Game {
    item := ring.New(size)
    max := 0
    startAt := map[int]*ring.Ring{}
    for _, s := range strings.Split(INPUT, "") {
	num, _ := strconv.Atoi(s)
	startAt[num] = item
	item.Value = num
	item = item.Next()
    }
    if size == 1_000_000 {
	for i:=10; i<=size; i++ {
	    startAt[i] = item
	    item.Value = i
	    item = item.Next()
	}
	max = 1_000_000
    } else {
	max = 9
    }
    return Game{item, 0, max, startAt}
}

func part1() string {
    game := createGame(len(INPUT))
    for i:=0; i<100; i++ {
	game.round()
    }
    start := game.startAt[1]
    output := strings.Builder{}
    start.Next().Do(func(v interface{}) {
	if v.(int) == 1 {
	    return
	}
	output.WriteString(fmt.Sprint(v))
    })
    return output.String()
}

func part2() int {
    game := createGame(1_000_000)
    for i:=0; i<10_000_000; i++ {
	game.round()
    }
    first := game.startAt[1].Next()
    second :=  first.Next()
    return first.Value.(int) * second.Value.(int)
}

// Used ring solution from
// https://gitlab.com/kurisuchan/advent-of-code-2020/-/blob/master/pkg/day23/day23.go
func main() {
    fmt.Printf("Part 1: %s\n", part1())
    fmt.Printf("Part 2: %d\n", part2())
}
