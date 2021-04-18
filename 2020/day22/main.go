package main

import (
    "fmt"
    "io/ioutil"
    "strings"
    "strconv"
)

type Deck struct {
    id int
    cards []int
}

func (d *Deck) copy(cards int) Deck {
    newCards := []int{}
    for i:=0; i<cards; i++ {
	newCards = append(newCards, d.cards[i])
    }
    return Deck{d.id, newCards}
}

func (d *Deck) getState() string {
    return strings.Trim(strings.Join(strings.Split(fmt.Sprint(d.cards), " "), ","), "[]")
}

func (d *Deck) pop() int {
    card := d.cards[0]
    d.cards = d.cards[1:]
    return card
}

func (d *Deck) append(a int, b int) {
    d.cards = append(d.cards, a)
    d.cards = append(d.cards, b)
}

func (d *Deck) len() int {
    return len(d.cards)
}

func (d *Deck) score() int {
    s := 0
    for i, card := range d.cards {
	s += (card * (len(d.cards) - i))
    }
    return s
}

func parsePlayerInput(id int, input string) Deck {
    cards := []int{}
    for _, line := range strings.Split(input, "\n") {
	if strings.HasPrefix(line, "Player") || line == "" {
	    continue
	}
	num, _ := strconv.Atoi(line)
	cards = append(cards, num)
    }
    return Deck{id, cards}
}

func parseInput() (Deck, Deck){
    data, _ := ioutil.ReadFile("input")
    playersRaw := strings.Split(string(data), "\n\n")
    p1 := parsePlayerInput(1, playersRaw[0])
    p2 := parsePlayerInput(2, playersRaw[1])
    return p1, p2
}

func playRound(p1 *Deck, p2 *Deck) {
    c1 := p1.pop()
    c2 := p2.pop()
    if c1 > c2 {
	p1.append(c1, c2)
    } else {
	p2.append(c2, c1)
    }
}

func playRecursive(p1 Deck, p2 Deck) Deck {
    prevRounds := map[string]bool{}
    for {
	state := p1.getState() + "|" + p2.getState()
	if prevRounds[state] {
	    return p1
	}
	prevRounds[state] = true
	c1 := p1.pop()
	c2 := p2.pop()
	if c1 <= p1.len() && c2 <= p2.len() {
	    winner := playRecursive(p1.copy(c1), p2.copy(c2))
	    if winner.id == p1.id {
		p1.append(c1, c2)
	    } else {
		p2.append(c2, c1)
	    }
	} else {
	    if c1 > c2 {
		p1.append(c1, c2)
	    } else {
		p2.append(c2, c1)
	    }
	}
	if p1.len() == 0 {
	    return p2
	} else if p2.len() == 0 {
	    return p1
	}
    }
}

func part1() int {
    p1, p2 := parseInput()
    for p1.len() > 0 && p2.len() > 0 {
	playRound(&p1, &p2)
    }
    if p1.len() == 0 {
	return p2.score()
    } else {
	return p1.score()
    }
}

func part2() int {
    p1, p2 := parseInput()
    winner := playRecursive(p1, p2)
    return winner.score()
}

func main() {
    fmt.Printf("Part 1: %d\n", part1())
    fmt.Printf("Part 2: %d\n", part2())
}
