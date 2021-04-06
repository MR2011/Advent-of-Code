package main

import (
    "fmt"
    "io/ioutil"
    "strings"
    "strconv"
)

type Ticket []int
type RuleCandidates map[int]map[Rule]bool

type Rule struct {
    name string
    min1 int
    max1 int
    min2 int
    max2 int
}

func parseTicket(s string) Ticket {
    ticket := Ticket{}
    for _, num := range strings.Split(s, ",") {
	i, _ := strconv.Atoi(num)
	ticket = append(ticket, i)
    }
    return ticket
}

func parseInput() ([]Rule, Ticket, []Ticket){
    data, _ := ioutil.ReadFile("input")
    parts := strings.Split(string(data), "\n\n")
    rules := []Rule{}
    for _, line := range strings.Split(parts[0], "\n") {
	rule := Rule{}
	fmt.Sscanf(line, "%s %d-%d or %d-%d",
	    &rule.name,
	    &rule.min1,
	    &rule.max1,
	    &rule.min2,
	    &rule.max2,
	)
	rules = append(rules, rule)
    }
    myTicket := parseTicket(strings.Split(parts[1], "\n")[1])
    tickets := []Ticket{}
    for i, line := range strings.Split(parts[2], "\n") {
	if i > 0  && len(line) > 0 {
	    tickets = append(tickets, parseTicket(line))
	}
    }
    return rules, myTicket, tickets
}


func validateField(value int, rule Rule) bool {
    if (value >= rule.min1 && value <= rule.max1) {
	return true
    }
    if (value >= rule.min2 && value <= rule.max2){
	return true
    }
    return false
}

func size(m map[Rule]bool) int {
    size := 0
    for _, v := range m {
	if v {
	    size++
	}
    }
    return size
}

func getValidTickets(tickets []Ticket, rules []Rule) ([]Ticket, int) {
    validTickets := []Ticket{}
    errRate := 0
    for _, ticket := range tickets {
	validTicket := true
	for _, num := range ticket {
	    validField := false
	    for _, rule := range rules {
		if validateField(num, rule) {
		    validField = true
		    break
		}
	    }
	    if !validField {
		validTicket = false
		errRate += num
	    }
	}
	if validTicket {
	    validTickets = append(validTickets, ticket)
	}
    }
    return validTickets, errRate
}

func getRuleCandidates(tickets []Ticket, rules []Rule, fieldSize int) RuleCandidates{
    ruleCandidates := RuleCandidates{}
    for field:=0; field<fieldSize; field++ {
	// Valid rules are stored in a Bool-Map
	ruleCandidates[field] = map[Rule]bool{}
	for _, rule := range rules {
	    validRule := true
	    for _, ticket := range tickets {
		if !validateField(ticket[field], rule) {
		    validRule = false
		    break
		}
	    }
	    if validRule {
		ruleCandidates[field][rule] = true
	    }
	}
    }
    return ruleCandidates
}

func reduceRuleCandidates(ruleCandidates RuleCandidates, fieldSize int) RuleCandidates{
    // For each field find all valid rules
    processed := map[Rule]bool{}
    for len(processed) < fieldSize {
	// Find field with only one valid rule and remove this rule for all
	// other fields
	for field, rules := range ruleCandidates {
	    if size(rules) == 1 {
		toRemove := Rule{}
		for rule, isValid := range rules {
		    if isValid {
			toRemove = rule
		    }
		}
		// Rule already removed from other fields?
		if processed[toRemove] {
		    continue
		}
		for f, _ := range ruleCandidates {
		    if f != field {
			ruleCandidates[f][toRemove] = false
		    }
		}
		processed[toRemove] = true
	    }
	}
    }
    return ruleCandidates
}

func part1() int {
    rules, _, tickets := parseInput()
    _, errRate := getValidTickets(tickets, rules)
    return errRate
}

func part2() int {
    rules, myTicket, tickets := parseInput()
    validTickets, _ := getValidTickets(tickets, rules)
    ruleCandidates := getRuleCandidates(validTickets, rules, len(myTicket))
    fieldRules := reduceRuleCandidates(ruleCandidates, len(myTicket))
    product := 1
    for field, rules := range fieldRules {
	for rule, value := range rules {
	    if value && strings.HasPrefix(rule.name, "departure") {
		product *= myTicket[field]
	    }
	}
    }
    return product
}

// I removed the whitespaces in the input rule names to make parsing easier
func main() {
    fmt.Printf("Part 1: %d\n", part1())
    fmt.Printf("Part 2: %d\n", part2())
}
